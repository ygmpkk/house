%
% (c) The University of Glasgow 2002
%
\section[ByteCodeGen]{Generate bytecode from Core}

\begin{code}
module ByteCodeGen ( UnlinkedBCO, byteCodeGen, coreExprToBCOs ) where

#include "HsVersions.h"

import ByteCodeInstr
import ByteCodeFFI	( mkMarshalCode, moan64 )
import ByteCodeAsm	( CompiledByteCode(..), UnlinkedBCO, 
			  assembleBCO, assembleBCOs, iNTERP_STACK_CHECK_THRESH )
import ByteCodeLink	( lookupStaticPtr )

import Outputable
import Name		( Name, getName, mkSystemName )
import Id
import FiniteMap
import ForeignCall	( ForeignCall(..), CCallTarget(..), CCallSpec(..) )
import HscTypes		( TypeEnv, typeEnvTyCons, typeEnvClasses )
import CoreUtils	( exprType )
import CoreSyn
import PprCore		( pprCoreExpr )
import Literal		( Literal(..), literalPrimRep )
import PrimRep
import PrimOp		( PrimOp(..) )
import CoreFVs		( freeVars )
import Type		( typePrimRep, isUnLiftedType, splitTyConApp_maybe )
import DataCon		( DataCon, dataConTag, fIRST_TAG, dataConTyCon, 
                          isUnboxedTupleCon, isNullaryDataCon, dataConWorkId,
			  dataConRepArity )
import TyCon		( tyConFamilySize, isDataTyCon, tyConDataCons,
			  isUnboxedTupleTyCon )
import Class		( Class, classTyCon )
import Type		( Type, repType, splitFunTys, dropForAlls )
import Util
import DataCon		( dataConRepArity )
import Var		( isTyVar )
import VarSet		( VarSet, varSetElems )
import TysPrim		( arrayPrimTyCon, mutableArrayPrimTyCon,
			  byteArrayPrimTyCon, mutableByteArrayPrimTyCon
			)
import PrimRep		( isFollowableRep )
import CmdLineOpts	( DynFlags, DynFlag(..) )
import ErrUtils		( showPass, dumpIfSet_dyn )
import Unique		( mkPseudoUnique3 )
import FastString	( FastString(..), unpackFS )
import Panic		( GhcException(..) )
import PprType		( pprType )
import SMRep		( arrWordsHdrSize, arrPtrsHdrSize, StgWord )
import Bitmap		( intsToReverseBitmap, mkBitmap )
import OrdList
import Constants	( wORD_SIZE )

import Data.List	( intersperse, sortBy, zip4, zip5, partition )
import Foreign		( Ptr, castPtr, mallocBytes, pokeByteOff, Word8 )
import Foreign.C	( CInt )
import Control.Exception	( throwDyn )

import GHC.Exts		( Int(..), ByteArray# )

import Control.Monad	( when )
import Data.Char	( ord )

-- -----------------------------------------------------------------------------
-- Generating byte code for a complete module 

byteCodeGen :: DynFlags
            -> [CoreBind]
	    -> TypeEnv
            -> IO CompiledByteCode
byteCodeGen dflags binds type_env
   = do showPass dflags "ByteCodeGen"
        let  local_tycons  = typeEnvTyCons  type_env
	     local_classes = typeEnvClasses type_env
	     tycs = local_tycons ++ map classTyCon local_classes

        let flatBinds = [ (bndr, freeVars rhs) 
			| (bndr, rhs) <- flattenBinds binds]

        (BcM_State final_ctr mallocd, proto_bcos)
           <- runBc (mapM schemeTopBind flatBinds)

        when (notNull mallocd)
             (panic "ByteCodeGen.byteCodeGen: missing final emitBc?")

        dumpIfSet_dyn dflags Opt_D_dump_BCOs
           "Proto-BCOs" (vcat (intersperse (char ' ') (map ppr proto_bcos)))

        assembleBCOs proto_bcos tycs
        
-- -----------------------------------------------------------------------------
-- Generating byte code for an expression

-- Returns: (the root BCO for this expression, 
--           a list of auxilary BCOs resulting from compiling closures)
coreExprToBCOs :: DynFlags
	       -> CoreExpr
               -> IO UnlinkedBCO
coreExprToBCOs dflags expr
 = do showPass dflags "ByteCodeGen"

      -- create a totally bogus name for the top-level BCO; this
      -- should be harmless, since it's never used for anything
      let invented_name  = mkSystemName (mkPseudoUnique3 0) FSLIT("ExprTopLevel")
          invented_id    = mkLocalId invented_name (panic "invented_id's type")
	  
      (BcM_State final_ctr mallocd, proto_bco) 
         <- runBc (schemeTopBind (invented_id, freeVars expr))

      when (notNull mallocd)
           (panic "ByteCodeGen.coreExprToBCOs: missing final emitBc?")

      dumpIfSet_dyn dflags Opt_D_dump_BCOs "Proto-BCOs" (ppr proto_bco)

      assembleBCO proto_bco


-- -----------------------------------------------------------------------------
-- Compilation schema for the bytecode generator

type BCInstrList = OrdList BCInstr

type Sequel = Int	-- back off to this depth before ENTER

-- Maps Ids to the offset from the stack _base_ so we don't have
-- to mess with it after each push/pop.
type BCEnv = FiniteMap Id Int	-- To find vars on the stack

ppBCEnv :: BCEnv -> SDoc
ppBCEnv p
   = text "begin-env"
     $$ nest 4 (vcat (map pp_one (sortBy cmp_snd (fmToList p))))
     $$ text "end-env"
     where
        pp_one (var, offset) = int offset <> colon <+> ppr var <+> ppr (idPrimRep var)
        cmp_snd x y = compare (snd x) (snd y)

-- Create a BCO and do a spot of peephole optimisation on the insns
-- at the same time.
mkProtoBCO
   :: name
   -> BCInstrList
   -> Either  [AnnAlt Id VarSet] (AnnExpr Id VarSet)
   -> Int
   -> Int
   -> [StgWord]
   -> Bool   	-- True <=> is a return point, rather than a function
   -> [Ptr ()]
   -> ProtoBCO name
mkProtoBCO nm instrs_ordlist origin arity bitmap_size bitmap
  is_ret mallocd_blocks
   = ProtoBCO {
	protoBCOName = nm,
	protoBCOInstrs = maybe_with_stack_check,
	protoBCOBitmap = bitmap,
	protoBCOBitmapSize = bitmap_size,
	protoBCOArity = arity,
	protoBCOExpr = origin,
	protoBCOPtrs = mallocd_blocks
      }
     where
        -- Overestimate the stack usage (in words) of this BCO,
        -- and if >= iNTERP_STACK_CHECK_THRESH, add an explicit
        -- stack check.  (The interpreter always does a stack check
        -- for iNTERP_STACK_CHECK_THRESH words at the start of each
        -- BCO anyway, so we only need to add an explicit on in the
        -- (hopefully rare) cases when the (overestimated) stack use
        -- exceeds iNTERP_STACK_CHECK_THRESH.
        maybe_with_stack_check
	   | is_ret = peep_d
		-- don't do stack checks at return points;
		-- everything is aggregated up to the top BCO
		-- (which must be a function)
           | stack_overest >= 65535
           = pprPanic "mkProtoBCO: stack use won't fit in 16 bits" 
                      (int stack_overest)
           | stack_overest >= iNTERP_STACK_CHECK_THRESH
           = STKCHECK stack_overest : peep_d
           | otherwise
           = peep_d	-- the supposedly common case
             
        stack_overest = sum (map bciStackUse peep_d)

        -- Merge local pushes
        peep_d = peep (fromOL instrs_ordlist)

        peep (PUSH_L off1 : PUSH_L off2 : PUSH_L off3 : rest)
           = PUSH_LLL off1 (off2-1) (off3-2) : peep rest
        peep (PUSH_L off1 : PUSH_L off2 : rest)
           = PUSH_LL off1 (off2-1) : peep rest
        peep (i:rest)
           = i : peep rest
        peep []
           = []

argBits :: [PrimRep] -> [Bool]
argBits [] = []
argBits (rep : args)
  | isFollowableRep rep = False : argBits args
  | otherwise = take (getPrimRepSize rep) (repeat True) ++ argBits args

-- -----------------------------------------------------------------------------
-- schemeTopBind

-- Compile code for the right-hand side of a top-level binding

schemeTopBind :: (Id, AnnExpr Id VarSet) -> BcM (ProtoBCO Name)


schemeTopBind (id, rhs)
  | Just data_con <- isDataConWorkId_maybe id,
    isNullaryDataCon data_con
  = 	-- Special case for the worker of a nullary data con.
	-- It'll look like this:	Nil = /\a -> Nil a
	-- If we feed it into schemeR, we'll get 
	--	Nil = Nil
	-- because mkConAppCode treats nullary constructor applications
	-- by just re-using the single top-level definition.  So
	-- for the worker itself, we must allocate it directly.
    emitBc (mkProtoBCO (getName id) (toOL [PACK data_con 0, ENTER])
                       (Right rhs) 0 0 [{-no bitmap-}] False{-not alts-})

  | otherwise
  = schemeR [{- No free variables -}] (id, rhs)

-- -----------------------------------------------------------------------------
-- schemeR

-- Compile code for a right-hand side, to give a BCO that,
-- when executed with the free variables and arguments on top of the stack,
-- will return with a pointer to the result on top of the stack, after
-- removing the free variables and arguments.
--
-- Park the resulting BCO in the monad.  Also requires the
-- variable to which this value was bound, so as to give the
-- resulting BCO a name. 

schemeR :: [Id] 		-- Free vars of the RHS, ordered as they
				-- will appear in the thunk.  Empty for
				-- top-level things, which have no free vars.
	-> (Id, AnnExpr Id VarSet)
	-> BcM (ProtoBCO Name)
schemeR fvs (nm, rhs) 
{-
   | trace (showSDoc (
              (char ' '
               $$ (ppr.filter (not.isTyVar).varSetElems.fst) rhs
               $$ pprCoreExpr (deAnnotate rhs)
               $$ char ' '
              ))) False
   = undefined
   | otherwise
-}
   = schemeR_wrk fvs nm rhs (collect [] rhs)

collect xs (_, AnnNote note e) = collect xs e
collect xs (_, AnnLam x e)     = collect (if isTyVar x then xs else (x:xs)) e
collect xs (_, not_lambda)     = (reverse xs, not_lambda)

schemeR_wrk fvs nm original_body (args, body)
   = let 
	 all_args  = reverse args ++ fvs
	 arity     = length all_args
	 -- all_args are the args in reverse order.  We're compiling a function
	 -- \fv1..fvn x1..xn -> e 
	 -- i.e. the fvs come first

         szsw_args = map idSizeW all_args
         szw_args  = sum szsw_args
         p_init    = listToFM (zip all_args (mkStackOffsets 0 szsw_args))

	 -- make the arg bitmap
	 bits = argBits (reverse (map idPrimRep all_args))
	 bitmap_size = length bits
	 bitmap = mkBitmap bits
     in
     schemeE szw_args 0 p_init body 		`thenBc` \ body_code ->
     emitBc (mkProtoBCO (getName nm) body_code (Right original_body)
		arity bitmap_size bitmap False{-not alts-})


fvsToEnv :: BCEnv -> VarSet -> [Id]
-- Takes the free variables of a right-hand side, and
-- delivers an ordered list of the local variables that will
-- be captured in the thunk for the RHS
-- The BCEnv argument tells which variables are in the local
-- environment: these are the ones that should be captured
--
-- The code that constructs the thunk, and the code that executes
-- it, have to agree about this layout
fvsToEnv p fvs = [v | v <- varSetElems fvs, 
		      isId v,		-- Could be a type variable
		      v `elemFM` p]

-- -----------------------------------------------------------------------------
-- schemeE

-- Compile code to apply the given expression to the remaining args
-- on the stack, returning a HNF.
schemeE :: Int -> Sequel -> BCEnv -> AnnExpr' Id VarSet -> BcM BCInstrList

-- Delegate tail-calls to schemeT.
schemeE d s p e@(AnnApp f a) 
   = schemeT d s p e

schemeE d s p e@(AnnVar v)
   | not (isUnLiftedType v_type)
   =  -- Lifted-type thing; push it in the normal way
     schemeT d s p e

   | otherwise
   = -- Returning an unlifted value.  
     -- Heave it on the stack, SLIDE, and RETURN.
     pushAtom d p (AnnVar v)	`thenBc` \ (push, szw) ->
     returnBc (push 			-- value onto stack
               `appOL`  mkSLIDE szw (d-s) 	-- clear to sequel
               `snocOL` RETURN_UBX v_rep)	-- go
   where
      v_type = idType v
      v_rep = typePrimRep v_type

schemeE d s p (AnnLit literal)
   = pushAtom d p (AnnLit literal)	`thenBc` \ (push, szw) ->
     let l_rep = literalPrimRep literal
     in  returnBc (push 			-- value onto stack
                   `appOL`  mkSLIDE szw (d-s) 	-- clear to sequel
                   `snocOL` RETURN_UBX l_rep)	-- go


schemeE d s p (AnnLet (AnnNonRec x (_,rhs)) (_,body))
   | (AnnVar v, args_r_to_l) <- splitApp rhs,
     Just data_con <- isDataConWorkId_maybe v,
     dataConRepArity data_con == length args_r_to_l
   = 	-- Special case for a non-recursive let whose RHS is a 
	-- saturatred constructor application.
	-- Just allocate the constructor and carry on
     mkConAppCode d s p data_con args_r_to_l	`thenBc` \ alloc_code ->
     schemeE (d+1) s (addToFM p x d) body	`thenBc` \ body_code ->
     returnBc (alloc_code `appOL` body_code)

-- General case for let.  Generates correct, if inefficient, code in
-- all situations.
schemeE d s p (AnnLet binds (_,body))
   = let (xs,rhss) = case binds of AnnNonRec x rhs  -> ([x],[rhs])
                                   AnnRec xs_n_rhss -> unzip xs_n_rhss
         n_binds = length xs

         fvss  = map (fvsToEnv p' . fst) rhss

         -- Sizes of free vars
         sizes = map (\rhs_fvs -> sum (map idSizeW rhs_fvs)) fvss

	 -- the arity of each rhs
	 arities = map (length . fst . collect []) rhss

         -- This p', d' defn is safe because all the items being pushed
         -- are ptrs, so all have size 1.  d' and p' reflect the stack
         -- after the closures have been allocated in the heap (but not
         -- filled in), and pointers to them parked on the stack.
         p'    = addListToFM p (zipE xs (mkStackOffsets d (nOfThem n_binds 1)))
         d'    = d + n_binds
         zipE  = zipEqual "schemeE"

         -- ToDo: don't build thunks for things with no free variables
         build_thunk dd [] size bco off
            = returnBc (PUSH_BCO bco
                        `consOL` unitOL (MKAP (off+size) size))
         build_thunk dd (fv:fvs) size bco off = do
              (push_code, pushed_szw) <- pushAtom dd p' (AnnVar fv) 
              more_push_code <- build_thunk (dd+pushed_szw) fvs size bco off
              returnBc (push_code `appOL` more_push_code)

         alloc_code = toOL (zipWith mkAlloc sizes arities)
	   where mkAlloc sz 0     = ALLOC_AP sz
		 mkAlloc sz arity = ALLOC_PAP arity sz

	 compile_bind d' fvs x rhs size off = do
		bco <- schemeR fvs (x,rhs)
		build_thunk d' fvs size bco off

	 compile_binds = 
	    [ compile_bind d' fvs x rhs size n
	    | (fvs, x, rhs, size, n) <- 
		zip5 fvss xs rhss sizes [n_binds, n_binds-1 .. 1]
	    ]
     in do
     body_code <- schemeE d' s p' body
     thunk_codes <- sequence compile_binds
     returnBc (alloc_code `appOL` concatOL thunk_codes `appOL` body_code)



schemeE d s p (AnnCase scrut bndr [(DataAlt dc, [bind1, bind2], rhs)])
   | isUnboxedTupleCon dc && VoidRep == typePrimRep (idType bind1)
	-- Convert 
	--	case .... of x { (# VoidRep'd-thing, a #) -> ... }
	-- to
	-- 	case .... of a { DEFAULT -> ... }
	-- becuse the return convention for both are identical.
	--
	-- Note that it does not matter losing the void-rep thing from the
	-- envt (it won't be bound now) because we never look such things up.

   = --trace "automagic mashing of case alts (# VoidRep, a #)" $
     doCase d s p scrut bind2 [(DEFAULT, [], rhs)] True{-unboxed tuple-}

   | isUnboxedTupleCon dc && VoidRep == typePrimRep (idType bind2)
   = --trace "automagic mashing of case alts (# a, VoidRep #)" $
     doCase d s p scrut bind1 [(DEFAULT, [], rhs)] True{-unboxed tuple-}

schemeE d s p (AnnCase scrut bndr [(DataAlt dc, [bind1], rhs)])
   | isUnboxedTupleCon dc
	-- Similarly, convert
	--	case .... of x { (# a #) -> ... }
	-- to
	--	case .... of a { DEFAULT -> ... }
   = --trace "automagic mashing of case alts (# a #)"  $
     doCase d s p scrut bind1 [(DEFAULT, [], rhs)] True{-unboxed tuple-}

schemeE d s p (AnnCase scrut bndr alts)
   = doCase d s p scrut bndr alts False{-not an unboxed tuple-}

schemeE d s p (AnnNote note (_, body))
   = schemeE d s p body

schemeE d s p other
   = pprPanic "ByteCodeGen.schemeE: unhandled case" 
               (pprCoreExpr (deAnnotate' other))


-- Compile code to do a tail call.  Specifically, push the fn,
-- slide the on-stack app back down to the sequel depth,
-- and enter.  Four cases:
--
-- 0.  (Nasty hack).
--     An application "GHC.Prim.tagToEnum# <type> unboxed-int".
--     The int will be on the stack.  Generate a code sequence
--     to convert it to the relevant constructor, SLIDE and ENTER.
--
-- 1.  The fn denotes a ccall.  Defer to generateCCall.
--
-- 2.  (Another nasty hack).  Spot (# a::VoidRep, b #) and treat
--     it simply as  b  -- since the representations are identical
--     (the VoidRep takes up zero stack space).  Also, spot
--     (# b #) and treat it as  b.
--
-- 3.  Application of a constructor, by defn saturated.
--     Split the args into ptrs and non-ptrs, and push the nonptrs, 
--     then the ptrs, and then do PACK and RETURN.
--
-- 4.  Otherwise, it must be a function call.  Push the args
--     right to left, SLIDE and ENTER.

schemeT :: Int 		-- Stack depth
        -> Sequel 	-- Sequel depth
        -> BCEnv 	-- stack env
        -> AnnExpr' Id VarSet 
        -> BcM BCInstrList

schemeT d s p app

--   | trace ("schemeT: env in = \n" ++ showSDocDebug (ppBCEnv p)) False
--   = panic "schemeT ?!?!"

--   | trace ("\nschemeT\n" ++ showSDoc (pprCoreExpr (deAnnotate' app)) ++ "\n") False
--   = error "?!?!" 

   -- Case 0
   | Just (arg, constr_names) <- maybe_is_tagToEnum_call
   = pushAtom d p arg	 		`thenBc` \ (push, arg_words) ->
     implement_tagToId constr_names	`thenBc` \ tagToId_sequence ->
     returnBc (push `appOL`  tagToId_sequence            
                    `appOL`  mkSLIDE 1 (d+arg_words-s)
                    `snocOL` ENTER)

   -- Case 1
   | Just (CCall ccall_spec) <- isFCallId_maybe fn
   = generateCCall d s p ccall_spec fn args_r_to_l

   -- Case 2: Constructor application
   | Just con <- maybe_saturated_dcon,
     isUnboxedTupleCon con
   = case args_r_to_l of
	[arg1,arg2] | isVoidRepAtom arg1 -> 
		  unboxedTupleReturn d s p arg2
	[arg1,arg2] | isVoidRepAtom arg2 -> 
		  unboxedTupleReturn d s p arg1
	_other -> unboxedTupleException

   -- Case 3: Ordinary data constructor
   | Just con <- maybe_saturated_dcon
   = mkConAppCode d s p con args_r_to_l	`thenBc` \ alloc_con ->
     returnBc (alloc_con	 `appOL` 
               mkSLIDE 1 (d - s) `snocOL`
               ENTER)

   -- Case 4: Tail call of function 
   | otherwise
   = doTailCall d s p fn args_r_to_l

   where
      -- Detect and extract relevant info for the tagToEnum kludge.
      maybe_is_tagToEnum_call
         = let extract_constr_Names ty
		 | Just (tyc, []) <- splitTyConApp_maybe (repType ty),
		   isDataTyCon tyc
		   = map (getName . dataConWorkId) (tyConDataCons tyc)
		   -- NOTE: use the worker name, not the source name of
		   -- the DataCon.  See DataCon.lhs for details.
		 | otherwise
		   = panic "maybe_is_tagToEnum_call.extract_constr_Ids"
           in
           case app of
              (AnnApp (_, AnnApp (_, AnnVar v) (_, AnnType t)) arg)
                 -> case isPrimOpId_maybe v of
                       Just TagToEnumOp -> Just (snd arg, extract_constr_Names t)
		       other		-> Nothing
              other -> Nothing

	-- Extract the args (R->L) and fn
	-- The function will necessarily be a variable, 
	-- because we are compiling a tail call
      (AnnVar fn, args_r_to_l) = splitApp app

      -- Only consider this to be a constructor application iff it is
      -- saturated.  Otherwise, we'll call the constructor wrapper.
      n_args = length args_r_to_l
      maybe_saturated_dcon  
	= case isDataConWorkId_maybe fn of
		Just con | dataConRepArity con == n_args -> Just con
		_ -> Nothing

-- -----------------------------------------------------------------------------
-- Generate code to build a constructor application, 
-- leaving it on top of the stack

mkConAppCode :: Int -> Sequel -> BCEnv
	     -> DataCon 		-- The data constructor
	     -> [AnnExpr' Id VarSet] 	-- Args, in *reverse* order
	     -> BcM BCInstrList

mkConAppCode orig_d s p con []	-- Nullary constructor
  = ASSERT( isNullaryDataCon con )
    returnBc (unitOL (PUSH_G (getName (dataConWorkId con))))
	-- Instead of doing a PACK, which would allocate a fresh
	-- copy of this constructor, use the single shared version.

mkConAppCode orig_d s p con args_r_to_l 
  = ASSERT( dataConRepArity con == length args_r_to_l )
    do_pushery orig_d (non_ptr_args ++ ptr_args)
 where
	-- The args are already in reverse order, which is the way PACK
	-- expects them to be.  We must push the non-ptrs after the ptrs.
      (ptr_args, non_ptr_args) = partition isPtrAtom args_r_to_l

      do_pushery d (arg:args)
         = pushAtom d p arg			`thenBc` \ (push, arg_words) ->
           do_pushery (d+arg_words) args	`thenBc` \ more_push_code ->
           returnBc (push `appOL` more_push_code)
      do_pushery d []
         = returnBc (unitOL (PACK con n_arg_words))
	 where
	   n_arg_words = d - orig_d


-- -----------------------------------------------------------------------------
-- Returning an unboxed tuple with one non-void component (the only
-- case we can handle).
--
-- Remember, we don't want to *evaluate* the component that is being
-- returned, even if it is a pointed type.  We always just return.

unboxedTupleReturn
	:: Int -> Sequel -> BCEnv
	-> AnnExpr' Id VarSet -> BcM BCInstrList
unboxedTupleReturn d s p arg = do
  (push, sz) <- pushAtom d p arg
  returnBc (push `appOL`
	    mkSLIDE sz (d-s) `snocOL`
	    RETURN_UBX (atomRep arg))

-- -----------------------------------------------------------------------------
-- Generate code for a tail-call

doTailCall
	:: Int -> Sequel -> BCEnv
	-> Id -> [AnnExpr' Id VarSet]
	-> BcM BCInstrList
doTailCall init_d s p fn args
  = do_pushes init_d args (map (primRepToArgRep.atomRep) args)
  where
  do_pushes d [] reps = do
	ASSERTM( null reps )
        (push_fn, sz) <- pushAtom d p (AnnVar fn)
	ASSERTM( sz == 1 )
	returnBc (push_fn `appOL` (
		  mkSLIDE ((d-init_d) + 1) (init_d - s) `appOL`
		  unitOL ENTER))
  do_pushes d args reps = do
      let (push_apply, n, rest_of_reps) = findPushSeq reps
	  (these_args, rest_of_args) = splitAt n args
      (next_d, push_code) <- push_seq d these_args
      instrs <- do_pushes (next_d + 1) rest_of_args rest_of_reps 
		--                ^^^ for the PUSH_APPLY_ instruction
      returnBc (push_code `appOL` (push_apply `consOL` instrs))

  push_seq d [] = return (d, nilOL)
  push_seq d (arg:args) = do
    (push_code, sz) <- pushAtom d p arg 
    (final_d, more_push_code) <- push_seq (d+sz) args
    return (final_d, push_code `appOL` more_push_code)

-- v. similar to CgStackery.findMatch, ToDo: merge
findPushSeq (RepP: RepP: RepP: RepP: RepP: RepP: RepP: rest)
  = (PUSH_APPLY_PPPPPPP, 7, rest)
findPushSeq (RepP: RepP: RepP: RepP: RepP: RepP: rest)
  = (PUSH_APPLY_PPPPPP, 6, rest)
findPushSeq (RepP: RepP: RepP: RepP: RepP: rest)
  = (PUSH_APPLY_PPPPP, 5, rest)
findPushSeq (RepP: RepP: RepP: RepP: rest)
  = (PUSH_APPLY_PPPP, 4, rest)
findPushSeq (RepP: RepP: RepP: rest)
  = (PUSH_APPLY_PPP, 3, rest)
findPushSeq (RepP: RepP: rest)
  = (PUSH_APPLY_PP, 2, rest)
findPushSeq (RepP: rest)
  = (PUSH_APPLY_P, 1, rest)
findPushSeq (RepV: rest)
  = (PUSH_APPLY_V, 1, rest)
findPushSeq (RepN: rest)
  = (PUSH_APPLY_N, 1, rest)
findPushSeq (RepF: rest)
  = (PUSH_APPLY_F, 1, rest)
findPushSeq (RepD: rest)
  = (PUSH_APPLY_D, 1, rest)
findPushSeq (RepL: rest)
  = (PUSH_APPLY_L, 1, rest)
findPushSeq _
  = panic "ByteCodeGen.findPushSeq"

-- -----------------------------------------------------------------------------
-- Case expressions

doCase  :: Int -> Sequel -> BCEnv
	-> AnnExpr Id VarSet -> Id -> [AnnAlt Id VarSet]
	-> Bool  -- True <=> is an unboxed tuple case, don't enter the result
	-> BcM BCInstrList
doCase d s p (_,scrut)
 bndr alts is_unboxed_tuple
  = let
        -- Top of stack is the return itbl, as usual.
        -- underneath it is the pointer to the alt_code BCO.
        -- When an alt is entered, it assumes the returned value is
        -- on top of the itbl.
        ret_frame_sizeW = 2

	-- An unlifted value gets an extra info table pushed on top
	-- when it is returned.
	unlifted_itbl_sizeW | isAlgCase = 0
	  		    | otherwise = 1

	-- depth of stack after the return value has been pushed
	d_bndr = d + ret_frame_sizeW + idSizeW bndr

	-- depth of stack after the extra info table for an unboxed return
	-- has been pushed, if any.  This is the stack depth at the
	-- continuation.
        d_alts = d_bndr + unlifted_itbl_sizeW

        -- Env in which to compile the alts, not including
        -- any vars bound by the alts themselves
        p_alts = addToFM p bndr (d_bndr - 1)

	bndr_ty = idType bndr
        isAlgCase = not (isUnLiftedType bndr_ty) && not is_unboxed_tuple

        -- given an alt, return a discr and code for it.
	codeALt alt@(DEFAULT, _, (_,rhs))
	   = schemeE d_alts s p_alts rhs	`thenBc` \ rhs_code ->
	     returnBc (NoDiscr, rhs_code)
        codeAlt alt@(discr, bndrs, (_,rhs))
	   -- primitive or nullary constructor alt: no need to UNPACK
	   | null real_bndrs = do
		rhs_code <- schemeE d_alts s p_alts rhs
                returnBc (my_discr alt, rhs_code)
	   -- algebraic alt with some binders
           | ASSERT(isAlgCase) otherwise =
             let
		 (ptrs,nptrs) = partition (isFollowableRep.idPrimRep) real_bndrs
		 ptr_sizes    = map idSizeW ptrs
		 nptrs_sizes  = map idSizeW nptrs
		 bind_sizes   = ptr_sizes ++ nptrs_sizes
		 size         = sum ptr_sizes + sum nptrs_sizes
		 -- the UNPACK instruction unpacks in reverse order...
		 p' = addListToFM p_alts 
			(zip (reverse (ptrs ++ nptrs))
			  (mkStackOffsets d_alts (reverse bind_sizes)))
	     in do
	     rhs_code <- schemeE (d_alts+size) s p' rhs
             return (my_discr alt, unitOL (UNPACK size) `appOL` rhs_code)
	   where
	     real_bndrs = filter (not.isTyVar) bndrs


        my_discr (DEFAULT, binds, rhs) = NoDiscr {-shouldn't really happen-}
        my_discr (DataAlt dc, binds, rhs) 
           | isUnboxedTupleCon dc
           = unboxedTupleException
           | otherwise
           = DiscrP (dataConTag dc - fIRST_TAG)
        my_discr (LitAlt l, binds, rhs)
           = case l of MachInt i     -> DiscrI (fromInteger i)
                       MachFloat r   -> DiscrF (fromRational r)
                       MachDouble r  -> DiscrD (fromRational r)
                       MachChar i    -> DiscrI i
                       _ -> pprPanic "schemeE(AnnCase).my_discr" (ppr l)

        maybe_ncons 
           | not isAlgCase = Nothing
           | otherwise 
           = case [dc | (DataAlt dc, _, _) <- alts] of
                []     -> Nothing
                (dc:_) -> Just (tyConFamilySize (dataConTyCon dc))

	-- the bitmap is relative to stack depth d, i.e. before the
	-- BCO, info table and return value are pushed on.
	-- This bit of code is v. similar to buildLivenessMask in CgBindery,
	-- except that here we build the bitmap from the known bindings of
	-- things that are pointers, whereas in CgBindery the code builds the
	-- bitmap from the free slots and unboxed bindings.
	-- (ToDo: merge?)
	bitmap = intsToReverseBitmap d{-size-} (sortLt (<) rel_slots)
	  where
	  binds = fmToList p
	  rel_slots = concat (map spread binds)
	  spread (id, offset)
		| isFollowableRep (idPrimRep id) = [ rel_offset ]
		| otherwise = []
		where rel_offset = d - offset - 1

     in do
     alt_stuff <- mapM codeAlt alts
     alt_final <- mkMultiBranch maybe_ncons alt_stuff
     let 
         alt_bco_name = getName bndr
         alt_bco = mkProtoBCO alt_bco_name alt_final (Left alts)
			0{-no arity-} d{-bitmap size-} bitmap True{-is alts-}
     -- in
--     trace ("case: bndr = " ++ showSDocDebug (ppr bndr) ++ "\ndepth = " ++ show d ++ "\nenv = \n" ++ showSDocDebug (ppBCEnv p) ++
--	     "\n      bitmap = " ++ show bitmap) $ do
     scrut_code <- schemeE (d + ret_frame_sizeW) (d + ret_frame_sizeW) p scrut
     alt_bco' <- emitBc alt_bco
     let push_alts
	    | isAlgCase = PUSH_ALTS alt_bco'
	    | otherwise = PUSH_ALTS_UNLIFTED alt_bco' (typePrimRep bndr_ty)
     returnBc (push_alts `consOL` scrut_code)


-- -----------------------------------------------------------------------------
-- Deal with a CCall.

-- Taggedly push the args onto the stack R->L,
-- deferencing ForeignObj#s and adjusting addrs to point to
-- payloads in Ptr/Byte arrays.  Then, generate the marshalling
-- (machine) code for the ccall, and create bytecodes to call that and
-- then return in the right way.  

generateCCall :: Int -> Sequel 		-- stack and sequel depths
              -> BCEnv
              -> CCallSpec		-- where to call
              -> Id 			-- of target, for type info
              -> [AnnExpr' Id VarSet]	-- args (atoms)
              -> BcM BCInstrList

generateCCall d0 s p ccall_spec@(CCallSpec target cconv safety) fn args_r_to_l
   = let 
         -- useful constants
         addr_sizeW = getPrimRepSize AddrRep

         -- Get the args on the stack, with tags and suitably
         -- dereferenced for the CCall.  For each arg, return the
         -- depth to the first word of the bits for that arg, and the
         -- PrimRep of what was actually pushed.

         pargs d [] = returnBc []
         pargs d (a:az) 
            = let arg_ty = repType (exprType (deAnnotate' a))

              in case splitTyConApp_maybe arg_ty of
                    -- Don't push the FO; instead push the Addr# it
                    -- contains.
		    Just (t, _)
		     | t == arrayPrimTyCon || t == mutableArrayPrimTyCon
                       -> pargs (d + addr_sizeW) az	`thenBc` \ rest ->
                          parg_ArrayishRep arrPtrsHdrSize d p a
							`thenBc` \ code ->
                          returnBc ((code,AddrRep):rest)

		     | t == byteArrayPrimTyCon || t == mutableByteArrayPrimTyCon
                       -> pargs (d + addr_sizeW) az	`thenBc` \ rest ->
                          parg_ArrayishRep arrWordsHdrSize d p a
							`thenBc` \ code ->
                          returnBc ((code,AddrRep):rest)

                    -- Default case: push taggedly, but otherwise intact.
                    other
                       -> pushAtom d p a		`thenBc` \ (code_a, sz_a) ->
                          pargs (d+sz_a) az		`thenBc` \ rest ->
                          returnBc ((code_a, atomRep a) : rest)

         -- Do magic for Ptr/Byte arrays.  Push a ptr to the array on
         -- the stack but then advance it over the headers, so as to
         -- point to the payload.
         parg_ArrayishRep hdrSizeW d p a
            = pushAtom d p a `thenBc` \ (push_fo, _) ->
              -- The ptr points at the header.  Advance it over the
              -- header and then pretend this is an Addr#.
              returnBc (push_fo `snocOL` 
                        SWIZZLE 0 (hdrSizeW * getPrimRepSize WordRep
                                            * wORD_SIZE))

     in
         pargs d0 args_r_to_l			`thenBc` \ code_n_reps ->
     let
         (pushs_arg, a_reps_pushed_r_to_l) = unzip code_n_reps

         push_args    = concatOL pushs_arg
         d_after_args = d0 + sum (map getPrimRepSize a_reps_pushed_r_to_l)
         a_reps_pushed_RAW
            | null a_reps_pushed_r_to_l || head a_reps_pushed_r_to_l /= VoidRep
            = panic "ByteCodeGen.generateCCall: missing or invalid World token?"
            | otherwise
            = reverse (tail a_reps_pushed_r_to_l)

         -- Now: a_reps_pushed_RAW are the reps which are actually on the stack.
         -- push_args is the code to do that.
         -- d_after_args is the stack depth once the args are on.

         -- Get the result rep.
         (returns_void, r_rep)
            = case maybe_getCCallReturnRep (idType fn) of
                 Nothing -> (True,  VoidRep)
                 Just rr -> (False, rr) 
         {-
         Because the Haskell stack grows down, the a_reps refer to 
         lowest to highest addresses in that order.  The args for the call
         are on the stack.  Now push an unboxed Addr# indicating
         the C function to call.  Then push a dummy placeholder for the 
         result.  Finally, emit a CCALL insn with an offset pointing to the 
         Addr# just pushed, and a literal field holding the mallocville
         address of the piece of marshalling code we generate.
         So, just prior to the CCALL insn, the stack looks like this 
         (growing down, as usual):
                 
            <arg_n>
            ...
            <arg_1>
            Addr# address_of_C_fn
            <placeholder-for-result#> (must be an unboxed type)

         The interpreter then calls the marshall code mentioned
         in the CCALL insn, passing it (& <placeholder-for-result#>), 
         that is, the addr of the topmost word in the stack.
         When this returns, the placeholder will have been
         filled in.  The placeholder is slid down to the sequel
         depth, and we RETURN.

         This arrangement makes it simple to do f-i-dynamic since the Addr#
         value is the first arg anyway.

         The marshalling code is generated specifically for this
         call site, and so knows exactly the (Haskell) stack
         offsets of the args, fn address and placeholder.  It
         copies the args to the C stack, calls the stacked addr,
         and parks the result back in the placeholder.  The interpreter
         calls it as a normal C call, assuming it has a signature
            void marshall_code ( StgWord* ptr_to_top_of_stack )
         -}
         -- resolve static address
         get_target_info
            = case target of
                 DynamicTarget
                    -> returnBc (False, panic "ByteCodeGen.generateCCall(dyn)")
                 StaticTarget target
                    -> ioToBc (lookupStaticPtr target) `thenBc` \res ->
                       returnBc (True, res)
     in
         get_target_info	`thenBc` \ (is_static, static_target_addr) ->
     let

         -- Get the arg reps, zapping the leading Addr# in the dynamic case
         a_reps -- | trace (showSDoc (ppr a_reps_pushed_RAW)) False = error "???"
                | is_static = a_reps_pushed_RAW
                | otherwise = if null a_reps_pushed_RAW 
                              then panic "ByteCodeGen.generateCCall: dyn with no args"
                              else tail a_reps_pushed_RAW

         -- push the Addr#
         (push_Addr, d_after_Addr)
            | is_static
            = (toOL [PUSH_UBX (Right static_target_addr) addr_sizeW],
               d_after_args + addr_sizeW)
            | otherwise	-- is already on the stack
            = (nilOL, d_after_args)

         -- Push the return placeholder.  For a call returning nothing,
         -- this is a VoidRep (tag).
         r_sizeW   = getPrimRepSize r_rep
         d_after_r = d_after_Addr + r_sizeW
         r_lit     = mkDummyLiteral r_rep
         push_r    = (if   returns_void 
                      then nilOL 
                      else unitOL (PUSH_UBX (Left r_lit) r_sizeW))

         -- generate the marshalling code we're going to call
         r_offW       = 0 
         addr_offW    = r_sizeW
         arg1_offW    = r_sizeW + addr_sizeW
         args_offW    = map (arg1_offW +) 
                            (init (scanl (+) 0 (map getPrimRepSize a_reps)))
     in
         ioToBc (mkMarshalCode cconv
                    (r_offW, r_rep) addr_offW
                    (zip args_offW a_reps))	`thenBc` \ addr_of_marshaller ->
         recordMallocBc addr_of_marshaller	`thenBc_`
     let
	 -- Offset of the next stack frame down the stack.  The CCALL
 	 -- instruction needs to describe the chunk of stack containing
	 -- the ccall args to the GC, so it needs to know how large it
	 -- is.  See comment in Interpreter.c with the CCALL instruction.
	 stk_offset   = d_after_r - s

         -- do the call
         do_call      = unitOL (CCALL stk_offset (castPtr addr_of_marshaller))
         -- slide and return
         wrapup       = mkSLIDE r_sizeW (d_after_r - r_sizeW - s)
                        `snocOL` RETURN_UBX r_rep
     in
         --trace (show (arg1_offW, args_offW  ,  (map getPrimRepSize a_reps) )) $
         returnBc (
         push_args `appOL`
         push_Addr `appOL` push_r `appOL` do_call `appOL` wrapup
         )


-- Make a dummy literal, to be used as a placeholder for FFI return
-- values on the stack.
mkDummyLiteral :: PrimRep -> Literal
mkDummyLiteral pr
   = case pr of
        CharRep   -> MachChar 0
        IntRep    -> MachInt 0
        WordRep   -> MachWord 0
        DoubleRep -> MachDouble 0
        FloatRep  -> MachFloat 0
        AddrRep   | getPrimRepSize AddrRep == getPrimRepSize WordRep -> MachWord 0
        _         -> moan64 "mkDummyLiteral" (ppr pr)


-- Convert (eg) 
--     GHC.Prim.Char# -> GHC.Prim.State# GHC.Prim.RealWorld
--                   -> (# GHC.Prim.State# GHC.Prim.RealWorld, GHC.Prim.Int# #)
--
-- to  Just IntRep
-- and check that an unboxed pair is returned wherein the first arg is VoidRep'd.
--
-- Alternatively, for call-targets returning nothing, convert
--
--     GHC.Prim.Char# -> GHC.Prim.State# GHC.Prim.RealWorld
--                   -> (# GHC.Prim.State# GHC.Prim.RealWorld #)
--
-- to  Nothing

maybe_getCCallReturnRep :: Type -> Maybe PrimRep
maybe_getCCallReturnRep fn_ty
   = let (a_tys, r_ty) = splitFunTys (dropForAlls fn_ty)
         maybe_r_rep_to_go  
            = if isSingleton r_reps then Nothing else Just (r_reps !! 1)
         (r_tycon, r_reps) 
            = case splitTyConApp_maybe (repType r_ty) of
                      (Just (tyc, tys)) -> (tyc, map typePrimRep tys)
                      Nothing -> blargh
         ok = ( ( r_reps `lengthIs` 2 && VoidRep == head r_reps)
                || r_reps == [VoidRep] )
              && isUnboxedTupleTyCon r_tycon
              && case maybe_r_rep_to_go of
                    Nothing    -> True
                    Just r_rep -> r_rep /= PtrRep
                                  -- if it was, it would be impossible 
                                  -- to create a valid return value 
                                  -- placeholder on the stack
         blargh = pprPanic "maybe_getCCallReturn: can't handle:" 
                           (pprType fn_ty)
     in 
     --trace (showSDoc (ppr (a_reps, r_reps))) $
     if ok then maybe_r_rep_to_go else blargh

-- Compile code which expects an unboxed Int on the top of stack,
-- (call it i), and pushes the i'th closure in the supplied list 
-- as a consequence.
implement_tagToId :: [Name] -> BcM BCInstrList
implement_tagToId names
   = ASSERT( notNull names )
     getLabelsBc (length names)			`thenBc` \ labels ->
     getLabelBc					`thenBc` \ label_fail ->
     getLabelBc 				`thenBc` \ label_exit ->
     zip4 labels (tail labels ++ [label_fail])
                 [0 ..] names			`bind`   \ infos ->
     map (mkStep label_exit) infos		`bind`   \ steps ->
     returnBc (concatOL steps
               `appOL` 
               toOL [LABEL label_fail, CASEFAIL, LABEL label_exit])
     where
        mkStep l_exit (my_label, next_label, n, name_for_n)
           = toOL [LABEL my_label, 
                   TESTEQ_I n next_label, 
                   PUSH_G name_for_n, 
                   JMP l_exit]


-- -----------------------------------------------------------------------------
-- pushAtom

-- Push an atom onto the stack, returning suitable code & number of
-- stack words used.
--
-- The env p must map each variable to the highest- numbered stack
-- slot for it.  For example, if the stack has depth 4 and we
-- tagged-ly push (v :: Int#) on it, the value will be in stack[4],
-- the tag in stack[5], the stack will have depth 6, and p must map v
-- to 5 and not to 4.  Stack locations are numbered from zero, so a
-- depth 6 stack has valid words 0 .. 5.

pushAtom :: Int -> BCEnv -> AnnExpr' Id VarSet -> BcM (BCInstrList, Int)

pushAtom d p (AnnApp f (_, AnnType _))
   = pushAtom d p (snd f)

pushAtom d p (AnnNote note e)
   = pushAtom d p (snd e)

pushAtom d p (AnnLam x e) 
   | isTyVar x 
   = pushAtom d p (snd e)

pushAtom d p (AnnVar v)

   | idPrimRep v == VoidRep
   = returnBc (nilOL, 0)

   | isFCallId v
   = pprPanic "pushAtom: shouldn't get an FCallId here" (ppr v)

   | Just primop <- isPrimOpId_maybe v
   = returnBc (unitOL (PUSH_PRIMOP primop), 1)

   | Just d_v <- lookupBCEnv_maybe p v  -- v is a local variable
   = returnBc (toOL (nOfThem sz (PUSH_L (d-d_v+sz-2))), sz)
	 -- d - d_v 		    the number of words between the TOS 
	 --			    and the 1st slot of the object
	 --
	 -- d - d_v - 1 	    the offset from the TOS of the 1st slot
	 --
	 -- d - d_v - 1 + sz - 1    the offset from the TOS of the last slot
	 --			    of the object.
	 --
	 -- Having found the last slot, we proceed to copy the right number of
	 -- slots on to the top of the stack.

    | otherwise  -- v must be a global variable
    = ASSERT(sz == 1) 
      returnBc (unitOL (PUSH_G (getName v)), sz)

    where
         sz = idSizeW v


pushAtom d p (AnnLit lit)
   = case lit of
        MachLabel fs _ -> code CodePtrRep
        MachWord w     -> code WordRep
        MachInt i      -> code IntRep
        MachFloat r    -> code FloatRep
        MachDouble r   -> code DoubleRep
        MachChar c     -> code CharRep
        MachStr s      -> pushStr s
     where
        code rep
           = let size_host_words = getPrimRepSize rep
             in  returnBc (unitOL (PUSH_UBX (Left lit) size_host_words), 
                           size_host_words)

        pushStr s 
           = let getMallocvilleAddr
                    = case s of
                         FastString _ l ba -> 
                            -- sigh, a string in the heap is no good to us.
                            -- We need a static C pointer, since the type of 
                            -- a string literal is Addr#.  So, copy the string 
                            -- into C land and remember the pointer so we can
			    -- free it later.
                            let n = I# l
                            -- CAREFUL!  Chars are 32 bits in ghc 4.09+
                            in  ioToBc (mallocBytes (n+1)) `thenBc` \ ptr ->
                                recordMallocBc ptr         `thenBc_`
                                ioToBc (
                                   do memcpy ptr ba (fromIntegral n)
				      pokeByteOff ptr n (fromIntegral (ord '\0') :: Word8)
                                      return ptr
                                   )
                         other -> panic "ByteCodeGen.pushAtom.pushStr"
             in
                getMallocvilleAddr `thenBc` \ addr ->
                -- Get the addr on the stack, untaggedly
                   returnBc (unitOL (PUSH_UBX (Right addr) 1), 1)

pushAtom d p other
   = pprPanic "ByteCodeGen.pushAtom" 
              (pprCoreExpr (deAnnotate (undefined, other)))

foreign import ccall unsafe "memcpy"
 memcpy :: Ptr a -> ByteArray# -> CInt -> IO ()


-- -----------------------------------------------------------------------------
-- Given a bunch of alts code and their discrs, do the donkey work
-- of making a multiway branch using a switch tree.
-- What a load of hassle!

mkMultiBranch :: Maybe Int	-- # datacons in tycon, if alg alt
				-- a hint; generates better code
				-- Nothing is always safe
              -> [(Discr, BCInstrList)] 
              -> BcM BCInstrList
mkMultiBranch maybe_ncons raw_ways
   = let d_way     = filter (isNoDiscr.fst) raw_ways
         notd_ways = naturalMergeSortLe 
                        (\w1 w2 -> leAlt (fst w1) (fst w2))
                        (filter (not.isNoDiscr.fst) raw_ways)

         mkTree :: [(Discr, BCInstrList)] -> Discr -> Discr -> BcM BCInstrList
         mkTree [] range_lo range_hi = returnBc the_default

         mkTree [val] range_lo range_hi
            | range_lo `eqAlt` range_hi 
            = returnBc (snd val)
            | otherwise
            = getLabelBc 				`thenBc` \ label_neq ->
              returnBc (mkTestEQ (fst val) label_neq 
			`consOL` (snd val
			`appOL`   unitOL (LABEL label_neq)
			`appOL`   the_default))

         mkTree vals range_lo range_hi
            = let n = length vals `div` 2
                  vals_lo = take n vals
                  vals_hi = drop n vals
                  v_mid = fst (head vals_hi)
              in
              getLabelBc 				`thenBc` \ label_geq ->
              mkTree vals_lo range_lo (dec v_mid) 	`thenBc` \ code_lo ->
              mkTree vals_hi v_mid range_hi 		`thenBc` \ code_hi ->
              returnBc (mkTestLT v_mid label_geq
                        `consOL` (code_lo
			`appOL`   unitOL (LABEL label_geq)
			`appOL`   code_hi))
 
         the_default 
            = case d_way of [] -> unitOL CASEFAIL
                            [(_, def)] -> def

         -- None of these will be needed if there are no non-default alts
         (mkTestLT, mkTestEQ, init_lo, init_hi)
            | null notd_ways
            = panic "mkMultiBranch: awesome foursome"
            | otherwise
            = case fst (head notd_ways) of {
              DiscrI _ -> ( \(DiscrI i) fail_label -> TESTLT_I i fail_label,
                            \(DiscrI i) fail_label -> TESTEQ_I i fail_label,
                            DiscrI minBound,
                            DiscrI maxBound );
              DiscrF _ -> ( \(DiscrF f) fail_label -> TESTLT_F f fail_label,
                            \(DiscrF f) fail_label -> TESTEQ_F f fail_label,
                            DiscrF minF,
                            DiscrF maxF );
              DiscrD _ -> ( \(DiscrD d) fail_label -> TESTLT_D d fail_label,
                            \(DiscrD d) fail_label -> TESTEQ_D d fail_label,
                            DiscrD minD,
                            DiscrD maxD );
              DiscrP _ -> ( \(DiscrP i) fail_label -> TESTLT_P i fail_label,
                            \(DiscrP i) fail_label -> TESTEQ_P i fail_label,
                            DiscrP algMinBound,
                            DiscrP algMaxBound )
              }

         (algMinBound, algMaxBound)
            = case maybe_ncons of
                 Just n  -> (0, n - 1)
                 Nothing -> (minBound, maxBound)

         (DiscrI i1) `eqAlt` (DiscrI i2) = i1 == i2
         (DiscrF f1) `eqAlt` (DiscrF f2) = f1 == f2
         (DiscrD d1) `eqAlt` (DiscrD d2) = d1 == d2
         (DiscrP i1) `eqAlt` (DiscrP i2) = i1 == i2
         NoDiscr     `eqAlt` NoDiscr     = True
         _           `eqAlt` _           = False

         (DiscrI i1) `leAlt` (DiscrI i2) = i1 <= i2
         (DiscrF f1) `leAlt` (DiscrF f2) = f1 <= f2
         (DiscrD d1) `leAlt` (DiscrD d2) = d1 <= d2
         (DiscrP i1) `leAlt` (DiscrP i2) = i1 <= i2
         NoDiscr     `leAlt` NoDiscr     = True
         _           `leAlt` _           = False

         isNoDiscr NoDiscr = True
         isNoDiscr _       = False

         dec (DiscrI i) = DiscrI (i-1)
         dec (DiscrP i) = DiscrP (i-1)
         dec other      = other		-- not really right, but if you
		-- do cases on floating values, you'll get what you deserve

         -- same snotty comment applies to the following
         minF, maxF :: Float
         minD, maxD :: Double
         minF = -1.0e37
         maxF =  1.0e37
         minD = -1.0e308
         maxD =  1.0e308
     in
         mkTree notd_ways init_lo init_hi


-- -----------------------------------------------------------------------------
-- Supporting junk for the compilation schemes

-- Describes case alts
data Discr 
   = DiscrI Int
   | DiscrF Float
   | DiscrD Double
   | DiscrP Int
   | NoDiscr

instance Outputable Discr where
   ppr (DiscrI i) = int i
   ppr (DiscrF f) = text (show f)
   ppr (DiscrD d) = text (show d)
   ppr (DiscrP i) = int i
   ppr NoDiscr    = text "DEF"


lookupBCEnv_maybe :: BCEnv -> Id -> Maybe Int
lookupBCEnv_maybe = lookupFM

idSizeW :: Id -> Int
idSizeW id = getPrimRepSize (typePrimRep (idType id))

unboxedTupleException :: a
unboxedTupleException 
   = throwDyn 
        (Panic 
           ("Bytecode generator can't handle unboxed tuples.  Possibly due\n" ++
            "\tto foreign import/export decls in source.  Workaround:\n" ++
            "\tcompile this module to a .o file, then restart session."))


mkSLIDE n d = if d == 0 then nilOL else unitOL (SLIDE n d)
bind x f    = f x

splitApp :: AnnExpr' id ann -> (AnnExpr' id ann, [AnnExpr' id ann])
	-- The arguments are returned in *right-to-left* order
splitApp (AnnApp (_,f) (_,a))
	       | isTypeAtom a = splitApp f
	       | otherwise    = case splitApp f of 
				     (f', as) -> (f', a:as)
splitApp (AnnNote n (_,e))    = splitApp e
splitApp e		      = (e, [])


isTypeAtom :: AnnExpr' id ann -> Bool
isTypeAtom (AnnType _) = True
isTypeAtom _           = False

isVoidRepAtom :: AnnExpr' id ann -> Bool
isVoidRepAtom (AnnVar v)        = typePrimRep (idType v) == VoidRep
isVoidRepAtom (AnnNote n (_,e)) = isVoidRepAtom e
isVoidRepAtom _ 	        = False

atomRep :: AnnExpr' Id ann -> PrimRep
atomRep (AnnVar v)    = typePrimRep (idType v)
atomRep (AnnLit l)    = literalPrimRep l
atomRep (AnnNote n b) = atomRep (snd b)
atomRep (AnnApp f (_, AnnType _)) = atomRep (snd f)
atomRep (AnnLam x e) | isTyVar x = atomRep (snd e)
atomRep other = pprPanic "atomRep" (ppr (deAnnotate (undefined,other)))

isPtrAtom :: AnnExpr' Id ann -> Bool
isPtrAtom e = isFollowableRep (atomRep e)

-- Let szsw be the sizes in words of some items pushed onto the stack,
-- which has initial depth d'.  Return the values which the stack environment
-- should map these items to.
mkStackOffsets :: Int -> [Int] -> [Int]
mkStackOffsets original_depth szsw
   = map (subtract 1) (tail (scanl (+) original_depth szsw))

-- -----------------------------------------------------------------------------
-- The bytecode generator's monad

data BcM_State 
   = BcM_State { 
	nextlabel :: Int,		-- for generating local labels
	malloced  :: [Ptr ()] }		-- ptrs malloced for current BCO
					-- Should be free()d when it is GCd

newtype BcM r = BcM (BcM_State -> IO (BcM_State, r))

ioToBc :: IO a -> BcM a
ioToBc io = BcM $ \st -> do 
  x <- io 
  return (st, x)

runBc :: BcM r -> IO (BcM_State, r)
runBc (BcM m) = m (BcM_State 0 []) 

thenBc :: BcM a -> (a -> BcM b) -> BcM b
thenBc (BcM expr) cont = BcM $ \st0 -> do
  (st1, q) <- expr st0
  let BcM k = cont q 
  (st2, r) <- k st1
  return (st2, r)

thenBc_ :: BcM a -> BcM b -> BcM b
thenBc_ (BcM expr) (BcM cont) = BcM $ \st0 -> do
  (st1, q) <- expr st0
  (st2, r) <- cont st1
  return (st2, r)

returnBc :: a -> BcM a
returnBc result = BcM $ \st -> (return (st, result))

instance Monad BcM where
  (>>=) = thenBc
  (>>)  = thenBc_
  return = returnBc

emitBc :: ([Ptr ()] -> ProtoBCO Name) -> BcM (ProtoBCO Name)
emitBc bco
  = BcM $ \st -> return (st{malloced=[]}, bco (malloced st))

recordMallocBc :: Ptr a -> BcM ()
recordMallocBc a
  = BcM $ \st -> return (st{malloced = castPtr a : malloced st}, ())

getLabelBc :: BcM Int
getLabelBc
  = BcM $ \st -> return (st{nextlabel = 1 + nextlabel st}, nextlabel st)

getLabelsBc :: Int -> BcM [Int]
getLabelsBc n
  = BcM $ \st -> let ctr = nextlabel st 
		 in return (st{nextlabel = ctr+n}, [ctr .. ctr+n-1])
\end{code}
