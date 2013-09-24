%
% (c) The GRASP/AQUA Project, Glasgow University, 1998
%
\section[Type]{Type - public interface}

\begin{code}
module Type (
        -- re-exports from TypeRep:
	Type, PredType, ThetaType,
	Kind, TyVarSubst, 

	superKind, superBoxity,				-- KX and BX respectively
	liftedBoxity, unliftedBoxity, 			-- :: BX
	openKindCon, 					-- :: KX
	typeCon,					-- :: BX -> KX
	liftedTypeKind, unliftedTypeKind, openTypeKind,	-- :: KX
	mkArrowKind, mkArrowKinds,			-- :: KX -> KX -> KX
	isTypeKind, isAnyTypeKind,
	funTyCon,

        -- exports from this module:
        hasMoreBoxityInfo, defaultKind,

	mkTyVarTy, mkTyVarTys, getTyVar, getTyVar_maybe, isTyVarTy,

	mkAppTy, mkAppTys, splitAppTy, splitAppTys, splitAppTy_maybe,

	mkFunTy, mkFunTys, splitFunTy, splitFunTy_maybe, splitFunTys, 
	funResultTy, funArgTy, zipFunTys, isFunTy,

	mkGenTyConApp, mkTyConApp, mkTyConTy, 
	tyConAppTyCon, tyConAppArgs, 
	splitTyConApp_maybe, splitTyConApp,

	mkSynTy, 

	repType, typePrimRep,

	mkForAllTy, mkForAllTys, splitForAllTy_maybe, splitForAllTys, 
	applyTy, applyTys, isForAllTy, dropForAlls,

	-- Source types
	SourceType(..), sourceTypeRep, mkPredTy, mkPredTys,

	-- Newtypes
	splitNewType_maybe,

	-- Lifting and boxity
	isUnLiftedType, isUnboxedTupleType, isAlgType, isStrictType, isPrimitiveType,

	-- Free variables
	tyVarsOfType, tyVarsOfTypes, tyVarsOfPred, tyVarsOfTheta,
	typeKind, addFreeTyVars,

	-- Tidying up for printing
	tidyType,      tidyTypes,
	tidyOpenType,  tidyOpenTypes,
	tidyTyVarBndr, tidyFreeTyVars,
	tidyOpenTyVar, tidyOpenTyVars,
	tidyTopType,   tidyPred,

	-- Comparison
	eqType, eqKind, 

	-- Seq
	seqType, seqTypes

    ) where

#include "HsVersions.h"

-- We import the representation and primitive functions from TypeRep.
-- Many things are reexported, but not the representation!

import TypeRep

-- Other imports:

import {-# SOURCE #-}	PprType( pprType )	-- Only called in debug messages
import {-# SOURCE #-}   Subst  ( substTyWith )

-- friends:
import Var	( Id, TyVar, tyVarKind, tyVarName, setTyVarName )
import VarEnv
import VarSet

import Name	( NamedThing(..), mkInternalName, tidyOccName )
import Class	( Class, classTyCon )
import TyCon	( TyCon, isRecursiveTyCon, isPrimTyCon,
		  isUnboxedTupleTyCon, isUnLiftedTyCon,
		  isFunTyCon, isNewTyCon, newTyConRep,
		  isAlgTyCon, isSynTyCon, tyConArity, 
	          tyConKind, getSynTyConDefn,
		  tyConPrimRep, 
		)

-- others
import CmdLineOpts	( opt_DictsStrict )
import SrcLoc		( noSrcLoc )
import PrimRep		( PrimRep(..) )
import Unique		( Uniquable(..) )
import Util		( mapAccumL, seqList, lengthIs, snocView )
import Outputable
import UniqSet		( sizeUniqSet )		-- Should come via VarSet
import Maybe		( isJust )
\end{code}


%************************************************************************
%*									*
\subsection{Stuff to do with kinds.}
%*									*
%************************************************************************

\begin{code}
hasMoreBoxityInfo :: Kind -> Kind -> Bool
-- (k1 `hasMoreBoxityInfo` k2) checks that k1 <: k2
hasMoreBoxityInfo k1 k2
  | k2 `eqKind` openTypeKind = isAnyTypeKind k1
  | otherwise	  	     = k1 `eqKind` k2
  where

isAnyTypeKind :: Kind -> Bool
-- True of kind * and *# and ?
isAnyTypeKind (TyConApp tc _) = tc == typeCon || tc == openKindCon
isAnyTypeKind (NoteTy _ k)    = isAnyTypeKind k
isAnyTypeKind other	      = False

isTypeKind :: Kind -> Bool
-- True of kind * and *#
isTypeKind (TyConApp tc _) = tc == typeCon
isTypeKind (NoteTy _ k)    = isTypeKind k
isTypeKind other	   = False

defaultKind :: Kind -> Kind
-- Used when generalising: default kind '?' to '*'
defaultKind kind | kind `eqKind` openTypeKind = liftedTypeKind
	         | otherwise	 	      = kind
\end{code}


%************************************************************************
%*									*
\subsection{Constructor-specific functions}
%*									*
%************************************************************************


---------------------------------------------------------------------
				TyVarTy
				~~~~~~~
\begin{code}
mkTyVarTy  :: TyVar   -> Type
mkTyVarTy  = TyVarTy

mkTyVarTys :: [TyVar] -> [Type]
mkTyVarTys = map mkTyVarTy -- a common use of mkTyVarTy

getTyVar :: String -> Type -> TyVar
getTyVar msg (TyVarTy tv)     = tv
getTyVar msg (SourceTy p)     = getTyVar msg (sourceTypeRep p)
getTyVar msg (NoteTy _ t)     = getTyVar msg t
getTyVar msg other	      = panic ("getTyVar: " ++ msg)

getTyVar_maybe :: Type -> Maybe TyVar
getTyVar_maybe (TyVarTy tv) 	= Just tv
getTyVar_maybe (NoteTy _ t) 	= getTyVar_maybe t
getTyVar_maybe (SourceTy p) 	= getTyVar_maybe (sourceTypeRep p)
getTyVar_maybe other	        = Nothing

isTyVarTy :: Type -> Bool
isTyVarTy (TyVarTy tv)     = True
isTyVarTy (NoteTy _ ty)    = isTyVarTy ty
isTyVarTy (SourceTy p)     = isTyVarTy (sourceTypeRep p)
isTyVarTy other            = False
\end{code}


---------------------------------------------------------------------
				AppTy
				~~~~~
We need to be pretty careful with AppTy to make sure we obey the 
invariant that a TyConApp is always visibly so.  mkAppTy maintains the
invariant: use it.

\begin{code}
mkAppTy orig_ty1 orig_ty2
  = ASSERT( not (isSourceTy orig_ty1) )	-- Source types are of kind *
    mk_app orig_ty1
  where
    mk_app (NoteTy _ ty1)    = mk_app ty1
    mk_app (TyConApp tc tys) = mkGenTyConApp tc (tys ++ [orig_ty2])
    mk_app ty1		     = AppTy orig_ty1 orig_ty2
	-- We call mkGenTyConApp because the TyConApp could be an 
	-- under-saturated type synonym.  GHC allows that; e.g.
	--	type Foo k = k a -> k a
	--	type Id x = x
	--	foo :: Foo Id -> Foo Id
	--
	-- Here Id is partially applied in the type sig for Foo,
	-- but once the type synonyms are expanded all is well

mkAppTys :: Type -> [Type] -> Type
mkAppTys orig_ty1 []	    = orig_ty1
	-- This check for an empty list of type arguments
	-- avoids the needless loss of a type synonym constructor.
	-- For example: mkAppTys Rational []
	--   returns to (Ratio Integer), which has needlessly lost
	--   the Rational part.
mkAppTys orig_ty1 orig_tys2
  = ASSERT( not (isSourceTy orig_ty1) )	-- Source types are of kind *
    mk_app orig_ty1
  where
    mk_app (NoteTy _ ty1)    = mk_app ty1
    mk_app (TyConApp tc tys) = mkTyConApp tc (tys ++ orig_tys2)
    mk_app ty1		     = foldl AppTy orig_ty1 orig_tys2

splitAppTy_maybe :: Type -> Maybe (Type, Type)
splitAppTy_maybe (FunTy ty1 ty2)   = Just (TyConApp funTyCon [ty1], ty2)
splitAppTy_maybe (AppTy ty1 ty2)   = Just (ty1, ty2)
splitAppTy_maybe (NoteTy _ ty)     = splitAppTy_maybe ty
splitAppTy_maybe (SourceTy p)      = splitAppTy_maybe (sourceTypeRep p)
splitAppTy_maybe (TyConApp tc tys) = case snocView tys of
					Nothing -> Nothing
					Just (tys',ty') -> Just (TyConApp tc tys', ty')
splitAppTy_maybe other	     	   = Nothing

splitAppTy :: Type -> (Type, Type)
splitAppTy ty = case splitAppTy_maybe ty of
			Just pr -> pr
			Nothing -> panic "splitAppTy"

splitAppTys :: Type -> (Type, [Type])
splitAppTys ty = split ty ty []
  where
    split orig_ty (AppTy ty arg)        args = split ty ty (arg:args)
    split orig_ty (NoteTy _ ty)         args = split orig_ty ty args
    split orig_ty (SourceTy p)          args = split orig_ty (sourceTypeRep p) args
    split orig_ty (FunTy ty1 ty2)       args = ASSERT( null args )
					       (TyConApp funTyCon [], [ty1,ty2])
    split orig_ty (TyConApp tc tc_args) args = (TyConApp tc [], tc_args ++ args)
    split orig_ty ty		        args = (orig_ty, args)
\end{code}


---------------------------------------------------------------------
				FunTy
				~~~~~

\begin{code}
mkFunTy :: Type -> Type -> Type
mkFunTy arg res = FunTy arg res

mkFunTys :: [Type] -> Type -> Type
mkFunTys tys ty = foldr FunTy ty tys

isFunTy :: Type -> Bool 
isFunTy ty = isJust (splitFunTy_maybe ty)

splitFunTy :: Type -> (Type, Type)
splitFunTy (FunTy arg res) = (arg, res)
splitFunTy (NoteTy _ ty)   = splitFunTy ty
splitFunTy (SourceTy p)    = splitFunTy (sourceTypeRep p)

splitFunTy_maybe :: Type -> Maybe (Type, Type)
splitFunTy_maybe (FunTy arg res) = Just (arg, res)
splitFunTy_maybe (NoteTy _ ty)   = splitFunTy_maybe ty
splitFunTy_maybe (SourceTy p)    = splitFunTy_maybe (sourceTypeRep p)
splitFunTy_maybe other	         = Nothing

splitFunTys :: Type -> ([Type], Type)
splitFunTys ty = split [] ty ty
  where
    split args orig_ty (FunTy arg res) = split (arg:args) res res
    split args orig_ty (NoteTy _ ty)   = split args orig_ty ty
    split args orig_ty (SourceTy p)    = split args orig_ty (sourceTypeRep p)
    split args orig_ty ty              = (reverse args, orig_ty)

zipFunTys :: Outputable a => [a] -> Type -> ([(a,Type)], Type)
zipFunTys orig_xs orig_ty = split [] orig_xs orig_ty orig_ty
  where
    split acc []     nty ty  	         = (reverse acc, nty)
    split acc (x:xs) nty (FunTy arg res) = split ((x,arg):acc) xs res res
    split acc xs     nty (NoteTy _ ty)   = split acc           xs nty ty
    split acc xs     nty (SourceTy p)    = split acc           xs nty (sourceTypeRep p)
    split acc (x:xs) nty ty              = pprPanic "zipFunTys" (ppr orig_xs <+> pprType orig_ty)
    
funResultTy :: Type -> Type
funResultTy (FunTy arg res) = res
funResultTy (NoteTy _ ty)   = funResultTy ty
funResultTy (SourceTy p)    = funResultTy (sourceTypeRep p)
funResultTy ty		    = pprPanic "funResultTy" (pprType ty)

funArgTy :: Type -> Type
funArgTy (FunTy arg res) = arg
funArgTy (NoteTy _ ty)   = funArgTy ty
funArgTy (SourceTy p)    = funArgTy (sourceTypeRep p)
funArgTy ty		 = pprPanic "funArgTy" (pprType ty)
\end{code}


---------------------------------------------------------------------
				TyConApp
				~~~~~~~~
@mkTyConApp@ is a key function, because it builds a TyConApp, FunTy or SourceTy,
as apppropriate.

\begin{code}
mkGenTyConApp :: TyCon -> [Type] -> Type
mkGenTyConApp tc tys
  | isSynTyCon tc = mkSynTy tc tys
  | otherwise     = mkTyConApp tc tys

mkTyConApp :: TyCon -> [Type] -> Type
-- Assumes TyCon is not a SynTyCon; use mkSynTy instead for those
mkTyConApp tycon tys
  | isFunTyCon tycon, [ty1,ty2] <- tys
  = FunTy ty1 ty2

  | isNewTyCon tycon,			-- A saturated newtype application;
    not (isRecursiveTyCon tycon),	-- Not recursive (we don't use SourceTypes for them)
    tys `lengthIs` tyConArity tycon     -- use the SourceType form
  = SourceTy (NType tycon tys)

  | otherwise
  = ASSERT(not (isSynTyCon tycon))
    TyConApp tycon tys

mkTyConTy :: TyCon -> Type
mkTyConTy tycon = ASSERT( not (isSynTyCon tycon) ) 
		  TyConApp tycon []

-- splitTyConApp "looks through" synonyms, because they don't
-- mean a distinct type, but all other type-constructor applications
-- including functions are returned as Just ..

tyConAppTyCon :: Type -> TyCon
tyConAppTyCon ty = fst (splitTyConApp ty)

tyConAppArgs :: Type -> [Type]
tyConAppArgs ty = snd (splitTyConApp ty)

splitTyConApp :: Type -> (TyCon, [Type])
splitTyConApp ty = case splitTyConApp_maybe ty of
			Just stuff -> stuff
			Nothing	   -> pprPanic "splitTyConApp" (pprType ty)

splitTyConApp_maybe :: Type -> Maybe (TyCon, [Type])
splitTyConApp_maybe (TyConApp tc tys) = Just (tc, tys)
splitTyConApp_maybe (FunTy arg res)   = Just (funTyCon, [arg,res])
splitTyConApp_maybe (NoteTy _ ty)     = splitTyConApp_maybe ty
splitTyConApp_maybe (SourceTy p)      = splitTyConApp_maybe (sourceTypeRep p)
splitTyConApp_maybe other	      = Nothing
\end{code}


---------------------------------------------------------------------
				SynTy
				~~~~~

\begin{code}
mkSynTy tycon tys
  | n_args == arity	-- Exactly saturated
  = mk_syn tys
  | n_args >  arity	-- Over-saturated
  = case splitAt arity tys of { (as,bs) -> mkAppTys (mk_syn as) bs }
	-- Its important to use mkAppTys, rather than (foldl AppTy),
	-- because (mk_syn as) might well return a partially-applied
	-- type constructor; indeed, usually will!
  | otherwise		-- Un-saturated
  = TyConApp tycon tys
	-- For the un-saturated case we build TyConApp directly
	-- (mkTyConApp ASSERTs that the tc isn't a SynTyCon).
	-- Here we are relying on checkValidType to find
	-- the error.  What we can't do is use mkSynTy with
	-- too few arg tys, because that is utterly bogus.

  where
    mk_syn tys = NoteTy (SynNote (TyConApp tycon tys))
			(substTyWith tyvars tys body)

    (tyvars, body) = ASSERT( isSynTyCon tycon ) getSynTyConDefn tycon
    arity 	   = tyConArity tycon
    n_args	   = length tys
\end{code}

Notes on type synonyms
~~~~~~~~~~~~~~~~~~~~~~
The various "split" functions (splitFunTy, splitRhoTy, splitForAllTy) try
to return type synonyms whereever possible. Thus

	type Foo a = a -> a

we want 
	splitFunTys (a -> Foo a) = ([a], Foo a)
not			           ([a], a -> a)

The reason is that we then get better (shorter) type signatures in 
interfaces.  Notably this plays a role in tcTySigs in TcBinds.lhs.


		Representation types
		~~~~~~~~~~~~~~~~~~~~
repType looks through 
	(a) for-alls, and
	(b) synonyms
	(c) predicates
	(d) usage annotations
	(e) [recursive] newtypes
It's useful in the back end.

Remember, non-recursive newtypes get expanded as part of the SourceTy case,
but recursive ones are represented by TyConApps and have to be expanded
by steam.

\begin{code}
repType :: Type -> Type
repType (ForAllTy _ ty)   = repType ty
repType (NoteTy   _ ty)   = repType ty
repType (SourceTy  p)     = repType (sourceTypeRep p)
repType (TyConApp tc tys) | isNewTyCon tc && tys `lengthIs` tyConArity tc
			  = repType (newTypeRep tc tys)
repType ty	 	  = ty


typePrimRep :: Type -> PrimRep
typePrimRep ty = case repType ty of
		   TyConApp tc _ -> tyConPrimRep tc
		   FunTy _ _	 -> PtrRep
		   AppTy _ _	 -> PtrRep	-- ??
		   TyVarTy _	 -> PtrRep
\end{code}



---------------------------------------------------------------------
				ForAllTy
				~~~~~~~~

\begin{code}
mkForAllTy :: TyVar -> Type -> Type
mkForAllTy tyvar ty
  = mkForAllTys [tyvar] ty

mkForAllTys :: [TyVar] -> Type -> Type
mkForAllTys tyvars ty = foldr ForAllTy ty tyvars

isForAllTy :: Type -> Bool
isForAllTy (NoteTy _ ty)  = isForAllTy ty
isForAllTy (ForAllTy _ _) = True
isForAllTy other_ty	  = False

splitForAllTy_maybe :: Type -> Maybe (TyVar, Type)
splitForAllTy_maybe ty = splitFAT_m ty
  where
    splitFAT_m (NoteTy _ ty)		= splitFAT_m ty
    splitFAT_m (SourceTy p)		= splitFAT_m (sourceTypeRep p)
    splitFAT_m (ForAllTy tyvar ty)	= Just(tyvar, ty)
    splitFAT_m _			= Nothing

splitForAllTys :: Type -> ([TyVar], Type)
splitForAllTys ty = split ty ty []
   where
     split orig_ty (ForAllTy tv ty)	  tvs = split ty ty (tv:tvs)
     split orig_ty (NoteTy _ ty)	  tvs = split orig_ty ty tvs
     split orig_ty (SourceTy p)		  tvs = split orig_ty (sourceTypeRep p) tvs
     split orig_ty t			  tvs = (reverse tvs, orig_ty)

dropForAlls :: Type -> Type
dropForAlls ty = snd (splitForAllTys ty)
\end{code}

-- (mkPiType now in CoreUtils)

applyTy, applyTys
~~~~~~~~~~~~~~~~~
Instantiate a for-all type with one or more type arguments.
Used when we have a polymorphic function applied to type args:
	f t1 t2
Then we use (applyTys type-of-f [t1,t2]) to compute the type of
the expression. 

\begin{code}
applyTy :: Type -> Type -> Type
applyTy (SourceTy p) 	 arg = applyTy (sourceTypeRep p) arg
applyTy (NoteTy _ fun)   arg = applyTy fun arg
applyTy (ForAllTy tv ty) arg = substTyWith [tv] [arg] ty
applyTy other		 arg = panic "applyTy"

applyTys :: Type -> [Type] -> Type
-- This function is interesting because 
--	a) the function may have more for-alls than there are args
--	b) less obviously, it may have fewer for-alls
-- For case (b) think of 
--	applyTys (forall a.a) [forall b.b, Int]
-- This really can happen, via dressing up polymorphic types with newtype
-- clothing.  Here's an example:
--	newtype R = R (forall a. a->a)
--	foo = case undefined :: R of
--		R f -> f ()

applyTys orig_fun_ty []      = orig_fun_ty
applyTys orig_fun_ty arg_tys 
  | n_tvs == n_args 	-- The vastly common case
  = substTyWith tvs arg_tys rho_ty
  | n_tvs > n_args 	-- Too many for-alls
  = substTyWith (take n_args tvs) arg_tys 
		(mkForAllTys (drop n_args tvs) rho_ty)
  | otherwise		-- Too many type args
  = ASSERT2( n_tvs > 0, pprType orig_fun_ty )	-- Zero case gives infnite loop!
    applyTys (substTyWith tvs (take n_tvs arg_tys) rho_ty)
	     (drop n_tvs arg_tys)
  where
    (tvs, rho_ty) = splitForAllTys orig_fun_ty 
    n_tvs = length tvs
    n_args = length arg_tys     
\end{code}


%************************************************************************
%*									*
\subsection{Source types}
%*									*
%************************************************************************

A "source type" is a type that is a separate type as far as the type checker is
concerned, but which has low-level representation as far as the back end is concerned.

Source types are always lifted.

The key function is sourceTypeRep which gives the representation of a source type:

\begin{code}
mkPredTy :: PredType -> Type
mkPredTy pred = SourceTy pred

mkPredTys :: ThetaType -> [Type]
mkPredTys preds = map SourceTy preds

sourceTypeRep :: SourceType -> Type
-- Convert a predicate to its "representation type";
-- the type of evidence for that predicate, which is actually passed at runtime
sourceTypeRep (IParam _ ty)     = ty
sourceTypeRep (ClassP clas tys) = mkTyConApp (classTyCon clas) tys
	-- Note the mkTyConApp; the classTyCon might be a newtype!
sourceTypeRep (NType  tc tys)   = newTypeRep tc tys
	-- ToDo: Consider caching this substitution in a NType

isSourceTy :: Type -> Bool
isSourceTy (NoteTy _ ty)  = isSourceTy ty
isSourceTy (SourceTy sty) = True
isSourceTy _	          = False


splitNewType_maybe :: Type -> Maybe Type
-- Newtypes that are recursive are reprsented by TyConApp, just
-- as they always were.  Occasionally we want to find their representation type.
-- NB: remember that in this module, non-recursive newtypes are transparent

splitNewType_maybe ty
  = case splitTyConApp_maybe ty of
	Just (tc,tys) | isNewTyCon tc -> ASSERT( tys `lengthIs` tyConArity tc )
						-- The assert should hold because repType should
						-- only be applied to *types* (of kind *)
					 Just (newTypeRep tc tys)
	other -> Nothing
			
-- A local helper function (not exported)
newTypeRep new_tycon tys = case newTyConRep new_tycon of
			     (tvs, rep_ty) -> substTyWith tvs tys rep_ty
\end{code}


%************************************************************************
%*									*
\subsection{Kinds and free variables}
%*									*
%************************************************************************

---------------------------------------------------------------------
		Finding the kind of a type
		~~~~~~~~~~~~~~~~~~~~~~~~~~
\begin{code}
typeKind :: Type -> Kind

typeKind (TyVarTy tyvar)	= tyVarKind tyvar
typeKind (TyConApp tycon tys)	= foldr (\_ k -> funResultTy k) (tyConKind tycon) tys
typeKind (NoteTy _ ty)		= typeKind ty
typeKind (SourceTy _)		= liftedTypeKind -- Predicates are always 
						 -- represented by lifted types
typeKind (AppTy fun arg)	= funResultTy (typeKind fun)

typeKind (FunTy arg res)	= fix_up (typeKind res)
				where
				  fix_up (TyConApp tycon _) |  tycon == typeCon
							    || tycon == openKindCon = liftedTypeKind
				  fix_up (NoteTy _ kind) = fix_up kind
				  fix_up kind	         = kind
		-- The basic story is 
		-- 	typeKind (FunTy arg res) = typeKind res
		-- But a function is lifted regardless of its result type
		-- Hence the strange fix-up.
		-- Note that 'res', being the result of a FunTy, can't have 
		-- a strange kind like (*->*).

typeKind (ForAllTy tv ty)	= typeKind ty
\end{code}


---------------------------------------------------------------------
		Free variables of a type
		~~~~~~~~~~~~~~~~~~~~~~~~
\begin{code}
tyVarsOfType :: Type -> TyVarSet
tyVarsOfType (TyVarTy tv)		= unitVarSet tv
tyVarsOfType (TyConApp tycon tys)	= tyVarsOfTypes tys
tyVarsOfType (NoteTy (FTVNote tvs) ty2) = tvs
tyVarsOfType (NoteTy (SynNote ty1) ty2)	= tyVarsOfType ty2	-- See note [Syn] below
tyVarsOfType (SourceTy sty)		= tyVarsOfSourceType sty
tyVarsOfType (FunTy arg res)		= tyVarsOfType arg `unionVarSet` tyVarsOfType res
tyVarsOfType (AppTy fun arg)		= tyVarsOfType fun `unionVarSet` tyVarsOfType arg
tyVarsOfType (ForAllTy tyvar ty)	= tyVarsOfType ty `minusVarSet` unitVarSet tyvar

-- 			Note [Syn]
-- Consider
--	type T a = Int
-- What are the free tyvars of (T x)?  Empty, of course!  
-- Here's the example that Ralf Laemmel showed me:
--	foo :: (forall a. C u a -> C u a) -> u
--	mappend :: Monoid u => u -> u -> u
--
--	bar :: Monoid u => u
--	bar = foo (\t -> t `mappend` t)
-- We have to generalise at the arg to f, and we don't
-- want to capture the constraint (Monad (C u a)) because
-- it appears to mention a.  Pretty silly, but it was useful to him.


tyVarsOfTypes :: [Type] -> TyVarSet
tyVarsOfTypes tys = foldr (unionVarSet.tyVarsOfType) emptyVarSet tys

tyVarsOfPred :: PredType -> TyVarSet
tyVarsOfPred = tyVarsOfSourceType	-- Just a subtype

tyVarsOfSourceType :: SourceType -> TyVarSet
tyVarsOfSourceType (IParam _ ty)  = tyVarsOfType ty
tyVarsOfSourceType (ClassP _ tys) = tyVarsOfTypes tys
tyVarsOfSourceType (NType _ tys)  = tyVarsOfTypes tys

tyVarsOfTheta :: ThetaType -> TyVarSet
tyVarsOfTheta = foldr (unionVarSet . tyVarsOfSourceType) emptyVarSet

-- Add a Note with the free tyvars to the top of the type
addFreeTyVars :: Type -> Type
addFreeTyVars ty@(NoteTy (FTVNote _) _)      = ty
addFreeTyVars ty			     = NoteTy (FTVNote (tyVarsOfType ty)) ty
\end{code}

%************************************************************************
%*									*
\subsection{TidyType}
%*									*
%************************************************************************

tidyTy tidies up a type for printing in an error message, or in
an interface file.

It doesn't change the uniques at all, just the print names.

\begin{code}
tidyTyVarBndr :: TidyEnv -> TyVar -> (TidyEnv, TyVar)
tidyTyVarBndr (tidy_env, subst) tyvar
  = case tidyOccName tidy_env (getOccName name) of
      (tidy', occ') -> 	-- New occname reqd
			((tidy', subst'), tyvar')
		    where
			subst' = extendVarEnv subst tyvar tyvar'
			tyvar' = setTyVarName tyvar name'
			name'  = mkInternalName (getUnique name) occ' noSrcLoc
				-- Note: make a *user* tyvar, so it printes nicely
				-- Could extract src loc, but no need.
  where
    name = tyVarName tyvar

tidyFreeTyVars :: TidyEnv -> TyVarSet -> TidyEnv
-- Add the free tyvars to the env in tidy form,
-- so that we can tidy the type they are free in
tidyFreeTyVars env tyvars = fst (tidyOpenTyVars env (varSetElems tyvars))

tidyOpenTyVars :: TidyEnv -> [TyVar] -> (TidyEnv, [TyVar])
tidyOpenTyVars env tyvars = mapAccumL tidyOpenTyVar env tyvars

tidyOpenTyVar :: TidyEnv -> TyVar -> (TidyEnv, TyVar)
-- Treat a new tyvar as a binder, and give it a fresh tidy name
tidyOpenTyVar env@(tidy_env, subst) tyvar
  = case lookupVarEnv subst tyvar of
	Just tyvar' -> (env, tyvar')		-- Already substituted
	Nothing	    -> tidyTyVarBndr env tyvar	-- Treat it as a binder

tidyType :: TidyEnv -> Type -> Type
tidyType env@(tidy_env, subst) ty
  = go ty
  where
    go (TyVarTy tv)	    = case lookupVarEnv subst tv of
				Nothing  -> TyVarTy tv
				Just tv' -> TyVarTy tv'
    go (TyConApp tycon tys) = let args = map go tys
			      in args `seqList` TyConApp tycon args
    go (NoteTy note ty)     = (NoteTy $! (go_note note)) $! (go ty)
    go (SourceTy sty)	    = SourceTy (tidySourceType env sty)
    go (AppTy fun arg)	    = (AppTy $! (go fun)) $! (go arg)
    go (FunTy fun arg)	    = (FunTy $! (go fun)) $! (go arg)
    go (ForAllTy tv ty)	    = ForAllTy tvp $! (tidyType envp ty)
			      where
			        (envp, tvp) = tidyTyVarBndr env tv

    go_note (SynNote ty)        = SynNote $! (go ty)
    go_note note@(FTVNote ftvs) = note	-- No need to tidy the free tyvars

tidyTypes env tys = map (tidyType env) tys

tidyPred :: TidyEnv -> SourceType -> SourceType
tidyPred = tidySourceType

tidySourceType :: TidyEnv -> SourceType -> SourceType
tidySourceType env (IParam n ty)     = IParam n (tidyType env ty)
tidySourceType env (ClassP clas tys) = ClassP clas (tidyTypes env tys)
tidySourceType env (NType tc tys)    = NType  tc   (tidyTypes env tys)
\end{code}


@tidyOpenType@ grabs the free type variables, tidies them
and then uses @tidyType@ to work over the type itself

\begin{code}
tidyOpenType :: TidyEnv -> Type -> (TidyEnv, Type)
tidyOpenType env ty
  = (env', tidyType env' ty)
  where
    env' = tidyFreeTyVars env (tyVarsOfType ty)

tidyOpenTypes :: TidyEnv -> [Type] -> (TidyEnv, [Type])
tidyOpenTypes env tys = mapAccumL tidyOpenType env tys

tidyTopType :: Type -> Type
tidyTopType ty = tidyType emptyTidyEnv ty
\end{code}



%************************************************************************
%*									*
\subsection{Liftedness}
%*									*
%************************************************************************

\begin{code}
isUnLiftedType :: Type -> Bool
	-- isUnLiftedType returns True for forall'd unlifted types:
	--	forall a. Int#
	--	forall a. (# a, a #)
	-- These can arise from the CPR transformation

isUnLiftedType (ForAllTy tv ty) = isUnLiftedType ty
isUnLiftedType (NoteTy _ ty)	= isUnLiftedType ty
isUnLiftedType (TyConApp tc _)  = isUnLiftedTyCon tc
isUnLiftedType (SourceTy _)	= False		-- All source types are lifted
isUnLiftedType other		= False	

isUnboxedTupleType :: Type -> Bool
isUnboxedTupleType ty = case splitTyConApp_maybe ty of
			   Just (tc, ty_args) -> isUnboxedTupleTyCon tc
			   other	      -> False

-- Should only be applied to *types*; hence the assert
isAlgType :: Type -> Bool
isAlgType ty = case splitTyConApp_maybe ty of
			Just (tc, ty_args) -> ASSERT( ty_args `lengthIs` tyConArity tc )
					      isAlgTyCon tc
			other		   -> False
\end{code}

@isStrictType@ computes whether an argument (or let RHS) should
be computed strictly or lazily, based only on its type.
Works just like isUnLiftedType, except that it has a special case 
for dictionaries.  Since it takes account of ClassP, you might think
this function should be in TcType, but isStrictType is used by DataCon,
which is below TcType in the hierarchy, so it's convenient to put it here.

\begin{code}
isStrictType (ForAllTy tv ty)		= isStrictType ty
isStrictType (NoteTy _ ty)   		= isStrictType ty
isStrictType (TyConApp tc _)		= isUnLiftedTyCon tc
isStrictType (SourceTy (ClassP clas _)) = opt_DictsStrict && not (isNewTyCon (classTyCon clas))
	-- We may be strict in dictionary types, but only if it 
	-- has more than one component.
	-- [Being strict in a single-component dictionary risks
	--  poking the dictionary component, which is wrong.]
isStrictType other			= False	
\end{code}

\begin{code}
isPrimitiveType :: Type -> Bool
-- Returns types that are opaque to Haskell.
-- Most of these are unlifted, but now that we interact with .NET, we
-- may have primtive (foreign-imported) types that are lifted
isPrimitiveType ty = case splitTyConApp_maybe ty of
			Just (tc, ty_args) -> ASSERT( ty_args `lengthIs` tyConArity tc )
					      isPrimTyCon tc
			other		   -> False
\end{code}


%************************************************************************
%*									*
\subsection{Sequencing on types
%*									*
%************************************************************************

\begin{code}
seqType :: Type -> ()
seqType (TyVarTy tv) 	  = tv `seq` ()
seqType (AppTy t1 t2) 	  = seqType t1 `seq` seqType t2
seqType (FunTy t1 t2) 	  = seqType t1 `seq` seqType t2
seqType (NoteTy note t2)  = seqNote note `seq` seqType t2
seqType (SourceTy p) 	  = seqPred p
seqType (TyConApp tc tys) = tc `seq` seqTypes tys
seqType (ForAllTy tv ty)  = tv `seq` seqType ty

seqTypes :: [Type] -> ()
seqTypes []       = ()
seqTypes (ty:tys) = seqType ty `seq` seqTypes tys

seqNote :: TyNote -> ()
seqNote (SynNote ty)  = seqType ty
seqNote (FTVNote set) = sizeUniqSet set `seq` ()

seqPred :: SourceType -> ()
seqPred (ClassP c tys) = c  `seq` seqTypes tys
seqPred (NType tc tys) = tc `seq` seqTypes tys
seqPred (IParam n ty)  = n  `seq` seqType ty
\end{code}


%************************************************************************
%*									*
\subsection{Equality on types}
%*									*
%************************************************************************

Comparison; don't use instances so that we know where it happens.
Look through newtypes but not usage types.

Note that eqType can respond 'False' for partial applications of newtypes.
Consider
	newtype Parser m a = MkParser (Foogle m a)

Does 	
	Monad (Parser m) `eqType` Monad (Foogle m)

Well, yes, but eqType won't see that they are the same. 
I don't think this is harmful, but it's soemthing to watch out for.

\begin{code}
eqType t1 t2 = eq_ty emptyVarEnv t1 t2
eqKind  = eqType	-- No worries about looking 

-- Look through Notes
eq_ty env (NoteTy _ t1)       t2	  	  = eq_ty env t1 t2
eq_ty env t1		      (NoteTy _ t2)       = eq_ty env t1 t2

-- Look through SourceTy.  This is where the looping danger comes from
eq_ty env (SourceTy sty1)     t2		  = eq_ty env (sourceTypeRep sty1) t2
eq_ty env t1		      (SourceTy sty2)     = eq_ty env t1 (sourceTypeRep sty2)

-- The rest is plain sailing
eq_ty env (TyVarTy tv1)       (TyVarTy tv2)       = case lookupVarEnv env tv1 of
							  Just tv1a -> tv1a == tv2
							  Nothing   -> tv1  == tv2
eq_ty env (ForAllTy tv1 t1)   (ForAllTy tv2 t2)   
	| tv1 == tv2				  = eq_ty (delVarEnv env tv1)        t1 t2
	| otherwise				  = eq_ty (extendVarEnv env tv1 tv2) t1 t2
eq_ty env (AppTy s1 t1)       (AppTy s2 t2)       = (eq_ty env s1 s2) && (eq_ty env t1 t2)
eq_ty env (FunTy s1 t1)       (FunTy s2 t2)       = (eq_ty env s1 s2) && (eq_ty env t1 t2)
eq_ty env (TyConApp tc1 tys1) (TyConApp tc2 tys2) = (tc1 == tc2) && (eq_tys env tys1 tys2)
eq_ty env t1		       t2		  = False

eq_tys env []        []        = True
eq_tys env (t1:tys1) (t2:tys2) = (eq_ty env t1 t2) && (eq_tys env tys1 tys2)
eq_tys env tys1      tys2      = False
\end{code}

