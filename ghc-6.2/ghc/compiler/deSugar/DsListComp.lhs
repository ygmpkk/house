%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[DsListComp]{Desugaring list comprehensions and array comprehensions}

\begin{code}
module DsListComp ( dsListComp, dsPArrComp ) where

#include "HsVersions.h"

import {-# SOURCE #-} DsExpr ( dsExpr, dsLet )

import BasicTypes	( Boxity(..) )
import TyCon		( tyConName )
import HsSyn		( Pat(..), HsExpr(..), Stmt(..),
			  HsMatchContext(..), HsStmtContext(..),
			  collectHsBinders )
import TcHsSyn		( TypecheckedStmt, TypecheckedPat, TypecheckedHsExpr,
			  hsPatType )
import CoreSyn

import DsMonad		-- the monadery used in the desugarer
import DsUtils

import CmdLineOpts	( DynFlag(..), dopt, opt_RulesOff )
import CoreUtils	( exprType, mkIfThenElse )
import Id		( idType )
import Var              ( Id )
import Type		( mkTyVarTy, mkFunTys, mkFunTy, Type,
			  splitTyConApp_maybe )
import TysPrim		( alphaTyVar )
import TysWiredIn	( nilDataCon, consDataCon, trueDataConId, falseDataConId, 
			  unitDataConId, unitTy, mkListTy )
import Match		( matchSimply )
import PrelNames	( foldrName, buildName, replicatePName, mapPName, 
			  filterPName, zipPName, crossPName, parrTyConName ) 
import PrelInfo		( pAT_ERROR_ID )
import SrcLoc		( noSrcLoc )
import Panic		( panic )
\end{code}

List comprehensions may be desugared in one of two ways: ``ordinary''
(as you would expect if you read SLPJ's book) and ``with foldr/build
turned on'' (if you read Gill {\em et al.}'s paper on the subject).

There will be at least one ``qualifier'' in the input.

\begin{code}
dsListComp :: [TypecheckedStmt] 
	   -> Type		-- Type of list elements
	   -> DsM CoreExpr

dsListComp quals elt_ty
  = getDOptsDs  `thenDs` \dflags ->
    if opt_RulesOff || dopt Opt_IgnoreInterfacePragmas dflags
	-- Either rules are switched off, or we are ignoring what there are;
	-- Either way foldr/build won't happen, so use the more efficient
	-- Wadler-style desugaring
  	|| isParallelComp quals
		-- Foldr-style desugaring can't handle
		-- parallel list comprehensions
  	then deListComp quals (mkNilExpr elt_ty)

   else		-- Foldr/build should be enabled, so desugar 
		-- into foldrs and builds
    newTyVarsDs [alphaTyVar]    `thenDs` \ [n_tyvar] ->
    let
	n_ty = mkTyVarTy n_tyvar
        c_ty = mkFunTys [elt_ty, n_ty] n_ty
    in
    newSysLocalsDs [c_ty,n_ty]		`thenDs` \ [c, n] ->
    dfListComp c n quals		`thenDs` \ result ->
    dsLookupGlobalId buildName	`thenDs` \ build_id ->
    returnDs (Var build_id `App` Type elt_ty 
			   `App` mkLams [n_tyvar, c, n] result)

  where isParallelComp (ParStmt bndrstmtss : _) = True
	isParallelComp _                        = False
\end{code}

%************************************************************************
%*									*
\subsection[DsListComp-ordinary]{Ordinary desugaring of list comprehensions}
%*									*
%************************************************************************

Just as in Phil's chapter~7 in SLPJ, using the rules for
optimally-compiled list comprehensions.  This is what Kevin followed
as well, and I quite happily do the same.  The TQ translation scheme
transforms a list of qualifiers (either boolean expressions or
generators) into a single expression which implements the list
comprehension.  Because we are generating 2nd-order polymorphic
lambda-calculus, calls to NIL and CONS must be applied to a type
argument, as well as their usual value arguments.
\begin{verbatim}
TE << [ e | qs ] >>  =  TQ << [ e | qs ] ++ Nil (typeOf e) >>

(Rule C)
TQ << [ e | ] ++ L >> = Cons (typeOf e) TE <<e>> TE <<L>>

(Rule B)
TQ << [ e | b , qs ] ++ L >> =
    if TE << b >> then TQ << [ e | qs ] ++ L >> else TE << L >>

(Rule A')
TQ << [ e | p <- L1, qs ]  ++  L2 >> =
  letrec
    h = \ u1 ->
    	  case u1 of
	    []        ->  TE << L2 >>
	    (u2 : u3) ->
		  (( \ TE << p >> -> ( TQ << [e | qs]  ++  (h u3) >> )) u2)
		    [] (h u3)
  in
    h ( TE << L1 >> )

"h", "u1", "u2", and "u3" are new variables.
\end{verbatim}

@deListComp@ is the TQ translation scheme.  Roughly speaking, @dsExpr@
is the TE translation scheme.  Note that we carry around the @L@ list
already desugared.  @dsListComp@ does the top TE rule mentioned above.

To the above, we add an additional rule to deal with parallel list
comprehensions.  The translation goes roughly as follows:
     [ e | p1 <- e11, let v1 = e12, p2 <- e13
         | q1 <- e21, let v2 = e22, q2 <- e23]
     =>
     [ e | ((x1, .., xn), (y1, ..., ym)) <-
               zip [(x1,..,xn) | p1 <- e11, let v1 = e12, p2 <- e13]
                   [(y1,..,ym) | q1 <- e21, let v2 = e22, q2 <- e23]]
where (x1, .., xn) are the variables bound in p1, v1, p2
      (y1, .., ym) are the variables bound in q1, v2, q2

In the translation below, the ParStmt branch translates each parallel branch
into a sub-comprehension, and desugars each independently.  The resulting lists
are fed to a zip function, we create a binding for all the variables bound in all
the comprehensions, and then we hand things off the the desugarer for bindings.
The zip function is generated here a) because it's small, and b) because then we
don't have to deal with arbitrary limits on the number of zip functions in the
prelude, nor which library the zip function came from.
The introduced tuples are Boxed, but only because I couldn't get it to work
with the Unboxed variety.

\begin{code}

deListComp :: [TypecheckedStmt] -> CoreExpr -> DsM CoreExpr

deListComp (ParStmt stmtss_w_bndrs : quals) list
  = mapDs do_list_comp stmtss_w_bndrs	`thenDs` \ exps ->
    mkZipBind qual_tys			`thenDs` \ (zip_fn, zip_rhs) ->

	-- Deal with [e | pat <- zip l1 .. ln] in example above
    deBindComp pat (Let (Rec [(zip_fn, zip_rhs)]) (mkApps (Var zip_fn) exps)) 
		   quals list

  where 
	bndrs_s = map snd stmtss_w_bndrs

	-- pat is the pattern ((x1,..,xn), (y1,..,ym)) in the example above
	pat	 = TuplePat pats Boxed
	pats	 = map mk_hs_tuple_pat bndrs_s

	-- Types of (x1,..,xn), (y1,..,yn) etc
	qual_tys = map mk_bndrs_tys bndrs_s

	do_list_comp (stmts, bndrs)
	  = dsListComp (stmts ++ [ResultStmt (mk_hs_tuple_expr bndrs) noSrcLoc])
		       (mk_bndrs_tys bndrs)

	mk_bndrs_tys bndrs = mkCoreTupTy (map idType bndrs)

	-- Last: the one to return
deListComp [ResultStmt expr locn] list	-- Figure 7.4, SLPJ, p 135, rule C above
  = dsExpr expr			`thenDs` \ core_expr ->
    returnDs (mkConsExpr (exprType core_expr) core_expr list)

	-- Non-last: must be a guard
deListComp (ExprStmt guard ty locn : quals) list	-- rule B above
  = dsExpr guard       		`thenDs` \ core_guard ->
    deListComp quals list	`thenDs` \ core_rest ->
    returnDs (mkIfThenElse core_guard core_rest list)

-- [e | let B, qs] = let B in [e | qs]
deListComp (LetStmt binds : quals) list
  = deListComp quals list	`thenDs` \ core_rest ->
    dsLet binds core_rest

deListComp (BindStmt pat list1 locn : quals) core_list2 -- rule A' above
  = dsExpr list1		    `thenDs` \ core_list1 ->
    deBindComp pat core_list1 quals core_list2
\end{code}


\begin{code}
deBindComp pat core_list1 quals core_list2
  = let
	u3_ty@u1_ty = exprType core_list1	-- two names, same thing

	-- u1_ty is a [alpha] type, and u2_ty = alpha
	u2_ty = hsPatType pat

	res_ty = exprType core_list2
	h_ty   = u1_ty `mkFunTy` res_ty
    in
    newSysLocalsDs [h_ty, u1_ty, u2_ty, u3_ty]	`thenDs` \ [h, u1, u2, u3] ->

    -- the "fail" value ...
    let
	core_fail   = App (Var h) (Var u3)
	letrec_body = App (Var h) core_list1
    in
    deListComp quals core_fail			`thenDs` \ rest_expr ->
    matchSimply (Var u2) (StmtCtxt ListComp) pat
		rest_expr core_fail		`thenDs` \ core_match ->
    let
	rhs = Lam u1 $
	      Case (Var u1) u1 [(DataAlt nilDataCon,  [], 	core_list2),
			        (DataAlt consDataCon, [u2, u3], core_match)]
    in
    returnDs (Let (Rec [(h, rhs)]) letrec_body)
\end{code}


\begin{code}
mkZipBind :: [Type] -> DsM (Id, CoreExpr)
-- mkZipBind [t1, t2] 
-- = (zip, \as1:[t1] as2:[t2] 
--	   -> case as1 of 
--		[] -> []
--		(a1:as'1) -> case as2 of
--				[] -> []
--				(a2:as'2) -> (a2,a2) : zip as'1 as'2)]

mkZipBind elt_tys 
  = mapDs newSysLocalDs  list_tys	`thenDs` \ ass ->
    mapDs newSysLocalDs  elt_tys	`thenDs` \ as' ->
    mapDs newSysLocalDs  list_tys	`thenDs` \ as's ->
    newSysLocalDs zip_fn_ty		`thenDs` \ zip_fn ->
    let 
	inner_rhs = mkConsExpr ret_elt_ty 
			(mkCoreTup (map Var as'))
			(mkVarApps (Var zip_fn) as's)
	zip_body  = foldr mk_case inner_rhs (zip3 ass as' as's)
    in
    returnDs (zip_fn, mkLams ass zip_body)
  where
    list_tys   = map mkListTy elt_tys
    ret_elt_ty = mkCoreTupTy elt_tys
    zip_fn_ty  = mkFunTys list_tys (mkListTy ret_elt_ty)

    mk_case (as, a', as') rest
	  = Case (Var as) as [(DataAlt nilDataCon,  [],        mkNilExpr ret_elt_ty),
			      (DataAlt consDataCon, [a', as'], rest)]

-- Helper functions that makes an HsTuple only for non-1-sized tuples
mk_hs_tuple_expr :: [Id] -> TypecheckedHsExpr
mk_hs_tuple_expr []   = HsVar unitDataConId
mk_hs_tuple_expr [id] = HsVar id
mk_hs_tuple_expr ids  = ExplicitTuple [ HsVar i | i <- ids ] Boxed

mk_hs_tuple_pat :: [Id] -> TypecheckedPat
mk_hs_tuple_pat [b] = VarPat b
mk_hs_tuple_pat bs  = TuplePat (map VarPat bs) Boxed
\end{code}


%************************************************************************
%*									*
\subsection[DsListComp-foldr-build]{Foldr/Build desugaring of list comprehensions}
%*									*
%************************************************************************

@dfListComp@ are the rules used with foldr/build turned on:

\begin{verbatim}
TE[ e | ]            c n = c e n
TE[ e | b , q ]      c n = if b then TE[ e | q ] c n else n
TE[ e | p <- l , q ] c n = let 
				f = \ x b -> case x of
						  p -> TE[ e | q ] c b
						  _ -> b
			   in
			   foldr f n l
\end{verbatim}

\begin{code}
dfListComp :: Id -> Id			-- 'c' and 'n'
	   -> [TypecheckedStmt] 	-- the rest of the qual's
	   -> DsM CoreExpr

	-- Last: the one to return
dfListComp c_id n_id [ResultStmt expr locn]
  = dsExpr expr			`thenDs` \ core_expr ->
    returnDs (mkApps (Var c_id) [core_expr, Var n_id])

	-- Non-last: must be a guard
dfListComp c_id n_id (ExprStmt guard ty locn  : quals)
  = dsExpr guard               			`thenDs` \ core_guard ->
    dfListComp c_id n_id quals	`thenDs` \ core_rest ->
    returnDs (mkIfThenElse core_guard core_rest (Var n_id))

dfListComp c_id n_id (LetStmt binds : quals)
  -- new in 1.3, local bindings
  = dfListComp c_id n_id quals	`thenDs` \ core_rest ->
    dsLet binds core_rest

dfListComp c_id n_id (BindStmt pat list1 locn : quals)
    -- evaluate the two lists
  = dsExpr list1				`thenDs` \ core_list1 ->

    -- find the required type
    let x_ty   = hsPatType pat
	b_ty   = idType n_id
    in

    -- create some new local id's
    newSysLocalsDs [b_ty,x_ty]			`thenDs` \ [b,x] ->

    -- build rest of the comprehesion
    dfListComp c_id b quals			`thenDs` \ core_rest ->

    -- build the pattern match
    matchSimply (Var x) (StmtCtxt ListComp) 
		pat core_rest (Var b)		`thenDs` \ core_expr ->

    -- now build the outermost foldr, and return
    dsLookupGlobalId foldrName		`thenDs` \ foldr_id ->
    returnDs (
      Var foldr_id `App` Type x_ty 
		   `App` Type b_ty
		   `App` mkLams [x, b] core_expr
		   `App` Var n_id
		   `App` core_list1
    )
\end{code}

%************************************************************************
%*									*
\subsection[DsPArrComp]{Desugaring of array comprehensions}
%*									*
%************************************************************************

\begin{code}

-- entry point for desugaring a parallel array comprehension
--
--   [:e | qss:] = <<[:e | qss:]>> () [:():]
--
dsPArrComp      :: [TypecheckedStmt] 
	        -> Type		    -- Don't use; called with `undefined' below
	        -> DsM CoreExpr
dsPArrComp qs _  =
  dsLookupGlobalId replicatePName			  `thenDs` \repP ->
  let unitArray = mkApps (Var repP) [Type unitTy, 
				     mkIntExpr 1, 
				     mkCoreTup []]
  in
  dePArrComp qs (TuplePat [] Boxed) unitArray

-- the work horse
--
dePArrComp :: [TypecheckedStmt] 
	   -> TypecheckedPat		-- the current generator pattern
	   -> CoreExpr			-- the current generator expression
	   -> DsM CoreExpr
--
--  <<[:e' | :]>> pa ea = mapP (\pa -> e') ea
--
dePArrComp [ResultStmt e' _] pa cea =
  dsLookupGlobalId mapPName				  `thenDs` \mapP    ->
  let ty = parrElemType cea
  in
  deLambda ty pa e'					  `thenDs` \(clam, 
								     ty'e') ->
  returnDs $ mkApps (Var mapP) [Type ty, Type ty'e', clam, cea]
--
--  <<[:e' | b, qs:]>> pa ea = <<[:e' | qs:]>> pa (filterP (\pa -> b) ea)
--
dePArrComp (ExprStmt b _ _ : qs) pa cea =
  dsLookupGlobalId filterPName			  `thenDs` \filterP  ->
  let ty = parrElemType cea
  in
  deLambda ty pa b					  `thenDs` \(clam,_) ->
  dePArrComp qs pa (mkApps (Var filterP) [Type ty, clam, cea])
--
--  <<[:e' | p <- e, qs:]>> pa ea = 
--    let ef = filterP (\x -> case x of {p -> True; _ -> False}) e
--    in
--    <<[:e' | qs:]>> (pa, p) (crossP ea ef)
--
dePArrComp (BindStmt p e _ : qs) pa cea =
  dsLookupGlobalId filterPName			  `thenDs` \filterP ->
  dsLookupGlobalId crossPName			  `thenDs` \crossP  ->
  dsExpr e					  `thenDs` \ce      ->
  let ty'cea = parrElemType cea
      ty'ce  = parrElemType ce
      false  = Var falseDataConId
      true   = Var trueDataConId
  in
  newSysLocalDs ty'ce					  `thenDs` \v       ->
  matchSimply (Var v) (StmtCtxt PArrComp) p true false      `thenDs` \pred    ->
  let cef    = mkApps (Var filterP) [Type ty'ce, mkLams [v] pred, ce]
      ty'cef = ty'ce				-- filterP preserves the type
      pa'    = TuplePat [pa, p] Boxed
  in
  dePArrComp qs pa' (mkApps (Var crossP) [Type ty'cea, Type ty'cef, cea, cef])
--
--  <<[:e' | let ds, qs:]>> pa ea = 
--    <<[:e' | qs:]>> (pa, (x_1, ..., x_n)) 
--		      (mapP (\v@pa -> (v, let ds in (x_1, ..., x_n))) ea)
--  where
--    {x_1, ..., x_n} = DV (ds)		-- Defined Variables
--
dePArrComp (LetStmt ds : qs) pa cea =
  dsLookupGlobalId mapPName				  `thenDs` \mapP    ->
  let xs     = collectHsBinders ds
      ty'cea = parrElemType cea
  in
  newSysLocalDs ty'cea					  `thenDs` \v       ->
  dsLet ds (mkCoreTup (map Var xs))			  `thenDs` \clet    ->
  newSysLocalDs (exprType clet)				  `thenDs` \let'v   ->
  let projBody = mkDsLet (NonRec let'v clet) $ 
		 mkCoreTup [Var v, Var let'v]
      errTy    = exprType projBody
      errMsg   = "DsListComp.dePArrComp: internal error!"
  in
  mkErrorAppDs pAT_ERROR_ID errTy errMsg                  `thenDs` \cerr    ->
  matchSimply (Var v) (StmtCtxt PArrComp) pa projBody cerr  `thenDs` \ccase   ->
  let pa'    = TuplePat [pa, TuplePat (map VarPat xs) Boxed] Boxed
      proj   = mkLams [v] ccase
  in
  dePArrComp qs pa' (mkApps (Var mapP) [Type ty'cea, proj, cea])
--
--  <<[:e' | qs | qss:]>> pa ea = 
--    <<[:e' | qss:]>> (pa, (x_1, ..., x_n)) 
--		       (zipP ea <<[:(x_1, ..., x_n) | qs:]>>)
--    where
--      {x_1, ..., x_n} = DV (qs)
--
dePArrComp (ParStmt []             : qss2) pa cea = dePArrComp qss2 pa cea
dePArrComp (ParStmt ((qs, xs):qss) : qss2) pa cea =
  dsLookupGlobalId zipPName				  `thenDs` \zipP    ->
  let pa'     = TuplePat [pa, TuplePat (map VarPat xs) Boxed] Boxed
      ty'cea  = parrElemType cea
      resStmt = ResultStmt (ExplicitTuple (map HsVar xs) Boxed) noSrcLoc
  in
  dsPArrComp (qs ++ [resStmt]) undefined		  `thenDs` \cqs     ->
  let ty'cqs = parrElemType cqs
      cea'   = mkApps (Var zipP) [Type ty'cea, Type ty'cqs, cea, cqs]
  in
  dePArrComp (ParStmt qss : qss2) pa' cea'

-- generate Core corresponding to `\p -> e'
--
deLambda        :: Type			-- type of the argument
		-> TypecheckedPat	-- argument pattern
		-> TypecheckedHsExpr	-- body
		-> DsM (CoreExpr, Type)
deLambda ty p e  =
  newSysLocalDs ty					  `thenDs` \v       ->
  dsExpr e						  `thenDs` \ce      ->
  let errTy    = exprType ce
      errMsg   = "DsListComp.deLambda: internal error!"
  in
  mkErrorAppDs pAT_ERROR_ID errTy errMsg                  `thenDs` \cerr    ->
  matchSimply (Var v) (StmtCtxt PArrComp) p ce cerr	  `thenDs` \res	    ->
  returnDs (mkLams [v] res, errTy)

-- obtain the element type of the parallel array produced by the given Core
-- expression
--
parrElemType   :: CoreExpr -> Type
parrElemType e  = 
  case splitTyConApp_maybe (exprType e) of
    Just (tycon, [ty]) | tyConName tycon == parrTyConName -> ty
    _							  -> panic
      "DsListComp.parrElemType: not a parallel array type"
\end{code}
