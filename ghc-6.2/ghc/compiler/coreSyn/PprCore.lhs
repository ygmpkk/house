%
% (c) The AQUA Project, Glasgow University, 1996-1998
%
%************************************************************************
%*									*
\section[PprCore]{Printing of Core syntax, including for interfaces}
%*									*
%************************************************************************

\begin{code}
module PprCore (
	pprCoreExpr, pprParendExpr, pprIdBndr,
	pprCoreBinding, pprCoreBindings, pprCoreAlt,
	pprIdRules, pprCoreRule
    ) where

#include "HsVersions.h"

import CoreSyn
import CostCentre	( pprCostCentreCore )
import Var		( Var )
import Id		( Id, idType, isDataConWorkId_maybe, idLBVarInfo, idArity,
			  idInfo, idInlinePragma, idOccInfo,
#ifdef OLD_STRICTNESS
			  idDemandInfo, 
#endif
			  globalIdDetails, isGlobalId, isExportedId, 
			  isSpecPragmaId, idNewDemandInfo
			)
import Var		( isTyVar )
import IdInfo		( IdInfo, megaSeqIdInfo, 
			  arityInfo, ppArityInfo, 
			  specInfo, pprNewStrictness,
			  workerInfo, ppWorkerInfo,
			  newStrictnessInfo, cafInfo, ppCafInfo,
#ifdef OLD_STRICTNESS
			  cprInfo, ppCprInfo, 
			  strictnessInfo, ppStrictnessInfo, 
#endif
			)
import DataCon		( dataConTyCon )
import TyCon		( tupleTyConBoxity, isTupleTyCon )
import PprType		( pprParendType, pprType, pprTyVarBndr )
import BasicTypes	( tupleParens )
import Util             ( lengthIs )
import Outputable
import FastString       ( mkFastString )
\end{code}

%************************************************************************
%*									*
\subsection{Public interfaces for Core printing (excluding instances)}
%*									*
%************************************************************************

@pprParendCoreExpr@ puts parens around non-atomic Core expressions.

\begin{code}
pprCoreBindings :: OutputableBndr b => [Bind b] -> SDoc
pprCoreBinding  :: OutputableBndr b => Bind b  -> SDoc
pprCoreExpr     :: OutputableBndr b => Expr b  -> SDoc
pprParendExpr   :: OutputableBndr b => Expr b  -> SDoc

pprCoreBindings = pprTopBinds
pprCoreBinding  = pprTopBind 

instance OutputableBndr b => Outputable (Bind b) where
    ppr bind = ppr_bind bind

instance OutputableBndr b => Outputable (Expr b) where
    ppr expr = pprCoreExpr expr
\end{code}


%************************************************************************
%*									*
\subsection{The guts}
%*									*
%************************************************************************

\begin{code}
pprTopBinds binds = vcat (map pprTopBind binds)

pprTopBind (NonRec binder expr)
 = ppr_binding (binder,expr) $$ text ""

pprTopBind (Rec binds)
  = vcat [ptext SLIT("Rec {"),
	  vcat (map ppr_binding binds),
	  ptext SLIT("end Rec }"),
	  text ""]
\end{code}

\begin{code}
ppr_bind :: OutputableBndr b => Bind b -> SDoc

ppr_bind (NonRec val_bdr expr) = ppr_binding (val_bdr, expr)
ppr_bind (Rec binds)  	       = vcat (map pp binds)
			       where
				 pp bind = ppr_binding bind <> semi

ppr_binding :: OutputableBndr b => (b, Expr b) -> SDoc
ppr_binding (val_bdr, expr)
  = pprBndr LetBind val_bdr $$ 
    (ppr val_bdr <+> equals <+> pprCoreExpr expr)
\end{code}

\begin{code}
pprParendExpr   expr = ppr_expr parens expr
pprCoreExpr expr = ppr_expr noParens expr

noParens :: SDoc -> SDoc
noParens pp = pp
\end{code}

\begin{code}
ppr_expr :: OutputableBndr b => (SDoc -> SDoc) -> Expr b -> SDoc
	-- The function adds parens in context that need
	-- an atomic value (e.g. function args)

ppr_expr add_par (Type ty)  = add_par (ptext SLIT("TYPE") <+> ppr ty)	-- Wierd
	           
ppr_expr add_par (Var name) = ppr name
ppr_expr add_par (Lit lit)  = ppr lit

ppr_expr add_par expr@(Lam _ _)
  = let
	(bndrs, body) = collectBinders expr
    in
    add_par $
    hang (ptext SLIT("\\") <+> sep (map (pprBndr LambdaBind) bndrs) <+> arrow)
	 2 (pprCoreExpr body)

ppr_expr add_par expr@(App fun arg)
  = case collectArgs expr of { (fun, args) -> 
    let
	pp_args     = sep (map pprArg args)
	val_args    = dropWhile isTypeArg args	 -- Drop the type arguments for tuples
	pp_tup_args = sep (punctuate comma (map pprArg val_args))
    in
    case fun of
	Var f -> case isDataConWorkId_maybe f of
			-- Notice that we print the *worker*
			-- for tuples in paren'd format.
		   Just dc | saturated && isTupleTyCon tc
			   -> tupleParens (tupleTyConBoxity tc) pp_tup_args
			   where
			     tc	       = dataConTyCon dc
			     saturated = val_args `lengthIs` idArity f

		   other -> add_par (hang (ppr f) 2 pp_args)

	other -> add_par (hang (pprParendExpr fun) 2 pp_args)
    }

ppr_expr add_par (Case expr var [(con,args,rhs)])
  = add_par $
    sep [sep [ptext SLIT("case") <+> pprCoreExpr expr,
	      hsep [ptext SLIT("of"),
		    ppr_bndr var,
		    char '{',
		    ppr_case_pat con args
	  ]],
	 pprCoreExpr rhs,
	 char '}'
    ]
  where
    ppr_bndr = pprBndr CaseBind

ppr_expr add_par (Case expr var alts)
  = add_par $
    sep [sep [ptext SLIT("case") <+> pprCoreExpr expr,
	      ptext SLIT("of") <+> ppr_bndr var <+> char '{'],
	 nest 2 (sep (punctuate semi (map pprCoreAlt alts))),
	 char '}'
    ]
  where
    ppr_bndr = pprBndr CaseBind
 

-- special cases: let ... in let ...
-- ("disgusting" SLPJ)

{-
ppr_expr add_par (Let bind@(NonRec val_bdr rhs@(Let _ _)) body)
  = add_par $
    vcat [
      hsep [ptext SLIT("let {"), (pprBndr LetBind val_bdr $$ ppr val_bndr), equals],
      nest 2 (pprCoreExpr rhs),
      ptext SLIT("} in"),
      pprCoreExpr body ]
-}

ppr_expr add_par (Let bind@(NonRec val_bdr rhs) expr@(Let _ _))
  = add_par
    (hang (ptext SLIT("let {"))
	  2 (hsep [ppr_binding (val_bdr,rhs),
		   ptext SLIT("} in")])
     $$
     pprCoreExpr expr)

-- general case (recursive case, too)
ppr_expr add_par (Let bind expr)
  = add_par $
    sep [hang (ptext keyword) 2 (ppr_bind bind),
	 hang (ptext SLIT("} in ")) 2 (pprCoreExpr expr)]
  where
    keyword = case bind of
		Rec _      -> SLIT("__letrec {")
		NonRec _ _ -> SLIT("let {")

ppr_expr add_par (Note (SCC cc) expr)
  = add_par (sep [pprCostCentreCore cc, pprCoreExpr expr])

#ifdef DEBUG
ppr_expr add_par (Note (Coerce to_ty from_ty) expr)
 = add_par $
   getPprStyle $ \ sty ->
   if debugStyle sty then
      sep [ptext SLIT("__coerce") <+> 
		sep [pprParendType to_ty, pprParendType from_ty],
	   pprParendExpr expr]
   else
      sep [hsep [ptext SLIT("__coerce"), pprParendType to_ty],
	          pprParendExpr expr]
#else
ppr_expr add_par (Note (Coerce to_ty from_ty) expr)
  = add_par $
    sep [sep [ptext SLIT("__coerce"), nest 2 (pprParendType to_ty)],
	 pprParendExpr expr]
#endif

ppr_expr add_par (Note InlineCall expr)
  = add_par (ptext SLIT("__inline_call") <+> pprParendExpr expr)

ppr_expr add_par (Note InlineMe expr)
  = add_par $ ptext SLIT("__inline_me") <+> pprParendExpr expr

ppr_expr add_par (Note (CoreNote s) expr)
  = add_par $ 
    sep [sep [ptext SLIT("__core_note"), pprHsString (mkFastString s)],
         pprParendExpr expr]

pprCoreAlt (con, args, rhs) 
  = hang (ppr_case_pat con args) 2 (pprCoreExpr rhs)

ppr_case_pat con@(DataAlt dc) args
  | isTupleTyCon tc
  = tupleParens (tupleTyConBoxity tc) (hsep (punctuate comma (map ppr_bndr args))) <+> arrow
  where
    ppr_bndr = pprBndr CaseBind
    tc = dataConTyCon dc

ppr_case_pat con args
  = ppr con <+> hsep (map ppr_bndr args) <+> arrow
  where
    ppr_bndr = pprBndr CaseBind

pprArg (Type ty) = ptext SLIT("@") <+> pprParendType ty
pprArg expr      = pprParendExpr expr

arrow = ptext SLIT("->")
\end{code}

Other printing bits-and-bobs used with the general @pprCoreBinding@
and @pprCoreExpr@ functions.

\begin{code}
instance OutputableBndr Var where
  pprBndr = pprCoreBinder

pprCoreBinder :: BindingSite -> Var -> SDoc
pprCoreBinder LetBind binder
  = vcat [sig, pprIdDetails binder, pragmas]
  where
    sig     = pprTypedBinder binder
    pragmas = ppIdInfo binder (idInfo binder)

-- Lambda bound type variables are preceded by "@"
pprCoreBinder LambdaBind bndr = pprTypedBinder bndr

-- Case bound things don't get a signature or a herald
pprCoreBinder CaseBind bndr = pprUntypedBinder bndr

pprUntypedBinder binder
  | isTyVar binder = ptext SLIT("@") <+> ppr binder	-- NB: don't print kind
  | otherwise      = pprIdBndr binder

pprTypedBinder binder
  | isTyVar binder  = ptext SLIT("@") <+> pprTyVarBndr binder
  | otherwise	    = pprIdBndr binder <+> dcolon <+> pprType (idType binder)
	-- The space before the :: is important; it helps the lexer
	-- when reading inferfaces.  Otherwise it would lex "a::b" as one thing.
	--
	-- It's important that the type is parenthesised too, at least when
	-- printing interfaces, because we get \ x::(a->b) y::(c->d) -> ...
	--	[Jun 2002: interfaces are now binary, so this doesn't matter]

-- pprIdBndr does *not* print the type
-- When printing any Id binder in debug mode, we print its inline pragma and one-shot-ness
pprIdBndr id = ppr id <+> 
	       (megaSeqIdInfo (idInfo id) `seq`
			-- Useful for poking on black holes
	        ifPprDebug (ppr (idInlinePragma id) <+> ppr (idOccInfo id) <+> 
#ifdef OLD_STRICTNESS
			    ppr (idDemandInfo id) <+>
#endif
			    ppr (idNewDemandInfo id) <+>
			    ppr (idLBVarInfo id)))
\end{code}


\begin{code}
pprIdDetails :: Id -> SDoc
pprIdDetails id | isGlobalId id     = ppr (globalIdDetails id)
		| isExportedId id   = ptext SLIT("[Exported]")
		| isSpecPragmaId id = ptext SLIT("[SpecPrag]")
		| otherwise	    = empty

ppIdInfo :: Id -> IdInfo -> SDoc
ppIdInfo b info
  = hsep [  ppArityInfo a,
	    ppWorkerInfo (workerInfo info),
	    ppCafInfo (cafInfo info),
#ifdef OLD_STRICTNESS
	    ppStrictnessInfo s,
            ppCprInfo m,
#endif
	    pprNewStrictness (newStrictnessInfo info),
	    vcat (map (pprCoreRule (ppr b)) (rulesRules p))
	-- Inline pragma, occ, demand, lbvar info
	-- printed out with all binders (when debug is on); 
	-- see PprCore.pprIdBndr
	]
  where
    a = arityInfo info
#ifdef OLD_STRICTNESS
    s = strictnessInfo info
    m = cprInfo info
#endif
    p = specInfo info
\end{code}


\begin{code}
pprIdRules :: [IdCoreRule] -> SDoc
pprIdRules rules = vcat (map pprIdRule rules)

pprIdRule :: IdCoreRule -> SDoc
pprIdRule (id,rule) = pprCoreRule (ppr id) rule

pprCoreRule :: SDoc -> CoreRule -> SDoc
pprCoreRule pp_fn (BuiltinRule name _)
  = ifPprDebug (ptext SLIT("Built in rule for") <+> pp_fn <> colon
		 <+> doubleQuotes (ftext name))

pprCoreRule pp_fn (Rule name act tpl_vars tpl_args rhs)
  = doubleQuotes (ftext name) <+> ppr act <+>
    sep [
	  ptext SLIT("__forall") <+> braces (sep (map pprTypedBinder tpl_vars)),
	  nest 2 (pp_fn <+> sep (map pprArg tpl_args)),
	  nest 2 (ptext SLIT("=") <+> pprCoreExpr rhs)
    ] <+> semi
\end{code}
