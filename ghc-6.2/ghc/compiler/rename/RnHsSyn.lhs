%
% (c) The AQUA Project, Glasgow University, 1996-1998
%
\section[RnHsSyn]{Specialisations of the @HsSyn@ syntax for the renamer}

\begin{code}
module RnHsSyn where

#include "HsVersions.h"

import HsSyn
import HsCore
import Class		( FunDep, DefMeth(..) )
import TyCon		( visibleDataCons, tyConName )
import TysWiredIn	( tupleTyCon, listTyCon, parrTyCon, charTyCon )
import Name		( Name, getName, isTyVarName )
import NameSet
import BasicTypes	( Boxity, FixitySig )
import Outputable
\end{code}


\begin{code}
type RenamedHsDecl		= HsDecl		Name
type RenamedArithSeqInfo	= ArithSeqInfo		Name
type RenamedClassOpSig		= Sig			Name
type RenamedConDecl		= ConDecl		Name
type RenamedContext		= HsContext 		Name
type RenamedRuleDecl		= RuleDecl		Name
type RenamedTyClDecl		= TyClDecl		Name
type RenamedDefaultDecl		= DefaultDecl		Name
type RenamedForeignDecl		= ForeignDecl		Name
type RenamedCoreDecl		= CoreDecl		Name
type RenamedGRHS		= GRHS			Name
type RenamedGRHSs		= GRHSs			Name
type RenamedHsBinds		= HsBinds		Name
type RenamedHsExpr		= HsExpr		Name
type RenamedInstDecl		= InstDecl		Name
type RenamedMatchContext	= HsMatchContext	Name
type RenamedMatch		= Match			Name
type RenamedMonoBinds		= MonoBinds		Name
type RenamedPat			= InPat			Name
type RenamedHsType		= HsType		Name
type RenamedHsPred		= HsPred		Name
type RenamedRecordBinds		= HsRecordBinds		Name
type RenamedSig			= Sig			Name
type RenamedStmt		= Stmt			Name
type RenamedFixitySig		= FixitySig		Name
type RenamedDeprecation		= DeprecDecl		Name
type RenamedHsCmd		= HsCmd			Name
type RenamedHsCmdTop		= HsCmdTop		Name
\end{code}

%************************************************************************
%*									*
\subsection{Free variables}
%*									*
%************************************************************************

These free-variable finders returns tycons and classes too.

\begin{code}
charTyCon_name, listTyCon_name, parrTyCon_name :: Name
charTyCon_name    = getName charTyCon
listTyCon_name    = getName listTyCon
parrTyCon_name    = getName parrTyCon

tupleTyCon_name :: Boxity -> Int -> Name
tupleTyCon_name boxity n = getName (tupleTyCon boxity n)

extractHsTyVars :: RenamedHsType -> NameSet
extractHsTyVars x = filterNameSet isTyVarName (extractHsTyNames x)

extractFunDepNames :: FunDep Name -> NameSet
extractFunDepNames (ns1, ns2) = mkNameSet ns1 `unionNameSets` mkNameSet ns2

extractHsTyNames   :: RenamedHsType -> NameSet
extractHsTyNames ty
  = get ty
  where
    get (HsAppTy ty1 ty2)      = get ty1 `unionNameSets` get ty2
    get (HsListTy ty)          = unitNameSet listTyCon_name `unionNameSets` get ty
    get (HsPArrTy ty)          = unitNameSet parrTyCon_name `unionNameSets` get ty
    get (HsTupleTy con tys)    = hsTupConFVs con `unionNameSets` extractHsTyNames_s tys
    get (HsFunTy ty1 ty2)      = get ty1 `unionNameSets` get ty2
    get (HsPredTy p)	       = extractHsPredTyNames p
    get (HsOpTy ty1 tycon ty2) = get ty1 `unionNameSets` get ty2 `unionNameSets`
				 case tycon of { HsTyOp n -> unitNameSet n ; 
						 HsArrow  -> emptyNameSet }
    get (HsParTy ty)           = get ty
    get (HsNumTy n)            = emptyNameSet
    get (HsTyVar tv)	       = unitNameSet tv
    get (HsKindSig ty k)       = get ty
    get (HsForAllTy (Just tvs) 
		    ctxt ty)   = (extractHsCtxtTyNames ctxt `unionNameSets` get ty)
					    `minusNameSet`
				  mkNameSet (hsTyVarNames tvs)
    get ty@(HsForAllTy Nothing _ _) = pprPanic "extractHsTyNames" (ppr ty)

extractHsTyNames_s  :: [RenamedHsType] -> NameSet
extractHsTyNames_s tys = foldr (unionNameSets . extractHsTyNames) emptyNameSet tys

extractHsCtxtTyNames :: RenamedContext -> NameSet
extractHsCtxtTyNames ctxt = foldr (unionNameSets . extractHsPredTyNames) emptyNameSet ctxt

-- You don't import or export implicit parameters,
-- so don't mention the IP names
extractHsPredTyNames (HsClassP cls tys)
  = unitNameSet cls `unionNameSets` extractHsTyNames_s tys
extractHsPredTyNames (HsIParam n ty)
  = extractHsTyNames ty
\end{code}


%************************************************************************
%*									*
\subsection{Free variables of declarations}
%*									*
%************************************************************************

Return the Names that must be in scope if we are to use this declaration.
In all cases this is set up for interface-file declarations:
	- for class decls we ignore the bindings
	- for instance decls likewise, plus the pragmas
	- for rule decls, we ignore HsRules
        - for data decls, we ignore derivings

	*** See "THE NAMING STORY" in HsDecls ****

\begin{code}
----------------
impDeclFVs :: RenamedHsDecl -> NameSet
	-- Just the ones that come from imports
impDeclFVs (InstD d) = instDeclFVs d
impDeclFVs (TyClD d) = tyClDeclFVs d

----------------
tyClDeclFVs :: RenamedTyClDecl -> NameSet
tyClDeclFVs (ForeignType {})
  = emptyFVs

tyClDeclFVs (IfaceSig {tcdType = ty, tcdIdInfo = id_infos})
  = extractHsTyNames ty			`plusFV` 
    plusFVs (map hsIdInfoFVs id_infos)

tyClDeclFVs (TyData {tcdCtxt = context, tcdTyVars = tyvars, tcdCons = condecls})
  = delFVs (map hsTyVarName tyvars)	$
    extractHsCtxtTyNames context	`plusFV`
    plusFVs (map conDeclFVs (visibleDataCons condecls))

tyClDeclFVs (TySynonym {tcdTyVars = tyvars, tcdSynRhs = ty})
  = delFVs (map hsTyVarName tyvars) (extractHsTyNames ty)

tyClDeclFVs (ClassDecl {tcdCtxt = context, tcdTyVars = tyvars, tcdFDs = fds, 
			tcdSigs = sigs, tcdMeths = maybe_meths})
  = delFVs (map hsTyVarName tyvars) $
    extractHsCtxtTyNames context	  `plusFV`
    plusFVs (map extractFunDepNames fds)  `plusFV`
    hsSigsFVs sigs			  `plusFV`
    dm_fvs
  where
    dm_fvs = case maybe_meths of
		Nothing -> mkFVs [v | ClassOpSig _ (DefMeth v) _ _ <- sigs]
		  -- No method bindings, so this class decl comes from an interface file, 
		  -- So we want to treat the default-method names as free (they should
		  -- be defined somewhere else).  [In source code this is not so; the class
		  -- decl will bind whatever default-methods are necessary.]
		Just _ -> emptyFVs	-- Source code, so the default methods
					-- are *bound* not *free*

----------------
hsSigsFVs sigs = plusFVs (map hsSigFVs sigs)

hsSigFVs (Sig v ty _) 	    = extractHsTyNames ty
hsSigFVs (SpecInstSig ty _) = extractHsTyNames ty
hsSigFVs (SpecSig v ty _)   = extractHsTyNames ty
hsSigFVs (ClassOpSig _ _ ty _) = extractHsTyNames ty
hsSigFVs other		    = emptyFVs

----------------
instDeclFVs (InstDecl inst_ty _ _ maybe_dfun _)
  = extractHsTyNames inst_ty	`plusFV` 
    (case maybe_dfun of { Just n -> unitFV n; Nothing -> emptyFVs })

----------------
ruleDeclFVs (HsRule _ _ _ _ _ _) = emptyFVs
ruleDeclFVs (IfaceRuleOut _ _)	 = emptyFVs
ruleDeclFVs (IfaceRule _ _ vars var args rhs _)
  = unitFV var `plusFV`
    (delFVs (map ufBinderName vars) $
     ufExprFVs rhs `plusFV` plusFVs (map ufExprFVs args))

----------------
conDeclFVs (ConDecl _ tyvars context details _)
  = delFVs (map hsTyVarName tyvars) $
    extractHsCtxtTyNames context	  `plusFV`
    conDetailsFVs details

conDetailsFVs (PrefixCon btys)    = plusFVs (map bangTyFVs btys)
conDetailsFVs (InfixCon bty1 bty2) = bangTyFVs bty1 `plusFV` bangTyFVs bty2
conDetailsFVs (RecCon flds)	   = plusFVs [bangTyFVs bty | (_, bty) <- flds]

bangTyFVs bty = extractHsTyNames (getBangType bty)

----------------
hsIdInfoFVs (HsUnfold _ unf) = ufExprFVs unf
hsIdInfoFVs (HsWorker n a)   = unitFV n
hsIdInfoFVs other	     = emptyFVs

----------------
ufExprFVs (UfVar n) 	  = unitFV n
ufExprFVs (UfLit l) 	  = emptyFVs
ufExprFVs (UfFCall cc ty) = extractHsTyNames ty
ufExprFVs (UfType ty)     = extractHsTyNames ty
ufExprFVs (UfTuple tc es) = hsTupConFVs tc `plusFV` plusFVs (map ufExprFVs es)
ufExprFVs (UfLam v e)     = ufBndrFVs v (ufExprFVs e)
ufExprFVs (UfApp e1 e2)   = ufExprFVs e1 `plusFV` ufExprFVs e2
ufExprFVs (UfCase e n as) = ufExprFVs e `plusFV` delFV n (plusFVs (map ufAltFVs as))
ufExprFVs (UfNote n e)	  = ufNoteFVs n `plusFV` ufExprFVs e
ufExprFVs (UfLet (UfNonRec b r) e) = ufExprFVs r `plusFV` ufBndrFVs b (ufExprFVs e)
ufExprFVs (UfLet (UfRec prs)    e) = foldr ufBndrFVs 
					   (foldr (plusFV . ufExprFVs . snd) (ufExprFVs e) prs)
					   (map fst prs) 

ufBndrFVs (UfValBinder n ty) fvs = extractHsTyNames ty `plusFV` delFV n fvs
ufBndrFVs (UfTyBinder  n k)  fvs = delFV n fvs

ufAltFVs (con, vs, e) = ufConFVs con `plusFV` delFVs vs (ufExprFVs e)

ufConFVs (UfDataAlt n)      = unitFV n
ufConFVs (UfTupleAlt t)     = hsTupConFVs t
ufConFVs other		    = emptyFVs

ufNoteFVs (UfCoerce ty) = extractHsTyNames ty
ufNoteFVs note		= emptyFVs

hsTupConFVs (HsTupCon bx n) = unitFV (tyConName (tupleTyCon bx n))
	-- Always return the TyCon; that'll suck in the data con
\end{code}


%************************************************************************
%*									*
\subsection{A few functions on generic defintions
%*									*
%************************************************************************

These functions on generics are defined over RenamedMatches, which is
why they are here and not in HsMatches.

\begin{code}
maybeGenericMatch :: RenamedMatch -> Maybe (RenamedHsType, RenamedMatch)
  -- Tells whether a Match is for a generic definition
  -- and extract the type from a generic match and put it at the front

maybeGenericMatch (Match (TypePat ty : pats) sig_ty grhss)
  = Just (ty, Match pats sig_ty grhss)

maybeGenericMatch other_match = Nothing
\end{code}
