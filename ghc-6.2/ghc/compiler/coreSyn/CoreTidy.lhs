%
% (c) The AQUA Project, Glasgow University, 1996-1998
%
%************************************************************************
%*									*
\section[PprCore]{Printing of Core syntax, including for interfaces}
%*									*
%************************************************************************

\begin{code}
module CoreTidy (
	tidyBind, tidyExpr, 
	tidyBndr, tidyBndrs, tidyVarOcc,
	tidyIdRules, pprTidyIdRules
    ) where

#include "HsVersions.h"

import CoreSyn
import CoreUtils	( exprArity )
import PprCore		( pprIdRules )
import Id		( Id, mkUserLocal, idInfo, setIdInfo, idUnique,
			  idType, idCoreRules )
import IdInfo		( setArityInfo, vanillaIdInfo,
			  newStrictnessInfo, setAllStrictnessInfo,
			  newDemandInfo, setNewDemandInfo )
import Type		( tidyType, tidyTyVarBndr )
import Var		( Var )
import VarEnv
import Name		( getOccName )
import OccName		( tidyOccName )
import SrcLoc		( noSrcLoc )
import Maybes		( orElse )
import Outputable
import Util		( mapAccumL )
\end{code}


This module contains "tidying" code for *nested* expressions, bindings, rules.
The code for *top-level* bindings is in TidyPgm.

%************************************************************************
%*									*
\subsection{Tidying expressions, rules}
%*									*
%************************************************************************

\begin{code}
tidyBind :: TidyEnv
	 -> CoreBind
	 ->  (TidyEnv, CoreBind)

tidyBind env (NonRec bndr rhs)
  = tidyLetBndr env (bndr,rhs) =: \ (env', bndr') ->
    (env', NonRec bndr' (tidyExpr env' rhs))

tidyBind env (Rec prs)
  = mapAccumL tidyLetBndr  env prs	=: \ (env', bndrs') ->
    map (tidyExpr env') (map snd prs)	=: \ rhss' ->
    (env', Rec (zip bndrs' rhss'))


------------  Expressions  --------------
tidyExpr :: TidyEnv -> CoreExpr -> CoreExpr
tidyExpr env (Var v)   	=  Var (tidyVarOcc env v)
tidyExpr env (Type ty) 	=  Type (tidyType env ty)
tidyExpr env (Lit lit) 	=  Lit lit
tidyExpr env (App f a) 	=  App (tidyExpr env f) (tidyExpr env a)
tidyExpr env (Note n e) =  Note (tidyNote env n) (tidyExpr env e)

tidyExpr env (Let b e) 
  = tidyBind env b 	=: \ (env', b') ->
    Let b' (tidyExpr env' e)

tidyExpr env (Case e b alts)
  = tidyBndr env b 	=: \ (env', b) ->
    Case (tidyExpr env e) b (map (tidyAlt env') alts)

tidyExpr env (Lam b e)
  = tidyBndr env b 	=: \ (env', b) ->
    Lam b (tidyExpr env' e)

------------  Case alternatives  --------------
tidyAlt env (con, vs, rhs)
  = tidyBndrs env vs 	=: \ (env', vs) ->
    (con, vs, tidyExpr env' rhs)

------------  Notes  --------------
tidyNote env (Coerce t1 t2)  = Coerce (tidyType env t1) (tidyType env t2)
tidyNote env note            = note


------------  Rules  --------------
tidyIdRules :: TidyEnv -> [IdCoreRule] -> [IdCoreRule]
tidyIdRules env [] = []
tidyIdRules env ((fn,rule) : rules)
  = tidyRule env rule  		=: \ rule ->
    tidyIdRules env rules 	=: \ rules ->
     ((tidyVarOcc env fn, rule) : rules)

tidyRule :: TidyEnv -> CoreRule -> CoreRule
tidyRule env rule@(BuiltinRule _ _) = rule
tidyRule env (Rule name act vars tpl_args rhs)
  = tidyBndrs env vars			=: \ (env', vars) ->
    map (tidyExpr env') tpl_args  	=: \ tpl_args ->
     (Rule name act vars tpl_args (tidyExpr env' rhs))

pprTidyIdRules :: Id -> SDoc
pprTidyIdRules id = pprIdRules (tidyIdRules emptyTidyEnv (idCoreRules id))
\end{code}


%************************************************************************
%*									*
\subsection{Tidying non-top-level binders}
%*									*
%************************************************************************

\begin{code}
tidyVarOcc :: TidyEnv -> Var -> Var
tidyVarOcc (_, var_env) v = lookupVarEnv var_env v `orElse` v

-- tidyBndr is used for lambda and case binders
tidyBndr :: TidyEnv -> Var -> (TidyEnv, Var)
tidyBndr env var
  | isTyVar var = tidyTyVarBndr env var
  | otherwise   = tidyIdBndr env var

tidyBndrs :: TidyEnv -> [Var] -> (TidyEnv, [Var])
tidyBndrs env vars = mapAccumL tidyBndr env vars

tidyLetBndr :: TidyEnv -> (Id, CoreExpr) -> (TidyEnv, Var)
-- Used for local (non-top-level) let(rec)s
tidyLetBndr env (id,rhs) 
  = ((tidy_env,new_var_env), final_id)
  where
    ((tidy_env,var_env), new_id) = tidyIdBndr env id

	-- We need to keep around any interesting strictness and
	-- demand info because later on we may need to use it when
	-- converting to A-normal form.
	-- eg.
	--	f (g x),  where f is strict in its argument, will be converted
	--	into  case (g x) of z -> f z  by CorePrep, but only if f still
	-- 	has its strictness info.
	--
	-- Similarly for the demand info - on a let binder, this tells 
	-- CorePrep to turn the let into a case.
	--
	-- Similarly arity info for eta expansion in CorePrep
	--
    final_id = new_id `setIdInfo` new_info
    idinfo   = idInfo id
    new_info = vanillaIdInfo
		`setArityInfo`		exprArity rhs
		`setAllStrictnessInfo`	newStrictnessInfo idinfo
		`setNewDemandInfo`	newDemandInfo idinfo

    -- Override the env we get back from tidyId with the new IdInfo
    -- so it gets propagated to the usage sites.
    new_var_env = extendVarEnv var_env id final_id

-- Non-top-level variables
tidyIdBndr :: TidyEnv -> Id -> (TidyEnv, Id)
tidyIdBndr env@(tidy_env, var_env) id
  = -- do this pattern match strictly, otherwise we end up holding on to
    -- stuff in the OccName.
    case tidyOccName tidy_env (getOccName id) of { (tidy_env', occ') -> 
    let 
	-- Give the Id a fresh print-name, *and* rename its type
	-- The SrcLoc isn't important now, 
	-- though we could extract it from the Id
	-- 
	-- All nested Ids now have the same IdInfo, namely vanillaIdInfo,
	-- which should save some space.
	-- But note that tidyLetBndr puts some of it back.
        ty'          	  = tidyType env (idType id)
	id'          	  = mkUserLocal occ' (idUnique id) ty' noSrcLoc
				`setIdInfo` vanillaIdInfo
	var_env'	  = extendVarEnv var_env id id'
    in
     ((tidy_env', var_env'), id')
   }
\end{code}

\begin{code}
m =: k = m `seq` k m
\end{code}
