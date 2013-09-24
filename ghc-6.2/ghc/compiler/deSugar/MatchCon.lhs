
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[MatchCon]{Pattern-matching constructors}

\begin{code}
module MatchCon ( matchConFamily ) where

#include "HsVersions.h"

import {-# SOURCE #-} Match	( match )

import HsSyn		( Pat(..), HsConDetails(..) )

import DsMonad
import DsUtils

import Id		( Id )
import Subst		( mkSubst, mkInScopeSet, bindSubst, substExpr )
import CoreFVs		( exprFreeVars )
import VarEnv		( emptySubstEnv )
import ListSetOps	( equivClassesByUniq )
import Unique		( Uniquable(..) )
\end{code}

We are confronted with the first column of patterns in a set of
equations, all beginning with constructors from one ``family'' (e.g.,
@[]@ and @:@ make up the @List@ ``family'').  We want to generate the
alternatives for a @Case@ expression.  There are several choices:
\begin{enumerate}
\item
Generate an alternative for every constructor in the family, whether
they are used in this set of equations or not; this is what the Wadler
chapter does.
\begin{description}
\item[Advantages:]
(a)~Simple.  (b)~It may also be that large sparsely-used constructor
families are mainly handled by the code for literals.
\item[Disadvantages:]
(a)~Not practical for large sparsely-used constructor families, e.g.,
the ASCII character set.  (b)~Have to look up a list of what
constructors make up the whole family.
\end{description}

\item
Generate an alternative for each constructor used, then add a default
alternative in case some constructors in the family weren't used.
\begin{description}
\item[Advantages:]
(a)~Alternatives aren't generated for unused constructors.  (b)~The
STG is quite happy with defaults.  (c)~No lookup in an environment needed.
\item[Disadvantages:]
(a)~A spurious default alternative may be generated.
\end{description}

\item
``Do it right:'' generate an alternative for each constructor used,
and add a default alternative if all constructors in the family
weren't used.
\begin{description}
\item[Advantages:]
(a)~You will get cases with only one alternative (and no default),
which should be amenable to optimisation.  Tuples are a common example.
\item[Disadvantages:]
(b)~Have to look up constructor families in TDE (as above).
\end{description}
\end{enumerate}

We are implementing the ``do-it-right'' option for now.  The arguments
to @matchConFamily@ are the same as to @match@; the extra @Int@
returned is the number of constructors in the family.

The function @matchConFamily@ is concerned with this
have-we-used-all-the-constructors? question; the local function
@match_cons_used@ does all the real work.
\begin{code}
matchConFamily :: [Id]
	       -> [EquationInfo]
	       -> DsM MatchResult

matchConFamily (var:vars) eqns_info
  = let
	-- Sort into equivalence classes by the unique on the constructor
	-- All the EqnInfos should start with a ConPat
	eqn_groups = equivClassesByUniq get_uniq eqns_info
	get_uniq (EqnInfo _ _ (ConPatOut data_con _ _ _ _ : _) _) = getUnique data_con
    in
	-- Now make a case alternative out of each group
    mapDs (match_con vars) eqn_groups	`thenDs` \ alts ->

    returnDs (mkCoAlgCaseMatchResult var alts)
\end{code}

And here is the local function that does all the work.  It is
more-or-less the @matchCon@/@matchClause@ functions on page~94 in
Wadler's chapter in SLPJ.

\begin{code}
match_con vars (eqn1@(EqnInfo _ _ (ConPatOut data_con (PrefixCon arg_pats) _ ex_tvs ex_dicts : _) _)
		: other_eqns)
  = -- Make new vars for the con arguments; avoid new locals where possible
    mapDs selectMatchVar arg_pats	`thenDs` \ arg_vars ->

    -- Now do the business to make the alt for _this_ ConPat ...
    match (arg_vars ++ vars) 
	  (map shift_con_pat (eqn1:other_eqns))	`thenDs` \ match_result ->

    --		[See "notes on do_subst" below this function]
    -- Make the ex_tvs and ex_dicts line up with those
    -- in the first pattern.  Remember, they are all guaranteed to be variables
    let
	match_result' | null ex_tvs     = match_result
		      | null other_eqns = match_result
		      | otherwise       = adjustMatchResult do_subst match_result
    in
	
    returnDs (data_con, ex_tvs ++ ex_dicts ++ arg_vars, match_result')
  where
    shift_con_pat :: EquationInfo -> EquationInfo
    shift_con_pat (EqnInfo n ctx (ConPatOut _ (PrefixCon arg_pats) _ _ _ : pats) match_result)
      = EqnInfo n ctx (arg_pats ++ pats) match_result

    other_pats = [p | EqnInfo _ _ (p:_) _ <- other_eqns]

    var_prs = concat [ (ex_tvs'   `zip` ex_tvs) ++ 
		       (ex_dicts' `zip` ex_dicts) 
		     | ConPatOut _ _ _ ex_tvs' ex_dicts' <- other_pats ]

    do_subst e = substExpr subst e
	       where
		 subst    = foldl (\ s (v', v) -> bindSubst s v' v) in_scope var_prs
		 in_scope = mkSubst (mkInScopeSet (exprFreeVars e)) emptySubstEnv
			-- We put all the free variables of e into the in-scope 
			-- set of the substitution, not because it is necessary,
			-- but to suppress the warning in Subst.lookupInScope
			-- Tiresome, but doing the substitution at all is rare.
\end{code}

Note on @shift_con_pats@ just above: does what the list comprehension in
@matchClause@ (SLPJ, p.~94) does, except things are trickier in real
life.  Works for @ConPats@, and we want it to fail catastrophically
for anything else (which a list comprehension wouldn't).
Cf.~@shift_lit_pats@ in @MatchLits@.


Notes on do_subst stuff
~~~~~~~~~~~~~~~~~~~~~~~
Consider
	data T = forall a. Ord a => T a (a->Int)

	f (T x f) True  = ...expr1...
	f (T y g) False = ...expr2..

When we put in the tyvars etc we get

	f (T a (d::Ord a) (x::a) (f::a->Int)) True =  ...expr1...
	f (T b (e::Ord a) (y::a) (g::a->Int)) True =  ...expr2...

After desugaring etc we'll get a single case:

	f = \t::T b::Bool -> 
	    case t of
	       T a (d::Ord a) (x::a) (f::a->Int)) ->
	    case b of
		True  -> ...expr1...
		False -> ...expr2...

*** We have to substitute [a/b, d/e] in expr2! **
That is what do_subst is doing.

Originally I tried to use 
	(\b -> let e = d in expr2) a 
to do this substitution.  While this is "correct" in a way, it fails
Lint, because e::Ord b but d::Ord a.  

So now I simply do the substitution properly using substExpr.

