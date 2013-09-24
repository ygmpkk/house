%
% (c) The AQUA Project, Glasgow University, 1993-1998
%
\section[Simplify]{The main module of the simplifier}

\begin{code}
module Simplify ( simplTopBinds, simplExpr ) where

#include "HsVersions.h"

import CmdLineOpts	( dopt, DynFlag(Opt_D_dump_inlinings),
			  SimplifierSwitch(..)
			)
import SimplMonad
import SimplUtils	( mkCase, mkLam, newId, prepareAlts,
			  simplBinder, simplBinders, simplLamBndrs, simplRecBndrs, simplLetBndr,
			  SimplCont(..), DupFlag(..), LetRhsFlag(..), 
			  mkStop, mkBoringStop,  pushContArgs,
			  contResultType, countArgs, contIsDupable, contIsRhsOrArg,
			  getContArgs, interestingCallContext, interestingArg, isStrictType
			)
import Var		( mustHaveLocalBinding )
import VarEnv
import Id		( Id, idType, idInfo, idArity, isDataConWorkId, 
			  setIdUnfolding, isDeadBinder,
			  idNewDemandInfo, setIdInfo,
			  setIdOccInfo, zapLamIdInfo, setOneShotLambda, 
			)
import OccName		( encodeFS )
import IdInfo		( OccInfo(..), isLoopBreaker,
			  setArityInfo, zapDemandInfo,
			  setUnfoldingInfo, 
			  occInfo
			)
import NewDemand	( isStrictDmd )
import DataCon		( dataConNumInstArgs, dataConRepStrictness )
import CoreSyn
import PprCore		( pprParendExpr, pprCoreExpr )
import CoreUnfold	( mkOtherCon, mkUnfolding, callSiteInline )
import CoreUtils	( exprIsDupable, exprIsTrivial, needsCaseBinding,
			  exprIsConApp_maybe, mkPiTypes, findAlt, 
			  exprType, exprIsValue, 
			  exprOkForSpeculation, exprArity, 
			  mkCoerce, mkCoerce2, mkSCC, mkInlineMe, mkAltExpr, applyTypeToArg
			)
import Rules		( lookupRule )
import BasicTypes	( isMarkedStrict )
import CostCentre	( currentCCS )
import Type		( isUnLiftedType, seqType, tyConAppArgs, funArgTy,
			  splitFunTy_maybe, splitFunTy, eqType
			)
import Subst		( mkSubst, substTy, substExpr, 
			  isInScope, lookupIdSubst, simplIdInfo
			)
import TysPrim		( realWorldStatePrimTy )
import PrelInfo		( realWorldPrimId )
import BasicTypes	( TopLevelFlag(..), isTopLevel, 
			  RecFlag(..), isNonRec
			)
import OrdList
import Maybe		( Maybe )
import Maybes		( orElse )
import Outputable
import Util             ( notNull )
\end{code}


The guts of the simplifier is in this module, but the driver loop for
the simplifier is in SimplCore.lhs.


-----------------------------------------
	*** IMPORTANT NOTE ***
-----------------------------------------
The simplifier used to guarantee that the output had no shadowing, but
it does not do so any more.   (Actually, it never did!)  The reason is
documented with simplifyArgs.


-----------------------------------------
	*** IMPORTANT NOTE ***
-----------------------------------------
Many parts of the simplifier return a bunch of "floats" as well as an
expression. This is wrapped as a datatype SimplUtils.FloatsWith.

All "floats" are let-binds, not case-binds, but some non-rec lets may
be unlifted (with RHS ok-for-speculation).



-----------------------------------------
	ORGANISATION OF FUNCTIONS
-----------------------------------------
simplTopBinds
  - simplify all top-level binders
  - for NonRec, call simplRecOrTopPair
  - for Rec,    call simplRecBind

	
	------------------------------
simplExpr (applied lambda)	==> simplNonRecBind
simplExpr (Let (NonRec ...) ..) ==> simplNonRecBind
simplExpr (Let (Rec ...)    ..) ==> simplify binders; simplRecBind

	------------------------------
simplRecBind	[binders already simplfied]
  - use simplRecOrTopPair on each pair in turn

simplRecOrTopPair [binder already simplified]
  Used for: recursive bindings (top level and nested)
	    top-level non-recursive bindings
  Returns: 
  - check for PreInlineUnconditionally
  - simplLazyBind

simplNonRecBind
  Used for: non-top-level non-recursive bindings
	    beta reductions (which amount to the same thing)
  Because it can deal with strict arts, it takes a 
	"thing-inside" and returns an expression

  - check for PreInlineUnconditionally
  - simplify binder, including its IdInfo
  - if strict binding
	simplStrictArg
	mkAtomicArgs
	completeNonRecX
    else
	simplLazyBind
	addFloats

simplNonRecX:	[given a *simplified* RHS, but an *unsimplified* binder]
  Used for: binding case-binder and constr args in a known-constructor case
  - check for PreInLineUnconditionally
  - simplify binder
  - completeNonRecX
 
	------------------------------
simplLazyBind:	[binder already simplified, RHS not]
  Used for: recursive bindings (top level and nested)
	    top-level non-recursive bindings
	    non-top-level, but *lazy* non-recursive bindings
	[must not be strict or unboxed]
  Returns floats + an augmented environment, not an expression
  - substituteIdInfo and add result to in-scope 
	[so that rules are available in rec rhs]
  - simplify rhs
  - mkAtomicArgs
  - float if exposes constructor or PAP
  - completeLazyBind


completeNonRecX:	[binder and rhs both simplified]
  - if the the thing needs case binding (unlifted and not ok-for-spec)
	build a Case
   else
	completeLazyBind
	addFloats

completeLazyBind: 	[given a simplified RHS]
	[used for both rec and non-rec bindings, top level and not]
  - try PostInlineUnconditionally
  - add unfolding [this is the only place we add an unfolding]
  - add arity



Right hand sides and arguments
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In many ways we want to treat 
	(a) the right hand side of a let(rec), and 
	(b) a function argument
in the same way.  But not always!  In particular, we would
like to leave these arguments exactly as they are, so they
will match a RULE more easily.
	
	f (g x, h x)	
	g (+ x)

It's harder to make the rule match if we ANF-ise the constructor,
or eta-expand the PAP:

	f (let { a = g x; b = h x } in (a,b))
	g (\y. + x y)

On the other hand if we see the let-defns

	p = (g x, h x)
	q = + x

then we *do* want to ANF-ise and eta-expand, so that p and q
can be safely inlined.   

Even floating lets out is a bit dubious.  For let RHS's we float lets
out if that exposes a value, so that the value can be inlined more vigorously.
For example

	r = let x = e in (x,x)

Here, if we float the let out we'll expose a nice constructor. We did experiments
that showed this to be a generally good thing.  But it was a bad thing to float
lets out unconditionally, because that meant they got allocated more often.

For function arguments, there's less reason to expose a constructor (it won't
get inlined).  Just possibly it might make a rule match, but I'm pretty skeptical.
So for the moment we don't float lets out of function arguments either.


Eta expansion
~~~~~~~~~~~~~~
For eta expansion, we want to catch things like

	case e of (a,b) -> \x -> case a of (p,q) -> \y -> r

If the \x was on the RHS of a let, we'd eta expand to bring the two
lambdas together.  And in general that's a good thing to do.  Perhaps
we should eta expand wherever we find a (value) lambda?  Then the eta
expansion at a let RHS can concentrate solely on the PAP case.


%************************************************************************
%*									*
\subsection{Bindings}
%*									*
%************************************************************************

\begin{code}
simplTopBinds :: SimplEnv -> [InBind] -> SimplM [OutBind]

simplTopBinds env binds
  = 	-- Put all the top-level binders into scope at the start
	-- so that if a transformation rule has unexpectedly brought
	-- anything into scope, then we don't get a complaint about that.
	-- It's rather as if the top-level binders were imported.
    simplRecBndrs env (bindersOfBinds binds)	`thenSmpl` \ (env, bndrs') -> 
    simpl_binds env binds bndrs'		`thenSmpl` \ (floats, _) ->
    freeTick SimplifierDone			`thenSmpl_`
    returnSmpl (floatBinds floats)
  where
	-- We need to track the zapped top-level binders, because
	-- they should have their fragile IdInfo zapped (notably occurrence info)
	-- That's why we run down binds and bndrs' simultaneously.
    simpl_binds :: SimplEnv -> [InBind] -> [OutId] -> SimplM (FloatsWith ())
    simpl_binds env []		 bs = ASSERT( null bs ) returnSmpl (emptyFloats env, ())
    simpl_binds env (bind:binds) bs = simpl_bind env bind bs 		`thenSmpl` \ (floats,env) ->
				      addFloats env floats		$ \env -> 
				      simpl_binds env binds (drop_bs bind bs)

    drop_bs (NonRec _ _) (_ : bs) = bs
    drop_bs (Rec prs)    bs	  = drop (length prs) bs

    simpl_bind env bind bs 
      = getDOptsSmpl				`thenSmpl` \ dflags ->
        if dopt Opt_D_dump_inlinings dflags then
	   pprTrace "SimplBind" (ppr (bindersOf bind)) $ simpl_bind1 env bind bs
	else
	   simpl_bind1 env bind bs

    simpl_bind1 env (NonRec b r) (b':_) = simplRecOrTopPair env TopLevel b b' r
    simpl_bind1 env (Rec pairs)  bs'    = simplRecBind      env TopLevel pairs bs'
\end{code}


%************************************************************************
%*									*
\subsection{simplNonRec}
%*									*
%************************************************************************

simplNonRecBind is used for
  * non-top-level non-recursive lets in expressions
  * beta reduction

It takes 
  * An unsimplified (binder, rhs) pair
  * The env for the RHS.  It may not be the same as the
	current env because the bind might occur via (\x.E) arg

It uses the CPS form because the binding might be strict, in which
case we might discard the continuation:
	let x* = error "foo" in (...x...)

It needs to turn unlifted bindings into a @case@.  They can arise
from, say: 	(\x -> e) (4# + 3#)

\begin{code}
simplNonRecBind :: SimplEnv
		-> InId 				-- Binder
	  	-> InExpr -> SimplEnv			-- Arg, with its subst-env
	  	-> OutType				-- Type of thing computed by the context
	  	-> (SimplEnv -> SimplM FloatsWithExpr)	-- The body
	  	-> SimplM FloatsWithExpr
#ifdef DEBUG
simplNonRecBind env bndr rhs rhs_se cont_ty thing_inside
  | isTyVar bndr
  = pprPanic "simplNonRecBind" (ppr bndr <+> ppr rhs)
#endif

simplNonRecBind env bndr rhs rhs_se cont_ty thing_inside
  | preInlineUnconditionally env NotTopLevel bndr
  = tick (PreInlineUnconditionally bndr)		`thenSmpl_`
    thing_inside (extendSubst env bndr (ContEx (getSubstEnv rhs_se) rhs))


  | isStrictDmd (idNewDemandInfo bndr) || isStrictType (idType bndr)	-- A strict let
  =  	-- Don't use simplBinder because that doesn't keep 
	-- fragile occurrence info in the substitution
    simplLetBndr env bndr					`thenSmpl` \ (env, bndr1) ->
    simplStrictArg AnRhs env rhs rhs_se (idType bndr1) cont_ty	$ \ env1 rhs1 ->

	-- Now complete the binding and simplify the body
    let
	-- simplLetBndr doesn't deal with the IdInfo, so we must
	-- do so here (c.f. simplLazyBind)
	bndr2  = bndr1 `setIdInfo` simplIdInfo (getSubst env) (idInfo bndr)
	env2   = modifyInScope env1 bndr2 bndr2
    in
    completeNonRecX env2 True {- strict -} bndr bndr2 rhs1 thing_inside

  | otherwise							-- Normal, lazy case
  =  	-- Don't use simplBinder because that doesn't keep 
	-- fragile occurrence info in the substitution
    simplLetBndr env bndr				`thenSmpl` \ (env, bndr') ->
    simplLazyBind env NotTopLevel NonRecursive
		  bndr bndr' rhs rhs_se 		`thenSmpl` \ (floats, env) ->
    addFloats env floats thing_inside
\end{code}

A specialised variant of simplNonRec used when the RHS is already simplified, notably
in knownCon.  It uses case-binding where necessary.

\begin{code}
simplNonRecX :: SimplEnv
	     -> InId 		-- Old binder
	     -> OutExpr		-- Simplified RHS
	     -> (SimplEnv -> SimplM FloatsWithExpr)
	     -> SimplM FloatsWithExpr

simplNonRecX env bndr new_rhs thing_inside
  | needsCaseBinding (idType bndr) new_rhs
	-- Make this test *before* the preInlineUnconditionally
	-- Consider 	case I# (quotInt# x y) of 
	--		  I# v -> let w = J# v in ...
	-- If we gaily inline (quotInt# x y) for v, we end up building an
	-- extra thunk:
	--		  let w = J# (quotInt# x y) in ...
	-- because quotInt# can fail.
  = simplBinder env bndr	`thenSmpl` \ (env, bndr') ->
    thing_inside env 		`thenSmpl` \ (floats, body) ->
    returnSmpl (emptyFloats env, Case new_rhs bndr' [(DEFAULT, [], wrapFloats floats body)])

  | preInlineUnconditionally env NotTopLevel  bndr
  	-- This happens; for example, the case_bndr during case of
	-- known constructor:  case (a,b) of x { (p,q) -> ... }
	-- Here x isn't mentioned in the RHS, so we don't want to
	-- create the (dead) let-binding  let x = (a,b) in ...
	--
	-- Similarly, single occurrences can be inlined vigourously
	-- e.g.  case (f x, g y) of (a,b) -> ....
	-- If a,b occur once we can avoid constructing the let binding for them.
  = thing_inside (extendSubst env bndr (ContEx emptySubstEnv new_rhs))

  | otherwise
  = simplBinder env bndr	`thenSmpl` \ (env, bndr') ->
    completeNonRecX env False {- Non-strict; pessimistic -} 
		    bndr bndr' new_rhs thing_inside

completeNonRecX env is_strict old_bndr new_bndr new_rhs thing_inside
  = mkAtomicArgs is_strict 
		 True {- OK to float unlifted -} 
		 new_rhs			`thenSmpl` \ (aux_binds, rhs2) ->

	-- Make the arguments atomic if necessary, 
	-- adding suitable bindings
    addAtomicBindsE env (fromOL aux_binds)	$ \ env ->
    completeLazyBind env NotTopLevel
		     old_bndr new_bndr rhs2	`thenSmpl` \ (floats, env) ->
    addFloats env floats thing_inside
\end{code}


%************************************************************************
%*									*
\subsection{Lazy bindings}
%*									*
%************************************************************************

simplRecBind is used for
	* recursive bindings only

\begin{code}
simplRecBind :: SimplEnv -> TopLevelFlag
	     -> [(InId, InExpr)] -> [OutId]
	     -> SimplM (FloatsWith SimplEnv)
simplRecBind env top_lvl pairs bndrs'
  = go env pairs bndrs'		`thenSmpl` \ (floats, env) ->
    returnSmpl (flattenFloats floats, env)
  where
    go env [] _ = returnSmpl (emptyFloats env, env)
	
    go env ((bndr, rhs) : pairs) (bndr' : bndrs')
	= simplRecOrTopPair env top_lvl bndr bndr' rhs 	`thenSmpl` \ (floats, env) ->
	  addFloats env floats (\env -> go env pairs bndrs')
\end{code}


simplRecOrTopPair is used for
	* recursive bindings (whether top level or not)
	* top-level non-recursive bindings

It assumes the binder has already been simplified, but not its IdInfo.

\begin{code}
simplRecOrTopPair :: SimplEnv
	     	  -> TopLevelFlag
	     	  -> InId -> OutId		-- Binder, both pre-and post simpl
	     	  -> InExpr 			-- The RHS and its environment
	     	  -> SimplM (FloatsWith SimplEnv)

simplRecOrTopPair env top_lvl bndr bndr' rhs
  | preInlineUnconditionally env top_lvl bndr  	-- Check for unconditional inline
  = tick (PreInlineUnconditionally bndr)	`thenSmpl_`
    returnSmpl (emptyFloats env, extendSubst env bndr (ContEx (getSubstEnv env) rhs))

  | otherwise
  = simplLazyBind env top_lvl Recursive bndr bndr' rhs env
	-- May not actually be recursive, but it doesn't matter
\end{code}


simplLazyBind is used for
	* recursive bindings (whether top level or not)
	* top-level non-recursive bindings
	* non-top-level *lazy* non-recursive bindings

[Thus it deals with the lazy cases from simplNonRecBind, and all cases
from SimplRecOrTopBind]

Nota bene:
    1. It assumes that the binder is *already* simplified, 
       and is in scope, but not its IdInfo

    2. It assumes that the binder type is lifted.

    3. It does not check for pre-inline-unconditionallly;
       that should have been done already.

\begin{code}
simplLazyBind :: SimplEnv
	      -> TopLevelFlag -> RecFlag
	      -> InId -> OutId		-- Binder, both pre-and post simpl
	      -> InExpr -> SimplEnv 	-- The RHS and its environment
	      -> SimplM (FloatsWith SimplEnv)

simplLazyBind env top_lvl is_rec bndr bndr1 rhs rhs_se
  = let	-- Transfer the IdInfo of the original binder to the new binder
	-- This is crucial: we must preserve
	--	strictness
	--	rules
	--	worker info
	-- etc.  To do this we must apply the current substitution, 
	-- which incorporates earlier substitutions in this very letrec group.
	--
	-- NB 1.  We do this *before* processing the RHS of the binder, so that
	-- its substituted rules are visible in its own RHS.
	-- This is important.  Manuel found cases where he really, really
	-- wanted a RULE for a recursive function to apply in that function's
 	-- own right-hand side.
	--
	-- NB 2: We do not transfer the arity (see Subst.substIdInfo)
	-- The arity of an Id should not be visible
	-- in its own RHS, else we eta-reduce
	--	f = \x -> f x
	-- to
	--	f = f
	-- which isn't sound.  And it makes the arity in f's IdInfo greater than
	-- the manifest arity, which isn't good.
	-- The arity will get added later.
	--
	-- NB 3: It's important that we *do* transer the loop-breaker OccInfo,
	-- because that's what stops the Id getting inlined infinitely, in the body
	-- of the letrec.

	-- NB 4: does no harm for non-recursive bindings

	bndr2  	  	  = bndr1 `setIdInfo` simplIdInfo (getSubst env) (idInfo bndr)
	env1	     	  = modifyInScope env bndr2 bndr2
	rhs_env      	  = setInScope rhs_se env1
 	is_top_level	  = isTopLevel top_lvl
	ok_float_unlifted = not is_top_level && isNonRec is_rec
	rhs_cont	  = mkStop (idType bndr1) AnRhs
    in
  	-- Simplify the RHS; note the mkStop, which tells 
	-- the simplifier that this is the RHS of a let.
    simplExprF rhs_env rhs rhs_cont		`thenSmpl` \ (floats, rhs1) ->

	-- If any of the floats can't be floated, give up now
	-- (The allLifted predicate says True for empty floats.)
    if (not ok_float_unlifted && not (allLifted floats)) then
	completeLazyBind env1 top_lvl bndr bndr2
			 (wrapFloats floats rhs1)
    else	

	-- ANF-ise a constructor or PAP rhs
    mkAtomicArgs False {- Not strict -} 
		 ok_float_unlifted rhs1 		`thenSmpl` \ (aux_binds, rhs2) ->

	-- If the result is a PAP, float the floats out, else wrap them
	-- By this time it's already been ANF-ised (if necessary)
    if isEmptyFloats floats && isNilOL aux_binds then	-- Shortcut a common case
	completeLazyBind env1 top_lvl bndr bndr2 rhs2

    else if is_top_level || exprIsTrivial rhs2 || exprIsValue rhs2 then
	-- 	WARNING: long dodgy argument coming up
	--	WANTED: a better way to do this
	--		
	-- We can't use "exprIsCheap" instead of exprIsValue, 
	-- because that causes a strictness bug.
	--     	   x = let y* = E in case (scc y) of { T -> F; F -> T}
	-- The case expression is 'cheap', but it's wrong to transform to
	-- 	   y* = E; x = case (scc y) of {...}
 	-- Either we must be careful not to float demanded non-values, or
	-- we must use exprIsValue for the test, which ensures that the
	-- thing is non-strict.  So exprIsValue => bindings are non-strict
	-- I think.  The WARN below tests for this.
	--
	-- We use exprIsTrivial here because we want to reveal lone variables.  
	-- E.g.  let { x = letrec { y = E } in y } in ...
	-- Here we definitely want to float the y=E defn. 
	-- exprIsValue definitely isn't right for that.
	--
	-- Again, the floated binding can't be strict; if it's recursive it'll
	-- be non-strict; if it's non-recursive it'd be inlined.
	--
	-- Note [SCC-and-exprIsTrivial]
	-- If we have
	--	y = let { x* = E } in scc "foo" x
	-- then we do *not* want to float out the x binding, because
	-- it's strict!  Fortunately, exprIsTrivial replies False to
	-- (scc "foo" x).

		-- There's a subtlety here.  There may be a binding (x* = e) in the
		-- floats, where the '*' means 'will be demanded'.  So is it safe
		-- to float it out?  Answer no, but it won't matter because
		-- we only float if (a) arg' is a WHNF, or (b) it's going to top level
		-- and so there can't be any 'will be demanded' bindings in the floats.
		-- Hence the warning
        ASSERT2( is_top_level || not (any demanded_float (floatBinds floats)), 
	         ppr (filter demanded_float (floatBinds floats)) )

	tick LetFloatFromLet			`thenSmpl_` (
	addFloats env1 floats			$ \ env2 ->
	addAtomicBinds env2 (fromOL aux_binds)	$ \ env3 ->
	completeLazyBind env3 top_lvl bndr bndr2 rhs2)

    else
	completeLazyBind env1 top_lvl bndr bndr2 (wrapFloats floats rhs1)

#ifdef DEBUG
demanded_float (NonRec b r) = isStrictDmd (idNewDemandInfo b) && not (isUnLiftedType (idType b))
		-- Unlifted-type (cheap-eagerness) lets may well have a demanded flag on them
demanded_float (Rec _)	    = False
#endif
\end{code}


%************************************************************************
%*									*
\subsection{Completing a lazy binding}
%*									*
%************************************************************************

completeLazyBind
	* deals only with Ids, not TyVars
	* takes an already-simplified binder and RHS
	* is used for both recursive and non-recursive bindings
	* is used for both top-level and non-top-level bindings

It does the following:
  - tries discarding a dead binding
  - tries PostInlineUnconditionally
  - add unfolding [this is the only place we add an unfolding]
  - add arity

It does *not* attempt to do let-to-case.  Why?  Because it is used for
	- top-level bindings (when let-to-case is impossible) 
	- many situations where the "rhs" is known to be a WHNF
		(so let-to-case is inappropriate).

\begin{code}
completeLazyBind :: SimplEnv
		 -> TopLevelFlag	-- Flag stuck into unfolding
		 -> InId 		-- Old binder
		 -> OutId		-- New binder
	         -> OutExpr		-- Simplified RHS
	   	 -> SimplM (FloatsWith SimplEnv)
-- We return a new SimplEnv, because completeLazyBind may choose to do its work
-- by extending the substitution (e.g. let x = y in ...)
-- The new binding (if any) is returned as part of the floats.
-- NB: the returned SimplEnv has the right SubstEnv, but you should
--     (as usual) use the in-scope-env from the floats

completeLazyBind env top_lvl old_bndr new_bndr new_rhs
  | postInlineUnconditionally env new_bndr occ_info new_rhs
  = 		-- Drop the binding
    tick (PostInlineUnconditionally old_bndr)	`thenSmpl_`
    returnSmpl (emptyFloats env, extendSubst env old_bndr (DoneEx new_rhs))
		-- Use the substitution to make quite, quite sure that the substitution
		-- will happen, since we are going to discard the binding

  |  otherwise
  = let
		-- Add arity info
  	new_bndr_info = idInfo new_bndr `setArityInfo` exprArity new_rhs

	-- Add the unfolding *only* for non-loop-breakers
	-- Making loop breakers not have an unfolding at all 
	-- means that we can avoid tests in exprIsConApp, for example.
	-- This is important: if exprIsConApp says 'yes' for a recursive
	-- thing, then we can get into an infinite loop

	-- If the unfolding is a value, the demand info may
	-- go pear-shaped, so we nuke it.  Example:
	--	let x = (a,b) in
	--	case x of (p,q) -> h p q x
	-- Here x is certainly demanded. But after we've nuked
	-- the case, we'll get just
	--	let x = (a,b) in h a b x
	-- and now x is not demanded (I'm assuming h is lazy)
	-- This really happens.  Similarly
	--	let f = \x -> e in ...f..f...
	-- After inling f at some of its call sites the original binding may
	-- (for example) be no longer strictly demanded.
	-- The solution here is a bit ad hoc...
	unfolding  = mkUnfolding (isTopLevel top_lvl) new_rhs
 	info_w_unf = new_bndr_info `setUnfoldingInfo` unfolding
        final_info | loop_breaker		= new_bndr_info
		   | isEvaldUnfolding unfolding = zapDemandInfo info_w_unf `orElse` info_w_unf
		   | otherwise			= info_w_unf

	final_id = new_bndr `setIdInfo` final_info
    in
		-- These seqs forces the Id, and hence its IdInfo,
		-- and hence any inner substitutions
    final_id					`seq`
    returnSmpl (unitFloat env final_id new_rhs, env)

  where 
    loop_breaker = isLoopBreaker occ_info
    old_info     = idInfo old_bndr
    occ_info     = occInfo old_info
\end{code}    



%************************************************************************
%*									*
\subsection[Simplify-simplExpr]{The main function: simplExpr}
%*									*
%************************************************************************

The reason for this OutExprStuff stuff is that we want to float *after*
simplifying a RHS, not before.  If we do so naively we get quadratic
behaviour as things float out.

To see why it's important to do it after, consider this (real) example:

	let t = f x
	in fst t
==>
	let t = let a = e1
		    b = e2
		in (a,b)
	in fst t
==>
	let a = e1
	    b = e2
	    t = (a,b)
	in
	a	-- Can't inline a this round, cos it appears twice
==>
	e1

Each of the ==> steps is a round of simplification.  We'd save a
whole round if we float first.  This can cascade.  Consider

	let f = g d
	in \x -> ...f...
==>
	let f = let d1 = ..d.. in \y -> e
	in \x -> ...f...
==>
	let d1 = ..d..
	in \x -> ...(\y ->e)...

Only in this second round can the \y be applied, and it 
might do the same again.


\begin{code}
simplExpr :: SimplEnv -> CoreExpr -> SimplM CoreExpr
simplExpr env expr = simplExprC env expr (mkStop expr_ty' AnArg)
		   where
		     expr_ty' = substTy (getSubst env) (exprType expr)
	-- The type in the Stop continuation, expr_ty', is usually not used
	-- It's only needed when discarding continuations after finding
	-- a function that returns bottom.
	-- Hence the lazy substitution


simplExprC :: SimplEnv -> CoreExpr -> SimplCont -> SimplM CoreExpr
	-- Simplify an expression, given a continuation
simplExprC env expr cont 
  = simplExprF env expr cont	`thenSmpl` \ (floats, expr) ->
    returnSmpl (wrapFloats floats expr)

simplExprF :: SimplEnv -> InExpr -> SimplCont -> SimplM FloatsWithExpr
	-- Simplify an expression, returning floated binds

simplExprF env (Var v)	        cont = simplVar env v cont
simplExprF env (Lit lit)	cont = rebuild env (Lit lit) cont
simplExprF env expr@(Lam _ _)   cont = simplLam env expr cont
simplExprF env (Note note expr) cont = simplNote env note expr cont
simplExprF env (App fun arg)    cont = simplExprF env fun (ApplyTo NoDup arg env cont)

simplExprF env (Type ty) cont
  = ASSERT( contIsRhsOrArg cont )
    simplType env ty			`thenSmpl` \ ty' ->
    rebuild env (Type ty') cont

simplExprF env (Case scrut bndr alts) cont
  | not (switchIsOn (getSwitchChecker env) NoCaseOfCase)
  = 	-- Simplify the scrutinee with a Select continuation
    simplExprF env scrut (Select NoDup bndr alts env cont)

  | otherwise
  = 	-- If case-of-case is off, simply simplify the case expression
	-- in a vanilla Stop context, and rebuild the result around it
    simplExprC env scrut case_cont	`thenSmpl` \ case_expr' ->
    rebuild env case_expr' cont
  where
    case_cont = Select NoDup bndr alts env (mkBoringStop (contResultType cont))

simplExprF env (Let (Rec pairs) body) cont
  = simplRecBndrs env (map fst pairs) 		`thenSmpl` \ (env, bndrs') -> 
	-- NB: bndrs' don't have unfoldings or rules
	-- We add them as we go down

    simplRecBind env NotTopLevel pairs bndrs' 	`thenSmpl` \ (floats, env) ->
    addFloats env floats 			$ \ env ->
    simplExprF env body cont

-- A non-recursive let is dealt with by simplNonRecBind
simplExprF env (Let (NonRec bndr rhs) body) cont
  = simplNonRecBind env bndr rhs env (contResultType cont)	$ \ env ->
    simplExprF env body cont


---------------------------------
simplType :: SimplEnv -> InType -> SimplM OutType
	-- Kept monadic just so we can do the seqType
simplType env ty
  = seqType new_ty   `seq`   returnSmpl new_ty
  where
    new_ty = substTy (getSubst env) ty
\end{code}


%************************************************************************
%*									*
\subsection{Lambdas}
%*									*
%************************************************************************

\begin{code}
simplLam env fun cont
  = go env fun cont
  where
    zap_it  = mkLamBndrZapper fun (countArgs cont)
    cont_ty = contResultType cont

      	-- Type-beta reduction
    go env (Lam bndr body) (ApplyTo _ (Type ty_arg) arg_se body_cont)
      =	ASSERT( isTyVar bndr )
	tick (BetaReduction bndr)			`thenSmpl_`
	simplType (setInScope arg_se env) ty_arg 	`thenSmpl` \ ty_arg' ->
	go (extendSubst env bndr (DoneTy ty_arg')) body body_cont

	-- Ordinary beta reduction
    go env (Lam bndr body) cont@(ApplyTo _ arg arg_se body_cont)
      = tick (BetaReduction bndr)				`thenSmpl_`
	simplNonRecBind env (zap_it bndr) arg arg_se cont_ty	$ \ env -> 
	go env body body_cont

	-- Not enough args, so there are real lambdas left to put in the result
    go env lam@(Lam _ _) cont
      = simplLamBndrs env bndrs		`thenSmpl` \ (env, bndrs') ->
	simplExpr env body 		`thenSmpl` \ body' ->
	mkLam env bndrs' body' cont	`thenSmpl` \ (floats, new_lam) ->
	addFloats env floats		$ \ env -> 
	rebuild env new_lam cont
      where
	(bndrs,body) = collectBinders lam

	-- Exactly enough args
    go env expr cont = simplExprF env expr cont

mkLamBndrZapper :: CoreExpr 	-- Function
		-> Int		-- Number of args supplied, *including* type args
		-> Id -> Id	-- Use this to zap the binders
mkLamBndrZapper fun n_args
  | n_args >= n_params fun = \b -> b		-- Enough args
  | otherwise		   = \b -> zapLamIdInfo b
  where
	-- NB: we count all the args incl type args
	-- so we must count all the binders (incl type lambdas)
    n_params (Note _ e) = n_params e
    n_params (Lam b e)  = 1 + n_params e
    n_params other	= 0::Int
\end{code}


%************************************************************************
%*									*
\subsection{Notes}
%*									*
%************************************************************************

\begin{code}
simplNote env (Coerce to from) body cont
  = let
	in_scope = getInScope env 

	addCoerce s1 k1 (CoerceIt t1 cont)
		-- 	coerce T1 S1 (coerce S1 K1 e)
		-- ==>
		--	e, 			if T1=K1
		--	coerce T1 K1 e,		otherwise
		--
		-- For example, in the initial form of a worker
		-- we may find 	(coerce T (coerce S (\x.e))) y
		-- and we'd like it to simplify to e[y/x] in one round 
		-- of simplification
	  | t1 `eqType` k1  = cont	 	-- The coerces cancel out
	  | otherwise       = CoerceIt t1 cont	-- They don't cancel, but 
						-- the inner one is redundant

	addCoerce t1t2 s1s2 (ApplyTo dup arg arg_se cont)
	  | not (isTypeArg arg),	-- This whole case only works for value args
					-- Could upgrade to have equiv thing for type apps too	
 	    Just (s1, s2) <- splitFunTy_maybe s1s2
		--	(coerce (T1->T2) (S1->S2) F) E
		-- ===> 
		--	coerce T2 S2 (F (coerce S1 T1 E))
		--
		-- t1t2 must be a function type, T1->T2, because it's applied to something
		-- but s1s2 might conceivably not be
		--
		-- When we build the ApplyTo we can't mix the out-types
		-- with the InExpr in the argument, so we simply substitute
		-- to make it all consistent.  It's a bit messy.
		-- But it isn't a common case.
	  = let 
		(t1,t2) = splitFunTy t1t2
		new_arg = mkCoerce2 s1 t1 (substExpr (mkSubst in_scope (getSubstEnv arg_se)) arg)
	    in
	    ApplyTo dup new_arg (zapSubstEnv env) (addCoerce t2 s2 cont)
			
	addCoerce to' _ cont = CoerceIt to' cont
    in
    simplType env to		`thenSmpl` \ to' ->
    simplType env from		`thenSmpl` \ from' ->
    simplExprF env body (addCoerce to' from' cont)

		
-- Hack: we only distinguish subsumed cost centre stacks for the purposes of
-- inlining.  All other CCCSs are mapped to currentCCS.
simplNote env (SCC cc) e cont
  = simplExpr (setEnclosingCC env currentCCS) e 	`thenSmpl` \ e' ->
    rebuild env (mkSCC cc e') cont

simplNote env InlineCall e cont
  = simplExprF env e (InlinePlease cont)

-- See notes with SimplMonad.inlineMode
simplNote env InlineMe e cont
  | contIsRhsOrArg cont		-- Totally boring continuation; see notes above
  =				-- Don't inline inside an INLINE expression
    simplExpr (setMode inlineMode env )  e	`thenSmpl` \ e' ->
    rebuild env (mkInlineMe e') cont

  | otherwise  	-- Dissolve the InlineMe note if there's
		-- an interesting context of any kind to combine with
		-- (even a type application -- anything except Stop)
  = simplExprF env e cont

simplNote env (CoreNote s) e cont
  = simplExpr env e    `thenSmpl` \ e' ->
    rebuild env (Note (CoreNote s) e') cont
\end{code}


%************************************************************************
%*									*
\subsection{Dealing with calls}
%*									*
%************************************************************************

\begin{code}
simplVar env var cont
  = case lookupIdSubst (getSubst env) var of
	DoneEx e	-> simplExprF (zapSubstEnv env) e cont
	ContEx se e     -> simplExprF (setSubstEnv env se) e cont
	DoneId var1 occ -> WARN( not (isInScope var1 (getSubst env)) && mustHaveLocalBinding var1,
				 text "simplVar:" <+> ppr var )
			   completeCall (zapSubstEnv env) var1 occ cont
		-- The template is already simplified, so don't re-substitute.
		-- This is VITAL.  Consider
		--	let x = e in
		--	let y = \z -> ...x... in
		--	\ x -> ...y...
		-- We'll clone the inner \x, adding x->x' in the id_subst
		-- Then when we inline y, we must *not* replace x by x' in
		-- the inlined copy!!

---------------------------------------------------------
--	Dealing with a call site

completeCall env var occ_info cont
  =     -- Simplify the arguments
    getDOptsSmpl					`thenSmpl` \ dflags ->
    let
	chkr			       = getSwitchChecker env
	(args, call_cont, inline_call) = getContArgs chkr var cont
	fn_ty			       = idType var
    in
    simplifyArgs env fn_ty args (contResultType call_cont)	$ \ env args ->

	-- Next, look for rules or specialisations that match
	--
	-- It's important to simplify the args first, because the rule-matcher
	-- doesn't do substitution as it goes.  We don't want to use subst_args
	-- (defined in the 'where') because that throws away useful occurrence info,
	-- and perhaps-very-important specialisations.
	--
	-- Some functions have specialisations *and* are strict; in this case,
	-- we don't want to inline the wrapper of the non-specialised thing; better
	-- to call the specialised thing instead.
	-- We used to use the black-listing mechanism to ensure that inlining of 
	-- the wrapper didn't occur for things that have specialisations till a 
	-- later phase, so but now we just try RULES first
	--
	-- You might think that we shouldn't apply rules for a loop breaker: 
	-- doing so might give rise to an infinite loop, because a RULE is
	-- rather like an extra equation for the function:
	--	RULE:		f (g x) y = x+y
	--	Eqn:		f a     y = a-y
	--
	-- But it's too drastic to disable rules for loop breakers.  
	-- Even the foldr/build rule would be disabled, because foldr 
	-- is recursive, and hence a loop breaker:
	--	foldr k z (build g) = g k z
	-- So it's up to the programmer: rules can cause divergence

    let
	in_scope   = getInScope env
	maybe_rule = case activeRule env of
			Nothing     -> Nothing	-- No rules apply
			Just act_fn -> lookupRule act_fn in_scope var args 
    in
    case maybe_rule of {
	Just (rule_name, rule_rhs) -> 
		tick (RuleFired rule_name)			`thenSmpl_`
		(if dopt Opt_D_dump_inlinings dflags then
		   pprTrace "Rule fired" (vcat [
			text "Rule:" <+> ftext rule_name,
			text "Before:" <+> ppr var <+> sep (map pprParendExpr args),
			text "After: " <+> pprCoreExpr rule_rhs,
			text "Cont:  " <+> ppr call_cont])
		 else
			id)		$
		simplExprF env rule_rhs call_cont ;
	
	Nothing -> 		-- No rules

	-- Next, look for an inlining
    let
	arg_infos = [ interestingArg arg | arg <- args, isValArg arg]

	interesting_cont = interestingCallContext (notNull args)
						  (notNull arg_infos)
						  call_cont

    	active_inline = activeInline env var occ_info
	maybe_inline  = callSiteInline dflags active_inline inline_call occ_info
				       var arg_infos interesting_cont
    in
    case maybe_inline of {
	Just unfolding  	-- There is an inlining!
	  ->  tick (UnfoldingDone var)		`thenSmpl_`
	      makeThatCall env var unfolding args call_cont

	;
	Nothing -> 		-- No inlining!

	-- Done
    rebuild env (mkApps (Var var) args) call_cont
    }}

makeThatCall :: SimplEnv
	     -> Id
	     -> InExpr		-- Inlined function rhs 
	     -> [OutExpr]	-- Arguments, already simplified
	     -> SimplCont	-- After the call
	     -> SimplM FloatsWithExpr
-- Similar to simplLam, but this time 
-- the arguments are already simplified
makeThatCall orig_env var fun@(Lam _ _) args cont
  = go orig_env fun args
  where
    zap_it = mkLamBndrZapper fun (length args)

      	-- Type-beta reduction
    go env (Lam bndr body) (Type ty_arg : args)
      =	ASSERT( isTyVar bndr )
	tick (BetaReduction bndr)			`thenSmpl_`
	go (extendSubst env bndr (DoneTy ty_arg)) body args

	-- Ordinary beta reduction
    go env (Lam bndr body) (arg : args)
      = tick (BetaReduction bndr)			`thenSmpl_`
	simplNonRecX env (zap_it bndr) arg 		$ \ env -> 
	go env body args

	-- Not enough args, so there are real lambdas left to put in the result
    go env fun args
      = simplExprF env fun (pushContArgs orig_env args cont)
	-- NB: orig_env; the correct environment to capture with
	-- the arguments.... env has been augmented with substitutions 
	-- from the beta reductions.

makeThatCall env var fun args cont
  = simplExprF env fun (pushContArgs env args cont)
\end{code}		   


%************************************************************************
%*									*
\subsection{Arguments}
%*									*
%************************************************************************

\begin{code}
---------------------------------------------------------
--	Simplifying the arguments of a call

simplifyArgs :: SimplEnv 
	     -> OutType				-- Type of the function
	     -> [(InExpr, SimplEnv, Bool)]	-- Details of the arguments
	     -> OutType				-- Type of the continuation
	     -> (SimplEnv -> [OutExpr] -> SimplM FloatsWithExpr)
	     -> SimplM FloatsWithExpr

-- [CPS-like because of strict arguments]

-- Simplify the arguments to a call.
-- This part of the simplifier may break the no-shadowing invariant
-- Consider
--	f (...(\a -> e)...) (case y of (a,b) -> e')
-- where f is strict in its second arg
-- If we simplify the innermost one first we get (...(\a -> e)...)
-- Simplifying the second arg makes us float the case out, so we end up with
--	case y of (a,b) -> f (...(\a -> e)...) e'
-- So the output does not have the no-shadowing invariant.  However, there is
-- no danger of getting name-capture, because when the first arg was simplified
-- we used an in-scope set that at least mentioned all the variables free in its
-- static environment, and that is enough.
--
-- We can't just do innermost first, or we'd end up with a dual problem:
--	case x of (a,b) -> f e (...(\a -> e')...)
--
-- I spent hours trying to recover the no-shadowing invariant, but I just could
-- not think of an elegant way to do it.  The simplifier is already knee-deep in
-- continuations.  We have to keep the right in-scope set around; AND we have
-- to get the effect that finding (error "foo") in a strict arg position will
-- discard the entire application and replace it with (error "foo").  Getting
-- all this at once is TOO HARD!

simplifyArgs env fn_ty args cont_ty thing_inside
  = go env fn_ty args thing_inside
  where
    go env fn_ty []	    thing_inside = thing_inside env []
    go env fn_ty (arg:args) thing_inside = simplifyArg env fn_ty arg cont_ty 		$ \ env arg' ->
					   go env (applyTypeToArg fn_ty arg') args 	$ \ env args' ->
					   thing_inside env (arg':args')

simplifyArg env fn_ty (Type ty_arg, se, _) cont_ty thing_inside
  = simplType (setInScope se env) ty_arg 	`thenSmpl` \ new_ty_arg ->
    thing_inside env (Type new_ty_arg)

simplifyArg env fn_ty (val_arg, arg_se, is_strict) cont_ty thing_inside 
  | is_strict 
  = simplStrictArg AnArg env val_arg arg_se arg_ty cont_ty thing_inside

  | otherwise	-- Lazy argument
		-- DO NOT float anything outside, hence simplExprC
		-- There is no benefit (unlike in a let-binding), and we'd
		-- have to be very careful about bogus strictness through 
		-- floating a demanded let.
  = simplExprC (setInScope arg_se env) val_arg
	       (mkStop arg_ty AnArg)		`thenSmpl` \ arg1 ->
   thing_inside env arg1
  where
    arg_ty = funArgTy fn_ty


simplStrictArg ::  LetRhsFlag
	        -> SimplEnv		-- The env of the call
		-> InExpr -> SimplEnv	-- The arg plus its env
		-> OutType		-- arg_ty: type of the argument
	        -> OutType		-- cont_ty: Type of thing computed by the context
	        -> (SimplEnv -> OutExpr -> SimplM FloatsWithExpr)	
	 			 	-- Takes an expression of type rhs_ty, 
		 			-- returns an expression of type cont_ty
					-- The env passed to this continuation is the
					-- env of the call, plus any new in-scope variables
	        -> SimplM FloatsWithExpr	-- An expression of type cont_ty

simplStrictArg is_rhs call_env arg arg_env arg_ty cont_ty thing_inside
  = simplExprF (setInScope arg_env call_env) arg
	       (ArgOf is_rhs arg_ty cont_ty (\ new_env -> thing_inside (setInScope call_env new_env)))
  -- Notice the way we use arg_env (augmented with in-scope vars from call_env) 
  --	to simplify the argument
  -- and call-env (augmented with in-scope vars from the arg) to pass to the continuation
\end{code}


%************************************************************************
%*									*
\subsection{mkAtomicArgs}
%*									*
%************************************************************************

mkAtomicArgs takes a putative RHS, checks whether it's a PAP or
constructor application and, if so, converts it to ANF, so that the 
resulting thing can be inlined more easily.  Thus
	x = (f a, g b)
becomes
	t1 = f a
	t2 = g b
	x = (t1,t2)

There are three sorts of binding context, specified by the two
boolean arguments

Strict
   OK-unlifted

N  N	Top-level or recursive			Only bind args of lifted type

N  Y	Non-top-level and non-recursive,	Bind args of lifted type, or
		but lazy			unlifted-and-ok-for-speculation

Y  Y	Non-top-level, non-recursive,		Bind all args
		 and strict (demanded)
	

For example, given

	x = MkC (y div# z)

there is no point in transforming to

	x = case (y div# z) of r -> MkC r

because the (y div# z) can't float out of the let. But if it was
a *strict* let, then it would be a good thing to do.  Hence the
context information.

\begin{code}
mkAtomicArgs :: Bool	-- A strict binding
	     -> Bool	-- OK to float unlifted args
	     -> OutExpr
	     -> SimplM (OrdList (OutId,OutExpr),  -- The floats (unusually) may include
			OutExpr)		  -- things that need case-binding,
						  -- if the strict-binding flag is on

mkAtomicArgs is_strict ok_float_unlifted rhs
  | (Var fun, args) <- collectArgs rhs,				-- It's an application
    isDataConWorkId fun || valArgCount args < idArity fun	-- And it's a constructor or PAP
  = go fun nilOL [] args	-- Have a go

  | otherwise = bale_out	-- Give up

  where
    bale_out = returnSmpl (nilOL, rhs)

    go fun binds rev_args [] 
	= returnSmpl (binds, mkApps (Var fun) (reverse rev_args))

    go fun binds rev_args (arg : args) 
	| exprIsTrivial arg 	-- Easy case
	= go fun binds (arg:rev_args) args

	| not can_float_arg	-- Can't make this arg atomic
  	= bale_out		-- ... so give up

	| otherwise	-- Don't forget to do it recursively
			-- E.g.  x = a:b:c:[]
	=  mkAtomicArgs is_strict ok_float_unlifted arg	`thenSmpl` \ (arg_binds, arg') ->
	   newId FSLIT("a") arg_ty			`thenSmpl` \ arg_id ->
	   go fun ((arg_binds `snocOL` (arg_id,arg')) `appOL` binds) 
	      (Var arg_id : rev_args) args
	where
	  arg_ty        = exprType arg
	  can_float_arg =  is_strict 
			|| not (isUnLiftedType arg_ty)
			|| (ok_float_unlifted && exprOkForSpeculation arg)


addAtomicBinds :: SimplEnv -> [(OutId,OutExpr)]
	       -> (SimplEnv -> SimplM (FloatsWith a))
	       -> SimplM (FloatsWith a)
addAtomicBinds env []         thing_inside = thing_inside env
addAtomicBinds env ((v,r):bs) thing_inside = addAuxiliaryBind env (NonRec v r) $ \ env -> 
					     addAtomicBinds env bs thing_inside

addAtomicBindsE :: SimplEnv -> [(OutId,OutExpr)]
	 	-> (SimplEnv -> SimplM FloatsWithExpr)
		-> SimplM FloatsWithExpr
-- Same again, but this time we're in an expression context,
-- and may need to do some case bindings

addAtomicBindsE env [] thing_inside 
  = thing_inside env
addAtomicBindsE env ((v,r):bs) thing_inside 
  | needsCaseBinding (idType v) r
  = addAtomicBindsE (addNewInScopeIds env [v]) bs thing_inside	`thenSmpl` \ (floats, expr) ->
    WARN( exprIsTrivial expr, ppr v <+> pprCoreExpr expr )
    returnSmpl (emptyFloats env, Case r v [(DEFAULT,[], wrapFloats floats expr)])

  | otherwise
  = addAuxiliaryBind env (NonRec v r) 	$ \ env -> 
    addAtomicBindsE env bs thing_inside
\end{code}


%************************************************************************
%*									*
\subsection{The main rebuilder}
%*									*
%************************************************************************

\begin{code}
rebuild :: SimplEnv -> OutExpr -> SimplCont -> SimplM FloatsWithExpr

rebuild env expr (Stop _ _ _)		      = rebuildDone env expr
rebuild env expr (ArgOf _ _ _ cont_fn)	      = cont_fn env expr
rebuild env expr (CoerceIt to_ty cont)	      = rebuild env (mkCoerce to_ty expr) cont
rebuild env expr (InlinePlease cont) 	      = rebuild env (Note InlineCall expr) cont
rebuild env expr (Select _ bndr alts se cont) = rebuildCase (setInScope se env) expr bndr alts cont
rebuild env expr (ApplyTo _ arg se cont)      = rebuildApp  (setInScope se env) expr arg cont

rebuildApp env fun arg cont
  = simplExpr env arg	`thenSmpl` \ arg' ->
    rebuild env (App fun arg') cont

rebuildDone env expr = returnSmpl (emptyFloats env, expr)
\end{code}


%************************************************************************
%*									*
\subsection{Functions dealing with a case}
%*									*
%************************************************************************

Blob of helper functions for the "case-of-something-else" situation.

\begin{code}
---------------------------------------------------------
-- 	Eliminate the case if possible

rebuildCase :: SimplEnv
	    -> OutExpr		-- Scrutinee
	    -> InId		-- Case binder
	    -> [InAlt]		-- Alternatives
	    -> SimplCont
	    -> SimplM FloatsWithExpr

rebuildCase env scrut case_bndr alts cont
  | Just (con,args) <- exprIsConApp_maybe scrut	
	-- Works when the scrutinee is a variable with a known unfolding
	-- as well as when it's an explicit constructor application
  = knownCon env (DataAlt con) args case_bndr alts cont

  | Lit lit <- scrut	-- No need for same treatment as constructors
			-- because literals are inlined more vigorously
  = knownCon env (LitAlt lit) [] case_bndr alts cont

  | otherwise
  = prepareAlts scrut case_bndr alts		`thenSmpl` \ (better_alts, handled_cons) -> 
	
	-- Deal with the case binder, and prepare the continuation;
	-- The new subst_env is in place
    prepareCaseCont env better_alts cont	`thenSmpl` \ (floats, (dup_cont, nondup_cont)) ->
    addFloats env floats			$ \ env ->	

	-- Deal with variable scrutinee
    simplCaseBinder env scrut case_bndr 	`thenSmpl` \ (alt_env, case_bndr', zap_occ_info) ->

	-- Deal with the case alternatives
    simplAlts alt_env zap_occ_info handled_cons
	      case_bndr' better_alts dup_cont	`thenSmpl` \ alts' ->

	-- Put the case back together
    mkCase scrut case_bndr' alts'		`thenSmpl` \ case_expr ->

	-- Notice that rebuildDone returns the in-scope set from env, not alt_env
	-- The case binder *not* scope over the whole returned case-expression
    rebuild env case_expr nondup_cont
\end{code}

simplCaseBinder checks whether the scrutinee is a variable, v.  If so,
try to eliminate uses of v in the RHSs in favour of case_bndr; that
way, there's a chance that v will now only be used once, and hence
inlined.

Note 1
~~~~~~
There is a time we *don't* want to do that, namely when
-fno-case-of-case is on.  This happens in the first simplifier pass,
and enhances full laziness.  Here's the bad case:
	f = \ y -> ...(case x of I# v -> ...(case x of ...) ... )
If we eliminate the inner case, we trap it inside the I# v -> arm,
which might prevent some full laziness happening.  I've seen this
in action in spectral/cichelli/Prog.hs:
	 [(m,n) | m <- [1..max], n <- [1..max]]
Hence the check for NoCaseOfCase.

Note 2
~~~~~~
There is another situation when we don't want to do it.  If we have

    case x of w1 { DEFAULT -> case x of w2 { A -> e1; B -> e2 }
	           ...other cases .... }

We'll perform the binder-swap for the outer case, giving

    case x of w1 { DEFAULT -> case w1 of w2 { A -> e1; B -> e2 } 
	           ...other cases .... }

But there is no point in doing it for the inner case, because w1 can't
be inlined anyway.  Furthermore, doing the case-swapping involves
zapping w2's occurrence info (see paragraphs that follow), and that
forces us to bind w2 when doing case merging.  So we get

    case x of w1 { A -> let w2 = w1 in e1
		   B -> let w2 = w1 in e2
	           ...other cases .... }

This is plain silly in the common case where w2 is dead.

Even so, I can't see a good way to implement this idea.  I tried
not doing the binder-swap if the scrutinee was already evaluated
but that failed big-time:

	data T = MkT !Int

	case v of w  { MkT x ->
	case x of x1 { I# y1 ->
	case x of x2 { I# y2 -> ...

Notice that because MkT is strict, x is marked "evaluated".  But to
eliminate the last case, we must either make sure that x (as well as
x1) has unfolding MkT y1.  THe straightforward thing to do is to do
the binder-swap.  So this whole note is a no-op.

Note 3
~~~~~~
If we replace the scrutinee, v, by tbe case binder, then we have to nuke
any occurrence info (eg IAmDead) in the case binder, because the
case-binder now effectively occurs whenever v does.  AND we have to do
the same for the pattern-bound variables!  Example:

	(case x of { (a,b) -> a }) (case x of { (p,q) -> q })

Here, b and p are dead.  But when we move the argment inside the first
case RHS, and eliminate the second case, we get

	case x or { (a,b) -> a b }

Urk! b is alive!  Reason: the scrutinee was a variable, and case elimination
happened.  Hence the zap_occ_info function returned by simplCaseBinder

\begin{code}
simplCaseBinder env (Var v) case_bndr
  | not (switchIsOn (getSwitchChecker env) NoCaseOfCase)

-- Failed try [see Note 2 above]
--     not (isEvaldUnfolding (idUnfolding v))

  = simplBinder env (zap case_bndr)		`thenSmpl` \ (env, case_bndr') ->
    returnSmpl (modifyInScope env v case_bndr', case_bndr', zap)
	-- We could extend the substitution instead, but it would be
	-- a hack because then the substitution wouldn't be idempotent
	-- any more (v is an OutId).  And this just just as well.
  where
    zap b = b `setIdOccInfo` NoOccInfo
	    
simplCaseBinder env other_scrut case_bndr 
  = simplBinder env case_bndr 		`thenSmpl` \ (env, case_bndr') ->
    returnSmpl (env, case_bndr', \ bndr -> bndr)	-- NoOp on bndr
\end{code}



\begin{code}
simplAlts :: SimplEnv 
	  -> (InId -> InId)		-- Occ-info zapper
	  -> [AltCon]			-- Alternatives the scrutinee can't be
					-- in the default case
	  -> OutId			-- Case binder
	  -> [InAlt] -> SimplCont
	  -> SimplM [OutAlt]		-- Includes the continuation

simplAlts env zap_occ_info handled_cons case_bndr' alts cont'
  = mapSmpl simpl_alt alts
  where
    inst_tys' = tyConAppArgs (idType case_bndr')

    simpl_alt (DEFAULT, _, rhs)
	= let
		-- In the default case we record the constructors that the
		-- case-binder *can't* be.
		-- We take advantage of any OtherCon info in the case scrutinee
		case_bndr_w_unf = case_bndr' `setIdUnfolding` mkOtherCon handled_cons
		env_with_unf    = modifyInScope env case_bndr' case_bndr_w_unf 
	  in
	  simplExprC env_with_unf rhs cont'	`thenSmpl` \ rhs' ->
	  returnSmpl (DEFAULT, [], rhs')

    simpl_alt (con, vs, rhs)
	= 	-- Deal with the pattern-bound variables
		-- Mark the ones that are in ! positions in the data constructor
		-- as certainly-evaluated.
		-- NB: it happens that simplBinders does *not* erase the OtherCon
		--     form of unfolding, so it's ok to add this info before 
		--     doing simplBinders
	  simplBinders env (add_evals con vs)		`thenSmpl` \ (env, vs') ->

		-- Bind the case-binder to (con args)
	  let
		unfolding    = mkUnfolding False (mkAltExpr con vs' inst_tys')
		env_with_unf = modifyInScope env case_bndr' (case_bndr' `setIdUnfolding` unfolding)
	  in
	  simplExprC env_with_unf rhs cont'		`thenSmpl` \ rhs' ->
	  returnSmpl (con, vs', rhs')


	-- add_evals records the evaluated-ness of the bound variables of
	-- a case pattern.  This is *important*.  Consider
	--	data T = T !Int !Int
	--
	--	case x of { T a b -> T (a+1) b }
	--
	-- We really must record that b is already evaluated so that we don't
	-- go and re-evaluate it when constructing the result.

    add_evals (DataAlt dc) vs = cat_evals vs (dataConRepStrictness dc)
    add_evals other_con    vs = vs

    cat_evals [] [] = []
    cat_evals (v:vs) (str:strs)
	| isTyVar v          = v	: cat_evals vs (str:strs)
	| isMarkedStrict str = evald_v  : cat_evals vs strs
	| otherwise          = zapped_v : cat_evals vs strs
	where
	  zapped_v = zap_occ_info v
	  evald_v  = zapped_v `setIdUnfolding` mkOtherCon []
\end{code}


%************************************************************************
%*									*
\subsection{Known constructor}
%*									*
%************************************************************************

We are a bit careful with occurrence info.  Here's an example

	(\x* -> case x of (a*, b) -> f a) (h v, e)

where the * means "occurs once".  This effectively becomes
	case (h v, e) of (a*, b) -> f a)
and then
	let a* = h v; b = e in f a
and then
	f (h v)

All this should happen in one sweep.

\begin{code}
knownCon :: SimplEnv -> AltCon -> [OutExpr]
	 -> InId -> [InAlt] -> SimplCont
	 -> SimplM FloatsWithExpr

knownCon env con args bndr alts cont
  = tick (KnownBranch bndr)	`thenSmpl_`
    case findAlt con alts of
	(DEFAULT, bs, rhs)     -> ASSERT( null bs )
				  simplNonRecX env bndr scrut	$ \ env ->
					-- This might give rise to a binding with non-atomic args
					-- like x = Node (f x) (g x)
					-- but no harm will be done
				  simplExprF env rhs cont
				where
				  scrut = case con of
					    LitAlt lit -> Lit lit
					    DataAlt dc -> mkConApp dc args

	(LitAlt lit, bs, rhs) ->  ASSERT( null bs )
				  simplNonRecX env bndr (Lit lit)	$ \ env ->
				  simplExprF env rhs cont

	(DataAlt dc, bs, rhs)  -> ASSERT( length bs + n_tys == length args )
				  bind_args env bs (drop n_tys args)	$ \ env ->
				  let
				    con_app  = mkConApp dc (take n_tys args ++ con_args)
				    con_args = [substExpr (getSubst env) (varToCoreExpr b) | b <- bs]
					-- args are aready OutExprs, but bs are InIds
				  in
				  simplNonRecX env bndr con_app		$ \ env ->
			          simplExprF env rhs cont
			       where
				  n_tys = dataConNumInstArgs dc	-- Non-existential type args
-- Ugh!
bind_args env [] _ thing_inside = thing_inside env

bind_args env (b:bs) (Type ty : args) thing_inside
  = bind_args (extendSubst env b (DoneTy ty)) bs args thing_inside
    
bind_args env (b:bs) (arg : args) thing_inside
  = simplNonRecX env b arg	$ \ env ->
    bind_args env bs args thing_inside
\end{code}


%************************************************************************
%*									*
\subsection{Duplicating continuations}
%*									*
%************************************************************************

\begin{code}
prepareCaseCont :: SimplEnv
		-> [InAlt] -> SimplCont
	        -> SimplM (FloatsWith (SimplCont,SimplCont)) 	
			-- Return a duplicatable continuation, a non-duplicable part 
			-- plus some extra bindings

	-- No need to make it duplicatable if there's only one alternative
prepareCaseCont env [alt] cont = returnSmpl (emptyFloats env, (cont, mkBoringStop (contResultType cont)))
prepareCaseCont env alts  cont = mkDupableCont env cont
\end{code}

\begin{code}
mkDupableCont :: SimplEnv -> SimplCont 
	      -> SimplM (FloatsWith (SimplCont, SimplCont))

mkDupableCont env cont
  | contIsDupable cont
  = returnSmpl (emptyFloats env, (cont, mkBoringStop (contResultType cont)))

mkDupableCont env (CoerceIt ty cont)
  = mkDupableCont env cont		`thenSmpl` \ (floats, (dup_cont, nondup_cont)) ->
    returnSmpl (floats, (CoerceIt ty dup_cont, nondup_cont))

mkDupableCont env (InlinePlease cont)
  = mkDupableCont env cont		`thenSmpl` \ (floats, (dup_cont, nondup_cont)) ->
    returnSmpl (floats, (InlinePlease dup_cont, nondup_cont))

mkDupableCont env cont@(ArgOf _ arg_ty _ _)
  =  returnSmpl (emptyFloats env, (mkBoringStop arg_ty, cont))
  	-- Do *not* duplicate an ArgOf continuation
	-- Because ArgOf continuations are opaque, we gain nothing by
	-- propagating them into the expressions, and we do lose a lot.
	-- Here's an example:
	--	&& (case x of { T -> F; F -> T }) E
	-- Now, && is strict so we end up simplifying the case with
	-- an ArgOf continuation.  If we let-bind it, we get
	--
	--	let $j = \v -> && v E
	--	in simplExpr (case x of { T -> F; F -> T })
	--		     (ArgOf (\r -> $j r)
	-- And after simplifying more we get
	--
	--	let $j = \v -> && v E
	--	in case of { T -> $j F; F -> $j T }
	-- Which is a Very Bad Thing
	--
	-- The desire not to duplicate is the entire reason that
	-- mkDupableCont returns a pair of continuations.
	--
	-- The original plan had:
  	-- e.g. 	(...strict-fn...) [...hole...]
	--	==>
	--		let $j = \a -> ...strict-fn...
	--		in $j [...hole...]

mkDupableCont env (ApplyTo _ arg se cont)
  = 	-- e.g. 	[...hole...] (...arg...)
	--	==>
	--		let a = ...arg... 
	--		in [...hole...] a
    simplExpr (setInScope se env) arg			`thenSmpl` \ arg' ->

    mkDupableCont env cont				`thenSmpl` \ (floats, (dup_cont, nondup_cont)) ->
    addFloats env floats				$ \ env ->

    if exprIsDupable arg' then
	returnSmpl (emptyFloats env, (ApplyTo OkToDup arg' (zapSubstEnv se) dup_cont, nondup_cont))
    else
    newId FSLIT("a") (exprType arg')			`thenSmpl` \ arg_id ->

    tick (CaseOfCase arg_id)				`thenSmpl_`
	-- Want to tick here so that we go round again,
	-- and maybe copy or inline the code.
	-- Not strictly CaseOfCase, but never mind

    returnSmpl (unitFloat env arg_id arg', 
		(ApplyTo OkToDup (Var arg_id) (zapSubstEnv se) dup_cont,
		 nondup_cont))
	-- But what if the arg should be case-bound? 
	-- This has been this way for a long time, so I'll leave it,
	-- but I can't convince myself that it's right.


mkDupableCont env (Select _ case_bndr alts se cont)
  = 	-- e.g.		(case [...hole...] of { pi -> ei })
	--	===>
	--		let ji = \xij -> ei 
	--		in case [...hole...] of { pi -> ji xij }
    tick (CaseOfCase case_bndr)					`thenSmpl_`
    let
	alt_env = setInScope se env
    in
    prepareCaseCont alt_env alts cont				`thenSmpl` \ (floats1, (dup_cont, nondup_cont)) ->
    addFloats alt_env floats1 					$ \ alt_env ->

    simplBinder alt_env case_bndr				`thenSmpl` \ (alt_env, case_bndr') ->
	-- NB: simplBinder does not zap deadness occ-info, so
	-- a dead case_bndr' will still advertise its deadness
	-- This is really important because in
	--	case e of b { (# a,b #) -> ... }
	-- b is always dead, and indeed we are not allowed to bind b to (# a,b #),
	-- which might happen if e was an explicit unboxed pair and b wasn't marked dead.
	-- In the new alts we build, we have the new case binder, so it must retain
	-- its deadness.

    mkDupableAlts alt_env case_bndr' alts dup_cont	`thenSmpl` \ (floats2, alts') ->
    addFloats alt_env floats2 				$ \ alt_env ->
    returnSmpl (emptyFloats alt_env, 
		(Select OkToDup case_bndr' alts' (zapSubstEnv se) 
			(mkBoringStop (contResultType dup_cont)),
		 nondup_cont))

mkDupableAlts :: SimplEnv -> OutId -> [InAlt] -> SimplCont
	      -> SimplM (FloatsWith [InAlt])
-- Absorbs the continuation into the new alternatives

mkDupableAlts env case_bndr' alts dupable_cont 
  = go env alts
  where
    go env [] = returnSmpl (emptyFloats env, [])
    go env (alt:alts)
	= mkDupableAlt env case_bndr' dupable_cont alt	`thenSmpl` \ (floats1, alt') ->
	  addFloats env floats1				$ \ env ->
	  go env alts					`thenSmpl` \ (floats2, alts') ->
	  returnSmpl (floats2, alt' : alts')
					
mkDupableAlt env case_bndr' cont alt@(con, bndrs, rhs)
  = simplBinders env bndrs				`thenSmpl` \ (env, bndrs') ->
    simplExprC env rhs cont				`thenSmpl` \ rhs' ->

    if exprIsDupable rhs' then
	returnSmpl (emptyFloats env, (con, bndrs', rhs'))
   	-- It is worth checking for a small RHS because otherwise we
	-- get extra let bindings that may cause an extra iteration of the simplifier to
	-- inline back in place.  Quite often the rhs is just a variable or constructor.
	-- The Ord instance of Maybe in PrelMaybe.lhs, for example, took several extra
	-- iterations because the version with the let bindings looked big, and so wasn't
	-- inlined, but after the join points had been inlined it looked smaller, and so
	-- was inlined.
	--
	-- NB: we have to check the size of rhs', not rhs. 
	-- Duplicating a small InAlt might invalidate occurrence information
	-- However, if it *is* dupable, we return the *un* simplified alternative,
	-- because otherwise we'd need to pair it up with an empty subst-env....
	-- but we only have one env shared between all the alts.
	-- (Remember we must zap the subst-env before re-simplifying something).
	-- Rather than do this we simply agree to re-simplify the original (small) thing later.

    else
    let
	rhs_ty'     = exprType rhs'
        used_bndrs' = filter (not . isDeadBinder) (case_bndr' : bndrs')
		-- The deadness info on the new binders is unscathed
    in
	-- If we try to lift a primitive-typed something out
	-- for let-binding-purposes, we will *caseify* it (!),
	-- with potentially-disastrous strictness results.  So
	-- instead we turn it into a function: \v -> e
	-- where v::State# RealWorld#.  The value passed to this function
	-- is realworld#, which generates (almost) no code.

	-- There's a slight infelicity here: we pass the overall 
	-- case_bndr to all the join points if it's used in *any* RHS,
	-- because we don't know its usage in each RHS separately

	-- We used to say "&& isUnLiftedType rhs_ty'" here, but now
	-- we make the join point into a function whenever used_bndrs'
	-- is empty.  This makes the join-point more CPR friendly. 
	-- Consider:	let j = if .. then I# 3 else I# 4
	--		in case .. of { A -> j; B -> j; C -> ... }
	--
	-- Now CPR doesn't w/w j because it's a thunk, so
	-- that means that the enclosing function can't w/w either,
	-- which is a lose.  Here's the example that happened in practice:
	--	kgmod :: Int -> Int -> Int
	--	kgmod x y = if x > 0 && y < 0 || x < 0 && y > 0
	--	            then 78
	--		    else 5
	--
	-- I have seen a case alternative like this:
	--	True -> \v -> ...
	-- It's a bit silly to add the realWorld dummy arg in this case, making
	--	$j = \s v -> ...
	--	   True -> $j s
	-- (the \v alone is enough to make CPR happy) but I think it's rare

    ( if null used_bndrs' 
	then newId FSLIT("w") realWorldStatePrimTy  	`thenSmpl` \ rw_id ->
	     returnSmpl ([rw_id], [Var realWorldPrimId])
	else 
	     returnSmpl (used_bndrs', map varToCoreExpr used_bndrs')
    )							`thenSmpl` \ (final_bndrs', final_args) ->

	-- See comment about "$j" name above
    newId (encodeFS FSLIT("$j")) (mkPiTypes final_bndrs' rhs_ty')	`thenSmpl` \ join_bndr ->
	-- Notice the funky mkPiTypes.  If the contructor has existentials
	-- it's possible that the join point will be abstracted over
	-- type varaibles as well as term variables.
	--  Example:  Suppose we have
	--	data T = forall t.  C [t]
	--  Then faced with
	--	case (case e of ...) of
	--	    C t xs::[t] -> rhs
	--  We get the join point
	--	let j :: forall t. [t] -> ...
	--	    j = /\t \xs::[t] -> rhs
	--	in
	--	case (case e of ...) of
	--	    C t xs::[t] -> j t xs
    let 
	-- We make the lambdas into one-shot-lambdas.  The
	-- join point is sure to be applied at most once, and doing so
	-- prevents the body of the join point being floated out by
	-- the full laziness pass
	really_final_bndrs     = map one_shot final_bndrs'
	one_shot v | isId v    = setOneShotLambda v
		   | otherwise = v
	join_rhs  = mkLams really_final_bndrs rhs'
	join_call = mkApps (Var join_bndr) final_args
    in
    returnSmpl (unitFloat env join_bndr join_rhs, (con, bndrs', join_call))
\end{code}
