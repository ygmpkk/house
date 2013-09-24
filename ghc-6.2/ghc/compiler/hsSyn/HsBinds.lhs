%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[HsBinds]{Abstract syntax: top-level bindings and signatures}

Datatype for: @HsBinds@, @Bind@, @Sig@, @MonoBinds@.

\begin{code}
module HsBinds where

#include "HsVersions.h"

import {-# SOURCE #-} HsExpr ( HsExpr, pprExpr,
			       Match,  pprFunBind,
			       GRHSs,  pprPatBind )

-- friends:
import HsImpExp		( pprHsVar )
import HsPat		( Pat )
import HsTypes		( HsType )
import PprCore		( {- instance Outputable (Expr a) -} )

--others:
import Name		( Name )
import PrelNames	( isUnboundName )
import NameSet		( NameSet, elemNameSet, nameSetToList )
import BasicTypes	( RecFlag(..), FixitySig(..), Activation(..), IPName )
import Outputable	
import SrcLoc		( SrcLoc )
import Var		( TyVar )
import Class            ( DefMeth (..) )
\end{code}

%************************************************************************
%*									*
\subsection{Bindings: @HsBinds@}
%*									*
%************************************************************************

The following syntax may produce new syntax which is not part of the input,
and which is instead a translation of the input to the typechecker.
Syntax translations are marked TRANSLATION in comments. New empty
productions are useful in development but may not appear in the final
grammar.

Collections of bindings, created by dependency analysis and translation:

\begin{code}
data HsBinds id		-- binders and bindees
  = EmptyBinds
  | ThenBinds	(HsBinds id) (HsBinds id)

  | MonoBind 			-- A mutually recursive group
	(MonoBinds id)
	[Sig id]		-- Empty on typechecker output, Type Signatures
	RecFlag

  | IPBinds			-- Implcit parameters
				-- Not allowed at top level
	[(IPName id, HsExpr id)]
\end{code}

\begin{code}
nullBinds :: HsBinds id -> Bool

nullBinds EmptyBinds		= True
nullBinds (ThenBinds b1 b2)	= nullBinds b1 && nullBinds b2
nullBinds (MonoBind b _ _)	= nullMonoBinds b
nullBinds (IPBinds b)   	= null b

mkMonoBind :: RecFlag -> MonoBinds id -> HsBinds id
mkMonoBind _      EmptyMonoBinds  = EmptyBinds
mkMonoBind is_rec mbinds 	  = MonoBind mbinds [] is_rec
\end{code}

\begin{code}
instance (OutputableBndr id) => Outputable (HsBinds id) where
    ppr binds = ppr_binds binds

ppr_binds EmptyBinds = empty
ppr_binds (ThenBinds binds1 binds2)
    = ppr_binds binds1 $$ ppr_binds binds2

ppr_binds (IPBinds binds)
  = sep (punctuate semi (map pp_item binds))
  where
    pp_item (id,rhs) = pprBndr LetBind id <+> equals <+> pprExpr rhs

ppr_binds (MonoBind bind sigs is_rec)
     = vcat [ppr_isrec,
     	     vcat (map ppr sigs),
	     ppr bind
       ]
     where
       ppr_isrec = getPprStyle $ \ sty -> 
		   if userStyle sty then empty else
		   case is_rec of
		   	Recursive    -> ptext SLIT("{- rec -}")
			NonRecursive -> ptext SLIT("{- nonrec -}")
\end{code}

%************************************************************************
%*									*
\subsection{Bindings: @MonoBinds@}
%*									*
%************************************************************************

Global bindings (where clauses)

\begin{code}
data MonoBinds id
  = EmptyMonoBinds

  | AndMonoBinds    (MonoBinds id)
		    (MonoBinds id)

  | FunMonoBind     id		-- Used for both functions 	f x = e
				-- and variables		f = \x -> e
				-- Reason: the Match stuff lets us have an optional
				--	   result type sig	f :: a->a = ...mentions a...
				--
				-- This also means that instance decls can only have
				-- FunMonoBinds, so if you change this, you'll need to
				-- change e.g. rnMethodBinds
		    Bool		-- True => infix declaration
		    [Match id]
		    SrcLoc

  | PatMonoBind     (Pat id)	-- The pattern is never a simple variable;
				-- That case is done by FunMonoBind
		    (GRHSs id)
		    SrcLoc

  | VarMonoBind	    id			-- TRANSLATION
		    (HsExpr id)

  | AbsBinds				-- Binds abstraction; TRANSLATION
		[TyVar]	  		-- Type variables
		[id]			-- Dicts
		[([TyVar], id, id)]	-- (type variables, polymorphic, momonmorphic) triples
		NameSet			-- Set of *polymorphic* variables that have an INLINE pragma
		(MonoBinds id)      -- The "business end"

	-- Creates bindings for *new* (polymorphic, overloaded) locals
	-- in terms of *old* (monomorphic, non-overloaded) ones.
	--
	-- See section 9 of static semantics paper for more details.
	-- (You can get a PhD for explaining the True Meaning
	--  of this last construct.)
\end{code}

What AbsBinds means
~~~~~~~~~~~~~~~~~~~
	 AbsBinds tvs
		  [d1,d2]
		  [(tvs1, f1p, f1m), 
		   (tvs2, f2p, f2m)]
		  BIND
means

	f1p = /\ tvs -> \ [d1,d2] -> letrec DBINDS and BIND 
				      in fm

	gp = ...same again, with gm instead of fm

This is a pretty bad translation, because it duplicates all the bindings.
So the desugarer tries to do a better job:

	fp = /\ [a,b] -> \ [d1,d2] -> case tp [a,b] [d1,d2] of
					(fm,gm) -> fm
	..ditto for gp..

	tp = /\ [a,b] -> \ [d1,d2] -> letrec DBINDS and BIND 
				       in (fm,gm)

\begin{code}
-- We keep the invariant that a MonoBinds is only empty 
-- if it is exactly EmptyMonoBinds

nullMonoBinds :: MonoBinds id -> Bool
nullMonoBinds EmptyMonoBinds	     = True
nullMonoBinds other_monobind	     = False

andMonoBinds :: MonoBinds id -> MonoBinds id -> MonoBinds id
andMonoBinds EmptyMonoBinds mb = mb
andMonoBinds mb EmptyMonoBinds = mb
andMonoBinds mb1 mb2 = AndMonoBinds mb1 mb2

andMonoBindList :: [MonoBinds id] -> MonoBinds id
andMonoBindList binds
  = loop1 binds
  where
    loop1 [] = EmptyMonoBinds
    loop1 (EmptyMonoBinds : binds) = loop1 binds
    loop1 (b:bs) = loop2 b bs

	-- acc is non-empty
    loop2 acc [] = acc
    loop2 acc (EmptyMonoBinds : bs) = loop2 acc bs
    loop2 acc (b:bs) = loop2 (acc `AndMonoBinds` b) bs
\end{code}


\begin{code}
instance OutputableBndr id => Outputable (MonoBinds id) where
    ppr mbind = ppr_monobind mbind


ppr_monobind :: OutputableBndr id => MonoBinds id -> SDoc
ppr_monobind EmptyMonoBinds = empty
ppr_monobind (AndMonoBinds binds1 binds2)
      = ppr_monobind binds1 $$ ppr_monobind binds2

ppr_monobind (PatMonoBind pat grhss locn)	= pprPatBind pat grhss
ppr_monobind (FunMonoBind fun inf matches locn) = pprFunBind fun matches
      -- ToDo: print infix if appropriate

ppr_monobind (VarMonoBind name expr)
      = sep [pprBndr LetBind name <+> equals, nest 4 (pprExpr expr)]

ppr_monobind (AbsBinds tyvars dictvars exports inlines val_binds)
     = sep [ptext SLIT("AbsBinds"),
	    brackets (interpp'SP tyvars),
	    brackets (interpp'SP dictvars),
	    brackets (sep (punctuate comma (map ppr exports))),
	    brackets (interpp'SP (nameSetToList inlines))]
       $$
       nest 4 ( vcat [pprBndr LetBind x | (_,x,_) <- exports]
			-- Print type signatures
		$$
		ppr val_binds )
\end{code}

%************************************************************************
%*									*
\subsection{@Sig@: type signatures and value-modifying user pragmas}
%*									*
%************************************************************************

It is convenient to lump ``value-modifying'' user-pragmas (e.g.,
``specialise this function to these four types...'') in with type
signatures.  Then all the machinery to move them into place, etc.,
serves for both.

\begin{code}
data Sig name
  = Sig		name		-- a bog-std type signature
		(HsType name)
		SrcLoc

  | ClassOpSig	name		-- Selector name
                (DefMeth name)	-- Default-method info
				-- See "THE NAMING STORY" in HsDecls
		(HsType name)
		SrcLoc

  | SpecSig 	name		-- specialise a function or datatype ...
		(HsType name)	-- ... to these types
		SrcLoc

  | InlineSig	Bool		-- True <=> INLINE f, False <=> NOINLINE f
	 	name		-- Function name
		Activation	-- When inlining is *active*
		SrcLoc

  | SpecInstSig (HsType name)	-- (Class tys); should be a specialisation of the 
				-- current instance decl
		SrcLoc

  | FixSig	(FixitySig name)	-- Fixity declaration
\end{code}

\begin{code}
okBindSig :: NameSet -> Sig Name -> Bool
okBindSig ns (ClassOpSig _ _ _ _) = False
okBindSig ns sig		  = sigForThisGroup ns sig

okClsDclSig :: Sig Name -> Bool
okClsDclSig (Sig _ _ _)       = False
okClsDclSig (SpecInstSig _ _) = False
okClsDclSig sig 	      = True	-- All others OK

okInstDclSig :: NameSet -> Sig Name -> Bool
okInstDclSig ns (Sig _ _ _)	  = False
okInstDclSig ns (FixSig _)	  = False
okInstDclSig ns (SpecInstSig _ _) = True
okInstDclSig ns sig		  = sigForThisGroup ns sig

sigForThisGroup ns sig 
  = case sigName sig of
	Nothing -> False
	Just n  -> n `elemNameSet` ns

sigName :: Sig name -> Maybe name
sigName (Sig         n _ _)        = Just n
sigName (ClassOpSig  n _ _ _)      = Just n
sigName (SpecSig     n _ _)        = Just n
sigName (InlineSig _ n _ _)        = Just n
sigName (FixSig (FixitySig n _ _)) = Just n
sigName other			   = Nothing

isFixitySig :: Sig name -> Bool
isFixitySig (FixSig _) = True
isFixitySig _	       = False

isClassOpSig :: Sig name -> Bool
isClassOpSig (ClassOpSig _ _ _ _) = True
isClassOpSig _			  = False

isPragSig :: Sig name -> Bool
	-- Identifies pragmas 
isPragSig (SpecSig _ _ _)     = True
isPragSig (InlineSig _ _ _ _) = True
isPragSig (SpecInstSig _ _)   = True
isPragSig other		      = False
\end{code}

\begin{code}
hsSigDoc (Sig        _ _ loc) 	      = (ptext SLIT("type signature"),loc)
hsSigDoc (ClassOpSig _ _ _ loc)       = (ptext SLIT("class-method type signature"), loc)
hsSigDoc (SpecSig    _ _ loc) 	      = (ptext SLIT("SPECIALISE pragma"),loc)
hsSigDoc (InlineSig True  _ _ loc)    = (ptext SLIT("INLINE pragma"),loc)
hsSigDoc (InlineSig False _ _ loc)    = (ptext SLIT("NOINLINE pragma"),loc)
hsSigDoc (SpecInstSig _ loc)	      = (ptext SLIT("SPECIALISE instance pragma"),loc)
hsSigDoc (FixSig (FixitySig _ _ loc)) = (ptext SLIT("fixity declaration"), loc)
\end{code}

\begin{code}
instance (Outputable name) => Outputable (Sig name) where
    ppr sig = ppr_sig sig

ppr_sig :: Outputable name => Sig name -> SDoc
ppr_sig (Sig var ty _)
      = sep [ppr var <+> dcolon, nest 4 (ppr ty)]

ppr_sig (ClassOpSig var dm ty _)
      = sep [ pprHsVar var <+> dcolon, 
	      nest 4 (ppr ty),
	      nest 4 (pp_dm_comment) ]
      where
	pp_dm = case dm of 
		  DefMeth _  -> equals 	-- Default method indicator
		  GenDefMeth -> semi    -- Generic method indicator
		  NoDefMeth  -> empty   -- No Method at all
	pp_dm_comment = case dm of 
		  DefMeth _  -> text "{- has default method -}"
		  GenDefMeth -> text "{- has generic method -}"
		  NoDefMeth  -> empty   -- No Method at all

ppr_sig (SpecSig var ty _)
      = sep [ hsep [text "{-# SPECIALIZE", ppr var, dcolon],
	      nest 4 (ppr ty <+> text "#-}")
	]

ppr_sig (InlineSig True var phase _)
      = hsep [text "{-# INLINE", ppr phase, ppr var, text "#-}"]

ppr_sig (InlineSig False var phase _)
      = hsep [text "{-# NOINLINE", ppr phase, ppr var, text "#-}"]

ppr_sig (SpecInstSig ty _)
      = hsep [text "{-# SPECIALIZE instance", ppr ty, text "#-}"]

ppr_sig (FixSig fix_sig) = ppr fix_sig
\end{code}

Checking for distinct signatures; oh, so boring


\begin{code}
eqHsSig :: Sig Name -> Sig Name -> Bool
eqHsSig (Sig n1 _ _)         (Sig n2 _ _)          = n1 == n2
eqHsSig (InlineSig b1 n1 _ _)(InlineSig b2 n2 _ _) = b1 == b2 && n1 == n2

eqHsSig (SpecInstSig ty1 _)  (SpecInstSig ty2 _)  = ty1 == ty2
eqHsSig (SpecSig n1 ty1 _)   (SpecSig n2 ty2 _)   =
    -- may have many specialisations for one value;
    -- but not ones that are exactly the same...
    (n1 == n2) && (ty1 == ty2)

eqHsSig _other1 _other2 = False
\end{code}
