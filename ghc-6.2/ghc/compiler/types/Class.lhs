%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[Class]{The @Class@ datatype}

\begin{code}
module Class (
	Class, ClassOpItem, FunDep,
	DefMeth (..),

	mkClass, classTyVars, classArity,
	classKey, className, classSelIds, classTyCon,
	classBigSig, classExtraBigSig, classTvsFds, classSCTheta,
 	classHasFDs
    ) where

#include "HsVersions.h"

import {-# SOURCE #-} TyCon	( TyCon )
import {-# SOURCE #-} TypeRep	( PredType )

import Var		( Id, TyVar )
import Name		( NamedThing(..), Name )
import BasicTypes	( Arity )
import Unique		( Unique, Uniquable(..) )
import Outputable
import Util             ( notNull )
\end{code}

%************************************************************************
%*									*
\subsection[Class-basic]{@Class@: basic definition}
%*									*
%************************************************************************

A @Class@ corresponds to a Greek kappa in the static semantics:

\begin{code}
data Class
  = Class {
	classKey  :: Unique,			-- Key for fast comparison
	className :: Name,
	
	classTyVars  :: [TyVar],		-- The class type variables
	classFunDeps :: [FunDep TyVar],		-- The functional dependencies

	classSCTheta :: [PredType],		-- Immediate superclasses, and the
	classSCSels  :: [Id],			-- corresponding selector functions to
						-- extract them from a dictionary of this
						-- class

	classOpStuff :: [ClassOpItem],		-- Ordered by tag

	classTyCon :: TyCon		-- The data type constructor for dictionaries
  }					-- of this class

type FunDep a	  = ([a],[a])	--  e.g. class C a b c |  a b -> c, a c -> b  where ...
				--  Here fun-deps are [([a,b],[c]), ([a,c],[b])]

type ClassOpItem = (Id, DefMeth Name)
	-- Selector function; contains unfolding
	-- Default-method info

data DefMeth id = NoDefMeth 		-- No default method
	        | DefMeth id 		-- A polymorphic default method (named id)
					-- 	(Only instantiated to RdrName and Name, never Id)
	        | GenDefMeth 		-- A generic default method
                deriving Eq  
\end{code}

The @mkClass@ function fills in the indirect superclasses.

\begin{code}
mkClass :: Name -> [TyVar]
	-> [([TyVar], [TyVar])]
	-> [PredType] -> [Id]
	-> [ClassOpItem]
	-> TyCon
	-> Class

mkClass name tyvars fds super_classes superdict_sels
	op_stuff tycon
  = Class {	classKey = getUnique name, 
		className = name,
		classTyVars = tyvars,
		classFunDeps = fds,
		classSCTheta = super_classes,
		classSCSels = superdict_sels,
		classOpStuff = op_stuff,
		classTyCon = tycon }
\end{code}

%************************************************************************
%*									*
\subsection[Class-selectors]{@Class@: simple selectors}
%*									*
%************************************************************************

The rest of these functions are just simple selectors.

\begin{code}
classArity :: Class -> Arity
classArity clas = length (classTyVars clas)
	-- Could memoise this

classSelIds (Class {classSCSels = sc_sels, classOpStuff = op_stuff})
  = sc_sels ++ [op_sel | (op_sel, _) <- op_stuff]

classTvsFds c
  = (classTyVars c, classFunDeps c)

classBigSig (Class {classTyVars = tyvars, classSCTheta = sc_theta, 
	 	    classSCSels = sc_sels, classOpStuff = op_stuff})
  = (tyvars, sc_theta, sc_sels, op_stuff)
classExtraBigSig (Class {classTyVars = tyvars, classFunDeps = fundeps,
			 classSCTheta = sc_theta, classSCSels = sc_sels,
			 classOpStuff = op_stuff})
  = (tyvars, fundeps, sc_theta, sc_sels, op_stuff)

classHasFDs :: Class -> Bool
classHasFDs (Class {classFunDeps = fundeps}) = notNull fundeps
\end{code}


%************************************************************************
%*									*
\subsection[Class-instances]{Instance declarations for @Class@}
%*									*
%************************************************************************

We compare @Classes@ by their keys (which include @Uniques@).

\begin{code}
instance Eq Class where
    c1 == c2 = classKey c1 == classKey c2
    c1 /= c2 = classKey c1 /= classKey c2

instance Ord Class where
    c1 <= c2 = classKey c1 <= classKey c2
    c1 <  c2 = classKey c1 <  classKey c2
    c1 >= c2 = classKey c1 >= classKey c2
    c1 >  c2 = classKey c1 >  classKey c2
    compare c1 c2 = classKey c1 `compare` classKey c2
\end{code}

\begin{code}
instance Uniquable Class where
    getUnique c = classKey c

instance NamedThing Class where
    getName clas = className clas

instance Outputable Class where
    ppr c = ppr (getName c)

instance Show Class where
    showsPrec p c = showsPrecSDoc p (ppr c)
\end{code}


