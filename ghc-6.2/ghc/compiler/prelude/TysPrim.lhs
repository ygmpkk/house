%
% (c) The AQUA Project, Glasgow University, 1994-1998
%
\section[TysPrim]{Wired-in knowledge about primitive types}

\begin{code}
module TysPrim(
	alphaTyVars, betaTyVars, alphaTyVar, betaTyVar, gammaTyVar, deltaTyVar,
	alphaTy, betaTy, gammaTy, deltaTy,
	openAlphaTy, openAlphaTyVar, openAlphaTyVars,

	primTyCons,

	charPrimTyCon, 		charPrimTy,
	intPrimTyCon,		intPrimTy,
	wordPrimTyCon,		wordPrimTy,
	addrPrimTyCon,		addrPrimTy,
	floatPrimTyCon,		floatPrimTy,
	doublePrimTyCon,	doublePrimTy,

	statePrimTyCon,		mkStatePrimTy,
	realWorldTyCon,		realWorldTy, realWorldStatePrimTy,

	arrayPrimTyCon,			mkArrayPrimTy, 
	byteArrayPrimTyCon,		byteArrayPrimTy,
	mutableArrayPrimTyCon,		mkMutableArrayPrimTy,
	mutableByteArrayPrimTyCon,	mkMutableByteArrayPrimTy,
	mutVarPrimTyCon,		mkMutVarPrimTy,

	mVarPrimTyCon,			mkMVarPrimTy,	
	stablePtrPrimTyCon,		mkStablePtrPrimTy,
	stableNamePrimTyCon,		mkStableNamePrimTy,
	bcoPrimTyCon,			bcoPrimTy,
	weakPrimTyCon,  		mkWeakPrimTy,
	foreignObjPrimTyCon,		foreignObjPrimTy,
	threadIdPrimTyCon,		threadIdPrimTy,
	
	int32PrimTyCon,		int32PrimTy,
	word32PrimTyCon,	word32PrimTy,

	int64PrimTyCon,		int64PrimTy,
	word64PrimTyCon,	word64PrimTy
  ) where

#include "HsVersions.h"

import Var		( TyVar, mkTyVar )
import Name		( Name, mkInternalName )
import OccName		( mkVarOcc )
import PrimRep		( PrimRep(..) )
import TyCon		( TyCon, ArgVrcs, mkPrimTyCon, mkLiftedPrimTyCon )
import Type		( mkTyConApp, mkTyConTy, mkTyVarTys, mkTyVarTy,
			  unliftedTypeKind, liftedTypeKind, openTypeKind, 
			  Kind, mkArrowKinds
			)
import SrcLoc		( noSrcLoc )
import Unique		( mkAlphaTyVarUnique )
import PrelNames
import FastString	( mkFastString )
import Outputable

import Char 		( ord, chr )
\end{code}

%************************************************************************
%*									*
\subsection{Primitive type constructors}
%*									*
%************************************************************************

\begin{code}
primTyCons :: [TyCon]
primTyCons 
  = [ addrPrimTyCon
    , arrayPrimTyCon
    , byteArrayPrimTyCon
    , charPrimTyCon
    , doublePrimTyCon
    , floatPrimTyCon
    , intPrimTyCon
    , int32PrimTyCon
    , int64PrimTyCon
    , foreignObjPrimTyCon
    , bcoPrimTyCon
    , weakPrimTyCon
    , mutableArrayPrimTyCon
    , mutableByteArrayPrimTyCon
    , mVarPrimTyCon
    , mutVarPrimTyCon
    , realWorldTyCon
    , stablePtrPrimTyCon
    , stableNamePrimTyCon
    , statePrimTyCon
    , threadIdPrimTyCon
    , wordPrimTyCon
    , word32PrimTyCon
    , word64PrimTyCon
    ]
\end{code}


%************************************************************************
%*									*
\subsection{Support code}
%*									*
%************************************************************************

alphaTyVars is a list of type variables for use in templates: 
	["a", "b", ..., "z", "t1", "t2", ... ]

\begin{code}
tyVarList :: Kind -> [TyVar]
tyVarList kind = [ mkTyVar (mkInternalName (mkAlphaTyVarUnique u) 
				(mkVarOcc (mkFastString name))
			 	noSrcLoc) kind
	         | u <- [2..],
		   let name | c <= 'z'  = [c]
		            | otherwise = 't':show u
			    where c = chr (u-2 + ord 'a')
	         ]

alphaTyVars :: [TyVar]
alphaTyVars = tyVarList liftedTypeKind

betaTyVars = tail alphaTyVars

alphaTyVar, betaTyVar, gammaTyVar :: TyVar
(alphaTyVar:betaTyVar:gammaTyVar:deltaTyVar:_) = alphaTyVars

alphaTys = mkTyVarTys alphaTyVars
(alphaTy:betaTy:gammaTy:deltaTy:_) = alphaTys

	-- openAlphaTyVar is prepared to be instantiated
	-- to a lifted or unlifted type variable.  It's used for the 
	-- result type for "error", so that we can have (error Int# "Help")
openAlphaTyVars :: [TyVar]
openAlphaTyVars@(openAlphaTyVar:_) = tyVarList openTypeKind

openAlphaTy = mkTyVarTy openAlphaTyVar

vrcPos,vrcZero :: (Bool,Bool)
vrcPos  = (True,False)
vrcZero = (False,False)

vrcsP,vrcsZ,vrcsZP :: ArgVrcs
vrcsP  = [vrcPos]
vrcsZ  = [vrcZero]
vrcsZP = [vrcZero,vrcPos]
\end{code}


%************************************************************************
%*									*
\subsection[TysPrim-basic]{Basic primitive types (@Char#@, @Int#@, etc.)}
%*									*
%************************************************************************

\begin{code}
-- only used herein
pcPrimTyCon :: Name -> ArgVrcs -> PrimRep -> TyCon
pcPrimTyCon name arg_vrcs rep
  = mkPrimTyCon name kind arity arg_vrcs rep
  where
    arity       = length arg_vrcs
    kind        = mkArrowKinds (replicate arity liftedTypeKind) result_kind
    result_kind = unliftedTypeKind -- all primitive types are unlifted

pcPrimTyCon0 :: Name -> PrimRep -> TyCon
pcPrimTyCon0 name rep
  = mkPrimTyCon name result_kind 0 [] rep
  where
    result_kind = unliftedTypeKind -- all primitive types are unlifted

charPrimTy	= mkTyConTy charPrimTyCon
charPrimTyCon	= pcPrimTyCon0 charPrimTyConName CharRep

intPrimTy	= mkTyConTy intPrimTyCon
intPrimTyCon	= pcPrimTyCon0 intPrimTyConName IntRep

int32PrimTy	= mkTyConTy int32PrimTyCon
int32PrimTyCon	= pcPrimTyCon0 int32PrimTyConName Int32Rep

int64PrimTy	= mkTyConTy int64PrimTyCon
int64PrimTyCon	= pcPrimTyCon0 int64PrimTyConName Int64Rep

wordPrimTy	= mkTyConTy wordPrimTyCon
wordPrimTyCon	= pcPrimTyCon0 wordPrimTyConName WordRep

word32PrimTy	= mkTyConTy word32PrimTyCon
word32PrimTyCon	= pcPrimTyCon0 word32PrimTyConName Word32Rep

word64PrimTy	= mkTyConTy word64PrimTyCon
word64PrimTyCon	= pcPrimTyCon0 word64PrimTyConName Word64Rep

addrPrimTy	= mkTyConTy addrPrimTyCon
addrPrimTyCon	= pcPrimTyCon0 addrPrimTyConName AddrRep

floatPrimTy	= mkTyConTy floatPrimTyCon
floatPrimTyCon	= pcPrimTyCon0 floatPrimTyConName FloatRep

doublePrimTy	= mkTyConTy doublePrimTyCon
doublePrimTyCon	= pcPrimTyCon0 doublePrimTyConName DoubleRep
\end{code}


%************************************************************************
%*									*
\subsection[TysPrim-state]{The @State#@ type (and @_RealWorld@ types)}
%*									*
%************************************************************************

State# is the primitive, unlifted type of states.  It has one type parameter,
thus
	State# RealWorld
or
	State# s

where s is a type variable. The only purpose of the type parameter is to
keep different state threads separate.  It is represented by nothing at all.

\begin{code}
mkStatePrimTy ty = mkTyConApp statePrimTyCon [ty]
statePrimTyCon	 = pcPrimTyCon statePrimTyConName vrcsZ VoidRep
\end{code}

RealWorld is deeply magical.  It is *primitive*, but it is not
*unlifted* (hence PtrRep).  We never manipulate values of type
RealWorld; it's only used in the type system, to parameterise State#.

\begin{code}
realWorldTyCon = mkLiftedPrimTyCon realWorldTyConName liftedTypeKind 0 [] PtrRep
realWorldTy	     = mkTyConTy realWorldTyCon
realWorldStatePrimTy = mkStatePrimTy realWorldTy	-- State# RealWorld
\end{code}

Note: the ``state-pairing'' types are not truly primitive, so they are
defined in \tr{TysWiredIn.lhs}, not here.


%************************************************************************
%*									*
\subsection[TysPrim-arrays]{The primitive array types}
%*									*
%************************************************************************

\begin{code}
arrayPrimTyCon		  = pcPrimTyCon  arrayPrimTyConName	       vrcsP  PtrRep
mutableArrayPrimTyCon	  = pcPrimTyCon  mutableArrayPrimTyConName     vrcsZP PtrRep
mutableByteArrayPrimTyCon = pcPrimTyCon  mutableByteArrayPrimTyConName vrcsZ  PtrRep
byteArrayPrimTyCon	  = pcPrimTyCon0 byteArrayPrimTyConName	              PtrRep

mkArrayPrimTy elt    	    = mkTyConApp arrayPrimTyCon [elt]
byteArrayPrimTy	    	    = mkTyConTy byteArrayPrimTyCon
mkMutableArrayPrimTy s elt  = mkTyConApp mutableArrayPrimTyCon [s, elt]
mkMutableByteArrayPrimTy s  = mkTyConApp mutableByteArrayPrimTyCon [s]
\end{code}

%************************************************************************
%*									*
\subsection[TysPrim-mut-var]{The mutable variable type}
%*									*
%************************************************************************

\begin{code}
mutVarPrimTyCon = pcPrimTyCon mutVarPrimTyConName vrcsZP PtrRep

mkMutVarPrimTy s elt 	    = mkTyConApp mutVarPrimTyCon [s, elt]
\end{code}

%************************************************************************
%*									*
\subsection[TysPrim-synch-var]{The synchronizing variable type}
%*									*
%************************************************************************

\begin{code}
mVarPrimTyCon = pcPrimTyCon mVarPrimTyConName vrcsZP PtrRep

mkMVarPrimTy s elt 	    = mkTyConApp mVarPrimTyCon [s, elt]
\end{code}

%************************************************************************
%*									*
\subsection[TysPrim-stable-ptrs]{The stable-pointer type}
%*									*
%************************************************************************

\begin{code}
stablePtrPrimTyCon = pcPrimTyCon stablePtrPrimTyConName vrcsP StablePtrRep

mkStablePtrPrimTy ty = mkTyConApp stablePtrPrimTyCon [ty]
\end{code}

%************************************************************************
%*									*
\subsection[TysPrim-stable-names]{The stable-name type}
%*									*
%************************************************************************

\begin{code}
stableNamePrimTyCon = pcPrimTyCon stableNamePrimTyConName vrcsP PtrRep

mkStableNamePrimTy ty = mkTyConApp stableNamePrimTyCon [ty]
\end{code}

%************************************************************************
%*									*
\subsection[TysPrim-foreign-objs]{The ``foreign object'' type}
%*									*
%************************************************************************

A Foreign Object is just a boxed, unlifted, Addr#.  They're needed
because finalisers (weak pointers) can't watch Addr#s, they can only
watch heap-resident objects.  

We can't use a lifted Addr# (such as Addr) because race conditions
could bite us.  For example, if the program deconstructed the Addr
before passing its contents to a ccall, and a weak pointer was
watching the Addr, the weak pointer might deduce that the Addr was
dead before it really was.

\begin{code}
foreignObjPrimTy    = mkTyConTy foreignObjPrimTyCon
foreignObjPrimTyCon = pcPrimTyCon0 foreignObjPrimTyConName PtrRep
\end{code}

%************************************************************************
%*									*
\subsection[TysPrim-BCOs]{The ``bytecode object'' type}
%*									*
%************************************************************************

\begin{code}
bcoPrimTy    = mkTyConTy bcoPrimTyCon
bcoPrimTyCon = pcPrimTyCon0 bcoPrimTyConName PtrRep
\end{code}
  
%************************************************************************
%*									*
\subsection[TysPrim-Weak]{The ``weak pointer'' type}
%*									*
%************************************************************************

\begin{code}
weakPrimTyCon = pcPrimTyCon weakPrimTyConName vrcsP PtrRep

mkWeakPrimTy v = mkTyConApp weakPrimTyCon [v]
\end{code}

%************************************************************************
%*									*
\subsection[TysPrim-thread-ids]{The ``thread id'' type}
%*									*
%************************************************************************

A thread id is represented by a pointer to the TSO itself, to ensure
that they are always unique and we can always find the TSO for a given
thread id.  However, this has the unfortunate consequence that a
ThreadId# for a given thread is treated as a root by the garbage
collector and can keep TSOs around for too long.

Hence the programmer API for thread manipulation uses a weak pointer
to the thread id internally.

\begin{code}
threadIdPrimTy    = mkTyConTy threadIdPrimTyCon
threadIdPrimTyCon = pcPrimTyCon0 threadIdPrimTyConName PtrRep
\end{code}
