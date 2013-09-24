{-% DrIFT (Automatic class derivations for Haskell) v1.1 %-}
{-% DrIFT (Automatic class derivations for Haskell) v1.1 %-}
-- 
--  (c) The University of Glasgow 2002
-- 
-- Binary interface file support.

module BinIface ( writeBinIface, readBinIface, v_IgnoreHiWay ) where

#include "HsVersions.h"

import HscTypes
import BasicTypes
import NewDemand
import HsTypes
import HsCore
import HsDecls
import HsBinds
import HsPat		( HsConDetails(..) )
import TyCon
import Class
import VarEnv
import CostCentre
import RdrName		( mkRdrUnqual, mkRdrQual )
import Name		( Name, nameOccName, nameModule_maybe )
import NameEnv		( NameEnv, lookupNameEnv, nameEnvElts )
import Module		( moduleName )
import OccName		( OccName )
import RnHsSyn
import DriverState	( v_Build_tag )
import CmdLineOpts	( opt_HiVersion )
import Panic
import SrcLoc
import Binary
import Util

import DATA_IOREF
import EXCEPTION	( throwDyn )
import Monad		( when )

#include "HsVersions.h"

-- ---------------------------------------------------------------------------
-- We write out a ModIface, but read it in as a ParsedIface.
-- There are some big differences, and some subtle ones.  We do most
-- of the conversion on the way out, so there is minimal fuss when we
-- read it back in again (see RnMonad.lhs)

-- The main difference is that all Names in a ModIface are RdrNames in
-- a ParsedIface, so when writing out a Name in binary we make sure it
-- is binary-compatible with a RdrName.

-- Other subtle differences: 
--	- pi_mod is a ModuleName, but mi_mod is a Module.  Hence we put
--	  Modules as ModuleNames.
--	- pi_exports and pi_usages, Names have
-- 	  to be converted to OccNames.
--	- pi_fixity is a NameEnv in ModIface,
-- 	  but a list of (Name,Fixity) pairs in ParsedIface.
--	- versioning is totally different.
--	- deprecations are different.

writeBinIface :: FilePath -> ModIface -> IO ()
writeBinIface hi_path mod_iface
  = putBinFileWithDict hi_path (mi_module mod_iface) mod_iface

readBinIface :: FilePath -> IO ParsedIface
readBinIface hi_path = getBinFileWithDict hi_path


-- %*********************************************************
-- %*						 	    *
-- 		All the Binary instances
-- %*							    *
-- %*********************************************************

-- BasicTypes
{-! for IPName derive: Binary !-}
{-! for Fixity derive: Binary !-}
{-! for FixityDirection derive: Binary !-}
{-! for NewOrData derive: Binary !-}
{-! for Boxity derive: Binary !-}
{-! for StrictnessMark derive: Binary !-}
{-! for Activation derive: Binary !-}

instance Binary Name where
  -- we must print these as RdrNames, because that's how they will be read in
  put_ bh name
   = case nameModule_maybe name of
       Just mod
	  | this_mod == mod -> put_ bh (mkRdrUnqual occ)
	  | otherwise       -> put_ bh (mkRdrQual (moduleName mod) occ)
       _ 		    -> put_ bh (mkRdrUnqual occ)
    where
      occ 	       = nameOccName name
      (this_mod,_,_,_) = getUserData bh

  get bh = error "can't Binary.get a Name"    

-- NewDemand
{-! for Demand derive: Binary !-}
{-! for Demands derive: Binary !-}
{-! for DmdResult derive: Binary !-}
{-! for StrictSig derive: Binary !-}

instance Binary DmdType where
	-- ignore DmdEnv when spitting out the DmdType
  put bh (DmdType _ ds dr) = do p <- put bh ds; put bh dr; return (castBin p)
  get bh = do ds <- get bh; dr <- get bh; return (DmdType emptyVarEnv ds dr)

-- TyCon
{-! for DataConDetails derive: Binary !-}

-- Class
{-! for DefMeth derive: Binary !-}

-- HsTypes
{-! for HsPred derive: Binary !-}
{-! for HsType derive: Binary !-}
{-! for HsTupCon derive: Binary !-}
{-! for HsTyVarBndr derive: Binary !-}

-- HsCore
{-! for UfExpr derive: Binary !-}
{-! for UfConAlt derive: Binary !-}
{-! for UfBinding derive: Binary !-}
{-! for UfBinder derive: Binary !-}
{-! for HsIdInfo derive: Binary !-}
{-! for UfNote derive: Binary !-}

-- HsDecls
{-! for ConDetails derive: Binary !-}
{-! for BangType derive: Binary !-}

instance (Binary name) => Binary (TyClDecl name) where
    put_ bh (IfaceSig name ty idinfo _) = do
	    putByte bh 0
	    put_ bh name
	    lazyPut bh ty
	    lazyPut bh idinfo
    put_ bh (ForeignType ae af ag ah) = 
	error "Binary.put_(TyClDecl): ForeignType"
    put_ bh (TyData ai aj ak al am _ (Just generics) _) = do
	    putByte bh 2
	    put_ bh ai
	    put_ bh aj
	    put_ bh ak
	    put_ bh al
	    put_ bh am
	    -- ignore Derivs
	    put_ bh generics -- Record whether generics needed or not
    put_ bh (TySynonym aq ar as _) = do
	    putByte bh 3
	    put_ bh aq
	    put_ bh ar
	    put_ bh as
    put_ bh c@(ClassDecl ctxt nm tyvars fds sigs _ _) = do
	    putByte bh 4
	    put_ bh ctxt
	    put_ bh nm
	    put_ bh tyvars
	    put_ bh fds
	    put_ bh sigs
		-- ignore methods (there should be none)
		-- ignore SrcLoc
    get bh = do
	    h <- getByte bh
	    case h of
	      0 -> do
		    name <- get bh
		    ty <- lazyGet bh
		    idinfo <- lazyGet bh
		    return (IfaceSig name ty idinfo noSrcLoc)
	      1 -> error "Binary.get(TyClDecl): ForeignType"
	      2 -> do
		    n_or_d <- get bh
		    ctx    <- get bh
		    nm     <- get bh
		    tyvars <- get bh
		    cons   <- get bh
		    generics <- get bh
		    return (TyData n_or_d ctx nm tyvars cons 
				Nothing (Just generics) noSrcLoc)
	      3 -> do
		    aq <- get bh
		    ar <- get bh
		    as <- get bh
		    return (TySynonym aq ar as noSrcLoc)
	      _ -> do
		    ctxt <- get bh
		    nm <- get bh
		    tyvars <- get bh
		    fds <- get bh
		    sigs <- get bh
		    return (ClassDecl ctxt nm tyvars fds sigs 
				      Nothing noSrcLoc)

instance (Binary name) => Binary (ConDecl name) where
    put_ bh (ConDecl aa ac ad ae _) = do
	    put_ bh aa
	    put_ bh ac
	    put_ bh ad
	    put_ bh ae
		-- ignore SrcLoc
    get bh = do
	  aa <- get bh
	  ac <- get bh
	  ad <- get bh
	  ae <- get bh
	  return (ConDecl aa ac ad ae noSrcLoc)

instance (Binary name) => Binary (InstDecl name) where
    put_ bh (InstDecl aa _ _ ad _) = do
	    put_ bh aa
		-- ignore MonoBinds
		-- ignore Sigs
	    put_ bh ad
		-- ignore SrcLoc
    get bh = do
	  aa <- get bh
	  ad <- get bh
	  return (InstDecl aa EmptyMonoBinds [{-no sigs-}] ad noSrcLoc)

instance (Binary name) => Binary (RuleDecl name) where
    put_ bh (IfaceRule ag ah ai aj ak al _) = do
	    put_ bh ag
	    put_ bh ah
	    put_ bh ai
	    put_ bh aj
	    put_ bh ak
	    put_ bh al
		-- ignore SrcLoc
    get bh = do     ag <- get bh
		    ah <- get bh
		    ai <- get bh
		    aj <- get bh
		    ak <- get bh
		    al <- get bh
		    return (IfaceRule ag ah ai aj ak al noSrcLoc)

instance (Binary name) => Binary (DeprecDecl name) where
    put_ bh (Deprecation aa ab _) = do
	    put_ bh aa
	    put_ bh ab
		-- ignore SrcLoc
    get bh = do
	  aa <- get bh
	  ab <- get bh
	  return (Deprecation aa ab noSrcLoc)

-- HsBinds
instance Binary name => Binary (Sig name) where
   put_ bh (ClassOpSig n def ty _) = do	put_ bh n; put_ bh def;	put_ bh ty
   get bh = do
	n <- get bh
	def <- get bh
	ty <- get bh
	return (ClassOpSig n def ty noSrcLoc)

-- CostCentre
{-! for IsCafCC derive: Binary !-}
{-! for IsDupdCC derive: Binary !-}
{-! for CostCentre derive: Binary !-}



instance Binary ModIface where
  put_ bh iface =  do
	put_ bh (show opt_HiVersion)
	build_tag <- readIORef v_Build_tag
	put  bh build_tag
	p <- put_ bh (moduleName (mi_module iface))
	put_ bh (mi_package iface)
	put_ bh (vers_module (mi_version iface))
	put_ bh (mi_orphan iface)
	-- no: mi_boot
	lazyPut bh (mi_deps iface)
	lazyPut bh (map usageToOccName (mi_usages iface))
	put_ bh (vers_exports (mi_version iface),
		 map exportItemToRdrExportItem (mi_exports iface))
	put_ bh (declsToVersionedDecls (dcl_tycl (mi_decls iface))
			(vers_decls (mi_version iface)))
	-- no: mi_globals
	put_ bh (collectFixities (mi_fixities iface) 
				 (dcl_tycl (mi_decls iface)))
	put_ bh (dcl_insts (mi_decls iface))
	lazyPut bh (vers_rules (mi_version iface), dcl_rules (mi_decls iface))
	lazyPut bh (deprecsToIfaceDeprecs (mi_deprecs iface))

  -- Read in as a ParsedIface, not a ModIface.  See above.
  get bh = error "Binary.get: ModIface"

declsToVersionedDecls :: [RenamedTyClDecl] -> NameEnv Version
   -> [(Version, RenamedTyClDecl)]
declsToVersionedDecls decls env 
  = map add_vers decls
  where add_vers d = 
	   case lookupNameEnv env (tyClDeclName d) of
		Nothing -> (initialVersion, d)
		Just v  -> (v, d)


--NOT REALLY: deprecsToIfaceDeprecs :: Deprecations -> IfaceDeprecs
deprecsToIfaceDeprecs NoDeprecs = Nothing
deprecsToIfaceDeprecs (DeprecAll txt) = Just (Left txt)
deprecsToIfaceDeprecs (DeprecSome env) = Just (Right (nameEnvElts env))


{-! for GenAvailInfo derive: Binary !-}
{-! for WhatsImported derive: Binary !-}

-- For binary interfaces we need to convert the ImportVersion Names to OccNames
usageToOccName :: Usage Name -> Usage OccName
usageToOccName usg
  = usg { usg_entities = [ (nameOccName n, v) | (n,v) <- usg_entities usg ] }

exportItemToRdrExportItem (mn, avails) 
  = (mn, map availInfoToRdrAvailInfo avails)

availInfoToRdrAvailInfo :: AvailInfo -> RdrAvailInfo
availInfoToRdrAvailInfo (Avail n)
   = Avail (nameOccName n)
availInfoToRdrAvailInfo (AvailTC n ns)
  = AvailTC (nameOccName n) (map nameOccName ns)

-- ---------------------------------------------------------------------------
-- Reading a binary interface into ParsedIface

instance Binary ParsedIface where
   put_ bh ParsedIface{
		 pi_mod = module_name,
		 pi_pkg = pkg_name,
		 pi_vers = module_ver,
		 pi_orphan = orphan,
		 pi_usages = usages,
		 pi_exports = exports,
		 pi_decls = tycl_decls,
		 pi_fixity = fixities,
		 pi_insts = insts,
		 pi_rules = rules,
		 pi_deprecs = deprecs } = do
	build_tag <- readIORef v_Build_tag
	put_ bh (show opt_HiVersion ++ build_tag)
	put_ bh module_name
	put_ bh pkg_name
	put_ bh module_ver
	put_ bh orphan
	lazyPut bh usages
	put_ bh exports
        put_ bh tycl_decls
	put_ bh fixities
	put_ bh insts
	lazyPut bh rules
	lazyPut bh deprecs
   get bh = do
	check_ver   <- get bh
	let our_ver = show opt_HiVersion
        when (check_ver /= our_ver) $
	   -- use userError because this will be caught by readIface
	   -- which will emit an error msg containing the iface module name.
	   throwDyn (ProgramError (
		"mismatched interface file versions: expected "
		++ our_ver ++ ", found " ++ check_ver))

	check_way <- get bh
        ignore_way <- readIORef v_IgnoreHiWay
	build_tag <- readIORef v_Build_tag
        when (not ignore_way && check_way /= build_tag) $
	   -- use userError because this will be caught by readIface
	   -- which will emit an error msg containing the iface module name.
	   throwDyn (ProgramError (
		"mismatched interface file ways: expected "
		++ build_tag ++ ", found " ++ check_way))

	module_name <- get bh		-- same rep. as Module, so that's ok
	pkg_name    <- get bh
	module_ver  <- get bh
	orphan      <- get bh
	deps	    <- lazyGet bh
	usages	    <- {-# SCC "bin_usages" #-} lazyGet bh
	exports	    <- {-# SCC "bin_exports" #-} get bh
        tycl_decls  <- {-# SCC "bin_tycldecls" #-} get bh
	fixities    <- {-# SCC "bin_fixities" #-} get bh
	insts       <- {-# SCC "bin_insts" #-} get bh
	rules	    <- {-# SCC "bin_rules" #-} lazyGet bh
	deprecs     <- {-# SCC "bin_deprecs" #-} lazyGet bh
	return (ParsedIface {
		 pi_mod = module_name,
		 pi_pkg = pkg_name,
		 pi_vers = module_ver,
		 pi_orphan = orphan,
		 pi_deps = deps,
		 pi_usages = usages,
		 pi_exports = exports,
		 pi_decls = tycl_decls,
		 pi_fixity = fixities,
		 pi_insts = reverse insts,
		 pi_rules = rules,
		 pi_deprecs = deprecs })

GLOBAL_VAR(v_IgnoreHiWay, False, Bool)

-- ----------------------------------------------------------------------------
{-* Generated by DrIFT-v1.0 : Look, but Don't Touch. *-}

--  Imported from other files :-

instance Binary Dependencies where
    put_ bh deps = do put_ bh (dep_mods deps)
		      put_ bh (dep_pkgs deps)
		      put_ bh (dep_orphs deps)

    get bh = do ms <- get bh 
		ps <- get bh
		os <- get bh
		return (Deps { dep_mods = ms, dep_pkgs = ps, dep_orphs = os })

instance (Binary name) => Binary (GenAvailInfo name) where
    put_ bh (Avail aa) = do
	    putByte bh 0
	    put_ bh aa
    put_ bh (AvailTC ab ac) = do
	    putByte bh 1
	    put_ bh ab
	    put_ bh ac
    get bh = do
	    h <- getByte bh
	    case h of
	      0 -> do aa <- get bh
		      return (Avail aa)
	      _ -> do ab <- get bh
		      ac <- get bh
		      return (AvailTC ab ac)

instance (Binary name) => Binary (Usage name) where
    put_ bh usg	= do 
	put_ bh (usg_name     usg)
	put_ bh (usg_mod      usg)
	put_ bh (usg_exports  usg)
	put_ bh (usg_entities usg)
	put_ bh (usg_rules    usg)

    get bh = do
	nm    <- get bh
	mod   <- get bh
	exps  <- get bh
	ents  <- get bh
	rules <- get bh
	return (Usage {	usg_name = nm, usg_mod = mod,
			usg_exports = exps, usg_entities = ents,
			usg_rules = rules })

instance Binary Activation where
    put_ bh NeverActive = do
	    putByte bh 0
    put_ bh AlwaysActive = do
	    putByte bh 1
    put_ bh (ActiveBefore aa) = do
	    putByte bh 2
	    put_ bh aa
    put_ bh (ActiveAfter ab) = do
	    putByte bh 3
	    put_ bh ab
    get bh = do
	    h <- getByte bh
	    case h of
	      0 -> do return NeverActive
	      1 -> do return AlwaysActive
	      2 -> do aa <- get bh
		      return (ActiveBefore aa)
	      _ -> do ab <- get bh
		      return (ActiveAfter ab)

instance Binary StrictnessMark where
    put_ bh MarkedUserStrict = do
	    putByte bh 0
    put_ bh MarkedStrict = do
	    putByte bh 1
    put_ bh MarkedUnboxed = do
	    putByte bh 2
    put_ bh NotMarkedStrict = do
	    putByte bh 3
    get bh = do
	    h <- getByte bh
	    case h of
	      0 -> do return MarkedUserStrict
	      1 -> do return MarkedStrict
	      2 -> do return MarkedUnboxed
	      _ -> do return NotMarkedStrict

instance Binary Boxity where
    put_ bh Boxed = do
	    putByte bh 0
    put_ bh Unboxed = do
	    putByte bh 1
    get bh = do
	    h <- getByte bh
	    case h of
	      0 -> do return Boxed
	      _ -> do return Unboxed

instance Binary NewOrData where
    put_ bh NewType = do
	    putByte bh 0
    put_ bh DataType = do
	    putByte bh 1
    get bh = do
	    h <- getByte bh
	    case h of
	      0 -> do return NewType
	      _ -> do return DataType

instance Binary FixityDirection where
    put_ bh InfixL = do
	    putByte bh 0
    put_ bh InfixR = do
	    putByte bh 1
    put_ bh InfixN = do
	    putByte bh 2
    get bh = do
	    h <- getByte bh
	    case h of
	      0 -> do return InfixL
	      1 -> do return InfixR
	      _ -> do return InfixN

instance Binary Fixity where
    put_ bh (Fixity aa ab) = do
	    put_ bh aa
	    put_ bh ab
    get bh = do
	  aa <- get bh
	  ab <- get bh
	  return (Fixity aa ab)

instance (Binary name) => Binary (FixitySig name) where
    put_ bh (FixitySig aa ab _) = do
	    put_ bh aa
	    put_ bh ab
    get bh = do
	  aa <- get bh
	  ab <- get bh
	  return (FixitySig aa ab noSrcLoc)

instance (Binary name) => Binary (IPName name) where
    put_ bh (Dupable aa) = do
	    putByte bh 0
	    put_ bh aa
    put_ bh (Linear ab) = do
	    putByte bh 1
	    put_ bh ab
    get bh = do
	    h <- getByte bh
	    case h of
	      0 -> do aa <- get bh
		      return (Dupable aa)
	      _ -> do ab <- get bh
		      return (Linear ab)

instance Binary Demand where
    put_ bh Top = do
	    putByte bh 0
    put_ bh Abs = do
	    putByte bh 1
    put_ bh (Call aa) = do
	    putByte bh 2
	    put_ bh aa
    put_ bh (Eval ab) = do
	    putByte bh 3
	    put_ bh ab
    put_ bh (Defer ac) = do
	    putByte bh 4
	    put_ bh ac
    put_ bh (Box ad) = do
	    putByte bh 5
	    put_ bh ad
    put_ bh Bot = do
	    putByte bh 6
    get bh = do
	    h <- getByte bh
	    case h of
	      0 -> do return Top
	      1 -> do return Abs
	      2 -> do aa <- get bh
		      return (Call aa)
	      3 -> do ab <- get bh
		      return (Eval ab)
	      4 -> do ac <- get bh
		      return (Defer ac)
	      5 -> do ad <- get bh
		      return (Box ad)
	      _ -> do return Bot

instance Binary Demands where
    put_ bh (Poly aa) = do
	    putByte bh 0
	    put_ bh aa
    put_ bh (Prod ab) = do
	    putByte bh 1
	    put_ bh ab
    get bh = do
	    h <- getByte bh
	    case h of
	      0 -> do aa <- get bh
		      return (Poly aa)
	      _ -> do ab <- get bh
		      return (Prod ab)

instance Binary DmdResult where
    put_ bh TopRes = do
	    putByte bh 0
    put_ bh RetCPR = do
	    putByte bh 1
    put_ bh BotRes = do
	    putByte bh 2
    get bh = do
	    h <- getByte bh
	    case h of
	      0 -> do return TopRes
	      1 -> do return RetCPR	-- Really use RetCPR even if -fcpr-off
					-- The wrapper was generated for CPR in 
					-- the imported module!
	      _ -> do return BotRes

instance Binary StrictSig where
    put_ bh (StrictSig aa) = do
	    put_ bh aa
    get bh = do
	  aa <- get bh
	  return (StrictSig aa)

instance (Binary name) => Binary (HsTyVarBndr name) where
    put_ bh (UserTyVar aa) = do
	    putByte bh 0
	    put_ bh aa
    put_ bh (IfaceTyVar ab ac) = do
	    putByte bh 1
	    put_ bh ab
	    put_ bh ac
    get bh = do
	    h <- getByte bh
	    case h of
	      0 -> do aa <- get bh
		      return (UserTyVar aa)
	      _ -> do ab <- get bh
		      ac <- get bh
		      return (IfaceTyVar ab ac)

instance Binary HsTupCon where
    put_ bh (HsTupCon ab ac) = do
	    put_ bh ab
	    put_ bh ac
    get bh = do
	  ab <- get bh
	  ac <- get bh
	  return (HsTupCon ab ac)

instance (Binary name) => Binary (HsTyOp name) where
    put_ bh HsArrow    = putByte bh 0
    put_ bh (HsTyOp n) = do putByte bh 1
			    put_ bh n

    get bh = do h <- getByte bh
		case h of
		  0 -> return HsArrow
		  1 -> do a <- get bh
		          return (HsTyOp a)

instance (Binary name) => Binary (HsType name) where
    put_ bh (HsForAllTy aa ab ac) = do
	    putByte bh 0
	    put_ bh aa
	    put_ bh ab
	    put_ bh ac
    put_ bh (HsTyVar ad) = do
	    putByte bh 1
	    put_ bh ad
    put_ bh (HsAppTy ae af) = do
	    putByte bh 2
	    put_ bh ae
	    put_ bh af
    put_ bh (HsFunTy ag ah) = do
	    putByte bh 3
	    put_ bh ag
	    put_ bh ah
    put_ bh (HsListTy ai) = do
	    putByte bh 4
	    put_ bh ai
    put_ bh (HsPArrTy aj) = do
	    putByte bh 5
	    put_ bh aj
    put_ bh (HsTupleTy ak al) = do
	    putByte bh 6
	    put_ bh ak
	    put_ bh al
    put_ bh (HsOpTy am an ao) = do
	    putByte bh 7
	    put_ bh am
	    put_ bh an
	    put_ bh ao
    put_ bh (HsNumTy ap) = do
	    putByte bh 8
	    put_ bh ap
    put_ bh (HsPredTy aq) = do
	    putByte bh 9
	    put_ bh aq
    put_ bh (HsKindSig ar as) = do
	    putByte bh 10
	    put_ bh ar
	    put_ bh as
    get bh = do
	    h <- getByte bh
	    case h of
	      0 -> do aa <- get bh
		      ab <- get bh
		      ac <- get bh
		      return (HsForAllTy aa ab ac)
	      1 -> do ad <- get bh
		      return (HsTyVar ad)
	      2 -> do ae <- get bh
		      af <- get bh
		      return (HsAppTy ae af)
	      3 -> do ag <- get bh
		      ah <- get bh
		      return (HsFunTy ag ah)
	      4 -> do ai <- get bh
		      return (HsListTy ai)
	      5 -> do aj <- get bh
		      return (HsPArrTy aj)
	      6 -> do ak <- get bh
		      al <- get bh
		      return (HsTupleTy ak al)
	      7 -> do am <- get bh
		      an <- get bh
		      ao <- get bh
		      return (HsOpTy am an ao)
	      8 -> do ap <- get bh
		      return (HsNumTy ap)
	      9 -> do aq <- get bh
		      return (HsPredTy aq)
	      _ -> do ar <- get bh
		      as <- get bh
		      return (HsKindSig ar as)

instance (Binary name) => Binary (HsPred name) where
    put_ bh (HsClassP aa ab) = do
	    putByte bh 0
	    put_ bh aa
	    put_ bh ab
    put_ bh (HsIParam ac ad) = do
	    putByte bh 1
	    put_ bh ac
	    put_ bh ad
    get bh = do
	    h <- getByte bh
	    case h of
	      0 -> do aa <- get bh
		      ab <- get bh
		      return (HsClassP aa ab)
	      _ -> do ac <- get bh
		      ad <- get bh
		      return (HsIParam ac ad)

instance (Binary name) => Binary (UfExpr name) where
    put_ bh (UfVar aa) = do
	    putByte bh 0
	    put_ bh aa
    put_ bh (UfType ab) = do
	    putByte bh 1
	    put_ bh ab
    put_ bh (UfTuple ac ad) = do
	    putByte bh 2
	    put_ bh ac
	    put_ bh ad
    put_ bh (UfLam ae af) = do
	    putByte bh 3
	    put_ bh ae
	    put_ bh af
    put_ bh (UfApp ag ah) = do
	    putByte bh 4
	    put_ bh ag
	    put_ bh ah
    put_ bh (UfCase ai aj ak) = do
	    putByte bh 5
	    put_ bh ai
	    put_ bh aj
	    put_ bh ak
    put_ bh (UfLet al am) = do
	    putByte bh 6
	    put_ bh al
	    put_ bh am
    put_ bh (UfNote an ao) = do
	    putByte bh 7
	    put_ bh an
	    put_ bh ao
    put_ bh (UfLit ap) = do
	    putByte bh 8
	    put_ bh ap
    put_ bh (UfFCall as at) = do
	    putByte bh 9
	    put_ bh as
	    put_ bh at
    get bh = do
	    h <- getByte bh
	    case h of
	      0 -> do aa <- get bh
		      return (UfVar aa)
	      1 -> do ab <- get bh
		      return (UfType ab)
	      2 -> do ac <- get bh
		      ad <- get bh
		      return (UfTuple ac ad)
	      3 -> do ae <- get bh
		      af <- get bh
		      return (UfLam ae af)
	      4 -> do ag <- get bh
		      ah <- get bh
		      return (UfApp ag ah)
	      5 -> do ai <- get bh
		      aj <- get bh
		      ak <- get bh
		      return (UfCase ai aj ak)
	      6 -> do al <- get bh
		      am <- get bh
		      return (UfLet al am)
	      7 -> do an <- get bh
		      ao <- get bh
		      return (UfNote an ao)
	      8 -> do ap <- get bh
		      return (UfLit ap)
	      _ -> do as <- get bh
		      at <- get bh
		      return (UfFCall as at)

instance (Binary name) => Binary (UfConAlt name) where
    put_ bh UfDefault = do
	    putByte bh 0
    put_ bh (UfDataAlt aa) = do
	    putByte bh 1
	    put_ bh aa
    put_ bh (UfTupleAlt ab) = do
	    putByte bh 2
	    put_ bh ab
    put_ bh (UfLitAlt ac) = do
	    putByte bh 3
	    put_ bh ac
    get bh = do
	    h <- getByte bh
	    case h of
	      0 -> do return UfDefault
	      1 -> do aa <- get bh
		      return (UfDataAlt aa)
	      2 -> do ab <- get bh
		      return (UfTupleAlt ab)
	      _ -> do ac <- get bh
		      return (UfLitAlt ac)

instance (Binary name) => Binary (UfBinding name) where
    put_ bh (UfNonRec aa ab) = do
	    putByte bh 0
	    put_ bh aa
	    put_ bh ab
    put_ bh (UfRec ac) = do
	    putByte bh 1
	    put_ bh ac
    get bh = do
	    h <- getByte bh
	    case h of
	      0 -> do aa <- get bh
		      ab <- get bh
		      return (UfNonRec aa ab)
	      _ -> do ac <- get bh
		      return (UfRec ac)

instance (Binary name) => Binary (UfBinder name) where
    put_ bh (UfValBinder aa ab) = do
	    putByte bh 0
	    put_ bh aa
	    put_ bh ab
    put_ bh (UfTyBinder ac ad) = do
	    putByte bh 1
	    put_ bh ac
	    put_ bh ad
    get bh = do
	    h <- getByte bh
	    case h of
	      0 -> do aa <- get bh
		      ab <- get bh
		      return (UfValBinder aa ab)
	      _ -> do ac <- get bh
		      ad <- get bh
		      return (UfTyBinder ac ad)

instance (Binary name) => Binary (HsIdInfo name) where
    put_ bh (HsArity aa) = do
	    putByte bh 0
	    put_ bh aa
    put_ bh (HsStrictness ab) = do
	    putByte bh 1
	    put_ bh ab
    put_ bh (HsUnfold ac ad) = do
	    putByte bh 2
	    put_ bh ac
	    put_ bh ad
    put_ bh HsNoCafRefs = do
	    putByte bh 3
    put_ bh (HsWorker ae af) = do
	    putByte bh 4
	    put_ bh ae
	    put_ bh af
    get bh = do
	    h <- getByte bh
	    case h of
	      0 -> do aa <- get bh
		      return (HsArity aa)
	      1 -> do ab <- get bh
		      return (HsStrictness ab)
	      2 -> do ac <- get bh
		      ad <- get bh
		      return (HsUnfold ac ad)
	      3 -> do return HsNoCafRefs
	      _ -> do ae <- get bh
		      af <- get bh
		      return (HsWorker ae af)

instance (Binary name) => Binary (UfNote name) where
    put_ bh (UfSCC aa) = do
	    putByte bh 0
	    put_ bh aa
    put_ bh (UfCoerce ab) = do
	    putByte bh 1
	    put_ bh ab
    put_ bh UfInlineCall = do
	    putByte bh 2
    put_ bh UfInlineMe = do
	    putByte bh 3
    put_ bh (UfCoreNote s) = do
            putByte bh 4
            put_ bh s
    get bh = do
	    h <- getByte bh
	    case h of
	      0 -> do aa <- get bh
		      return (UfSCC aa)
	      1 -> do ab <- get bh
		      return (UfCoerce ab)
	      2 -> do return UfInlineCall
	      3 -> do return UfInlineMe
              _ -> do ac <- get bh
                      return (UfCoreNote ac)

instance (Binary name) => Binary (BangType name) where
    put_ bh (BangType aa ab) = do
	    put_ bh aa
	    put_ bh ab
    get bh = do
	  aa <- get bh
	  ab <- get bh
	  return (BangType aa ab)

instance (Binary name, Binary arg) => Binary (HsConDetails name arg) where
    put_ bh (PrefixCon aa) = do
	    putByte bh 0
	    put_ bh aa
    put_ bh (InfixCon ab ac) = do
	    putByte bh 1
	    put_ bh ab
	    put_ bh ac
    put_ bh (RecCon ad) = do
	    putByte bh 2
	    put_ bh ad
    get bh = do
	    h <- getByte bh
	    case h of
	      0 -> do aa <- get bh
		      return (PrefixCon aa)
	      1 -> do ab <- get bh
		      ac <- get bh
		      return (InfixCon ab ac)
	      _ -> do ad <- get bh
		      return (RecCon ad)

instance (Binary datacon) => Binary (DataConDetails datacon) where
    put_ bh (DataCons aa) = do
	    putByte bh 0
	    put_ bh aa
    put_ bh Unknown = do
	    putByte bh 1
    put_ bh (HasCons ab) = do
	    putByte bh 2
	    put_ bh ab
    get bh = do
	    h <- getByte bh
	    case h of
	      0 -> do aa <- get bh
		      return (DataCons aa)
	      1 -> do return Unknown
	      _ -> do ab <- get bh
		      return (HasCons ab)

instance (Binary id) => Binary (DefMeth id) where
    put_ bh NoDefMeth = do
	    putByte bh 0
    put_ bh (DefMeth aa) = do
	    putByte bh 1
	    put_ bh aa
    put_ bh GenDefMeth = do
	    putByte bh 2
    get bh = do
	    h <- getByte bh
	    case h of
	      0 -> do return NoDefMeth
	      1 -> do aa <- get bh
		      return (DefMeth aa)
	      _ -> do return GenDefMeth

instance Binary IsCafCC where
    put_ bh CafCC = do
	    putByte bh 0
    put_ bh NotCafCC = do
	    putByte bh 1
    get bh = do
	    h <- getByte bh
	    case h of
	      0 -> do return CafCC
	      _ -> do return NotCafCC

instance Binary IsDupdCC where
    put_ bh OriginalCC = do
	    putByte bh 0
    put_ bh DupdCC = do
	    putByte bh 1
    get bh = do
	    h <- getByte bh
	    case h of
	      0 -> do return OriginalCC
	      _ -> do return DupdCC

instance Binary CostCentre where
    put_ bh NoCostCentre = do
	    putByte bh 0
    put_ bh (NormalCC aa ab ac ad) = do
	    putByte bh 1
	    put_ bh aa
	    put_ bh ab
	    put_ bh ac
	    put_ bh ad
    put_ bh (AllCafsCC ae) = do
	    putByte bh 2
	    put_ bh ae
    get bh = do
	    h <- getByte bh
	    case h of
	      0 -> do return NoCostCentre
	      1 -> do aa <- get bh
		      ab <- get bh
		      ac <- get bh
		      ad <- get bh
		      return (NormalCC aa ab ac ad)
	      _ -> do ae <- get bh
		      return (AllCafsCC ae)
