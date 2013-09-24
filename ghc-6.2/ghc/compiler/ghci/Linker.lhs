%
% (c) The University of Glasgow 2000
%

-- --------------------------------------
-- 	The dynamic linker for GHCi      
-- --------------------------------------

This module deals with the top-level issues of dynamic linking,
calling the object-code linker and the byte-code linker where
necessary.


\begin{code}

{-# OPTIONS -optc-DNON_POSIX_SOURCE -#include "Linker.h" #-}

module Linker ( HValue, initDynLinker, showLinkerState,
		linkExpr, unload, extendLinkEnv, 
		linkPackages,
	) where

#include "../includes/config.h"
#include "HsVersions.h"

import ObjLink		( loadDLL, loadObj, unloadObj, resolveObjs, initObjLinker )
import ByteCodeLink	( HValue, ClosureEnv, extendClosureEnv, linkBCO )
import ByteCodeItbls	( ItblEnv )
import ByteCodeAsm	( CompiledByteCode(..), bcoFreeNames, UnlinkedBCO(..))

import Packages
import DriverState	( v_Library_paths, v_Opt_l, v_Ld_inputs, getStaticOpts, v_ExplicitPackages )
import DriverPhases	( isObjectFilename, isDynLibFilename )
import DriverUtil	( getFileSuffix )
#ifdef darwin_TARGET_OS
import DriverState	( v_Cmdline_frameworks, v_Framework_paths )
#endif
import Finder		( findModule, findLinkable )
import HscTypes
import Name		( Name,  nameModule, isExternalName, isWiredInName )
import NameEnv
import NameSet		( nameSetToList )
import Module
import FastString	( FastString(..), unpackFS )
import ListSetOps	( minusList )
import CmdLineOpts	( DynFlags(verbosity), getDynFlags )
import BasicTypes	( SuccessFlag(..), succeeded, failed )
import Outputable
import Panic            ( GhcException(..) )
import Util             ( zipLazy, global )

-- Standard libraries
import Control.Monad	( when, filterM, foldM )

import Data.IORef	( IORef, readIORef, writeIORef )
import Data.List	( partition, nub )

import System.IO	( putStr, putStrLn, hPutStrLn, stderr, fixIO )
import System.Directory	( doesFileExist )

import Control.Exception ( block, throwDyn )

#if __GLASGOW_HASKELL__ >= 503
import GHC.IOBase	( IO(..) )
#else
import PrelIOBase	( IO(..) )
#endif
\end{code}


%************************************************************************
%*									*
			The Linker's state
%*									*
%************************************************************************

The persistent linker state *must* match the actual state of the 
C dynamic linker at all times, so we keep it in a private global variable.


The PersistentLinkerState maps Names to actual closures (for
interpreted code only), for use during linking.

\begin{code}
GLOBAL_VAR(v_PersistentLinkerState, panic "Dynamic linker not initialised", PersistentLinkerState)
GLOBAL_VAR(v_InitLinkerDone, False, Bool)	-- Set True when dynamic linker is initialised

data PersistentLinkerState
   = PersistentLinkerState {

	-- Current global mapping from Names to their true values
        closure_env :: ClosureEnv,

	-- The current global mapping from RdrNames of DataCons to
	-- info table addresses.
	-- When a new Unlinked is linked into the running image, or an existing
	-- module in the image is replaced, the itbl_env must be updated
	-- appropriately.
        itbl_env    :: ItblEnv,

	-- The currently loaded interpreted modules (home package)
	bcos_loaded :: [Linkable],

	-- And the currently-loaded compiled modules (home package)
	objs_loaded :: [Linkable],

	-- The currently-loaded packages; always object code
	-- Held, as usual, in dependency order; though I am not sure if
	-- that is really important
	pkgs_loaded :: [PackageName]
     }

emptyPLS :: PersistentLinkerState
emptyPLS = PersistentLinkerState { closure_env = emptyNameEnv,
                                   itbl_env    = emptyNameEnv,
				   pkgs_loaded = init_pkgs_loaded,
				   bcos_loaded = [],
				   objs_loaded = [] }

-- Packages that don't need loading, because the compiler 
-- shares them with the interpreted program.
--
-- The linker's symbol table is populated with RTS symbols using an
-- explicit list.  See rts/Linker.c for details.
init_pkgs_loaded = [ FSLIT("rts") ]
\end{code}

\begin{code}
extendLinkEnv :: [(Name,HValue)] -> IO ()
-- Automatically discards shadowed bindings
extendLinkEnv new_bindings
  = do	pls <- readIORef v_PersistentLinkerState
	let new_closure_env = extendClosureEnv (closure_env pls) new_bindings
	    new_pls = pls { closure_env = new_closure_env }
	writeIORef v_PersistentLinkerState new_pls

-- filterNameMap removes from the environment all entries except 
-- 	those for a given set of modules;
-- Note that this removes all *local* (i.e. non-isExternal) names too 
--	(these are the temporary bindings from the command line).
-- Used to filter both the ClosureEnv and ItblEnv

filterNameMap :: [ModuleName] -> NameEnv (Name, a) -> NameEnv (Name, a)
filterNameMap mods env 
   = filterNameEnv keep_elt env
   where
     keep_elt (n,_) = isExternalName n 
		      && (moduleName (nameModule n) `elem` mods)
\end{code}


\begin{code}
showLinkerState :: IO ()
-- Display the persistent linker state
showLinkerState
  = do pls <- readIORef v_PersistentLinkerState
       printDump (vcat [text "----- Linker state -----",
			text "Pkgs:" <+> ppr (pkgs_loaded pls),
			text "Objs:" <+> ppr (objs_loaded pls),
			text "BCOs:" <+> ppr (bcos_loaded pls)])
\end{code}
			
	


%************************************************************************
%*									*
\subsection{Initialisation}
%*									*
%************************************************************************

We initialise the dynamic linker by

a) calling the C initialisation procedure

b) Loading any packages specified on the command line,
   now held in v_ExplicitPackages

c) Loading any packages specified on the command line,
   now held in the -l options in v_Opt_l

d) Loading any .o/.dll files specified on the command line,
   now held in v_Ld_inputs

e) Loading any MacOS frameworks

\begin{code}
initDynLinker :: IO ()
-- This function is idempotent; if called more than once, it does nothing
-- This is useful in Template Haskell, where we call it before trying to link
initDynLinker 
  = do	{ done <- readIORef v_InitLinkerDone
	; if done then return () 
		  else do { writeIORef v_InitLinkerDone True
			  ; reallyInitDynLinker }
	}

reallyInitDynLinker
  = do  { dflags <- getDynFlags

		-- Initialise the linker state
	; writeIORef v_PersistentLinkerState emptyPLS

	 	-- (a) initialise the C dynamic linker
	; initObjLinker 

		-- (b) Load packages from the command-line
	; expl <- readIORef v_ExplicitPackages
	; linkPackages dflags expl

	   	-- (c) Link libraries from the command-line
	; opt_l  <- getStaticOpts v_Opt_l
	; let minus_ls = [ lib | '-':'l':lib <- opt_l ]

	   	-- (d) Link .o files from the command-line
	; lib_paths <- readIORef v_Library_paths
	; cmdline_ld_inputs <- readIORef v_Ld_inputs

	; classified_ld_inputs <- mapM classifyLdInput cmdline_ld_inputs

	   	-- (e) Link any MacOS frameworks
#ifdef darwin_TARGET_OS	
	; framework_paths <- readIORef v_Framework_paths
	; frameworks      <- readIORef v_Cmdline_frameworks
#else
	; let frameworks      = []
	; let framework_paths = []
#endif
		-- Finally do (c),(d),(e)	
        ; let cmdline_lib_specs = [ l | Just l <- classified_ld_inputs ]
			       ++ map DLL       minus_ls 
			       ++ map Framework frameworks
	; if null cmdline_lib_specs then return ()
				    else do

 	{ mapM_ (preloadLib dflags lib_paths framework_paths) cmdline_lib_specs
	; maybePutStr dflags "final link ... "
	; ok <- resolveObjs

	; if succeeded ok then maybePutStrLn dflags "done"
	  else throwDyn (InstallationError "linking extra libraries/objects failed")
	}}

classifyLdInput :: FilePath -> IO (Maybe LibrarySpec)
classifyLdInput f
  | isObjectFilename f = return (Just (Object f))
  | isDynLibFilename f = return (Just (DLLPath f))
  | otherwise 	       = do
	hPutStrLn stderr ("Warning: ignoring unrecognised input `" ++ f ++ "'")
	return Nothing

preloadLib :: DynFlags -> [String] -> [String] -> LibrarySpec -> IO ()
preloadLib dflags lib_paths framework_paths lib_spec
  = do maybePutStr dflags ("Loading object " ++ showLS lib_spec ++ " ... ")
       case lib_spec of
          Object static_ish
             -> do b <- preload_static lib_paths static_ish
                   maybePutStrLn dflags (if b  then "done"
	 					else "not found")
	 
          DLL dll_unadorned
             -> do maybe_errstr <- loadDynamic lib_paths dll_unadorned
                   case maybe_errstr of
                      Nothing -> maybePutStrLn dflags "done"
                      Just mm -> preloadFailed mm lib_paths lib_spec

	  DLLPath dll_path
	     -> do maybe_errstr <- loadDLL dll_path
                   case maybe_errstr of
                      Nothing -> maybePutStrLn dflags "done"
                      Just mm -> preloadFailed mm lib_paths lib_spec

#ifdef darwin_TARGET_OS
	  Framework framework
             -> do maybe_errstr <- loadFramework framework_paths framework
                   case maybe_errstr of
                      Nothing -> maybePutStrLn dflags "done"
                      Just mm -> preloadFailed mm framework_paths lib_spec
#endif
  where
    preloadFailed :: String -> [String] -> LibrarySpec -> IO ()
    preloadFailed sys_errmsg paths spec
       = do maybePutStr dflags
    	       ("failed.\nDynamic linker error message was:\n   " 
                    ++ sys_errmsg  ++ "\nWhilst trying to load:  " 
                    ++ showLS spec ++ "\nDirectories to search are:\n"
                    ++ unlines (map ("   "++) paths) )
            give_up
    
    -- Not interested in the paths in the static case.
    preload_static paths name
       = do b <- doesFileExist name
            if not b then return False
                     else loadObj name >> return True
    
    give_up = throwDyn $ 
	      CmdLineError "user specified .o/.so/.DLL could not be loaded."
\end{code}


%************************************************************************
%*									*
		Link a byte-code expression
%*									*
%************************************************************************

\begin{code}
linkExpr :: HscEnv -> PersistentCompilerState
	 -> UnlinkedBCO -> IO HValue

-- Link a single expression, *including* first linking packages and 
-- modules that this expression depends on.
--
-- Raises an IO exception if it can't find a compiled version of the
-- dependents to link.

linkExpr hsc_env pcs root_ul_bco
  = do {  
	-- Initialise the linker (if it's not been done already)
     initDynLinker

	-- Find what packages and linkables are required
   ; (lnks, pkgs) <- getLinkDeps hpt pit needed_mods

	-- Link the packages and modules required
   ; linkPackages dflags pkgs
   ; ok <- linkModules dflags lnks
   ; if failed ok then
	dieWith empty
     else do {

	-- Link the expression itself
     pls <- readIORef v_PersistentLinkerState
   ; let ie = itbl_env pls
	 ce = closure_env pls

	-- Link the necessary packages and linkables
   ; (_, (root_hval:_)) <- linkSomeBCOs False ie ce [root_ul_bco]
   ; return root_hval
   }}
   where
     pit    = eps_PIT (pcs_EPS pcs)
     hpt    = hsc_HPT hsc_env
     dflags = hsc_dflags hsc_env
     free_names = nameSetToList (bcoFreeNames root_ul_bco)

     needed_mods :: [Module]
     needed_mods = [ nameModule n | n <- free_names, 
				    isExternalName n,	 	-- Names from other modules
				    not (isWiredInName n)	-- Exclude wired-in names
		   ]						-- (see note below)
	-- Exclude wired-in names because we may not have read
	-- their interface files, so getLinkDeps will fail
	-- All wired-in names are in the base package, which we link
	-- by default, so we can safely ignore them here.
 
dieWith msg = throwDyn (ProgramError (showSDoc msg))

getLinkDeps :: HomePackageTable -> PackageIfaceTable
	    -> [Module]				-- If you need these
	    -> IO ([Linkable], [PackageName])	-- ... then link these first
-- Fails with an IO exception if it can't find enough files

getLinkDeps hpt pit mods
-- Find all the packages and linkables that a set of modules depends on
 = do {	pls <- readIORef v_PersistentLinkerState ;
	let {
	-- 1.  Find the dependent home-pkg-modules/packages from each iface
	    (mods_s, pkgs_s) = unzip (map get_deps mods) ;

	-- 2.  Exclude ones already linked
	--	Main reason: avoid findModule calls in get_linkable
	    mods_needed = nub (concat mods_s) `minusList` linked_mods     ;
	    pkgs_needed = nub (concat pkgs_s) `minusList` pkgs_loaded pls ;

	    linked_mods = map linkableModName (objs_loaded pls ++ bcos_loaded pls)
	} ;
	
	-- 3.  For each dependent module, find its linkable
	--     This will either be in the HPT or (in the case of one-shot
	--     compilation) we may need to use maybe_getFileLinkable
	lnks_needed <- mapM get_linkable mods_needed ;

	return (lnks_needed, pkgs_needed) }
  where
    get_deps :: Module -> ([ModuleName],[PackageName])
	-- Get the things needed for the specified module
	-- This is rather similar to the code in RnNames.importsFromImportDecl
    get_deps mod
	| isHomeModule (mi_module iface) 
	= (moduleName mod : [m | (m,_) <- dep_mods deps], dep_pkgs deps)
	| otherwise
	= ([], mi_package iface : dep_pkgs deps)
	where
	  iface = get_iface mod
	  deps  = mi_deps iface

    get_iface mod = case lookupIface hpt pit mod of
			    Just iface -> iface
			    Nothing    -> pprPanic "getLinkDeps" (no_iface mod)
    no_iface mod = ptext SLIT("No iface for") <+> ppr mod
	-- This one is a GHC bug

    no_obj mod = dieWith (ptext SLIT("No compiled code for") <+> ppr mod)
	-- This one is a build-system bug

    get_linkable mod_name	-- A home-package module
	| Just mod_info <- lookupModuleEnvByName hpt mod_name 
	= return (hm_linkable mod_info)
	| otherwise	
	=	-- It's not in the HPT because we are in one shot mode, 
		-- so use the Finder to get a ModLocation...
	  do { mb_stuff <- findModule mod_name ;
	       case mb_stuff of {
		  Left _ -> no_obj mod_name ;
		  Right (_, loc) -> do {

		-- ...and then find the linkable for it
	       mb_lnk <- findLinkable mod_name loc ;
	       case mb_lnk of {
		  Nothing -> no_obj mod_name ;
		  Just lnk -> return lnk
	  }}}} 
\end{code}


%************************************************************************
%*									*
		Link some linkables
	The linkables may consist of a mixture of 
	byte-code modules and object modules
%*									*
%************************************************************************

\begin{code}
linkModules :: DynFlags -> [Linkable] -> IO SuccessFlag
linkModules dflags linkables
  = block $ do  -- don't want to be interrupted by ^C in here
	
	let (objs, bcos) = partition isObjectLinkable 
                              (concatMap partitionLinkable linkables)

		-- Load objects first; they can't depend on BCOs
	ok_flag <- dynLinkObjs dflags objs

	if failed ok_flag then 
		return Failed
	  else do
		dynLinkBCOs bcos
		return Succeeded
		

-- HACK to support f-x-dynamic in the interpreter; no other purpose
partitionLinkable :: Linkable -> [Linkable]
partitionLinkable li
   = let li_uls = linkableUnlinked li
         li_uls_obj = filter isObject li_uls
         li_uls_bco = filter isInterpretable li_uls
     in 
         case (li_uls_obj, li_uls_bco) of
            (objs@(_:_), bcos@(_:_)) 
               -> [li{linkableUnlinked=li_uls_obj}, li{linkableUnlinked=li_uls_bco}]
            other
               -> [li]

findModuleLinkable_maybe :: [Linkable] -> ModuleName -> Maybe Linkable
findModuleLinkable_maybe lis mod
   = case [LM time nm us | LM time nm us <- lis, nm == mod] of
        []   -> Nothing
        [li] -> Just li
        many -> pprPanic "findModuleLinkable" (ppr mod)

filterModuleLinkables :: (ModuleName -> Bool) -> [Linkable] -> [Linkable]
filterModuleLinkables p ls = filter (p . linkableModName) ls

linkableInSet :: Linkable -> [Linkable] -> Bool
linkableInSet l objs_loaded =
  case findModuleLinkable_maybe objs_loaded (linkableModName l) of
	Nothing -> False
	Just m  -> linkableTime l == linkableTime m
\end{code}


%************************************************************************
%*									*
\subsection{The object-code linker}
%*									*
%************************************************************************

\begin{code}
dynLinkObjs :: DynFlags -> [Linkable] -> IO SuccessFlag
	-- Side-effects the PersistentLinkerState

dynLinkObjs dflags objs
  = do	pls <- readIORef v_PersistentLinkerState

	-- Load the object files and link them
	let (objs_loaded', new_objs) = rmDupLinkables (objs_loaded pls) objs
	    pls1 		     = pls { objs_loaded = objs_loaded' }
	    unlinkeds 		     = concatMap linkableUnlinked new_objs

	mapM loadObj (map nameOfObject unlinkeds)

	-- Link the all together
	ok <- resolveObjs

	-- If resolving failed, unload all our 
	-- object modules and carry on
	if succeeded ok then do
		writeIORef v_PersistentLinkerState pls1
		return Succeeded
	  else do
		pls2 <- unload_wkr dflags [] pls1
		writeIORef v_PersistentLinkerState pls2
		return Failed


rmDupLinkables :: [Linkable] 	-- Already loaded
	       -> [Linkable]	-- New linkables
	       -> ([Linkable],	-- New loaded set (including new ones)
		   [Linkable])	-- New linkables (excluding dups)
rmDupLinkables already ls
  = go already [] ls
  where
    go already extras [] = (already, extras)
    go already extras (l:ls)
	| linkableInSet l already = go already     extras     ls
	| otherwise		  = go (l:already) (l:extras) ls
\end{code}

%************************************************************************
%*									*
\subsection{The byte-code linker}
%*									*
%************************************************************************

\begin{code}
dynLinkBCOs :: [Linkable] -> IO ()
	-- Side-effects the persistent linker state
dynLinkBCOs bcos
  = do	pls <- readIORef v_PersistentLinkerState

	let (bcos_loaded', new_bcos) = rmDupLinkables (bcos_loaded pls) bcos
	    pls1 		     = pls { bcos_loaded = bcos_loaded' }
	    unlinkeds :: [Unlinked]
	    unlinkeds 		     = concatMap linkableUnlinked new_bcos

	    cbcs :: [CompiledByteCode]
	    cbcs      = map byteCodeOfObject unlinkeds
		      
	    	      
	    ul_bcos    = [b | ByteCode bs _  <- cbcs, b <- bs]
	    ies	       = [ie | ByteCode _ ie <- cbcs]
	    gce	      = closure_env pls
            final_ie  = foldr plusNameEnv (itbl_env pls) ies

        (final_gce, linked_bcos) <- linkSomeBCOs True final_ie gce ul_bcos
		-- What happens to these linked_bcos?

	let pls2 = pls1 { closure_env = final_gce,
			  itbl_env    = final_ie }

	writeIORef v_PersistentLinkerState pls2
	return ()

-- Link a bunch of BCOs and return them + updated closure env.
linkSomeBCOs :: Bool 	-- False <=> add _all_ BCOs to returned closure env
                        -- True  <=> add only toplevel BCOs to closure env
             -> ItblEnv 
             -> ClosureEnv 
             -> [UnlinkedBCO]
             -> IO (ClosureEnv, [HValue])
			-- The returned HValues are associated 1-1 with
			-- the incoming unlinked BCOs.  Each gives the
			-- value of the corresponding unlinked BCO
					

linkSomeBCOs toplevs_only ie ce_in ul_bcos
   = do let nms = map unlinkedBCOName ul_bcos
        hvals <- fixIO 
                    ( \ hvs -> let ce_out = extendClosureEnv ce_in (zipLazy nms hvs)
                               in  mapM (linkBCO ie ce_out) ul_bcos )

        let ce_all_additions = zip nms hvals
            ce_top_additions = filter (isExternalName.fst) ce_all_additions
            ce_additions     = if toplevs_only then ce_top_additions 
                                               else ce_all_additions
            ce_out = -- make sure we're not inserting duplicate names into the 
		     -- closure environment, which leads to trouble.
		     ASSERT (all (not . (`elemNameEnv` ce_in)) (map fst ce_additions))
		     extendClosureEnv ce_in ce_additions
        return (ce_out, hvals)

\end{code}


%************************************************************************
%*									*
		Unload some object modules
%*									*
%************************************************************************

\begin{code}
-- ---------------------------------------------------------------------------
-- Unloading old objects ready for a new compilation sweep.
--
-- The compilation manager provides us with a list of linkables that it
-- considers "stable", i.e. won't be recompiled this time around.  For
-- each of the modules current linked in memory,
--
--	* if the linkable is stable (and it's the same one - the
--	  user may have recompiled the module on the side), we keep it,
--
--	* otherwise, we unload it.
--
--      * we also implicitly unload all temporary bindings at this point.

unload :: DynFlags -> [Linkable] -> IO ()
-- The 'linkables' are the ones to *keep*

unload dflags linkables
  = block $ do -- block, so we're safe from Ctrl-C in here

	pls	<- readIORef v_PersistentLinkerState
	new_pls <- unload_wkr dflags linkables pls
	writeIORef v_PersistentLinkerState new_pls

       	let verb = verbosity dflags
       	when (verb >= 3) $ do
	    hPutStrLn stderr (showSDoc
		(text "unload: retaining objs" <+> ppr (objs_loaded new_pls)))
	    hPutStrLn stderr (showSDoc
		(text "unload: retaining bcos" <+> ppr (bcos_loaded new_pls)))

       	return ()

unload_wkr :: DynFlags
           -> [Linkable]		-- stable linkables
	   -> PersistentLinkerState
           -> IO PersistentLinkerState
-- Does the core unload business
-- (the wrapper blocks exceptions and deals with the PLS get and put)

unload_wkr dflags linkables pls
  = do	let (objs_to_keep, bcos_to_keep) = partition isObjectLinkable linkables

	objs_loaded' <- filterM (maybeUnload objs_to_keep) (objs_loaded pls)
        bcos_loaded' <- filterM (maybeUnload bcos_to_keep) (bcos_loaded pls)

       	let objs_retained = map linkableModName objs_loaded'
	    bcos_retained = map linkableModName bcos_loaded'
	    itbl_env'     = filterNameMap bcos_retained (itbl_env pls)
            closure_env'  = filterNameMap bcos_retained (closure_env pls)
  	    new_pls = pls { itbl_env = itbl_env',
			    closure_env = closure_env',
			    bcos_loaded = bcos_loaded',
			    objs_loaded = objs_loaded' }

	return new_pls
  where
    maybeUnload :: [Linkable] -> Linkable -> IO Bool
    maybeUnload keep_linkables lnk
      | linkableInSet lnk linkables = return True
      | otherwise		    
      = do mapM_ unloadObj [f | DotO f <- linkableUnlinked lnk]
		-- The components of a BCO linkable may contain
		-- dot-o files.  Which is very confusing.
		--
		-- But the BCO parts can be unlinked just by 
		-- letting go of them (plus of course depopulating
		-- the symbol table which is done in the main body)
	   return False
\end{code}


%************************************************************************
%*									*
		Loading packages
%*									*
%************************************************************************


\begin{code}
data LibrarySpec 
   = Object FilePath 	-- Full path name of a .o file, including trailing .o
			-- For dynamic objects only, try to find the object 
			-- file in all the directories specified in 
			-- v_Library_paths before giving up.

   | DLL String		-- "Unadorned" name of a .DLL/.so
			--  e.g.    On unix     "qt"  denotes "libqt.so"
			--          On WinDoze  "burble"  denotes "burble.DLL"
			--  loadDLL is platform-specific and adds the lib/.so/.DLL
			--  suffixes platform-dependently

   | DLLPath FilePath   -- Absolute or relative pathname to a dynamic library
			-- (ends with .dll or .so).

   | Framework String	-- Only used for darwin, but does no harm

-- If this package is already part of the GHCi binary, we'll already
-- have the right DLLs for this package loaded, so don't try to
-- load them again.
-- 
-- But on Win32 we must load them 'again'; doing so is a harmless no-op
-- as far as the loader is concerned, but it does initialise the list
-- of DLL handles that rts/Linker.c maintains, and that in turn is 
-- used by lookupSymbol.  So we must call addDLL for each library 
-- just to get the DLL handle into the list.
partOfGHCi
#          if defined(mingw32_TARGET_OS) || defined(darwin_TARGET_OS)
           = [ ]
#          else
           = [ "base", "haskell98", "haskell-src", "readline" ]
#          endif

showLS (Object nm)    = "(static) " ++ nm
showLS (DLL nm)       = "(dynamic) " ++ nm
showLS (DLLPath nm)   = "(dynamic) " ++ nm
showLS (Framework nm) = "(framework) " ++ nm

linkPackages :: DynFlags -> [PackageName] -> IO ()
-- Link exactly the specified packages, and their dependents
-- (unless of course they are already linked)
-- The dependents are linked automatically, and it doesn't matter
-- what order you specify the input packages.
--
-- NOTE: in fact, since each module tracks all the packages it depends on,
--	 we don't really need to use the package-config dependencies.
-- However we do need the package-config stuff (to find aux libs etc),
-- and following them lets us load libraries in the right order, which 
-- perhaps makes the error message a bit more localised if we get a link
-- failure.  So the dependency walking code is still here.

linkPackages dflags new_pkgs
   = do	{ pls 	  <- readIORef v_PersistentLinkerState
	; pkg_map <- getPackageConfigMap

	; pkgs' <- link pkg_map (pkgs_loaded pls) new_pkgs

	; writeIORef v_PersistentLinkerState (pls { pkgs_loaded = pkgs' })
	}
   where
     link :: PackageConfigMap -> [PackageName] -> [PackageName] -> IO [PackageName]
     link pkg_map pkgs new_pkgs 
	= foldM (link_one pkg_map) pkgs new_pkgs

     link_one pkg_map pkgs new_pkg
	| new_pkg `elem` pkgs	-- Already linked
	= return pkgs

	| Just pkg_cfg <- lookupPkg pkg_map new_pkg
	= do { 	-- Link dependents first
	       pkgs' <- link pkg_map pkgs (packageDependents pkg_cfg)
		-- Now link the package itself
	     ; linkPackage dflags pkg_cfg
	     ; return (new_pkg : pkgs') }

	| otherwise
	= throwDyn (CmdLineError ("unknown package name: " ++ packageNameString new_pkg))


linkPackage :: DynFlags -> PackageConfig -> IO ()
linkPackage dflags pkg
   = do 
        let dirs      =  Packages.library_dirs pkg
        let libs      =  Packages.hs_libraries pkg ++ extra_libraries pkg
				++ [ lib | '-':'l':lib <- extra_ld_opts pkg ]
        classifieds   <- mapM (locateOneObj dirs) libs

        -- Complication: all the .so's must be loaded before any of the .o's.  
	let dlls = [ dll | DLL dll    <- classifieds ]
	    objs = [ obj | Object obj <- classifieds ]

	maybePutStr dflags ("Loading package " ++ Packages.name pkg ++ " ... ")

	-- See comments with partOfGHCi
	when (Packages.name pkg `notElem` partOfGHCi) $ do
	    loadFrameworks pkg
	    mapM_ (load_dyn dirs) dlls
	
	-- After loading all the DLLs, we can load the static objects.
	mapM_ loadObj objs

        maybePutStr dflags "linking ... "
        ok <- resolveObjs
	if succeeded ok then maybePutStrLn dflags "done."
	      else panic ("can't load package `" ++ name pkg ++ "'")

load_dyn dirs dll = do r <- loadDynamic dirs dll
		       case r of
			 Nothing  -> return ()
			 Just err -> throwDyn (CmdLineError ("can't load .so/.DLL for: " 
                                 			      ++ dll ++ " (" ++ err ++ ")" ))
#ifndef darwin_TARGET_OS
loadFrameworks pkg = return ()
#else
loadFrameworks pkg = mapM_ load frameworks
  where
    fw_dirs    = Packages.framework_dirs pkg
    frameworks = Packages.extra_frameworks pkg

    load fw = do  r <- loadFramework fw_dirs fw
		  case r of
		    Nothing  -> return ()
		    Just err -> throwDyn (CmdLineError ("can't load framework: " 
                               			        ++ fw ++ " (" ++ err ++ ")" ))
#endif

-- Try to find an object file for a given library in the given paths.
-- If it isn't present, we assume it's a dynamic library.
locateOneObj :: [FilePath] -> String -> IO LibrarySpec
locateOneObj dirs lib
  = do	{ mb_obj_path <- findFile mk_obj_path dirs 
	; case mb_obj_path of
	    Just obj_path -> return (Object obj_path)
	    Nothing	  -> return (DLL lib) }		-- We assume
   where
     mk_obj_path dir = dir ++ '/':lib ++ ".o"


-- ----------------------------------------------------------------------------
-- Loading a dyanmic library (dlopen()-ish on Unix, LoadLibrary-ish on Win32)

-- return Nothing == success, else Just error message from dlopen
loadDynamic paths rootname
  = do	{ mb_dll <- findFile mk_dll_path paths
	; case mb_dll of
	    Just dll -> loadDLL dll
	    Nothing  -> loadDLL (mkSOName rootname) }
			-- Tried all our known library paths, so let 
			-- dlopen() search its own builtin paths now.
  where
    mk_dll_path dir = dir ++ '/':mkSOName rootname

#if defined(darwin_TARGET_OS)
mkSOName root = "lib" ++ root ++ ".dylib"
#elif defined(mingw32_TARGET_OS)
-- Win32 DLLs have no .dll extension here, because addDLL tries
-- both foo.dll and foo.drv
mkSOName root = root
#else
mkSOName root = "lib" ++ root ++ ".so"
#endif

-- Darwin / MacOS X only: load a framework
-- a framework is a dynamic library packaged inside a directory of the same
-- name. They are searched for in different paths than normal libraries.
#ifdef darwin_TARGET_OS
loadFramework extraPaths rootname
   = do	{ mb_fwk <- findFile mk_fwk (extraPaths ++ defaultFrameworkPaths)
	; case mb_fwk of
	    Just fwk_path -> loadDLL fwk_path
	    Nothing	  -> return (Just "not found") }
 		-- Tried all our known library paths, but dlopen()
		-- has no built-in paths for frameworks: give up
   where
     mk_fwk dir = dir ++ '/' : rootname ++ ".framework/" ++ rootname
	-- sorry for the hardcoded paths, I hope they won't change anytime soon:
     defaultFrameworkPaths = ["/Library/Frameworks", "/System/Library/Frameworks"]
#endif
\end{code}

%************************************************************************
%*									*
		Helper functions
%*									*
%************************************************************************

\begin{code}
findFile :: (FilePath -> FilePath)	-- Maps a directory path to a file path
	 -> [FilePath]			-- Directories to look in
	 -> IO (Maybe FilePath)		-- The first file path to match
findFile mk_file_path [] 
  = return Nothing
findFile mk_file_path (dir:dirs)
  = do	{ let file_path = mk_file_path dir
	; b <- doesFileExist file_path
	; if b then 
	     return (Just file_path)
	  else
	     findFile mk_file_path dirs }
\end{code}

\begin{code}
maybePutStr dflags s | verbosity dflags > 0 = putStr s
		     | otherwise	    = return ()

maybePutStrLn dflags s | verbosity dflags > 0 = putStrLn s
		       | otherwise	      = return ()
\end{code}
