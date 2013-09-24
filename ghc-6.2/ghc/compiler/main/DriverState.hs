-----------------------------------------------------------------------------
-- $Id: DriverState.hs,v 1.96.2.1 2003/11/19 10:02:08 simonmar Exp $
--
-- Settings for the driver
--
-- (c) The University of Glasgow 2002
--
-----------------------------------------------------------------------------

module DriverState where

#include "../includes/config.h"
#include "HsVersions.h"

import ParsePkgConf	( loadPackageConfig )
import SysTools		( getTopDir )
import Packages
import CmdLineOpts
import DriverPhases
import DriverUtil
import UniqFM		( eltsUFM )
import Util
import Config
import Panic

import DATA_IOREF	( IORef, readIORef, writeIORef )
import EXCEPTION

import List
import Char  
import Monad
import Maybe		( fromJust, isJust )
import Directory	( doesDirectoryExist )

-----------------------------------------------------------------------------
-- non-configured things

cHaskell1Version = "5" -- i.e., Haskell 98

-----------------------------------------------------------------------------
-- GHC modes of operation

data GhcMode
  = DoMkDependHS			-- ghc -M
  | DoMkDLL				-- ghc --mk-dll
  | StopBefore Phase			-- ghc -E | -C | -S | -c
  | DoMake				-- ghc --make
  | DoInteractive			-- ghc --interactive
  | DoLink				-- [ the default ]
  | DoEval String			-- ghc -e
  deriving (Eq,Show)

GLOBAL_VAR(v_GhcMode,     DoLink, GhcMode)
GLOBAL_VAR(v_GhcModeFlag, "",     String)

setMode :: GhcMode -> String -> IO ()
setMode m flag = do
  old_mode <- readIORef v_GhcMode
  old_flag <- readIORef v_GhcModeFlag
  when (notNull old_flag && flag /= old_flag) $
      throwDyn (UsageError 
          ("cannot use `" ++ old_flag ++ "' with `" ++ flag ++ "'"))
  writeIORef v_GhcMode m
  writeIORef v_GhcModeFlag flag

isCompManagerMode DoMake        = True
isCompManagerMode DoInteractive = True
isCompManagerMode _             = False

-----------------------------------------------------------------------------
-- Global compilation flags

-- Cpp-related flags
v_Hs_source_cpp_opts = global
	[ "-D__HASKELL1__="++cHaskell1Version
	, "-D__GLASGOW_HASKELL__="++cProjectVersionInt				
	, "-D__HASKELL98__"
	, "-D__CONCURRENT_HASKELL__"
	]
{-# NOINLINE v_Hs_source_cpp_opts #-}


-- Keep output from intermediate phases
GLOBAL_VAR(v_Keep_hi_diffs, 		False, 		Bool)
GLOBAL_VAR(v_Keep_hc_files,		False,		Bool)
GLOBAL_VAR(v_Keep_s_files,		False,		Bool)
GLOBAL_VAR(v_Keep_raw_s_files,		False,		Bool)
GLOBAL_VAR(v_Keep_tmp_files, 		False, 		Bool)
#ifdef ILX
GLOBAL_VAR(v_Keep_il_files,		False,		Bool)
GLOBAL_VAR(v_Keep_ilx_files,		False,		Bool)
#endif

-- Misc
GLOBAL_VAR(v_Scale_sizes_by,    	1.0,		Double)
GLOBAL_VAR(v_Static, 			True,		Bool)
GLOBAL_VAR(v_NoLink, 			False,		Bool)
GLOBAL_VAR(v_NoHsMain, 			False, 		Bool)
GLOBAL_VAR(v_MainModIs,			Nothing,	Maybe String)
GLOBAL_VAR(v_MainFunIs,			Nothing,	Maybe String)
GLOBAL_VAR(v_Recomp,  			True,		Bool)
GLOBAL_VAR(v_Collect_ghc_timing, 	False,		Bool)
GLOBAL_VAR(v_Do_asm_mangling,		True,		Bool)
GLOBAL_VAR(v_Excess_precision,		False,		Bool)
GLOBAL_VAR(v_Read_DotGHCi,		True,		Bool)

-- Preprocessor flags
GLOBAL_VAR(v_Hs_source_pp_opts, [], [String])

-----------------------------------------------------------------------------
-- Splitting object files (for libraries)

GLOBAL_VAR(v_Split_object_files,	False,		Bool)
GLOBAL_VAR(v_Split_info,		("",0),		(String,Int))
	-- The split prefix and number of files

	
can_split :: Bool
can_split =  prefixMatch "i386"    cTARGETPLATFORM
	  || prefixMatch "alpha"   cTARGETPLATFORM
	  || prefixMatch "hppa"    cTARGETPLATFORM
	  || prefixMatch "m68k"    cTARGETPLATFORM
	  || prefixMatch "mips"    cTARGETPLATFORM
	  || prefixMatch "powerpc" cTARGETPLATFORM
	  || prefixMatch "rs6000"  cTARGETPLATFORM
	  || prefixMatch "sparc"   cTARGETPLATFORM

-----------------------------------------------------------------------------
-- Compiler output options

GLOBAL_VAR(v_Output_dir,  Nothing, Maybe String)
GLOBAL_VAR(v_Output_file, Nothing, Maybe String)
GLOBAL_VAR(v_Output_hi,   Nothing, Maybe String)

-- called to verify that the output files & directories
-- point somewhere valid. 
--
-- The assumption is that the directory portion of these output
-- options will have to exist by the time 'verifyOutputFiles'
-- is invoked.
-- 
verifyOutputFiles :: IO ()
verifyOutputFiles = do
  odir <- readIORef v_Output_dir
  when (isJust odir) $ do
     let dir = fromJust odir
     flg <- doesDirectoryExist dir
     when (not flg) (nonExistentDir "-odir" dir)
  ofile <- readIORef v_Output_file
  when (isJust ofile) $ do
     let fn = fromJust ofile
     flg <- doesDirNameExist fn
     when (not flg) (nonExistentDir "-o" fn)
  ohi <- readIORef v_Output_hi
  when (isJust ohi) $ do
     let hi = fromJust ohi
     flg <- doesDirNameExist hi
     when (not flg) (nonExistentDir "-ohi" hi)
 where
   nonExistentDir flg dir = 
     throwDyn (CmdLineError ("error: directory portion of " ++ 
                             show dir ++ " does not exist (used with " ++ 
			     show flg ++ " option.)"))

GLOBAL_VAR(v_Object_suf,  phaseInputExt Ln, String)
GLOBAL_VAR(v_HC_suf,  	  Nothing, Maybe String)
GLOBAL_VAR(v_Hi_dir,      Nothing, Maybe String)
GLOBAL_VAR(v_Hi_suf,      "hi",	   String)

GLOBAL_VAR(v_Ld_inputs,	[],      [String])

odir_ify :: String -> IO String
odir_ify f = do
  odir_opt <- readIORef v_Output_dir
  case odir_opt of
	Nothing -> return f
	Just d  -> return (replaceFilenameDirectory f d)

osuf_ify :: String -> IO String
osuf_ify f = do
  osuf <- readIORef v_Object_suf
  return (replaceFilenameSuffix f osuf)

GLOBAL_VAR(v_StgStats,                  False, Bool)

buildStgToDo :: IO [ StgToDo ]
buildStgToDo = do
  stg_stats <- readIORef v_StgStats
  let flags1 | stg_stats = [ D_stg_stats ]
	     | otherwise = [ ]

	-- STG passes
  ways_ <- readIORef v_Ways
  let flags2 | WayProf `elem` ways_ = StgDoMassageForProfiling : flags1
	     | otherwise            = flags1

  return flags2

-----------------------------------------------------------------------------
-- Paths & Libraries

split_marker = ':'   -- not configurable (ToDo)

v_Import_paths, v_Include_paths, v_Library_paths :: IORef [String]
GLOBAL_VAR(v_Import_paths,  ["."], [String])
GLOBAL_VAR(v_Include_paths, ["."], [String])
GLOBAL_VAR(v_Library_paths, [],	 [String])

#ifdef darwin_TARGET_OS
GLOBAL_VAR(v_Framework_paths, [], [String])
GLOBAL_VAR(v_Cmdline_frameworks, [], [String])
#endif

addToDirList :: IORef [String] -> String -> IO ()
addToDirList ref path
  = do paths           <- readIORef ref
       shiny_new_ones  <- splitUp path
       writeIORef ref (paths ++ filter notNull shiny_new_ones)
		-- empty paths are ignored: there might be a trailing
		-- ':' in the initial list, for example.  Empty paths can
		-- cause confusion when they are translated into -I options
		-- for passing to gcc.
  where
    splitUp ::String -> IO [String]
#ifdef mingw32_TARGET_OS
     -- 'hybrid' support for DOS-style paths in directory lists.
     -- 
     -- That is, if "foo:bar:baz" is used, this interpreted as
     -- consisting of three entries, 'foo', 'bar', 'baz'.
     -- However, with "c:/foo:c:\\foo;x:/bar", this is interpreted
     -- as four elts, "c:/foo", "c:\\foo", "x", and "/bar" --
     -- *provided* c:/foo exists and x:/bar doesn't.
     --
     -- Notice that no attempt is made to fully replace the 'standard'
     -- split marker ':' with the Windows / DOS one, ';'. The reason being
     -- that this will cause too much breakage for users & ':' will
     -- work fine even with DOS paths, if you're not insisting on being silly.
     -- So, use either.
    splitUp []         = return []
    splitUp (x:':':div:xs) 
      | div `elem` dir_markers = do
          let (p,rs) = findNextPath xs
          ps  <- splitUp rs
           {-
             Consult the file system to check the interpretation
             of (x:':':div:p) -- this is arguably excessive, we
             could skip this test & just say that it is a valid
             dir path.
           -}
          flg <- doesDirectoryExist (x:':':div:p)
          if flg then
             return ((x:':':div:p):ps)
           else
             return ([x]:(div:p):ps)
    splitUp xs = do
      let (p,rs) = findNextPath xs
      ps <- splitUp rs
      return (cons p ps)
    
    cons "" xs = xs
    cons x  xs = x:xs

    -- will be called either when we've consumed nought or the "<Drive>:/" part of
    -- a DOS path, so splitting is just a Q of finding the next split marker.
    findNextPath xs = 
        case break (`elem` split_markers) xs of
	   (p, d:ds) -> (p, ds)
	   (p, xs)   -> (p, xs)

    split_markers :: [Char]
    split_markers = [':', ';']

    dir_markers :: [Char]
    dir_markers = ['/', '\\']

#else
    splitUp xs = return (split split_marker xs)
#endif

-- ----------------------------------------------------------------------------
-- Loading the package config file

readPackageConf :: String -> IO ()
readPackageConf conf_file = do
  proto_pkg_configs <- loadPackageConfig conf_file
  top_dir 	    <- getTopDir
  let pkg_configs = mungePackagePaths top_dir proto_pkg_configs
  extendPackageConfigMap pkg_configs

mungePackagePaths :: String -> [PackageConfig] -> [PackageConfig]
-- Replace the string "$libdir" at the beginning of a path
-- with the current libdir (obtained from the -B option).
mungePackagePaths top_dir ps = map munge_pkg ps
 where 
  munge_pkg p = p{ import_dirs  = munge_paths (import_dirs p),
		   include_dirs = munge_paths (include_dirs p),
    		   library_dirs = munge_paths (library_dirs p),
		   framework_dirs = munge_paths (framework_dirs p) }

  munge_paths = map munge_path

  munge_path p 
	  | Just p' <- maybePrefixMatch "$libdir" p = top_dir ++ p'
	  | otherwise				    = p


-- -----------------------------------------------------------------------------
-- The list of packages requested on the command line

-- The package list reflects what packages were given as command-line options,
-- plus their dependent packages.  It is maintained in dependency order;
-- earlier packages may depend on later ones, but not vice versa
GLOBAL_VAR(v_ExplicitPackages, initPackageList, [PackageName])

initPackageList = [basePackage, rtsPackage]
	-- basePackage is part of this list entirely because of 
	-- wired-in names in GHCi.  See the notes on wired-in names in
	-- Linker.linkExpr.  By putting the base backage in initPackageList
	-- we make sure that it'll always by linked.


-- add a package requested from the command-line
addPackage :: String -> IO ()
addPackage package = do
  pkg_details <- getPackageConfigMap
  ps  <- readIORef v_ExplicitPackages
  ps' <- add_package pkg_details ps (mkPackageName package)
		-- Throws an exception if it fails
  writeIORef v_ExplicitPackages ps'

-- internal helper
add_package :: PackageConfigMap -> [PackageName]
	    -> PackageName -> IO [PackageName]
add_package pkg_details ps p	
  | p `elem` ps	-- Check if we've already added this package
  = return ps
  | Just details <- lookupPkg pkg_details p
  -- Add the package's dependents also
  = do ps' <- foldM (add_package pkg_details) ps (packageDependents details)
       return (p : ps')
  | otherwise
  = throwDyn (CmdLineError ("unknown package name: " ++ packageNameString p))


-- -----------------------------------------------------------------------------
-- Extracting information from the packages in scope

-- Many of these functions take a list of packages: in those cases,
-- the list is expected to contain the "dependent packages",
-- i.e. those packages that were found to be depended on by the
-- current module/program.  These can be auto or non-auto packages, it
-- doesn't really matter.  The list is always combined with the list
-- of explicit (command-line) packages to determine which packages to
-- use.

getPackageImportPath :: IO [String]
getPackageImportPath = do
  ps <- getExplicitAndAutoPackageConfigs
		  -- import dirs are always derived from the 'auto' 
		  -- packages as well as the explicit ones
  return (nub (filter notNull (concatMap import_dirs ps)))

getPackageIncludePath :: [PackageName] -> IO [String]
getPackageIncludePath pkgs = do
  ps <- getExplicitPackagesAnd pkgs
  return (nub (filter notNull (concatMap include_dirs ps)))

	-- includes are in reverse dependency order (i.e. rts first)
getPackageCIncludes :: [PackageConfig] -> IO [String]
getPackageCIncludes pkg_configs = do
  return (reverse (nub (filter notNull (concatMap c_includes pkg_configs))))

getPackageLibraryPath :: [PackageName] -> IO [String]
getPackageLibraryPath pkgs = do 
  ps <- getExplicitPackagesAnd pkgs
  return (nub (filter notNull (concatMap library_dirs ps)))

getPackageLinkOpts :: [PackageName] -> IO [String]
getPackageLinkOpts pkgs = do
  ps <- getExplicitPackagesAnd pkgs
  tag <- readIORef v_Build_tag
  static <- readIORef v_Static
  let 
	imp        = if static then "" else "_imp"
	suffix     = if null tag then "" else '_':tag
      	libs p     = map (++suffix) (hACK (hs_libraries p)) ++ extra_libraries p
	imp_libs p = map (++imp) (libs p)
	all_opts p = map ("-l" ++) (imp_libs p) ++ extra_ld_opts p

  return (concat (map all_opts ps))
  where
     -- This is a totally horrible (temporary) hack, for Win32.  Problem is
     -- that package.conf for Win32 says that the main prelude lib is 
     -- split into HSbase1, HSbase2 and HSbase3, which is needed due to a bug
     -- in the GNU linker (PEi386 backend). However, we still only
     -- have HSbase.a for static linking, not HSbase{1,2,3}.a
     -- getPackageLibraries is called to find the .a's to add to the static
     -- link line.  On Win32, this hACK detects HSbase{1,2,3} and 
     -- replaces them with HSbase, so static linking still works.
     -- Libraries needed for dynamic (GHCi) linking are discovered via
     -- different route (in InteractiveUI.linkPackage).
     -- See driver/PackageSrc.hs for the HSbase1/HSbase2 split definition.
     -- THIS IS A STRICTLY TEMPORARY HACK (famous last words ...)
     -- JRS 04 Sept 01: Same appalling hack for HSwin32[1,2]
     -- KAA 29 Mar  02: Same appalling hack for HSobjectio[1,2,3,4]
     hACK libs
#      if !defined(mingw32_TARGET_OS) && !defined(cygwin32_TARGET_OS)
       = libs
#      else
       = if   "HSbase1" `elem` libs && "HSbase2" `elem` libs && "HSbase3" `elem` libs
         then "HSbase" : filter (not.(isPrefixOf "HSbase")) libs
         else
         if   "HSwin321" `elem` libs && "HSwin322" `elem` libs
         then "HSwin32" : filter (not.(isPrefixOf "HSwin32")) libs
         else 
         if   "HSobjectio1" `elem` libs && "HSobjectio2" `elem` libs && "HSobjectio3" `elem` libs && "HSobjectio4" `elem` libs
	 then "HSobjectio" : filter (not.(isPrefixOf "HSobjectio")) libs
         else 
         libs
#      endif

getPackageExtraGhcOpts :: IO [String]
getPackageExtraGhcOpts = do
  ps <- getExplicitAndAutoPackageConfigs
  return (concatMap extra_ghc_opts ps)

getPackageExtraCcOpts :: [PackageName] -> IO [String]
getPackageExtraCcOpts pkgs = do
  ps <- getExplicitPackagesAnd pkgs
  return (concatMap extra_cc_opts ps)

#ifdef darwin_TARGET_OS
getPackageFrameworkPath  :: [PackageName] -> IO [String]
getPackageFrameworkPath pkgs = do
  ps <- getExplicitPackagesAnd pkgs
  return (nub (filter notNull (concatMap framework_dirs ps)))

getPackageFrameworks  :: [PackageName] -> IO [String]
getPackageFrameworks pkgs = do
  ps <- getExplicitPackagesAnd pkgs
  return (concatMap extra_frameworks ps)
#endif

-- -----------------------------------------------------------------------------
-- Package Utils

getExplicitPackagesAnd :: [PackageName] -> IO [PackageConfig]
getExplicitPackagesAnd pkg_names = do
  pkg_map <- getPackageConfigMap
  expl <- readIORef v_ExplicitPackages
  all_pkgs <- foldM (add_package pkg_map) expl pkg_names
  getPackageDetails all_pkgs

-- return all packages, including both the auto packages and the explicit ones
getExplicitAndAutoPackageConfigs :: IO [PackageConfig]
getExplicitAndAutoPackageConfigs = do
  pkg_map <- getPackageConfigMap
  let auto_packages = [ mkPackageName (name p) | p <- eltsUFM pkg_map, auto p ]
  getExplicitPackagesAnd auto_packages

-----------------------------------------------------------------------------
-- Ways

-- The central concept of a "way" is that all objects in a given
-- program must be compiled in the same "way".  Certain options change
-- parameters of the virtual machine, eg. profiling adds an extra word
-- to the object header, so profiling objects cannot be linked with
-- non-profiling objects.

-- After parsing the command-line options, we determine which "way" we
-- are building - this might be a combination way, eg. profiling+ticky-ticky.

-- We then find the "build-tag" associated with this way, and this
-- becomes the suffix used to find .hi files and libraries used in
-- this compilation.

GLOBAL_VAR(v_Build_tag, "", String)

data WayName
  = WayProf
  | WayUnreg
  | WayTicky
  | WayPar
  | WayGran
  | WaySMP
  | WayNDP
  | WayDebug
  | WayUser_a
  | WayUser_b
  | WayUser_c
  | WayUser_d
  | WayUser_e
  | WayUser_f
  | WayUser_g
  | WayUser_h
  | WayUser_i
  | WayUser_j
  | WayUser_k
  | WayUser_l
  | WayUser_m
  | WayUser_n
  | WayUser_o
  | WayUser_A
  | WayUser_B
  deriving (Eq,Ord)

GLOBAL_VAR(v_Ways, [] ,[WayName])

allowed_combination way = way `elem` combs
  where  -- the sub-lists must be ordered according to WayName, 
         -- because findBuildTag sorts them
    combs                = [ [WayProf, WayUnreg], 
			     [WayProf, WaySMP]  ,
			     [WayProf, WayNDP]  ]

findBuildTag :: IO [String]  -- new options
findBuildTag = do
  way_names <- readIORef v_Ways
  case sort way_names of
     []  -> do  -- writeIORef v_Build_tag ""
	        return []

     [w] -> do let details = lkupWay w
	       writeIORef v_Build_tag (wayTag details)
	       return (wayOpts details)

     ws  -> if not (allowed_combination ws)
		then throwDyn (CmdLineError $
				"combination not supported: "  ++
   				foldr1 (\a b -> a ++ '/':b) 
				(map (wayName . lkupWay) ws))
		else let stuff = map lkupWay ws
			 tag   = concat (map wayTag stuff)
			 flags = map wayOpts stuff
		     in do
		     writeIORef v_Build_tag tag
		     return (concat flags)

lkupWay w = 
   case lookup w way_details of
	Nothing -> error "findBuildTag"
	Just details -> details

data Way = Way {
  wayTag   :: String,
  wayName  :: String,
  wayOpts  :: [String]
  }

way_details :: [ (WayName, Way) ]
way_details =
  [ (WayProf, Way  "p" "Profiling"  
	[ "-fscc-profiling"
	, "-DPROFILING"
	, "-optc-DPROFILING"
	, "-fvia-C" ]),

    (WayTicky, Way  "t" "Ticky-ticky Profiling"  
	[ "-fticky-ticky"
	, "-DTICKY_TICKY"
	, "-optc-DTICKY_TICKY"
	, "-fvia-C" ]),

    (WayUnreg, Way  "u" "Unregisterised" 
	unregFlags ),

    -- optl's below to tell linker where to find the PVM library -- HWL
    (WayPar, Way  "mp" "Parallel" 
	[ "-fparallel"
	, "-D__PARALLEL_HASKELL__"
	, "-optc-DPAR"
	, "-package concurrent"
        , "-optc-w"
        , "-optl-L${PVM_ROOT}/lib/${PVM_ARCH}"
        , "-optl-lpvm3"
        , "-optl-lgpvm3"
	, "-fvia-C" ]),

    -- at the moment we only change the RTS and could share compiler and libs!
    (WayPar, Way  "mt" "Parallel ticky profiling" 
	[ "-fparallel"
	, "-D__PARALLEL_HASKELL__"
	, "-optc-DPAR"
	, "-optc-DPAR_TICKY"
	, "-package concurrent"
        , "-optc-w"
        , "-optl-L${PVM_ROOT}/lib/${PVM_ARCH}"
        , "-optl-lpvm3"
        , "-optl-lgpvm3"
	, "-fvia-C" ]),

    (WayPar, Way  "md" "Distributed" 
	[ "-fparallel"
	, "-D__PARALLEL_HASKELL__"
	, "-D__DISTRIBUTED_HASKELL__"
	, "-optc-DPAR"
	, "-optc-DDIST"
	, "-package concurrent"
        , "-optc-w"
        , "-optl-L${PVM_ROOT}/lib/${PVM_ARCH}"
        , "-optl-lpvm3"
        , "-optl-lgpvm3"
	, "-fvia-C" ]),

    (WayGran, Way  "mg" "GranSim" 
	[ "-fgransim"
	, "-D__GRANSIM__"
	, "-optc-DGRAN"
	, "-package concurrent"
	, "-fvia-C" ]),

    (WaySMP, Way  "s" "SMP"
	[ "-fsmp"
	, "-optc-pthread"
#ifndef freebsd_TARGET_OS
	, "-optl-pthread"
#endif
	, "-optc-DSMP"
	, "-fvia-C" ]),

    (WayNDP, Way  "ndp" "Nested data parallelism"
	[ "-fparr"
	, "-fflatten"]),

    (WayUser_a,  Way  "a"  "User way 'a'"  ["$WAY_a_REAL_OPTS"]),	
    (WayUser_b,  Way  "b"  "User way 'b'"  ["$WAY_b_REAL_OPTS"]),	
    (WayUser_c,  Way  "c"  "User way 'c'"  ["$WAY_c_REAL_OPTS"]),	
    (WayUser_d,  Way  "d"  "User way 'd'"  ["$WAY_d_REAL_OPTS"]),	
    (WayUser_e,  Way  "e"  "User way 'e'"  ["$WAY_e_REAL_OPTS"]),	
    (WayUser_f,  Way  "f"  "User way 'f'"  ["$WAY_f_REAL_OPTS"]),	
    (WayUser_g,  Way  "g"  "User way 'g'"  ["$WAY_g_REAL_OPTS"]),	
    (WayUser_h,  Way  "h"  "User way 'h'"  ["$WAY_h_REAL_OPTS"]),	
    (WayUser_i,  Way  "i"  "User way 'i'"  ["$WAY_i_REAL_OPTS"]),	
    (WayUser_j,  Way  "j"  "User way 'j'"  ["$WAY_j_REAL_OPTS"]),	
    (WayUser_k,  Way  "k"  "User way 'k'"  ["$WAY_k_REAL_OPTS"]),	
    (WayUser_l,  Way  "l"  "User way 'l'"  ["$WAY_l_REAL_OPTS"]),	
    (WayUser_m,  Way  "m"  "User way 'm'"  ["$WAY_m_REAL_OPTS"]),	
    (WayUser_n,  Way  "n"  "User way 'n'"  ["$WAY_n_REAL_OPTS"]),	
    (WayUser_o,  Way  "o"  "User way 'o'"  ["$WAY_o_REAL_OPTS"]),	
    (WayUser_A,  Way  "A"  "User way 'A'"  ["$WAY_A_REAL_OPTS"]),	
    (WayUser_B,  Way  "B"  "User way 'B'"  ["$WAY_B_REAL_OPTS"]) 
  ]

unregFlags = 
   [ "-optc-DNO_REGS"
   , "-optc-DUSE_MINIINTERPRETER"
   , "-fno-asm-mangling"
   , "-funregisterised"
   , "-fvia-C" ]

-----------------------------------------------------------------------------
-- Options for particular phases

GLOBAL_VAR(v_Opt_dep,    [], [String])
GLOBAL_VAR(v_Anti_opt_C, [], [String])
GLOBAL_VAR(v_Opt_C,      [], [String])
GLOBAL_VAR(v_Opt_l,      [], [String])
GLOBAL_VAR(v_Opt_dll,    [], [String])

getStaticOpts :: IORef [String] -> IO [String]
getStaticOpts ref = readIORef ref >>= return . reverse
