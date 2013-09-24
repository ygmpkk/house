{-# OPTIONS -#include "Linker.h" #-}
-----------------------------------------------------------------------------
-- $Id: InteractiveUI.hs,v 1.159.2.1 2003/11/19 10:01:20 simonmar Exp $
--
-- GHC Interactive User Interface
--
-- (c) The GHC Team 2000
--
-----------------------------------------------------------------------------
module InteractiveUI ( 
	interactiveUI,  -- :: CmState -> [FilePath] -> IO ()
	ghciWelcomeMsg
   ) where

#include "../includes/config.h"
#include "HsVersions.h"

import CompManager
import HscTypes		( TyThing(..), HomeModInfo(hm_linkable), HomePackageTable,
			  isObjectLinkable, GhciMode(..) )
import HsSyn		( TyClDecl(..), ConDecl(..), Sig(..) )
import MkIface		( ifaceTyThing )
import DriverFlags
import DriverState
import DriverUtil	( remove_spaces )
import Linker		( showLinkerState, linkPackages )
import Util
import IdInfo		( GlobalIdDetails(..) )
import Id		( isImplicitId, idName, globalIdDetails )
import Class		( className )
import TyCon		( tyConName, tyConClass_maybe, isPrimTyCon, DataConDetails(..) )
import DataCon		( dataConName )
import FieldLabel	( fieldLabelTyCon )
import SrcLoc		( isGoodSrcLoc )
import Module		( showModMsg, lookupModuleEnv )
import Name		( Name, isHomePackageName, nameSrcLoc, nameOccName,
			  NamedThing(..) )
import OccName		( isSymOcc )
import BasicTypes	( defaultFixity, SuccessFlag(..) )
import Packages
import Outputable
import CmdLineOpts	( DynFlag(..), DynFlags(..), getDynFlags, saveDynFlags,
			  restoreDynFlags, dopt_unset )
import Panic 		hiding ( showException )
import Config

#ifndef mingw32_HOST_OS
import DriverUtil( handle )
import System.Posix
#if __GLASGOW_HASKELL__ > 504
	hiding (getEnv)
#endif
#endif

#if HAVE_READLINE_HEADERS && HAVE_READLINE_LIBS
import Control.Concurrent	( yield )	-- Used in readline loop
import System.Console.Readline as Readline
#endif

--import SystemExts

import Control.Exception as Exception
import Data.Dynamic
import Control.Concurrent

import Numeric
import Data.List
import System.Cmd
import System.CPUTime
import System.Environment
import System.Directory
import System.IO as IO
import Data.Char
import Control.Monad as Monad

import GHC.Exts		( unsafeCoerce# )

import Data.IORef	( IORef, newIORef, readIORef, writeIORef )

import System.Posix.Internals ( setNonBlockingFD )

-----------------------------------------------------------------------------

ghciWelcomeMsg = "\ 
\   ___         ___ _\n\ 
\  / _ \\ /\\  /\\/ __(_)\n\ 
\ / /_\\// /_/ / /  | |      GHC Interactive, version " ++ cProjectVersion ++ ", for Haskell 98.\n\ 
\/ /_\\\\/ __  / /___| |      http://www.haskell.org/ghc/\n\ 
\\\____/\\/ /_/\\____/|_|      Type :? for help.\n"

GLOBAL_VAR(commands, builtin_commands, [(String, String -> GHCi Bool)])

builtin_commands :: [(String, String -> GHCi Bool)]
builtin_commands = [
  ("add",	keepGoingPaths addModule),
  ("browse",    keepGoing browseCmd),
  ("cd",    	keepGoing changeDirectory),
  ("def",	keepGoing defineMacro),
  ("help",	keepGoing help),
  ("?",		keepGoing help),
  ("info",      keepGoing info),
  ("load",	keepGoingPaths loadModule),
  ("module",	keepGoing setContext),
  ("reload",	keepGoing reloadModule),
  ("set",	keepGoing setCmd),
  ("show",	keepGoing showCmd),
  ("type",	keepGoing typeOfExpr),
  ("unset",	keepGoing unsetOptions),
  ("undef",     keepGoing undefineMacro),
  ("quit",	quit)
  ]

keepGoing :: (String -> GHCi ()) -> (String -> GHCi Bool)
keepGoing a str = a str >> return False

keepGoingPaths :: ([FilePath] -> GHCi ()) -> (String -> GHCi Bool)
keepGoingPaths a str = a (toArgs str) >> return False

shortHelpText = "use :? for help.\n"

-- NOTE: spaces at the end of each line to workaround CPP/string gap bug.
helpText = "\ 
\ Commands available from the prompt:\n\ 
\\n\ 
\   <stmt>		       evaluate/run <stmt>\n\ 
\   :add <filename> ...        add module(s) to the current target set\n\ 
\   :browse [*]<module>	       display the names defined by <module>\n\ 
\   :cd <dir>		       change directory to <dir>\n\ 
\   :def <cmd> <expr>          define a command :<cmd>\n\ 
\   :help, :?		       display this list of commands\n\ 
\   :info [<name> ...]         display information about the given names\n\ 
\   :load <filename> ...       load module(s) and their dependents\n\ 
\   :module [+/-] [*]<mod> ... set the context for expression evaluation\n\ 
\   :reload		       reload the current module set\n\ 
\\n\ 
\   :set <option> ...	       set options\n\ 
\   :set args <arg> ...	       set the arguments returned by System.getArgs\n\ 
\   :set prog <progname>       set the value returned by System.getProgName\n\ 
\\n\ 
\   :show modules	       show the currently loaded modules\n\ 
\   :show bindings	       show the current bindings made at the prompt\n\ 
\\n\ 
\   :type <expr>	       show the type of <expr>\n\ 
\   :undef <cmd> 	       undefine user-defined command :<cmd>\n\ 
\   :unset <option> ...	       unset options\n\ 
\   :quit		       exit GHCi\n\ 
\   :!<command>		       run the shell command <command>\n\ 
\\n\ 
\ Options for `:set' and `:unset':\n\ 
\\n\ 
\    +r			revert top-level expressions after each evaluation\n\ 
\    +s                 print timing/memory stats after each evaluation\n\ 
\    +t			print type after evaluation\n\ 
\    -<flags>		most GHC command line flags can also be set here\n\ 
\                         (eg. -v2, -fglasgow-exts, etc.)\n\ 
\"

interactiveUI :: [FilePath] -> Maybe String -> IO ()
interactiveUI srcs maybe_expr = do
   dflags <- getDynFlags

   cmstate <- cmInit Interactive;

   hFlush stdout
   hSetBuffering stdout NoBuffering

	-- Initialise buffering for the *interpreted* I/O system
   cmstate <- initInterpBuffering cmstate dflags

	-- We don't want the cmd line to buffer any input that might be
	-- intended for the program, so unbuffer stdin.
   hSetBuffering stdin NoBuffering

	-- initial context is just the Prelude
   cmstate <- cmSetContext cmstate dflags [] ["Prelude"]

#if HAVE_READLINE_HEADERS && HAVE_READLINE_LIBS
   Readline.initialize
#endif

   startGHCi (runGHCi srcs dflags maybe_expr)
	GHCiState{ progname = "<interactive>",
		   args = [],
		   targets = srcs,
		   cmstate = cmstate,
		   options = [] }

#if HAVE_READLINE_HEADERS && HAVE_READLINE_LIBS
   Readline.resetTerminal Nothing
#endif

   return ()

runGHCi :: [FilePath] -> DynFlags -> Maybe String -> GHCi ()
runGHCi paths dflags maybe_expr = do
  read_dot_files <- io (readIORef v_Read_DotGHCi)

  when (read_dot_files) $ do
    -- Read in ./.ghci.
    let file = "./.ghci"
    exists <- io (doesFileExist file)
    when exists $ do
       dir_ok  <- io (checkPerms ".")
       file_ok <- io (checkPerms file)
       when (dir_ok && file_ok) $ do
  	  either_hdl <- io (IO.try (openFile "./.ghci" ReadMode))
  	  case either_hdl of
  	     Left e    -> return ()
  	     Right hdl -> fileLoop hdl False
    
  when (read_dot_files) $ do
    -- Read in $HOME/.ghci
    either_dir <- io (IO.try (getEnv "HOME"))
    case either_dir of
       Left e -> return ()
       Right dir -> do
  	  cwd <- io (getCurrentDirectory)
  	  when (dir /= cwd) $ do
  	     let file = dir ++ "/.ghci"
  	     ok <- io (checkPerms file)
  	     when ok $ do
  	       either_hdl <- io (IO.try (openFile file ReadMode))
  	       case either_hdl of
  		  Left e    -> return ()
  		  Right hdl -> fileLoop hdl False

  -- Perform a :load for files given on the GHCi command line
  when (not (null paths)) $
     ghciHandle showException $
	loadModule paths

  -- if verbosity is greater than 0, or we are connected to a
  -- terminal, display the prompt in the interactive loop.
  is_tty <- io (hIsTerminalDevice stdin)
  let show_prompt = verbosity dflags > 0 || is_tty

  case maybe_expr of
	Nothing -> 
	    -- enter the interactive loop
	    interactiveLoop is_tty show_prompt
	Just expr -> do
	    -- just evaluate the expression we were given
	    runCommand expr
	    return ()

  -- and finally, exit
  io $ do when (verbosity dflags > 0) $ putStrLn "Leaving GHCi."


interactiveLoop is_tty show_prompt = do
  -- Ignore ^C exceptions caught here
  ghciHandleDyn (\e -> case e of 
			Interrupted -> ghciUnblock (interactiveLoop is_tty show_prompt)
			_other      -> return ()) $ do

  -- read commands from stdin
#if HAVE_READLINE_HEADERS && HAVE_READLINE_LIBS
  if (is_tty) 
	then readlineLoop
	else fileLoop stdin show_prompt
#else
  fileLoop stdin show_prompt
#endif


-- NOTE: We only read .ghci files if they are owned by the current user,
-- and aren't world writable.  Otherwise, we could be accidentally 
-- running code planted by a malicious third party.

-- Furthermore, We only read ./.ghci if . is owned by the current user
-- and isn't writable by anyone else.  I think this is sufficient: we
-- don't need to check .. and ../.. etc. because "."  always refers to
-- the same directory while a process is running.

checkPerms :: String -> IO Bool
checkPerms name =
#ifdef mingw32_HOST_OS
  return True
#else
  DriverUtil.handle (\_ -> return False) $ do
     st <- getFileStatus name
     me <- getRealUserID
     if fileOwner st /= me then do
   	putStrLn $ "WARNING: " ++ name ++ " is owned by someone else, IGNORING!"
   	return False
      else do
   	let mode =  fileMode st
   	if (groupWriteMode == (mode `intersectFileModes` groupWriteMode))
   	   || (otherWriteMode == (mode `intersectFileModes` otherWriteMode)) 
   	   then do
   	       putStrLn $ "*** WARNING: " ++ name ++ 
   			  " is writable by someone else, IGNORING!"
   	       return False
   	  else return True
#endif

fileLoop :: Handle -> Bool -> GHCi ()
fileLoop hdl prompt = do
   cmstate <- getCmState
   (mod,imports) <- io (cmGetContext cmstate)
   when prompt (io (putStr (mkPrompt mod imports)))
   l <- io (IO.try (hGetLine hdl))
   case l of
	Left e | isEOFError e -> return ()
	       | otherwise    -> io (ioError e)
	Right l -> 
	  case remove_spaces l of
	    "" -> fileLoop hdl prompt
	    l  -> do quit <- runCommand l
          	     if quit then return () else fileLoop hdl prompt

stringLoop :: [String] -> GHCi ()
stringLoop [] = return ()
stringLoop (s:ss) = do
   case remove_spaces s of
	"" -> stringLoop ss
	l  -> do quit <- runCommand l
                 if quit then return () else stringLoop ss

mkPrompt toplevs exports
   = concat (intersperse " " (map ('*':) toplevs ++ exports)) ++ "> "

#if HAVE_READLINE_HEADERS && HAVE_READLINE_LIBS
readlineLoop :: GHCi ()
readlineLoop = do
   cmstate <- getCmState
   (mod,imports) <- io (cmGetContext cmstate)
   io yield
   l <- io (readline (mkPrompt mod imports)
	  	`finally` setNonBlockingFD 0)
		-- readline sometimes puts stdin into blocking mode,
		-- so we need to put it back for the IO library
   case l of
	Nothing -> return ()
	Just l  ->
	  case remove_spaces l of
	    "" -> readlineLoop
	    l  -> do
        	  io (addHistory l)
  	  	  quit <- runCommand l
          	  if quit then return () else readlineLoop
#endif

runCommand :: String -> GHCi Bool
runCommand c = ghciHandle handler (doCommand c)

-- This is the exception handler for exceptions generated by the
-- user's code; it normally just prints out the exception.  The
-- handler must be recursive, in case showing the exception causes
-- more exceptions to be raised.
--
-- Bugfix: if the user closed stdout or stderr, the flushing will fail,
-- raising another exception.  We therefore don't put the recursive
-- handler arond the flushing operation, so if stderr is closed
-- GHCi will just die gracefully rather than going into an infinite loop.
handler :: Exception -> GHCi Bool
handler exception = do
  flushInterpBuffers
  io installSignalHandlers
  ghciHandle handler (showException exception >> return False)

showException (DynException dyn) =
  case fromDynamic dyn of
    Nothing               -> io (putStrLn ("*** Exception: (unknown)"))
    Just Interrupted      -> io (putStrLn "Interrupted.")
    Just (CmdLineError s) -> io (putStrLn s)	 -- omit the location for CmdLineError
    Just ph@PhaseFailed{} -> io (putStrLn (showGhcException ph "")) -- ditto
    Just other_ghc_ex     -> io (print other_ghc_ex)

showException other_exception
  = io (putStrLn ("*** Exception: " ++ show other_exception))

doCommand (':' : command) = specialCommand command
doCommand stmt
   = do timeIt (do nms <- runStmt stmt; finishEvalExpr nms)
        return False

runStmt :: String -> GHCi [Name]
runStmt stmt
 | null (filter (not.isSpace) stmt) = return []
 | otherwise
 = do st <- getGHCiState
      dflags <- io getDynFlags
      let dflags' = dopt_unset dflags Opt_WarnUnusedBinds
      (new_cmstate, result) <- 
	io $ withProgName (progname st) $ withArgs (args st) $
	cmRunStmt (cmstate st) dflags' stmt
      setGHCiState st{cmstate = new_cmstate}
      case result of
	CmRunFailed      -> return []
	CmRunException e -> showException e >> return []
	CmRunOk names    -> return names

-- possibly print the type and revert CAFs after evaluating an expression
finishEvalExpr names
 = do b <- isOptionSet ShowType
      cmstate <- getCmState
      when b (mapM_ (showTypeOfName cmstate) names)

      flushInterpBuffers
      io installSignalHandlers
      b <- isOptionSet RevertCAFs
      io (when b revertCAFs)
      return True

showTypeOfName :: CmState -> Name -> GHCi ()
showTypeOfName cmstate n
   = do maybe_str <- io (cmTypeOfName cmstate n)
	case maybe_str of
	  Nothing  -> return ()
	  Just str -> io (putStrLn (showSDoc (ppr n) ++ " :: " ++ str))

specialCommand :: String -> GHCi Bool
specialCommand ('!':str) = shellEscape (dropWhile isSpace str)
specialCommand str = do
  let (cmd,rest) = break isSpace str
  cmds <- io (readIORef commands)
  case [ (s,f) | (s,f) <- cmds, prefixMatch cmd s ] of
     []      -> io (hPutStr stdout ("unknown command `:" ++ cmd ++ "'\n" 
		                    ++ shortHelpText) >> return False)
     [(_,f)] -> f (dropWhile isSpace rest)
     cs      -> io (hPutStrLn stdout ("prefix " ++ cmd ++ 
			    	      " matches multiple commands (" ++ 
	         	     	       foldr1 (\a b -> a ++ ',':b) (map fst cs)
					 ++ ")") >> return False)

noArgs c = throwDyn (CmdLineError ("command `" ++ c ++ "' takes no arguments"))


-----------------------------------------------------------------------------
-- To flush buffers for the *interpreted* computation we need
-- to refer to *its* stdout/stderr handles

GLOBAL_VAR(flush_interp,       error "no flush_interp", IO ())
GLOBAL_VAR(turn_off_buffering, error "no flush_stdout", IO ())

no_buf_cmd = "IO.hSetBuffering IO.stdout IO.NoBuffering" ++
	     " Prelude.>> IO.hSetBuffering IO.stderr IO.NoBuffering"
flush_cmd  = "IO.hFlush IO.stdout Prelude.>> IO.hFlush IO.stderr"

initInterpBuffering :: CmState -> DynFlags -> IO CmState
initInterpBuffering cmstate dflags
 = do (cmstate, maybe_hval) <- cmCompileExpr cmstate dflags no_buf_cmd
	
      case maybe_hval of
	Just hval -> writeIORef turn_off_buffering (unsafeCoerce# hval :: IO ())
	other	  -> panic "interactiveUI:setBuffering"
	
      (cmstate, maybe_hval) <- cmCompileExpr cmstate dflags flush_cmd
      case maybe_hval of
	Just hval -> writeIORef flush_interp (unsafeCoerce# hval :: IO ())
	_         -> panic "interactiveUI:flush"

      turnOffBuffering	-- Turn it off right now

      return cmstate


flushInterpBuffers :: GHCi ()
flushInterpBuffers
 = io $ do Monad.join (readIORef flush_interp)
           return ()

turnOffBuffering :: IO ()
turnOffBuffering
 = do Monad.join (readIORef turn_off_buffering)
      return ()

-----------------------------------------------------------------------------
-- Commands

help :: String -> GHCi ()
help _ = io (putStr helpText)

info :: String -> GHCi ()
info "" = throwDyn (CmdLineError "syntax: `:i <thing-you-want-info-about>'")
info s = do
  let names = words s
  init_cms <- getCmState
  dflags <- io getDynFlags
  let 
    infoThings cms [] = return cms
    infoThings cms (name:names) = do
      (cms, stuff) <- io (cmInfoThing cms dflags name)
      io (putStrLn (showSDocForUser unqual (
	    vcat (intersperse (text "") (map showThing stuff))))
         )
      infoThings cms names

    unqual = cmGetPrintUnqual init_cms

    showThing (ty_thing, fixity) 
	= vcat [ text "-- " <> showTyThing ty_thing, 
		 showFixity fixity (getName ty_thing),
	         ppr (ifaceTyThing True{-omit prags-} ty_thing) ]

    showFixity fix name
	| fix == defaultFixity = empty
	| otherwise            = ppr fix <+> 
				 (if isSymOcc (nameOccName name)
				  then ppr name
				  else char '`' <> ppr name <> char '`')

    showTyThing (AClass cl)
       = hcat [ppr cl, text " is a class", showSrcLoc (className cl)]
    showTyThing (ADataCon dc)
       = hcat [ppr dc, text " is a data constructor", showSrcLoc (dataConName dc)]
    showTyThing (ATyCon ty)
       | isPrimTyCon ty
       = hcat [ppr ty, text " is a primitive type constructor"]
       | otherwise
       = hcat [ppr ty, text " is a type constructor", showSrcLoc (tyConName ty)]
    showTyThing (AnId   id)
       = hcat [ppr id, text " is a ", idDescr id, showSrcLoc (idName id)]

    idDescr id
       = case globalIdDetails id of
	    RecordSelId lbl -> text "record selector for type" <+> ppr (fieldLabelTyCon lbl)
	    ClassOpId cls   -> text "method in class" <+> ppr cls
       	    otherwise       -> text "variable"

	-- also print out the source location for home things
    showSrcLoc name
	| isHomePackageName name && isGoodSrcLoc loc
	= hsep [ text ", defined at", ppr loc ]
	| otherwise
	= empty
	where loc = nameSrcLoc name

  cms <- infoThings init_cms names
  setCmState cms
  return ()

addModule :: [FilePath] -> GHCi ()
addModule files = do
  state <- getGHCiState
  dflags <- io (getDynFlags)
  io (revertCAFs)			-- always revert CAFs on load/add.
  files <- mapM expandPath files
  let new_targets = files ++ targets state 
  graph <- io (cmDepAnal (cmstate state) dflags new_targets)
  (cmstate1, ok, mods) <- io (cmLoadModules (cmstate state) dflags graph)
  setGHCiState state{ cmstate = cmstate1, targets = new_targets }
  setContextAfterLoad mods
  modulesLoadedMsg ok mods dflags

changeDirectory :: String -> GHCi ()
changeDirectory dir = do
  state    <- getGHCiState
  when (targets state /= []) $
	io $ putStr "Warning: changing directory causes all loaded modules to be unloaded, \n\ 
	\because the search path has changed.\n"
  dflags   <- io getDynFlags
  cmstate1 <- io (cmUnload (cmstate state) dflags)
  setGHCiState state{ cmstate = cmstate1, targets = [] }
  setContextAfterLoad []
  dir <- expandPath dir
  io (setCurrentDirectory dir)

defineMacro :: String -> GHCi ()
defineMacro s = do
  let (macro_name, definition) = break isSpace s
  cmds <- io (readIORef commands)
  if (null macro_name) 
	then throwDyn (CmdLineError "invalid macro name") 
	else do
  if (macro_name `elem` map fst cmds) 
	then throwDyn (CmdLineError 
		("command `" ++ macro_name ++ "' is already defined"))
	else do

  -- give the expression a type signature, so we can be sure we're getting
  -- something of the right type.
  let new_expr = '(' : definition ++ ") :: String -> IO String"

  -- compile the expression
  cms <- getCmState
  dflags <- io getDynFlags
  (new_cmstate, maybe_hv) <- io (cmCompileExpr cms dflags new_expr)
  setCmState new_cmstate
  case maybe_hv of
     Nothing -> return ()
     Just hv -> io (writeIORef commands --
		    ((macro_name, keepGoing (runMacro hv)) : cmds))

runMacro :: HValue{-String -> IO String-} -> String -> GHCi ()
runMacro fun s = do
  str <- io ((unsafeCoerce# fun :: String -> IO String) s)
  stringLoop (lines str)

undefineMacro :: String -> GHCi ()
undefineMacro macro_name = do
  cmds <- io (readIORef commands)
  if (macro_name `elem` map fst builtin_commands) 
	then throwDyn (CmdLineError
		("command `" ++ macro_name ++ "' cannot be undefined"))
	else do
  if (macro_name `notElem` map fst cmds) 
	then throwDyn (CmdLineError 
		("command `" ++ macro_name ++ "' not defined"))
	else do
  io (writeIORef commands (filter ((/= macro_name) . fst) cmds))


loadModule :: [FilePath] -> GHCi ()
loadModule fs = timeIt (loadModule' fs)

loadModule' :: [FilePath] -> GHCi ()
loadModule' files = do
  state <- getGHCiState
  dflags <- io getDynFlags

  -- expand tildes
  files <- mapM expandPath files

  -- do the dependency anal first, so that if it fails we don't throw
  -- away the current set of modules.
  graph <- io (cmDepAnal (cmstate state) dflags files)

  -- Dependency anal ok, now unload everything
  cmstate1 <- io (cmUnload (cmstate state) dflags)
  setGHCiState state{ cmstate = cmstate1, targets = [] }

  io (revertCAFs)  -- always revert CAFs on load.
  (cmstate2, ok, mods) <- io (cmLoadModules cmstate1 dflags graph)
  setGHCiState state{ cmstate = cmstate2, targets = files }

  setContextAfterLoad mods
  modulesLoadedMsg ok mods dflags


reloadModule :: String -> GHCi ()
reloadModule "" = do
  state <- getGHCiState
  dflags <- io getDynFlags
  case targets state of
   [] -> io (putStr "no current target\n")
   paths -> do
	-- do the dependency anal first, so that if it fails we don't throw
	-- away the current set of modules.
	graph <- io (cmDepAnal (cmstate state) dflags paths)

	io (revertCAFs)		-- always revert CAFs on reload.
	(cmstate1, ok, mods) 
		<- io (cmLoadModules (cmstate state) dflags graph)
        setGHCiState state{ cmstate=cmstate1 }
	setContextAfterLoad mods
	modulesLoadedMsg ok mods dflags

reloadModule _ = noArgs ":reload"

setContextAfterLoad [] = setContext prel
setContextAfterLoad (m:_) = do
  cmstate <- getCmState
  b <- io (cmModuleIsInterpreted cmstate m)
  if b then setContext ('*':m) else setContext m

modulesLoadedMsg ok mods dflags =
  when (verbosity dflags > 0) $ do
   let mod_commas 
	| null mods = text "none."
	| otherwise = hsep (
	    punctuate comma (map text mods)) <> text "."
   case ok of
    Failed ->
       io (putStrLn (showSDoc (text "Failed, modules loaded: " <> mod_commas)))
    Succeeded  ->
       io (putStrLn (showSDoc (text "Ok, modules loaded: " <> mod_commas)))


typeOfExpr :: String -> GHCi ()
typeOfExpr str 
  = do cms <- getCmState
       dflags <- io getDynFlags
       (new_cmstate, maybe_tystr) <- io (cmTypeOfExpr cms dflags str)
       setCmState new_cmstate
       case maybe_tystr of
	  Nothing    -> return ()
	  Just tystr -> io (putStrLn tystr)

quit :: String -> GHCi Bool
quit _ = return True

shellEscape :: String -> GHCi Bool
shellEscape str = io (system str >> return False)

-----------------------------------------------------------------------------
-- Browing a module's contents

browseCmd :: String -> GHCi ()
browseCmd m = 
  case words m of
    ['*':m] | looksLikeModuleName m -> browseModule m False
    [m]     | looksLikeModuleName m -> browseModule m True
    _ -> throwDyn (CmdLineError "syntax:  :browse <module>")

browseModule m exports_only = do
  cms <- getCmState
  dflags <- io getDynFlags

  is_interpreted <- io (cmModuleIsInterpreted cms m)
  when (not is_interpreted && not exports_only) $
	throwDyn (CmdLineError ("module `" ++ m ++ "' is not interpreted"))

  -- temporarily set the context to the module we're interested in,
  -- just so we can get an appropriate PrintUnqualified
  (as,bs) <- io (cmGetContext cms)
  cms1 <- io (if exports_only then cmSetContext cms dflags [] [prel,m]
			      else cmSetContext cms dflags [m] [])
  cms2 <- io (cmSetContext cms1 dflags as bs)

  (cms3, things) <- io (cmBrowseModule cms2 dflags m exports_only)

  setCmState cms3

  let unqual = cmGetPrintUnqual cms1 -- NOTE: cms1 with the new context

      things' = filter wantToSee things

      wantToSee (AnId id)    = not (isImplicitId id)
      wantToSee (ADataCon _) = False	-- They'll come via their TyCon
      wantToSee _ 	     = True

      thing_names = map getName things

      thingDecl thing@(AnId id)  = ifaceTyThing True{-omit prags-} thing

      thingDecl thing@(AClass c) =
        let rn_decl = ifaceTyThing True{-omit prags-} thing in
	case rn_decl of
	  ClassDecl { tcdSigs = cons } -> 
		rn_decl{ tcdSigs = filter methodIsVisible cons }
	  other -> other
        where
           methodIsVisible (ClassOpSig n _ _ _) = n `elem` thing_names

      thingDecl thing@(ATyCon t) =
        let rn_decl = ifaceTyThing True{-omit prags-} thing in
	case rn_decl of
	  TyData { tcdCons = DataCons cons } -> 
		rn_decl{ tcdCons = DataCons (filter conIsVisible cons) }
	  other -> other
        where
	  conIsVisible (ConDecl n _ _ _ _) = n `elem` thing_names

  io (putStrLn (showSDocForUser unqual (
   	 vcat (map (ppr . thingDecl) things')))
   )

-----------------------------------------------------------------------------
-- Setting the module context

setContext str
  | all sensible mods = fn mods
  | otherwise = throwDyn (CmdLineError "syntax:  :module [+/-] [*]M1 ... [*]Mn")
  where
    (fn, mods) = case str of 
			'+':stuff -> (addToContext,      words stuff)
			'-':stuff -> (removeFromContext, words stuff)
			stuff     -> (newContext,        words stuff) 

    sensible ('*':m) = looksLikeModuleName m
    sensible m       = looksLikeModuleName m

newContext mods = do
  cms <- getCmState
  dflags <- io getDynFlags
  (as,bs) <- separate cms mods [] []
  let bs' = if null as && prel `notElem` bs then prel:bs else bs
  cms' <- io (cmSetContext cms dflags as bs')
  setCmState cms'

separate cmstate []           as bs = return (as,bs)
separate cmstate (('*':m):ms) as bs = do
   b <- io (cmModuleIsInterpreted cmstate m)
   if b then separate cmstate ms (m:as) bs
   	else throwDyn (CmdLineError ("module `" ++ m ++ "' is not interpreted"))
separate cmstate (m:ms)       as bs = separate cmstate ms as (m:bs)

prel = "Prelude"


addToContext mods = do
  cms <- getCmState
  dflags <- io getDynFlags
  (as,bs) <- io (cmGetContext cms)

  (as',bs') <- separate cms mods [] []

  let as_to_add = as' \\ (as ++ bs)
      bs_to_add = bs' \\ (as ++ bs)

  cms' <- io (cmSetContext cms dflags 
			(as ++ as_to_add) (bs ++ bs_to_add))
  setCmState cms'


removeFromContext mods = do
  cms <- getCmState
  dflags <- io getDynFlags
  (as,bs) <- io (cmGetContext cms)

  (as_to_remove,bs_to_remove) <- separate cms mods [] []

  let as' = as \\ (as_to_remove ++ bs_to_remove)
      bs' = bs \\ (as_to_remove ++ bs_to_remove)

  cms' <- io (cmSetContext cms dflags as' bs')
  setCmState cms'

----------------------------------------------------------------------------
-- Code for `:set'

-- set options in the interpreter.  Syntax is exactly the same as the
-- ghc command line, except that certain options aren't available (-C,
-- -E etc.)
--
-- This is pretty fragile: most options won't work as expected.  ToDo:
-- figure out which ones & disallow them.

setCmd :: String -> GHCi ()
setCmd ""
  = do st <- getGHCiState
       let opts = options st
       io $ putStrLn (showSDoc (
   	      text "options currently set: " <> 
   	      if null opts
   		   then text "none."
   		   else hsep (map (\o -> char '+' <> text (optToStr o)) opts)
   	   ))
setCmd str
  = case words str of
	("args":args) -> setArgs args
	("prog":prog) -> setProg prog
	wds -> setOptions wds

setArgs args = do
  st <- getGHCiState
  setGHCiState st{ args = args }

setProg [prog] = do
  st <- getGHCiState
  setGHCiState st{ progname = prog }
setProg _ = do
  io (hPutStrLn stderr "syntax: :set prog <progname>")

setOptions wds =
   do -- first, deal with the GHCi opts (+s, +t, etc.)
      let (plus_opts, minus_opts)  = partition isPlus wds
      mapM_ setOpt plus_opts

      -- now, the GHC flags
      pkgs_before <- io (readIORef v_ExplicitPackages)
      leftovers   <- io (processArgs static_flags minus_opts [])
      pkgs_after  <- io (readIORef v_ExplicitPackages)

      -- update things if the users wants more packages
      let new_packages = pkgs_after \\ pkgs_before
      when (not (null new_packages)) $
	 newPackages new_packages

      -- don't forget about the extra command-line flags from the 
      -- extra_ghc_opts fields in the new packages
      new_package_details <- io (getPackageDetails new_packages)
      let pkg_extra_opts = concatMap extra_ghc_opts new_package_details
      pkg_extra_dyn <- io (processArgs static_flags pkg_extra_opts [])

      -- then, dynamic flags
      io $ do 
	restoreDynFlags
        leftovers <- processArgs dynamic_flags (leftovers ++ pkg_extra_dyn) []
	saveDynFlags

        if (not (null leftovers))
		then throwDyn (CmdLineError ("unrecognised flags: " ++ 
						unwords leftovers))
		else return ()


unsetOptions :: String -> GHCi ()
unsetOptions str
  = do -- first, deal with the GHCi opts (+s, +t, etc.)
       let opts = words str
	   (minus_opts, rest1) = partition isMinus opts
	   (plus_opts, rest2)  = partition isPlus rest1

       if (not (null rest2)) 
	  then io (putStrLn ("unknown option: `" ++ head rest2 ++ "'"))
	  else do

       mapM_ unsetOpt plus_opts
 
       -- can't do GHC flags for now
       if (not (null minus_opts))
	  then throwDyn (CmdLineError "can't unset GHC command-line flags")
	  else return ()

isMinus ('-':s) = True
isMinus _ = False

isPlus ('+':s) = True
isPlus _ = False

setOpt ('+':str)
  = case strToGHCiOpt str of
	Nothing -> io (putStrLn ("unknown option: `" ++ str ++ "'"))
	Just o  -> setOption o

unsetOpt ('+':str)
  = case strToGHCiOpt str of
	Nothing -> io (putStrLn ("unknown option: `" ++ str ++ "'"))
	Just o  -> unsetOption o

strToGHCiOpt :: String -> (Maybe GHCiOption)
strToGHCiOpt "s" = Just ShowTiming
strToGHCiOpt "t" = Just ShowType
strToGHCiOpt "r" = Just RevertCAFs
strToGHCiOpt _   = Nothing

optToStr :: GHCiOption -> String
optToStr ShowTiming = "s"
optToStr ShowType   = "t"
optToStr RevertCAFs = "r"

newPackages new_pkgs = do	-- The new packages are already in v_Packages
  state    <- getGHCiState
  dflags   <- io getDynFlags
  cmstate1 <- io (cmUnload (cmstate state) dflags)
  setGHCiState state{ cmstate = cmstate1, targets = [] }
  io (linkPackages dflags new_pkgs)
  setContextAfterLoad []

-- ---------------------------------------------------------------------------
-- code for `:show'

showCmd str =
  case words str of
	["modules" ] -> showModules
	["bindings"] -> showBindings
	["linker"]   -> io showLinkerState
	_ -> throwDyn (CmdLineError "syntax:  :show [modules|bindings]")

showModules = do
  cms <- getCmState
  let (mg, hpt) = cmGetModInfo cms
  mapM_ (showModule hpt) mg


showModule :: HomePackageTable -> ModSummary -> GHCi ()
showModule hpt mod_summary
  = case lookupModuleEnv hpt mod of
	Nothing	      -> panic "missing linkable"
	Just mod_info -> io (putStrLn (showModMsg obj_linkable mod locn))
		      where
			 obj_linkable = isObjectLinkable (hm_linkable mod_info)
  where
    mod = ms_mod mod_summary
    locn = ms_location mod_summary

showBindings = do
  cms <- getCmState
  let
	unqual = cmGetPrintUnqual cms
	showBinding b = putStrLn (showSDocForUser unqual (ppr (ifaceTyThing True{-omit prags-} b)))

  io (mapM_ showBinding (cmGetBindings cms))
  return ()


-----------------------------------------------------------------------------
-- GHCi monad

data GHCiState = GHCiState
     { 
	progname       :: String,
	args	       :: [String],
	targets        :: [FilePath],
	cmstate        :: CmState,
	options        :: [GHCiOption]
     }

data GHCiOption 
	= ShowTiming		-- show time/allocs after evaluation
	| ShowType		-- show the type of expressions
	| RevertCAFs		-- revert CAFs after every evaluation
	deriving Eq

newtype GHCi a = GHCi { unGHCi :: IORef GHCiState -> IO a }

startGHCi :: GHCi a -> GHCiState -> IO a
startGHCi g state = do ref <- newIORef state; unGHCi g ref

instance Monad GHCi where
  (GHCi m) >>= k  =  GHCi $ \s -> m s >>= \a -> unGHCi (k a) s
  return a  = GHCi $ \s -> return a

ghciHandleDyn :: Typeable t => (t -> GHCi a) -> GHCi a -> GHCi a
ghciHandleDyn h (GHCi m) = GHCi $ \s -> 
   Exception.catchDyn (m s) (\e -> unGHCi (h e) s)

getGHCiState   = GHCi $ \r -> readIORef r
setGHCiState s = GHCi $ \r -> writeIORef r s

-- for convenience...
getCmState = getGHCiState >>= return . cmstate
setCmState cms = do s <- getGHCiState; setGHCiState s{cmstate=cms}

isOptionSet :: GHCiOption -> GHCi Bool
isOptionSet opt
 = do st <- getGHCiState
      return (opt `elem` options st)

setOption :: GHCiOption -> GHCi ()
setOption opt
 = do st <- getGHCiState
      setGHCiState (st{ options = opt : filter (/= opt) (options st) })

unsetOption :: GHCiOption -> GHCi ()
unsetOption opt
 = do st <- getGHCiState
      setGHCiState (st{ options = filter (/= opt) (options st) })

io :: IO a -> GHCi a
io m = GHCi { unGHCi = \s -> m >>= return }

-----------------------------------------------------------------------------
-- recursive exception handlers

-- Don't forget to unblock async exceptions in the handler, or if we're
-- in an exception loop (eg. let a = error a in a) the ^C exception
-- may never be delivered.  Thanks to Marcin for pointing out the bug.

ghciHandle :: (Exception -> GHCi a) -> GHCi a -> GHCi a
ghciHandle h (GHCi m) = GHCi $ \s -> 
   Exception.catch (m s) 
	(\e -> unGHCi (ghciUnblock (h e)) s)

ghciUnblock :: GHCi a -> GHCi a
ghciUnblock (GHCi a) = GHCi $ \s -> Exception.unblock (a s)

-----------------------------------------------------------------------------
-- timing & statistics

timeIt :: GHCi a -> GHCi a
timeIt action
  = do b <- isOptionSet ShowTiming
       if not b 
	  then action 
	  else do allocs1 <- io $ getAllocations
		  time1   <- io $ getCPUTime
		  a <- action
		  allocs2 <- io $ getAllocations
		  time2   <- io $ getCPUTime
		  io $ printTimes (allocs2 - allocs1) (time2 - time1)
		  return a

foreign import ccall "getAllocations" getAllocations :: IO Int

printTimes :: Int -> Integer -> IO ()
printTimes allocs psecs
   = do let secs = (fromIntegral psecs / (10^12)) :: Float
	    secs_str = showFFloat (Just 2) secs
	putStrLn (showSDoc (
		 parens (text (secs_str "") <+> text "secs" <> comma <+> 
			 int allocs <+> text "bytes")))

-----------------------------------------------------------------------------
-- reverting CAFs
	
revertCAFs :: IO ()
revertCAFs = do
  rts_revertCAFs
  turnOffBuffering
	-- Have to turn off buffering again, because we just 
	-- reverted stdout, stderr & stdin to their defaults.

foreign import ccall "revertCAFs" rts_revertCAFs  :: IO ()  
	-- Make it "safe", just in case

-- -----------------------------------------------------------------------------
-- Utils

expandPath :: String -> GHCi String
expandPath path = 
  case dropWhile isSpace path of
   ('~':d) -> do
	tilde <- io (getEnv "HOME")	-- will fail if HOME not defined
	return (tilde ++ '/':d)
   other -> 
	return other
