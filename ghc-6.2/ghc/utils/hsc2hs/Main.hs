{-# OPTIONS -fglasgow-exts #-}

------------------------------------------------------------------------
-- $Id: Main.hs,v 1.48 2003/08/27 14:11:17 panne Exp $
--
-- Program for converting .hsc files to .hs files, by converting the
-- file into a C program which is run to generate the Haskell source.
-- Certain items known only to the C compiler can then be used in
-- the Haskell module; for example #defined constants, byte offsets
-- within structures, etc.
--
-- See the documentation in the Users' Guide for more details.

#if __GLASGOW_HASKELL__ >= 504
import System.Console.GetOpt
#else
import GetOpt
#endif

import Config
import System        (getProgName, getArgs, ExitCode(..), exitWith, system)
import Directory     (removeFile,doesFileExist)
import Monad         (MonadPlus(..), liftM, liftM2, when, unless)
import Char          (isAlpha, isAlphaNum, isSpace, isDigit, toUpper, intToDigit, ord)
import List          (intersperse, isSuffixOf)
import IO            (hPutStr, hPutStrLn, stderr)

#include "../../includes/config.h"

#ifdef mingw32_HOST_OS
import Foreign

#if __GLASGOW_HASKELL__ >= 504
import Foreign.C.String
#else
import CString
#endif
#endif



version :: String
version = "hsc2hs version 0.65\n"

data Flag
    = Help
    | Version
    | Template  String
    | Compiler  String
    | Linker    String
    | CompFlag  String
    | LinkFlag  String
    | NoCompile
    | Include   String
    | Define    String (Maybe String)
    | Output    String
    | Verbose

template_flag (Template _) = True
template_flag _		   = False

include :: String -> Flag
include s@('\"':_) = Include s
include s@('<' :_) = Include s
include s          = Include ("\""++s++"\"")

define :: String -> Flag
define s = case break (== '=') s of
    (name, [])      -> Define name Nothing
    (name, _:value) -> Define name (Just value)

options :: [OptDescr Flag]
options = [
    Option ['o'] ["output"]     (ReqArg Output     "FILE")
        "name of main output file",
    Option ['t'] ["template"]   (ReqArg Template   "FILE")
        "template file",
    Option ['c'] ["cc"]         (ReqArg Compiler   "PROG")
        "C compiler to use",
    Option ['l'] ["ld"]         (ReqArg Linker     "PROG")
        "linker to use",
    Option ['C'] ["cflag"]      (ReqArg CompFlag   "FLAG")
        "flag to pass to the C compiler",
    Option ['I'] []             (ReqArg (CompFlag . ("-I"++)) "DIR")
        "passed to the C compiler",
    Option ['L'] ["lflag"]      (ReqArg LinkFlag   "FLAG")
        "flag to pass to the linker",
    Option ['i'] ["include"]    (ReqArg include    "FILE")
        "as if placed in the source",
    Option ['D'] ["define"]     (ReqArg define "NAME[=VALUE]")
        "as if placed in the source",
    Option []    ["no-compile"] (NoArg  NoCompile)
        "stop after writing *_hsc_make.c",
    Option ['v'] ["verbose"]    (NoArg  Verbose)
        "dump commands to stderr",
    Option ['?'] ["help"]       (NoArg  Help)
        "display this help and exit",
    Option ['V'] ["version"]    (NoArg  Version)
        "output version information and exit" ]
    

main :: IO ()
main = do
    prog <- getProgramName
    let header = "Usage: "++prog++" [OPTIONS] INPUT.hsc [...]\n"
    args <- getArgs
    let (flags, files, errs) = getOpt Permute options args

	-- If there is no Template flag explicitly specified, try
	-- to find one by looking near the executable.  This only
	-- works on Win32 (getExecDir). On Unix, there's a wrapper 
	-- script which specifies an explicit template flag.
    flags_w_tpl <- if any template_flag flags then
			return flags
		   else	
			do mb_path <- getExecDir "/bin/hsc2hs.exe"
			   add_opt <-
			    case mb_path of
			      Nothing   -> return id
			      Just path -> do
				let templ = path ++ "/template-hsc.h"
				flg <- doesFileExist templ
				if flg 
				 then return ((Template templ):)
				 else return id
		           return (add_opt flags) 
    case (files, errs) of
        (_, _)
            | any isHelp    flags_w_tpl -> bye (usageInfo header options)
            | any isVersion flags_w_tpl -> bye version
            where
            isHelp    Help    = True; isHelp    _ = False
            isVersion Version = True; isVersion _ = False
        (files@(_:_), []) -> mapM_ (processFile flags_w_tpl) files
        (_,   errs) -> die (concat errs ++ usageInfo header options)

getProgramName :: IO String
getProgramName = liftM (`withoutSuffix` "-bin") getProgName
   where str `withoutSuffix` suff
            | suff `isSuffixOf` str = take (length str - length suff) str
            | otherwise             = str

bye :: String -> IO a
bye s = putStr s >> exitWith ExitSuccess

die :: String -> IO a
die s = hPutStr stderr s >> exitWith (ExitFailure 1)

processFile :: [Flag] -> String -> IO ()
processFile flags name 
  = do let file_name = dosifyPath name
       s <- readFile file_name
       case parser of
    	   Parser p -> case p (SourcePos file_name 1) s of
    	       Success _ _ _ toks -> output flags file_name toks
    	       Failure (SourcePos name' line) msg ->
    		   die (name'++":"++show line++": "++msg++"\n")

------------------------------------------------------------------------
-- A deterministic parser which remembers the text which has been parsed.

newtype Parser a = Parser (SourcePos -> String -> ParseResult a)

data ParseResult a = Success !SourcePos String String a
                   | Failure !SourcePos String

data SourcePos = SourcePos String !Int

updatePos :: SourcePos -> Char -> SourcePos
updatePos pos@(SourcePos name line) ch = case ch of
    '\n' -> SourcePos name (line + 1)
    _    -> pos

instance Monad Parser where
    return a = Parser $ \pos s -> Success pos [] s a
    Parser m >>= k =
        Parser $ \pos s -> case m pos s of
            Success pos' out1 s' a -> case k a of
                Parser k' -> case k' pos' s' of
                    Success pos'' out2 imp'' b ->
                        Success pos'' (out1++out2) imp'' b
                    Failure pos'' msg -> Failure pos'' msg
            Failure pos' msg -> Failure pos' msg
    fail msg = Parser $ \pos _ -> Failure pos msg

instance MonadPlus Parser where
    mzero                     = fail "mzero"
    Parser m `mplus` Parser n =
        Parser $ \pos s -> case m pos s of
            success@(Success _ _ _ _) -> success
            Failure _ _               -> n pos s

getPos :: Parser SourcePos
getPos = Parser $ \pos s -> Success pos [] s pos

setPos :: SourcePos -> Parser ()
setPos pos = Parser $ \_ s -> Success pos [] s ()

message :: Parser a -> String -> Parser a
Parser m `message` msg =
    Parser $ \pos s -> case m pos s of
        success@(Success _ _ _ _) -> success
        Failure pos' _            -> Failure pos' msg

catchOutput_ :: Parser a -> Parser String
catchOutput_ (Parser m) =
    Parser $ \pos s -> case m pos s of
        Success pos' out s' _ -> Success pos' [] s' out
        Failure pos' msg      -> Failure pos' msg

fakeOutput :: Parser a -> String -> Parser a
Parser m `fakeOutput` out =
    Parser $ \pos s -> case m pos s of
        Success pos' _ s' a -> Success pos' out s' a
        Failure pos' msg    -> Failure pos' msg

lookAhead :: Parser String
lookAhead = Parser $ \pos s -> Success pos [] s s

satisfy :: (Char -> Bool) -> Parser Char
satisfy p =
    Parser $ \pos s -> case s of
        c:cs | p c -> Success (updatePos pos c) [c] cs c
        _          -> Failure pos "Bad character"

char_ :: Char -> Parser ()
char_ c = do
    satisfy (== c) `message` (show c++" expected")
    return ()

anyChar_ :: Parser ()
anyChar_ = do
    satisfy (const True) `message` "Unexpected end of file"
    return ()

any2Chars_ :: Parser ()
any2Chars_ = anyChar_ >> anyChar_

many :: Parser a -> Parser [a]
many p = many1 p `mplus` return []

many1 :: Parser a -> Parser [a]
many1 p = liftM2 (:) p (many p)

many_ :: Parser a -> Parser ()
many_ p = many1_ p `mplus` return ()

many1_ :: Parser a -> Parser ()
many1_ p = p >> many_ p

manySatisfy, manySatisfy1 :: (Char -> Bool) -> Parser String
manySatisfy  = many  . satisfy
manySatisfy1 = many1 . satisfy

manySatisfy_, manySatisfy1_ :: (Char -> Bool) -> Parser ()
manySatisfy_  = many_  . satisfy
manySatisfy1_ = many1_ . satisfy

------------------------------------------------------------------------
-- Parser of hsc syntax.

data Token
    = Text    SourcePos String
    | Special SourcePos String String

parser :: Parser [Token]
parser = do
    pos <- getPos
    t <- catchOutput_ text
    s <- lookAhead
    rest <- case s of
        []  -> return []
        _:_ -> liftM2 (:) (special `fakeOutput` []) parser
    return (if null t then rest else Text pos t : rest)

text :: Parser ()
text = do
    s <- lookAhead
    case s of
        []        -> return ()
        c:_ | isAlpha c || c == '_' -> do
            anyChar_
            manySatisfy_ (\c' -> isAlphaNum c' || c' == '_' || c' == '\'')
            text
        c:_ | isHsSymbol c -> do
            symb <- catchOutput_ (manySatisfy_ isHsSymbol)
            case symb of
                "#" -> return ()
                '-':'-':symb' | all (== '-') symb' -> do
                    return () `fakeOutput` symb
                    manySatisfy_ (/= '\n')
                    text
                _ -> do
                    return () `fakeOutput` unescapeHashes symb
                    text
        '\"':_    -> do anyChar_; hsString '\"'; text
        '\'':_    -> do anyChar_; hsString '\''; text
        '{':'-':_ -> do any2Chars_; linePragma `mplus` hsComment; text
        _:_       -> do anyChar_; text

hsString :: Char -> Parser ()
hsString quote = do
    s <- lookAhead
    case s of
        []               -> return ()
        c:_ | c == quote -> anyChar_
        '\\':c:_
            | isSpace c  -> do
                anyChar_
                manySatisfy_ isSpace
                char_ '\\' `mplus` return ()
                hsString quote
            | otherwise  -> do any2Chars_; hsString quote
        _:_              -> do anyChar_; hsString quote

hsComment :: Parser ()
hsComment = do
    s <- lookAhead
    case s of
        []        -> return ()
        '-':'}':_ -> any2Chars_
        '{':'-':_ -> do any2Chars_; hsComment; hsComment
        _:_       -> do anyChar_; hsComment

linePragma :: Parser ()
linePragma = do
    char_ '#'
    manySatisfy_ isSpace
    satisfy (\c -> c == 'L' || c == 'l')
    satisfy (\c -> c == 'I' || c == 'i')
    satisfy (\c -> c == 'N' || c == 'n')
    satisfy (\c -> c == 'E' || c == 'e')
    manySatisfy1_ isSpace
    line <- liftM read $ manySatisfy1 isDigit
    manySatisfy1_ isSpace
    char_ '\"'
    name <- manySatisfy (/= '\"')
    char_ '\"'
    manySatisfy_ isSpace
    char_ '#'
    char_ '-'
    char_ '}'
    setPos (SourcePos name (line - 1))

isHsSymbol :: Char -> Bool
isHsSymbol '!' = True; isHsSymbol '#' = True; isHsSymbol '$'  = True
isHsSymbol '%' = True; isHsSymbol '&' = True; isHsSymbol '*'  = True
isHsSymbol '+' = True; isHsSymbol '.' = True; isHsSymbol '/'  = True
isHsSymbol '<' = True; isHsSymbol '=' = True; isHsSymbol '>'  = True
isHsSymbol '?' = True; isHsSymbol '@' = True; isHsSymbol '\\' = True
isHsSymbol '^' = True; isHsSymbol '|' = True; isHsSymbol '-'  = True
isHsSymbol '~' = True
isHsSymbol _   = False

unescapeHashes :: String -> String
unescapeHashes []          = []
unescapeHashes ('#':'#':s) = '#' : unescapeHashes s
unescapeHashes (c:s)       = c   : unescapeHashes s

lookAheadC :: Parser String
lookAheadC = liftM joinLines lookAhead
    where
    joinLines []            = []
    joinLines ('\\':'\n':s) = joinLines s
    joinLines (c:s)         = c : joinLines s

satisfyC :: (Char -> Bool) -> Parser Char
satisfyC p = do
    s <- lookAhead
    case s of
        '\\':'\n':_ -> do any2Chars_ `fakeOutput` []; satisfyC p
        _           -> satisfy p

charC_ :: Char -> Parser ()
charC_ c = do
    satisfyC (== c) `message` (show c++" expected")
    return ()

anyCharC_ :: Parser ()
anyCharC_ = do
    satisfyC (const True) `message` "Unexpected end of file"
    return ()

any2CharsC_ :: Parser ()
any2CharsC_ = anyCharC_ >> anyCharC_

manySatisfyC :: (Char -> Bool) -> Parser String
manySatisfyC = many . satisfyC

manySatisfyC_ :: (Char -> Bool) -> Parser ()
manySatisfyC_ = many_ . satisfyC

special :: Parser Token
special = do
    manySatisfyC_ (\c -> isSpace c && c /= '\n')
    s <- lookAheadC
    case s of
        '{':_ -> do
            anyCharC_
            manySatisfyC_ isSpace
            sp <- keyArg (== '\n')
            charC_ '}'
            return sp
        _ -> keyArg (const False)

keyArg :: (Char -> Bool) -> Parser Token
keyArg eol = do
    pos <- getPos
    key <- keyword `message` "hsc keyword or '{' expected"
    manySatisfyC_ (\c' -> isSpace c' && c' /= '\n' || eol c')
    arg <- catchOutput_ (argument eol)
    return (Special pos key arg)

keyword :: Parser String
keyword = do
    c  <- satisfyC (\c' -> isAlpha c' || c' == '_')
    cs <- manySatisfyC (\c' -> isAlphaNum c' || c' == '_')
    return (c:cs)

argument :: (Char -> Bool) -> Parser ()
argument eol = do
    s <- lookAheadC
    case s of
        []          -> return ()
        c:_ | eol c -> do anyCharC_;               argument eol
        '\n':_      -> return ()
        '\"':_      -> do anyCharC_; cString '\"'; argument eol
        '\'':_      -> do anyCharC_; cString '\''; argument eol
        '(':_       -> do anyCharC_; nested ')';   argument eol
        ')':_       -> return ()
        '/':'*':_   -> do any2CharsC_; cComment;   argument eol
        '/':'/':_   -> do
            any2CharsC_; manySatisfyC_ (/= '\n');  argument eol
        '[':_       -> do anyCharC_; nested ']';   argument eol
        ']':_       -> return ()
        '{':_       -> do anyCharC_; nested '}';   argument eol
        '}':_       -> return ()
        _:_         -> do anyCharC_;               argument eol

nested :: Char -> Parser ()
nested c = do argument (== '\n'); charC_ c

cComment :: Parser ()
cComment = do
    s <- lookAheadC
    case s of
        []        -> return ()
        '*':'/':_ -> do any2CharsC_
        _:_       -> do anyCharC_; cComment

cString :: Char -> Parser ()
cString quote = do
    s <- lookAheadC
    case s of
        []               -> return ()
        c:_ | c == quote -> anyCharC_
        '\\':_:_         -> do any2CharsC_; cString quote
        _:_              -> do anyCharC_; cString quote

------------------------------------------------------------------------
-- Write the output files.

splitName :: String -> (String, String)
splitName name =
    case break (== '/') name of
        (file, [])       -> ([], file)
        (dir,  sep:rest) -> (dir++sep:restDir, restFile)
            where
            (restDir, restFile) = splitName rest

splitExt :: String -> (String, String)
splitExt name =
    case break (== '.') name of
        (base, [])         -> (base, [])
        (base, sepRest@(sep:rest))
            | null restExt -> (base,               sepRest)
            | otherwise    -> (base++sep:restBase, restExt)
            where
            (restBase, restExt) = splitExt rest

output :: [Flag] -> String -> [Token] -> IO ()
output flags name toks = do
    
    (outName, outDir, outBase) <- case [f | Output f <- flags] of
        []
            | not (null ext) &&
              last ext == 'c'   -> return (dir++base++init ext,  dir, base)
            | ext == ".hs"      -> return (dir++base++"_out.hs", dir, base)
            | otherwise         -> return (dir++base++".hs",     dir, base)
            where
            (dir,  file) = splitName name
            (base, ext)  = splitExt  file
        [f] -> let
            (dir,  file) = splitName f
            (base, _)    = splitExt file
            in return (f, dir, base)
        _ -> onlyOne "output file"
    
    let cProgName    = outDir++outBase++"_hsc_make.c"
        oProgName    = outDir++outBase++"_hsc_make.o"
        progName     = outDir++outBase++"_hsc_make" ++ progNameSuffix
	outHFile     = outBase++"_hsc.h"
        outHName     = outDir++outHFile
        outCName     = outDir++outBase++"_hsc.c"
	
	beVerbose    = any (\ x -> case x of { Verbose{} -> True; _ -> False}) flags

    let execProgName
            | null outDir = '.':pathSep:progName
            | otherwise   = progName
    
    let specials = [(pos, key, arg) | Special pos key arg <- toks]
    
    let needsC = any (\(_, key, _) -> key == "def") specials
        needsH = needsC
    
    let includeGuard = map fixChar outHName
            where
            fixChar c | isAlphaNum c = toUpper c
                      | otherwise    = '_'

          -- try locating GHC..on Win32, look in the vicinity of hsc2hs.
        locateGhc def = do
	    mb <- getExecDir "bin/hsc2hs.exe"
	    case mb of
	      Nothing -> return def
	      Just x  -> do
	         let ghc_path = dosifyPath (x ++ "bin/ghc.exe")
	         flg <- doesFileExist ghc_path
		 if flg 
		  then return ghc_path
		  else return def
    
    compiler <- case [c | Compiler c <- flags] of
        []  -> locateGhc "ghc"
        [c] -> return c
        _   -> onlyOne "compiler"
    
    linker <- case [l | Linker l <- flags] of
        []  -> locateGhc compiler
        [l] -> return l
        _   -> onlyOne "linker"

    writeFile cProgName $
        concatMap outFlagHeaderCProg flags++
        concatMap outHeaderCProg specials++
        "\nint main (int argc, char *argv [])\n{\n"++
        outHeaderHs flags (if needsH then Just outHName else Nothing) specials++
        outHsLine (SourcePos name 0)++
        concatMap outTokenHs toks++
        "    return 0;\n}\n"
    
    unless (null [() | NoCompile <- flags]) $ exitWith ExitSuccess


    
    compilerStatus <- systemL beVerbose $
        compiler++
        " -c"++
        concat [" "++f | CompFlag f <- flags]++
        " "++cProgName++
        " -o "++oProgName
    case compilerStatus of
        e@(ExitFailure _) -> exitWith e
        _                 -> return ()
    removeFile cProgName
    
    linkerStatus <- systemL beVerbose $
        linker++
        concat [" "++f | LinkFlag f <- flags]++
        " "++oProgName++
        " -o "++progName
    case linkerStatus of
        e@(ExitFailure _) -> exitWith e
        _                 -> return ()
    removeFile oProgName
    
    progStatus <- systemL beVerbose (execProgName++" >"++outName)
    removeFile progName
    case progStatus of
        e@(ExitFailure _) -> exitWith e
        _                 -> return ()
    
    when needsH $ writeFile outHName $
        "#ifndef "++includeGuard++"\n\ 
        \#define "++includeGuard++"\n\ 
        \#if " ++
	"__GLASGOW_HASKELL__ && __GLASGOW_HASKELL__ < 409\n\ 
        \#include <Rts.h>\n\ 
        \#endif\n\ 
        \#include <HsFFI.h>\n\ 
        \#if __NHC__\n\ 
        \#undef HsChar\n\ 
        \#define HsChar int\n\ 
        \#endif\n"++
        concatMap outFlagH flags++
        concatMap outTokenH specials++
        "#endif\n"
    
    when needsC $ writeFile outCName $
        "#include \""++outHFile++"\"\n"++
        concatMap outTokenC specials
	-- NB. outHFile not outHName; works better when processed
	-- by gcc or mkdependC.

systemL :: Bool -> String -> IO ExitCode
systemL flg s = do
  when flg (hPutStrLn stderr ("Executing: " ++ s))
  system s

onlyOne :: String -> IO a
onlyOne what = die ("Only one "++what++" may be specified\n")

outFlagHeaderCProg :: Flag -> String
outFlagHeaderCProg (Template t)          = "#include \""++t++"\"\n"
outFlagHeaderCProg (Include  f)          = "#include "++f++"\n"
outFlagHeaderCProg (Define   n Nothing)  = "#define "++n++"\n"
outFlagHeaderCProg (Define   n (Just v)) = "#define "++n++" "++v++"\n"
outFlagHeaderCProg _                     = ""

outHeaderCProg :: (SourcePos, String, String) -> String
outHeaderCProg (pos, key, arg) = case key of
    "include"           -> outCLine pos++"#include "++arg++"\n"
    "define"            -> outCLine pos++"#define "++arg++"\n"
    "undef"             -> outCLine pos++"#undef "++arg++"\n"
    "def"               -> case arg of
        's':'t':'r':'u':'c':'t':' ':_ -> outCLine pos++arg++"\n"
        't':'y':'p':'e':'d':'e':'f':' ':_ -> outCLine pos++arg++"\n"
        _ -> ""
    _ | conditional key -> outCLine pos++"#"++key++" "++arg++"\n"
    "let"               -> case break (== '=') arg of
        (_,      "")     -> ""
        (header, _:body) -> case break isSpace header of
            (name, args) ->
                outCLine pos++
                "#define hsc_"++name++"("++dropWhile isSpace args++") \ 
                \printf ("++joinLines body++");\n"
    _ -> ""
    where
    joinLines = concat . intersperse " \\\n" . lines

outHeaderHs :: [Flag] -> Maybe String -> [(SourcePos, String, String)] -> String
outHeaderHs flags inH toks =
    "#if " ++
    "__GLASGOW_HASKELL__ && __GLASGOW_HASKELL__ < 409\n\ 
    \    printf (\"{-# OPTIONS -optc-D" ++
    "__GLASGOW_HASKELL__=%d #-}\\n\", \ 
    \__GLASGOW_HASKELL__);\n\ 
    \#endif\n"++
    case inH of
        Nothing -> concatMap outFlag flags++concatMap outSpecial toks
        Just f  -> outOption ("-#include \""++f++"\"")
    where
    outFlag (Include f)          = outOption ("-#include "++f)
    outFlag (Define  n Nothing)  = outOption ("-optc-D"++n)
    outFlag (Define  n (Just v)) = outOption ("-optc-D"++n++"="++v)
    outFlag _                    = ""
    outSpecial (pos, key, arg) = case key of
        "include"                  -> outOption ("-#include "++arg)
        "define" | goodForOptD arg -> outOption ("-optc-D"++toOptD arg)
                 | otherwise       -> ""
        _ | conditional key        -> outCLine pos++"#"++key++" "++arg++"\n"
        _                          -> ""
    goodForOptD arg = case arg of
        ""              -> True
        c:_ | isSpace c -> True
        '(':_           -> False
        _:s             -> goodForOptD s
    toOptD arg = case break isSpace arg of
        (name, "")      -> name
        (name, _:value) -> name++'=':dropWhile isSpace value
    outOption s = "    printf (\"{-# OPTIONS %s #-}\\n\", \""++
                  showCString s++"\");\n"

outTokenHs :: Token -> String
outTokenHs (Text pos text) =
    case break (== '\n') text of
        (all, [])       -> outText all
        (first, _:rest) ->
            outText (first++"\n")++
            outHsLine pos++
            outText rest
    where
    outText s = "    fputs (\""++showCString s++"\", stdout);\n"
outTokenHs (Special pos key arg) =
    case key of
        "include"           -> ""
        "define"            -> ""
        "undef"             -> ""
        "def"               -> ""
        _ | conditional key -> outCLine pos++"#"++key++" "++arg++"\n"
        "let"               -> ""
        "enum"              -> outCLine pos++outEnum arg
        _                   -> outCLine pos++"    hsc_"++key++" ("++arg++");\n"

outEnum :: String -> String
outEnum arg =
    case break (== ',') arg of
        (_, [])        -> ""
        (t, _:afterT) -> case break (== ',') afterT of
            (f, afterF) -> let
                enums []    = ""
                enums (_:s) = case break (== ',') s of
                    (enum, rest) -> let
                        this = case break (== '=') $ dropWhile isSpace enum of
                            (name, []) ->
                                "    hsc_enum ("++t++", "++f++", \ 
                                \hsc_haskellize (\""++name++"\"), "++
                                name++");\n"
                            (hsName, _:cName) ->
                                "    hsc_enum ("++t++", "++f++", \ 
                                \printf (\"%s\", \""++hsName++"\"), "++
                                cName++");\n"
                        in this++enums rest
                in enums afterF

outFlagH :: Flag -> String
outFlagH (Include  f)          = "#include "++f++"\n"
outFlagH (Define   n Nothing)  = "#define "++n++"\n"
outFlagH (Define   n (Just v)) = "#define "++n++" "++v++"\n"
outFlagH _                     = ""

outTokenH :: (SourcePos, String, String) -> String
outTokenH (pos, key, arg) =
    case key of
        "include" -> outCLine pos++"#include "++arg++"\n"
        "define"  -> outCLine pos++"#define " ++arg++"\n"
        "undef"   -> outCLine pos++"#undef "  ++arg++"\n"
        "def"     -> outCLine pos++case arg of
            's':'t':'r':'u':'c':'t':' ':_ -> arg++"\n"
            't':'y':'p':'e':'d':'e':'f':' ':_ -> arg++"\n"
            'i':'n':'l':'i':'n':'e':' ':_ ->
                "#ifdef __GNUC__\n\ 
                \extern\n\ 
                \#endif\n"++
                arg++"\n"
            _ -> "extern "++header++";\n"
            where header = takeWhile (\c -> c /= '{' && c /= '=') arg
        _ | conditional key -> outCLine pos++"#"++key++" "++arg++"\n"
        _ -> ""

outTokenC :: (SourcePos, String, String) -> String
outTokenC (pos, key, arg) =
    case key of
        "def" -> case arg of
            's':'t':'r':'u':'c':'t':' ':_ -> ""
            't':'y':'p':'e':'d':'e':'f':' ':_ -> ""
            'i':'n':'l':'i':'n':'e':' ':arg' ->
		case span (\c -> c /= '{' && c /= '=') arg' of
		(header, body) ->
		    outCLine pos++
		    "#ifndef __GNUC__\n\ 
		    \extern inline\n\ 
		    \#endif\n"++
		    header++
		    "\n#ifndef __GNUC__\n\ 
		    \;\n\ 
		    \#else\n"++
		    body++
		    "\n#endif\n"
            _ -> outCLine pos++arg++"\n"
        _ | conditional key -> outCLine pos++"#"++key++" "++arg++"\n"
        _ -> ""

conditional :: String -> Bool
conditional "if"      = True
conditional "ifdef"   = True
conditional "ifndef"  = True
conditional "elif"    = True
conditional "else"    = True
conditional "endif"   = True
conditional "error"   = True
conditional "warning" = True
conditional _         = False

outCLine :: SourcePos -> String
outCLine (SourcePos name line) =
    "# "++show line++" \""++showCString (snd (splitName name))++"\"\n"

outHsLine :: SourcePos -> String
outHsLine (SourcePos name line) =
    "    hsc_line ("++show (line + 1)++", \""++
    showCString (snd (splitName name))++"\");\n"

showCString :: String -> String
showCString = concatMap showCChar
    where
    showCChar '\"' = "\\\""
    showCChar '\'' = "\\\'"
    showCChar '?'  = "\\?"
    showCChar '\\' = "\\\\"
    showCChar c | c >= ' ' && c <= '~' = [c]
    showCChar '\a' = "\\a"
    showCChar '\b' = "\\b"
    showCChar '\f' = "\\f"
    showCChar '\n' = "\\n\"\n           \""
    showCChar '\r' = "\\r"
    showCChar '\t' = "\\t"
    showCChar '\v' = "\\v"
    showCChar c    = ['\\',
                      intToDigit (ord c `quot` 64),
                      intToDigit (ord c `quot` 8 `mod` 8),
                      intToDigit (ord c          `mod` 8)]



-----------------------------------------
--	Cut and pasted from ghc/compiler/SysTools
-- Convert paths foo/baz to foo\baz on Windows


#if defined(mingw32_HOST_OS)
subst a b ls = map (\ x -> if x == a then b else x) ls
unDosifyPath xs = subst '\\' '/' xs
dosifyPath xs = subst '/' '\\' xs

getExecDir :: String -> IO (Maybe String)
-- (getExecDir cmd) returns the directory in which the current
--	  	    executable, which should be called 'cmd', is running
-- So if the full path is /a/b/c/d/e, and you pass "d/e" as cmd,
-- you'll get "/a/b/c" back as the result
getExecDir cmd
  = allocaArray len $ \buf -> do
	ret <- getModuleFileName nullPtr buf len
	if ret == 0 then return Nothing
	            else do s <- peekCString buf
			    return (Just (reverse (drop (length cmd) 
							(reverse (unDosifyPath s)))))
  where
    len = 2048::Int -- Plenty, PATH_MAX is 512 under Win32.

foreign import stdcall "GetModuleFileNameA" unsafe
  getModuleFileName :: Ptr () -> CString -> Int -> IO Int32

#else
dosifyPath xs = xs

getExecDir :: String -> IO (Maybe String) 
getExecDir s = do return Nothing
#endif
