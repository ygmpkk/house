{-								-*-haskell-*-
-----------------------------------------------------------------------------
$Id: Parser.y,v 1.122.2.5 2003/11/27 13:34:55 simonmar Exp $

Haskell grammar.

Author(s): Simon Marlow, Sven Panne 1997, 1998, 1999
-----------------------------------------------------------------------------
-}

{
module Parser ( parseModule, parseStmt, parseIdentifier, parseIface ) where

#include "HsVersions.h"

import HsSyn
import HsTypes		( mkHsTupCon )

import RdrHsSyn
import HscTypes		( ParsedIface(..), IsBootInterface, noDependencies )
import Lexer
import RdrName
import PrelNames	( mAIN_Name, funTyConName, listTyConName, 
			  parrTyConName, consDataConName )
import TysWiredIn	( unitTyCon, unitDataCon, tupleTyCon, 
			  tupleCon, nilDataCon )
import ForeignCall	( Safety(..), CExportSpec(..), 
			  CCallConv(..), CCallTarget(..), defaultCCallConv,
			)
import OccName		( UserFS, varName, tcName, dataName, tcClsName, tvName )
import TyCon		( DataConDetails(..) )
import DataCon		( DataCon, dataConName )
import SrcLoc		( SrcLoc )
import Module
import CmdLineOpts	( opt_SccProfilingOn, opt_InPackage )
import Type		( Kind, mkArrowKind, liftedTypeKind )
import BasicTypes	( Boxity(..), Fixity(..), FixityDirection(..), 
			  IPName(..), NewOrData(..), StrictnessMark(..),
			  Activation(..), FixitySig(..) )
import Panic

import GLAEXTS
import CStrings		( CLabelString )
import FastString
import Maybes		( orElse )
import Outputable
import Char		( ord )

}

{-
-----------------------------------------------------------------------------
Conflicts: 29 shift/reduce, [SDM 19/9/2002]

10 for abiguity in 'if x then y else z + 1'		[State 136]
	(shift parses as 'if x then y else (z + 1)', as per longest-parse rule)
	10 because op might be: : - ! * . `x` VARSYM CONSYM QVARSYM QCONSYM

1 for ambiguity in 'if x then y else z with ?x=3' 	[State 136]
	(shift parses as 'if x then y else (z with ?x=3)'

1 for ambiguity in 'if x then y else z :: T'		[State 136]
	(shift parses as 'if x then y else (z :: T)', as per longest-parse rule)

8 for ambiguity in 'e :: a `b` c'.  Does this mean 	[States 160,246]
	(e::a) `b` c, or 
	(e :: (a `b` c))

1 for ambiguity in 'let ?x ...'				[State 268]
	the parser can't tell whether the ?x is the lhs of a normal binding or
	an implicit binding.  Fortunately resolving as shift gives it the only
	sensible meaning, namely the lhs of an implicit binding.

1 for ambiguity in '{-# RULES "name" [ ... #-}		[State 332]
	we don't know whether the '[' starts the activation or not: it
  	might be the start of the declaration with the activation being
	empty.  --SDM 1/4/2002

1 for ambiguity in '{-# RULES "name" forall = ... #-}' 	[State 394]
	since 'forall' is a valid variable name, we don't know whether
	to treat a forall on the input as the beginning of a quantifier
	or the beginning of the rule itself.  Resolving to shift means
	it's always treated as a quantifier, hence the above is disallowed.
	This saves explicitly defining a grammar for the rule lhs that
	doesn't include 'forall'.

6 for conflicts between `fdecl' and `fdeclDEPRECATED', 	[States 384,385]
  	which are resolved correctly, and moreover, 
  	should go away when `fdeclDEPRECATED' is removed.

-----------------------------------------------------------------------------
-}

%token
 '_'            { T _ _ ITunderscore }		-- Haskell keywords
 'as' 		{ T _ _ ITas }
 'case' 	{ T _ _ ITcase }  	
 'class' 	{ T _ _ ITclass } 
 'data' 	{ T _ _ ITdata } 
 'default' 	{ T _ _ ITdefault }
 'deriving' 	{ T _ _ ITderiving }
 'do' 		{ T _ _ ITdo }
 'else' 	{ T _ _ ITelse }
 'hiding' 	{ T _ _ IThiding }
 'if' 		{ T _ _ ITif }
 'import' 	{ T _ _ ITimport }
 'in' 		{ T _ _ ITin }
 'infix' 	{ T _ _ ITinfix }
 'infixl' 	{ T _ _ ITinfixl }
 'infixr' 	{ T _ _ ITinfixr }
 'instance' 	{ T _ _ ITinstance }
 'let' 		{ T _ _ ITlet }
 'module' 	{ T _ _ ITmodule }
 'newtype' 	{ T _ _ ITnewtype }
 'of' 		{ T _ _ ITof }
 'qualified' 	{ T _ _ ITqualified }
 'then' 	{ T _ _ ITthen }
 'type' 	{ T _ _ ITtype }
 'where' 	{ T _ _ ITwhere }
 '_scc_'	{ T _ _ ITscc }	      -- ToDo: remove

 'forall'	{ T _ _ ITforall }			-- GHC extension keywords
 'foreign'	{ T _ _ ITforeign }
 'export'	{ T _ _ ITexport }
 'label'	{ T _ _ ITlabel } 
 'dynamic'	{ T _ _ ITdynamic }
 'safe'		{ T _ _ ITsafe }
 'threadsafe'	{ T _ _ ITthreadsafe }
 'unsafe'	{ T _ _ ITunsafe }
 'mdo'		{ T _ _ ITmdo }
 'stdcall'      { T _ _ ITstdcallconv }
 'ccall'        { T _ _ ITccallconv }
 'dotnet'       { T _ _ ITdotnet }
 'proc'		{ T _ _ ITproc }		-- for arrow notation extension
 'rec'		{ T _ _ ITrec }		-- for arrow notation extension

 '{-# SPECIALISE'  { T _ _ ITspecialise_prag }
 '{-# SOURCE'	   { T _ _ ITsource_prag }
 '{-# INLINE'      { T _ _ ITinline_prag }
 '{-# NOINLINE'    { T _ _ ITnoinline_prag }
 '{-# RULES'	   { T _ _ ITrules_prag }
 '{-# CORE'        { T _ _ ITcore_prag }              -- hdaume: annotated core
 '{-# SCC'	   { T _ _ ITscc_prag }
 '{-# DEPRECATED'  { T _ _ ITdeprecated_prag }
 '{-# UNPACK'      { T _ _ ITunpack_prag }
 '#-}'		   { T _ _ ITclose_prag }

 '..'		{ T _ _ ITdotdot }  			-- reserved symbols
 ':'		{ T _ _ ITcolon }
 '::'		{ T _ _ ITdcolon }
 '='		{ T _ _ ITequal }
 '\\'		{ T _ _ ITlam }
 '|'		{ T _ _ ITvbar }
 '<-'		{ T _ _ ITlarrow }
 '->'		{ T _ _ ITrarrow }
 '@'		{ T _ _ ITat }
 '~'		{ T _ _ ITtilde }
 '=>'		{ T _ _ ITdarrow }
 '-'		{ T _ _ ITminus }
 '!'		{ T _ _ ITbang }
 '*'		{ T _ _ ITstar }
 '-<'		{ T _ _ ITlarrowtail }		-- for arrow notation
 '>-'		{ T _ _ ITrarrowtail }		-- for arrow notation
 '-<<'		{ T _ _ ITLarrowtail }		-- for arrow notation
 '>>-'		{ T _ _ ITRarrowtail }		-- for arrow notation
 '.'		{ T _ _ ITdot }

 '{'		{ T _ _ ITocurly } 			-- special symbols
 '}'		{ T _ _ ITccurly }
 '{|'           { T _ _ ITocurlybar }
 '|}'           { T _ _ ITccurlybar }
 vocurly	{ T _ _ ITvocurly } -- virtual open curly (from layout)
 vccurly	{ T _ _ ITvccurly } -- virtual close curly (from layout)
 '['		{ T _ _ ITobrack }
 ']'		{ T _ _ ITcbrack }
 '[:'		{ T _ _ ITopabrack }
 ':]'		{ T _ _ ITcpabrack }
 '('		{ T _ _ IToparen }
 ')'		{ T _ _ ITcparen }
 '(#'		{ T _ _ IToubxparen }
 '#)'		{ T _ _ ITcubxparen }
 '(|'		{ T _ _ IToparenbar }
 '|)'		{ T _ _ ITcparenbar }
 ';'		{ T _ _ ITsemi }
 ','		{ T _ _ ITcomma }
 '`'		{ T _ _ ITbackquote }

 VARID   	{ T _ _ (ITvarid    $$) }		-- identifiers
 CONID   	{ T _ _ (ITconid    $$) }
 VARSYM  	{ T _ _ (ITvarsym   $$) }
 CONSYM  	{ T _ _ (ITconsym   $$) }
 QVARID  	{ T _ _ (ITqvarid   $$) }
 QCONID  	{ T _ _ (ITqconid   $$) }
 QVARSYM 	{ T _ _ (ITqvarsym  $$) }
 QCONSYM 	{ T _ _ (ITqconsym  $$) }

 IPDUPVARID   	{ T _ _ (ITdupipvarid   $$) }		-- GHC extension
 IPSPLITVARID  	{ T _ _ (ITsplitipvarid $$) }		-- GHC extension

 CHAR		{ T _ _ (ITchar     $$) }
 STRING		{ T _ _ (ITstring   $$) }
 INTEGER	{ T _ _ (ITinteger  $$) }
 RATIONAL	{ T _ _ (ITrational $$) }

 PRIMCHAR	{ T _ _ (ITprimchar   $$) }
 PRIMSTRING	{ T _ _ (ITprimstring $$) }
 PRIMINTEGER	{ T _ _ (ITprimint    $$) }
 PRIMFLOAT	{ T _ _ (ITprimfloat  $$) }
 PRIMDOUBLE	{ T _ _ (ITprimdouble $$) }
 
-- Template Haskell
'[|'            { T _ _ ITopenExpQuote  }       
'[p|'           { T _ _ ITopenPatQuote  }      
'[t|'           { T _ _ ITopenTypQuote  }      
'[d|'           { T _ _ ITopenDecQuote  }      
'|]'            { T _ _ ITcloseQuote    }
ID_SPLICE       { T _ _ (ITidEscape $$) }     -- $x
'$('	        { T _ _ ITparenEscape   }     -- $( exp )
REIFY_TYPE	{ T _ _ ITreifyType }	
REIFY_DECL	{ T _ _ ITreifyDecl }	
REIFY_FIXITY	{ T _ _ ITreifyFixity }

%monad { P } { >>= } { return }
%lexer { lexer } { T _ _ ITeof }
%name parseModule module
%name parseStmt   maybe_stmt
%name parseIdentifier  identifier
%name parseIface iface
%tokentype { Token }
%%

-----------------------------------------------------------------------------
-- Module Header

-- The place for module deprecation is really too restrictive, but if it
-- was allowed at its natural place just before 'module', we get an ugly
-- s/r conflict with the second alternative. Another solution would be the
-- introduction of a new pragma DEPRECATED_MODULE, but this is not very nice,
-- either, and DEPRECATED is only expected to be used by people who really
-- know what they are doing. :-)

module 	:: { RdrNameHsModule }
 	: srcloc 'module' modid maybemoddeprec maybeexports 'where' body 
		{ HsModule (Just (mkHomeModule $3)) $5 (fst $7) (snd $7) $4 $1 }
	| srcloc missing_module_keyword top close
		{ HsModule Nothing Nothing (fst $3) (snd $3) Nothing $1 }

missing_module_keyword :: { () }
	: {- empty -}				{% pushCurrentContext }

maybemoddeprec :: { Maybe DeprecTxt }
	: '{-# DEPRECATED' STRING '#-}' 	{ Just $2 }
	|  {- empty -}				{ Nothing }

body 	:: { ([RdrNameImportDecl], [RdrNameHsDecl]) }
	:  '{'            top '}'		{ $2 }
 	|      vocurly    top close		{ $2 }

top 	:: { ([RdrNameImportDecl], [RdrNameHsDecl]) }
	: importdecls				{ (reverse $1,[]) }
	| importdecls ';' cvtopdecls		{ (reverse $1,$3) }
	| cvtopdecls				{ ([],$1) }

cvtopdecls :: { [RdrNameHsDecl] }
	: topdecls			{ cvTopDecls $1 }

-----------------------------------------------------------------------------
-- Interfaces (.hi-boot files)

iface   :: { ParsedIface }
	: 'module' modid 'where' ifacebody
	  {	    ParsedIface {
			pi_mod     = $2,
			pi_pkg     = opt_InPackage,
			pi_vers    = 1, 		-- Module version
			pi_orphan  = False,
			pi_exports = (1,[($2,mkIfaceExports $4)]),
			pi_deps    = noDependencies,
			pi_usages  = [],
			pi_fixity  = [],
			pi_insts   = [],
			pi_decls   = map (\x -> (1,x)) $4,
		 	pi_rules   = (1,[]),
		 	pi_deprecs = Nothing
	   	    }
	   }

ifacebody :: { [RdrNameTyClDecl] }
	:  '{'            ifacedecls '}'		{ $2 }
 	|      vocurly    ifacedecls close		{ $2 }

ifacedecls :: { [RdrNameTyClDecl] }
	: ifacedecl ';' ifacedecls	{ $1 : $3 }
	| ';' ifacedecls		{ $2 }
	| ifacedecl			{ [$1] }
	| {- empty -}			{ [] }

ifacedecl :: { RdrNameTyClDecl }
	: tycl_decl			{ $1 }
	| srcloc var '::' sigtype	{ IfaceSig $2 $4 [] $1 }

-----------------------------------------------------------------------------
-- The Export List

maybeexports :: { Maybe [RdrNameIE] }
	:  '(' exportlist ')'			{ Just $2 }
	|  {- empty -}				{ Nothing }

exportlist :: { [RdrNameIE] }
 	:  exportlist ',' export		{ $3 : $1 }
	|  exportlist ','			{ $1 }
 	|  export				{ [$1]  }
	|  {- empty -}				{ [] }

   -- No longer allow things like [] and (,,,) to be exported
   -- They are built in syntax, always available
export 	:: { RdrNameIE }
	:  qvar					{ IEVar $1 }
	|  oqtycon				{ IEThingAbs $1 }
	|  oqtycon '(' '..' ')'			{ IEThingAll $1 }
	|  oqtycon '(' ')'		        { IEThingWith $1 [] }
	|  oqtycon '(' qcnames ')'		{ IEThingWith $1 (reverse $3) }
	|  'module' modid			{ IEModuleContents $2 }

qcnames :: { [RdrName] }
	:  qcnames ',' qcname			{ $3 : $1 }
	|  qcname				{ [$1]  }

qcname 	:: { RdrName }	-- Variable or data constructor
	:  qvar					{ $1 }
	|  gcon					{ $1 }

-----------------------------------------------------------------------------
-- Import Declarations

-- import decls can be *empty*, or even just a string of semicolons
-- whereas topdecls must contain at least one topdecl.

importdecls :: { [RdrNameImportDecl] }
	: importdecls ';' importdecl		{ $3 : $1 }
	| importdecls ';'			{ $1 }
	| importdecl				{ [ $1 ] }
	| {- empty -}				{ [] }

importdecl :: { RdrNameImportDecl }
	: 'import' srcloc maybe_src optqualified modid maybeas maybeimpspec 
		{ ImportDecl $5 $3 $4 $6 $7 $2 }

maybe_src :: { IsBootInterface }
	: '{-# SOURCE' '#-}'			{ True }
	| {- empty -}				{ False }

optqualified :: { Bool }
      	: 'qualified'                           { True  }
      	| {- empty -}				{ False }

maybeas :: { Maybe ModuleName }
      	: 'as' modid                            { Just $2 }
      	| {- empty -}				{ Nothing }

maybeimpspec :: { Maybe (Bool, [RdrNameIE]) }
	: impspec				{ Just $1 }
	| {- empty -}				{ Nothing }

impspec :: { (Bool, [RdrNameIE]) }
	:  '(' exportlist ')'  			{ (False, reverse $2) }
	|  'hiding' '(' exportlist ')' 		{ (True,  reverse $3) }

-----------------------------------------------------------------------------
-- Fixity Declarations

prec 	:: { Int }
	: {- empty -}				{ 9 }
	| INTEGER				{% checkPrecP (fromInteger $1) }

infix 	:: { FixityDirection }
	: 'infix'				{ InfixN  }
	| 'infixl'				{ InfixL  }
	| 'infixr'				{ InfixR }

ops   	:: { [RdrName] }
	: ops ',' op				{ $3 : $1 }
	| op					{ [$1] }

-----------------------------------------------------------------------------
-- Top-Level Declarations

topdecls :: { [RdrBinding] }	-- Reversed
	: topdecls ';' topdecl		{ $3 : $1 }
	| topdecls ';'			{ $1 }
	| topdecl			{ [$1] }

topdecl :: { RdrBinding }
  	: tycl_decl			{ RdrHsDecl (TyClD $1) }
	| srcloc 'instance' inst_type where
		{ let (binds,sigs) = cvMonoBindsAndSigs $4
		  in RdrHsDecl (InstD (InstDecl $3 binds sigs Nothing $1)) }
	| srcloc 'default' '(' comma_types0 ')'		{ RdrHsDecl (DefD (DefaultDecl $4 $1)) }
	| 'foreign' fdecl				{ RdrHsDecl $2 }
	| '{-# DEPRECATED' deprecations '#-}'	 	{ RdrBindings (reverse $2) }
	| '{-# RULES' rules '#-}'		 	{ RdrBindings (reverse $2) }
	| srcloc '$(' exp ')'				{ RdrHsDecl (SpliceD (SpliceDecl $3 $1)) }
      	| decl						{ $1 }

tycl_decl :: { RdrNameTyClDecl }
 	: srcloc 'type' syn_hdr '=' ctype	
		-- Note ctype, not sigtype.
		-- We allow an explicit for-all but we don't insert one
		-- in 	type Foo a = (b,b)
		-- Instead we just say b is out of scope
 		{ let (tc,tvs) = $3 in TySynonym tc tvs $5 $1 }


	| srcloc 'data' tycl_hdr constrs deriving
		{ mkTyData DataType $3 (DataCons (reverse $4)) $5 $1 }

	| srcloc 'newtype' tycl_hdr '=' newconstr deriving
		{ mkTyData NewType $3 (DataCons [$5]) $6 $1 }

	| srcloc 'class' tycl_hdr fds where
		{ let 
			(binds,sigs) = cvMonoBindsAndSigs $5 
		  in
	 	  mkClassDecl $3 $4 sigs (Just binds) $1 }

syn_hdr :: { (RdrName, [RdrNameHsTyVar]) }	-- We don't retain the syntax of an infix
						-- type synonym declaration. Oh well.
	: tycon tv_bndrs		{ ($1, $2) }
	| tv_bndr tyconop tv_bndr 	{ ($2, [$1,$3]) }

-- tycl_hdr parses the header of a type or class decl,
-- which takes the form
--	T a b
-- 	Eq a => T a
--	(Eq a, Ord b) => T a b
-- Rather a lot of inlining here, else we get reduce/reduce errors
tycl_hdr :: { (RdrNameContext, RdrName, [RdrNameHsTyVar]) }
	: context '=>' type		{% checkTyClHdr $3	>>= \ (tc,tvs) ->
					   return ($1, tc, tvs) }
	| type				{% checkTyClHdr $1	>>= \ (tc,tvs) ->
					   return ([], tc, tvs) }

-----------------------------------------------------------------------------
-- Nested declarations

decls 	:: { [RdrBinding] }	-- Reversed
	: decls ';' decl		{ $3 : $1 }
	| decls ';'			{ $1 }
	| decl				{ [$1] }
	| {- empty -}			{ [] }


decllist :: { [RdrBinding] }	-- Reversed
	: '{'            decls '}'	{ $2 }
	|     vocurly    decls close	{ $2 }

where 	:: { [RdrBinding] }	-- Reversed
				-- No implicit parameters
	: 'where' decllist		{ $2 }
	| {- empty -}			{ [] }

binds 	::  { RdrNameHsBinds }	-- May have implicit parameters
	: decllist			{ cvBinds $1 }
	| '{'            dbinds '}'	{ IPBinds $2 }
	|     vocurly    dbinds close	{ IPBinds $2 }

wherebinds :: { RdrNameHsBinds }	-- May have implicit parameters
	: 'where' binds			{ $2 }
	| {- empty -}			{ EmptyBinds }



-----------------------------------------------------------------------------
-- Transformation Rules

rules	:: { [RdrBinding] }	-- Reversed
	:  rules ';' rule			{ $3 : $1 }
        |  rules ';'				{ $1 }
        |  rule					{ [$1] }
	|  {- empty -}				{ [] }

rule  	:: { RdrBinding }
	: STRING activation rule_forall infixexp '=' srcloc exp
	     { RdrHsDecl (RuleD (HsRule $1 $2 $3 $4 $7 $6)) }

activation :: { Activation }           -- Omitted means AlwaysActive
        : {- empty -}                           { AlwaysActive }
        | explicit_activation                   { $1 }

inverse_activation :: { Activation }   -- Omitted means NeverActive
        : {- empty -}                           { NeverActive }
        | explicit_activation                   { $1 }

explicit_activation :: { Activation }  -- In brackets
        : '[' INTEGER ']'                       { ActiveAfter  (fromInteger $2) }
        | '[' '~' INTEGER ']'                   { ActiveBefore (fromInteger $3) }

rule_forall :: { [RdrNameRuleBndr] }
	: 'forall' rule_var_list '.'            { $2 }
        | {- empty -}				{ [] }

rule_var_list :: { [RdrNameRuleBndr] }
        : rule_var				{ [$1] }
        | rule_var rule_var_list		{ $1 : $2 }

rule_var :: { RdrNameRuleBndr }
	: varid                              	{ RuleBndr $1 }
       	| '(' varid '::' ctype ')'             	{ RuleBndrSig $2 $4 }

-----------------------------------------------------------------------------
-- Deprecations (c.f. rules)

deprecations :: { [RdrBinding] }	-- Reversed
	: deprecations ';' deprecation		{ $3 : $1 }
	| deprecations ';' 			{ $1 }
	| deprecation				{ [$1] }
	| {- empty -}				{ [] }

-- SUP: TEMPORARY HACK, not checking for `module Foo'
deprecation :: { RdrBinding }
	: srcloc depreclist STRING
		{ RdrBindings
			[ RdrHsDecl (DeprecD (Deprecation n $3 $1)) | n <- $2 ] }


-----------------------------------------------------------------------------
-- Foreign import and export declarations

-- for the time being, the following accepts foreign declarations conforming
-- to the FFI Addendum, Version 1.0 as well as pre-standard declarations
--
-- * a flag indicates whether pre-standard declarations have been used and
--   triggers a deprecation warning further down the road
--
-- NB: The first two rules could be combined into one by replacing `safety1'
--     with `safety'.  However, the combined rule conflicts with the
--     DEPRECATED rules.
--
fdecl :: { RdrNameHsDecl }
fdecl : srcloc 'import' callconv safety1 fspec	{% mkImport $3 $4       $5 $1 }
      | srcloc 'import' callconv         fspec	{% mkImport $3 (PlaySafe False) $4 $1 }
      | srcloc 'export'	callconv         fspec  {% mkExport $3          $4 $1 }
        -- the following syntax is DEPRECATED
      | srcloc fdecl1DEPRECATED			{ ForD ($2 True $1) }
      | srcloc fdecl2DEPRECATED			{ $2 $1 }

fdecl1DEPRECATED :: { Bool -> SrcLoc -> ForeignDecl RdrName }
fdecl1DEPRECATED 
  ----------- DEPRECATED label decls ------------
  : 'label' ext_name varid '::' sigtype
    { ForeignImport $3 $5 (CImport defaultCCallConv (PlaySafe False) nilFS nilFS 
				   (CLabel ($2 `orElse` mkExtName $3))) }

  ----------- DEPRECATED ccall/stdcall decls ------------
  --
  -- NB: This business with the case expression below may seem overly
  --	 complicated, but it is necessary to avoid some conflicts.

    -- DEPRECATED variant #1: lack of a calling convention specification
    --			      (import) 
  | 'import' {-no callconv-} ext_name safety varid_no_unsafe '::' sigtype
    { let
	target = StaticTarget ($2 `orElse` mkExtName $4)
      in
      ForeignImport $4 $6 (CImport defaultCCallConv $3 nilFS nilFS 
				   (CFunction target)) }

    -- DEPRECATED variant #2: external name consists of two separate strings
    --			      (module name and function name) (import)
  | 'import' callconv STRING STRING safety varid_no_unsafe '::' sigtype
    {% case $2 of
         DNCall      -> parseError "Illegal format of .NET foreign import"
	 CCall cconv -> return $
           let
	     imp = CFunction (StaticTarget $4)
	   in
	   ForeignImport $6 $8 (CImport cconv $5 nilFS nilFS imp) }

    -- DEPRECATED variant #3: `unsafe' after entity
  | 'import' callconv STRING 'unsafe' varid_no_unsafe '::' sigtype
    {% case $2 of
         DNCall      -> parseError "Illegal format of .NET foreign import"
	 CCall cconv -> return $
           let
	     imp = CFunction (StaticTarget $3)
	   in
	   ForeignImport $5 $7 (CImport cconv PlayRisky nilFS nilFS imp) }

    -- DEPRECATED variant #4: use of the special identifier `dynamic' without
    --			      an explicit calling convention (import)
  | 'import' {-no callconv-} 'dynamic' safety varid_no_unsafe '::' sigtype
    { ForeignImport $4 $6 (CImport defaultCCallConv $3 nilFS nilFS 
				   (CFunction DynamicTarget)) }

    -- DEPRECATED variant #5: use of the special identifier `dynamic' (import)
  | 'import' callconv 'dynamic' safety varid_no_unsafe '::' sigtype
    {% case $2 of
         DNCall      -> parseError "Illegal format of .NET foreign import"
	 CCall cconv -> return $
	   ForeignImport $5 $7 (CImport cconv $4 nilFS nilFS 
					(CFunction DynamicTarget)) }

    -- DEPRECATED variant #6: lack of a calling convention specification
    --			      (export) 
  | 'export' {-no callconv-} ext_name varid '::' sigtype
    { ForeignExport $3 $5 (CExport (CExportStatic ($2 `orElse` mkExtName $3) 
				   defaultCCallConv)) }

    -- DEPRECATED variant #7: external name consists of two separate strings
    --			      (module name and function name) (export)
  | 'export' callconv STRING STRING varid '::' sigtype
    {% case $2 of
         DNCall      -> parseError "Illegal format of .NET foreign import"
	 CCall cconv -> return $
           ForeignExport $5 $7 
			 (CExport (CExportStatic $4 cconv)) }

    -- DEPRECATED variant #8: use of the special identifier `dynamic' without
    --			      an explicit calling convention (export)
  | 'export' {-no callconv-} 'dynamic' varid '::' sigtype
    { ForeignImport $3 $5 (CImport defaultCCallConv (PlaySafe False) nilFS nilFS 
				   CWrapper) }

    -- DEPRECATED variant #9: use of the special identifier `dynamic' (export)
  | 'export' callconv 'dynamic' varid '::' sigtype
    {% case $2 of
         DNCall      -> parseError "Illegal format of .NET foreign import"
	 CCall cconv -> return $
	   ForeignImport $4 $6 (CImport cconv (PlaySafe False) nilFS nilFS CWrapper) }

  ----------- DEPRECATED .NET decls ------------
  -- NB: removed the .NET call declaration, as it is entirely subsumed
  --     by the new standard FFI declarations

fdecl2DEPRECATED :: { SrcLoc -> RdrNameHsDecl }
fdecl2DEPRECATED 
  : 'import' 'dotnet' 'type' ext_name tycon
	  { \loc -> TyClD (ForeignType $5 $4 DNType loc) }
    -- left this one unchanged for the moment as type imports are not
    -- covered currently by the FFI standard -=chak


callconv :: { CallConv }
	  : 'stdcall'			{ CCall  StdCallConv }
	  | 'ccall'			{ CCall  CCallConv   }
	  | 'dotnet'			{ DNCall	     }

safety :: { Safety }
	: 'unsafe'			{ PlayRisky }
	| 'safe'			{ PlaySafe False }
	| 'threadsafe'			{ PlaySafe True  }
	| {- empty -}			{ PlaySafe False }

safety1 :: { Safety }
	: 'unsafe'			{ PlayRisky }
	| 'safe'			{ PlaySafe  False }
	| 'threadsafe'			{ PlaySafe  True }
	  -- only needed to avoid conflicts with the DEPRECATED rules

fspec :: { (FastString, RdrName, RdrNameHsType) }
       : STRING var '::' sigtype      { ($1      , $2, $4) }
       |        var '::' sigtype      { (nilFS, $1, $3) }
         -- if the entity string is missing, it defaults to the empty string;
         -- the meaning of an empty entity string depends on the calling
         -- convention

-- DEPRECATED syntax
ext_name :: { Maybe CLabelString }
	: STRING		{ Just $1 }
	| STRING STRING		{ Just $2 }	-- Ignore "module name" for now
	| {- empty -}           { Nothing }


-----------------------------------------------------------------------------
-- Type signatures

opt_sig :: { Maybe RdrNameHsType }
	: {- empty -}			{ Nothing }
	| '::' sigtype			{ Just $2 }

opt_asig :: { Maybe RdrNameHsType }
	: {- empty -}			{ Nothing }
	| '::' atype			{ Just $2 }

sigtypes :: { [RdrNameHsType] }
	: sigtype			{ [ $1 ] }
	| sigtypes ',' sigtype		{ $3 : $1 }

sigtype :: { RdrNameHsType }
	: ctype				{ mkHsForAllTy Nothing [] $1 }

sig_vars :: { [RdrName] }
	 : sig_vars ',' var		{ $3 : $1 }
	 | var				{ [ $1 ] }

-----------------------------------------------------------------------------
-- Types

-- A ctype is a for-all type
ctype	:: { RdrNameHsType }
	: 'forall' tv_bndrs '.' ctype	{ mkHsForAllTy (Just $2) [] $4 }
	| context '=>' type		{ mkHsForAllTy Nothing   $1 $3 }
	-- A type of form (context => type) is an *implicit* HsForAllTy
	| type				{ $1 }

-- We parse a context as a btype so that we don't get reduce/reduce
-- errors in ctype.  The basic problem is that
--	(Eq a, Ord a)
-- looks so much like a tuple type.  We can't tell until we find the =>
context :: { RdrNameContext }
	: btype 			{% checkContext $1 }

type :: { RdrNameHsType }
	: ipvar '::' gentype		{ mkHsIParamTy $1 $3 }
	| gentype			{ $1 }

gentype :: { RdrNameHsType }
        : btype                         { $1 }
        | btype qtyconop gentype        { HsOpTy $1 (HsTyOp $2) $3 }
        | btype  '`' tyvar '`' gentype  { HsOpTy $1 (HsTyOp $3) $5 }
	| btype '->' gentype		{ HsOpTy $1 HsArrow $3 }

btype :: { RdrNameHsType }
	: btype atype			{ HsAppTy $1 $2 }
	| atype				{ $1 }

atype :: { RdrNameHsType }
	: gtycon			{ HsTyVar $1 }
	| tyvar				{ HsTyVar $1 }
	| '(' type ',' comma_types1 ')'	{ HsTupleTy (mkHsTupCon tcName Boxed  ($2:$4)) ($2:$4) }
	| '(#' comma_types1 '#)'	{ HsTupleTy (mkHsTupCon tcName Unboxed     $2) $2      }
	| '[' type ']'			{ HsListTy  $2 }
	| '[:' type ':]'		{ HsPArrTy  $2 }
	| '(' ctype ')'		        { HsParTy   $2 }
	| '(' ctype '::' kind ')'	{ HsKindSig $2 $4 }
-- Generics
        | INTEGER                       { HsNumTy $1 }

-- An inst_type is what occurs in the head of an instance decl
--	e.g.  (Foo a, Gaz b) => Wibble a b
-- It's kept as a single type, with a MonoDictTy at the right
-- hand corner, for convenience.
inst_type :: { RdrNameHsType }
	: ctype				{% checkInstType $1 }

comma_types0  :: { [RdrNameHsType] }
	: comma_types1			{ $1 }
	| {- empty -}			{ [] }

comma_types1	:: { [RdrNameHsType] }
	: type				{ [$1] }
	| type  ',' comma_types1	{ $1 : $3 }

tv_bndrs :: { [RdrNameHsTyVar] }
	 : tv_bndr tv_bndrs		{ $1 : $2 }
	 | {- empty -}			{ [] }

tv_bndr :: { RdrNameHsTyVar }
	: tyvar				{ UserTyVar $1 }
	| '(' tyvar '::' kind ')'	{ IfaceTyVar $2 $4 }

fds :: { [([RdrName], [RdrName])] }
	: {- empty -}			{ [] }
	| '|' fds1			{ reverse $2 }

fds1 :: { [([RdrName], [RdrName])] }
	: fds1 ',' fd			{ $3 : $1 }
	| fd				{ [$1] }

fd :: { ([RdrName], [RdrName]) }
	: varids0 '->' varids0		{ (reverse $1, reverse $3) }

varids0	:: { [RdrName] }
	: {- empty -}			{ [] }
	| varids0 tyvar			{ $2 : $1 }

-----------------------------------------------------------------------------
-- Kinds

kind	:: { Kind }
	: akind			{ $1 }
	| akind '->' kind	{ mkArrowKind $1 $3 }

akind	:: { Kind }
	: '*'			{ liftedTypeKind }
	| '(' kind ')'		{ $2 }


-----------------------------------------------------------------------------
-- Datatype declarations

newconstr :: { RdrNameConDecl }
	: srcloc conid atype	{ ConDecl $2 [] [] (PrefixCon [unbangedType $3]) $1 }
	| srcloc conid '{' var '::' ctype '}'
				{ ConDecl $2 [] [] (RecCon [($4, unbangedType $6)]) $1 }

constrs :: { [RdrNameConDecl] }
        : {- empty; a GHC extension -}  { [] }
        | '=' constrs1                  { $2 }

constrs1 :: { [RdrNameConDecl] }
	: constrs1 '|' constr		{ $3 : $1 }
	| constr			{ [$1] }

constr :: { RdrNameConDecl }
	: srcloc forall context '=>' constr_stuff
		{ ConDecl (fst $5) $2 $3 (snd $5) $1 }
	| srcloc forall constr_stuff
		{ ConDecl (fst $3) $2 [] (snd $3) $1 }

forall :: { [RdrNameHsTyVar] }
	: 'forall' tv_bndrs '.'		{ $2 }
	| {- empty -}			{ [] }

constr_stuff :: { (RdrName, RdrNameConDetails) }
	: btype				{% mkPrefixCon $1 [] }
	| btype strict_mark atype satypes {% mkPrefixCon $1 (BangType $2 $3 : $4) }
	| oqtycon '{' '}' 		{% mkRecCon $1 [] }
	| oqtycon '{' fielddecls '}' 	{% mkRecCon $1 $3 }
	| sbtype conop sbtype		{ ($2, InfixCon $1 $3) }

satypes	:: { [RdrNameBangType] }
	: atype satypes			{ unbangedType $1 : $2 }
	| strict_mark atype satypes	{ BangType $1 $2 : $3 }
	| {- empty -}			{ [] }

sbtype :: { RdrNameBangType }
	: btype				{ unbangedType $1 }
	| strict_mark atype		{ BangType $1 $2 }

fielddecls :: { [([RdrName],RdrNameBangType)] }
	: fielddecl ',' fielddecls	{ $1 : $3 }
	| fielddecl			{ [$1] }

fielddecl :: { ([RdrName],RdrNameBangType) }
	: sig_vars '::' stype		{ (reverse $1, $3) }

stype :: { RdrNameBangType }
	: ctype				{ unbangedType $1 }
	| strict_mark atype		{ BangType $1 $2 }

strict_mark :: { StrictnessMark }
	: '!'				{ MarkedUserStrict }
	| '{-# UNPACK' '#-}' '!'	{ MarkedUserUnboxed }

deriving :: { Maybe RdrNameContext }
	: {- empty -}			{ Nothing }
	| 'deriving' context		{ Just $2 }
             -- Glasgow extension: allow partial 
             -- applications in derivings

-----------------------------------------------------------------------------
-- Value definitions

{- There's an awkward overlap with a type signature.  Consider
	f :: Int -> Int = ...rhs...
   Then we can't tell whether it's a type signature or a value
   definition with a result signature until we see the '='.
   So we have to inline enough to postpone reductions until we know.
-}

{-
  ATTENTION: Dirty Hackery Ahead! If the second alternative of vars is var
  instead of qvar, we get another shift/reduce-conflict. Consider the
  following programs:
  
     { (^^) :: Int->Int ; }          Type signature; only var allowed

     { (^^) :: Int->Int = ... ; }    Value defn with result signature;
				     qvar allowed (because of instance decls)
  
  We can't tell whether to reduce var to qvar until after we've read the signatures.
-}

decl 	:: { RdrBinding }
	: sigdecl			{ $1 }
	| infixexp srcloc opt_sig rhs	{% checkValDef $1 $3 $4 $2 }

rhs	:: { RdrNameGRHSs }
	: '=' srcloc exp wherebinds	{ GRHSs (unguardedRHS $3 $2) $4 placeHolderType }
	| gdrhs	wherebinds		{ GRHSs (reverse $1)         $2 placeHolderType }

gdrhs :: { [RdrNameGRHS] }
	: gdrhs gdrh			{ $2 : $1 }
	| gdrh				{ [$1] }

gdrh :: { RdrNameGRHS }
	: '|' srcloc quals '=' exp  	{ GRHS (reverse (ResultStmt $5 $2 : $3)) $2 }

sigdecl :: { RdrBinding }
	: infixexp srcloc '::' sigtype		
				{% checkValSig $1 $4 $2 }
		-- See the above notes for why we need infixexp here
	| var ',' sig_vars srcloc '::' sigtype	
				{ mkSigDecls [ Sig n $6 $4 | n <- $1:$3 ] }
	| srcloc infix prec ops	{ mkSigDecls [ FixSig (FixitySig n (Fixity $3 $2) $1)
					     | n <- $4 ] }
	| '{-# INLINE'   srcloc activation qvar '#-}'	      
				{ RdrHsDecl (SigD (InlineSig True  $4 $3 $2)) }
	| '{-# NOINLINE' srcloc inverse_activation qvar '#-}' 
				{ RdrHsDecl (SigD (InlineSig False $4 $3 $2)) }
	| '{-# SPECIALISE' srcloc qvar '::' sigtypes '#-}'
			 	{ mkSigDecls  [ SpecSig $3 t $2 | t <- $5] }
	| '{-# SPECIALISE' srcloc 'instance' inst_type '#-}'
				{ RdrHsDecl (SigD (SpecInstSig $4 $2)) }

-----------------------------------------------------------------------------
-- Expressions

exp   :: { RdrNameHsExpr }
	: infixexp '::' sigtype		{ ExprWithTySig $1 $3 }
	| fexp srcloc '-<' exp		{ HsArrApp $1 $4 placeHolderType HsFirstOrderApp True $2 }
	| fexp srcloc '>-' exp		{ HsArrApp $4 $1 placeHolderType HsFirstOrderApp False $2 }
	| fexp srcloc '-<<' exp		{ HsArrApp $1 $4 placeHolderType HsHigherOrderApp True $2 }
	| fexp srcloc '>>-' exp		{ HsArrApp $4 $1 placeHolderType HsHigherOrderApp False $2 }
	| infixexp			{ $1 }

infixexp :: { RdrNameHsExpr }
	: exp10				{ $1 }
	| infixexp qop exp10		{ (OpApp $1 (HsVar $2) 
						(panic "fixity") $3 )}

exp10 :: { RdrNameHsExpr }
	: '\\' srcloc aexp aexps opt_asig '->' srcloc exp	
			{% checkPatterns $2 ($3 : reverse $4) >>= \ ps -> 
			   return (HsLam (Match ps $5 
					    (GRHSs (unguardedRHS $8 $7) 
						   EmptyBinds placeHolderType))) }
  	| 'let' binds 'in' exp			{ HsLet $2 $4 }
	| 'if' srcloc exp 'then' exp 'else' exp { HsIf $3 $5 $7 $2 }
   	| 'case' srcloc exp 'of' altslist	{ HsCase $3 $5 $2 }
	| '-' fexp				{ mkHsNegApp $2 }
  	| srcloc 'do' stmtlist			{% checkDo $3  >>= \ stmts ->
						   return (mkHsDo DoExpr stmts $1) }
  	| srcloc 'mdo' stmtlist			{% checkMDo $3  >>= \ stmts ->
						   return (mkHsDo MDoExpr stmts $1) }

        | scc_annot exp		    		{ if opt_SccProfilingOn
							then HsSCC $1 $2
							else HsPar $2 }

	| 'proc' srcloc aexp '->' srcloc exp	
			{% checkPattern $2 $3 >>= \ p -> 
			   return (HsProc p (HsCmdTop $6 [] placeHolderType undefined) $5) }

        | '{-# CORE' STRING '#-}' exp           { HsCoreAnn $2 $4 }    -- hdaume: core annotation

	| reifyexp				{ HsReify $1 }
	| fexp					{ $1 }

scc_annot :: { FastString }
	: '_scc_' STRING			{ $2 }
	| '{-# SCC' STRING '#-}'		{ $2 }

ccallid :: { FastString }
	:  VARID				{ $1 }
	|  CONID				{ $1 }

fexp 	:: { RdrNameHsExpr }
	: fexp aexp				{ (HsApp $1 $2) }
  	| aexp					{ $1 }

reifyexp :: { HsReify RdrName }
	: REIFY_DECL gtycon  			{ Reify ReifyDecl $2 }
	| REIFY_DECL qvar			{ Reify ReifyDecl $2 }
	| REIFY_TYPE qcname			{ Reify ReifyType $2 }
	| REIFY_FIXITY qcname			{ Reify ReifyFixity $2 }

aexps0 	:: { [RdrNameHsExpr] }
	: aexps					{ reverse $1 }

aexps 	:: { [RdrNameHsExpr] }
	: aexps aexp				{ $2 : $1 }
  	| {- empty -}				{ [] }

aexp	:: { RdrNameHsExpr }
	: qvar '@' aexp			{ EAsPat $1 $3 }
	| '~' aexp			{ ELazyPat $2 }
	| aexp1				{ $1 }

aexp1	:: { RdrNameHsExpr }
        : aexp1 '{' fbinds '}'	 	{% (mkRecConstrOrUpdate $1 (reverse $3)) }
  	| aexp2				{ $1 }

-- Here was the syntax for type applications that I was planning
-- but there are difficulties (e.g. what order for type args)
-- so it's not enabled yet.
 	| qcname '{|' gentype '|}'          { (HsApp (HsVar $1) (HsType $3)) }

aexp2	:: { RdrNameHsExpr }
	: ipvar				{ HsIPVar $1 }
	| qcname			{ HsVar $1 }
	| literal			{ HsLit $1 }
	| INTEGER			{ HsOverLit $! mkHsIntegral $1 }
	| RATIONAL			{ HsOverLit $! mkHsFractional $1 }
	| '(' exp ')'			{ HsPar $2 }
	| '(' exp ',' texps ')'		{ ExplicitTuple ($2 : reverse $4) Boxed}
	| '(#' texps '#)'		{ ExplicitTuple (reverse $2)      Unboxed }
	| '[' list ']'                  { $2 }
	| '[:' parr ':]'                { $2 }
	| '(' infixexp qop ')'		{ (SectionL $2 (HsVar $3))  }
	| '(' qopm infixexp ')'		{ (SectionR $2 $3) }
	| '_'				{ EWildPat }
	
	-- MetaHaskell Extension
	| srcloc ID_SPLICE              { mkHsSplice (HsVar (mkUnqual varName $2)) $1 }  -- $x
	| srcloc '$(' exp ')'   	{ mkHsSplice $3 $1 }                             -- $( exp )
	| srcloc '[|' exp '|]'          { HsBracket (ExpBr $3) $1 }                       
	| srcloc '[t|' ctype '|]'       { HsBracket (TypBr $3) $1 }                       
	| srcloc '[p|' infixexp '|]'    {% checkPattern $1 $3 >>= \p ->
					   return (HsBracket (PatBr p) $1) }
	| srcloc '[d|' cvtopbody '|]'	{ HsBracket (DecBr (mkGroup $3)) $1 }

	-- arrow notation extension
	| srcloc '(|' aexp2 cmdargs '|)'
					{ HsArrForm $3 Nothing (reverse $4) $1 }

cmdargs	:: { [RdrNameHsCmdTop] }
	: cmdargs acmd			{ $2 : $1 }
  	| {- empty -}			{ [] }

acmd	:: { RdrNameHsCmdTop }
	: aexp2				{ HsCmdTop $1 [] placeHolderType undefined }

cvtopbody :: { [RdrNameHsDecl] }
	:  '{'            cvtopdecls '}'		{ $2 }
	|      vocurly    cvtopdecls close		{ $2 }

texps :: { [RdrNameHsExpr] }
	: texps ',' exp			{ $3 : $1 }
	| exp				{ [$1] }


-----------------------------------------------------------------------------
-- List expressions

-- The rules below are little bit contorted to keep lexps left-recursive while
-- avoiding another shift/reduce-conflict.

list :: { RdrNameHsExpr }
	: exp				{ ExplicitList placeHolderType [$1] }
	| lexps 			{ ExplicitList placeHolderType (reverse $1) }
	| exp '..'			{ ArithSeqIn (From $1) }
	| exp ',' exp '..' 		{ ArithSeqIn (FromThen $1 $3) }
	| exp '..' exp	 		{ ArithSeqIn (FromTo $1 $3) }
	| exp ',' exp '..' exp		{ ArithSeqIn (FromThenTo $1 $3 $5) }
	| exp srcloc pquals		{ mkHsDo ListComp
						 (reverse (ResultStmt $1 $2 : $3))
						 $2
					}

lexps :: { [RdrNameHsExpr] }
	: lexps ',' exp 		{ $3 : $1 }
	| exp ',' exp			{ [$3,$1] }

-----------------------------------------------------------------------------
-- List Comprehensions

pquals :: { [RdrNameStmt] }	-- Either a singleton ParStmt, or a reversed list of Stmts
	: pquals1			{ case $1 of
					    [qs] -> qs
					    qss  -> [ParStmt stmtss]
						 where
						    stmtss = [ (reverse qs, undefined) 
						    	     | qs <- qss ]
					}
			
pquals1 :: { [[RdrNameStmt]] }
	: pquals1 '|' quals		{ $3 : $1 }
	| '|' quals			{ [$2] }

quals :: { [RdrNameStmt] }
	: quals ',' qual		{ $3 : $1 }
	| qual				{ [$1] }

-----------------------------------------------------------------------------
-- Parallel array expressions

-- The rules below are little bit contorted; see the list case for details.
-- Note that, in contrast to lists, we only have finite arithmetic sequences.
-- Moreover, we allow explicit arrays with no element (represented by the nil
-- constructor in the list case).

parr :: { RdrNameHsExpr }
	: 				{ ExplicitPArr placeHolderType [] }
	| exp				{ ExplicitPArr placeHolderType [$1] }
	| lexps 			{ ExplicitPArr placeHolderType 
						       (reverse $1) }
	| exp '..' exp	 		{ PArrSeqIn (FromTo $1 $3) }
	| exp ',' exp '..' exp		{ PArrSeqIn (FromThenTo $1 $3 $5) }
	| exp srcloc pquals		{  mkHsDo PArrComp 
						  (reverse (ResultStmt $1 $2 : $3))
						  $2
					}

-- We are reusing `lexps' and `pquals' from the list case.

-----------------------------------------------------------------------------
-- Case alternatives

altslist :: { [RdrNameMatch] }
	: '{'            alts '}'	{ reverse $2 }
	|     vocurly    alts  close	{ reverse $2 }

alts    :: { [RdrNameMatch] }
        : alts1				{ $1 }
	| ';' alts			{ $2 }

alts1 	:: { [RdrNameMatch] }
	: alts1 ';' alt			{ $3 : $1 }
	| alts1 ';'			{ $1 }
	| alt				{ [$1] }

alt 	:: { RdrNameMatch }
	: srcloc infixexp opt_sig ralt wherebinds
					{% (checkPattern $1 $2 >>= \p ->
				   	   return (Match [p] $3
					             (GRHSs $4 $5 placeHolderType))  )}

ralt :: { [RdrNameGRHS] }
	: '->' srcloc exp		{ [GRHS [ResultStmt $3 $2] $2] }
	| gdpats			{ reverse $1 }

gdpats :: { [RdrNameGRHS] }
	: gdpats gdpat			{ $2 : $1 }
	| gdpat				{ [$1] }

gdpat	:: { RdrNameGRHS }
	: srcloc '|' quals '->' exp 	{ GRHS (reverse (ResultStmt $5 $1:$3)) $1}

-----------------------------------------------------------------------------
-- Statement sequences

stmtlist :: { [RdrNameStmt] }
	: '{'         	stmts '}'	{ $2 }
	|     vocurly   stmts close	{ $2 }

--	do { ;; s ; s ; ; s ;; }
-- The last Stmt should be a ResultStmt, but that's hard to enforce
-- here, because we need too much lookahead if we see do { e ; }
-- So we use ExprStmts throughout, and switch the last one over
-- in ParseUtils.checkDo instead
stmts :: { [RdrNameStmt] }
	: stmt stmts_help		{ $1 : $2 }
	| ';' stmts			{ $2 }
	| {- empty -}			{ [] }

stmts_help :: { [RdrNameStmt] }
	: ';' stmts			{ $2 }
	| {- empty -}			{ [] }

-- For typing stmts at the GHCi prompt, where 
-- the input may consist of just comments.
maybe_stmt :: { Maybe RdrNameStmt }
	: stmt				{ Just $1 }
	| {- nothing -}			{ Nothing }

stmt  :: { RdrNameStmt }
	: qual				{ $1 }
	| srcloc infixexp '->' exp	{% checkPattern $1 $4 >>= \p ->
					   return (BindStmt p $2 $1) }
  	| srcloc 'rec' stmtlist		{ RecStmt $3 undefined undefined undefined }

qual  :: { RdrNameStmt }
	: srcloc infixexp '<-' exp	{% checkPattern $1 $2 >>= \p ->
					   return (BindStmt p $4 $1) }
	| srcloc exp			{ ExprStmt $2 placeHolderType $1 }
  	| srcloc 'let' binds		{ LetStmt $3 }

-----------------------------------------------------------------------------
-- Record Field Update/Construction

fbinds :: { RdrNameHsRecordBinds }
	: fbinds1			{ $1 }
	| {- empty -}			{ [] }

fbinds1	:: { RdrNameHsRecordBinds }
	: fbinds1 ',' fbind		{ $3 : $1 }
	| fbind				{ [$1] }

fbind	:: { (RdrName, RdrNameHsExpr) }
	: qvar '=' exp			{ ($1,$3) }

-----------------------------------------------------------------------------
-- Implicit Parameter Bindings

dbinding :: { [(IPName RdrName, RdrNameHsExpr)] }
	: '{' dbinds '}'		{ $2 }
	| vocurly dbinds close		{ $2 }

dbinds 	:: { [(IPName RdrName, RdrNameHsExpr)] }
	: dbinds ';' dbind		{ $3 : $1 }
	| dbinds ';'			{ $1 }
	| dbind				{ [$1] }
--	| {- empty -}			{ [] }

dbind	:: { (IPName RdrName, RdrNameHsExpr) }
dbind	: ipvar '=' exp			{ ($1, $3) }

-----------------------------------------------------------------------------
-- Variables, Constructors and Operators.

identifier :: { RdrName }
	: qvar				{ $1 }
	| gcon				{ $1 }
	| qop				{ $1 }

depreclist :: { [RdrName] }
depreclist : deprec_var			{ [$1] }
	   | deprec_var ',' depreclist	{ $1 : $3 }

deprec_var :: { RdrName }
deprec_var : var			{ $1 }
	   | tycon			{ $1 }

gcon 	:: { RdrName }	-- Data constructor namespace
	: sysdcon		{ nameRdrName (dataConName $1) }
 	| qcon			{ $1 }
-- the case of '[:' ':]' is part of the production `parr'

sysdcon	:: { DataCon }	-- Wired in data constructors
	: '(' ')'		{ unitDataCon }
	| '(' commas ')'	{ tupleCon Boxed $2 }
	| '[' ']'		{ nilDataCon }

var 	:: { RdrName }
	: varid			{ $1 }
	| '(' varsym ')'	{ $2 }

qvar 	:: { RdrName }
	: qvarid		{ $1 }
	| '(' varsym ')'	{ $2 }
	| '(' qvarsym1 ')'	{ $2 }
-- We've inlined qvarsym here so that the decision about
-- whether it's a qvar or a var can be postponed until
-- *after* we see the close paren.

ipvar	:: { IPName RdrName }
	: IPDUPVARID		{ Dupable (mkUnqual varName $1) }
	| IPSPLITVARID		{ Linear  (mkUnqual varName $1) }

qcon	:: { RdrName }
	: qconid		{ $1 }
	| '(' qconsym ')'	{ $2 }

varop	:: { RdrName }
	: varsym		{ $1 }
	| '`' varid '`'		{ $2 }

qvarop :: { RdrName }
	: qvarsym		{ $1 }
	| '`' qvarid '`'	{ $2 }

qvaropm :: { RdrName }
	: qvarsym_no_minus	{ $1 }
	| '`' qvarid '`'	{ $2 }

conop :: { RdrName }
	: consym		{ $1 }	
	| '`' conid '`'		{ $2 }

qconop :: { RdrName }
	: qconsym		{ $1 }
	| '`' qconid '`'	{ $2 }

-----------------------------------------------------------------------------
-- Type constructors

gtycon 	:: { RdrName }	-- A "general" qualified tycon
	: oqtycon			{ $1 }
	| '(' ')'			{ getRdrName unitTyCon }
	| '(' commas ')'		{ getRdrName (tupleTyCon Boxed $2) }
	| '(' '->' ')'			{ nameRdrName funTyConName }
	| '[' ']'			{ nameRdrName listTyConName }
	| '[:' ':]'			{ nameRdrName parrTyConName }

oqtycon :: { RdrName }	-- An "ordinary" qualified tycon
	: qtycon			{ $1 }
 	| '(' qtyconsym ')'		{ $2 }

qtyconop :: { RdrName }	-- Qualified or unqualified
	: qtyconsym			{ $1 }
	| '`' qtycon '`'		{ $2 }

tyconop	:: { RdrName }	-- Unqualified
	: tyconsym			{ $1 }
	| '`' tycon '`'			{ $2 }

qtycon :: { RdrName }	-- Qualified or unqualified
	: QCONID			{ mkQual tcClsName $1 }
	| tycon				{ $1 }

tycon 	:: { RdrName }	-- Unqualified
	: CONID				{ mkUnqual tcClsName $1 }

qtyconsym :: { RdrName }
	: QCONSYM			{ mkQual tcClsName $1 }
	| tyconsym			{ $1 }

tyconsym :: { RdrName }
	: CONSYM			{ mkUnqual tcClsName $1 }

-----------------------------------------------------------------------------
-- Any operator

op	:: { RdrName }   -- used in infix decls
	: varop			{ $1 }
	| conop 		{ $1 }

qop	:: { RdrName {-HsExpr-} }   -- used in sections
	: qvarop		{ $1 }
	| qconop		{ $1 }

qopm	:: { RdrNameHsExpr }   -- used in sections
	: qvaropm		{ HsVar $1 }
	| qconop		{ HsVar $1 }

-----------------------------------------------------------------------------
-- VarIds

qvarid :: { RdrName }
	: varid			{ $1 }
	| QVARID		{ mkQual varName $1 }

varid :: { RdrName }
	: varid_no_unsafe 	{ $1 }
	| 'unsafe'		{ mkUnqual varName FSLIT("unsafe") }
	| 'safe'		{ mkUnqual varName FSLIT("safe") }
	| 'threadsafe'		{ mkUnqual varName FSLIT("threadsafe") }

varid_no_unsafe :: { RdrName }
	: VARID			{ mkUnqual varName $1 }
	| special_id		{ mkUnqual varName $1 }
	| 'forall'		{ mkUnqual varName FSLIT("forall") }

tyvar 	:: { RdrName }
	: VARID			{ mkUnqual tvName $1 }
	| special_id		{ mkUnqual tvName $1 }
	| 'unsafe' 		{ mkUnqual tvName FSLIT("unsafe") }
	| 'safe' 		{ mkUnqual tvName FSLIT("safe") }
	| 'threadsafe' 		{ mkUnqual tvName FSLIT("threadsafe") }

-- These special_ids are treated as keywords in various places, 
-- but as ordinary ids elsewhere.   'special_id' collects all these
-- except 'unsafe' and 'forall' whose treatment differs depending on context
special_id :: { UserFS }
special_id
	: 'as'			{ FSLIT("as") }
	| 'qualified'		{ FSLIT("qualified") }
	| 'hiding'		{ FSLIT("hiding") }
	| 'export'		{ FSLIT("export") }
	| 'label'		{ FSLIT("label")  }
	| 'dynamic'		{ FSLIT("dynamic") }
	| 'stdcall'             { FSLIT("stdcall") }
	| 'ccall'               { FSLIT("ccall") }

-----------------------------------------------------------------------------
-- Variables 

qvarsym :: { RdrName }
	: varsym		{ $1 }
	| qvarsym1		{ $1 }

qvarsym_no_minus :: { RdrName }
	: varsym_no_minus	{ $1 }
	| qvarsym1		{ $1 }

qvarsym1 :: { RdrName }
qvarsym1 : QVARSYM		{ mkQual varName $1 }

varsym :: { RdrName }
	: varsym_no_minus 	{ $1 }
	| '-'			{ mkUnqual varName FSLIT("-") }

varsym_no_minus :: { RdrName } -- varsym not including '-'
	: VARSYM		{ mkUnqual varName $1 }
	| special_sym		{ mkUnqual varName $1 }


-- See comments with special_id
special_sym :: { UserFS }
special_sym : '!'	{ FSLIT("!") }
	    | '.' 	{ FSLIT(".") }
 	    | '*' 	{ FSLIT("*") }

-----------------------------------------------------------------------------
-- Data constructors

qconid :: { RdrName }	-- Qualified or unqualifiedb
	: conid			{ $1 }
	| QCONID		{ mkQual dataName $1 }

conid 	:: { RdrName }
	: CONID			{ mkUnqual dataName $1 }

qconsym :: { RdrName }	-- Qualified or unqualified
	: consym		{ $1 }
	| QCONSYM		{ mkQual dataName $1 }

consym :: { RdrName }
	: CONSYM		{ mkUnqual dataName $1 }

	-- ':' means only list cons
	| ':'			{ nameRdrName consDataConName }
				-- NB: SrcName because we are reading source


-----------------------------------------------------------------------------
-- Literals

literal :: { HsLit }
	: CHAR 			{ HsChar       (ord $1) } --TODO remove ord
	| STRING		{ HsString     $1 }
	| PRIMINTEGER		{ HsIntPrim    $1 }
	| PRIMCHAR		{ HsCharPrim   (ord $1) } --TODO remove ord
	| PRIMSTRING		{ HsStringPrim $1 }
	| PRIMFLOAT		{ HsFloatPrim  $1 }
	| PRIMDOUBLE		{ HsDoublePrim $1 }

srcloc :: { SrcLoc }	:	{% getSrcLoc }

-----------------------------------------------------------------------------
-- Layout

close :: { () }
	: vccurly		{ () } -- context popped in lexer.
	| error			{% popContext }

-----------------------------------------------------------------------------
-- Miscellaneous (mostly renamings)

modid 	:: { ModuleName }
	: CONID			{ mkModuleNameFS $1 }
        | QCONID		{ mkModuleNameFS
				   (mkFastString
				     (unpackFS (fst $1) ++ 
					'.':unpackFS (snd $1)))
				}

commas :: { Int }
	: commas ','			{ $1 + 1 }
	| ','				{ 2 }

-----------------------------------------------------------------------------

{
happyError :: P a
happyError = srcParseFail
}
