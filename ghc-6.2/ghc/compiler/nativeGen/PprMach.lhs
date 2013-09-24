%
% (c) The AQUA Project, Glasgow University, 1996-1998
%
\section[PprMach]{Pretty-printing assembly language}

We start with the @pprXXX@s with some cross-platform commonality
(e.g., @pprReg@); we conclude with the no-commonality monster,
@pprInstr@.

\begin{code}
#include "nativeGen/NCG.h"

module PprMach ( pprInstr, pprSize, pprUserReg IF_OS_darwin(COMMA pprDyldSymbolStub, ) ) where

#include "HsVersions.h"

import MachRegs		-- may differ per-platform
import MachMisc

import CLabel		( pprCLabel, externallyVisibleCLabel, labelDynamic )
import Stix		( CodeSegment(..) )
import Panic		( panic )
import Pretty
import FastString
import qualified Outputable

#if __GLASGOW_HASKELL__ >= 504
import Data.Array.ST
import Data.Word	( Word8, Word16 )
#else
import MutableArray
import Word             ( Word16 )
#endif

import MONAD_ST

import Char		( chr, ord )
import Maybe		( isJust )

asmSDoc d = Outputable.withPprStyleDoc (
	      Outputable.mkCodeStyle Outputable.AsmStyle) d
pprCLabel_asm l = asmSDoc (pprCLabel l)
\end{code}

%************************************************************************
%*									*
\subsection{@pprReg@: print a @Reg@}
%*									*
%************************************************************************

For x86, the way we print a register name depends
on which bit of it we care about.  Yurgh.
\begin{code}
pprUserReg :: Reg -> Doc
pprUserReg = pprReg IF_ARCH_i386(L,)

pprReg :: IF_ARCH_i386(Size ->,) Reg -> Doc

pprReg IF_ARCH_i386(s,) r
  = case r of
      RealReg i      -> ppr_reg_no IF_ARCH_i386(s,) i
      VirtualRegI u  -> text "%vI_" <> asmSDoc (pprVRegUnique u)
      VirtualRegF u  -> text "%vF_" <> asmSDoc (pprVRegUnique u)
  where
#if alpha_TARGET_ARCH
    ppr_reg_no :: Int -> Doc
    ppr_reg_no i = ptext
      (case i of {
	 0 -> SLIT("$0");    1 -> SLIT("$1");
	 2 -> SLIT("$2");    3 -> SLIT("$3");
	 4 -> SLIT("$4");    5 -> SLIT("$5");
	 6 -> SLIT("$6");    7 -> SLIT("$7");
	 8 -> SLIT("$8");    9 -> SLIT("$9");
	10 -> SLIT("$10");  11 -> SLIT("$11");
	12 -> SLIT("$12");  13 -> SLIT("$13");
	14 -> SLIT("$14");  15 -> SLIT("$15");
	16 -> SLIT("$16");  17 -> SLIT("$17");
	18 -> SLIT("$18");  19 -> SLIT("$19");
	20 -> SLIT("$20");  21 -> SLIT("$21");
	22 -> SLIT("$22");  23 -> SLIT("$23");
	24 -> SLIT("$24");  25 -> SLIT("$25");
	26 -> SLIT("$26");  27 -> SLIT("$27");
	28 -> SLIT("$28");  29 -> SLIT("$29");
	30 -> SLIT("$30");  31 -> SLIT("$31");
	32 -> SLIT("$f0");  33 -> SLIT("$f1");
	34 -> SLIT("$f2");  35 -> SLIT("$f3");
	36 -> SLIT("$f4");  37 -> SLIT("$f5");
	38 -> SLIT("$f6");  39 -> SLIT("$f7");
	40 -> SLIT("$f8");  41 -> SLIT("$f9");
	42 -> SLIT("$f10"); 43 -> SLIT("$f11");
	44 -> SLIT("$f12"); 45 -> SLIT("$f13");
	46 -> SLIT("$f14"); 47 -> SLIT("$f15");
	48 -> SLIT("$f16"); 49 -> SLIT("$f17");
	50 -> SLIT("$f18"); 51 -> SLIT("$f19");
	52 -> SLIT("$f20"); 53 -> SLIT("$f21");
	54 -> SLIT("$f22"); 55 -> SLIT("$f23");
	56 -> SLIT("$f24"); 57 -> SLIT("$f25");
	58 -> SLIT("$f26"); 59 -> SLIT("$f27");
	60 -> SLIT("$f28"); 61 -> SLIT("$f29");
	62 -> SLIT("$f30"); 63 -> SLIT("$f31");
	_  -> SLIT("very naughty alpha register")
      })
#endif
#if i386_TARGET_ARCH
    ppr_reg_no :: Size -> Int -> Doc
    ppr_reg_no B  = ppr_reg_byte
    ppr_reg_no Bu = ppr_reg_byte
    ppr_reg_no W  = ppr_reg_word
    ppr_reg_no Wu = ppr_reg_word
    ppr_reg_no _  = ppr_reg_long

    ppr_reg_byte i = ptext
      (case i of {
	 0 -> SLIT("%al");     1 -> SLIT("%bl");
	 2 -> SLIT("%cl");     3 -> SLIT("%dl");
	_  -> SLIT("very naughty I386 byte register")
      })

    ppr_reg_word i = ptext
      (case i of {
	 0 -> SLIT("%ax");     1 -> SLIT("%bx");
	 2 -> SLIT("%cx");     3 -> SLIT("%dx");
	 4 -> SLIT("%si");     5 -> SLIT("%di");
	 6 -> SLIT("%bp");     7 -> SLIT("%sp");
	_  -> SLIT("very naughty I386 word register")
      })

    ppr_reg_long i = ptext
      (case i of {
	 0 -> SLIT("%eax");    1 -> SLIT("%ebx");
	 2 -> SLIT("%ecx");    3 -> SLIT("%edx");
	 4 -> SLIT("%esi");    5 -> SLIT("%edi");
	 6 -> SLIT("%ebp");    7 -> SLIT("%esp");
	 8 -> SLIT("%fake0");  9 -> SLIT("%fake1");
	10 -> SLIT("%fake2"); 11 -> SLIT("%fake3");
	12 -> SLIT("%fake4"); 13 -> SLIT("%fake5");
	_  -> SLIT("very naughty I386 register")
      })
#endif
#if sparc_TARGET_ARCH
    ppr_reg_no :: Int -> Doc
    ppr_reg_no i = ptext
      (case i of {
	 0 -> SLIT("%g0");   1 -> SLIT("%g1");
	 2 -> SLIT("%g2");   3 -> SLIT("%g3");
	 4 -> SLIT("%g4");   5 -> SLIT("%g5");
	 6 -> SLIT("%g6");   7 -> SLIT("%g7");
	 8 -> SLIT("%o0");   9 -> SLIT("%o1");
	10 -> SLIT("%o2");  11 -> SLIT("%o3");
	12 -> SLIT("%o4");  13 -> SLIT("%o5");
	14 -> SLIT("%o6");  15 -> SLIT("%o7");
	16 -> SLIT("%l0");  17 -> SLIT("%l1");
	18 -> SLIT("%l2");  19 -> SLIT("%l3");
	20 -> SLIT("%l4");  21 -> SLIT("%l5");
	22 -> SLIT("%l6");  23 -> SLIT("%l7");
	24 -> SLIT("%i0");  25 -> SLIT("%i1");
	26 -> SLIT("%i2");  27 -> SLIT("%i3");
	28 -> SLIT("%i4");  29 -> SLIT("%i5");
	30 -> SLIT("%i6");  31 -> SLIT("%i7");
	32 -> SLIT("%f0");  33 -> SLIT("%f1");
	34 -> SLIT("%f2");  35 -> SLIT("%f3");
	36 -> SLIT("%f4");  37 -> SLIT("%f5");
	38 -> SLIT("%f6");  39 -> SLIT("%f7");
	40 -> SLIT("%f8");  41 -> SLIT("%f9");
	42 -> SLIT("%f10"); 43 -> SLIT("%f11");
	44 -> SLIT("%f12"); 45 -> SLIT("%f13");
	46 -> SLIT("%f14"); 47 -> SLIT("%f15");
	48 -> SLIT("%f16"); 49 -> SLIT("%f17");
	50 -> SLIT("%f18"); 51 -> SLIT("%f19");
	52 -> SLIT("%f20"); 53 -> SLIT("%f21");
	54 -> SLIT("%f22"); 55 -> SLIT("%f23");
	56 -> SLIT("%f24"); 57 -> SLIT("%f25");
	58 -> SLIT("%f26"); 59 -> SLIT("%f27");
	60 -> SLIT("%f28"); 61 -> SLIT("%f29");
	62 -> SLIT("%f30"); 63 -> SLIT("%f31");
	_  -> SLIT("very naughty sparc register")
      })
#endif
#if powerpc_TARGET_ARCH
#if darwin_TARGET_OS
    ppr_reg_no :: Int -> Doc
    ppr_reg_no i = ptext
      (case i of {
	 0 -> SLIT("r0");   1 -> SLIT("r1");
	 2 -> SLIT("r2");   3 -> SLIT("r3");
	 4 -> SLIT("r4");   5 -> SLIT("r5");
	 6 -> SLIT("r6");   7 -> SLIT("r7");
	 8 -> SLIT("r8");   9 -> SLIT("r9");
	10 -> SLIT("r10");  11 -> SLIT("r11");
	12 -> SLIT("r12");  13 -> SLIT("r13");
	14 -> SLIT("r14");  15 -> SLIT("r15");
	16 -> SLIT("r16");  17 -> SLIT("r17");
	18 -> SLIT("r18");  19 -> SLIT("r19");
	20 -> SLIT("r20");  21 -> SLIT("r21");
	22 -> SLIT("r22");  23 -> SLIT("r23");
	24 -> SLIT("r24");  25 -> SLIT("r25");
	26 -> SLIT("r26");  27 -> SLIT("r27");
	28 -> SLIT("r28");  29 -> SLIT("r29");
	30 -> SLIT("r30");  31 -> SLIT("r31");
	32 -> SLIT("f0");  33 -> SLIT("f1");
	34 -> SLIT("f2");  35 -> SLIT("f3");
	36 -> SLIT("f4");  37 -> SLIT("f5");
	38 -> SLIT("f6");  39 -> SLIT("f7");
	40 -> SLIT("f8");  41 -> SLIT("f9");
	42 -> SLIT("f10"); 43 -> SLIT("f11");
	44 -> SLIT("f12"); 45 -> SLIT("f13");
	46 -> SLIT("f14"); 47 -> SLIT("f15");
	48 -> SLIT("f16"); 49 -> SLIT("f17");
	50 -> SLIT("f18"); 51 -> SLIT("f19");
	52 -> SLIT("f20"); 53 -> SLIT("f21");
	54 -> SLIT("f22"); 55 -> SLIT("f23");
	56 -> SLIT("f24"); 57 -> SLIT("f25");
	58 -> SLIT("f26"); 59 -> SLIT("f27");
	60 -> SLIT("f28"); 61 -> SLIT("f29");
	62 -> SLIT("f30"); 63 -> SLIT("f31");
	_  -> SLIT("very naughty powerpc register")
      })
#else
    ppr_reg_no :: Int -> Doc
    ppr_reg_no i | i <= 31 = int i	-- GPRs
                 | i <= 63 = int (i-32) -- FPRs
		 | otherwise = ptext SLIT("very naughty powerpc register")
#endif
#endif
\end{code}

%************************************************************************
%*									*
\subsection{@pprSize@: print a @Size@}
%*									*
%************************************************************************

\begin{code}
pprSize :: Size -> Doc

pprSize x = ptext (case x of
#if alpha_TARGET_ARCH
	 B  -> SLIT("b")
	 Bu -> SLIT("bu")
--	 W  -> SLIT("w") UNUSED
--	 Wu -> SLIT("wu") UNUSED
	 L  -> SLIT("l")
	 Q  -> SLIT("q")
--	 FF -> SLIT("f") UNUSED
--	 DF -> SLIT("d") UNUSED
--	 GF -> SLIT("g") UNUSED
--	 SF -> SLIT("s") UNUSED
	 TF -> SLIT("t")
#endif
#if i386_TARGET_ARCH
	B   -> SLIT("b")
	Bu  -> SLIT("b")
	W   -> SLIT("w")
	Wu  -> SLIT("w")
	L   -> SLIT("l")
	Lu  -> SLIT("l")
	F   -> SLIT("s")
	DF  -> SLIT("l")
	F80 -> SLIT("t")
#endif
#if sparc_TARGET_ARCH
	B   -> SLIT("sb")
	Bu  -> SLIT("ub")
        H   -> SLIT("sh")
        Hu  -> SLIT("uh")
	W   -> SLIT("")
	F   -> SLIT("")
	DF  -> SLIT("d")
    )
pprStSize :: Size -> Doc
pprStSize x = ptext (case x of
	B   -> SLIT("b")
	Bu  -> SLIT("b")
	H   -> SLIT("h")
	Hu  -> SLIT("h")
	W   -> SLIT("")
	F   -> SLIT("")
	DF  -> SLIT("d")
#endif
#if powerpc_TARGET_ARCH
	B   -> SLIT("b")
	Bu  -> SLIT("b")
        H   -> SLIT("h")
        Hu  -> SLIT("h")
	W   -> SLIT("w")
	F   -> SLIT("fs")
	DF  -> SLIT("fd")
#endif
    )
\end{code}

%************************************************************************
%*									*
\subsection{@pprCond@: print a @Cond@}
%*									*
%************************************************************************

\begin{code}
pprCond :: Cond -> Doc

pprCond c = ptext (case c of {
#if alpha_TARGET_ARCH
	EQQ  -> SLIT("eq");
	LTT  -> SLIT("lt");
	LE  -> SLIT("le");
	ULT -> SLIT("ult");
	ULE -> SLIT("ule");
	NE  -> SLIT("ne");
	GTT  -> SLIT("gt");
	GE  -> SLIT("ge")
#endif
#if i386_TARGET_ARCH
	GEU	-> SLIT("ae");	LU    -> SLIT("b");
	EQQ	-> SLIT("e");	GTT    -> SLIT("g");
	GE	-> SLIT("ge");	GU    -> SLIT("a");
	LTT	-> SLIT("l");	LE    -> SLIT("le");
	LEU	-> SLIT("be");	NE    -> SLIT("ne");
	NEG	-> SLIT("s");	POS   -> SLIT("ns");
        CARRY   -> SLIT("c");   OFLO  -> SLIT("o");
	ALWAYS	-> SLIT("mp")	-- hack
#endif
#if sparc_TARGET_ARCH
	ALWAYS	-> SLIT("");	NEVER -> SLIT("n");
	GEU	-> SLIT("geu");	LU    -> SLIT("lu");
	EQQ	-> SLIT("e");	GTT   -> SLIT("g");
	GE	-> SLIT("ge");	GU    -> SLIT("gu");
	LTT	-> SLIT("l");	LE    -> SLIT("le");
	LEU	-> SLIT("leu");	NE    -> SLIT("ne");
	NEG	-> SLIT("neg");	POS   -> SLIT("pos");
	VC	-> SLIT("vc");	VS    -> SLIT("vs")
#endif
#if powerpc_TARGET_ARCH
	ALWAYS  -> SLIT("");
	EQQ	-> SLIT("eq");	NE    -> SLIT("ne");
	LTT     -> SLIT("lt");  GE    -> SLIT("ge");
	GTT     -> SLIT("gt");  LE    -> SLIT("le");
	LU      -> SLIT("lt");  GEU   -> SLIT("ge");
	GU      -> SLIT("gt");  LEU   -> SLIT("le");
#endif
    })
\end{code}

%************************************************************************
%*									*
\subsection{@pprImm@: print an @Imm@}
%*									*
%************************************************************************

\begin{code}
pprImm :: Imm -> Doc

pprImm (ImmInt i)     = int i
pprImm (ImmInteger i) = integer i
pprImm (ImmCLbl l)    = (if labelDynamic l then text "__imp_" else empty)
                        <> pprCLabel_asm l
pprImm (ImmIndex l i) = (if labelDynamic l then text "__imp_" else empty)
                        <> pprCLabel_asm l <> char '+' <> int i
pprImm (ImmLit s)     = s

pprImm (ImmLab dll s) = (if underscorePrefix then char '_' else empty)
                        <> (if dll then text "_imp__" else empty)
                        <> s

#if sparc_TARGET_ARCH
pprImm (LO i)
  = hcat [ pp_lo, pprImm i, rparen ]
  where
    pp_lo = text "%lo("

pprImm (HI i)
  = hcat [ pp_hi, pprImm i, rparen ]
  where
    pp_hi = text "%hi("
#endif
#if powerpc_TARGET_ARCH
#if darwin_TARGET_OS
pprImm (LO i)
  = hcat [ pp_lo, pprImm i, rparen ]
  where
    pp_lo = text "lo16("

pprImm (HI i)
  = hcat [ pp_hi, pprImm i, rparen ]
  where
    pp_hi = text "hi16("

pprImm (HA i)
  = hcat [ pp_ha, pprImm i, rparen ]
  where
    pp_ha = text "ha16("
#else
pprImm (LO i)
  = pprImm i <> text "@l"

pprImm (HI i)
  = pprImm i <> text "@h"

pprImm (HA i)
  = pprImm i <> text "@ha"
#endif
#endif
\end{code}

%************************************************************************
%*									*
\subsection{@pprAddr@: print an @Addr@}
%*									*
%************************************************************************

\begin{code}
pprAddr :: MachRegsAddr -> Doc

#if alpha_TARGET_ARCH
pprAddr (AddrReg r) = parens (pprReg r)
pprAddr (AddrImm i) = pprImm i
pprAddr (AddrRegImm r1 i)
  = (<>) (pprImm i) (parens (pprReg r1))
#endif

-------------------

#if i386_TARGET_ARCH
pprAddr (ImmAddr imm off)
  = let	pp_imm = pprImm imm
    in
    if (off == 0) then
	pp_imm
    else if (off < 0) then
	pp_imm <> int off
    else
	pp_imm <> char '+' <> int off

pprAddr (AddrBaseIndex base index displacement)
  = let
	pp_disp  = ppr_disp displacement
	pp_off p = pp_disp <> char '(' <> p <> char ')'
	pp_reg r = pprReg L r
    in
    case (base,index) of
      (Nothing, Nothing)    -> pp_disp
      (Just b,  Nothing)    -> pp_off (pp_reg b)
      (Nothing, Just (r,i)) -> pp_off (pp_reg r <> comma <> int i)
      (Just b,  Just (r,i)) -> pp_off (pp_reg b <> comma <> pp_reg r 
                                       <> comma <> int i)
  where
    ppr_disp (ImmInt 0) = empty
    ppr_disp imm        = pprImm imm
#endif

-------------------

#if sparc_TARGET_ARCH
pprAddr (AddrRegReg r1 (RealReg 0)) = pprReg r1

pprAddr (AddrRegReg r1 r2)
  = hcat [ pprReg r1, char '+', pprReg r2 ]

pprAddr (AddrRegImm r1 (ImmInt i))
  | i == 0 = pprReg r1
  | not (fits13Bits i) = largeOffsetError i
  | otherwise = hcat [ pprReg r1, pp_sign, int i ]
  where
    pp_sign = if i > 0 then char '+' else empty

pprAddr (AddrRegImm r1 (ImmInteger i))
  | i == 0 = pprReg r1
  | not (fits13Bits i) = largeOffsetError i
-------------------

  | otherwise  = hcat [ pprReg r1, pp_sign, integer i ]
  where
    pp_sign = if i > 0 then char '+' else empty

pprAddr (AddrRegImm r1 imm)
  = hcat [ pprReg r1, char '+', pprImm imm ]
#endif
#if powerpc_TARGET_ARCH
pprAddr (AddrRegReg r1 r2)
  = error "PprMach.pprAddr (AddrRegReg) unimplemented"

pprAddr (AddrRegImm r1 (ImmInt i)) = hcat [ int i, char '(', pprReg r1, char ')' ]
pprAddr (AddrRegImm r1 (ImmInteger i)) = hcat [ integer i, char '(', pprReg r1, char ')' ]
pprAddr (AddrRegImm r1 imm) = hcat [ pprImm imm, char '(', pprReg r1, char ')' ]
#endif
\end{code}

%************************************************************************
%*									*
\subsection{@pprInstr@: print an @Instr@}
%*									*
%************************************************************************

\begin{code}
pprInstr :: Instr -> Doc

--pprInstr (COMMENT s) = empty -- nuke 'em
pprInstr (COMMENT s)
   =  IF_ARCH_alpha( ((<>) (ptext SLIT("\t# ")) (ftext s))
     ,IF_ARCH_sparc( ((<>) (ptext SLIT("! "))   (ftext s))
     ,IF_ARCH_i386( ((<>) (ptext SLIT("# "))   (ftext s))
     ,IF_ARCH_powerpc( ((<>) (ptext SLIT("; ")) (ftext s))
     ,))))

pprInstr (DELTA d)
   = pprInstr (COMMENT (mkFastString ("\tdelta = " ++ show d)))

pprInstr (SEGMENT TextSegment)
    =  IF_ARCH_alpha(ptext SLIT("\t.text\n\t.align 3") {-word boundary-}
      ,IF_ARCH_sparc(ptext SLIT(".text\n\t.align 4") {-word boundary-}
      ,IF_ARCH_i386((text ".text\n\t.align 4,0x90") {-needs per-OS variation!-}
      ,IF_ARCH_powerpc(ptext SLIT(".text\n.align 2")
      ,))))

pprInstr (SEGMENT DataSegment)
    = ptext
	 IF_ARCH_alpha(SLIT("\t.data\n\t.align 3")
	,IF_ARCH_sparc(SLIT(".data\n\t.align 8") {-<8 will break double constants -}
	,IF_ARCH_i386(SLIT(".data\n\t.align 4")
        ,IF_ARCH_powerpc(SLIT(".data\n.align 2")
	,))))

pprInstr (SEGMENT RoDataSegment)
    = ptext
	 IF_ARCH_alpha(SLIT("\t.data\n\t.align 3")
	,IF_ARCH_sparc(SLIT(".data\n\t.align 8") {-<8 will break double constants -}
	,IF_ARCH_i386(SLIT(".section .rodata\n\t.align 4")
        ,IF_ARCH_powerpc(IF_OS_darwin(SLIT(".const_data\n.align 2"),
                                      SLIT(".section .rodata\n\t.align 2"))
	,))))

pprInstr (LABEL clab)
  = let
	pp_lab = pprCLabel_asm clab
    in
    hcat [
	if not (externallyVisibleCLabel clab) then
	    empty
	else
	    hcat [ptext
			 IF_ARCH_alpha(SLIT("\t.globl\t")
		        ,IF_ARCH_i386(SLIT(".globl ")
			,IF_ARCH_sparc(SLIT(".global\t")
		        ,IF_ARCH_powerpc(SLIT(".globl ")
			,))))
			, pp_lab, char '\n'],
	pp_lab,
	char ':'
    ]

pprInstr (ASCII False{-no backslash conversion-} str)
  = hcat [ ptext SLIT("\t.asciz "), char '\"', text str, char '"' ]

pprInstr (ASCII True str)
  = vcat (map do1 (str ++ [chr 0]))
    where
       do1 :: Char -> Doc
       do1 c = ptext SLIT("\t.byte\t0x") <> hshow (ord c)

       hshow :: Int -> Doc
       hshow n | n >= 0 && n <= 255
               = char (tab !! (n `div` 16)) <> char (tab !! (n `mod` 16))
       tab = "0123456789ABCDEF"


pprInstr (DATA s xs)
  = vcat (concatMap (ppr_item s) xs)
    where

#if alpha_TARGET_ARCH
            ppr_item = error "ppr_item on Alpha"
#endif
#if sparc_TARGET_ARCH
        -- copy n paste of x86 version
	ppr_item B  x = [ptext SLIT("\t.byte\t") <> pprImm x]
	ppr_item W  x = [ptext SLIT("\t.long\t") <> pprImm x]
	ppr_item F  (ImmFloat r)
           = let bs = floatToBytes (fromRational r)
             in  map (\b -> ptext SLIT("\t.byte\t") <> pprImm (ImmInt b)) bs
    	ppr_item DF (ImmDouble r)
           = let bs = doubleToBytes (fromRational r)
             in  map (\b -> ptext SLIT("\t.byte\t") <> pprImm (ImmInt b)) bs
#endif
#if i386_TARGET_ARCH
	ppr_item B  x = [ptext SLIT("\t.byte\t") <> pprImm x]
	ppr_item L  x = [ptext SLIT("\t.long\t") <> pprImm x]
	ppr_item F  (ImmFloat r)
           = let bs = floatToBytes (fromRational r)
             in  map (\b -> ptext SLIT("\t.byte\t") <> pprImm (ImmInt b)) bs
    	ppr_item DF (ImmDouble r)
           = let bs = doubleToBytes (fromRational r)
             in  map (\b -> ptext SLIT("\t.byte\t") <> pprImm (ImmInt b)) bs
#endif
#if powerpc_TARGET_ARCH
	ppr_item B  x = [ptext SLIT("\t.byte\t") <> pprImm x]
	ppr_item Bu  x = [ptext SLIT("\t.byte\t") <> pprImm x]
	ppr_item H  x = [ptext SLIT("\t.short\t") <> pprImm x]
	ppr_item Hu  x = [ptext SLIT("\t.short\t") <> pprImm x]
	ppr_item W  x = [ptext SLIT("\t.long\t") <> pprImm x]
	ppr_item F  (ImmFloat r)
           = let bs = floatToBytes (fromRational r)
             in  map (\b -> ptext SLIT("\t.byte\t") <> pprImm (ImmInt b)) bs
    	ppr_item DF (ImmDouble r)
           = let bs = doubleToBytes (fromRational r)
             in  map (\b -> ptext SLIT("\t.byte\t") <> pprImm (ImmInt b)) bs
#endif

-- fall through to rest of (machine-specific) pprInstr...
\end{code}

%************************************************************************
%*									*
\subsubsection{@pprInstr@ for an Alpha}
%*									*
%************************************************************************

\begin{code}
#if alpha_TARGET_ARCH

pprInstr (LD size reg addr)
  = hcat [
	ptext SLIT("\tld"),
	pprSize size,
	char '\t',
	pprReg reg,
	comma,
	pprAddr addr
    ]

pprInstr (LDA reg addr)
  = hcat [
	ptext SLIT("\tlda\t"),
	pprReg reg,
	comma,
	pprAddr addr
    ]

pprInstr (LDAH reg addr)
  = hcat [
	ptext SLIT("\tldah\t"),
	pprReg reg,
	comma,
	pprAddr addr
    ]

pprInstr (LDGP reg addr)
  = hcat [
	ptext SLIT("\tldgp\t"),
	pprReg reg,
	comma,
	pprAddr addr
    ]

pprInstr (LDI size reg imm)
  = hcat [
	ptext SLIT("\tldi"),
	pprSize size,
	char '\t',
	pprReg reg,
	comma,
	pprImm imm
    ]

pprInstr (ST size reg addr)
  = hcat [
	ptext SLIT("\tst"),
	pprSize size,
	char '\t',
	pprReg reg,
	comma,
	pprAddr addr
    ]

pprInstr (CLR reg)
  = hcat [
	ptext SLIT("\tclr\t"),
	pprReg reg
    ]

pprInstr (ABS size ri reg)
  = hcat [
	ptext SLIT("\tabs"),
	pprSize size,
	char '\t',
	pprRI ri,
	comma,
	pprReg reg
    ]

pprInstr (NEG size ov ri reg)
  = hcat [
	ptext SLIT("\tneg"),
	pprSize size,
	if ov then ptext SLIT("v\t") else char '\t',
	pprRI ri,
	comma,
	pprReg reg
    ]

pprInstr (ADD size ov reg1 ri reg2)
  = hcat [
	ptext SLIT("\tadd"),
	pprSize size,
	if ov then ptext SLIT("v\t") else char '\t',
	pprReg reg1,
	comma,
	pprRI ri,
	comma,
	pprReg reg2
    ]

pprInstr (SADD size scale reg1 ri reg2)
  = hcat [
	ptext (case scale of {{-UNUSED:L -> SLIT("\ts4");-} Q -> SLIT("\ts8")}),
	ptext SLIT("add"),
	pprSize size,
	char '\t',
	pprReg reg1,
	comma,
	pprRI ri,
	comma,
	pprReg reg2
    ]

pprInstr (SUB size ov reg1 ri reg2)
  = hcat [
	ptext SLIT("\tsub"),
	pprSize size,
	if ov then ptext SLIT("v\t") else char '\t',
	pprReg reg1,
	comma,
	pprRI ri,
	comma,
	pprReg reg2
    ]

pprInstr (SSUB size scale reg1 ri reg2)
  = hcat [
	ptext (case scale of {{-UNUSED:L -> SLIT("\ts4");-} Q -> SLIT("\ts8")}),
	ptext SLIT("sub"),
	pprSize size,
	char '\t',
	pprReg reg1,
	comma,
	pprRI ri,
	comma,
	pprReg reg2
    ]

pprInstr (MUL size ov reg1 ri reg2)
  = hcat [
	ptext SLIT("\tmul"),
	pprSize size,
	if ov then ptext SLIT("v\t") else char '\t',
	pprReg reg1,
	comma,
	pprRI ri,
	comma,
	pprReg reg2
    ]

pprInstr (DIV size uns reg1 ri reg2)
  = hcat [
	ptext SLIT("\tdiv"),
	pprSize size,
	if uns then ptext SLIT("u\t") else char '\t',
	pprReg reg1,
	comma,
	pprRI ri,
	comma,
	pprReg reg2
    ]

pprInstr (REM size uns reg1 ri reg2)
  = hcat [
	ptext SLIT("\trem"),
	pprSize size,
	if uns then ptext SLIT("u\t") else char '\t',
	pprReg reg1,
	comma,
	pprRI ri,
	comma,
	pprReg reg2
    ]

pprInstr (NOT ri reg)
  = hcat [
	ptext SLIT("\tnot"),
	char '\t',
	pprRI ri,
	comma,
	pprReg reg
    ]

pprInstr (AND reg1 ri reg2) = pprRegRIReg SLIT("and") reg1 ri reg2
pprInstr (ANDNOT reg1 ri reg2) = pprRegRIReg SLIT("andnot") reg1 ri reg2
pprInstr (OR reg1 ri reg2) = pprRegRIReg SLIT("or") reg1 ri reg2
pprInstr (ORNOT reg1 ri reg2) = pprRegRIReg SLIT("ornot") reg1 ri reg2
pprInstr (XOR reg1 ri reg2) = pprRegRIReg SLIT("xor") reg1 ri reg2
pprInstr (XORNOT reg1 ri reg2) = pprRegRIReg SLIT("xornot") reg1 ri reg2

pprInstr (SLL reg1 ri reg2) = pprRegRIReg SLIT("sll") reg1 ri reg2
pprInstr (SRL reg1 ri reg2) = pprRegRIReg SLIT("srl") reg1 ri reg2
pprInstr (SRA reg1 ri reg2) = pprRegRIReg SLIT("sra") reg1 ri reg2

pprInstr (ZAP reg1 ri reg2) = pprRegRIReg SLIT("zap") reg1 ri reg2
pprInstr (ZAPNOT reg1 ri reg2) = pprRegRIReg SLIT("zapnot") reg1 ri reg2

pprInstr (NOP) = ptext SLIT("\tnop")

pprInstr (CMP cond reg1 ri reg2)
  = hcat [
	ptext SLIT("\tcmp"),
	pprCond cond,
	char '\t',
	pprReg reg1,
	comma,
	pprRI ri,
	comma,
	pprReg reg2
    ]

pprInstr (FCLR reg)
  = hcat [
	ptext SLIT("\tfclr\t"),
	pprReg reg
    ]

pprInstr (FABS reg1 reg2)
  = hcat [
	ptext SLIT("\tfabs\t"),
	pprReg reg1,
	comma,
	pprReg reg2
    ]

pprInstr (FNEG size reg1 reg2)
  = hcat [
	ptext SLIT("\tneg"),
	pprSize size,
	char '\t',
	pprReg reg1,
	comma,
	pprReg reg2
    ]

pprInstr (FADD size reg1 reg2 reg3) = pprSizeRegRegReg SLIT("add") size reg1 reg2 reg3
pprInstr (FDIV size reg1 reg2 reg3) = pprSizeRegRegReg SLIT("div") size reg1 reg2 reg3
pprInstr (FMUL size reg1 reg2 reg3) = pprSizeRegRegReg SLIT("mul") size reg1 reg2 reg3
pprInstr (FSUB size reg1 reg2 reg3) = pprSizeRegRegReg SLIT("sub") size reg1 reg2 reg3

pprInstr (CVTxy size1 size2 reg1 reg2)
  = hcat [
	ptext SLIT("\tcvt"),
	pprSize size1,
	case size2 of {Q -> ptext SLIT("qc"); _ -> pprSize size2},
	char '\t',
	pprReg reg1,
	comma,
	pprReg reg2
    ]

pprInstr (FCMP size cond reg1 reg2 reg3)
  = hcat [
	ptext SLIT("\tcmp"),
	pprSize size,
	pprCond cond,
	char '\t',
	pprReg reg1,
	comma,
	pprReg reg2,
	comma,
	pprReg reg3
    ]

pprInstr (FMOV reg1 reg2)
  = hcat [
	ptext SLIT("\tfmov\t"),
	pprReg reg1,
	comma,
	pprReg reg2
    ]

pprInstr (BI ALWAYS reg lab) = pprInstr (BR lab)

pprInstr (BI NEVER reg lab) = empty

pprInstr (BI cond reg lab)
  = hcat [
	ptext SLIT("\tb"),
	pprCond cond,
	char '\t',
	pprReg reg,
	comma,
	pprImm lab
    ]

pprInstr (BF cond reg lab)
  = hcat [
	ptext SLIT("\tfb"),
	pprCond cond,
	char '\t',
	pprReg reg,
	comma,
	pprImm lab
    ]

pprInstr (BR lab)
  = (<>) (ptext SLIT("\tbr\t")) (pprImm lab)

pprInstr (JMP reg addr hint)
  = hcat [
	ptext SLIT("\tjmp\t"),
	pprReg reg,
	comma,
	pprAddr addr,
	comma,
	int hint
    ]

pprInstr (BSR imm n)
  = (<>) (ptext SLIT("\tbsr\t")) (pprImm imm)

pprInstr (JSR reg addr n)
  = hcat [
	ptext SLIT("\tjsr\t"),
	pprReg reg,
	comma,
	pprAddr addr
    ]

pprInstr (FUNBEGIN clab)
  = hcat [
	if (externallyVisibleCLabel clab) then
	    hcat [ptext SLIT("\t.globl\t"), pp_lab, char '\n']
	else
	    empty,
	ptext SLIT("\t.ent "),
	pp_lab,
	char '\n',
	pp_lab,
	pp_ldgp,
	pp_lab,
	pp_frame
    ]
    where
	pp_lab = pprCLabel_asm clab

        -- NEVER use commas within those string literals, cpp will ruin your day
	pp_ldgp  = hcat [ ptext SLIT(":\n\tldgp $29"), char ',', ptext SLIT("0($27)\n") ]
	pp_frame = hcat [ ptext SLIT("..ng:\n\t.frame $30"), char ',',
                          ptext SLIT("4240"), char ',',
                          ptext SLIT("$26"), char ',',
                          ptext SLIT("0\n\t.prologue 1") ]

pprInstr (FUNEND clab)
  = (<>) (ptext SLIT("\t.align 4\n\t.end ")) (pprCLabel_asm clab)
\end{code}

Continue with Alpha-only printing bits and bobs:
\begin{code}
pprRI :: RI -> Doc

pprRI (RIReg r) = pprReg r
pprRI (RIImm r) = pprImm r

pprRegRIReg :: LitString -> Reg -> RI -> Reg -> Doc
pprRegRIReg name reg1 ri reg2
  = hcat [
 	char '\t',
	ptext name,
	char '\t',
	pprReg reg1,
	comma,
	pprRI ri,
	comma,
	pprReg reg2
    ]

pprSizeRegRegReg :: LitString -> Size -> Reg -> Reg -> Reg -> Doc
pprSizeRegRegReg name size reg1 reg2 reg3
  = hcat [
	char '\t',
	ptext name,
	pprSize size,
	char '\t',
	pprReg reg1,
	comma,
	pprReg reg2,
	comma,
	pprReg reg3
    ]

#endif /* alpha_TARGET_ARCH */
\end{code}

%************************************************************************
%*									*
\subsubsection{@pprInstr@ for an I386}
%*									*
%************************************************************************

\begin{code}
#if i386_TARGET_ARCH

pprInstr v@(MOV size s@(OpReg src) d@(OpReg dst)) -- hack
  | src == dst
  =
#if 0 /* #ifdef DEBUG */
    (<>) (ptext SLIT("# warning: ")) (pprSizeOpOp SLIT("mov") size s d)
#else
    empty
#endif
pprInstr (MOV size src dst)
  = pprSizeOpOp SLIT("mov") size src dst
pprInstr (MOVZxL sizes src dst) = pprSizeOpOpCoerce SLIT("movz") sizes L src dst
pprInstr (MOVSxL sizes src dst) = pprSizeOpOpCoerce SLIT("movs") sizes L src dst

-- here we do some patching, since the physical registers are only set late
-- in the code generation.
pprInstr (LEA size (OpAddr (AddrBaseIndex src1@(Just reg1) (Just (reg2,1)) (ImmInt 0))) dst@(OpReg reg3))
  | reg1 == reg3
  = pprSizeOpOp SLIT("add") size (OpReg reg2) dst
pprInstr (LEA size (OpAddr (AddrBaseIndex src1@(Just reg1) (Just (reg2,1)) (ImmInt 0))) dst@(OpReg reg3))
  | reg2 == reg3
  = pprSizeOpOp SLIT("add") size (OpReg reg1) dst
pprInstr (LEA size (OpAddr (AddrBaseIndex src1@(Just reg1) Nothing displ)) dst@(OpReg reg3))
  | reg1 == reg3
  = pprInstr (ADD size (OpImm displ) dst)
pprInstr (LEA size src dst) = pprSizeOpOp SLIT("lea") size src dst

pprInstr (ADD size (OpImm (ImmInt (-1))) dst)
  = pprSizeOp SLIT("dec") size dst
pprInstr (ADD size (OpImm (ImmInt 1)) dst)
  = pprSizeOp SLIT("inc") size dst
pprInstr (ADD size src dst)
  = pprSizeOpOp SLIT("add") size src dst
pprInstr (SUB size src dst) = pprSizeOpOp SLIT("sub") size src dst
pprInstr (IMUL size op1 op2) = pprSizeOpOp SLIT("imul") size op1 op2

{- A hack.  The Intel documentation says that "The two and three
   operand forms [of IMUL] may also be used with unsigned operands
   because the lower half of the product is the same regardless if
   (sic) the operands are signed or unsigned.  The CF and OF flags,
   however, cannot be used to determine if the upper half of the
   result is non-zero."  So there.  
-} 
pprInstr (MUL size op1 op2) = pprSizeOpOp SLIT("imul") size op1 op2

pprInstr (AND size src dst) = pprSizeOpOp SLIT("and") size src dst
pprInstr (OR  size src dst) = pprSizeOpOp SLIT("or")  size src dst
pprInstr (XOR size src dst) = pprSizeOpOp SLIT("xor")  size src dst
pprInstr (NOT size op) = pprSizeOp SLIT("not") size op
pprInstr (NEGI size op) = pprSizeOp SLIT("neg") size op

pprInstr (SHL size imm dst) = pprSizeImmOp SLIT("shl") size imm dst
pprInstr (SAR size imm dst) = pprSizeImmOp SLIT("sar") size imm dst
pprInstr (SHR size imm dst) = pprSizeImmOp SLIT("shr") size imm dst
pprInstr (BT  size imm src) = pprSizeImmOp SLIT("bt")  size imm src

pprInstr (CMP size src dst) = pprSizeOpOp SLIT("cmp")  size src dst
pprInstr (TEST size src dst) = pprSizeOpOp SLIT("test")  size src dst
pprInstr (PUSH size op) = pprSizeOp SLIT("push") size op
pprInstr (POP size op) = pprSizeOp SLIT("pop") size op
pprInstr PUSHA = ptext SLIT("\tpushal")
pprInstr POPA = ptext SLIT("\tpopal")

pprInstr NOP = ptext SLIT("\tnop")
pprInstr CLTD = ptext SLIT("\tcltd")

pprInstr (SETCC cond op) = pprCondInstr SLIT("set") cond (pprOperand B op)

pprInstr (JXX cond lab) = pprCondInstr SLIT("j") cond (pprCLabel_asm lab)

pprInstr (JMP dsts (OpImm imm)) = (<>) (ptext SLIT("\tjmp ")) (pprImm imm)
pprInstr (JMP dsts op)          = (<>) (ptext SLIT("\tjmp *")) (pprOperand L op)
pprInstr (CALL (Left imm))      = (<>) (ptext SLIT("\tcall ")) (pprImm imm)
pprInstr (CALL (Right reg))     = (<>) (ptext SLIT("\tcall *")) (pprReg L reg)

-- First bool indicates signedness; second whether quot or rem
pprInstr (IQUOT sz src dst) = pprInstr_quotRem True True sz src dst
pprInstr (IREM  sz src dst) = pprInstr_quotRem True False sz src dst

pprInstr (QUOT sz src dst) = pprInstr_quotRem False True sz src dst
pprInstr (REM  sz src dst) = pprInstr_quotRem False False sz src dst

pprInstr (IMUL64 sd_hi sd_lo) = pprInstr_imul64 sd_hi sd_lo


-- Simulating a flat register set on the x86 FP stack is tricky.
-- you have to free %st(7) before pushing anything on the FP reg stack
-- so as to preclude the possibility of a FP stack overflow exception.
pprInstr g@(GMOV src dst)
   | src == dst
   = empty
   | otherwise 
   = pprG g (hcat [gtab, gpush src 0, gsemi, gpop dst 1])

-- GLD sz addr dst ==> FFREE %st(7) ; FLDsz addr ; FSTP (dst+1)
pprInstr g@(GLD sz addr dst)
 = pprG g (hcat [gtab, text "ffree %st(7) ; fld", pprSize sz, gsp, 
                 pprAddr addr, gsemi, gpop dst 1])

-- GST sz src addr ==> FFREE %st(7) ; FLD dst ; FSTPsz addr
pprInstr g@(GST sz src addr)
 = pprG g (hcat [gtab, gpush src 0, gsemi, 
                 text "fstp", pprSize sz, gsp, pprAddr addr])

pprInstr g@(GLDZ dst)
 = pprG g (hcat [gtab, text "ffree %st(7) ; fldz ; ", gpop dst 1])
pprInstr g@(GLD1 dst)
 = pprG g (hcat [gtab, text "ffree %st(7) ; fld1 ; ", gpop dst 1])

pprInstr g@(GFTOI src dst) 
   = pprInstr (GDTOI src dst)
pprInstr g@(GDTOI src dst) 
   = pprG g (hcat [gtab, text "subl $4, %esp ; ", 
                   gpush src 0, gsemi, text "fistpl 0(%esp) ; popl ", 
                   pprReg L dst])

pprInstr g@(GITOF src dst) 
   = pprInstr (GITOD src dst)
pprInstr g@(GITOD src dst) 
   = pprG g (hcat [gtab, text "pushl ", pprReg L src, 
                   text " ; ffree %st(7); fildl (%esp) ; ",
                   gpop dst 1, text " ; addl $4,%esp"])

{- Gruesome swamp follows.  If you're unfortunate enough to have ventured
   this far into the jungle AND you give a Rat's Ass (tm) what's going
   on, here's the deal.  Generate code to do a floating point comparison
   of src1 and src2, of kind cond, and set the Zero flag if true.

   The complications are to do with handling NaNs correctly.  We want the
   property that if either argument is NaN, then the result of the
   comparison is False ... except if we're comparing for inequality,
   in which case the answer is True.

   Here's how the general (non-inequality) case works.  As an
   example, consider generating the an equality test:

     pushl %eax		-- we need to mess with this
     <get src1 to top of FPU stack>
     fcomp <src2 location in FPU stack> and pop pushed src1
		-- Result of comparison is in FPU Status Register bits
		-- C3 C2 and C0
     fstsw %ax	-- Move FPU Status Reg to %ax
     sahf	-- move C3 C2 C0 from %ax to integer flag reg
     -- now the serious magic begins
     setpo %ah	   -- %ah = if comparable(neither arg was NaN) then 1 else 0
     sete  %al     -- %al = if arg1 == arg2 then 1 else 0
     andb %ah,%al  -- %al &= %ah
                   -- so %al == 1 iff (comparable && same); else it holds 0
     decb %al	   -- %al == 0, ZeroFlag=1  iff (comparable && same); 
                      else %al == 0xFF, ZeroFlag=0
     -- the zero flag is now set as we desire.
     popl %eax

   The special case of inequality differs thusly:

     setpe %ah     -- %ah = if incomparable(either arg was NaN) then 1 else 0
     setne %al     -- %al = if arg1 /= arg2 then 1 else 0
     orb %ah,%al   -- %al = if (incomparable || different) then 1 else 0
     decb %al      -- if (incomparable || different) then (%al == 0, ZF=1)
                                                     else (%al == 0xFF, ZF=0)
-}
pprInstr g@(GCMP cond src1 src2) 
   | case cond of { NE -> True; other -> False }
   = pprG g (vcat [
        hcat [gtab, text "pushl %eax ; ",gpush src1 0],
        hcat [gtab, text "fcomp ", greg src2 1, 
                    text "; fstsw %ax ; sahf ;  setpe %ah"],
        hcat [gtab, text "setne %al ;  ",
              text "orb %ah,%al ;  decb %al ;  popl %eax"]
    ])
   | otherwise
   = pprG g (vcat [
        hcat [gtab, text "pushl %eax ; ",gpush src1 0],
        hcat [gtab, text "fcomp ", greg src2 1, 
                    text "; fstsw %ax ; sahf ;  setpo %ah"],
        hcat [gtab, text "set", pprCond (fix_FP_cond cond), text " %al ;  ",
              text "andb %ah,%al ;  decb %al ;  popl %eax"]
    ])
    where
        {- On the 486, the flags set by FP compare are the unsigned ones!
           (This looks like a HACK to me.  WDP 96/03)
        -}
        fix_FP_cond :: Cond -> Cond
        fix_FP_cond GE   = GEU
        fix_FP_cond GTT  = GU
        fix_FP_cond LTT  = LU
        fix_FP_cond LE   = LEU
        fix_FP_cond EQQ  = EQQ
        fix_FP_cond NE   = NE
        -- there should be no others


pprInstr g@(GABS sz src dst)
   = pprG g (hcat [gtab, gpush src 0, text " ; fabs ; ", gpop dst 1])
pprInstr g@(GNEG sz src dst)
   = pprG g (hcat [gtab, gpush src 0, text " ; fchs ; ", gpop dst 1])

pprInstr g@(GSQRT sz src dst)
   = pprG g (hcat [gtab, gpush src 0, text " ; fsqrt"] $$ 
             hcat [gtab, gcoerceto sz, gpop dst 1])
pprInstr g@(GSIN sz src dst)
   = pprG g (hcat [gtab, gpush src 0, text " ; fsin"] $$ 
             hcat [gtab, gcoerceto sz, gpop dst 1])
pprInstr g@(GCOS sz src dst)
   = pprG g (hcat [gtab, gpush src 0, text " ; fcos"] $$ 
             hcat [gtab, gcoerceto sz, gpop dst 1])
pprInstr g@(GTAN sz src dst)
   = pprG g (hcat [gtab, text "ffree %st(6) ; ",
                   gpush src 0, text " ; fptan ; ", 
                   text " fstp %st(0)"] $$
             hcat [gtab, gcoerceto sz, gpop dst 1])

-- In the translations for GADD, GMUL, GSUB and GDIV,
-- the first two cases are mere optimisations.  The otherwise clause
-- generates correct code under all circumstances.

pprInstr g@(GADD sz src1 src2 dst)
   | src1 == dst
   = pprG g (text "\t#GADD-xxxcase1" $$ 
             hcat [gtab, gpush src2 0,
                   text " ; faddp %st(0),", greg src1 1])
   | src2 == dst
   = pprG g (text "\t#GADD-xxxcase2" $$ 
             hcat [gtab, gpush src1 0,
                   text " ; faddp %st(0),", greg src2 1])
   | otherwise
   = pprG g (hcat [gtab, gpush src1 0, 
                   text " ; fadd ", greg src2 1, text ",%st(0)",
                   gsemi, gpop dst 1])


pprInstr g@(GMUL sz src1 src2 dst)
   | src1 == dst
   = pprG g (text "\t#GMUL-xxxcase1" $$ 
             hcat [gtab, gpush src2 0,
                   text " ; fmulp %st(0),", greg src1 1])
   | src2 == dst
   = pprG g (text "\t#GMUL-xxxcase2" $$ 
             hcat [gtab, gpush src1 0,
                   text " ; fmulp %st(0),", greg src2 1])
   | otherwise
   = pprG g (hcat [gtab, gpush src1 0, 
                   text " ; fmul ", greg src2 1, text ",%st(0)",
                   gsemi, gpop dst 1])


pprInstr g@(GSUB sz src1 src2 dst)
   | src1 == dst
   = pprG g (text "\t#GSUB-xxxcase1" $$ 
             hcat [gtab, gpush src2 0,
                   text " ; fsubrp %st(0),", greg src1 1])
   | src2 == dst
   = pprG g (text "\t#GSUB-xxxcase2" $$ 
             hcat [gtab, gpush src1 0,
                   text " ; fsubp %st(0),", greg src2 1])
   | otherwise
   = pprG g (hcat [gtab, gpush src1 0, 
                   text " ; fsub ", greg src2 1, text ",%st(0)",
                   gsemi, gpop dst 1])


pprInstr g@(GDIV sz src1 src2 dst)
   | src1 == dst
   = pprG g (text "\t#GDIV-xxxcase1" $$ 
             hcat [gtab, gpush src2 0,
                   text " ; fdivrp %st(0),", greg src1 1])
   | src2 == dst
   = pprG g (text "\t#GDIV-xxxcase2" $$ 
             hcat [gtab, gpush src1 0,
                   text " ; fdivp %st(0),", greg src2 1])
   | otherwise
   = pprG g (hcat [gtab, gpush src1 0, 
                   text " ; fdiv ", greg src2 1, text ",%st(0)",
                   gsemi, gpop dst 1])


pprInstr GFREE 
   = vcat [ ptext SLIT("\tffree %st(0) ;ffree %st(1) ;ffree %st(2) ;ffree %st(3)"),
            ptext SLIT("\tffree %st(4) ;ffree %st(5) ;ffree %st(6) ;ffree %st(7)") 
          ]


pprInstr_quotRem signed isQuot sz src dst
   | case sz of L -> False; _ -> True
   = panic "pprInstr_quotRem: dunno how to do non-32bit operands"
   | otherwise
   = vcat [
     (text "\t# BEGIN " <> fakeInsn),
     (text "\tpushl $0;  pushl %eax;  pushl %edx;  pushl " <> pprOperand sz src),
     (text "\tmovl " <> pprOperand sz dst <> text ",%eax;  " <> widen_to_64),
     (x86op <> text " 0(%esp);  movl " <> text resReg <> text ",12(%esp)"),
     (text "\tpopl %edx;  popl %edx;  popl %eax;  popl " <> pprOperand sz dst),
     (text "\t# END   " <> fakeInsn)
     ]
     where
        widen_to_64 | signed     = text "cltd"
                    | not signed = text "xorl %edx,%edx"
        x86op = if signed then text "\tidivl" else text "\tdivl"
        resReg = if isQuot then "%eax" else "%edx"
        opStr  | signed     = if isQuot then "IQUOT" else "IREM"
               | not signed = if isQuot then "QUOT"  else "REM"
        fakeInsn = text opStr <+> pprOperand sz src 
                              <> char ',' <+> pprOperand sz dst

-- Emit code to make hi_reg:lo_reg be the 64-bit product of hi_reg and lo_reg
pprInstr_imul64 hi_reg lo_reg
   = let fakeInsn = text "imul64" <+> pp_hi_reg <> comma <+> pp_lo_reg
         pp_hi_reg = pprReg L hi_reg
         pp_lo_reg = pprReg L lo_reg
     in     
         vcat [
            text "\t# BEGIN " <> fakeInsn,
            text "\tpushl" <+> pp_hi_reg <> text" ;  pushl" <+> pp_lo_reg,
            text "\tpushl %eax ; pushl %edx",
            text "\tmovl 12(%esp), %eax ; imull 8(%esp)",
            text "\tmovl %edx, 12(%esp) ; movl %eax, 8(%esp)",
            text "\tpopl %edx ; popl %eax",
            text "\tpopl" <+> pp_lo_reg <> text " ;  popl" <+> pp_hi_reg,
            text "\t# END   " <> fakeInsn
         ]


--------------------------

-- coerce %st(0) to the specified size
gcoerceto DF = empty
gcoerceto  F = empty --text "subl $4,%esp ; fstps (%esp) ; flds (%esp) ; addl $4,%esp ; "

gpush reg offset
   = hcat [text "ffree %st(7) ; fld ", greg reg offset]
gpop reg offset
   = hcat [text "fstp ", greg reg offset]

bogus = text "\tbogus"
greg reg offset = text "%st(" <> int (gregno reg - 8+offset) <> char ')'
gsemi = text " ; "
gtab  = char '\t'
gsp   = char ' '

gregno (RealReg i) = i
gregno other       = --pprPanic "gregno" (ppr other)
                     999   -- bogus; only needed for debug printing

pprG :: Instr -> Doc -> Doc
pprG fake actual
   = (char '#' <> pprGInstr fake) $$ actual

pprGInstr (GMOV src dst)   = pprSizeRegReg SLIT("gmov") DF src dst
pprGInstr (GLD sz src dst) = pprSizeAddrReg SLIT("gld") sz src dst
pprGInstr (GST sz src dst) = pprSizeRegAddr SLIT("gst") sz src dst

pprGInstr (GLDZ dst) = pprSizeReg SLIT("gldz") DF dst
pprGInstr (GLD1 dst) = pprSizeReg SLIT("gld1") DF dst

pprGInstr (GFTOI src dst) = pprSizeSizeRegReg SLIT("gftoi") F L  src dst
pprGInstr (GDTOI src dst) = pprSizeSizeRegReg SLIT("gdtoi") DF L src dst

pprGInstr (GITOF src dst) = pprSizeSizeRegReg SLIT("gitof") L F  src dst
pprGInstr (GITOD src dst) = pprSizeSizeRegReg SLIT("gitod") L DF src dst

pprGInstr (GCMP co src dst) = pprCondRegReg SLIT("gcmp_") DF co src dst
pprGInstr (GABS sz src dst) = pprSizeRegReg SLIT("gabs") sz src dst
pprGInstr (GNEG sz src dst) = pprSizeRegReg SLIT("gneg") sz src dst
pprGInstr (GSQRT sz src dst) = pprSizeRegReg SLIT("gsqrt") sz src dst
pprGInstr (GSIN sz src dst) = pprSizeRegReg SLIT("gsin") sz src dst
pprGInstr (GCOS sz src dst) = pprSizeRegReg SLIT("gcos") sz src dst
pprGInstr (GTAN sz src dst) = pprSizeRegReg SLIT("gtan") sz src dst

pprGInstr (GADD sz src1 src2 dst) = pprSizeRegRegReg SLIT("gadd") sz src1 src2 dst
pprGInstr (GSUB sz src1 src2 dst) = pprSizeRegRegReg SLIT("gsub") sz src1 src2 dst
pprGInstr (GMUL sz src1 src2 dst) = pprSizeRegRegReg SLIT("gmul") sz src1 src2 dst
pprGInstr (GDIV sz src1 src2 dst) = pprSizeRegRegReg SLIT("gdiv") sz src1 src2 dst
\end{code}

Continue with I386-only printing bits and bobs:
\begin{code}
pprDollImm :: Imm -> Doc

pprDollImm i =  ptext SLIT("$") <> pprImm i

pprOperand :: Size -> Operand -> Doc
pprOperand s (OpReg r)   = pprReg s r
pprOperand s (OpImm i)   = pprDollImm i
pprOperand s (OpAddr ea) = pprAddr ea

pprSizeImmOp :: LitString -> Size -> Imm -> Operand -> Doc
pprSizeImmOp name size imm op1
  = hcat [
        char '\t',
	ptext name,
	pprSize size,
	space,
	char '$',
	pprImm imm,
	comma,
	pprOperand size op1
    ]
	
pprSizeOp :: LitString -> Size -> Operand -> Doc
pprSizeOp name size op1
  = hcat [
    	char '\t',
	ptext name,
    	pprSize size,
	space,
	pprOperand size op1
    ]

pprSizeOpOp :: LitString -> Size -> Operand -> Operand -> Doc
pprSizeOpOp name size op1 op2
  = hcat [
    	char '\t',
	ptext name,
    	pprSize size,
	space,
	pprOperand size op1,
	comma,
	pprOperand size op2
    ]

pprSizeByteOpOp :: LitString -> Size -> Operand -> Operand -> Doc
pprSizeByteOpOp name size op1 op2
  = hcat [
    	char '\t',
	ptext name,
    	pprSize size,
	space,
	pprOperand B op1,
	comma,
	pprOperand size op2
    ]

pprSizeOpReg :: LitString -> Size -> Operand -> Reg -> Doc
pprSizeOpReg name size op1 reg
  = hcat [
    	char '\t',
	ptext name,
    	pprSize size,
	space,
	pprOperand size op1,
	comma,
	pprReg size reg
    ]

pprSizeReg :: LitString -> Size -> Reg -> Doc
pprSizeReg name size reg1
  = hcat [
    	char '\t',
	ptext name,
    	pprSize size,
	space,
	pprReg size reg1
    ]

pprSizeRegReg :: LitString -> Size -> Reg -> Reg -> Doc
pprSizeRegReg name size reg1 reg2
  = hcat [
    	char '\t',
	ptext name,
    	pprSize size,
	space,
	pprReg size reg1,
        comma,
        pprReg size reg2
    ]

pprCondRegReg :: LitString -> Size -> Cond -> Reg -> Reg -> Doc
pprCondRegReg name size cond reg1 reg2
  = hcat [
    	char '\t',
	ptext name,
    	pprCond cond,
	space,
	pprReg size reg1,
        comma,
        pprReg size reg2
    ]

pprSizeSizeRegReg :: LitString -> Size -> Size -> Reg -> Reg -> Doc
pprSizeSizeRegReg name size1 size2 reg1 reg2
  = hcat [
    	char '\t',
	ptext name,
    	pprSize size1,
        pprSize size2,
	space,
	pprReg size1 reg1,

        comma,
        pprReg size2 reg2
    ]

pprSizeRegRegReg :: LitString -> Size -> Reg -> Reg -> Reg -> Doc
pprSizeRegRegReg name size reg1 reg2 reg3
  = hcat [
    	char '\t',
	ptext name,
    	pprSize size,
	space,
	pprReg size reg1,
        comma,
        pprReg size reg2,
        comma,
        pprReg size reg3
    ]

pprSizeAddr :: LitString -> Size -> MachRegsAddr -> Doc
pprSizeAddr name size op
  = hcat [
    	char '\t',
	ptext name,
    	pprSize size,
	space,
	pprAddr op
    ]

pprSizeAddrReg :: LitString -> Size -> MachRegsAddr -> Reg -> Doc
pprSizeAddrReg name size op dst
  = hcat [
    	char '\t',
	ptext name,
    	pprSize size,
	space,
	pprAddr op,
	comma,
	pprReg size dst
    ]

pprSizeRegAddr :: LitString -> Size -> Reg -> MachRegsAddr -> Doc
pprSizeRegAddr name size src op
  = hcat [
    	char '\t',
	ptext name,
    	pprSize size,
	space,
	pprReg size src,
	comma,
	pprAddr op
    ]

pprOpOp :: LitString -> Size -> Operand -> Operand -> Doc
pprOpOp name size op1 op2
  = hcat [
    	char '\t',
	ptext name, space,
	pprOperand size op1,
	comma,
	pprOperand size op2
    ]

pprSizeOpOpCoerce :: LitString -> Size -> Size -> Operand -> Operand -> Doc
pprSizeOpOpCoerce name size1 size2 op1 op2
  = hcat [ char '\t', ptext name, pprSize size1, pprSize size2, space,
	pprOperand size1 op1,
	comma,
	pprOperand size2 op2
    ]

pprCondInstr :: LitString -> Cond -> Doc -> Doc
pprCondInstr name cond arg
  = hcat [ char '\t', ptext name, pprCond cond, space, arg]

#endif /* i386_TARGET_ARCH */
\end{code}

%************************************************************************
%*									*
\subsubsection{@pprInstr@ for a SPARC}
%*									*
%************************************************************************

\begin{code}
#if sparc_TARGET_ARCH

-- a clumsy hack for now, to handle possible double alignment problems

-- even clumsier, to allow for RegReg regs that show when doing indexed
-- reads (bytearrays).
--

-- Translate to the following:
--    add g1,g2,g1
--    ld  [g1],%fn
--    ld  [g1+4],%f(n+1)
--    sub g1,g2,g1           -- to restore g1
pprInstr (LD DF (AddrRegReg g1 g2) reg)
  = vcat [
       hcat [ptext SLIT("\tadd\t"), pprReg g1,comma,pprReg g2,comma,pprReg g1],
       hcat [pp_ld_lbracket, pprReg g1, pp_rbracket_comma, pprReg reg],
       hcat [pp_ld_lbracket, pprReg g1, ptext SLIT("+4]"), comma, pprReg (fPair reg)],
       hcat [ptext SLIT("\tsub\t"), pprReg g1,comma,pprReg g2,comma,pprReg g1]
    ]

-- Translate to
--    ld  [addr],%fn
--    ld  [addr+4],%f(n+1)
pprInstr (LD DF addr reg) | isJust off_addr
  = vcat [
       hcat [pp_ld_lbracket, pprAddr addr, pp_rbracket_comma, pprReg reg],
       hcat [pp_ld_lbracket, pprAddr addr2, pp_rbracket_comma,pprReg (fPair reg)]
    ]
  where
    off_addr = addrOffset addr 4
    addr2 = case off_addr of Just x -> x


pprInstr (LD size addr reg)
  = hcat [
       ptext SLIT("\tld"),
       pprSize size,
       char '\t',
       lbrack,
       pprAddr addr,
       pp_rbracket_comma,
       pprReg reg
    ]

-- The same clumsy hack as above

-- Translate to the following:
--    add g1,g2,g1
--    st  %fn,[g1]
--    st  %f(n+1),[g1+4]
--    sub g1,g2,g1           -- to restore g1
pprInstr (ST DF reg (AddrRegReg g1 g2))
 = vcat [
       hcat [ptext SLIT("\tadd\t"), pprReg g1,comma,pprReg g2,comma,pprReg g1],
       hcat [ptext SLIT("\tst\t"), pprReg reg, pp_comma_lbracket, 
             pprReg g1,	rbrack],
       hcat [ptext SLIT("\tst\t"), pprReg (fPair reg), pp_comma_lbracket,
             pprReg g1, ptext SLIT("+4]")],
       hcat [ptext SLIT("\tsub\t"), pprReg g1,comma,pprReg g2,comma,pprReg g1]
    ]

-- Translate to
--    st  %fn,[addr]
--    st  %f(n+1),[addr+4]
pprInstr (ST DF reg addr) | isJust off_addr 
 = vcat [
      hcat [ptext SLIT("\tst\t"), pprReg reg, pp_comma_lbracket, 
            pprAddr addr, rbrack],
      hcat [ptext SLIT("\tst\t"), pprReg (fPair reg), pp_comma_lbracket,
            pprAddr addr2, rbrack]
    ]
  where
    off_addr = addrOffset addr 4
    addr2 = case off_addr of Just x -> x

-- no distinction is made between signed and unsigned bytes on stores for the
-- Sparc opcodes (at least I cannot see any, and gas is nagging me --SOF),
-- so we call a special-purpose pprSize for ST..

pprInstr (ST size reg addr)
  = hcat [
       ptext SLIT("\tst"),
       pprStSize size,
       char '\t',
       pprReg reg,
       pp_comma_lbracket,
       pprAddr addr,
       rbrack
    ]

pprInstr (ADD x cc reg1 ri reg2)
  | not x && not cc && riZero ri
  = hcat [ ptext SLIT("\tmov\t"), pprReg reg1, comma, pprReg reg2 ]
  | otherwise
  = pprRegRIReg (if x then SLIT("addx") else SLIT("add")) cc reg1 ri reg2

pprInstr (SUB x cc reg1 ri reg2)
  | not x && cc && reg2 == g0
  = hcat [ ptext SLIT("\tcmp\t"), pprReg reg1, comma, pprRI ri ]
  | not x && not cc && riZero ri
  = hcat [ ptext SLIT("\tmov\t"), pprReg reg1, comma, pprReg reg2 ]
  | otherwise
  = pprRegRIReg (if x then SLIT("subx") else SLIT("sub")) cc reg1 ri reg2

pprInstr (AND  b reg1 ri reg2) = pprRegRIReg SLIT("and")  b reg1 ri reg2
pprInstr (ANDN b reg1 ri reg2) = pprRegRIReg SLIT("andn") b reg1 ri reg2

pprInstr (OR b reg1 ri reg2)
  | not b && reg1 == g0
  = let doit = hcat [ ptext SLIT("\tmov\t"), pprRI ri, comma, pprReg reg2 ]
    in  case ri of
           RIReg rrr | rrr == reg2 -> empty
           other                   -> doit
  | otherwise
  = pprRegRIReg SLIT("or") b reg1 ri reg2

pprInstr (ORN b reg1 ri reg2) = pprRegRIReg SLIT("orn") b reg1 ri reg2

pprInstr (XOR  b reg1 ri reg2) = pprRegRIReg SLIT("xor")  b reg1 ri reg2
pprInstr (XNOR b reg1 ri reg2) = pprRegRIReg SLIT("xnor") b reg1 ri reg2

pprInstr (SLL reg1 ri reg2) = pprRegRIReg SLIT("sll") False reg1 ri reg2
pprInstr (SRL reg1 ri reg2) = pprRegRIReg SLIT("srl") False reg1 ri reg2
pprInstr (SRA reg1 ri reg2) = pprRegRIReg SLIT("sra") False reg1 ri reg2

pprInstr (RDY rd) = ptext SLIT("\trd\t%y,") <> pprReg rd
pprInstr (SMUL b reg1 ri reg2) = pprRegRIReg SLIT("smul")  b reg1 ri reg2
pprInstr (UMUL b reg1 ri reg2) = pprRegRIReg SLIT("umul")  b reg1 ri reg2

pprInstr (SETHI imm reg)
  = hcat [
	ptext SLIT("\tsethi\t"),
	pprImm imm,
	comma,
	pprReg reg
    ]

pprInstr NOP = ptext SLIT("\tnop")

pprInstr (FABS F reg1 reg2) = pprSizeRegReg SLIT("fabs") F reg1 reg2
pprInstr (FABS DF reg1 reg2)
  = (<>) (pprSizeRegReg SLIT("fabs") F reg1 reg2)
    (if (reg1 == reg2) then empty
     else (<>) (char '\n')
    	  (pprSizeRegReg SLIT("fmov") F (fPair reg1) (fPair reg2)))

pprInstr (FADD size reg1 reg2 reg3)
  = pprSizeRegRegReg SLIT("fadd") size reg1 reg2 reg3
pprInstr (FCMP e size reg1 reg2)
  = pprSizeRegReg (if e then SLIT("fcmpe") else SLIT("fcmp")) size reg1 reg2
pprInstr (FDIV size reg1 reg2 reg3)
  = pprSizeRegRegReg SLIT("fdiv") size reg1 reg2 reg3

pprInstr (FMOV F reg1 reg2) = pprSizeRegReg SLIT("fmov") F reg1 reg2
pprInstr (FMOV DF reg1 reg2)
  = (<>) (pprSizeRegReg SLIT("fmov") F reg1 reg2)
    (if (reg1 == reg2) then empty
     else (<>) (char '\n')
    	  (pprSizeRegReg SLIT("fmov") F (fPair reg1) (fPair reg2)))

pprInstr (FMUL size reg1 reg2 reg3)
  = pprSizeRegRegReg SLIT("fmul") size reg1 reg2 reg3

pprInstr (FNEG F reg1 reg2) = pprSizeRegReg SLIT("fneg") F reg1 reg2
pprInstr (FNEG DF reg1 reg2)
  = (<>) (pprSizeRegReg SLIT("fneg") F reg1 reg2)
    (if (reg1 == reg2) then empty
     else (<>) (char '\n')
    	  (pprSizeRegReg SLIT("fmov") F (fPair reg1) (fPair reg2)))

pprInstr (FSQRT size reg1 reg2)     = pprSizeRegReg SLIT("fsqrt") size reg1 reg2
pprInstr (FSUB size reg1 reg2 reg3) = pprSizeRegRegReg SLIT("fsub") size reg1 reg2 reg3
pprInstr (FxTOy size1 size2 reg1 reg2)
  = hcat [
    	ptext SLIT("\tf"),
	ptext
    	(case size1 of
    	    W  -> SLIT("ito")
    	    F  -> SLIT("sto")
    	    DF -> SLIT("dto")),
	ptext
    	(case size2 of
    	    W  -> SLIT("i\t")
    	    F  -> SLIT("s\t")
    	    DF -> SLIT("d\t")),
	pprReg reg1, comma, pprReg reg2
    ]


pprInstr (BI cond b lab)
  = hcat [
	ptext SLIT("\tb"), pprCond cond,
	if b then pp_comma_a else empty,
	char '\t',
	pprImm lab
    ]

pprInstr (BF cond b lab)
  = hcat [
	ptext SLIT("\tfb"), pprCond cond,
	if b then pp_comma_a else empty,
	char '\t',
	pprImm lab
    ]

pprInstr (JMP dsts addr) = (<>) (ptext SLIT("\tjmp\t")) (pprAddr addr)

pprInstr (CALL (Left imm) n _)
  = hcat [ ptext SLIT("\tcall\t"), pprImm imm, comma, int n ]
pprInstr (CALL (Right reg) n _)
  = hcat [ ptext SLIT("\tcall\t"), pprReg reg, comma, int n ]
\end{code}

Continue with SPARC-only printing bits and bobs:
\begin{code}
pprRI :: RI -> Doc
pprRI (RIReg r) = pprReg r
pprRI (RIImm r) = pprImm r

pprSizeRegReg :: LitString -> Size -> Reg -> Reg -> Doc
pprSizeRegReg name size reg1 reg2
  = hcat [
    	char '\t',
	ptext name,
    	(case size of
    	    F  -> ptext SLIT("s\t")
    	    DF -> ptext SLIT("d\t")),
	pprReg reg1,
	comma,
	pprReg reg2
    ]

pprSizeRegRegReg :: LitString -> Size -> Reg -> Reg -> Reg -> Doc
pprSizeRegRegReg name size reg1 reg2 reg3
  = hcat [
    	char '\t',
	ptext name,
    	(case size of
    	    F  -> ptext SLIT("s\t")
    	    DF -> ptext SLIT("d\t")),
	pprReg reg1,
	comma,
	pprReg reg2,
	comma,
	pprReg reg3
    ]

pprRegRIReg :: LitString -> Bool -> Reg -> RI -> Reg -> Doc
pprRegRIReg name b reg1 ri reg2
  = hcat [
	char '\t',
	ptext name,
	if b then ptext SLIT("cc\t") else char '\t',
	pprReg reg1,
	comma,
	pprRI ri,
	comma,
	pprReg reg2
    ]

pprRIReg :: LitString -> Bool -> RI -> Reg -> Doc
pprRIReg name b ri reg1
  = hcat [
	char '\t',
	ptext name,
	if b then ptext SLIT("cc\t") else char '\t',
	pprRI ri,
	comma,
	pprReg reg1
    ]

pp_ld_lbracket    = ptext SLIT("\tld\t[")
pp_rbracket_comma = text "],"
pp_comma_lbracket = text ",["
pp_comma_a	  = text ",a"

#endif /* sparc_TARGET_ARCH */
\end{code}

%************************************************************************
%*									*
\subsubsection{@pprInstr@ for PowerPC}
%*									*
%************************************************************************

\begin{code}
#if powerpc_TARGET_ARCH
pprInstr (LD sz reg addr) = hcat [
	char '\t',
	ptext SLIT("l"),
	ptext (case sz of
	    B   -> SLIT("ba")
	    Bu  -> SLIT("bz")
	    H   -> SLIT("ha")
	    Hu  -> SLIT("hz")
	    W   -> SLIT("wz")
	    F   -> SLIT("fs")
	    DF  -> SLIT("fd")),
	char '\t',
	pprReg reg,
	ptext SLIT(", "),
	pprAddr addr
    ]
pprInstr (ST sz reg addr) = hcat [
	char '\t',
	ptext SLIT("st"),
	pprSize sz,
	char '\t',
	pprReg reg,
	ptext SLIT(", "),
	pprAddr addr
    ]
pprInstr (STU sz reg addr) = hcat [
	char '\t',
	ptext SLIT("st"),
	pprSize sz,
	ptext SLIT("u\t"),
	pprReg reg,
	ptext SLIT(", "),
	pprAddr addr
    ]
pprInstr (LIS reg imm) = hcat [
	char '\t',
	ptext SLIT("lis"),
	char '\t',
	pprReg reg,
	ptext SLIT(", "),
	pprImm imm
    ]
pprInstr (LI reg imm) = hcat [
	char '\t',
	ptext SLIT("li"),
	char '\t',
	pprReg reg,
	ptext SLIT(", "),
	pprImm imm
    ]
pprInstr (MR reg1 reg2) 
    | reg1 == reg2 = empty
    | otherwise = hcat [
	char '\t',
	case regClass reg1 of
	    RcInteger -> ptext SLIT("mr")
	    _ -> ptext SLIT("fmr"),
	char '\t',
	pprReg reg1,
	ptext SLIT(", "),
	pprReg reg2
    ]
pprInstr (CMP sz reg ri) = hcat [
	char '\t',
	op,
	char '\t',
	pprReg reg,
	ptext SLIT(", "),
	pprRI ri
    ]
    where
	op = hcat [
		ptext SLIT("cmp"),
		pprSize sz,
		case ri of
		    RIReg _ -> empty
		    RIImm _ -> char 'i'
	    ]
pprInstr (CMPL sz reg ri) = hcat [
	char '\t',
	op,
	char '\t',
	pprReg reg,
	ptext SLIT(", "),
	pprRI ri
    ]
    where
	op = hcat [
		ptext SLIT("cmpl"),
		pprSize sz,
		case ri of
		    RIReg _ -> empty
		    RIImm _ -> char 'i'
	    ]
pprInstr (BCC cond lbl) = hcat [
	char '\t',
	ptext SLIT("b"),
	pprCond cond,
	char '\t',
	pprCLabel_asm lbl
    ]

pprInstr (MTCTR reg) = hcat [
	char '\t',
	ptext SLIT("mtctr"),
	char '\t',
	pprReg reg
    ]
pprInstr (BCTR _) = hcat [
	char '\t',
	ptext SLIT("bctr")
    ]
pprInstr (BL imm _) = hcat [
	char '\t',
	ptext SLIT("bl"),
	char '\t',
	pprImm imm
    ]
pprInstr (BCTRL _) = hcat [
	char '\t',
	ptext SLIT("bctrl")
    ]
pprInstr (ADD reg1 reg2 ri) = pprLogic SLIT("add") reg1 reg2 ri
pprInstr (SUBF reg1 reg2 reg3) = pprLogic SLIT("subf") reg1 reg2 (RIReg reg3)
pprInstr (MULLW reg1 reg2 ri@(RIReg _)) = pprLogic SLIT("mullw") reg1 reg2 ri
pprInstr (MULLW reg1 reg2 ri@(RIImm _)) = pprLogic SLIT("mull") reg1 reg2 ri
pprInstr (DIVW reg1 reg2 reg3) = pprLogic SLIT("divw") reg1 reg2 (RIReg reg3)
pprInstr (DIVWU reg1 reg2 reg3) = pprLogic SLIT("divwu") reg1 reg2 (RIReg reg3)

    	-- for some reason, "andi" doesn't exist.
	-- we'll use "andi." instead.
pprInstr (AND reg1 reg2 (RIImm imm)) = hcat [
	char '\t',
	ptext SLIT("andi."),
	char '\t',
	pprReg reg1,
	ptext SLIT(", "),
	pprReg reg2,
	ptext SLIT(", "),
	pprImm imm
    ]
pprInstr (AND reg1 reg2 ri) = pprLogic SLIT("and") reg1 reg2 (toUI16 ri)

pprInstr (OR reg1 reg2 ri) = pprLogic SLIT("or") reg1 reg2 (toUI16 ri)
pprInstr (XOR reg1 reg2 ri) = pprLogic SLIT("xor") reg1 reg2 (toUI16 ri)

pprInstr (XORIS reg1 reg2 imm) = hcat [
	char '\t',
	ptext SLIT("xoris"),
	char '\t',
	pprReg reg1,
	ptext SLIT(", "),
	pprReg reg2,
	ptext SLIT(", "),
	pprImm imm
    ]

pprInstr (SLW reg1 reg2 ri) = pprLogic SLIT("slw") reg1 reg2 ri
pprInstr (SRW reg1 reg2 ri) = pprLogic SLIT("srw") reg1 reg2 ri
pprInstr (SRAW reg1 reg2 ri) = pprLogic SLIT("sraw") reg1 reg2 ri
pprInstr (NEG reg1 reg2) = pprUnary SLIT("neg") reg1 reg2
pprInstr (NOT reg1 reg2) = pprUnary SLIT("not") reg1 reg2

pprInstr (FADD sz reg1 reg2 reg3) = pprBinaryF SLIT("fadd") sz reg1 reg2 reg3
pprInstr (FSUB sz reg1 reg2 reg3) = pprBinaryF SLIT("fsub") sz reg1 reg2 reg3
pprInstr (FMUL sz reg1 reg2 reg3) = pprBinaryF SLIT("fmul") sz reg1 reg2 reg3
pprInstr (FDIV sz reg1 reg2 reg3) = pprBinaryF SLIT("fdiv") sz reg1 reg2 reg3
pprInstr (FNEG reg1 reg2) = pprUnary SLIT("fneg") reg1 reg2

pprInstr (FCMP reg1 reg2) = hcat [
	char '\t',
	ptext SLIT("fcmpu\tcr0, "),
	    -- Note: we're using fcmpu, not fcmpo
	    -- The difference is with fcmpo, compare with NaN is an invalid operation.
	    -- We don't handle invalid fp ops, so we don't care
	pprReg reg1,
	ptext SLIT(", "),
	pprReg reg2
    ]

pprInstr (FCTIWZ reg1 reg2) = pprUnary SLIT("fctiwz") reg1 reg2

pprInstr _ = ptext SLIT("something")

pprLogic op reg1 reg2 ri = hcat [
	char '\t',
	ptext op,
	case ri of
	    RIReg _ -> empty
	    RIImm _ -> char 'i',
	char '\t',
	pprReg reg1,
	ptext SLIT(", "),
	pprReg reg2,
	ptext SLIT(", "),
	pprRI ri
    ]
    
pprUnary op reg1 reg2 = hcat [
	char '\t',
	ptext op,
	char '\t',
	pprReg reg1,
	ptext SLIT(", "),
	pprReg reg2
    ]
    
pprBinaryF op sz reg1 reg2 reg3 = hcat [
	char '\t',
	ptext op,
	pprFSize sz,
	char '\t',
	pprReg reg1,
	ptext SLIT(", "),
	pprReg reg2,
	ptext SLIT(", "),
	pprReg reg3
    ]
    
pprRI :: RI -> Doc
pprRI (RIReg r) = pprReg r
pprRI (RIImm r) = pprImm r

pprFSize DF = empty
pprFSize F = char 's'

-- hack to ensure that negative vals come out in non-negative form
-- (assuming that fromIntegral{Int->Word16} will do a 'c-style'
-- conversion, and not throw a fit/exception.)
toUI16 :: RI -> RI
toUI16 (RIImm (ImmInt x)) 
  | x < 0 = RIImm (ImmInt (fromIntegral ((fromIntegral x) :: Word16)))
toUI16 (RIImm (ImmInteger x)) 
  | x < 0 = RIImm (ImmInt (fromIntegral ((fromIntegral x) :: Word16)))
toUI16 x = x

{-
  The Mach-O object file format used in Darwin/Mac OS X needs a so-called
  "symbol stub" for every function that might be imported from a dynamic
  library.
  The stubs are always the same, and they are all output at the end of the
  generated assembly (see AsmCodeGen.lhs), so we don't use the Instr datatype.
  Instead, we just pretty-print it directly.
-}

#if darwin_TARGET_OS
pprDyldSymbolStub fn =
    vcat [
	ptext SLIT(".symbol_stub"),
	ptext SLIT("L_") <> ftext fn <> ptext SLIT("$stub:"),
	    ptext SLIT("\t.indirect_symbol _") <> ftext fn,
	    ptext SLIT("\tlis r11,ha16(L_") <> ftext fn <> ptext SLIT("$lazy_ptr)"),
	    ptext SLIT("\tlwz r12,lo16(L_") <> ftext fn <> ptext SLIT("$lazy_ptr)(r11)"),
	    ptext SLIT("\tmtctr r12"),
	    ptext SLIT("\taddi r11,r11,lo16(L_") <> ftext fn <> ptext SLIT("$lazy_ptr)"),
	    ptext SLIT("\tbctr"),
	ptext SLIT(".lazy_symbol_pointer"),
	ptext SLIT("L_") <> ftext fn <> ptext SLIT("$lazy_ptr:"),
	    ptext SLIT("\t.indirect_symbol _") <> ftext fn,
	    ptext SLIT("\t.long dyld_stub_binding_helper")
    ]
#endif


#endif /* powerpc_TARGET_ARCH */
\end{code}

\begin{code}
#if __GLASGOW_HASKELL__ >= 504
newFloatArray :: (Int,Int) -> ST s (STUArray s Int Float)
newFloatArray = newArray_

newDoubleArray :: (Int,Int) -> ST s (STUArray s Int Double)
newDoubleArray = newArray_

castFloatToCharArray :: STUArray s Int Float -> ST s (STUArray s Int Word8)
castFloatToCharArray = castSTUArray

castDoubleToCharArray :: STUArray s Int Double -> ST s (STUArray s Int Word8)
castDoubleToCharArray = castSTUArray

writeFloatArray :: STUArray s Int Float -> Int -> Float -> ST s ()
writeFloatArray = writeArray

writeDoubleArray :: STUArray s Int Double -> Int -> Double -> ST s ()
writeDoubleArray = writeArray

readCharArray :: STUArray s Int Word8 -> Int -> ST s Char
readCharArray arr i = do 
  w <- readArray arr i
  return $! (chr (fromIntegral w))

#else

castFloatToCharArray :: MutableByteArray s t -> ST s (MutableByteArray s t)
castFloatToCharArray = return

castDoubleToCharArray :: MutableByteArray s t -> ST s (MutableByteArray s t)


castDoubleToCharArray = return

#endif

-- floatToBytes and doubleToBytes convert to the host's byte
-- order.  Providing that we're not cross-compiling for a 
-- target with the opposite endianness, this should work ok
-- on all targets.

-- ToDo: this stuff is very similar to the shenanigans in PprAbs,
-- could they be merged?

floatToBytes :: Float -> [Int]
floatToBytes f
   = runST (do
        arr <- newFloatArray ((0::Int),3)
        writeFloatArray arr 0 f
	arr <- castFloatToCharArray arr
        i0 <- readCharArray arr 0
        i1 <- readCharArray arr 1
        i2 <- readCharArray arr 2
        i3 <- readCharArray arr 3
        return (map ord [i0,i1,i2,i3])
     )

doubleToBytes :: Double -> [Int]
doubleToBytes d
   = runST (do
        arr <- newDoubleArray ((0::Int),7)
        writeDoubleArray arr 0 d
	arr <- castDoubleToCharArray arr
        i0 <- readCharArray arr 0
        i1 <- readCharArray arr 1
        i2 <- readCharArray arr 2
        i3 <- readCharArray arr 3
        i4 <- readCharArray arr 4
        i5 <- readCharArray arr 5
        i6 <- readCharArray arr 6
        i7 <- readCharArray arr 7
        return (map ord [i0,i1,i2,i3,i4,i5,i6,i7])
     )
\end{code}
