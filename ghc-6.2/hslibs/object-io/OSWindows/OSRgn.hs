{-# OPTIONS -#include "OSWindows\Windows_C_12\cpicture_121.h" #-}

module OSRgn ( OSRgnHandle
             , osNewRgn, osNewRectRgn, osDisposeRgn, osRectRgn, osPolyRgn
             , osSectRgn, osUnionRgn, osDiffRgn
             , osGetRgnBox, osIsEmptyRgn, osNoRgn
             ) where


--	********************************************************************************
--	Clean to Haskell Standard Object I/O library, version 1.2
--	
--	Osrgn contains OS operations to manage regions.
--	********************************************************************************


import OSTypes
import PictCCall_12
import RgnCCall_12
import Foreign.Ptr(Ptr(..), nullPtr)


type	OSRgnHandle
	= Ptr ()
type	OSPointH
	= Ptr Int

osNoRgn = nullPtr

--	Region creation and disposal operations.
osNewRgn :: IO OSRgnHandle
osNewRgn = winCreateRectRgn 0 0 1 1

osNewRectRgn :: Rect -> IO OSRgnHandle
osNewRectRgn (Rect {rleft=rleft,rtop=rtop,rright=rright,rbottom=rbottom})
	= winCreateRectRgn rleft rtop rright rbottom

osDisposeRgn :: OSRgnHandle -> IO ()
osDisposeRgn osrgn = winDeleteObject osrgn


--	Setting the shape of a Region.
osRectRgn :: Rect -> OSRgnHandle -> IO OSRgnHandle
osRectRgn (Rect {rleft=rleft,rtop=rtop,rright=rright,rbottom=rbottom}) osrgn
	= winSetRgnToRect rleft rtop rright rbottom osrgn

osPolyRgn :: (Int,Int) -> [(Int,Int)] -> OSRgnHandle -> IO OSRgnHandle
osPolyRgn base shape osrgn
	= do {
		osrgn1 <- winCombineRgn osrgn osrgn osrgn rgn_DIFF;
		if   len==0
		then return osrgn1
		else 
		do {
			shapeH <- winAllocPolyShape len;
			setPolyShape shapeH 0 base shape;
			prgn   <- winCreatePolygonRgn shapeH len winding;
			osrgn2 <- winCombineRgn osrgn1 prgn prgn rgn_COPY;
			winDeleteObject prgn;
			winFreePolyShape shapeH;
			return osrgn2
		}
	  }
	where
		len = length shape
		
		setPolyShape :: OSPointH -> Int -> (Int,Int) -> [(Int,Int)] -> IO ()
		setPolyShape shapeH i (x,y) ((vx,vy):vs)
			= winSetPolyPoint i x y shapeH >> setPolyShape shapeH (i+1) (x+vx,y+vy) vs
		setPolyShape _ _ _ _ = return ()


--	Combining the shapes of two Regions into a new Region.
osSectRgn :: OSRgnHandle -> OSRgnHandle -> IO OSRgnHandle
osSectRgn rgn1 rgn2
	= do {
		rrgn  <- winCreateRectRgn 0 0 1 1;
		rrgn1 <- winCombineRgn rrgn rgn1 rgn2 rgn_AND;
		return rrgn1
	  }

osUnionRgn :: OSRgnHandle -> OSRgnHandle -> IO OSRgnHandle
osUnionRgn rgn1 rgn2
	= do {
		rrgn  <- winCreateRectRgn 0 0 1 1;
		rrgn1 <- winCombineRgn rrgn rgn1 rgn2 rgn_OR;
		return rrgn1
	  }

osDiffRgn :: OSRgnHandle -> OSRgnHandle -> IO OSRgnHandle
osDiffRgn rgn1 rgn2
	= do {
		rrgn  <- winCreateRectRgn 0 0 1 1;
		rrgn1 <- winCombineRgn rrgn rgn1 rgn2 rgn_DIFF;
		return rrgn1
	  }


--	Region property access functions.
osGetRgnBox :: OSRgnHandle -> IO (Bool,Rect)
osGetRgnBox rgn
	= do {
		(l,t, r,b, isRect,_) <- winGetRgnBox rgn;
		return (isRect,Rect {rleft=l,rtop=t,rright=r,rbottom=b})
	  }

osIsEmptyRgn :: OSRgnHandle -> IO Bool
osIsEmptyRgn rgn
	= do {
		(_,_,_,_,_,isempty) <- winGetRgnBox rgn;
		return isempty
	  }
