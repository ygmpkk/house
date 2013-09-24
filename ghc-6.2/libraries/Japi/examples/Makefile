HCOPTS = -package japi -O

all: mandel2 canvas rubberband frame alert componentlistener borderpanel daemon

mandel2: mandel2.hs
	../../../ghc/compiler/ghc-inplace $(HCOPTS) mandel2.hs -o mandel2
canvas: canvas.hs
	../../../ghc/compiler/ghc-inplace $(HCOPTS) canvas.hs -o canvas
rubberband: rubberband.hs
	../../../ghc/compiler/ghc-inplace $(HCOPTS) rubberband.hs -o rubberband
frame: frame.hs
	../../../ghc/compiler/ghc-inplace $(HCOPTS) frame.hs -o frame
alert: alert.hs
	../../../ghc/compiler/ghc-inplace $(HCOPTS) alert.hs -o alert
componentlistener: componentlistener.hs
	../../../ghc/compiler/ghc-inplace $(HCOPTS) componentlistener.hs -o componentlistener
borderpanel: borderpanel.hs
	../../../ghc/compiler/ghc-inplace $(HCOPTS) borderpanel.hs -o borderpanel
daemon: daemon.hs
	../../../ghc/compiler/ghc-inplace $(HCOPTS) daemon.hs -o daemon

clean:
	rm -f *.o *.exe *.hi *~