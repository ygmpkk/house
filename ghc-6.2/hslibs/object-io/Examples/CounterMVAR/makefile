HC          = ghc-4.08.1 -static
HC_DIRS     = -i../../CleanStdEnv:../../OSWindows:../../ObjectIO:../../OSWindows/Windows_C_12 -I../../ObjectIO
MY_CONFIG   = -DMVAR=1
SYSLIB_OPTS = -syslib concurrent -syslib lang
HC_OPTS     = -fglasgow-exts $(SYSLIB_OPTS) -cpp $(EXTRA_HC_OPTS) $(HC_DIRS) $(MY_CONFIG)
LINK_OPTS   = -ladvapi32 -lcomctl32 -lcomdlg32 -lgdi32 -lole32 -lshell32 -luser32 -lwinmm -lwinspool -lwsock32
CC_OPTS     = -static
H_SRCS = Main.hs CounterControl.hs \
         ../../ObjectIO/Commondef.hs		../../ObjectIO/Controlaccess.hs		../../ObjectIO/Controlcreate.hs \
         ../../ObjectIO/Controlinternal.hs	../../ObjectIO/Controllayout.hs		../../ObjectIO/Controlvalidate.hs \
         ../../ObjectIO/Device.hs		../../ObjectIO/Deviceevents.hs		../../ObjectIO/Devicesystemstate.hs \
         ../../ObjectIO/Id.hs			../../ObjectIO/IOstate.hs		../../ObjectIO/Key.hs \
         ../../ObjectIO/Layout.hs		../../ObjectIO/Processdevice.hs		../../ObjectIO/Receiveraccess.hs \
         ../../ObjectIO/Receiverdefaccess.hs	../../ObjectIO/Receiverdevice.hs	../../ObjectIO/Receiverhandle.hs \
         ../../ObjectIO/Receiverid.hs		../../ObjectIO/StdReceiver.hs		../../ObjectIO/StdReceiverAttribute.hs \
         ../../ObjectIO/StdReceiverDef.hs	../../ObjectIO/Receivertable.hs		../../ObjectIO/Roundrobin.hs \
         ../../ObjectIO/Scheduler.hs		../../ObjectIO/StdControl.hs		../../ObjectIO/StdControlAttribute.hs \
         ../../ObjectIO/StdControlClass.hs	../../ObjectIO/StdControlDef.hs		../../ObjectIO/StdGUI.hs \
         ../../ObjectIO/StdId.hs		../../ObjectIO/StdIO.hs			../../ObjectIO/StdIOBasic.hs \
         ../../ObjectIO/StdIOCommon.hs		../../ObjectIO/StdKey.hs		../../ObjectIO/StdMaybe.hs \
         ../../ObjectIO/StdPictureDef.hs	../../ObjectIO/StdProcess.hs		../../ObjectIO/StdProcessAttribute.hs \
         ../../ObjectIO/StdProcessDef.hs	../../ObjectIO/StdWindow.hs		../../ObjectIO/StdWindowAttribute.hs \
         ../../ObjectIO/StdWindowDef.hs		../../ObjectIO/Systemid.hs		../../ObjectIO/Windowaccess.hs \
         ../../ObjectIO/Windowcreate.hs		../../ObjectIO/Windowdevice.hs		\
         ../../ObjectIO/WindowdeviceMVAR.hs	../../ObjectIO/Windowdispose.hs		../../ObjectIO/Windowhandle.hs \
         ../../ObjectIO/Windowvalidate.hs	../../ObjectIO/World.hs			../../ObjectIO/Wstate.hs \
         ../../ObjectIO/Wstateaccess.hs \
         \
         ../../CleanStdEnv/CleanExtras.hs	../../CleanStdEnv/CleanStdFunc.hs       ../../CleanStdEnv/CleanStdList.hs \
         ../../CleanStdEnv/CleanStdMisc.hs	../../CleanStdEnv/CleanStdTuple.hs \
         \
         ../../OSWindows/ClCCall_12.hs		../../OSWindows/ClCrossCall_12.hs	../../OSWindows/Cutil_12.hs \
         ../../OSWindows/MenuCrossCall_12.hs	../../OSWindows/Osdocumentinterface.hs	../../OSWindows/Osevent.hs \
         ../../OSWindows/Osfont.hs		../../OSWindows/Ospicture.hs		../../OSWindows/Osrgn.hs \
         ../../OSWindows/Ossystem.hs		../../OSWindows/Ostoolbar.hs		../../OSWindows/Ostoolbox.hs \
         ../../OSWindows/Ostypes.hs		../../OSWindows/Oswindow.hs		../../OSWindows/PictCCall_12.hs \
         ../../OSWindows/Processevent.hs	../../OSWindows/Receiverevent.hs	../../OSWindows/RgnCCall_12.hs \
         ../../OSWindows/Trace_12.hs		../../OSWindows/WindowCCall_12.hs	../../OSWindows/WindowCrossCall_12.hs \
         ../../OSWindows/Windowevent.hs
C_SRCS = ../../OSWindows/Windows_C_12/cAcceleratorTable_121.c \
         ../../OSWindows/Windows_C_12/cCCallSystem_121.c \
         ../../OSWindows/Windows_C_12/cCCallWindows_121.c \
         ../../OSWindows/Windows_C_12/cCrossCallClipboard_121.c \
         ../../OSWindows/Windows_C_12/cCrossCallCursor_121.c \
         ../../OSWindows/Windows_C_12/cCrossCallFileSelectors_121.c \
         ../../OSWindows/Windows_C_12/cCrossCallFont_121.c \
         ../../OSWindows/Windows_C_12/cCrossCallMenus_121.c \
         ../../OSWindows/Windows_C_12/cCrossCallPrinter_121.c \
         ../../OSWindows/Windows_C_12/cCrossCallProcedureTable_121.c \
         ../../OSWindows/Windows_C_12/cCrossCallWindows_121.c \
         ../../OSWindows/Windows_C_12/cCrossCall_121.c \
         ../../OSWindows/Windows_C_12/cCrossCallxDI_121.c \
         ../../OSWindows/Windows_C_12/cdebug_121.c \
         ../../OSWindows/Windows_C_12/cpicture_121.c \
         ../../OSWindows/Windows_C_12/cprinter_121.c \
         ../../OSWindows/Windows_C_12/util_121.c \
         ../../OSWindows/Windows_C_12/w.c

H_OBJS = $(patsubst %.hs,%.o,$(H_SRCS))
C_OBJS = $(patsubst %.c,%.o,$(C_SRCS))
OBJS   = $(H_OBJS) $(C_OBJS)

H_HIS  = $(patsubst %.hs,%.hi,$(H_SRCS))

.SUFFIXES : .o .hi .lhs .hc .s .hs .c
all :: Main.o Main.exe

clean:
	rm -f $(H_OBJS)

veryclean:
	rm -f $(H_OBJS) $(H_HIS)

Main.exe : $(OBJS)
	@rm -f $@
	$(HC) -o $@ $(OBJS) $(HC_OPTS) $(LINK_OPTS)

# Standard suffix rules
.o.hi:
	@:

.lhs.o:
	$(HC) -c $< $(HC_OPTS)

.hs.o:
	$(HC) -c $< $(HC_OPTS)

.c.o:
	$(HC) -c $< $(CC_OPTS)

depend ::
	$(HC) -M $(SYSLIB_OPTS) $(H_SRCS) $(HC_DIRS)
# DO NOT DELETE: Beginning of Haskell dependencies
Main.o : Main.hs
Main.o : ../../CleanStdEnv/CleanStdMisc.hi
Main.o : ./CounterControl.hi
Main.o : ../../ObjectIO/StdIO.hi
CounterControl.o : CounterControl.hs
CounterControl.o : ../../ObjectIO/StdIO.hi
../../ObjectIO/Commondef.o : ../../ObjectIO/Commondef.hs
../../ObjectIO/Commondef.o : ../../OSWindows/Ostypes.hi
../../ObjectIO/Commondef.o : ../../CleanStdEnv/CleanStdFunc.hi
../../ObjectIO/Commondef.o : ../../CleanStdEnv/CleanStdList.hi
../../ObjectIO/Commondef.o : ../../ObjectIO/StdIOCommon.hi
../../ObjectIO/Commondef.o : ../../CleanStdEnv/CleanExtras.hi
../../ObjectIO/Controlaccess.o : ../../ObjectIO/Controlaccess.hs
../../ObjectIO/Controlaccess.o : ../../CleanStdEnv/CleanStdList.hi
../../ObjectIO/Controlaccess.o : ../../ObjectIO/Commondef.hi
../../ObjectIO/Controlaccess.o : ../../ObjectIO/Id.hi
../../ObjectIO/Controlaccess.o : ../../ObjectIO/StdMaybe.hi
../../ObjectIO/Controlaccess.o : ../../ObjectIO/Windowaccess.hi
../../ObjectIO/Controlaccess.o : ../../ObjectIO/Wstateaccess.hi
../../ObjectIO/Controlcreate.o : ../../ObjectIO/Controlcreate.hs
../../ObjectIO/Controlcreate.o : ../../ObjectIO/Commondef.hi
../../ObjectIO/Controlcreate.o : ../../ObjectIO/Controllayout.hi
../../ObjectIO/Controlcreate.o : ../../ObjectIO/Controlvalidate.hi
../../ObjectIO/Controlcreate.o : ../../OSWindows/Oswindow.hi
../../ObjectIO/Controlcreate.o : ../../ObjectIO/StdControlAttribute.hi
../../ObjectIO/Controlcreate.o : ../../ObjectIO/Windowaccess.hi
../../ObjectIO/Controlinternal.o : ../../ObjectIO/Controlinternal.hs
../../ObjectIO/Controlinternal.o : ../../CleanStdEnv/CleanStdMisc.hi
../../ObjectIO/Controlinternal.o : ../../ObjectIO/Commondef.hi
../../ObjectIO/Controlinternal.o : ../../OSWindows/Oswindow.hi
../../ObjectIO/Controlinternal.o : ../../ObjectIO/Wstateaccess.hi
../../ObjectIO/Controlinternal.o : ../../ObjectIO/StdControlAttribute.hi
../../ObjectIO/Controlinternal.o : ../../ObjectIO/Controlvalidate.hi
../../ObjectIO/Controlinternal.o : ../../ObjectIO/Windowaccess.hi
../../ObjectIO/Controllayout.o : ../../ObjectIO/Controllayout.hs
../../ObjectIO/Controllayout.o : ../../CleanStdEnv/CleanStdList.hi
../../ObjectIO/Controllayout.o : ../../ObjectIO/Commondef.hi
../../ObjectIO/Controllayout.o : ../../ObjectIO/Id.hi
../../ObjectIO/Controllayout.o : ../../ObjectIO/Layout.hi
../../ObjectIO/Controllayout.o : ../../OSWindows/Oswindow.hi
../../ObjectIO/Controllayout.o : ../../ObjectIO/StdControlAttribute.hi
../../ObjectIO/Controllayout.o : ../../ObjectIO/Windowaccess.hi
../../ObjectIO/Controlvalidate.o : ../../ObjectIO/Controlvalidate.hs
../../ObjectIO/Controlvalidate.o : ../../ObjectIO/Commondef.hi
../../ObjectIO/Controlvalidate.o : ../../ObjectIO/Id.hi
../../ObjectIO/Controlvalidate.o : ../../OSWindows/Oswindow.hi
../../ObjectIO/Controlvalidate.o : ../../ObjectIO/Windowhandle.hi
../../ObjectIO/Device.o : ../../ObjectIO/Device.hs
../../ObjectIO/Deviceevents.o : ../../ObjectIO/Deviceevents.hs
../../ObjectIO/Deviceevents.o : ../../OSWindows/Osevent.hi
../../ObjectIO/Deviceevents.o : ../../OSWindows/Ostypes.hi
../../ObjectIO/Deviceevents.o : ../../ObjectIO/StdIOCommon.hi
../../ObjectIO/Deviceevents.o : ../../ObjectIO/Windowhandle.hi
../../ObjectIO/Devicesystemstate.o : ../../ObjectIO/Devicesystemstate.hs
../../ObjectIO/Devicesystemstate.o : ../../ObjectIO/Device.hi
../../ObjectIO/Devicesystemstate.o : ../../ObjectIO/Receiverhandle.hi
../../ObjectIO/Devicesystemstate.o : ../../ObjectIO/Windowhandle.hi
../../ObjectIO/Id.o : ../../ObjectIO/Id.hs
../../ObjectIO/Id.o : ../../CleanStdEnv/CleanStdFunc.hi
../../ObjectIO/Id.o : ../../CleanStdEnv/CleanStdList.hi
../../ObjectIO/Id.o : ../../ObjectIO/Device.hi
../../ObjectIO/Id.o : ../../ObjectIO/Systemid.hi
../../ObjectIO/IOstate.o : ../../ObjectIO/IOstate.hs
../../ObjectIO/IOstate.o : ../../CleanStdEnv/CleanStdFunc.hi
../../ObjectIO/IOstate.o : ../../CleanStdEnv/CleanStdList.hi
../../ObjectIO/IOstate.o : ../../CleanStdEnv/CleanStdMisc.hi
../../ObjectIO/IOstate.o : ../../ObjectIO/Device.hi
../../ObjectIO/IOstate.o : ../../ObjectIO/Deviceevents.hi
../../ObjectIO/IOstate.o : ../../ObjectIO/Devicesystemstate.hi
../../ObjectIO/IOstate.o : ../../ObjectIO/Id.hi
../../ObjectIO/IOstate.o : ../../OSWindows/Osdocumentinterface.hi
../../ObjectIO/IOstate.o : ../../OSWindows/Osevent.hi
../../ObjectIO/IOstate.o : ../../OSWindows/Ossystem.hi
../../ObjectIO/IOstate.o : ../../OSWindows/Ostoolbox.hi
../../ObjectIO/IOstate.o : ../../ObjectIO/Receivertable.hi
../../ObjectIO/IOstate.o : ../../ObjectIO/StdGUI.hi
../../ObjectIO/IOstate.o : ../../ObjectIO/StdIOBasic.hi
../../ObjectIO/IOstate.o : ../../ObjectIO/Systemid.hi
../../ObjectIO/Key.o : ../../ObjectIO/Key.hs
../../ObjectIO/Layout.o : ../../ObjectIO/Layout.hs
../../ObjectIO/Layout.o : ../../CleanStdEnv/CleanStdList.hi
../../ObjectIO/Layout.o : ../../CleanStdEnv/CleanStdMisc.hi
../../ObjectIO/Layout.o : ../../ObjectIO/Commondef.hi
../../ObjectIO/Layout.o : ../../ObjectIO/Id.hi
../../ObjectIO/Layout.o : ../../ObjectIO/Windowhandle.hi
../../ObjectIO/Processdevice.o : ../../ObjectIO/Processdevice.hs
../../ObjectIO/Processdevice.o : ../../CleanStdEnv/CleanStdMisc.hi
../../ObjectIO/Processdevice.o : ../../ObjectIO/Commondef.hi
../../ObjectIO/Processdevice.o : ../../ObjectIO/IOstate.hi
../../ObjectIO/Processdevice.o : ../../OSWindows/Osdocumentinterface.hi
../../ObjectIO/Processdevice.o : ../../OSWindows/Processevent.hi
../../ObjectIO/Processdevice.o : ../../ObjectIO/StdProcessAttribute.hi
../../ObjectIO/Receiveraccess.o : ../../ObjectIO/Receiveraccess.hs
../../ObjectIO/Receiveraccess.o : ../../ObjectIO/Id.hi
../../ObjectIO/Receiveraccess.o : ../../ObjectIO/Receiverdefaccess.hi
../../ObjectIO/Receiveraccess.o : ../../ObjectIO/Receiverhandle.hi
../../ObjectIO/Receiveraccess.o : ../../ObjectIO/IOstate.hi
../../ObjectIO/Receiverdefaccess.o : ../../ObjectIO/Receiverdefaccess.hs
../../ObjectIO/Receiverdefaccess.o : ../../ObjectIO/Commondef.hi
../../ObjectIO/Receiverdefaccess.o : ../../ObjectIO/StdReceiverAttribute.hi
../../ObjectIO/Receiverdefaccess.o : ../../ObjectIO/StdReceiverDef.hi
../../ObjectIO/Receiverdevice.o : ../../ObjectIO/Receiverdevice.hs
../../ObjectIO/Receiverdevice.o : ../../ObjectIO/Commondef.hi
../../ObjectIO/Receiverdevice.o : ../../ObjectIO/Id.hi
../../ObjectIO/Receiverdevice.o : ../../ObjectIO/IOstate.hi
../../ObjectIO/Receiverdevice.o : ../../OSWindows/Receiverevent.hi
../../ObjectIO/Receiverdevice.o : ../../ObjectIO/Receiverhandle.hi
../../ObjectIO/Receiverdevice.o : ../../ObjectIO/Receiverid.hi
../../ObjectIO/Receiverdevice.o : ../../ObjectIO/Receivertable.hi
../../ObjectIO/Receiverhandle.o : ../../ObjectIO/Receiverhandle.hs
../../ObjectIO/Receiverhandle.o : ../../ObjectIO/Id.hi
../../ObjectIO/Receiverhandle.o : ../../ObjectIO/StdReceiverDef.hi
../../ObjectIO/Receiverid.o : ../../ObjectIO/Receiverid.hs
../../ObjectIO/Receiverid.o : ../../ObjectIO/IOstate.hi
../../ObjectIO/Receiverid.o : ../../ObjectIO/Receivertable.hi
../../ObjectIO/StdReceiver.o : ../../ObjectIO/StdReceiver.hs
../../ObjectIO/StdReceiver.o : ../../CleanStdEnv/CleanStdList.hi
../../ObjectIO/StdReceiver.o : ../../CleanStdEnv/CleanStdMisc.hi
../../ObjectIO/StdReceiver.o : ../../ObjectIO/Commondef.hi
../../ObjectIO/StdReceiver.o : ../../ObjectIO/Deviceevents.hi
../../ObjectIO/StdReceiver.o : ../../ObjectIO/Id.hi
../../ObjectIO/StdReceiver.o : ../../ObjectIO/IOstate.hi
../../ObjectIO/StdReceiver.o : ../../OSWindows/Osevent.hi
../../ObjectIO/StdReceiver.o : ../../ObjectIO/Receiveraccess.hi
../../ObjectIO/StdReceiver.o : ../../ObjectIO/Receiverdefaccess.hi
../../ObjectIO/StdReceiver.o : ../../ObjectIO/Receiverdevice.hi
../../ObjectIO/StdReceiver.o : ../../ObjectIO/Receiverhandle.hi
../../ObjectIO/StdReceiver.o : ../../ObjectIO/Receiverid.hi
../../ObjectIO/StdReceiver.o : ../../ObjectIO/Receivertable.hi
../../ObjectIO/StdReceiver.o : ../../ObjectIO/Scheduler.hi
../../ObjectIO/StdReceiver.o : ../../ObjectIO/StdReceiverAttribute.hi
../../ObjectIO/StdReceiver.o : ../../OSWindows/Trace_12.hi
../../ObjectIO/StdReceiverAttribute.o : ../../ObjectIO/StdReceiverAttribute.hs
../../ObjectIO/StdReceiverAttribute.o : ../../ObjectIO/StdGUI.hi
../../ObjectIO/StdReceiverAttribute.o : ../../ObjectIO/StdIOCommon.hi
../../ObjectIO/StdReceiverAttribute.o : ../../ObjectIO/StdReceiverDef.hi
../../ObjectIO/StdReceiverDef.o : ../../ObjectIO/StdReceiverDef.hs
../../ObjectIO/StdReceiverDef.o : ../../ObjectIO/StdGUI.hi
../../ObjectIO/StdReceiverDef.o : ../../ObjectIO/StdIOCommon.hi
../../ObjectIO/Receivertable.o : ../../ObjectIO/Receivertable.hs
../../ObjectIO/Receivertable.o : ../../CleanStdEnv/CleanStdMisc.hi
../../ObjectIO/Receivertable.o : ../../ObjectIO/Commondef.hi
../../ObjectIO/Receivertable.o : ../../ObjectIO/Device.hi
../../ObjectIO/Receivertable.o : ../../ObjectIO/Id.hi
../../ObjectIO/Receivertable.o : ../../ObjectIO/StdMaybe.hi
../../ObjectIO/Receivertable.o : ../../ObjectIO/Systemid.hi
../../ObjectIO/Roundrobin.o : ../../ObjectIO/Roundrobin.hs
../../ObjectIO/Roundrobin.o : ../../ObjectIO/Commondef.hi
../../ObjectIO/Scheduler.o : ../../ObjectIO/Scheduler.hs
../../ObjectIO/Scheduler.o : ../../OSWindows/ClCrossCall_12.hi
../../ObjectIO/Scheduler.o : ../../CleanStdEnv/CleanStdFunc.hi
../../ObjectIO/Scheduler.o : ../../CleanStdEnv/CleanStdList.hi
../../ObjectIO/Scheduler.o : ../../CleanStdEnv/CleanStdMisc.hi
../../ObjectIO/Scheduler.o : ../../ObjectIO/Commondef.hi
../../ObjectIO/Scheduler.o : ../../ObjectIO/Id.hi
../../ObjectIO/Scheduler.o : ../../ObjectIO/IOstate.hi
../../ObjectIO/Scheduler.o : ../../OSWindows/Osevent.hi
../../ObjectIO/Scheduler.o : ../../ObjectIO/StdProcessDef.hi
../../ObjectIO/Scheduler.o : ../../ObjectIO/Roundrobin.hi
../../ObjectIO/Scheduler.o : ../../ObjectIO/World.hi
../../ObjectIO/StdControl.o : ../../ObjectIO/StdControl.hs
../../ObjectIO/StdControl.o : ../../CleanStdEnv/CleanStdList.hi
../../ObjectIO/StdControl.o : ../../CleanStdEnv/CleanStdMisc.hi
../../ObjectIO/StdControl.o : ../../ObjectIO/Commondef.hi
../../ObjectIO/StdControl.o : ../../ObjectIO/Controlaccess.hi
../../ObjectIO/StdControl.o : ../../ObjectIO/Controlinternal.hi
../../ObjectIO/StdControl.o : ../../ObjectIO/Controllayout.hi
../../ObjectIO/StdControl.o : ../../ObjectIO/Id.hi
../../ObjectIO/StdControl.o : ../../ObjectIO/IOstate.hi
../../ObjectIO/StdControl.o : ../../OSWindows/Ossystem.hi
../../ObjectIO/StdControl.o : ../../ObjectIO/StdControlClass.hi
../../ObjectIO/StdControl.o : ../../ObjectIO/Windowaccess.hi
../../ObjectIO/StdControl.o : ../../ObjectIO/Wstate.hi
../../ObjectIO/StdControlAttribute.o : ../../ObjectIO/StdControlAttribute.hs
../../ObjectIO/StdControlAttribute.o : ../../ObjectIO/StdControlDef.hi
../../ObjectIO/StdControlClass.o : ../../ObjectIO/StdControlClass.hs
../../ObjectIO/StdControlClass.o : ../../CleanStdEnv/CleanExtras.hi
../../ObjectIO/StdControlClass.o : ../../CleanStdEnv/CleanStdList.hi
../../ObjectIO/StdControlClass.o : ../../CleanStdEnv/CleanStdMisc.hi
../../ObjectIO/StdControlClass.o : ../../ObjectIO/Commondef.hi
../../ObjectIO/StdControlClass.o : ../../ObjectIO/IOstate.hi
../../ObjectIO/StdControlClass.o : ../../OSWindows/Osfont.hi
../../ObjectIO/StdControlClass.o : ../../OSWindows/Ostypes.hi
../../ObjectIO/StdControlClass.o : ../../OSWindows/Oswindow.hi
../../ObjectIO/StdControlClass.o : ../../ObjectIO/StdControlAttribute.hi
../../ObjectIO/StdControlClass.o : ../../ObjectIO/Windowhandle.hi
../../ObjectIO/StdControlDef.o : ../../ObjectIO/StdControlDef.hs
../../ObjectIO/StdControlDef.o : ../../ObjectIO/StdGUI.hi
../../ObjectIO/StdControlDef.o : ../../ObjectIO/StdIOCommon.hi
../../ObjectIO/StdGUI.o : ../../ObjectIO/StdGUI.hs
../../ObjectIO/StdGUI.o : ../../ObjectIO/IOstate.hi-boot
../../ObjectIO/StdGUI.o : ../../ObjectIO/StdIOCommon.hi
../../ObjectIO/StdId.o : ../../ObjectIO/StdId.hs
../../ObjectIO/StdId.o : ../../ObjectIO/Id.hi
../../ObjectIO/StdId.o : ../../ObjectIO/IOstate.hi
../../ObjectIO/StdId.o : ../../ObjectIO/StdMaybe.hi
../../ObjectIO/StdId.o : ../../ObjectIO/World.hi
../../ObjectIO/StdIO.o : ../../ObjectIO/StdIO.hs
../../ObjectIO/StdIO.o : ../../ObjectIO/StdId.hi
../../ObjectIO/StdIO.o : ../../ObjectIO/StdIOBasic.hi
../../ObjectIO/StdIO.o : ../../ObjectIO/StdIOCommon.hi
../../ObjectIO/StdIO.o : ../../ObjectIO/StdKey.hi
../../ObjectIO/StdIO.o : ../../ObjectIO/StdMaybe.hi
../../ObjectIO/StdIO.o : ../../ObjectIO/StdGUI.hi
../../ObjectIO/StdIO.o : ../../ObjectIO/StdPictureDef.hi
../../ObjectIO/StdIO.o : ../../ObjectIO/StdProcessDef.hi
../../ObjectIO/StdIO.o : ../../ObjectIO/StdProcessAttribute.hi
../../ObjectIO/StdIO.o : ../../ObjectIO/StdProcess.hi
../../ObjectIO/StdIO.o : ../../ObjectIO/StdControlDef.hi
../../ObjectIO/StdIO.o : ../../ObjectIO/StdControlAttribute.hi
../../ObjectIO/StdIO.o : ../../ObjectIO/StdControlClass.hi
../../ObjectIO/StdIO.o : ../../ObjectIO/StdControl.hi
../../ObjectIO/StdIO.o : ../../ObjectIO/StdReceiverDef.hi
../../ObjectIO/StdIO.o : ../../ObjectIO/StdReceiverAttribute.hi
../../ObjectIO/StdIO.o : ../../ObjectIO/StdReceiver.hi
../../ObjectIO/StdIO.o : ../../ObjectIO/StdWindowDef.hi
../../ObjectIO/StdIO.o : ../../ObjectIO/StdWindowAttribute.hi
../../ObjectIO/StdIO.o : ../../ObjectIO/StdWindow.hi
../../ObjectIO/StdIO.o : ../../ObjectIO/IOstate.hi
../../ObjectIO/StdIOBasic.o : ../../ObjectIO/StdIOBasic.hs
../../ObjectIO/StdIOBasic.o : ../../CleanStdEnv/CleanExtras.hi
../../ObjectIO/StdIOCommon.o : ../../ObjectIO/StdIOCommon.hs
../../ObjectIO/StdIOCommon.o : ../../CleanStdEnv/CleanExtras.hi
../../ObjectIO/StdIOCommon.o : ../../ObjectIO/Id.hi
../../ObjectIO/StdIOCommon.o : ../../ObjectIO/StdIOBasic.hi
../../ObjectIO/StdIOCommon.o : ../../ObjectIO/StdKey.hi
../../ObjectIO/StdIOCommon.o : ../../ObjectIO/StdMaybe.hi
../../ObjectIO/StdKey.o : ../../ObjectIO/StdKey.hs
../../ObjectIO/StdKey.o : ../../ObjectIO/Key.hi
../../ObjectIO/StdMaybe.o : ../../ObjectIO/StdMaybe.hs
../../ObjectIO/StdMaybe.o : ../../CleanStdEnv/CleanExtras.hi
../../ObjectIO/StdMaybe.o : ../../CleanStdEnv/CleanStdFunc.hi
../../ObjectIO/StdPictureDef.o : ../../ObjectIO/StdPictureDef.hs
../../ObjectIO/StdPictureDef.o : ../../ObjectIO/StdIOBasic.hi
../../ObjectIO/StdPictureDef.o : ../../OSWindows/Osfont.hi
../../ObjectIO/StdProcess.o : ../../ObjectIO/StdProcess.hs
../../ObjectIO/StdProcess.o : ../../CleanStdEnv/CleanStdMisc.hi
../../ObjectIO/StdProcess.o : ../../ObjectIO/Commondef.hi
../../ObjectIO/StdProcess.o : ../../ObjectIO/IOstate.hi
../../ObjectIO/StdProcess.o : ../../ObjectIO/Processdevice.hi
../../ObjectIO/StdProcess.o : ../../ObjectIO/Scheduler.hi
../../ObjectIO/StdProcess.o : ../../ObjectIO/StdProcessDef.hi
../../ObjectIO/StdProcessAttribute.o : ../../ObjectIO/StdProcessAttribute.hs
../../ObjectIO/StdProcessAttribute.o : ../../ObjectIO/StdProcessDef.hi
../../ObjectIO/StdProcessDef.o : ../../ObjectIO/StdProcessDef.hs
../../ObjectIO/StdProcessDef.o : ../../ObjectIO/StdGUI.hi
../../ObjectIO/StdProcessDef.o : ../../ObjectIO/StdIOCommon.hi
../../ObjectIO/StdWindow.o : ../../ObjectIO/StdWindow.hs
../../ObjectIO/StdWindow.o : ../../CleanStdEnv/CleanStdMisc.hi
../../ObjectIO/StdWindow.o : ../../ObjectIO/Commondef.hi
../../ObjectIO/StdWindow.o : ../../ObjectIO/Controlvalidate.hi
../../ObjectIO/StdWindow.o : ../../ObjectIO/Id.hi
../../ObjectIO/StdWindow.o : ../../ObjectIO/IOstate.hi
../../ObjectIO/StdWindow.o : ../../ObjectIO/StdControlClass.hi
../../ObjectIO/StdWindow.o : ../../ObjectIO/StdWindowDef.hi
../../ObjectIO/StdWindow.o : ../../ObjectIO/StdWindowAttribute.hi
../../ObjectIO/StdWindow.o : ../../ObjectIO/Windowaccess.hi
../../ObjectIO/StdWindow.o : ../../ObjectIO/Windowcreate.hi
../../ObjectIO/StdWindow.o : ../../ObjectIO/Windowdevice.hi
../../ObjectIO/StdWindow.o : ../../ObjectIO/Windowdispose.hi
../../ObjectIO/StdWindow.o : ../../ObjectIO/Windowvalidate.hi
../../ObjectIO/StdWindowAttribute.o : ../../ObjectIO/StdWindowAttribute.hs
../../ObjectIO/StdWindowAttribute.o : ../../ObjectIO/StdWindowDef.hi
../../ObjectIO/StdWindowDef.o : ../../ObjectIO/StdWindowDef.hs
../../ObjectIO/StdWindowDef.o : ../../ObjectIO/StdGUI.hi
../../ObjectIO/StdWindowDef.o : ../../ObjectIO/StdIOCommon.hi
../../ObjectIO/Systemid.o : ../../ObjectIO/Systemid.hs
../../ObjectIO/Windowaccess.o : ../../ObjectIO/Windowaccess.hs
../../ObjectIO/Windowaccess.o : ../../CleanStdEnv/CleanExtras.hi
../../ObjectIO/Windowaccess.o : ../../CleanStdEnv/CleanStdFunc.hi
../../ObjectIO/Windowaccess.o : ../../CleanStdEnv/CleanStdMisc.hi
../../ObjectIO/Windowaccess.o : ../../ObjectIO/Commondef.hi
../../ObjectIO/Windowaccess.o : ../../OSWindows/Ossystem.hi
../../ObjectIO/Windowaccess.o : ../../ObjectIO/Windowhandle.hi
../../ObjectIO/Windowcreate.o : ../../ObjectIO/Windowcreate.hs
../../ObjectIO/Windowcreate.o : ../../CleanStdEnv/CleanStdFunc.hi
../../ObjectIO/Windowcreate.o : ../../ObjectIO/Commondef.hi
../../ObjectIO/Windowcreate.o : ../../ObjectIO/Controlcreate.hi
../../ObjectIO/Windowcreate.o : ../../ObjectIO/IOstate.hi
../../ObjectIO/Windowcreate.o : ../../OSWindows/Osevent.hi
../../ObjectIO/Windowcreate.o : ../../OSWindows/Ostoolbox.hi
../../ObjectIO/Windowcreate.o : ../../OSWindows/Oswindow.hi
../../ObjectIO/Windowcreate.o : ../../ObjectIO/StdWindowAttribute.hi
../../ObjectIO/Windowcreate.o : ../../ObjectIO/Windowaccess.hi
../../ObjectIO/Windowcreate.o : ../../ObjectIO/Windowhandle.hi
../../ObjectIO/Windowcreate.o : ../../ObjectIO/Windowvalidate.hi
../../ObjectIO/Windowdevice.o : ../../ObjectIO/Windowdevice.hs
../../ObjectIO/Windowdevice.o : ../../ObjectIO/WindowdeviceMVAR.hi
../../ObjectIO/WindowdeviceMVAR.o : ../../ObjectIO/WindowdeviceMVAR.hs
../../ObjectIO/WindowdeviceMVAR.o : ../../CleanStdEnv/CleanStdList.hi
../../ObjectIO/WindowdeviceMVAR.o : ../../CleanStdEnv/CleanStdMisc.hi
../../ObjectIO/WindowdeviceMVAR.o : ../../CleanStdEnv/CleanStdTuple.hi
../../ObjectIO/WindowdeviceMVAR.o : ../../ObjectIO/Commondef.hi
../../ObjectIO/WindowdeviceMVAR.o : ../../ObjectIO/Controlcreate.hi
../../ObjectIO/WindowdeviceMVAR.o : ../../ObjectIO/Deviceevents.hi
../../ObjectIO/WindowdeviceMVAR.o : ../../ObjectIO/Id.hi
../../ObjectIO/WindowdeviceMVAR.o : ../../ObjectIO/IOstate.hi
../../ObjectIO/WindowdeviceMVAR.o : ../../OSWindows/Oswindow.hi
../../ObjectIO/WindowdeviceMVAR.o : ../../ObjectIO/StdControlAttribute.hi
../../ObjectIO/WindowdeviceMVAR.o : ../../ObjectIO/StdWindowAttribute.hi
../../ObjectIO/WindowdeviceMVAR.o : ../../ObjectIO/Windowaccess.hi
../../ObjectIO/WindowdeviceMVAR.o : ../../ObjectIO/Windowcreate.hi
../../ObjectIO/WindowdeviceMVAR.o : ../../ObjectIO/Windowdispose.hi
../../ObjectIO/WindowdeviceMVAR.o : ../../OSWindows/Windowevent.hi
../../ObjectIO/WindowdeviceMVAR.o : ../../ObjectIO/Windowhandle.hi
../../ObjectIO/Windowdispose.o : ../../ObjectIO/Windowdispose.hs
../../ObjectIO/Windowdispose.o : ../../ObjectIO/Commondef.hi
../../ObjectIO/Windowdispose.o : ../../ObjectIO/Controlvalidate.hi
../../ObjectIO/Windowdispose.o : ../../ObjectIO/Id.hi
../../ObjectIO/Windowdispose.o : ../../ObjectIO/IOstate.hi
../../ObjectIO/Windowdispose.o : ../../OSWindows/Ostoolbox.hi
../../ObjectIO/Windowdispose.o : ../../OSWindows/Oswindow.hi
../../ObjectIO/Windowdispose.o : ../../ObjectIO/Scheduler.hi
../../ObjectIO/Windowdispose.o : ../../ObjectIO/Windowaccess.hi
../../ObjectIO/Windowdispose.o : ../../ObjectIO/Windowhandle.hi
../../ObjectIO/Windowhandle.o : ../../ObjectIO/Windowhandle.hs
../../ObjectIO/Windowhandle.o : ../../ObjectIO/Commondef.hi
../../ObjectIO/Windowhandle.o : ../../OSWindows/Ospicture.hi
../../ObjectIO/Windowhandle.o : ../../OSWindows/Ostypes.hi
../../ObjectIO/Windowhandle.o : ../../ObjectIO/StdControlDef.hi
../../ObjectIO/Windowhandle.o : ../../ObjectIO/StdWindowDef.hi
../../ObjectIO/Windowvalidate.o : ../../ObjectIO/Windowvalidate.hs
../../ObjectIO/Windowvalidate.o : ../../CleanStdEnv/CleanStdMisc.hi
../../ObjectIO/Windowvalidate.o : ../../ObjectIO/Commondef.hi
../../ObjectIO/Windowvalidate.o : ../../ObjectIO/Controllayout.hi
../../ObjectIO/Windowvalidate.o : ../../ObjectIO/Id.hi
../../ObjectIO/Windowvalidate.o : ../../ObjectIO/IOstate.hi
../../ObjectIO/Windowvalidate.o : ../../OSWindows/Ossystem.hi
../../ObjectIO/Windowvalidate.o : ../../OSWindows/Oswindow.hi
../../ObjectIO/Windowvalidate.o : ../../ObjectIO/StdId.hi
../../ObjectIO/Windowvalidate.o : ../../ObjectIO/StdWindowAttribute.hi
../../ObjectIO/Windowvalidate.o : ../../ObjectIO/Windowaccess.hi
../../ObjectIO/World.o : ../../ObjectIO/World.hs
../../ObjectIO/Wstate.o : ../../ObjectIO/Wstate.hs
../../ObjectIO/Wstate.o : ../../ObjectIO/Commondef.hi
../../ObjectIO/Wstate.o : ../../OSWindows/Oswindow.hi
../../ObjectIO/Wstate.o : ../../ObjectIO/Windowhandle.hi
../../ObjectIO/Wstateaccess.o : ../../ObjectIO/Wstateaccess.hs
../../ObjectIO/Wstateaccess.o : ../../CleanStdEnv/CleanStdList.hi
../../ObjectIO/Wstateaccess.o : ../../ObjectIO/Commondef.hi
../../ObjectIO/Wstateaccess.o : ../../ObjectIO/Id.hi
../../ObjectIO/Wstateaccess.o : ../../ObjectIO/Wstate.hi
../../CleanStdEnv/CleanExtras.o : ../../CleanStdEnv/CleanExtras.hs
../../CleanStdEnv/CleanStdFunc.o : ../../CleanStdEnv/CleanStdFunc.hs
../../CleanStdEnv/CleanStdList.o : ../../CleanStdEnv/CleanStdList.hs
../../CleanStdEnv/CleanStdList.o : ../../CleanStdEnv/CleanExtras.hi
../../CleanStdEnv/CleanStdMisc.o : ../../CleanStdEnv/CleanStdMisc.hs
../../CleanStdEnv/CleanStdTuple.o : ../../CleanStdEnv/CleanStdTuple.hs
../../OSWindows/ClCCall_12.o : ../../OSWindows/ClCCall_12.hs
../../OSWindows/ClCCall_12.o : ../../OSWindows/Cutil_12.hi
../../OSWindows/ClCCall_12.o : ../../OSWindows/Ostoolbox.hi
../../OSWindows/ClCrossCall_12.o : ../../OSWindows/ClCrossCall_12.hs
../../OSWindows/ClCrossCall_12.o : ../../OSWindows/Ostoolbox.hi
../../OSWindows/ClCrossCall_12.o : ../../OSWindows/Ostypes.hi
../../OSWindows/ClCrossCall_12.o : ../../OSWindows/Cutil_12.hi
../../OSWindows/Cutil_12.o : ../../OSWindows/Cutil_12.hs
../../OSWindows/MenuCrossCall_12.o : ../../OSWindows/MenuCrossCall_12.hs
../../OSWindows/MenuCrossCall_12.o : ../../OSWindows/Ostypes.hi
../../OSWindows/Osdocumentinterface.o : ../../OSWindows/Osdocumentinterface.hs
../../OSWindows/Osdocumentinterface.o : ../../OSWindows/ClCrossCall_12.hi
../../OSWindows/Osdocumentinterface.o : ../../OSWindows/Cutil_12.hi
../../OSWindows/Osdocumentinterface.o : ../../ObjectIO/Commondef.hi
../../OSWindows/Osdocumentinterface.o : ../../OSWindows/MenuCrossCall_12.hi
../../OSWindows/Osdocumentinterface.o : ../../OSWindows/Ostoolbar.hi
../../OSWindows/Osdocumentinterface.o : ../../OSWindows/Ostoolbox.hi
../../OSWindows/Osdocumentinterface.o : ../../OSWindows/Ostypes.hi
../../OSWindows/Osdocumentinterface.o : ../../ObjectIO/StdIOCommon.hi
../../OSWindows/Osdocumentinterface.o : ../../ObjectIO/StdMaybe.hi
../../OSWindows/Osdocumentinterface.o : ../../OSWindows/WindowCrossCall_12.hi
../../OSWindows/Osevent.o : ../../OSWindows/Osevent.hs
../../OSWindows/Osevent.o : ../../OSWindows/ClCrossCall_12.hi
../../OSWindows/Osevent.o : ../../ObjectIO/Commondef.hi
../../OSWindows/Osevent.o : ../../OSWindows/Cutil_12.hi
../../OSWindows/Osevent.o : ../../OSWindows/Ostoolbox.hi
../../OSWindows/Osevent.o : ../../OSWindows/Ostypes.hi
../../OSWindows/Osevent.o : ../../ObjectIO/Receivertable.hi
../../OSWindows/Osevent.o : ../../OSWindows/Trace_12.hi
../../OSWindows/Osfont.o : ../../OSWindows/Osfont.hs
../../OSWindows/Osfont.o : ../../OSWindows/Cutil_12.hi
../../OSWindows/Osfont.o : ../../OSWindows/ClCCall_12.hi
../../OSWindows/Osfont.o : ../../OSWindows/ClCrossCall_12.hi
../../OSWindows/Osfont.o : ../../ObjectIO/Commondef.hi
../../OSWindows/Osfont.o : ../../OSWindows/PictCCall_12.hi
../../OSWindows/Ospicture.o : ../../OSWindows/Ospicture.hs
../../OSWindows/Ospicture.o : ../../ObjectIO/Commondef.hi
../../OSWindows/Ospicture.o : ../../OSWindows/Osrgn.hi
../../OSWindows/Ospicture.o : ../../OSWindows/Osfont.hi
../../OSWindows/Ospicture.o : ../../OSWindows/Ostoolbox.hi
../../OSWindows/Ospicture.o : ../../OSWindows/PictCCall_12.hi
../../OSWindows/Ospicture.o : ../../ObjectIO/StdIOBasic.hi
../../OSWindows/Ospicture.o : ../../ObjectIO/StdPictureDef.hi
../../OSWindows/Osrgn.o : ../../OSWindows/Osrgn.hs
../../OSWindows/Osrgn.o : ../../OSWindows/Ostypes.hi
../../OSWindows/Osrgn.o : ../../OSWindows/PictCCall_12.hi
../../OSWindows/Osrgn.o : ../../OSWindows/RgnCCall_12.hi
../../OSWindows/Ossystem.o : ../../OSWindows/Ossystem.hs
../../OSWindows/Ossystem.o : ../../OSWindows/ClCCall_12.hi
../../OSWindows/Ossystem.o : ../../OSWindows/ClCrossCall_12.hi
../../OSWindows/Ossystem.o : ../../OSWindows/Osdocumentinterface.hi
../../OSWindows/Ossystem.o : ../../OSWindows/Osfont.hi
../../OSWindows/Ossystem.o : ../../OSWindows/Ostypes.hi
../../OSWindows/Ossystem.o : ../../ObjectIO/StdMaybe.hi
../../OSWindows/Ossystem.o : ../../OSWindows/WindowCrossCall_12.hi
../../OSWindows/Ostoolbar.o : ../../OSWindows/Ostoolbar.hs
../../OSWindows/Ostoolbar.o : ../../OSWindows/ClCrossCall_12.hi
../../OSWindows/Ostoolbar.o : ../../CleanStdEnv/CleanStdMisc.hi
../../OSWindows/Ostoolbar.o : ../../OSWindows/Ostypes.hi
../../OSWindows/Ostoolbar.o : ../../OSWindows/PictCCall_12.hi
../../OSWindows/Ostoolbar.o : ../../OSWindows/WindowCCall_12.hi
../../OSWindows/Ostoolbox.o : ../../OSWindows/Ostoolbox.hs
../../OSWindows/Ostypes.o : ../../OSWindows/Ostypes.hs
../../OSWindows/Oswindow.o : ../../OSWindows/Oswindow.hs
../../OSWindows/Oswindow.o : ../../OSWindows/ClCCall_12.hi
../../OSWindows/Oswindow.o : ../../OSWindows/ClCrossCall_12.hi
../../OSWindows/Oswindow.o : ../../CleanStdEnv/CleanStdList.hi
../../OSWindows/Oswindow.o : ../../OSWindows/Cutil_12.hi
../../OSWindows/Oswindow.o : ../../ObjectIO/Commondef.hi
../../OSWindows/Oswindow.o : ../../OSWindows/Osdocumentinterface.hi
../../OSWindows/Oswindow.o : ../../OSWindows/Osevent.hi
../../OSWindows/Oswindow.o : ../../OSWindows/Osfont.hi
../../OSWindows/Oswindow.o : ../../OSWindows/Ospicture.hi
../../OSWindows/Oswindow.o : ../../OSWindows/Osrgn.hi
../../OSWindows/Oswindow.o : ../../OSWindows/Ossystem.hi
../../OSWindows/Oswindow.o : ../../OSWindows/Ostoolbox.hi
../../OSWindows/Oswindow.o : ../../OSWindows/Ostypes.hi
../../OSWindows/Oswindow.o : ../../OSWindows/RgnCCall_12.hi
../../OSWindows/Oswindow.o : ../../OSWindows/WindowCCall_12.hi
../../OSWindows/Oswindow.o : ../../OSWindows/WindowCrossCall_12.hi
../../OSWindows/Oswindow.o : ../../ObjectIO/Windowhandle.hi
../../OSWindows/PictCCall_12.o : ../../OSWindows/PictCCall_12.hs
../../OSWindows/PictCCall_12.o : ../../OSWindows/Ostoolbox.hi
../../OSWindows/PictCCall_12.o : ../../OSWindows/Ostypes.hi
../../OSWindows/PictCCall_12.o : ../../OSWindows/RgnCCall_12.hi
../../OSWindows/PictCCall_12.o : ../../OSWindows/Cutil_12.hi
../../OSWindows/Processevent.o : ../../OSWindows/Processevent.hs
../../OSWindows/Processevent.o : ../../OSWindows/ClCrossCall_12.hi
../../OSWindows/Processevent.o : ../../ObjectIO/Commondef.hi
../../OSWindows/Processevent.o : ../../ObjectIO/Deviceevents.hi
../../OSWindows/Processevent.o : ../../ObjectIO/IOstate.hi
../../OSWindows/Processevent.o : ../../OSWindows/Osevent.hi
../../OSWindows/Processevent.o : ../../OSWindows/Ostypes.hi
../../OSWindows/Receiverevent.o : ../../OSWindows/Receiverevent.hs
../../OSWindows/Receiverevent.o : ../../ObjectIO/Deviceevents.hi
../../OSWindows/Receiverevent.o : ../../ObjectIO/IOstate.hi
../../OSWindows/Receiverevent.o : ../../OSWindows/Osevent.hi
../../OSWindows/RgnCCall_12.o : ../../OSWindows/RgnCCall_12.hs
../../OSWindows/RgnCCall_12.o : ../../OSWindows/Cutil_12.hi
../../OSWindows/RgnCCall_12.o : ../../OSWindows/Ostoolbox.hi
../../OSWindows/Trace_12.o : ../../OSWindows/Trace_12.hs
../../OSWindows/WindowCCall_12.o : ../../OSWindows/WindowCCall_12.hs
../../OSWindows/WindowCCall_12.o : ../../OSWindows/Cutil_12.hi
../../OSWindows/WindowCCall_12.o : ../../OSWindows/Ostoolbox.hi
../../OSWindows/WindowCCall_12.o : ../../OSWindows/Ostypes.hi
../../OSWindows/WindowCCall_12.o : ../../OSWindows/RgnCCall_12.hi
../../OSWindows/WindowCCall_12.o : ../../OSWindows/PictCCall_12.hi
../../OSWindows/WindowCrossCall_12.o : ../../OSWindows/WindowCrossCall_12.hs
../../OSWindows/WindowCrossCall_12.o : ../../OSWindows/ClCrossCall_12.hi
../../OSWindows/WindowCrossCall_12.o : ../../CleanStdEnv/CleanExtras.hi
../../OSWindows/WindowCrossCall_12.o : ../../CleanStdEnv/CleanStdMisc.hi
../../OSWindows/WindowCrossCall_12.o : ../../OSWindows/Cutil_12.hi
../../OSWindows/WindowCrossCall_12.o : ../../OSWindows/Ostypes.hi
../../OSWindows/WindowCrossCall_12.o : ../../OSWindows/ClCCall_12.hi
../../OSWindows/WindowCrossCall_12.o : ../../OSWindows/ClCrossCall_12.hi
../../OSWindows/WindowCrossCall_12.o : ../../OSWindows/PictCCall_12.hi
../../OSWindows/Windowevent.o : ../../OSWindows/Windowevent.hs
../../OSWindows/Windowevent.o : ../../OSWindows/ClCCall_12.hi
../../OSWindows/Windowevent.o : ../../OSWindows/ClCrossCall_12.hi
../../OSWindows/Windowevent.o : ../../CleanStdEnv/CleanStdMisc.hi
../../OSWindows/Windowevent.o : ../../ObjectIO/Commondef.hi
../../OSWindows/Windowevent.o : ../../ObjectIO/Controlcreate.hi
../../OSWindows/Windowevent.o : ../../OSWindows/Cutil_12.hi
../../OSWindows/Windowevent.o : ../../ObjectIO/Deviceevents.hi
../../OSWindows/Windowevent.o : ../../ObjectIO/IOstate.hi
../../OSWindows/Windowevent.o : ../../OSWindows/Osevent.hi
../../OSWindows/Windowevent.o : ../../ObjectIO/StdControlAttribute.hi
../../OSWindows/Windowevent.o : ../../ObjectIO/Windowaccess.hi
# DO NOT DELETE: End of Haskell dependencies
