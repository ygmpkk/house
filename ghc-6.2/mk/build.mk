GhcCompilerWays =
SRC_HC_OPTS += -H32m -O -DSTANDALONE
SRC_CC_OPTS += -DSTANDALONE
HSC2HS_OPTS += -DSTANDALONE
GhcHcOpts = -DSTANDALONE
GhcLibHcOpts = -O -DSTANDALONE -optc-DSTANDALONE
GhcLibWays =
# To enable debug:
#GhcRtsHcOpts = -optc-DSTANDALONE -optc-DDEBUG -optc-UPROFILING
#GhcRtsCcOpts = -O -DSTANDALONE -DDEBUG -UPROFILING
GhcRtsHcOpts = -optc-DSTANDALONE
GhcRtsCcOpts = -O -DSTANDALONE
SplitObjs = NO
GHCBootLibs = YES
GhcLibsWithUnix = NO
GhcWithInterpreter=NO
