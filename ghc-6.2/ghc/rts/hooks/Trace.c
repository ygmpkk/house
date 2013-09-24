/* -----------------------------------------------------------------------------
 * $Id$
 *
 * User-overridable RTS hooks.
 *
 * ---------------------------------------------------------------------------*/

#include "Rts.h"

void
PreTraceHook (long fd STG_UNUSED)
{
  /* Default is not to print anything, however this might be useful:
   *
   * const char msg[]="\nTrace On:\n";
   * write(fd,msg,sizeof(msg)-1);
   */
}

void
PostTraceHook (long fd STG_UNUSED)
{
  /* Default is not to print anything, however this might be useful:
   *
   * const char msg[]="\nTrace Off.\n";
   * write(fd,msg,sizeof(msg)-1);
   */
}

