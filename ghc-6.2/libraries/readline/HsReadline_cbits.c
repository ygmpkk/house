#include <stdio.h>
#include <readline/readline.h>

void hs_rl_message (const char *s) 
{
    rl_message ("%s", s);
}
