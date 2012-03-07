#include <stdio.h>

#include <nds.h>


void print_int(int x)
{
  iprintf("print_int: %d\n", x);
}


void print_bool(bool b)
{
  if (b)
    iprintf("print_bool: true\n");
  else
    iprintf("print_bool: false\n");
}


