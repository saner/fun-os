/*---------------------------------------------------------------------------------

	Phase #01
  Sandbox
  Used to run Scheme code

---------------------------------------------------------------------------------*/
#include <nds.h>
#include <stdio.h>

#include "basic_lib.h"

int scheme_entry(void);

int main(void)
{
	consoleDemoInit();

	iprintf("Phase#01 Sandbox\n");

  // run scheme
  int taggedResult = scheme_entry();
  int result = taggedResult >> 2;
	iprintf("scheme_entry returned: %i", result);

	while(1) {
		swiWaitForVBlank();
	}

}
