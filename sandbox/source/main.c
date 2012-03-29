/*---------------------------------------------------------------------------------

	Phase #01
  Sandbox
  Used to run Scheme code

---------------------------------------------------------------------------------*/
#include <nds.h>
#include <stdio.h>

#include "basic_lib.h"

int scheme_entry(void* mem_addr, int mem_size);

int main(void)
{
	consoleDemoInit();

	iprintf("Phase#01 Sandbox\n");

  // run scheme
  // allocate 3.5MB memory for scheme
  // 1.5MB used for a stack
  // 2.0MB used for a heap
  int memory_size = 3670016;
  void* scheme_memory = malloc(memory_size);
  if (scheme_memory == NULL)
  {
    iprintf("ERROR: Couldn't allocate memory for scheme");

    while(1) {
      swiWaitForVBlank();
    }
  }

  int taggedResult = scheme_entry(scheme_memory, memory_size);
  int result = taggedResult >> 3;
	iprintf("scheme_entry returned: %i", result);

	while(1) {
		swiWaitForVBlank();
	}

}
