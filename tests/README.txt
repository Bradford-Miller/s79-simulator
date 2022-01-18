Time-stamp: <2021-10-13 13:28:43 gorbag>

This directory contains what should be increasingly complex (in that it exercises more
nanoinstructions) microcode files. These are loaded by test number in the function
scheme-79:test, and are intended to assist in the development of the nanocode and
verifying that the microcode, front panel, internal logic, and timing are working as the
implementation proceeds.

test-n.mcr is the microcode for test N.

test-n.lisp contains a function (test-n) that when called initilizes the psudo-external
memory and any other needed peripherals (?) to allow the microcode to be properly
exercised. Typically, that would include an s-code function to be executed by the
scheme-79 processor, and should be used instead of (reset) which will clear memory and is
probably not what is intended. It may also include a function (check-test-n) which should
return non-nil if the test ran as expected (e.g. we now have the correct contents of
"external" memory, or the "final" contents of the machine registers are correct).

See the diagnostics package for how microinstructions, nanoinstructions and test suites
are run and metered.

 test-0: check boot code up until we would do GC to organize memory. Instead, stuff a
         constant into memtop and build a simple stack which we manipulate to test basic
         stack functions, register access, and (pseudo) memory access. Note that this is
         NOT an exhausive test of all registers, just a quick check that we should be able
         to make it through the boot process other than organizing the initial free list.

 test-1: Continue boot code thru initial GC to organize memory. We complete when the
         initial GC finishes and would otherwise dispatch on the first machine opcode
         which should be at the top of the stack (?). The test initializes memory with a
         small spagetti stack and some garbage between stack members and the result should
         show the stack got compacted into low memory with the highest memory location
         used for the stack also being where the pointer to the next allocatable CONS is.
         
 
