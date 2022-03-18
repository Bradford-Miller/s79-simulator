# Scheme-79 Chip Reimplimentation in Simulation and FPGA

Time-stamp: <2022-03-18 12:13:54 gorbag>

This is a "first cut" at a software simulation/emulation of the
SCHEME-79 chip (by GLS, also Jack Holloway, Jerry Sussman and Alan
Bell).  My intent is to (at least) use this to clarify anything in the
paper that is unclear before proceeeding to HDL implementation, as
well as to develop the specific representation (that is binary tables)
for the microcontrol and nanocontrol stores.

The core reference for this implementation is MIT AI memos 514 and
559: "Design of LISP-Based Processors or, SCHEME: A Dielectric LISP
or, Finite Memories Considered Harmful or, LAMBDA: the Ultimate
Opcode" and "The SCHEME-79 Chip" respectively.

## Loading and Running 

Right now, only the simulator is supported, a future release will
generate HDL suitable for importing into an appropriate FPGA tool
suite!

### Installation

Clone and install the CL-LIB repository, then the fpga-support library
repository. The former can be installed anywhere ASDF can find it, the
latter should be installed as a sibling to the current package (i.e.
have a common parent directory).

### Compile

### Run

Generally after compiling and loading, I do something like (where the
number supplied to the test function corresponds to the specfic test
set, q.v.)

``` 
    (test :n 0)
    (start-console)
```

This will use the microcode for the specified test and bring up the
console. From there, press "step" and you should see the diplay
populate ready to run the first instruction in the test (in the case
of test 0, above, the chip boot code). If you then "run" it should run
through the boot stage (in this case before GCing memory) and do some
simple stack maniplulations. If, when the microcode gets to the "DONE"
tag the registers and memory are correct per the test's specification,
it will indicate the test was successful. (You can see clock-by-clock
tracing of the microcode in the output pane of the listener).

You can also use the console to start the DSO and a diagnostics panel.
The DSO will indicate pad and internal register control timing for the
simulated run; the diagnostics panel lists micro and nanoinstructions
and how many times they have been executed since the system was
loaded. (They should be started before running the simulation). When a
test is successfully completed (or gets to the DONE tag but does not
pass the test) the instructions executed during the cycle are marked as
failed or successful in the diagnostics panel to help target
debugging. Predicates are marked up separately so the success and
failure arms can both be checked. (Consider this a primitive code
coverage tool).

Additional tests may also be run, however, after the first test (with
the console exposed) the sequence to run them is to press "step" to
get a clear display, "reset" to set the internal microcode PC
appropriately for the newly loaded microcode, and then "step" again to
see the first microinstruction of the new test. It is not necessary to
rerun `(start-console)` as it should already be displayed.

## Liberties Taken

Since I'm not set on reproducing the actual chip they developed, a few
(I tried to stay reasonable) liberties were taken in order to simplify
the code and FPGA. Memory and chip space are not as tight as their
requirements (a given maximum chip size) and I did try to stay true to
the actual operation of the chip. Regardless, I document substantive
(intentional :) changes to their design here:

### Registers

While I kept their registers, some control lines were added.
Specifically:

Control Wire | Description
------------ | -----------
from-displacement | added to *val* to support their &val-displacement-to-exp-displacement u-op
from-frame | added to *val* to support their &val-frame-to-exp-frame u-op
from-type | added to *exp* to support dispatch-on-exp-allowing-interrupts. Note that from-type will shift the bits into the low order bits suitable for loading directly into the micro-pc; this may change to something more distinguished in the future so the library knows to specify a multiplexor.
not-mark-bit | added to *bus* to allow direct detection of this state. I'm sure inverters are cheap even on an FPGA, but I need some way to actually force the issue (so this may change in the future).
mark! unmark! | controls added to bus to set the appropriate bits (set/unset the mark-bit)
type! pointer! | controls added to bus to set the appropriate bits (set/unset the type-not-pointer bit)

### Pads

I added a reset pad to allow external circuitry (ha) to reset the
chip. Reset is quite common for chips of this era, so I imagine it was
left off due to a constraint on bonded pads or packaging. For the most
part, this is to allow a simple interface to a button on the FPGA
later.

### Instructions

While the microcode is well documented, not all of the specific
instructions are. I'm postulating eval-exp-popj-to (also
see S-Code, below), means the following, and currently expand it into:

``` 
(progn
    (&set-type *stack* <tag>)
    (dispatch (fetch *exp*)))
```

The first statement sets up the continuation from the dispatch on EXP which is
pointing to the GLOBAL cell for our function, and the second interprets it (so
it should get the GLOBAL value for the symbol) and then continues into
internal-apply (typically). That's going to do another
dispatch-on-exp-allowing-interrupts so we should get the CLOSURE at that
point.

## Test Sets:

Note that normally on completion the machine would loop on location
"done" (and the microcode is set up to do this!), however the TEST mode
overrides this and halts the machine to examine memory and registers
and test that they are correct. (under the tests directory test-<n>.mcr
contains the test's associated microcode, and test-<n>.lisp contains
code to set up the memory on RESET and to check the results when the
test in completed. In some cases, breakpoints may be extablished as
well, though typically I would expect this only for tests that have not
yet passed.

Some documentation:

Filename | Description
-------- | -----------
test-0 | boot function replicates the initial boot in terms of setting *memtop* from the initial memory and then gets a stack pointer from a simulated interrupt. Then it does a push and pop to reverse two items that were in our initial stack (directly placed into memory). This tests register assignment, fetching car and cdr of a location from memory, incrementing registers, getting a pointer from an interrupt, and doing basic stack operations. 
test-1 | boot function extended to do the initial GC which should consolodate free space and set up the register pointers correctly to allow CONSing. (Note CONS was tested in test-0). Some initial garbage is put into memory to make sure it is ignored by the mark/sweep algorithm.
test-2 | hand-compiled APPEND function run on a couple list structures (from the AIM's description of APPEND's S-Code). This is the first test of the chip's ability to actually interpret Scheme S-Code rather than just the internal microcode, and I expect future tests to be more elaborated to exercise all of the various microcoded functions. Note that at the time this test was written we do not yet have an S-Code compiler, so a future test may check an APPEND function output by that (future) compiler!

## S-Code Notes

So neither AIM directly documents S-Code (compiled scheme code directly
executed by the machine) but does give a (mostly complete) example for APPEND
(which appears in test-2). Additionaly, while the representation for the
APPEND function itself is sketched out, only AIM 514 shows the representation
of how it might be invoked. Needless to say, the tags used for AIM 514 don't
directly map to AIM 559, and further the specific representation doesn't work
if we try to copy it. So the following are some notes on what I've discovered
through trial and error and some analysis of the APPEND code that is provided.

### Invoking the top-level function
After BOOT-LOAD runs, we have *stack* pointing to the top of the stack (the
function to be evaluated) with a type of BOOT-LOAD-RETURN, which is entered
after the initial GC completes.  We then assign *EXP* to the car of the stack
(the car of the cons cell the stack points to) and evaluate that as our
top-level function. Since the CDR of that cell will be ignored, we have to
make sure it's something valid if it only has a valid CAR, which means it
can't be type FIRST-ARGUMENT (which would start collecting arguments for a
function call) because the CDR is the continuation after the first argument is
set. So I beleive we are required to use SEQUENCE here as a NIL rest of
sequence can be successfully ignored (we will see DONE, presumably as set up
by BOOT-LOAD-RETURN when we've finished evaluating the first sequence
element).

### The general form of a function call
Test-2 shows a general invocation of a function; the initial SEQUENCE points
to a CONS cell whose CAR is of type FIRST-ARGUMENT which points to a single
element list of our first argument. The CDR of the SEQUENCE target cell is NIL
(or in this case could be DONE since we won't have anything else to do). The
cell 1st-arg points to has a CAR that points to the list struction that is our
first argument, while the CDR is of type LAST-ARG and points to another CONS
whose CAR is a pointer to the actual list that is our second argument to
append and whose CDR points to to the global value cell for our APPEND
function itself as the continuation.

Anyway, the key insight is to understand the processor will do recursive
descent on the EXP (expression to be evaluated), expecting the arguments
to a function first, and then looking in the CDR of the last thing
processed for the continuation (the next thing to do) rather than what
you might expect from a standard lisp interpreter.

[Insert picture here]

Of additional note, the TR presented LOCAL links as <frame>, <offset> which
makes intuitive sense (the frame is so we can move up the stack and talk about
the context of the caller for the purposes of finding local variable
definitions. However, the representation within a register or memory is <type>
<offset> <frame>, so I've added an optional argument to the make-word helper
function such that if the frame is specified (a third value) it creates the
right word, and otherwise just expects <type> <data> as is usual for most
cases (see, e.g., test-2.lisp).

## Status:

Note the TODO.txt file documents specific tasks that are planned (in
some sense ;-) or previous TODO items that have been completed.

#### 3-18-22 BWM
Still working on test-2. It mostly seems to be working correctly (single
stepping through the microcode to be sure everything is copasetic). Last bug
found today was the frame/displacement issue mentioned above; I had just
slammed the displacement directly into the low order bits of the data field,
based on the provided diagram for APPEND, which is intuitive but wrong!
Anyway, test-2 is fixed so hopefully we can get a clean run soon and I can
increment the dot release before starting to work on the VHDL generator for
this (which, presumably, will require some recoding of the simulator as well
as the paradigm is better elaborated). Also clear that 64 words isn't enough
to even run this append example, so doubling the size of memory to handle the
recursive call without invoking GC which will require setting up an interrupt
generator/handler. Test-1 already demonstrated GC works, but for GC's
triggered by consing, a pin is set and the user (hardware) is expected to
generate an interrupt and vector to a handler. In our case we will want to
call the GC in the microcode as we don't need anything more elaborate, but I
was going to wait until at least test-3 to set all that up. Plus as we get
more complexity in the user-level code we will want a scheme->s-code compiler
as well, but all this seems to be a distraction over our goal of getting an
FPGA generator. So that will wait at least until we have some version (that
can pass test-2) built on an FPGA! (after which we may focus on scheme-86
anyway and build an s-code compiler that works with both).

#### 1-11-22 BWM
test-1 works and have repatriated more code into
../fpga-support as well as some refactoring to more cleanly split
between generic fpga processing library code and specific scheme-79
code (some of which is done through defining methods or setting
variables defined in the fpga-support hierarchy).

#### 10-21-21 BWM 
Segregating code into that which supports both
simulation and translation into HDL under ../fpga-support, and nearly
have test-1 working (documented below) with the exception at this
point of dispatch-on-stack type functions (which is how we get into
the general scheme interpreter, but also run the finalization of the
boot process).

Also added a diagnostics 'front' panel which illustrates which
microinstructions, nanoinstructions, micro predicates and tests have
been run along with some statistics.

#### 8-20-21 BWM 
test-0 passes (finally!), so designating future work
as version 0.2 and incremented versions of component files to 0.1.x.
Source level support has been added to the console (we can report
microcode by the line), the top N values of the stack are displayed,
and step/run/halt and even micro-step have been added as console
operations. Additionaly the reset-line has been added (not part of the
original chip) and the console also can test for a halt address being
reached (e.g. DONE in the microcode) so it can display that in the log
and on the console as well as automatically run diagnostic tests (in
the future) to establish if the run was successful.

#### 6-21-21 BWM 
First cut at a digital storage oscilloscope simulation
to allow debugging of the timing of signals to the pads to be a bit
easier than microstepping through the front-panel interface watching
for changes. Note that I now have a test directory with partial
microcode files that allow testing to proceed in terms of exercising
more and more of the underlying architecture and instruction set. The
format of tests is to have a pair of files (with the same primary name
but .lisp and .mcr extensions) containing the microcode in the mcr
file and initial setup (including the initial contents of the
machine's memory) in the .lisp file. The function 
```
		(test &key n validate-only-p)
``` 
is defined in the
file scheme-79-defs.lisp where n designates a test file (named
'test-n') and if validate-only-p is non-nil it only checks but does
not compile the microcode (so the machine will not be set up to run
the test).

#### 2-9-21 BWM

Some updates: After some thought, trying to replicate all of the
ancillary software that is (partially) described in the AIMs isn't
really needed, and as the "real goal" after some retrocomputing fun and
learning more about how to build a processor (on an FPGA) is to also
have a set of tools that will help design a new kind of distributed
processing system, I refocused on the lower levels of the SCHEME-79
implementation and have been building it around an (optional)
front-panel that should make it clearer what is going on and make it
easier to develop and debug the nanocode and other parts of the
implementation. So rather than just an "interpreter" for the microcode,
I've tried to come up with a more or less faithful representation
(using bit vectors where possible) of the registers, the register
control lines, the sense lines, etc. as described for the chip, and
also am trying to emulate what such a chip would do during each of the
phases of the two-phase clock using initialization list that run during
each part of the clock (e.g. rising, falling, etc.).  While this won't
be completely accurate with respect to how this is implemented in, say,
HDL, it should be a better path toward building tools that will
eventually help transform a lisp-like state machine representation
directly into RTL or HDL. And that's worth doing the implementation
"the hard way" since by having those tools the ultimate goal of
building more advanced Actor based machines in hardware should be
simpler.

So at this point I am currently writing nanocode that matches the
microcode while implementing machine instructions and writing a
microcode compiler (essentially the machine instructions are
implemented to generate a binary version of the microcode that will run
the nanocode FSM just as on the original chip). Since I'm doing this
only with the microcode and only 3 example of nanocode it's not a fast
process, but again the goal here is to put together tools and
methodology that will not only help construct SCHEME-79 but also
SCHEME-86 and then the later systems. This has meant a larger focus on
something I initially ignored - building a simulated console that shows
most of the internal machine state in one view, similar to the
blinkenlights of yore (but at least at this point represents register
contents as octal numbers :-).

#### 1-9-21 BWM

Version 0.1 designated, given that there is a primitive front panel
allowing the registers to be loaded and read, and the control/sense
lines to these registers have been tested. See additional notes in the
2-9-21 entry about the front panel. NB: the versions of particular
files/packages has NOT been incremented, so remain with a 0.0 prefix.
I expect them to trail the "scheme-79" version until the first major
release (which will probably also get uploaded to github at that
point)

#### 9-11-20 Bradford W. Miller

At this point, the microcode and other code present in the above
papers have been transcribed. Of that, microcode.mcr is a more or
less faithful copy of the appendix in AIM-559, and probably the most
important for this project.

Next we need to write an interpreter for that microcode (which was not
presented in these papers though they give enough clues along with the
context and comments accompanying the microcode itself that it should
not be difficult),

Then we need to write at least a test compiler from SCHEME (possibly a
subset) into the representation (S-code) used by the microcode. Or
just use a fragment which seems to be what's in the TRs.

And finally we need to actually implement the interpreter in HDL. 

At that point we should be able to go to the next level and look at
SCHEME-86 (AIM-1040) and improvements there, finally getting to the
final phase where we focus on the distributed/comms side (getting back
to Hewitt Actors) and look at how to build a "distributed SCHEME"
supercomputing system!

## Contact

Ideally, posting bugs in the project or repository would be the ideal
way to contact me (even if it's just a misunderstanding of the
documentation).



