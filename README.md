# Scheme-79 Chip Reimplimentation in Simulation and FPGA

Time-stamp: <2022-02-08 18:22:35 gorbag>

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

Generally after compiling and loading, I do something like

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
These will indicate pad and internal register control timing for the
simulated run. (They should be started before running the simulation).

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
the actual operation of the chip. Regardless, I document (intentional
:) changes to their design here

### Registers

While I kept their registers, some control lines were added.
Specifically:

Control Wire | Description
------------ | -----------
from-displacement | added to *val* to support their &val-displacement-to-exp-displacement u-op
from-frame | added to *val* to support their &val-frame-to-exp-frame u-op
not-mark-bit | added to *bus* to allow direct detection of this state. I'm sure inverters are cheap even on an FPGA, but I need some way to actually force the issue (so this may change in the future).
mark! unmark! | controls added to bus to set the appropriate bits (set/unset the mark-bit)
type! pointer! | controls added to bus to set the appropriate bits (set/unset the type-not-pointer bit)

### Pads

I added a reset pad to allow external circuitry (ha) to reset the
chip. Reset is quite common for chips of this era, so I imagine it was
left off due to a constraint on bonded pads or packaging.


### Test Sets:
Some documentation:

Filename | Description
-------- | -----------
test-0 | boot function replicates the initial boot in terms of setting *memtop* from the initial memory and then gets a stack pointer from a simulated interrupt. Then it does a push and pop to reverse two items that were in our initial stack (directly placed into memory). This tests register assignment, fetching car and cdr of a location from memory, incrementing registers, getting a pointer from an interrupt, and doing basic stack operations. On completion the machine loops on location "done".
test-1 | boot function extended to do the initial GC which should consolodate free space and set up the register pointers correctly to allow CONSing. (Note CONS was tested in test-0). Some initial garbage is put into memory to make sure it is ignored by the mark/sweep algorithm.
test-2 | hand-compiled APPEND function run on a couple list structures (from the AIM's description of APPEND's S-Code)

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
really needed, and as the "real goal" after some retrocomputing fun
and learning more about how to build a processor (on an FPGA) is to
also have a set of tools that will help design a new kind of
distributed processing system, I refocused on the lower levels of the
SCHEME-79 implementation and have been building it around an
(optional) front-panel that should make it clearer what is going on
and make it easier to develop and debug the nanocode and other parts
of the implementation. So rather than just an "interpreter" for the
microcode, I've tried to come up with a more or less faithful
representation (using bit vectors where possible) of the registers,
the register control lines, the sense lines, etc. as described for the
chip, and also am trying to emulate what such a chip would do during
each of the phases of the two-phase clock using initialization list
that run during each part of the clock (e.g. rising, falling, etc.).
While this won't be completely accurate with respect to how this is
implemented in, say, HDL, it should be a better path toward building
tools that will eventually help transform a lisp-like state machine
representation directly into RTL or HDL. And that's worth doing the
implementation "the hard way" since by having those tools the ultimate
goal of building more advanced Actor based machines in hardware should
be simpler.

So at this point I am currently writing nanocode that matches the
microcode while implementing machine instructions and writing a
microcode compiler (essentially the machine instructions are
implemented to generate a binary version of the microcode that will
run the nanocode FSM just as on the original chip). Since I'm doing
this only with the microcode and only 3 example of nanocode it's not a
fast process, but again the goal here is to put together tools and
methodology that will not only help construct SCHEME-79 but also
SCHEME-86 and then the later systems. This has meant a larger focus on
something I initially ignored - building a simulated console that
shows most of the internal machine state in one view, similar to the
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
papers have been transcribed. Of that, microcode.lisp is a more or
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



