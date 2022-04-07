(in-package :scheme-79)

(scheme-79-version-reporter "S79 Dev Support" 0 4 1
                            "Time-stamp: <2022-04-07 12:53:45 gorbag>"
                            "change default microcode location")

;; 0.4.1   4/ 7/22 with introduction of plas directory, move microcode there
;;                    and change default location here

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 0.4.0   3/18/22 snapping a line: 0.4 release of scheme-79 supports test-0 thru test-3. ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 0.3.0   1/11/22 snapping a line: 0.3 release of scheme-79 supports  test-0 and test-1. ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; 0.1.13  1/ 7/22 move *debug-compiler*, *debug-assembler* to
;;                     support/project-defs.lisp

;; 0.1.12  1/ 5/22 declare register names to be in microlisp-shared
;;                     here so it's defined before we compile references

;; 0.1.11  1/ 4/22 move *debug-validator* to support/project-defs.lisp

;; 0.1.10 12/15/21 move export-ulisp-symbol to ulisp-defs

;; 0.1.9  12/14/21 set up *project-machine-pkg*

;; 0.1.8  12/ 8/21 move *current-test* to test-support.lisp

;; 0.1.7  12/ 3/21 microlisp replaces scheme-79-mcr as the microcode
;;                   package (under support) move test fn to
;;                   test-support

;; 0.1.6  10/21/21 nano-pc is now a bit vector (used in (test))

;; 0.1.5  10/15/21 add *ulang-pkg* as we seem to do a lot of
;;                    find-package on that

;; 0.1.4   9/29/21 turn off debug-pad-timing by default

;; 0.1.3   9/ 7/21 Aesthetics

;; 0.1.2   9/ 2/21 move some "generic" functions (like announcements) to
;;                    support/common.lisp

;; 0.1.1   8/21/21 test: don't play with the PC or set reset if we're
;;                    just trying to validate the microcode

;; 0.1.0   8/20/21 "official" 0.2 release: test-0 passed!

;; 0.0.14  8/13/21 *debug-dataflow*

;; 0.0.13  8/ 6/21 declare-diagnostic-suite

;; 0.0.12  7/19/21 use set-micro-pc

;; 0.0.11  6/26/21 move various debug parameters here

;; 0.0.10  3/16/21 test resets the micro and nano PC to 0

;; 0.0.9   2/24/21 announce-scheme-79-version optionally prints out file
;;                    stamps

;; 0.0.8   2/22/21 date-string

;; 0.0.7   2/19/21 increase banner-length to 120 to make it easier to
;;                    spot in long logs, also make print-semi-line a
;;                    standalone function (was internal to
;;                    announcement-banner)

;; 0.0.6   2/18/21 move *scheme-mach* definition here

;; 0.0.5   2/11/21 test function expanded to support multiple
;;                    incremental tests

;; 0.0.4   2/ 6/21 add tbd function to keep track of stubs (also dumps
;;                    stack on lispworks so we can find it)

;; 0.0.3   2/ 1/21 set-pad should be in scheme-mach package

;; 0.0.2   1/13/21 Stub out reset function to act as if (new) reset
;;                   pad has been triggered (high during ph1 and ph2
;;                   for one clock?)

;; 0.0.1   1/ 4/21 Add test (microcode) function to make loading
;;                   initial microcode easier.

;; these are mostly low level definitions for the package,
;; (configuration), not about the implementation of the simulator or
;; the language per se. However, we've added some declarations for symbols
;; so they get into the right packages before we compile the associated files.

;; we need to add certain symbols to microlisp-shared before we compile their definitions

;; registers (see machine-defs.lisp)

(in-package :microlisp-shared)
(export '(*bus* *memtop* *newcell* *scan-up* *exp* *scan-down* *val*
          *rel-tem-2* *stack-top* *retpc-count-mark* *args* *leader* *rel-tem-1*
          *stack* *display* *node-pointer* *nil* *address* *memory* *interrupt*
          *nano-pc* *micro-pc*))

(in-package :scheme-79)

(defparameter *default-microcode* "src:scheme-79;simulator;plas;microcode.mcr"
  "Where (test) looks for the standard microcode (this can be
  overridden by parameters on (test))")

;; various debug flags
(defparameter *debug-external-pads* t
  "If non-nil, log interactions with simulated external entities (like
  memory) and external pads")

;; simulation of the external memory itself
(defparameter *warn-when-beyond-memtop* t
  "Generate a warning if we try to read or write beyond the value of
he *memtop* register (for debugging).  Note that occasionally this
will be ok (e.g. during boot when memtop has not been established)")

(defparameter *dump-values-per-row* 2
  "When dumping memory, the number of addresses to print per row")

(defparameter *debug-microcontroller* t
  "log microcontroller info when the u-pc is updated")

(defparameter *debug-defnano* nil
  "Turn on extra debugging while expanding defnano forms")

(defparameter *debug-nanocontroller* t
  "log verbosely nanocontroller updates")

(defparameter *debug-dataflow* t
  "show dataflow from and to registers in the log")

(defparameter *debug-timing* (or *debug-pad-timing* *debug-microcontroller* *debug-nanocontroller*)
  "Turn on some additional messages when we are debugging anything
  related to timing, e.g. tick, phase entry...")

(defvar *scheme-mach* (find-package :scheme-mach))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setq *ulang-pkg* (find-package :microlisp)) ; we could create a scheme-79 version of microlisp here.
  (setq *ulang-shared-pkg* (find-package :microlisp-shared)) ; ditto
  (setq *project-machine-pkg* (find-package :scheme-mach))) ; we have to have our own project machine package though.

(defun reset ()
  "Same as setting the reset-line on the chip for a clock cycle"
  (note-if (or *debug-microcontroller* *debug-external-pads*) "Setting *reset* pad")
  (set-pad 'scheme-mach:*reset*))

(defun power-on-reset ()
  (scheme-mach:single-step t) ; prep UI
  (reset) ; send reset signal
  (scheme-mach:single-step t)) ; allow it to be processed
