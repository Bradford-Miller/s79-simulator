(in-package :cl-user)
(defvar *scheme-79-version-reporter-initializations* nil)

(cl-lib:detailed-version-reporter "S79 defpackages" 0 4 2
                                  "Time-stamp: <2022-04-12 18:01:31 gorbag>"
                                  "export processor control functions"
                                  :initialization-list-symbol
                                  *scheme-79-version-reporter-initializations*)

;; 0.4.2   4/12/22 export processor control functions (like reset) from :scheme-79

;; 0.4.1   4/ 7/22 moving memory dump routines to console/mem.lisp as they are
;;                    implemented for simulation only

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 0.4.0   3/18/22 snapping a line: 0.4 release of scheme-79 supports test-0 thru test-3. ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 0.3.9   3/18/22 make sure we create the *stack* symbol since we haven't loaded the file that does so yet

;; 0.3.8   3/15/22 import *stack* into external-chips so dump-memory can use it to 
;;                    trace the stack

;; 0.3.7   2/18/22 export *frame-field-length* and *displacement-field-length*

;; 0.3.6   2/15/22 note-breakpoint-reached

;; 0.3.5   2/ 9/22 way too many things (fns, variables) with "line" in their name
;;                    and it's ambiguous.  Splitting so "line" refers to,
;;                    e.g. an output (log) line, "expression" refers to a
;;                    'line' of code (single expression in nano or microcode
;;                    land typically, and because we used (READ) it wasn't
;;                    confined to a single input line anyway) and "wire" to
;;                    refer to, e.g., a control or sense 'line' on a register.

;; 0.3.4   1/26/22 *breakpoint-descs*

;; 0.3.3   1/24/22 *mask-interrupts*

;; 0.3.2   1/21/22 new s-code package for the (future) s-code compiler

;; 0.3.1   1/13/22 change references from internal-freeze to run-nano
;;                    for consistancy with AIM

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 0.3.0   1/11/22 snapping a line: 0.3 release of scheme-79 supports  test-0 and test-1. ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 0.1.37  1/10/22 move defchip-pad, defchip-pads etc. to support/clocked/pads.lisp
;;                 remove scheme-i package as it's replaced by :fpga-pla-build-tools

;; 0.1.36  1/ 7/22 move *debug-compiler*, *debug-assembler* to support/project-defs

;; 0.1.35  1/ 6/22 predicates-used now in microlisp-int

;; 0.1.34  1/ 4/22 move ucode-syntax to microlisp-int
;;                      *register-control-wires* *register-sense-wires* -> microlisp-int
;;                      validate-register-* -> microlisp-int
;;                      read-microcode-for-interpreter -> microlisp-int
;;                      *debug-validator*, *debug-precompiler* -> fpga-project-defs

;; 0.1.33 12/16/21 move some symbols to support

;; 0.1.32 12/14/21 new special-register-p fn

;; 0.1.31 12/10/21 get #:from and #:to from microlisp

;; 0.1.30 12/ 9/21 *ulang-pkg* move to support/project-defs

;; 0.1.29 12/ 8/21 export *default-microcode* from :scheme-79

;; 0.1.28 12/ 2/21 moving some defns from :scheme-mach to :fpga-pla-build-tools

;; 0.1.27 10/26/21 export from-type stuff and new *control-wires*
;;                   *sense-wires*

;; 0.1.26 10/21/21 export *nano-pc-size*, *nano-pc-max-address*
;;                   import :fpga-plas where it will be needed

;; 0.1.25 10/20/21 defchip-special-reg

;; 0.1.24 10/15/21 export *ulang-pkg*, all-microfunctions

;; 0.1.23 10/13/21 add *last-conditional-result* for diagnostics

;; 0.1.22 10/12/21 add predicate-test-objects for diagnostics

;; 0.1.21 10/ 7/21 breakpoint fns and *breakpoints* var

;; 0.1.20  9/28/21 run-until-tick fn

;; 0.1.19  9/27/21 ok, finally put progn, cond, if into scheme-79-mcr
;;                    package to help clean up that bit of cruftiness
;;                    (won't need to say cl:if anymore which is the
;;                    far more common case than scheme-79-mcr:if)

;; 0.1.18  9/24/21 export mark! etc as control wires (so console sees
;;                    them). Should automate on declaration (TBD)

;; 0.1.18  9/20/21 move clock support init lists to :fpga-clocked

;; 0.1.17  9/18/21 defupred-inverse and invert-predicate in core-support

;; 0.1.16  9/16/21 moved a number of clock (phase) related fns and
;;                    variables to support (see below)

;; 0.1.15  9/14/21 add new support packages: fpga-combinatorics and
;;                    fpga-clocked

;; 0.1.14  9/13/21 debug-compile-expression: a debug version of compile expression
;;                    exported from :scheme-79 and writes to
;;                    *standard-output* instead of the intermediate
;;                    file to check if the compiler is working. Note,
;;                    this has to be called AFTER validating the
;;                    microcode to set up the internal state for the
;;                    compiler.
;;                 export register-mark!-p etc.

;; 0.1.13  9/ 9/21 move copy-field to support/common
;;                    add some field-length parameters to scheme-mach

;; 0.1.12  9/ 8/21 moved some low-level functions from diagnostics to
;;                    fpga-diagnostics

;; 0.1.11  9/ 7/21 move rewind-stream to support/common

;; 0.1.10  9/ 4/21 introduce debug-support (part of fpga-support), and
;;                    fpga-gui-support.

;; 0.1.9   9/ 3/21 microcontrol-symbol-value

;; 0.1.8   9/ 2/21 branch-type

;; 0.1.7   9/ 1/21 sense-wire-real-name

;; 0.1.6   8/31/21 generate-ucode-with-constant

;; 0.1.5   8/27/21 move some fns from scheme-i to common

;; 0.1.4   8/26/21 register-p

;; 0.1.3   8/24/21 common package, new "if" ucode (macro)

;; 0.1.2   8/23/21 defupred and accessors

;; 0.1.1   8/22/21 differentiate microcode 'cond' from cl:cond

;; 0.1.0   8/20/21 "official" 0.2 release: test-0 passed!

;; 0.0.47  8/14/21 generate-success/fail-mt-name

;; 0.0.46  8/ 9/21 new diagnostics package, export some internal functions and vars to support

;; 0.0.45  8/ 2/21 *interrupt-address* wasn't being exported form
;;                   external-support so test-0.lisp set the wrong
;;                   one!

;; 0.0.44  7/30/21 external pad timing symbols (e.g. *test-ale-to-expect-address*)

;; 0.0.43  7/20/21 *input-pad-types* *output-pad-types*

;; 0.0.42  7/19/21 set-micro-pc ; fix #:to and #:to-type getting commented out accidentally

;; 0.0.41  7/16/21 export *dso*

;; 0.0.40  7/10/21 fix go-to collision

;; 0.0.39  7/ 7/21 export machine-ready-p

;; 0.0.38  7/ 6/21 export neutral clock phase lists *neutral1-list* *neutral2-list*
;;                   finish moving test-pad-immediate (0.0.36); didn't delete from source ;-)

;; 0.0.37  7/ 2/21 export next-phase

;; 0.0.36  6/27/21 export test-pad-immediate (moved from external-support to machine-defs)

;; 0.0.35  6/25/21 (reset-dso)

;; 0.0.34  6/17/21 *total-clock-phases*

;; 0.0.33  6/ 5/21 *all-ph-pre-update-list* *all-pad-names* *all-bus-names*

;; 0.0.32  6/ 2/21 DSO related interfaces

;; 0.0.31  5/28/21

;; 0.0.30  4/26/21 eliminate *freeze-state*

;; 0.0.29  4/21/21 export find-likely-microcode-tag so console can use it

;; 0.0.28  3/24/21 add 'note' for things less serious than 'warn'.

;; 0.0.27  3/ 9/21 force export pads

;; 0.0.26  3/ 7/21 *-code-to-anaphor

;; 0.0.25  3/ 5/21 sense-wire-encoding-to-symbol

;; 0.0.24  3/ 4/21 export finalize-*controller-array fns

;; 0.0.23  2/27/21 translate aliased registers

;; 0.0.22  2/26/21 strip-register-name

;; 0.0.21  2/24/21 ucode-function-expansions

;; 0.0.20  2/23/21 export generate-conditional-ucode

;; 0.0.19  2/22/21 export date-string

;; 0.0.18  2/20/21 export rewind-stream from :scheme-i

;; 0.0.17  2/19/21 export some symbols to track what the compiler is
;;                    working on like *function-being-compiled*

;; 0.0.16  2/17/21 export *scheme-mach* as the package for :scheme-mach

;; 0.0.15  2/11/21 share *cold-boot-memory-array*

;; 0.0.14  2/ 6/21 make *microcontrol-array* shared into the MCR environment for compilation
;; 0.0.13  1/31/21 separate package :external-chips for stuff that's outside the chip itself
;; 0.0.12  1/29/21 pad related exports
;; 0.0.11  1/25/21 more clock related exports (*tick* ...)
;; 0.0.10  1/17/21 export analyze-code
;; 0.0.9   1/12/21 export microcode/nanocode access functions
;; 0.0.8   1/10/21 export pointer-bit
;; 0.0.7   1/ 8/21 export register-field-accessor
;; 0.0.6   1/ 6/21 export clock-related functions and variables
;; 0.0.5   1/ 2/21 export copy-field from scheme-i (used in console and scheme-mach)
;; 0.0.4  12/28/20 move &forms to :scheme-mach
;; 0.0.3  12/ 3/20 export defmicromacro
;; 0.0.2  12/ 2/20 export read-microcode-for-interpreter from :scheme-79

(defpackage :scheme-79
  ;; package for my own specific extensions or paraphenalia. The
  ;; intent is that this package have project-specific functions and
  ;; variables that aren't part of the machine itself, though a future
  ;; version might just call it :fpga-project-defs or something similar
  ;; rather than tie it to the specific machine we are trying to run
  ;; (most of the following functionsa aren't really that particular
  ;; for the scheme-79 chip)
  (:use :fpga-pads :debug-support :fpga-project-defs :fpga-support :common :cl-lib common-lisp)
  (:export #:scheme-79-version-reporter
           #:announce-scheme-79-version
           #:reset-uinstruction-metrics
           #:*scheme-mach*
           #:*default-microcode*

           ;; control functions (these may be common to all processors?)
           #:reset #:power-on-reset

           ;; for debugging (also may be generally common to most/all processors?)
           #:disassemble-microcontrol-array
           #:debug-compile-expression
           #:dump-memory #:dump-memory-with-types #:clear-memory

           ;; debug flags
           #:*debug-external-pads*
           #:*warn-when-beyond-memtop*
           #:*dump-values-per-row*
           #:*debug-microcontroller*
           #:*debug-nanocontroller*
           #:*debug-timing*
           #:*debug-defnano*
           #:*debug-dataflow*
           ))

(in-package :scheme-79)

(defmacro scheme-79-version-reporter (part-name  major-version minor-version micro-version date comments)
  `(cl-lib:detailed-version-reporter ,part-name ,major-version ,minor-version ,micro-version ,date ,comments 
                                     :initialization-list-symbol
                                     cl-user::*scheme-79-version-reporter-initializations*))

;; this is more or less independent of the machine being simulated
;; (they don't all have to be used!), so might be moved into a
;; :simulation-ui-support package
(defpackage :diagnostics ; core support for diagnostics (automated or manual tests)
  (:use :fpga-diagnostics :microlisp-int :fpga-pla-build-tools :debug-support :fpga-support :fpga-project-defs
   :common :cl-lib common-lisp :scheme-79) 
  (:export
    #:generate-fail-mt-name #:generate-success-mt-name

   #:create-microtest-objects #:*microtest-objects*
   #:create-nanotest-objects #:*nanotest-objects*
   #:create-test-suite-objects #:*test-suite-objects* #:*test-suite*
   #:create-predicate-test-objects #:*predicate-test-objects*

   #:declare-diagnostic-suite

   #:*last-conditional-result*

   #:clear-metrics))

;; this is more or less a hack to allow us to share between stuff that
;; is implementing the machine (internally and externally) and the
;; specific chip.

(defpackage :scheme-shared ;; shared between machine and external we
  ;; share clock state as that should be externally generated and
  ;; input to the chip, but for now we've reversed it and internally
  ;; generate. but by sharing the state everyone has access which is
  ;; as it should be. we also share the initialization lists rather
  ;; than reimplement them for external and internal separately
  ;; (though that would be more accurate for emulation)
  (:use
   ;; for the moment while we untangle stuff that is for the compiler/assembler
   :microlisp-int :microlisp-shared :fpga-pla-build-tools 
   ;; and stuff that is for the simulator
   :fpga-plas :fpga-registers :fpga-pads :fpga-combinatorics :fpga-clocked :debug-support :fpga-support
   :fpga-project-defs :common :scheme-79 :common-lisp :cl-lib)
  (:export
   ;; clock related (so pervasive)
   #:*ph1* #:*ph2* ; the pads

   ;; for initialization
   #:*cold-boot-memory-array*

   ;; not part of the original scheme-79 chip, but useful!
   #:*halt-address* #:*breakpoints* #:*breakpoint-descs*
   ))

;; while this was originally intended to be limited to stuff specific
;; to the scheme-79 implementation, there's a lot here that really
;; could be pulled into a simulation-support type package to simplify
;; implementing other machines in the future.
(defpackage :scheme-mach
  ;; the actual machine. exported symbols are "machine microcode" and
  ;; generally start with #\&
  (:use
   ;; for the moment while we untangle stuff that is for the compiler/assembler
   :microlisp-int :microlisp-shared :fpga-pla-build-tools 
   ;; and stuff that is for the simulator
   :fpga-plas :fpga-registers :fpga-pads :fpga-clocked :fpga-combinatorics
   :fpga-diagnostics :debug-support :fpga-support :fpga-project-defs :common
   :scheme-79 :scheme-shared :common-lisp :cl-lib)

  (:export
   #:*address-field-length* #:*address-field-mask* #:data-field-length* #:*type-field-length*
   #:*displacement-field-length* #:*frame-field-length*
   
   #:*maximum-memory-size* #:*maximum-memory-content*

   ;; parsing out words
   #:get-type-bits #:get-displacement-bits #:get-frame-bits #:get-address-bits
   #:break-out-bits-as-integers

   ;; internal version of the clock
   #:init-clock #:get-clock-status #:update-clock #:single-step 
   #:run #:stop #:running-p #:set-running-p #:run-until-tick

   ;; additional running functions for breakpoints
   #:clear-breakpoints #:clear-breakpoint #:set-breakpoint #:run-until-breakpoint 

   ;; register definitions
   #:defureg

   ;; simulator itself
   #:set-micro-pc #:*micro-pc-size* #:*micro-pc-max-address*
   #:*nano-pc-size* #:*nano-pc-max-address*

   ;; lookup functions (used after loading microcode!)
   #:get-microcode #:get-nanocode
   #:collect-ncode 
   #:collect-ucode #:sense-wire-encoding-to-symbol #:sense-wire-real-name

   ;; all the registers pads & &forms should be automatically
   ;; exported, but sometimes there are ordering difficulties...
   #:*reset* #:*ale* #:*freeze* #:*read* #:*read-state* #:*load-state*
   #:*interrupt-request* #:*write* #:*cdr* 
   #:*read-interrupt* #:*gc-needed*
   ;; pseudo pads
   #:*conditional* #:*run-nano* #:*mask-interrupts*

   ;; names of the control and sense wires
   #:to #:to-type
   #:to-displacement #:to-frame #:to-address #:from #:from-decremented
   #:from-incremented #:from-type #:mark! #:unmark! #:type! #:pointer!
   #:*control-wires*

   #:address=bus #:type=bus #:=bus #:mark-bit
   #:pointer-bit #:type-not-pointer #:frame=0 #:displacement=0
   #:address=0 #:type=pointer #:type=type
   #:*sense-wires*

   #:from-type-const ;; special

   ;; not sure why this keeps generating a package conflict
   #:address=bus-newcell

   ;; defflags doesn't do the an export for us, so we do it here for
   ;;   now
   #:register-to-p #:register-to-type-p
   #:register-to-displacement-p #:register-to-frame-p
   #:register-to-address-p #:register-from-p
   #:register-from-decremented-p #:register-from-incremented-p
   #:register-address=bus-p #:register-type=bus-p #:register-=bus-p
   #:register-mark-bit-p #:register-type-not-pointer-p
   #:register-frame=0-p #:register-displacement=0-p
   #:register-address=0-p
   #:register-mark!-p #:register-unmark!-p #:register-type!-p #:register-pointer!-p
   #:register-from-type-p

   ;; external pad timing
   #:*test-ale-to-expect-address* #:*get-address-from-pads* #:*put-memory-content-onto-pads*
   #:*get-memory-content-from-pads* #:*test-for-read-interrupt*

   ;; internal timing
   #:*run-microcontroller* #:*run-external-data-transfer* #:*run-nanocontroller-p1*
   #:*run-register-controls* #:*update-sense-wires* #:*run-nanocontroller-p2*

   ;; microcode and nanocode compilation support - sets up bit
   ;; encoding for register references, for instance

   #:clear-anaphors #:setup-anaphors #:lookup-from-anaphor
   #:*from-to-nano-operations*
   #:from-code-to-anaphor #:to-code-to-anaphor
   #:lookup-to-anaphor #:translate-alias
   #:analyze-expression 

   #:compile-parameter
   #:generate-ucode #:generate-conditional-ucode
   #:generate-ucode-with-constant #:generate-conditional-ucode-with-constant
   #:compile-parameter-args-last
   #:compile-embedded-expression

   #:decode-nanocontrol-registers
   #:decode-nanocontrol-pads

   ;; see sim-machine-external.lisp
   #:machine-ready-p
   ))

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; this symbol isn't defined when we first load this file, so create it here so we can import it
  (export (list (intern "*STACK*" (find-package 'microlisp-shared))) (find-package 'microlisp-shared)))


;; while this is mostly specific to the scheme-79 chip, it is
;; specifically about external support chips (like memory) that are
;; supporting using the chip (decoding the pads, and pretending to act
;; like memory chips or whatever). since it's somewhat independent of
;; the chip description it's probalby best to split this into a
;; support part and stuff that comes from the chip definition (like
;; pad names and timing)
(defpackage :external-chips
  ;; should run off pads, not machine internals (exceptions are
  ;; explicit and typically for debugging)
  (:use :microlisp-int :fpga-registers :fpga-pads :fpga-clocked :fpga-combinatorics
   :debug-support :fpga-support :fpga-project-defs :common
   :scheme-79 :scheme-shared :cl-lib :common-lisp)
  (:import-from :scheme-mach #:*reset* #:*ale*
                #:*freeze* #:*read* #:*read-state* #:*load-state* #:*interrupt-request*
                #:*write* #:*cdr* #:*read-interrupt* #:*gc-needed* #:*run-nano*

                #:*input-pad-types* #:*output-pad-types* #:get-address-bits 

                #:*test-ale-to-expect-address* #:*get-address-from-pads* #:*put-memory-content-onto-pads*
                #:*get-memory-content-from-pads* #:*test-for-read-interrupt*

                #:*address-field-length* #:*address-field-mask* #:data-field-length* #:*type-field-length*)
  (:import-from :microlisp-shared #:*stack*)

  (:export
   #:*warn-when-beyond-memtop* 
   
   ;; used to set up an interrupt address for the external simulator to respond with, typically
   ;; in test-n.lisp see external-support.lisp
   #:*interrupt-address* 
   
   #:init-memory-for-cold-boot 
   #:read-address #:write-address #:invalid-address-p
   #:*memory-vector* ;; the actual memory array, used by mem.lisp
   ))

;; this is intended to be the interpreter for the microcode, but not
;; depend on the microcode itself. This is built on top of :microlisp-int
;; in the shared library, and may go away in favor of that as things get generalized.

(defpackage :scheme-79-mcr-i ;; an interpreter for the microcode and definitions for the macros, etc.
  (:use :microlisp-int :microlisp-shared :fpga-pla-build-tools
        :fpga-plas :fpga-registers :fpga-pads :fpga-clocked :fpga-combinatorics :debug-support
        :fpga-support :fpga-project-defs :common
        :scheme-79 :named-readtables :scheme-shared :scheme-mach :cl-lib :common-lisp)
  (:shadowing-import-from :microlisp  #:defmacro #:deftype)

  (:export
   ;; microcode operations that require special handling
   #:gc-special-type ; so we don't warn on this one
   #:bus ; can defreg the bus (*bus* internally)
   #:disassemble-microcode #:disassemble-microcontrol-array

   ;; useful for console
   #:find-likely-microcode-tag 
   ))

;; this is a ui that has some general stuff and specific stuff for the
;; scheme-79 chip, and probably should be segregated appropriately.
#+capi
(defpackage :s79-console
  ;;special package for the capi console for debugging/operating the
  ;;computer and peripherals
  (:use :microlisp-int :microlisp-shared :fpga-gui-support :fpga-pla-build-tools
        :fpga-plas :fpga-registers :fpga-pads :fpga-clocked
        :fpga-combinatorics :debug-support :fpga-support :fpga-project-defs
        :common :scheme-79 :scheme-shared :scheme-mach :external-chips
        :fpga-diagnostics :diagnostics
        :capi :cl-lib :hcl :lispworks common-lisp)
  (:shadowing-import-from :common #:date-string)
  (:shadowing-import-from :scheme-79-mcr-i #:find-likely-microcode-tag #:microcodes-used
                          #:get-uc-annotation)

  (:export 
   #:*halt-address*
   #:note-breakpoint-reached
   
   #:*dso* #:*console*

   #:clear-register-description-pane
           
   #:update-microinstruction-metrics
   #:*diagnostics-interface* #:redraw-diagnostics
   #:update-diagnostics #:reset-uinstruction-metrics
   #:update-predicates #:update-test-suites
   #:update-microtests #:update-nanotests
   #:dw1-tests #:dw2-tests #:dw3-tests #:dw4-tests

   ;; moved here as not part of machine
   ;; some debug-useful functions dealing with "external" memory
   #:dump-memory #:clear-memory #:dump-memory-with-types #:compare-memory))

;; while i'm not curently using this package yet, the intent is this
;; is where the scheme langage and ->s-code functions would go,
;; essentially giving us a higher level interface to an abstract
;; "scheme machine" which then gets implemented by the scheme-79 chip.
;; as well as a cross-compiler to the machine
(defpackage :s-code
  ;; internal code for the s-code compiler
  (:use :cl-lib common-lisp)
  (:export
   ))

(defpackage :scheme
  ;; public face for scheme itself (upon which we can call the s-code
  ;; compiler to prepare code for the chip) we want to define our own
  ;; special set of scheme-lisp fns - this prevents us from using the
  ;; standard packages
  (:use :fpga-project-defs :common :scheme-79) 
  (:export
   ))



