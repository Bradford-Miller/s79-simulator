;; Time-stamp: <2022-04-12 17:03:53 gorbag>

;;  2/18/21: add memory window
;; 10/ 8/21: flip order of core-support and ucode-defs as we introduce a dependency

(cl:in-package :asdf)

;; make sure we load the support asd file (you can redirect this to
;; where you put it or comment out if asdf already should know about
;; it)

(load "../fpga-support/fpga-support.asd")

(defsystem :scheme-79-sim
  :serial t ; inefficient but simple. 
  :depends-on (named-readtables fpga-support :cl-lib :fiveam) ;adding fiveam for regression tests
  :components
  (
   (:file "packages")
   (:file "scheme-79-project-defs")     
   (:file "scheme-79-defs")
   
   (:file "console/diagnostics-defs")

   (:file "clock")                     ; clock related functions
   (:file "clock-triggers")            ; where we relate various process starts
                                       ; to the clock phases
   
   (:file "machine-predefs")           ; this and the next two files define the
                                       ; lower level of the machine, i.e. the
                                       ; registers and control lines.
   (:file "machine-defs")
   (:file "machine-wires")

   (:file "machine")

   (:file "sim-machine-external-defs")

   (:file "sim-machine-external")      ; pad definitions and support
   (:file "sim-machine-internal")      ; depends on ucode-defs

   (:file "machine-nano")              ; simulator support for compiled
                                       ; nanocode.
   (:file "machine-micro")             ; simulator support for compiled
                                       ; microcode.
   (:file "external-support")          ; technically not part of the scheme79
                                       ; chip (though note much of this will be
                                       ; part of our FPGA!)

   ;; This is the compiler/assembler for microcode and nanocode It has some
   ;; dependencies on the machine definitions, but does not in and of itself
   ;; actually specify the machine so is now segregated
   
   (:module "plas" :serial t           ; code to create the nanocontrol and
                                       ; microcontrol array
    :components
    (
     (:file "ucode-defs")              ; here to get the symbols defined
                                       ; (exports them): these are symbols that
                                       ; ucode source has that get compiled
                                       ; into (different?) nano operations by
                                       ; the ucode-compiler. Must preceed
                                       ; core-support for defufn variables
     (:file "compiler-defs")
     (:file "storage-manager")         ; memory/gc related microcode defs
     (:file "mcode-assembler")
     (:file "s79-microfunctions")
     (:file "s79-micro-macros")        ; defines macros used in the microcode
                                       ; like progn and cond
     (:file "ncode-assembler")         ; defines the nano instructions, need to
                                       ; read this before the microcode gets
                                       ; compiled.
     (:file "s79-nanocode")             
     (:file "ucode-support-ops")       ; microcode ops that are used in the
                                       ; expansion of MIT's ops
     (:file "upla-assembler")          ; lower level imlementation for
                                       ; compiling microcode

     #||(:file "microcode")||#         ; this gets loaded manually (at this
                                       ; point) using fn
                                       ; read-microcode-for-interpreter or
                                       ; compile-microcode
     ))
   
   ;; THIS ISN'T IMPLEMENTED YET
   ;; scheme->s-code compiler
   (:module "s-code" :serial t
    :components
    ((:file "s-code"))) ; more to come

   ;; The rest of the files are for testing and debugging, though the console
   ;; is very useful to run the machine, everything it does can be controlled
   ;; via lisp functions instead.
   
   (:module "console" :serial t
    :components
    (;; console, memory window, and DSO have only been implemented under LispWorks CAPI
     (:file "control-fns")
     #+capi
     (:file "s79-console-defs")
     #+capi
     (:file "s79-console")
     #+capi
     (:file "dso")
     ;; note that mem.lisp now contains the memory dump routines (which aren't
     ;; dependent on CAPI) they probably should be split out into the tests
     ;; directory for that reason as they are needed for debugging if the
     ;; memory panel isn't available. I put them there because the "mem" panel
     ;; is the only user that calls them directly (except compare-memory which is
     ;; used by some of the tests)
     #+capi
     (:file "mem")
     ;; similarly for diagnostics panel: a generalized simple grid for
     ;; presenting test results (or just tracking them if manually
     ;; operated):
     #+capi
     (:file "diagnostics-panel")
     #+capi
     (:file "diagnostics-support")
     ))

   
   (:module "tests" :serial t
    :components
    ((:file "test-support")  ; functions used by the test suites not defined as part of diagnostics (machine specific)
     (:file "compiler-regression-tests"))) ; 5am tests just on the compiler output
   
   (:file "load-last")))
