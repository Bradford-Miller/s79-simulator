;; Time-stamp: <2022-02-03 14:58:30 gorbag>

;; 10/8/21: flip order of core-support and ucode-defs as we introduce a dependency

(cl:in-package :asdf)

;; make sure we load the support asd file (you can redirect this to
;; where you put it or comment out if asdf already should know about
;; it)

(load "../fpga-support/fpga-support.asd")

(defsystem :scheme-79-sim
  :serial t ; inefficient but simple. 
  :depends-on (named-readtables fpga-support :cl-lib :five-am) ;adding five-am for regression tests
  :components
  (
   (:file "packages")
   (:file "scheme-79-project-defs")     
   (:file "scheme-79-defs") 
   (:file "diagnostics-defs")

   (:file "ucode-defs")                 ; here to get the symbols
                                        ; defined (exports them):
                                        ; these are symbols that ucode
                                        ; source has that get compiled
                                        ; into (different?) nano
                                        ; operations by the
                                        ; ucode-compiler. Must preceed
                                        ; core-support for defufn
                                        ; variables

   (:file "compiler-defs")

   (:file "clock")                      ; clock related functions
   (:file "clock-triggers")             ; where we relate various
                                        ; process starts to the clock
                                        ; phases
   (:file "machine-predefs") ; this and the next two files define the
                             ; lower level of the machine, i.e. the
                             ; registers and control lines.
   (:file "machine-defs")
   (:file "machine")
   (:file "storage-manager") ;; memory/gc related part of the machine

   (:file "sim-machine-external-defs")

   (:file "sim-machine-external") ; pad definitions and support
   (:file "sim-machine-internal") ; depends on ucode-defs
   (:file "s79-micro-macros")     ; defines macros used in the microcode like progn and cond
   (:file "machine-nano")    ; defines the nano instructions, need to
                             ; read this before the microcode gets
                                        ; compiled.

   (:file "s79-nanocode")    ; load after machine-nano at least...

   (:file "machine-micro")   ; lower level support for compiled microcode.
   
   (:file "ucode-support-ops")  ; microcode ops that are used in the expansion of MIT's ops
   
   (:file "upla-assembler")  ; lower level imlementation for compiling microcode

   ;;   (:file "microcode") ; this gets loaded manually (at this
   ;;   point) using fn read-microcode-for-interpreter or
   ;;   compile-microcode

   (:file "external-support") ; technically not part of the scheme79
                              ; chip (though note much of this will be
                                        ; part of our FPGA!)
   
   ;; console and DSO have only been implemented under LispWorks CAPI
   #+capi
   (:module "console" :serial t
    :components
    ((:file "diagnostics-panel") ; a generalized simple grid for presenting test results (or just tracking them if manually operated)
     (:file "s79-console-defs")
     (:file "s79-console")
     (:file "dso")))

   ;; similarly for diagnostics panel
   #+capi
   (:file "diagnostics-support")

   ;; scheme->s-code compiler
   (:module "s-code" :serial t
    :components
    ((:file "s-code"))) ; more to come

   (:module "tests" :serial t
    :components
    ((:file "test-support")  ; functions used by the test suites not defined as part of diagnostics (machine specific)
     (:file "compiler-regression-tests"))) ; 5am tests just on the compiler output
   
   (:file "load-last")))
