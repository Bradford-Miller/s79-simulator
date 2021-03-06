(in-package :scheme-79)

(scheme-79-version-reporter "S79 Project Defs" 0 4 0
                            "Time-stamp: <2022-03-18 15:31:31 gorbag>"
                            "line disambiguation")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 0.4.0   3/18/22 snapping a line: 0.4 release of scheme-79 supports test-0 thru test-3. ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;         2/24/22 add some comments about project variables *word-size* and
;;                    *register-size*

;; 0.3.1   2/ 9/22 way too many things (fns, variables) with "line" in their name
;;                    and it's ambiguous.  Splitting so "line" refers to,
;;                    e.g. an output (log) line, "expression" refers to a
;;                    'line' of code (single expression in nano or microcode
;;                    land typically, and because we used (READ) it wasn't
;;                    confined to a single input line anyway) and "wire" to
;;                    refer to, e.g., a control or sense 'line' on a register.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 0.3.0   1/11/22 snapping a line: 0.3 release of scheme-79 supports  test-0 and test-1. ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; 0.1.2  1/11/22 add method for announce-project-version which gets invoked
;;                  from the support library

;; 0.1.1  1/ 7/22 move setup of debug flags defined in support/project-defs
;;                  here

;; 0.1.0  9/10/21 Separate from scheme-79-defs as this describes the
;;                  PROJECT to segregate from stuff about the MACHINE
;;                  or SIMULATION

;; development banners

(defparameter *scheme-79-version-number* "0.4")

;; 0.0.3  1/11/21 repatriated much of register support and high level
;;                    compiler/assembler to fpga-support. Also made
;;                    some fns generic so more detailed/specific versions
;;                    can be defined by the project. Macros for most of
;;                    the chip setup were also repatriated, but the actual
;;                    invoke is still here with the project.
;;                Some minor refactoring as well to better segregate
;;                    between general processor definition and specifics for
;;                    a particular processor project, but more needs to be
;;                    done in that direction.

;; N/A   11/21/21 test-1 passed; may have been before this date since
;;                    I failed to notice it was generating a correct
;;                    memory layout but tester wasn't looking at that.
;;                    test-1 tests types on the stack (so we can do
;;                    dispatch-on-stack), type constants (&set-type),
;;                    more predicates (not just the one inside of
;;                    cons), marking/unmarking, cond/if, etc. while
;;                    performing a mark/sweep compacting GC. Deferring
;;                    release until I clean up some of the code and
;;                    reorganize a bit.

;; 0.0.2  8/20/21 test-0 passed (stack operations, read interrupt
;;                    pointer, external memory simulation, memory
;;                    read/write (car and cdr), source level support
;;                    in console (report microcode by expression) stack
;;                    support in console (show top N values),
;;                    step/u-step/run/halt support in console, basic
;;                    microcode and nanocode, test setup and
;;                    evaluation, diagnostics panel, DSO, reset wire,
;;                    halt address reached test,

;; 0.0.1  : 1/ 9/21 Registers and control wires tested and working
;; through console (clock synchronous)

(defmethod announce-project-version :after (&optional (stream *error-output*) version-only-p)
  (announce-scheme-79-version stream version-only-p))

(defun announce-scheme-79-version (&optional (stream *error-output*) version-only-p)
  ;(fpga-support:announce-fpga-support-version stream version-only-p)

  (announcement-banner (format nil "Scheme-79 VERSION ~A" *scheme-79-version-number*)
                       stream)

  (unless version-only-p
    (let ((*error-output* stream))
      (report-version nil 'cl-user::*scheme-79-version-reporter-initializations*)))
  
  (terpri stream))

(eval-when (:load-toplevel :execute)
  (setq *debug-validator* nil) ; verbose ucode validation
  (setq *debug-pad-timing* nil) ; log timing info based on set-pad and clear-pad
  (setq *debug-compiler* t) ;log issues instead of breaking
  (setq *debug-assembler* t) ; keep intermediate files from assembler passes

  ;; these are the default values from fpga-support, but in case we want to
  ;; change them in the future...

  ;; Note there would be a large impact on other project-level code if we do...
  
  ;;(setq *word-size* 32)
  ;;(setq *register-size* *word-size*)
  )

