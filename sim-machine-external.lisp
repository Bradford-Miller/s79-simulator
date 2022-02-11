(in-package :scheme-mach)

(scheme-79:scheme-79-version-reporter "Scheme Machine Sim Ext Ops" 0 3 2
                                      "Time-stamp: <2022-02-09 12:34:47 gorbag>"
                                      "line disambiguation")

;; 0.3.2   2/ 9/22 way too many things (fns, variables) with "line" in their name
;;                    and it's ambiguous.  Splitting so "line" refers to,
;;                    e.g. an output (log) line, "expression" refers to a
;;                    'line' of code (single expression in nano or microcode
;;                    land typically, and because we used (READ) it wasn't
;;                    confined to a single input line anyway) and "wire" to
;;                    refer to, e.g., a control or sense 'line' on a register.

;; 0.3.1   1/18/22 cleanup obsolete code: removing special treatment of registers
;;                    which required multiple control wires for TO as new covering
;;                    set computation deals with it.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 0.3.0   1/11/22 snapping a line: 0.3 release of scheme-79 supports  test-0 and test-1. ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; 0.1.9   1/10/22 split variables and pad defs to sim-machine-external-defs

;; 0.1.8   1/ 5/22 make microlisp package refs explicit where needed

;; 0.1.7  12/14/21 use new special-register-p fn

;; 0.1.6  12/ 3/21 update scheme-79-mcr -> microlisp

;; 0.1.5  12/ 1/21 have boot fn dump initial memory contents to
;;                     background stream for human comparison to final
;;                     content (if needed)

;; 0.1.4  11/ 2/21 clear register-description-pane on reset

;; 0.1.3   9/ 9/21 limit copy to address pads to *address-field-length*

;; 0.1.2   9/ 4/21 use non-pointer-type-name->int

;; 0.1.1   8/24/21 cond vs. cl:cond, if vs. cl:if

;; 0.1.0   8/20/21 "official" 0.2 release: test-0 passed!

;; 0.0.20  8/17/21 use note-if

;; 0.0.19  8/13/21 set halt-address when calling test

;; 0.0.18  8/11/21 assign <reg> &get-interrupt-pointer didn't specify to-register <reg> in compilation; also
;;                   when destination is *stack* use specialized version (stack is not valid for TO, must use
;;                   TO-TYPE and TO-ADR)

;; 0.0.17  8/ 3/21 additional detail to debug messages

;; 0.0.16  7/27/21 adjust timing for *read*, *write* and *cdr* so a full clock each.

;; 0.0.15  7/26/21 run-cache-sync should also copy *interrupt-pads* to
;;                   *interrupt* when *read-interrupt*

;; 0.0.14  7/20/21 run-cache-sync to sync between pads and internal
;;                   cache

;; 0.0.13  7/ 7/21 *ale* is now high for a full tick (ph2-rising to
;;                   ph2-rising)

;; 0.0.12  6/25/21 reset now issues a warning if there is no microcode
;;                   or it has not been validated/compiled.

;; 0.0.11  4/26/21   increase freeze validity to 6 phases (given 8 microticks per cycle)
;; 0.0.10  3/29/21   temporarily(?) change check of *reset* pin to :ph1-falling (probably a different bug)
;; 0.0.9   3/18/21   change check of *reset* pin to :ph1-rising
;; 0.0.8   2/19/21   eliminate unneeded assert in &get-interrupt-routine-pointer
;; 0.0.7   2/14/21   implement &get-interrupt-routine-pointer
;; 0.0.6   2/11/21   add *cold-boot-memory-array* to allow test functions to directly store things in memory
;;                     during reset (should be very useful for testing)
;; 0.0.5   1/31/21   move simulation of external memory to external-support.lisp
;;                     add ph1 and ph2 as outputs (for now) - inputs in original but we need to synchronize...
;; 0.0.4   1/25/21   add first part of reset logic (set up external memory)
;; 0.0.3   1/20/21   extend pad definitions with clock cycle validity information
;; 0.0.2   1/15/21   define gc-needed pad as a :latched-output
;; 0.0.1   1/13/21   Implement external memory functions (read-address, write-address...)
;;                     Document new "reset" pad
;;                     Add pad declarations based on the new macros

;; Closer to a hardware simulator corresponding with AIM 559
;; based on the published microcode and description

(defun machine-ready-p (&optional warn-p)
  (let ((retval t))
    ;; check that we have microcode
    (unless (some #'identity *microcontrol-array*)
      (setq retval nil)
      (when warn-p
        (warn "No microcode loaded!")))

    ;; check that we have precompiled tables
    (mlet (ignore1 from-anaphor-table ignore2 to-anaphor-table)
        (debug-anaphors)  
      (declare (ignore ignore1 ignore2))
      (unless (or (some #'identity from-anaphor-table)
                  (some #'identity to-anaphor-table))
        (setq retval nil)
        (when warn-p
          (warn "microcode has not been validated! (no anaphor tables)"))))
    retval))

(defun do-reset ()
  (declare (special *halt-address* s79-console:*console*))
  
  (note "invoking do-reset T: ~D uT: ~S" *tick* *symbolic-clock-phase*)
  ;; clear the reset wire until/unless we have an external driver
  ;; 7-25-21 note that reset will clear automatically at the end of the cycle so leave it alone for now.
  ;;(clear-pad '*reset* t)
  (machine-ready-p t) ; we reset regardless, but issue warnings if relevant
  
  (init-memory-for-cold-boot) ; presumably some external logic normally does this...
  (init-clock t)

  ;; initialize the micro-pc to boot-load
  (set-micro-pc (non-pointer-type-name->int 'microlisp::boot-load))
  ;; if there is a DONE tag, use it for the halt-address
  (let*-non-null ((halt-tag-address (non-pointer-type-name->int 'microlisp::done)))
     ;; for the console to auto-stop, and maybe kick off test validation
     ;; note the tag is a goto instruction in the initial table, so we look there for the actual
     ;; loop address
     (setq *halt-address* (car (elt *microcontrol-array* halt-tag-address)))
     (note "Auto-halt set to done"))
  ;; if the console is running, clear out prior test status
  (when s79-console:*console*
    (s79-console:clear-register-description-pane s79-console:*console*))
  
  ;; record initial content of memory
  (format hcl:*background-output* "~&~%Initial memory contents~%")
  (let ((*error-output* hcl:*background-output*))
    ;; may have garbage in the registers, so set range to dump
    (dump-memory 0 :end *initial-memtop*))
  
  (note "*micro-pc* set to boot-load: good luck!"))

;; related uinstructions:

;; &get-interrupt-routine-pointer
(defufn &get-interrupt-routine-pointer (:constituent t)

  (ecase *enclosing-opcode*
    (assign ; better have a *to-register*...
     (write-generated-code *upla-stream* nil
                           `(((to ,*to-register*) microlisp-shared::do-simple-get-interrupt-pointer))
                           "&get-interrupt-routine-pointer"))))

;; &read-from-pads

;; &write-to-pads

;; MEMORY:

;; here we simulate the chip's external memory. Eventually I hope to
;; make it easier to load/dump as well, but for now we just establish
;; enough for the boot sequence.

(defun init-memory-for-cold-boot ()
  ;;this is driven by 'external circuitry'...
  "Simple minimal memory initialization based on paper" 
  ;; need to make address 0 car be NIL and address 0 CDR be NIL's property list (OBARRAY).
  ;; car of address 1 should be the initial *memtop*
  (let ((external-chips::*warn-when-beyond-memtop* nil)) ; haven't set memtop yet
    (declare (special external-chips::*warn-when-beyond-memtop*)) ; load order
    
    (external-chips:write-address 0 nil 0)
    (external-chips:write-address 0 t 0) ;for now - need to set up an obarray!
    (external-chips:write-address 1 nil *initial-memtop*)

    ;; initialize memory after that (e.g. with a program)
    (when *cold-boot-memory-array*
      (let ((addr 2))
        (cl:cond
         ((consp *cold-boot-memory-array*) ; can implement as list or array!
          (let ((external-memory-copy (copy-list *cold-boot-memory-array*))) ; so we can pop it
            (while external-memory-copy
              (external-chips:write-address addr nil (pop external-memory-copy))
              (when (car external-memory-copy)
                (external-chips:write-address addr t (pop external-memory-copy)))
              (incf addr))))
         (t ; should be a vector then
          (let ((ptr 0))
            (while (< ptr (fill-pointer *cold-boot-memory-array*))
              (external-chips:write-address addr nil (aref *cold-boot-memory-array* ptr))
              (incf ptr)
              (when (< ptr (fill-pointer *cold-boot-memory-array*))
                (external-chips:write-address addr t (aref *cold-boot-memory-array* ptr))
                (incf ptr)
                (incf addr))))))))))

;; synchronous copy from/to cache and pads

(defun run-cache-sync ()
  ;; check the value of signals and see if we need to copy from or to our internal cache
  (cl:cond
    ((test-pad-immediate '*ale*) ; we need to push out an address
     (note-if *debug-external-pads*
              "run-cache-sync: Copying *address* ~s to *address-pads*" *address*)
     (copy-field *address* *address-pads* :result-bits *address-field-length*)) ; limit to address length so we don't put the type on the address bus.

    ((test-pad-immediate '*read*)
     (note-if *debug-external-pads*
              "run-cache-sync: Copying *memory-pads* ~s to *memory*" *memory-pads*)
     (copy-field *memory-pads* *memory*))

    ((test-pad-immediate '*write*)
     (note-if *debug-external-pads*
              "run-cache-sync: Copying *memory* ~s to *memory-pads*" *memory*)
     (copy-field *memory* *memory-pads*))
    ;; read-interrupt works like read (timeing wise) but no ALE prompt.
    ((test-pad-immediate '*read-interrupt*)
     (note-if *debug-external-pads*
              "run-cache-sync: Copying *interrupt-pads* ~s to *interrupt*" *interrupt-pads*)
     (copy-field *interrupt-pads* *interrupt*))))

(execute-during-clock ("synchronous copy from and to pads" *run-external-data-transfer*)
   (run-cache-sync))
