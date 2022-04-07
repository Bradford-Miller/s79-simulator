(in-package :scheme-mach)

(scheme-79:scheme-79-version-reporter "Scheme Machine Micro" 0 4 2
                                      "Time-stamp: <2022-04-07 12:32:34 gorbag>"
                                      "repatriate")

;; 0.4.2   4/ 7/22 code used to analyze microlisp and build PLA split off to
;;                    plas/mcode-assembler

;; 0.4.1   3/23/22 note-banner no longer needs 2nd arg

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 0.4.0   3/18/22 snapping a line: 0.4 release of scheme-79 supports test-0 thru test-3. ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 0.3.6   2/18/22 make set-micro-pc generic so we can hook it (e.g.
;;                    with an :around method)

;; 0.3.5   2/15/22 call note-breakpoint-reached so console can refresh 
;;                    properly

;; 0.3.4   2/ 3/22 when we detect that the micro-pc is being set to the
;;                    same address and we are running (not single
;;                    stepping) we execute (stop).

;; 0.3.3   1/26/22 add register use for micro-call, micro-return
;;                 support "permanent" breakpoints in clear-breakpoints,
;;                   set-breakpoint, etc.

;; 0.3.2   1/25/22 separate out uops in analyze-code that have implicit
;;                    register references and add those to our counts

;; xxxxx   1/19/22 remove some TBDs in the comments (they were done already)

;; 0.3.1   1/13/22 change references from internal-freeze to run-nano
;;                    for consistancy with AIM

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 0.3.0   1/11/22 snapping a line: 0.3 release of scheme-79 supports  test-0 and test-1. ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; 0.1.16  1/ 6/22 move mark-register-use to ulisp-validation
;;                 make analyze-code generic as validator uses it in support library
;;                 use property accessor fns as appropriate

;; 0.1.15  1/ 5/22 fix package refs

;; 0.1.14 12/14/21 use new special-register-p, existing register-flag-accessor fns

;; 0.1.13 12/ 3/21 update scheme-79-mcr-i -> "" (if from microlisp-int or fpga-pla-build-tools)
;;                   update scheme-79-mcr -> microlisp

;; 0.1.12 11/24/21 user note-banner

;; 0.1.11 10/25/21 following up with that conversion, now use pla-read
;;                     instead of aref on plas

;; 0.1.10 10/20/21 *micro-pc* is now a bit vector...

;; 0.1.9  10/13/21 set *last-conditional-result* for diagnostics

;; 0.1.8  10/ 7/21 add support for breakpoints (based on uaddresses)

;; 0.1.7  10/ 5/21 since we don't have actual continuous update until a
;;                     latch operation, some actions have to
;;                     be rerun on phase changes or other events to
;;                     correctly reflect their state. In this case we
;;                     add a run-sense-controls immediately before
;;                     run-microcontroller, specifically so
;;                     conditionals update the PC based on the most
;;                     current register values. Our adding an update
;;                     immediately after we run the register controls
;;                     (in machine-nano) should be sufficient of
;;                     course, but this just "makes sure" ;-), like
;;                     "sync;sync;sync" in unix.

;; 0.1.6   9/30/21 fix cases in run-microcontroller - bus not being
;;                     loaded with type under
;;                     sense-type-and-branch-const

;; 0.1.5   9/28/21 when we update the u-pc, also record the from and to
;;                     register values of the new instruction so we
;;                     can make better sense of the logs (what the
;;                     registers had at the time the instruction
;;                     executed!)

;; 0.1.4   9/27/21  scheme-79-mcr:progn, if, cond

;; 0.1.3   9/14/21 when printing annotation for microfunction start,
;;                     show conditional effect

;; 0.1.2   8/26/21 &set-type fixed in analyze-expression
;;                 use register-p fn

;; 0.1.1   8/24/21 differentiate microcode 'cond' from cl:cond, if from cl:if

;; 0.1.0   8/20/21 "official" 0.2 release: test-0 passed!

;; 0.0.16     8/16/21 print out the microfunction (source code) to the debug stream

;; 0.0.15     8/ 7/21 add metric collection when micro-pc updated

;; 0.0.14     8/ 6/21 mark pseudo-register usage of *address*, *memory*
;;                       and *interrupt* for statistical purposes

;; 0.0.13     7/19/21 set-micro-pc (to make it easier to trace updates)

;; 0.0.12     7/ 7/21 Check *internal-freeze* (now run-nano 1/13/22)

;; 0.0.11     6/27/21 Use test-pad-immediate on *freeze*; delay
;;                       run-microcontroller until :ph2-high so we can
;;                       allow *freeze* to be asserted by prior
;;                       nanocontroller activity (when multi-
;;                       instruction)

;; 0.0.10     6/26/21 Consolodate debug flags to scheme-79-defs.lisp

;; 0.0.9      6/25/21 add *debug-microcontroller* parameter

;; 0.0.8      3/29/21 use test-pad to check *freeze* in run-microcontroller

;; 0.0.7      3/15/21 collect-ucode: check for loops
;;                       implement run-microcontroller during :ph2-rising
;;                       - checks if nanocontroller has run on current microcode
;;                         and if so, updates *micro-pc* (unless *freeze* is asserted)

;; 0.0.6      3/ 2/21 stub finalize-microcontroller-array

;; 0.0.5      2/27/21 translate aliased registers (translate-alias)

;; 0.0.4      2-25-21 only treat :to references in mark-register-use
;;                       as potentially special...

;; 0.0.3      2-24-21 stub run-microcontroller placeholder
;;                       not sure yet if this is needed anymore since
;;                       most stuff happens automatically off the clock
;;                       which is as it should be. But we have a run-nanocode
;;                       (so far ;-) so this is symmetric.

;; 0.0.2      2-13-21 add save and restore to analyze-expression

;; 0.0.1      1-17-21 analyze-code

;; 0.0.0      1-14-21 new

;; Closer to a hardware simulator corresponding with AIM 559
;; based on the published microcode and description

;; here we implement the various described mechanisms on the chip
;; which we are simulating.

(defun get-condition (pc)
  (declare (bit-vector pc))
  ;; pc points to a BRANCH instruction, determine the condition function
  (destructuring-bind (next nano-op from sense-code) (pla-read *microcontrol-array* pc)
    (declare (ignore next nano-op from))
    (let* ((sense-wire-symbol (sense-wire-encoding-to-symbol sense-code))
           (test-fn (register-flag-accessor (sense-wire-name sense-wire-symbol)))
           (register-symbol (sense-wire-register sense-wire-symbol)))
      #'(lambda () 
          (progfoo (funcall test-fn register-symbol)
            (when *debug-microcontroller*
              (note "checking sense-wire ~A : ~S" sense-wire-symbol foo)))))))
    
(defgeneric set-micro-pc (new-pc-value)
  (:documentation "setting up the micro-pc for a new value, of interest to debugging tools too"))

(defmethod set-micro-pc (new-pc-value)
  (let* ((new-pc-integer (if (integerp new-pc-value)
                            new-pc-value
                            (bit-vector->integer new-pc-value)))
         (new-pc-bits (if (integerp new-pc-value)
                          (integer->bit-vector new-pc-value)
                          new-pc-value))
         (conditional-p (test-pad-immediate '*conditional*))
         (condition-holds-p (when conditional-p (funcall (get-condition *micro-pc*)))))
    
    (when conditional-p
      (setq *last-conditional-result* condition-holds-p)) ; so we can update diagnostics later
    
    (when *debug-microcontroller*
      (let ((u-instruction (elt *microcontrol-array* new-pc-integer)))
        (destructuring-bind (next nano-op from to) (if u-instruction u-instruction '(nil nil nil nil))
          (declare (ignore next nano-op))

          (note "updating micro-pc to #o~o (tick ~D ph ~s)" new-pc-integer *tick* *symbolic-clock-phase*)
          (let ((from-reg (from-code-to-anaphor from))
                (to-reg (to-code-to-anaphor to))
                (from-to-reg (from-code-to-anaphor to)))
            (note "from: [~d] ~A (~o) to: [~d] ~A (~o) from-to: ~A (~o)"  ; put the register values into the log so we can see them later
                  from from-reg (if (not (null from-reg))
                                    (symbol-value from-reg)
                                    0)
                  to to-reg (if (not (null to-reg))
                                (symbol-value to-reg)
                                0)
                  from-to-reg (if (not (null from-to-reg))
                                  (symbol-value from-to-reg)
                                  0)))))

      ;; probably should use note-banner for this (tbd)
      (note "*************************************************") ; make it easy to find in the log
      (cl:cond
       ((not condition-holds-p) ; might not be a conditional
        (let*-non-null ((annotation (get-uc-annotation new-pc-integer)))
          (note "*** Microfunction Start: ~A ~A***" annotation (cl:if conditional-p "(Condition fails) " ""))))
       (t ; it was conditional and it holds
        (let*-non-null ((annotation (get-uc-annotation (logior new-pc-integer #o1))))
          (note "*** Microfunction Start: ~A (Condition holds) ***" annotation))))
      (note "*************************************************"))
    
    (when (and *halt-address* ; note we always check this and before we check
                              ; for a loop (typical halt address is a loop on
                              ; that address)
               (= new-pc-integer *halt-address*))
      (note-banner '("HALT ADDRESS REACHED!"))
      (stop))

    (flet ((set-new-pc (bits)
             (cl:cond
              ((and *debug-microcontroller* ; add a tight loop test when debugging
                    (running-p)
                    (equalp bits *micro-pc*)) ;trying to set to current address
               (note-banner '("Loop at micro address #o~o detected! Stopping"))
               (stop))
              (t
               (copy-field bits *micro-pc*)))))
      ;; if we are testing a conditional, we need to OR in the low order bit of the new address with the condition
      (cl:cond
       ((and conditional-p
             (not (test-pad-immediate '*reset*))) ; if we're resetting just get on with it

        (note-if *debug-microcontroller* "evaluating conditional effect on pc")

        (set-new-pc
         ;; now that we've changed our representation to bit-vector, it
         ;; should be easier to just deal with the low order bit, no?
         ;; That's how the FPGA will do it!!
         (integer->bit-vector 
          (cl:cond
           (condition-holds-p
            ;; or in as low order bit of pc
            (progfoo (logior new-pc-integer #o1)
                     (note-if *debug-microcontroller* "condition TRUE, new pc: ~o" foo)))
           (t
            (note-if *debug-microcontroller* "condition FALSE, new pc: ~o" new-pc-integer)
            new-pc-integer)))))

       (t
        (set-new-pc new-pc-bits)))

    (unless (test-pad-immediate '*reset*)
      #+capi (s79-console:update-microinstruction-metrics (bit-vector->integer *micro-pc*))) ; only if diagnostics are available (needs capi since we store the tests on the panel)

    ;; check for breakpoint
    (when (member (bit-vector->integer *micro-pc*) *breakpoints* :test #'=)
      (note-banner '("BREAKPOINT REACHED!"))
      (set-running-p nil)
      #+capi (s79-console:note-breakpoint-reached)) ; let the console know about it

    *micro-pc*)))

(defun clear-breakpoints ()
  (setq *breakpoints* nil)
  (setq *breakpoint-descs* (delete nil *breakpoint-descs* :key #'cdr)) ; get rid of all temps
  (mapc #'(lambda (entry)
            (pushnew (car entry) *breakpoints*)) ; reestablish permanents
        *breakpoint-descs*))

(defun clear-breakpoint (uaddress)
  (when (assoc uaddress *breakpoint-descs*)
    (setq *breakpoint-descs* (delete uaddress *breakpoint-descs* :key #'car)))
  (setq *breakpoints* (delete uaddress *breakpoints*)))

(defun set-breakpoint (uaddress &optional breakpoint-type)
  "Breakpoint-type can be nil for a temporary or :permanent for a
permanent breakpoint (will not be cleared by (clear-breakpoints) though
(clear-breakpoint) can still do so)"
  (update-alist uaddress breakpoint-type *breakpoint-descs*)
  (pushnew uaddress *breakpoints*))

(defun run-until-breakpoint (uaddress)
  (set-breakpoint uaddress)
  (run))

;;; the microcode simulator
(defun run-microcontroller ()
  "Basically just queues up the nanocontroller to do the actual work"
  ;; has the nanocontroller run on the current instruction?
  (cl:cond
    ((and *nanocontroller-ran-p*
          (not (test-pad-immediate '*freeze*))
          (not (test-pad-immediate '*run-nano*)))
     (let ((new-upc (car (pla-read *microcontrol-array* *micro-pc*))))
       (setq *nanocontroller-ran-p* nil)
       (set-micro-pc new-upc)

       ;; if the instruction involves moving a constant value to the bus, we do
       ;; that here, though it really should happen as a result of executing
       ;; the nanocode (TBD)

       ;; nanocode will handle the TO expression and then the shift from the
       ;; bus into the destination happens during *run-register-controls* (next
       ;; tick)

       (destructuring-bind (next-ucode nop from to) (pla-read *microcontrol-array* *micro-pc*)
         (declare (ignore next-ucode to))
         (let ((nanoop (car (rassoc nop *nanocontrol-symtab*))))
           (case nanoop ; this is just for programmer's convenience so cases are symbolic
             ((microlisp-shared::do-set-typec-exp
                microlisp-shared::do-set-typec-newcell
                microlisp-shared::do-set-typec-val
                microlisp-shared::do-set-typec-retpc-count-mark
                microlisp-shared::do-set-typec-stack
                microlisp-shared::sense-type-and-branch-const)
              (load-bus-field 'type from)) ;use the constant or register in the from field

             ;; we may not need this case since it should be standard
             ((microlisp-shared::do-set-type-exp
                microlisp-shared::do-set-type-newcell
                microlisp-shared::do-set-type-val
                microlisp-shared::do-set-type-retpc-count-mark
                microlisp-shared::do-set-type-stack
                microlisp-shared::sense-type-and-branch)
              (let ((source-register (from-code-to-anaphor from)))
                (assert source-register () "Invalid FROM for ~A" nanoop)
                (load-bus (symbol-value source-register))))

             (t ; nanocode will do it all
              nil))))))
        
    (*debug-microcontroller*
     (note "not updating micro-pc (tick ~D ph ~s)" *tick* *symbolic-clock-phase*)
     nil)
    (t
     nil))
     
  *micro-pc*)

;; changed to ph1-high to allow other ph1-rising signals to stabilize (including *freeze* !) 6/27/21
(execute-during-clock ("run-microcontroller" *run-microcontroller*)
  (progn (run-sense-controls) ; so conditionals will be evaluated correctly                    
         (run-microcontroller)))
