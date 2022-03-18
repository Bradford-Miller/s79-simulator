(in-package :external-chips)

(scheme-79:scheme-79-version-reporter "Scheme Storage Manager" 0 4 0
                                      "Time-stamp: <2022-03-18 15:28:29 gorbag>"
                                      "*word-size* get-address-bits")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 0.4.0   3/18/22 snapping a line: 0.4 release of scheme-79 supports test-0 thru test-3. ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 0.3.6   3/14/22 more enhancements to dump-memory to show which
;;                    locations are part of the stack

;; 0.3.5   2/24/22 use new *word-size* parameter, get-address-bits fn

;; 0.3.4   2/23/22 enhance dump-memory to show which registers refer to a 
;;                    given memory location

;; 0.3.3   2/18/22 dump-memory cosmetics

;; 0.3.2   2/ 4/22 add a decode-p arg to dump-memory. It will report the
;;                    mark bit and type for the car and cdr without also 
;;                    being as verbose as dump-memory-with-types

;; 0.3.1   1/14/22 have the debug message for memory read report what was read
;;                    fix dump-memory to write final cdr (fencepost error)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 0.3.0   1/11/22 snapping a line: 0.3 release of scheme-79 supports  test-0 and test-1. ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; 0.1.6   1/ 7/22 use new pad-defn and get-pad-defn-value functions

;; 0.1.5  12/15/21 microlisp -> microlisp-shared

;; 0.1.4  12/ 3/21 .. 12/10/21 update scheme-79-mcr-i -> "" (if from
;;                      microlisp-int or fpga-pla-build-tools)
;;                             update scheme-mach -> microlisp if register or pad name

;; 0.1.3  11/12/21 new dump-memory-with-types to make reading memory
;;                     dumps easier

;; 0.1.2  10/12/21 compare-memory should return immediately on failure

;; 0.1.1   9/14/21 read- and write-address now mask off anything but the
;;                     low 24 bits of the address

;; 0.1.0   8/20/21 "official" 0.2 release: test-0 passed!

;; 0.0.12    8/20/21 new compare-memory fn for testing the content of the
;;                     (external) memory against a goal to be used for test
;;                     functions to check if a particular result has obtained.

;; 0.0.11    8/17/21 use note-if

;; 0.0.10    8- 2-21  more extensive comments on *interrupt-address*

;; 0.0.9     7-30-21  use variables instead of hardcoded phases set
;;                       up in clock-triggers.lisp

;; 0.0.8     7-26-21  make debug messages from monitor-pads clearer
;;                       (they're from perspective of the external
;;                       circuit, not the chip!)
;;                    make integer->bit-vector specify the number of
;;                       bits (else it uses the minimum and we'll get
;;                       a length mismatch when we copy it to the
;;                       register)

;; 0.0.7     7-20-21  *input-pad-types* *output-pad-types*
;;                       debug messages for monitor-pads

;; 0.0.6     7-15-21  add option to read-in integer as integer (for console display)

;; 0.0.5     7- 7-21  turn read-in integers into bit-vectors (read-address, write-address)

;; 0.0.4     7- 6-21  move test-pad-immediate to machine-defs.lisp

;; 0.0.3     6-26-21  move *debug-external-pads*, *warn-when-beyond-memtop*
;;                       and *dump-values-per-row* to scheme-79-defs

;; 0.0.2     3-11-21  fix extraneous 'car' when writing to memory

;; 0.0.1     2- 1-21 explicitly tag references to scheme-mach functions
;;                       and variables used for debug or explicit
;;                       synchronization with machine design
;;                       (e.g. memory size) NB: outside of debug we
;;                       need to make sure we don't reference internal
;;                       state - it must be on pads!

;; 0.0.0     1-31-21 move memory interface code here from
;;                     sim-machine-external.

;; Isolating simulation routines that are external to the scheme-79 chip in this file, including memory.

;; now we define the code that performs the read and write cycles to
;; the memory, i.e. puts/gets data to/from the pads during the right
;; clock cycles note that it should react to the pads - i.e. given
;; what it sees on the simulated external pads of the chip it should
;; gate in the information from memory.

;; note that the reading and writing of pseuo-registers or loading
;; them from the pads should happen in nanocode, not here. This is
;; pretending we have interface logic to external memory chips and how
;; those chips will react to seeing the appropriate signals on the
;; external pins of the scheme-79 chip!
(add-initialization "External Support Monitor Pads"
                    '(monitor-pads)
                    ()
                    '*all-ph-list*)

;; set up in case we want to provide an interrupt address when the chip asks
(defvar *interrupt-address* 0
  "This should be (bound to) a memory address for the interrupt
  handler that will be returned when read-interrupt is asserted. 
  This is handled by (monitor-pads), but a more sophisticated 
  external hardware simulation (e.g. for DMA or something) might
  implement something different.

  Eventually we should make this point to a default
  location (e.g. HALT or some such)")

;; pad access

;; by now the pads have been defined so we can import them (export was
;; through a macro so we can't do this in package.lisp)
(import '(microlisp-shared:*ale*))

(defun set-pad-external (pad-name)
  ;; pad better be a chip input or io
  (assert 
   (and (pad-defn pad-name)
        (member (get-pad-defn-value pad-name :type) *input-pad-types*))
   (pad-name)
   "Pad ~s is not an input or io pad" pad-name)

  (note-if *debug-external-pads* "SPE: Setting pad ~A tick ~D clock ~S" pad-name *tick* *symbolic-clock-phase*)

  (setf (bit (symbol-value pad-name) 0) 1))

(defun clear-pad-external (pad-name)
  (assert 
   (and (pad-defn pad-name)
        (member (get-pad-defn-value pad-name :type) *input-pad-types*))
   (pad-name)
   "Pad ~s is not an input or io pad" pad-name)

  (note-if *debug-external-pads* "CPE: Clearing pad ~A tick ~D clock ~S" pad-name *tick* *symbolic-clock-phase*)
  
  (setf (bit (symbol-value pad-name) 0) 0))

(let ((expect-address nil)
      (address 0))
  ;; for debugging
  (defun monitor-pads-state ()
    (note "expect-address: ~s address: ~s" expect-address address))

  (defun monitor-pads ()
    "Main loop for external support chips - observes the pad pattern and executes a state machine"
    ;; we get called on each phase change of the clock, so we need to
    ;; match the phase with the clock.  I matched this as best I can
    ;; with figures 8 and 9 of the AIM, but note that it shows data
    ;; validity late in the phase1 cycle dropping at phase1 end, so no
    ;; really good place to trigger, so I'm using the mid cycle (after
    ;; the phase rises) as the place to gate data in and out. In
    ;; hardware we can add a delay to be mid-cycle for a given clock
    ;; frequency... or we can be better about making sure the vaility
    ;; is determined across one of the clock edges... one possibility
    ;; is we'd have it valid during :ph1-falling and then drop it
    ;; during :ph2-rising. Two phase clocks seem to be out of style
    ;; these days regardless, as is triggering on the falling edge but
    ;; I'm trying to get as close to the original design as I can
    ;; here.
    (let ((current-clock *symbolic-clock-phase*))
      (cond
        ((and (eql current-clock *test-ale-to-expect-address*)
              (test-pad-immediate 'microlisp-shared:*ale*))

         (note-if *debug-external-pads* "monitor-pads: external circuit expecting an address in next tick")
        
         (setq expect-address t))

        ((and (eql current-clock *get-address-from-pads*)
              expect-address) ;; not sure we can't just check that ALE
                              ;; is high here since it would be during
                              ;; phase 1 if an address is gated out.

         (note-if *debug-external-pads* "monitor-pads: external circuit reading the address from the pads ~s"
                  microlisp-shared:*address-pads*) ; should be defined by now
         
         (setq address (bit-vector->integer microlisp-shared:*address-pads*))
         (setq expect-address nil))

        ((and (eql current-clock *put-memory-content-onto-pads*)
              (not (test-pad-immediate 'microlisp-shared:*freeze*))
              (test-pad-immediate 'microlisp-shared:*read*))


         (progfoo (external-memory-read address)
                  (note-if *debug-external-pads* "monitor-pads: external circuit read ~s at address ~s" foo address)))
        
        ((and (eql current-clock *get-memory-content-from-pads*)
              (not (test-pad-immediate 'microlisp-shared:*freeze*))
              (test-pad-immediate 'microlisp-shared:*write*))

         (note-if *debug-external-pads* "monitor-pads: external circuit writing ~s to address ~s"
                  microlisp-shared:*memory-pads* address)
         
         (external-memory-write address))
        ((and (eql current-clock *test-for-read-interrupt*)
              (test-pad-immediate 'microlisp-shared:*read-interrupt*)) ; like a read but a special interrupt line from external systems.
         ;; (also used during the boot process to set the initial stack)

         (note-if *debug-external-pads*
                  "monitor-pads: external circuit sending an interrupt vector ~s"
                  *interrupt-address*)
         
         (external-interrupt-vector *interrupt-address*))
        ))))
  
(defun external-memory-read (address)
  "perform a READ based on the pad pattern"
  (copy-field (read-address address (test-pad-immediate 'microlisp-shared:*cdr*)) microlisp-shared:*memory-pads*))

(defun external-memory-write (address)
  "perform a WRITE based on the pad pattern"
  (write-address address (test-pad-immediate 'microlisp-shared:*cdr*) microlisp-shared:*memory-pads*))

;; while we're at it

(defun external-interrupt-vector (address)
  "sets the pads during an interrupt cycle to a particular interrupt vector"
  (copy-field (integer->bit-vector address :result-bits scheme-mach::*address-field-length*) 
              microlisp-shared:*interrupt-pads*))

;; code to simulate an external RAM

(defparameter *format-car-for-dm-with-types* "M=~o P=~o T=~A ~48tD~4,'0o F~4,'0o A~8,'0o~58t . ")

(defparameter *format-cdr-for-dm-with-types* "M=~o P=~o T=~A ~110tD~4,'0o F~4,'0o A~8,'0o")

(defparameter *format-car-for-dm-decode-p* "~A~A ~40t~8,'0o~49t . ")

(defparameter *format-cdr-for-dm-decode-p* "~A~A ~80t~8,'0o ~A")

(defvar *memory-vector* (make-array scheme-mach:*maximum-memory-size* :element-type 'fixnum :initial-element 0)
  "The simulator's memory for the machine. Segregated so we don't clear it when recompiling the following")

(defun invalid-address-p (address &optional error-p)
  (let ((int-address (cl:if (bit-vector-p address)
                       (bit-vector->integer address)
                       address)))
    (unless (< int-address scheme-mach:*maximum-memory-size*)
      (cl:if error-p
        (error "Address ~24,'0o is outside attached memory bounds" int-address)
        :memory-bounds-exceeded))
    (unless (<= int-address (bit-vector->integer microlisp-shared:*memtop*)) ; we're doing this for debug, not effect so OK to reference internal register...
      (cl:if error-p
        (error "accessing memory at ~24,'0o which is beyond *memtop* at ~24,'0o!" int-address (bit-vector->integer microlisp-shared:*memtop*))
        :memtop-exceeded))
    nil)); means ok.
  
(defun read-address (address cdr-p &optional as-integer-p)
  "return the car (or cdr) at the address"
  (let ((int-address (logand *address-field-mask* ; only use the address, not the type or mark bit
                             (cl:if (bit-vector-p address)
                               (bit-vector->integer address)
                               address))))

    ;; do our own validation here to drop into the debugger
    (assert (< int-address scheme-mach:*maximum-memory-size*) (int-address)
      "Address ~24,'0o is outside attached memory bounds" int-address)
    (when (and *warn-when-beyond-memtop* (> int-address (bit-vector->integer microlisp-shared:*memtop*)))
      (warn "reading memory at ~24,'0o which is beyond *memtop* at ~24,'0o!"
            int-address (bit-vector->integer microlisp-shared:*memtop*)))
    (let ((actual-address (+ (* 2 int-address) (cl:if cdr-p 1 0))))
      (cl:if as-integer-p ;; used for display typically
        (elt *memory-vector* actual-address)
        (integer->bit-vector (elt *memory-vector* actual-address) :result-bits *word-size*))))) ; add pad

(defun write-address (address cdr-p new-value)
  "write to (car/cdr) of the address"
  (let ((int-address (logand *address-field-mask* ; only use the address, not the type or mark bit
                             (cl:if (bit-vector-p address)
                               (bit-vector->integer address)
                               address))))

    ;; do our own validation here to drop into the debugger
    (assert (< int-address scheme-mach:*maximum-memory-size*) (int-address)
      "Address ~24,'0o is outside attached memory bounds" int-address)
    (when (and *warn-when-beyond-memtop* (> int-address (bit-vector->integer microlisp-shared:*memtop*)))
      (warn "writing memory at ~24,'0o which is beyond *memtop* at ~24,'0o!"
            int-address (bit-vector->integer microlisp-shared:*memtop*)))
    (let ((new-value-as-integer (cl:if (bit-vector-p new-value)
                                  (bit-vector->integer new-value)
                                  new-value)))
      (assert (< new-value-as-integer scheme-mach:*maximum-memory-content*) (new-value)
        "Value ~o is outside the attached memory content capabilities" new-value)
      (let ((actual-address (+ (* 2 int-address) (cl:if cdr-p 1 0))))
        (setf (elt *memory-vector* actual-address) new-value-as-integer)))))

(defun clear-memory ()
  "used for debugging"
  (dotimes (i scheme-mach:*maximum-memory-size*)
    (setf (elt *memory-vector* i) 0)))

(defun dump-memory-internal (start end print-fn row-leader-print-fn)
  (let ((row-count 0))
    (do ((i (* 2 start) (1+ i))) ; array offset
        ((> i (1+ (* 2 (min scheme-mach:*maximum-memory-size* end)))))
      (when (= row-count 0)
        (funcall row-leader-print-fn i))
      (funcall print-fn i)
      (incf row-count)
      (when (>= row-count *dump-values-per-row*)
        (terpri *error-output*)
        (setq row-count 0)))))

(let ((all-regs nil))
  (defun dump-memory (&optional (start 0) &key (end (bit-vector->integer microlisp-shared:*memtop*)) 
                                (decode-p t) (show-registers nil)
                                show-stack-membership
                                show-display-membership); not implemented yet
    "used for debugging. decode-p prints the mark bit, and type for
each memory address in range; field specific data should use
\(dump-memory-with-types)"
    (let ((reg-address-alist nil)) ; clear each time so we get LAG

      (unless all-regs
        ;; we defer this until **machine-registers** is available
        (setq all-regs (mapcan #'(lambda (x) (if (endp (cdr x)) (list (car x)))) **machine-registers**)))

      (when show-registers
        (mapc #'(lambda (reg-name)
                  (let* ((reg-value (bit-vector->integer (get-address-bits (symbol-value reg-name))))
                         (existing-entry (cdr (assoc reg-value reg-address-alist))))
                    (update-alist reg-value (list* reg-name existing-entry) reg-address-alist)))
              all-regs))

      (when show-stack-membership
        ;; use the reg-address-alist to add indicators that the current address is on the stack
        (let* ((stack-ptr (logand *address-field-mask* (bit-vector->integer *stack*)))
               (existing-entry (cdr (assoc stack-ptr reg-address-alist))))
          (while (plusp stack-ptr)
            (update-alist stack-ptr (list* :s existing-entry) reg-address-alist)
            (setq stack-ptr (logand *address-field-mask* (read-address stack-ptr t t)))
            (setq existing-entry (cdr (assoc stack-ptr reg-address-alist))))))

      (when show-display-membership
        nil) ;; (TBD) .. tricky because it's saved on the stack so we have to know which elements are the display

      (dump-memory-internal
       start end
       (cl:if decode-p
         #'(lambda (i) (mlet (mark ptr type disp frame addr)
                           (scheme-mach:break-out-bits-as-integers (elt *memory-vector* i))
                         (declare (ignore ptr disp frame)) ; run dump-memory-with-types if you want 
                                                           ; these broken out 
                                                           ; (included in fields printed!)
                         (format *error-output* 
                                 (cl:if (evenp i)
                                   *format-car-for-dm-decode-p* ; left value (CAR)
                                   *format-cdr-for-dm-decode-p*) ; right value (CDR)
                                 (cl:if (plusp mark)
                                   "*"
                                   " ")
                                 (int->type-name type)
                                 addr
                                 (if (and (or show-registers show-stack-membership)
                                          (cdr (assoc (floor (/ i 2)) reg-address-alist)))
                                   (format nil "(~{~A ~})" (cdr (assoc (floor (/ i 2)) reg-address-alist)))
                                   ""))))
         #'(lambda (i) (format *error-output* "~11,'0o " (elt *memory-vector* i))))
       #'(lambda (i) (format *error-output* "~8,'0o: " (/ i 2)))))))

(defun dump-memory-with-types (&optional (start 0) &key (end (bit-vector->integer microlisp-shared:*memtop*)))
  "similar to (dump-memory) but prints out each field separately"
  (let ((*dump-values-per-row* 2)) ; override if needed
    (dump-memory-internal
     start end
     #'(lambda (i) (mlet (mark ptr type disp frame addr)
                       (scheme-mach:break-out-bits-as-integers (elt *memory-vector* i))
                     (format *error-output* 
                             (cl:if (evenp i)
                               *format-car-for-dm-with-types* ; left value (CAR)
                               *format-cdr-for-dm-with-types*) ; right value (CDR)
                             mark ptr 
                             (int->type-name type)
                             disp frame addr)))
     #'(lambda (i) (format *error-output* "~8,'0o: " (/ i 2))))))

;; Memory comparison function for testing

(defun compare-memory (goal-array start-address len &optional failure-handling)
  "Given a goal-array of memory (a list of numbers similaar to what the test-n sets up as *cold-boot-memory-array*) a
starting offset into the simulated memory and a length, we compare the two arrays, returning three values, the first
indicating success or failure, the second indicating the address and the third indicating if the CAR or the CDR was
the first part of that address to fail the comparison (we check the CAR before the CDR). Failure-handling does nothing
if NIL (the default). If :warn a warning is issued, and otherwise an error is signalled."
  (let ((remaining-words len)
        (current-address start-address)
        (cdr-p nil)
        (goal (copy-list goal-array)))
    (while (plusp remaining-words)
       (let ((mem (read-address current-address cdr-p t))
             (goal-mem (if (bit-vector-p (car goal))
                         (bit-vector->integer (car goal))
                         (car goal)))) ; comparison as integers is simpler
         (cond
           ((eql goal-mem mem)
            (setq cdr-p (not cdr-p))
            (pop goal)
            (when (not cdr-p)
              (incf current-address)
              (decf remaining-words)))
           ((eql failure-handling :warn)
            (warn "Compare-memory failure: expected ~o at ~a of address ~o but saw ~o"
                  goal-mem
                  (cl:if cdr-p "CDR" "CAR")
                  current-address
                  mem)
            (return-from compare-memory (values nil current-address (cl:if cdr-p :cdr :car))))
           (failure-handling
            (error "Compare-memory failure: expected ~o at ~a of address ~o but saw ~o"
                   goal-mem
                   (cl:if cdr-p "CDR" "CAR")
                   current-address
                   mem)
            (return-from compare-memory (values nil current-address (cl:if cdr-p :cdr :car))))
           (t
            (return-from compare-memory (values nil current-address (cl:if cdr-p :cdr :car)))))))
    (values t nil nil)))
