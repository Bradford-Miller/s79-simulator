(in-package :scheme-mach)

(scheme-79:scheme-79-version-reporter "Scheme Machine Clock Trig" 0 3 1
                                      "Time-stamp: <2022-01-13 13:56:10 gorbag>"
                                      "internal-freeze -> run-nano")

;; 0.3.1   1/13/22 change references from internal-freeze to run-nano
;;                    for consistancy with AIM

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 0.3.0   1/11/22 snapping a line: 0.3 release of scheme-79 supports  test-0 and test-1. ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 0.1.0   8/20/21 "official" 0.2 release: test-0 passed!

;; 0.0.4 07-31-21 should be defparameter instead of defvar (so the
;;                  values get updated when we reload this file)

;;                flip
;;                  when register controls and external sync to pads
;;                  happens (register controls first, else address not
;;                  updated before we output it). That means we need
;;                  to run external-data-transfer both before and
;;                  after register controls so inputs synced, register
;;                  controls happen, outputs synced.


;; 0.0.3 07-30-21 run-external-data-transfer on :ph1-rising so before run-register-controls

;; 0.0.2 07-27-21 run-register-controls on :ph1-high (so just before
;;                  we clear the controls)
;;                cosmetics

;; 0.0.1 07-20-21 run p2 of nanocontroller on :ph1-falling

;; 0.0.0 07-19-21 Create variables for clock-related (timing) constants
;;                  here so we have one place to change things


;; signal/pad declarations are still in sim-machine-external and
;; sim-machine-internal (defchip-pad(s) )...

;; microcontroller
(defparameter *run-microcontroller* :ph1-falling)

;; external pad read/write (run-cache-sync)
(defparameter *run-external-data-transfer* :ph1-high)

;; nanocontroller

;; should be on :ph2-rising not :ph1-rising 7/15/21
(defparameter *run-nanocontroller-p1* :ph2-rising
  "P1 of nanocontroller decodes the microcode state, sets the nano-pc
  and executes the first nano line")

;; this is :ph1-high because we want external-data transfer to happen
;; the various controls get set up first.
(defparameter *run-register-controls* :ph1-rising
  "This is when register *from-controls* and *to-controls* are run -
  e.g. register copies via the bus happens.")

;; update sense lines
(defparameter *update-sense-lines* :ph1-falling
  "Note that as ph2 is when the chip transitions to the new state, it
  makes sense that we set the sense lines after any data movement
  between registers (see *run-register-controls*).  These should not
  be affected by any changes to the control lines (only
  run-register-controls) so should be OK to be same as
  run-nanocontroller-p2.")

;; 7/20/21 BWM try ph1-falling so register control lines are valid
;;             during READ and WRITE this shouldn't screw up
;;             nanoncontroller-ph1 as it runs on ph2-rising, but may
;;             need run-nano true to keep the microcode from
;;             changing out from under us during ph1-rising (?) though
;;             nanocode should already have decoded it and set the
;;             right control lines so maybe not...
(defparameter *run-nanocontroller-p2* #|| :ph2-falling ||# :ph1-falling 
  "P2 of nanocontroller reruns register controls (in case they were
  changed from external pad sync) then cleans up the register lines
  set by run-nanocontroller-p1 and if there is a subsequent
  nanoinstruction, gets that set up for the next nanocontroller P1.")

;; note that we also hardcode clock phases into the pad monitoring
;; code in external-support.lisp (not sure if we should generify and
;; move it here... mainly to support future chip emulation...

;; when external pads should be sampled (ideally). See monitor-pads in external-support.lisp

(defparameter *test-ale-to-expect-address* :ph2-falling
  "This is when we sample ALE and if set, we expect an address in the phase
  *get-address-from-pads*")

(defparameter *get-address-from-pads* :ph1-high
  "If we had a positive ALE during *test-ale-to-expect-address*, this is when 
  we should read the address from the pads")

(defparameter *put-memory-content-onto-pads* :ph1-rising
  "If we see a read signal, and no freeze is asserted, we put the
  content of memory from the prior address onto the pads")

(defparameter *get-memory-content-from-pads* :ph1-high
  "If we see a write signal, and no freeze is asserted, we put the
  content of the pads into the memory")

(defparameter *test-for-read-interrupt* :ph1-rising
  "If read-interrupt is high during this phase, we put the interrupt
  vector onto the pads")
