(in-package :scheme-mach)

(scheme-79:scheme-79-version-reporter "Scheme Machine Sim Base" 0 3 0
                                      "Time-stamp: <2022-01-11 15:15:20 gorbag>"
                                      "0.3 release!")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 0.3.0   1/11/22 snapping a line: 0.3 release of scheme-79 supports  test-0 and test-1. ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; 0.1.4   1/ 4/22 fix repatriated function refs

;; 0.1.3  10/15/21 use *scheme-mach*

;; 0.1.2   8/27/21 get-bus-field

;; 0.1.1   8/23/21 cond vs cl:cond

;; 0.1.0   8/20/21 "official" 0.2 release: test-0 passed!

;; 0.0.7      6-26-21 Consolodate clock fns into their own file

;; 0.0.6      6-17-21 *total-clock-phases*

;; 0.0.5      6- 5-21 run *all-ph-pre-update-list*

;; 0.0.4      2- 3-21 increment *tick* on each major clock cycle
;; 0.0.3      1-24-21 add *ph1-high-list* and *ph2-high-list* for initializations
;; 0.0.2      1- 6-21 clock related functions
;; 0.0.1     12-23-20 bus load functions

;; Closer to a hardware simulator corresponding with AIM 559
;; based on the published microcode and description

;; here we implement the various described mechanisms on the chip
;; which we are simulating.

(defun load-bus (from-register)
  (copy-field from-register *bus*))

(defun get-bus-field (field)
  "Returns the value of just the field from the *bus*"
  (let ((bus-field-symbol (make-register-field-symbol '*BUS* field)))
    (symbol-value bus-field-symbol)))

(defun load-bus-field (field from-register-name-or-constant)
  ;; validate register if needed
  (let (from-field-symbol to-field-symbol)
    (unless (numberp from-register-name-or-constant)
      (validate-register-control 'from from-register-name-or-constant) ; not quite right - fix later
      (setq from-field-symbol (make-register-field-symbol from-register-name-or-constant field)))
    (setq to-field-symbol (make-register-field-symbol '*BUS* field))

    (cl:cond
      (from-field-symbol
       (copy-field (symbol-value from-field-symbol) (symbol-value to-field-symbol)))
      (t
       ;; local to the instruction, so presumably already on the bus,
       ;; but for now we just slam it there.  Note that constants
       ;; should be size of the field we are writing to, may need to
       ;; extend out the bits to make that happen...
       (copy-field (integer->bit-vector from-register-name-or-constant)
                   (symbol-value to-field-symbol))))))

(defun load-register-from-bus (register-name)
  ;; check that register can be loaded
  (validate-register-control 'to register-name) ; not quite right?
  (copy-field *bus* (symbol-value register-name)))

(defun load-register-field-from-bus (register-name field-name)
  ;; check that field can be loaded
  (validate-register-control
   (intern (format nil "TO-~A" field-name) *project-machine-pkg*)
   register-name)

  (copy-field (symbol-value (make-register-field-symbol '*bus* field-name))
              (symbol-value (make-register-field-symbol register-name field-name))))


