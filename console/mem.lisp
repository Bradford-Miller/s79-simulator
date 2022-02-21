(in-package :s79-console)

(scheme-79:scheme-79-version-reporter "Scheme Machine Memory Window" 0 3 0
                                      "Time-stamp: <2022-02-18 18:52:08 gorbag>"
                                      "new")

;; 0.3.0   2/18/22 New window for showing the contents of memory 
;;                    (essentially a floating, updating, dump-memory)

(defparameter *memory-window-initial-width* 800)

(defparameter *memory-window-initial-height* 1000)

(defvar *mem* nil)

(defun reset-mem ()
  (when (mem-running-p)
    (redraw-mem *mem*)))

(defun mem-running-p ()
  *mem*) ; probably should check that it's exposed...

(defun start-mem ()
  (let ((interface (make-instance 'mem-interface
                                  :best-width *memory-window-initial-width*
                                  :best-height *memory-window-initial-height*)))
    (setq *mem* interface)
    (capi:display interface)))

(defun redraw-mem (interface)
  (gp:invalidate-rectangle (slot-value interface 'content-viewer)))

(defun update-contents (interface new-text)
  (setf (memory-contents-text interface) new-text)
  (setf (display-pane-text (slot-value interface 'content-viewer)) new-text)
  (redraw-mem interface))

(defun mem-update (interface)
  ;; get a memory dump. We should be able to select parameters (TBD),
  ;; but for now just do a generic dump.  note our intent is to
  ;; annotate the memory dump, e.g., with where the registers are
  ;; pointing. We can either do this through a separte column, or
  ;; integrating something into the memory-dump generator
  ;; (memory-dump-internal). (TBD)

  ;; for now, just do a straight dump so we have something we can look
  ;; at.
  (let ((*error-output* (make-string-output-stream))) ; memory dumps are to *error-output*
    (dump-memory)
    (update-contents interface (get-output-stream-string *error-output*))))

(capi:define-interface mem-interface ()
  ((memory-contents-text :initform ""
                         :accessor memory-contents-text))
  (:panes
   (title
    capi:title-pane
    :text "Memory Viewer")
   (content-viewer
    capi:display-pane
    :text memory-contents-text))

  (:layouts
   (default
    capi:column-layout
    '(title content-viewer))))

(defmethod capi:interface-display :after ((interface mem-interface))
  (mem-update interface))
