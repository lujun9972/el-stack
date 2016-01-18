(require 'cl-lib)
(require 'eieio)
(require 'stack)

(defclass stack-vector ()
  ((content :initform (vector) :initarg :content :protection :protected)
   (top :initform -1)))

(defun stack-vector-create (capacity)
  "Create a stack"
  (make-instance 'stack-vector :content (make-vector capacity nil)))

(defun stack-length ((stack-object stack-vector))
  (+ 1 (oref stack-object top)))

(defun stack-clear ((stack-object stack-vector))
  (dotimes (n (stack-length stack-object))
    (setf (aref stack-object n) nil)))

(defun stack-empty-p ((stack-object stack-vector))
  (= 0 (stack-length stack-object)))

(defun stack-top ((stack-object stack-vector))
  (when (stack-empty-p stack-object)
    (error "stack is empty"))
  (aref (stack-content stack-object)
        (oref stack-object top)))

(defun stack-pop ((stack-object stack-vector))
  (when (stack-empty-p stack-object)
    (error "stack is empty"))
  (let ((element (stack-top stack-object))
        (top (oref stack-object top)))
    (setf (aref (stack-content stack-object) top)
          nil)
    (cl-incf (oref stack-object top) -1)
    element))

(defun stack-full-p ((stack-object stack-vector))
  "Wether the stack is full"
  (= (oref stack-object top) (- (length (stack-content stack-object)) -1)))

(defun stack-push ((stack-object stack-vector) element)
  (when (stack-full-p stack-object)
    (error "stack is full"))
  (cl-incf (oref stack-object top))
  (setf (aref (stack-content stack-object)
              (oref stack-object top))
        element))
