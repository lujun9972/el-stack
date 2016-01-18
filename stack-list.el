(require 'eieio)
(require 'stack)

(defclass stack-list (stack)
  ((content :initform nil :initarg :content :protection :protected)))

(defmethod stack-clear ((stack-object stack-list))
  (setf (stack-content stack-object) nil)
  stack-object)

(defmethod stack-empty-p ((stack-object stack-list))
  (null (stack-content stack-object)))

(defmethod stack-top ((stack-object stack-list))
  (car (stack-content stack-object)))

(defmethod stack-pop ((stack-object stack-list))
  (pop (stack-content stack-object)))

(defmethod stack-push ((stack-object stack-list) element)
  (push element (stack-content stack-object)))

(defmethod stack-length ((stack-object stack-list))
  (length (stack-content stack-object)))
