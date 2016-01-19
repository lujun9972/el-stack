(require 'cl-lib)
(require 'eieio)
(require 'stack)

(defclass stack-share-vector (stack)
  ((content :initform (vector) :initarg :content :protection :protected)
   (top1 :initform -1)
   (top2 :initform -2)))

(defun stack-share-vector-create (capacity)
  "Create a stack"
  (make-instance 'stack-share-vector
                 :content (make-vector capacity nil)
                 :top0 -1
                 :top1 capacity))

(defmethod stack-capacity ((stack-object stack-share-vector))
  (length (stack-content stack-object)))

(defmethod stack-length ((stack-object stack-share-vector))
  (list  (+ 1 (oref stack-object top0))
         (- (stack-capacity stack-object) top1)))

(defmethod stack-clear ((stack-object stack-share-vector))
  (setf (stack-content stack-object) (make-vector (stack-capacity stack-object) nil))
  (setf (oref stack-object top0) -1)
  (setf (oref stack-object top1) (stack-capacity stack-object))
  stack-object)

(defun stack-share-vector--check-stack-number (stack-number)
  ""
  (unless (member stack-number '(0 1))
    (error "no stack %d" stack-number)))

(defmethod stack-empty-p ((stack-object stack-share-vector) stack-number)
  (stack-share-vector--check-stack-number stack-number)
  (= 0 (nth stack-number (stack-length stack-object))))

(defmethod stack-top ((stack-object stack-share-vector) stack-number)
  (stack-share-vector--check-stack-number stack-number)
  (when (stack-empty-p stack-object stack-number)
    (error "stack %d is empty" stack-number))
  (cond ((= stack-number 0)
         (aref (stack-content stack-object)
               (oref stack-object top0)))
        ((= stack-number 1)
         (aref (stack-content stack-object)
               (oref stack-object top1)))))

(defmethod stack-pop ((stack-object stack-share-vector) stack-number)
  (stack-share-vector--check-stack-number stack-number)
  (when (stack-empty-p stack-object stack-number)
    (error "stack %d is empty" stack-number))
  (let ((element (stack-top stack-object stack-number)))
    (cond ((= 0 stack-number)
           (let ((top (oref stack-object top0)))
             (setf (aref (stack-content stack-object) top)
                   nil)
             (cl-incf (oref stack-object top) -1)))
          ((= 1 stack-number)
           (let ((top (oref stack-object top1)))
             (setf (aref (stack-content stack-object) top)
                   nil)
             (cl-incf (oref stack-object top) 1))))
    element))

(defmethod stack-full-p ((stack-object stack-share-vector))
  "Wether the stack is full"
  (let ((top0 (oref stack-object top0))
        (top1 (oref stack-object top1)))
    (= top1 (+ top0 1))))

(defmethod stack-push ((stack-object stack-share-vector) element stack-number)
  (when (stack-full-p stack-object)
    (error "stack is full"))
  (cond ((= 0 stack-number)
         (cl-incf (oref stack-object top0))
         (setf (aref (stack-content stack-object)
                     (oref stack-object top0))
               element))
        ((= 1 stack-number)
         (cl-incf (oref stack-object top1) -1)
         (setf (aref (stack-content stack-object)
                     (oref stack-object top1))
               element)))
  )

(provide 'stack-share-vector)
