(require 'eieio)

(defclass stack ()
  ((content :accessor stack-content :protection :protected :documentation "The content of stack"))
  :abstract t)

(defgeneric stack-clear (stack-object)
  "remove all elements from the stack")

(defgeneric stack-empty-p (stack-object)
  "wether the stack is empty")

(defgeneric stack-top (stack-object)
  "get the top element of stack")

(defgeneric stack-push (stack-object element)
  "push an element to the stack")

(defgeneric stack-pop (stack-object)
  "remove and return top element from stack")

(defgeneric stack-length (stack-object)
  "length of stack")

(provide 'stack)
