(require 'ert)
(require 'stack-vector)

(ert-deftest stack-vector-test ()
  ""
  (let ((stack-object (stack-vector-create 1)))
    (should (stack-empty-p stack-object))
    (should (= 0 (stack-length stack-object)))
    (should-error (stack-top stack-object))
    (should-error (stack-pop stack-object))
    ;; 
    (stack-push stack-object 1)
    (should (stack-full-p stack-object))
    (should-error (stack-push stack-object 2))
    (should (= 1 (stack-length stack-object)))
    (should-not (stack-empty-p stack-object))
    (should (= 1 (stack-top stack-object)))
    (should (= 1 (stack-pop stack-object)))
    (should (stack-empty-p stack-object))
    (should (= 0 (stack-length stack-object)))
    (should-error (stack-top stack-object))
    (should-error (stack-pop stack-object))
    ;; 
    (stack-resize stack-object)
    (stack-push stack-object 1)
    (stack-push stack-object 2)
    (should-error (stack-push stack-object 3))
    (should (stack-full-p stack-object))
    (should (= 2 (stack-pop stack-object)))
    ;; 
    (stack-clear stack-object)
    (should (stack-empty-p stack-object))))