(require 'ert)
(require 'stack-list)

(ert-deftest stack-list-test ()
  ""
  (let ((stack-object (stack-list-create)))
    (should (stack-empty-p stack-object))
    (should (= 0 (stack-length stack-object)))
    (should-error (stack-top stack-object))
    (should-error (stack-pop stack-object))
    ;; 
    (stack-push stack-object 1)
    (should (= 1 (stack-length stack-object)))
    (should-not (stack-empty-p stack-object))
    (should (= 1 (stack-top stack-object)))
    (should (= 1 (stack-pop stack-object)))
    (should (stack-empty-p stack-object))
    (should (= 0 (stack-length stack-object)))
    (should-error (stack-top stack-object))
    (should-error (stack-pop stack-object))
    ;; 
    (stack-push stack-object 1)
    (stack-push stack-object 2)
    (should (= 2 (stack-pop stack-object)))
    ;; 
    (stack-clear stack-object)
    (should (stack-empty-p stack-object))))
