(in-package #:cl-statemachine-to-swift)

(defun unique-list (lst)
  "Documentation for unique-list with parameters lst"
  (let ((seen (make-hash-table :test 'equal)))
    (loop for var in lst
          do (unless (gethash var seen) (setf (gethash var seen) t)))
    (loop for key being the hash-keys of seen collect key)))))
