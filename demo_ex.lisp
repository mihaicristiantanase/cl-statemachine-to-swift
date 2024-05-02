(ql:quickload "cl-statemachine-to-swift")

(defparameter machine
  (make-instance
   'cl-statemachine-to-swift:Machine
   :context 'demo-ex
   :states '((a-decision
              (flag-a . a)
              (flag-b . b)
              (flag-c . c-decision)
              d)
             a
             b
             (c-decision . ((flag-c1 . e) (flag-c2 . f) g))
             d
             e
             f
             g)
   :transitions '((d go-to-g g)
                  (g go-to-a a)
                  (g execute-something nil)
                  (a go-to-b b)
                  (e go-to-f f))))

(cl-statemachine-to-swift:save-and-check-swift machine
  "/tmp/StateMachine.swift"
  "/tmp/StateMachineUsage.swift")
