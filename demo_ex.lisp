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
   :actions '(go-to-b go-to-a go-to-f go-to-g keep-state)
   :transitions '((d go-to-g g)
                  (g go-to-a a)
                  (g keep-state nil)
                  (a go-to-b b)
                  (e go-to-f f))))
