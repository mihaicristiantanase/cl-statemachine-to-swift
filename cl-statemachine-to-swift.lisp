;;;; cl-statemachine-to-swift.lisp

;;; TODO: beautify code (ex: join "else", "catch" lines)
;;; TODO: remove trailing white spaces
;;; TODO: cleanup common lisp code
;;; TODO(mihai): fill in skeleton functions for actions in usage output files
;; TODO(mihai): fill in flags

(in-package #:cl-statemachine-to-swift)

(defclass Machine ()
  ((context
    :initarg :context)
   (states
    :initarg :states
    :initform nil)
   (transitions
    :initarg :transitions
    :initform nil)
   (actions
    :initform nil)))

(defparameter *machine* nil)

(defparameter *errors*
  '((impossible-action . (state action))
    (transition-not-set . (state action))
    (invalid-transition . (state action))
    invalid-decision
    (action-error . (string))))

(defparameter *indent* 0)
(defparameter *stream* nil)

(defun stable-state (state machine)
  (not (null (find state (slot-value machine 'states)))))

(defgeneric get-start (machine))
(defgeneric get-states (machine))
(defgeneric get-unstable-state-decisions (state machine))

(defmethod initialize-instance :after ((machine Machine) &key)
  (dolist (transition (slot-value machine 'transitions))
    (unless (caddr transition)
      (setf (caddr transition) (car transition))))
  ;; auto-fill actions
  (setf (slot-value machine 'actions)
        (unique-list
         (mapcar #'second (slot-value machine 'transitions)))))

(defmethod get-start ((machine Machine))
  (car (get-states machine)))

(defmethod get-states ((machine Machine))
  (mapcar #'(lambda (s) (if (symbolp s) s (car s)))
          (slot-value machine 'states)))

(defmethod get-unstable-state-decisions (state (machine Machine))
  (dolist (s (slot-value machine 'states))
    (when (and (listp s) (eq (car s) state))
      (return-from get-unstable-state-decisions (cdr s)))))

(defun wl (&optional (fmt "") &rest body)
  "Write line"
  (format *stream* (make-string *indent* :initial-element #\Space))
  (apply #'format *stream* fmt body)
  (format *stream* "~%"))

(defun wlb (&optional (fmt "") &rest body)
  "Write Line Block"
  (wl fmt body)
  (wl))

(defun sym->camelcase (sym )
  (cl-change-case:camel-case (symbol-name sym)))

(defun sym->pascalcase (sym )
  (cl-change-case:pascal-case (symbol-name sym)))

(defun sym->decision (sym)
  (format nil "is~a" (sym->pascalcase sym)))

(defun define-swift-enum (name super cases)
  (wl "enum ~a~:[~;: ~a~] {" name super super)
  (dolist (c cases)
    (let ((params (when (listp c)
                    (format nil "~{~a~^, ~}"
                            (mapcar (lambda (s) (cl-change-case:pascal-case (symbol-name s))) (cdr c))))))
      (wl "  case ~a~:[~;(~a)~]" (sym->camelcase (if (listp c) (car c) c)) params params)))
  (wl "}")
  (wl))

(defun define-swift-doc (doc)
  (wl "/**")
  (wl " * ~a" doc)
  (wl " */"))

(defmacro define-swift-block (firstline &rest body)
  `(progn
     (wl (concatenate 'string ,firstline " {"))
     (let ((*indent* (+ *indent* 2)))
       ,@body
       )
     (wl "}")))

(defmacro define-swift-cont (firstline &rest body)
  `(progn
     (wl ,firstline)
     (let ((*indent* (+ *indent* 2)))
       ,@body
       )))

(defmacro define-swift-pfun (name params &rest body)
  `(define-swift-block (format nil "private func ~a(~a)" ,name ,params) ,@body))

(defmacro define-swift-fun (name params &rest body)
  `(define-swift-block (format nil "func ~a(~a)" ,name ,params) ,@body))

(defmacro define-swift-class (name &rest body)
  `(define-swift-block (format nil "class ~a" ,name) ,@body))

(defmacro loop-decisions ((varname) &rest body)
  (let ((v (gensym)))
    `(let (,v)
       (dolist (state (get-states *machine*))
         (unless (stable-state state *machine*)
           (loop for ,varname in (get-unstable-state-decisions state *machine*)
                 when (listp ,varname) do
                   (let ((,varname (car ,varname)))
                     (unless (find ,varname ,v)
                       (push ,varname ,v)
                       ,@body))))))))

(defun gen-code-stream ()
  (wl "//")
  (wl "// This file is generated with cl-statemachine-to-swift")
  (wl "// Changes are not recommended.")
  (wl "//")
  (wl)
  (wl "import Foundation")
  (wl)
  (define-swift-class "StateMachine"
      (wl "typealias Completion = (Bool, Error?) -> Void")
    (wl "typealias ActionExecutor = (@escaping Completion) -> Void")
    (wl "typealias Transition = (State, Action, State)")
    (wlb "typealias Decision = () -> Bool?")
    (define-swift-enum "Err" "Error" *errors*)
    (define-swift-doc "The states of the state machine. A state fully defines properties necessary to decide user actions.")
    (define-swift-enum "State" nil (get-states *machine*))
    (define-swift-doc "The actions of the state machine. An action connects two states.")
    (define-swift-enum "Action" nil (slot-value *machine* 'actions))
    (define-swift-doc "Flag to indicate whether or not this class prints debugging messages.")
    (wlb "var isLogEnabled = false")
    (define-swift-doc "Current state.")
    (wlb "private(set) var state: State!")
    (define-swift-doc "Last action.")
    (wlb "private(set) var lastAction: Action!")
    (define-swift-doc "Last action error.")
    (wlb "private(set) var lastActionError: Error?")
    (define-swift-doc "Actions")
    (dolist (action (slot-value *machine* 'actions))
      (wl "private var action~a: ActionExecutor!" (sym->pascalcase action)))
    (wl)
    (define-swift-doc "Decisions")
    (loop-decisions (decision)
                    (wl "private var ~a: Decision!" (sym->decision decision)))
    (wl)
    (define-swift-doc "Transitions")
    (wl "private var transitions: [Transition] = [")
    (dolist (trans (slot-value *machine* 'transitions))
      (wl "  (~{~a~^, ~}),"
          (mapcar #'(lambda (s) (concatenate 'string "." (sym->camelcase s))) trans)))
    (wlb "]")
    (wl "static func create() -> StateMachine {")
    (wl (format nil "  return StateMachine(.~a)" (sym->camelcase (get-start *machine*))))
    (wlb "}")
    (define-swift-doc "Description of the error from last action.")
    (define-swift-block "func errorDescription() -> String?"
        (define-swift-block "if let error = lastActionError"
            (define-swift-block "if let err = error as? Err"
                (define-swift-cont "return \"\\(err)\""
                    (wl ".replacingOccurrences(of: \"(\", with: \":\")")
                  (wl ".replacingOccurrences(of: \")\", with: \"\")")
                  (wl ".replacingOccurrences(of: \"\\\"\", with: \" \")")
                  (wl ".trimmingCharacters(in: .whitespacesAndNewlines)")))
          (wl "return error.localizedDescription"))
      (wl "return nil"))
    (loop-decisions (decision)
                    (wl)
                    (define-swift-doc (format nil "Set decision for ~a" (sym->decision decision)))
                    (define-swift-fun (format nil "setDecision~a" (sym->pascalcase decision))
                      "_ decision: @escaping Decision"
                      (wl (format nil "~a = decision" (sym->decision decision)))))
    (dolist (action (slot-value *machine* 'actions))
      (let ((ap (sym->pascalcase action))
            (ac (sym->camelcase action)))
        (wl)
        (define-swift-doc (format nil "Set action .~a" ac))
        (define-swift-fun (format nil "setAction~a" ap) "_ action: @escaping ActionExecutor"
          (wl (format nil "action~a = action" ap)))
        (wl)
        (define-swift-doc (format nil "Execute action .~a from current state" ac))
        (define-swift-fun (format nil "doAction~a" ap) "_ completion: @escaping Completion"
          (wl (format nil "log(\"doAction~a\")" ap))
          (wl (format nil "doAction(.~a, completion)" ac)))))
    (wl)
    (define-swift-doc "Start method. Must be called, otherwise, the state machine is not running.")
    (define-swift-fun "start" ""
      (wl "// check decisions")
      (loop-decisions (decision)
                      (define-swift-block (format nil "if ~a == nil" (sym->decision decision))
                          (wl (format nil "fatalError(\"Machine not started because decision '~a' is missing\")"
                                      (sym->camelcase decision)))))
      (wl)
      (wl "// check actions")
      (dolist (action (slot-value *machine* 'actions))
        (define-swift-block (format nil "if action~a == nil" (sym->pascalcase action))
            (wl (format nil "fatalError(\"Machine not started because action '~a' is missing\")"
                        (sym->camelcase action)))))
      (wl)
      (wl "// start the machine")
      (define-swift-block "do"
          (wl "try moveToState(state)"))
      (define-swift-block "catch"
          (wl "fatalError(\"\\(error)\")")))
    (wl)
    (wl "private init(_ state: State) {")
    (wl "  self.state = state")
    (wlb "}")
    (define-swift-pfun "doAction" "_ action: Action, _ completion: @escaping Completion"
      (wl "lastAction = action")
      (wl)
      (wl "var actionExec: ActionExecutor!")
      (define-swift-block "switch action"
          (dolist (action (slot-value *machine* 'actions))
            (let ((ac (sym->camelcase action))
                  (ap (sym->pascalcase action)))
              (wl (format nil "case .~a:" ac))
              (wl (format nil "actionExec = action~a" ap)))))
      (wl)
      (wl "do {")
      (wl "  let transition = try findTransition(action)")
      (wl "  if actionExec == nil {")
      (wl "    throw Err.transitionNotSet(state, action)")
      (wl "  }")
      (wl "  actionExec {")
      (wl "    [weak self] success, error in")
      (wl "    if error != nil {")
      (wl "      self?.lastActionError = error")
      (wl "      completion(false, error)")
      (wl "      return")
      (wl "    }")
      (wl "    do {")
      (wl "      try self?.moveToState(transition.2)")
      (wl "      self?.lastActionError = error")
      (wl "      completion(success, error)")
      (wl "    }")
      (wl "    catch {")
      (wl "      self?.lastActionError = error")
      (wl "      completion(false, error)")
      (wl "    }")
      (wl "  }")
      (wl "}")
      (wl "catch {")
      (wl "  lastActionError = error")
      (wl "  completion(false, error)")
      (wl "}"))
    (wl)
    (wl "private func findTransition(_ action: Action) throws -> Transition {")
    (wl "  for t in transitions {")
    (wl "    if t.0 == state, t.1 == action {")
    (wl "      return t")
    (wl "    }")
    (wl "  }")
    (wl "  throw Err.impossibleAction(state, action)")
    (wlb "}")
    (define-swift-block "private func moveToState(_ state: State) throws"
        (wl "self.state = state")
      (wlb "log(\"moveToState \\(state)\")")
      (define-swift-block "switch state"
          (dolist (state (get-states *machine*))
            (wl (format nil "case .~a:" (sym->camelcase state)))
            (unless (stable-state state *machine*)
              (loop for decision in (get-unstable-state-decisions state *machine*)
                    for i from 0 do
                      (if (listp decision)
                          (let ((sd (sym->decision (car decision))))
                            (define-swift-block (format nil "~:[~;else ~]if ~a() ?? false" (> i 0) sd)
                                (wl (format nil "try moveToState(.~a)" (sym->camelcase (cdr decision))))))
                          (define-swift-block "else"
                              (wl (format nil "try moveToState(.~a)" (sym->camelcase decision)))))))
            (wl "break"))))
    (wl)
    (define-swift-pfun "log" "_ msg: String"
      (define-swift-block "if isLogEnabled"
          (wl "print(\"StateMachine: \\(msg)\")")))))

(defun gen-usage-stream ()
  (wl "sm = StateMachine.create()")
  (loop-decisions (decision)
                  (wl (format nil "sm.setDecision~a = { [weak self] in /*TODO*/ }"
                              (sym->pascalcase decision))))
  (dolist (action (slot-value *machine* 'actions))
    (wl (format nil "sm.setAction~a { [weak self] in self?.~a~a($0) }"
                (sym->pascalcase action)
                (sym->camelcase action)
                (sym->pascalcase (slot-value *machine* 'context)))))
  (wl "sm.start()"))

(defun gen-code ()
  (with-output-to-string (*stream*)
    (gen-code-stream)))

(defun gen-usage ()
  (with-output-to-string (*stream*)
    (gen-usage-stream)))

(defun save-and-check-swift (machine path-code path-usage)
  (let* ((*machine* machine)
         (code (gen-code))
         (code-usage (gen-usage)))
    (with-open-file (f path-code :direction :output :if-exists :supersede)
      (write-string code f))
    (format t "~&~a" (shell:run t "swift" path-code))
    (with-open-file (f path-usage :direction :output :if-exists :supersede)
      (write-string code-usage f))
    (format t "~&Done. Please check ~a for the generated Swift file~% ~
                 and ~a for a sample code of how to use."
            path-code path-usage)))
