# cl-statemachine-to-swift

_Mihai Cristian Tănase <mihaicristian.tanase@gmail.com>_

This is a project to generate a StateMachine swift class from a state machine described in Common Lisp.

## License

[MIT License](LICENSE.md)

## Usage

From the  following BMPN diagram:
![img](demo_ex.png)

*manually* writing the the following code

```lisp
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
             (c-decision
              (flag-c1 . e)
              (flag-c2 . f)
              g)
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
```

* generates the main state machine file at `/tmp/StateMachine.swift`:

```swift
//
// This file is generated with cl-statemachine-to-swift
// Changes are not recommended.
//

import Foundation

class StateMachine {
  typealias Completion = (Bool, Error?) -> Void
  typealias ActionExecutor = (@escaping Completion) -> Void
  typealias Transition = (State, Action, State)
  typealias Decision = () -> Bool?
  
  enum Err: Error {
    case impossibleAction(State, Action)
    case transitionNotSet(State, Action)
    case invalidTransition(State, Action)
    case invalidDecision
    case actionError(String)
  }
  
  /**
   * The states of the state machine. A state fully defines properties necessary to decide user actions.
   */
  enum State {
    case aDecision
    case a
    case b
    case cDecision
    case d
    case e
    case f
    case g
  }
  
  /**
   * The actions of the state machine. An action connects two states.
   */
  enum Action {
    case executeSomething
    case goToA
    case goToB
    case goToF
    case goToG
  }
  
  /**
   * Flag to indicate whether or not this class prints debugging messages.
   */
  var isLogEnabled = false
  
  /**
   * Current state.
   */
  private(set) var state: State!
  
  /**
   * Last action.
   */
  private(set) var lastAction: Action!
  
  /**
   * Last action error.
   */
  private(set) var lastActionError: Error?
  
  /**
   * Actions
   */
  private var actionExecuteSomething: ActionExecutor!
  private var actionGoToA: ActionExecutor!
  private var actionGoToB: ActionExecutor!
  private var actionGoToF: ActionExecutor!
  private var actionGoToG: ActionExecutor!
  
  /**
   * Decisions
   */
  private var isFlagA: Decision!
  private var isFlagB: Decision!
  private var isFlagC: Decision!
  private var isFlagC1: Decision!
  private var isFlagC2: Decision!
  
  /**
   * Transitions
   */
  private var transitions: [Transition] = [
    (.d, .goToG, .g),
    (.g, .goToA, .a),
    (.g, .executeSomething, .g),
    (.a, .goToB, .b),
    (.e, .goToF, .f),
  ]
  
  static func create() -> StateMachine {
    return StateMachine(.aDecision)
  }
  
  /**
   * Description of the error from last action.
   */
  func errorDescription() -> String? {
    if let error = lastActionError {
      if let err = error as? Err {
        return "\(err)"
          .replacingOccurrences(of: "(", with: ":")
          .replacingOccurrences(of: ")", with: "")
          .replacingOccurrences(of: "\"", with: " ")
          .trimmingCharacters(in: .whitespacesAndNewlines)
      }
      return error.localizedDescription
    }
    return nil
  }
  
  /**
   * Set decision for isFlagA
   */
  func setDecisionFlagA(_ decision: @escaping Decision) {
    isFlagA = decision
  }
  
  /**
   * Set decision for isFlagB
   */
  func setDecisionFlagB(_ decision: @escaping Decision) {
    isFlagB = decision
  }
  
  /**
   * Set decision for isFlagC
   */
  func setDecisionFlagC(_ decision: @escaping Decision) {
    isFlagC = decision
  }
  
  /**
   * Set decision for isFlagC1
   */
  func setDecisionFlagC1(_ decision: @escaping Decision) {
    isFlagC1 = decision
  }
  
  /**
   * Set decision for isFlagC2
   */
  func setDecisionFlagC2(_ decision: @escaping Decision) {
    isFlagC2 = decision
  }
  
  /**
   * Set action .executeSomething
   */
  func setActionExecuteSomething(_ action: @escaping ActionExecutor) {
    actionExecuteSomething = action
  }
  
  /**
   * Execute action .executeSomething from current state
   */
  func doActionExecuteSomething(_ completion: @escaping Completion) {
    log("doActionExecuteSomething")
    doAction(.executeSomething, completion)
  }
  
  /**
   * Set action .goToA
   */
  func setActionGoToA(_ action: @escaping ActionExecutor) {
    actionGoToA = action
  }
  
  /**
   * Execute action .goToA from current state
   */
  func doActionGoToA(_ completion: @escaping Completion) {
    log("doActionGoToA")
    doAction(.goToA, completion)
  }
  
  /**
   * Set action .goToB
   */
  func setActionGoToB(_ action: @escaping ActionExecutor) {
    actionGoToB = action
  }
  
  /**
   * Execute action .goToB from current state
   */
  func doActionGoToB(_ completion: @escaping Completion) {
    log("doActionGoToB")
    doAction(.goToB, completion)
  }
  
  /**
   * Set action .goToF
   */
  func setActionGoToF(_ action: @escaping ActionExecutor) {
    actionGoToF = action
  }
  
  /**
   * Execute action .goToF from current state
   */
  func doActionGoToF(_ completion: @escaping Completion) {
    log("doActionGoToF")
    doAction(.goToF, completion)
  }
  
  /**
   * Set action .goToG
   */
  func setActionGoToG(_ action: @escaping ActionExecutor) {
    actionGoToG = action
  }
  
  /**
   * Execute action .goToG from current state
   */
  func doActionGoToG(_ completion: @escaping Completion) {
    log("doActionGoToG")
    doAction(.goToG, completion)
  }
  
  /**
   * Start method. Must be called, otherwise, the state machine is not running.
   */
  func start() {
    // check decisions
    if isFlagA == nil {
      fatalError("Machine not started because decision 'flagA' is missing")
    }
    if isFlagB == nil {
      fatalError("Machine not started because decision 'flagB' is missing")
    }
    if isFlagC == nil {
      fatalError("Machine not started because decision 'flagC' is missing")
    }
    if isFlagC1 == nil {
      fatalError("Machine not started because decision 'flagC1' is missing")
    }
    if isFlagC2 == nil {
      fatalError("Machine not started because decision 'flagC2' is missing")
    }
    
    // check actions
    if actionExecuteSomething == nil {
      fatalError("Machine not started because action 'executeSomething' is missing")
    }
    if actionGoToA == nil {
      fatalError("Machine not started because action 'goToA' is missing")
    }
    if actionGoToB == nil {
      fatalError("Machine not started because action 'goToB' is missing")
    }
    if actionGoToF == nil {
      fatalError("Machine not started because action 'goToF' is missing")
    }
    if actionGoToG == nil {
      fatalError("Machine not started because action 'goToG' is missing")
    }
    
    // start the machine
    do {
      try moveToState(state)
    }
    catch {
      fatalError("\(error)")
    }
  }
  
  private init(_ state: State) {
    self.state = state
  }
  
  private func doAction(_ action: Action, _ completion: @escaping Completion) {
    lastAction = action
    
    var actionExec: ActionExecutor!
    switch action {
      case .executeSomething:
      actionExec = actionExecuteSomething
      case .goToA:
      actionExec = actionGoToA
      case .goToB:
      actionExec = actionGoToB
      case .goToF:
      actionExec = actionGoToF
      case .goToG:
      actionExec = actionGoToG
    }
    
    do {
      let transition = try findTransition(action)
      if actionExec == nil {
        throw Err.transitionNotSet(state, action)
      }
      actionExec {
        [weak self] success, error in
        if error != nil {
          self?.lastActionError = error
          completion(false, error)
          return
        }
        do {
          try self?.moveToState(transition.2)
          self?.lastActionError = error
          completion(success, error)
        }
        catch {
          self?.lastActionError = error
          completion(false, error)
        }
      }
    }
    catch {
      lastActionError = error
      completion(false, error)
    }
  }
  
  private func findTransition(_ action: Action) throws -> Transition {
    for t in transitions {
      if t.0 == state, t.1 == action {
        return t
      }
    }
    throw Err.impossibleAction(state, action)
  }
  
  private func moveToState(_ state: State) throws {
    self.state = state
    log("moveToState \(state)")
    
    switch state {
      case .aDecision:
      if isFlagA() ?? false {
        try moveToState(.a)
      }
      else if isFlagB() ?? false {
        try moveToState(.b)
      }
      else if isFlagC() ?? false {
        try moveToState(.cDecision)
      }
      else {
        try moveToState(.d)
      }
      break
      case .a:
      break
      case .b:
      break
      case .cDecision:
      if isFlagC1() ?? false {
        try moveToState(.e)
      }
      else if isFlagC2() ?? false {
        try moveToState(.f)
      }
      else {
        try moveToState(.g)
      }
      break
      case .d:
      break
      case .e:
      break
      case .f:
      break
      case .g:
      break
    }
  }
  
  private func log(_ msg: String) {
    if isLogEnabled {
      print("StateMachine: \(msg)")
    }
  }
}
```

* and the usage file at `/tmp/StateMachineUsage.swift`:

```swift
@main
class StateMachineTest {
  func test() {
    let sm = StateMachine.create()
    sm.setDecisionFlagA { [weak self] in /*TODO*/ self?.tautology() }
    sm.setDecisionFlagB { [weak self] in /*TODO*/ self?.tautology() }
    sm.setDecisionFlagC { [weak self] in /*TODO*/ self?.tautology() }
    sm.setDecisionFlagC1 { [weak self] in /*TODO*/ self?.tautology() }
    sm.setDecisionFlagC2 { [weak self] in /*TODO*/ self?.tautology() }
    sm.setActionExecuteSomething { [weak self] in self?.executeSomethingDemoEx($0) }
    sm.setActionGoToA { [weak self] in self?.goToADemoEx($0) }
    sm.setActionGoToB { [weak self] in self?.goToBDemoEx($0) }
    sm.setActionGoToF { [weak self] in self?.goToFDemoEx($0) }
    sm.setActionGoToG { [weak self] in self?.goToGDemoEx($0) }
    sm.start()
  }
  private func executeSomethingDemoEx(_ completion: @escaping StateMachine.Completion) {
    // TODO: add logic for executeSomethingDemoEx
  }
  private func goToADemoEx(_ completion: @escaping StateMachine.Completion) {
    // TODO: add logic for goToADemoEx
  }
  private func goToBDemoEx(_ completion: @escaping StateMachine.Completion) {
    // TODO: add logic for goToBDemoEx
  }
  private func goToFDemoEx(_ completion: @escaping StateMachine.Completion) {
    // TODO: add logic for goToFDemoEx
  }
  private func goToGDemoEx(_ completion: @escaping StateMachine.Completion) {
    // TODO: add logic for goToGDemoEx
  }
  private func tautology() -> Bool {
    return true
  }
  static func main() {
    StateMachineTest().test()
  }
}
```

## TODO

1. Generate the Machine object automatically by parsing the BMPN diagram;

1. Beautify code (ex: join "else", "catch" lines);

1. Remove trailing white spaces;

1. Cleanup Common Lisp code;
