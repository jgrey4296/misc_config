;; Finite State Machine in Lisp
;; All Entry/Exit/Actions expect a 2 arg function of state|edge+data
;; A Strategy is a 2 arg function of edges+data
;;
(defstruct FSM/FSM states)
(defstruct FSM/State id entry actions exit loop start edges)
(defstruct FSM/Edge target interrupts actions)
(defstruct FSM/Context id data)

;; Main Act:
(cl-defun FSM/act (fsm ctx strategy &key exit entry)
  ;; Trigger a change of state in an FSM
  (let* ((data (FSM/Context-data ctx))
         (current (gethash (FSM/Context-id ctx) (FSM/FSM-states fsm)))
         (edges (hash-table-values (FSM/State-edges current)))
         ;; from current state, use strategy on available edges
         (edge (funcall strategy edges data))
         (target (gethash (FSM/Edge-target edge) (FSM/FSM-states fsm)))
         (exit-actions (FSM/State-exit current))
         (edge-actions (FSM/Edge-actions edge))
         (entry-actions (FSM/State-entry target))
         (state-actions (FSM/State-action target))
         )
    ;; move
    (setf (FSM/Context-id ctx) (FSM/State-id target))
    ;; perform exit -> move -> entry -> state actions
    (if exit
        (mapc (lambda (f) (funcall f current data)) exit))
    (mapc (lambda (f) (funcall f current data)) exit-actions)
    (mapc (lambda (f) (funcall f edge data)) edge-actions)
    (if entry
        (mapc (lambda (f) (funcall f target data)) entry))
    (mapc (lambda (f) (funcall f target data)) entry-actions)
    (mapc (lambda (f) (funcall f target data)) state-actions)
    ;; return ctx
    ctx
    )
  )
;; Creation
(defun FSM/make-context (id)
  (make-FSM/Context :id id :data (make-hash-table)))
(defun FSM/make-FSM ()
  ;; Create a Basic uninitialized FSM
  (make-FSM/FSM :states (make-hash-table))
  )
(cl-defun FSM/make-state (fsm id &key loop entry actions exit)
  ;; Create a basic state
  (let ((state (make-FSM/State :id id
                               :entry entry
                               :actions actions
                               :exit exit
                               :loop loop
                               :edges (make-hash-table)
                               )))
    (puthash id state (FSM/FSM-states fsm))
    state
    )
  )
(defun FSM/make-edge (fsm state1 state2 &optional actions)
  ;; Create a basic edge between two states
  (let* ((source (gethash state1 (FSM/FSM-states fsm)))
         (target-id state2)
         (edge (make-FSM/Edge :target target-id
                              :interrupts (make-hash-table)
                              :actions actions)))
    (puthash target-id edge (FSM/State-edges source))
    edge
    )
  )
;; Updating
(defun FSM/add-actions (fsm id type &rest actions)
  ;; Add an action to :entry :action or :exit hooks of a state
  (let ((state (gethash id (FSM/FSM-states fsm))))
    (cond ((eq type :entry) (setf (FSM/State-entry state) (concatenate 'list actions (FSM/State-entry state))))
          ((eq type :action) (setf (FSM/State-action state) (concatenate 'list actions (FSM/State-action state))))
          ((eq type :exit) (setf (FSM/State-exit state) (concatenate 'list actions (FSM/State-exit state))))
          (t (message "Unrecognized Action Type")))
    )
  )
(defun FSM/remove-state (fsm id)
  ;; Remove a state from an FSM and clean up
  ;; remove the state from the fsm
  ;; remove any edges that point to the state
  )
(defun FSM/remove-edge (fsm id id2)
  ;; Remove an edge from an FSM and clean up
  ;; remove the edge from the fsm
  ;; remove the edge from its states

  )
;; Util
(defun FSM/copy (fsm)
  ;; Copy an FSM
  ;; copy all states
  ;; copy all edges
  ;; make new FSM
  )
(defun FSM/verify (fsm)
  ;; Verify an FSM
  ;; Check there is only one start state
  ;; Check there are no duplicate states
  ;; Check there are no duplicate edges
  ;; Check every state is reachable
  )
(defun FSM/stats (fsm)
  ;; Get a description of the FSM
  ;; collect number of edges per node,
  ;; number of states, edges
  ;; actions etc
  )

;; Example
(defun FSM/example ()
  (let* ((fsm (FSM/make-FSM))
         (state1 (FSM/make-state fsm :state1))
         (state2 (FSM/make-state fsm :state2 :entry '((lambda (state data) (message "state 2 action")))))
         (edge (FSM/make-edge fsm :state1 :state2))
         (ctx (FSM/make-context :state1))
         )
    (FSM/make-state fsm :state3)
    (FSM/make-state fsm :state4)
    (FSM/make-edge fsm :state2 :state3)
    (FSM/make-edge fsm :state3 :state4)
    (FSM/make-edge fsm :state4 :state1)
    (FSM/make-edge fsm :state1 :state3)
    ;; (FSM/add-actions fsm :state1 :exit '(lambda (state data) (message "exit test")))
    ;; (FSM/add-actions fsm :state2 :entry '(lambda (state data) (message "entry test")))


    (cl-loop repeat 15 do
             (FSM/act fsm ctx '(lambda (edges data) (nth (random (length edges)) edges))
                      :exit '((lambda (state data) (message "Leaving %s" (FSM/State-id state))))
                      :entry '((lambda (state data) (message "Entering %s" (FSM/State-id state))))
                      )

             )
    )
  )
