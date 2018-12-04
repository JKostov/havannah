(load "helpersPhase2.lisp")

;Function that check if the move is valid for the sent state
(defun checkIfValid (state letter index)
    (cond 
        ( (string/= (cadr (assoc index (cadr (assoc letter state :test #'string=)))) "-" ) '() )
        ( t t )
    )
)

;Insert move, check if it is a valid one, creates a copy of the sent state and plays the move on the copy state and then it changes the current player
(defun enterMoveReturnNewState (state)
    (format t "~%~a is on the move. Insert your move (A 1):" *currentPlayer*)
    (setq move (read))
    (cond
        ( (not (listp move)) (format t "~%Invalid move! Please insert the move in list such as (B 2):") (enterMoveReturnNewState state) )
        ( (not (= (length move) 2)) (format t "~%Invalid move! The move list should contain 2 elements (C 3):") (enterMoveReturnNewState state) )
        ( (null (checkIfValid state (car move) (cadr move))) (format t "~%Invalid move!") (enterMoveReturnNewState state) )
        ( t (prog1 
                (playMoveOnStateForPlayer (car move) (cadr move) state *currentPlayer*) 
                (prepareAndAddToMoveGraph (list (string (car move)) (cadr move)) *currentPlayer* state)
                (setq *gameOver* (or (checkBridgeEndGame (list (string (car move)) (cadr move)) *currentPlayer*) (checkForkEndGame (list (string (car move)) (cadr move)) *currentPlayer* (returnLatestState)) (checkRingEndGame (list (string (car move)) (cadr move)) *currentPlayer* (returnLatestState))))
                (if (null *gameOver*) (changePlayer)) 
            )
        )
    )
)

;Function used for changing the current player after a valid move
(defun changePlayer ()
    (cond
        ( (equal *currentPlayer* "X") (setq *currentPlayer* "O"))
        (t (setq *currentPlayer* "X"))
    )
)

;Creates a copy of the sent state and plays move on the copy state for the sent player(X or O)
(defun playMoveOnStateForPlayer (letter index state player)
    (setq copyState (copy-tree state))
    (cond 
        ((string/= (cadr (assoc index (cadr (assoc letter copyState :test #'string=)))) "-" ) '())
        (t (setf (cadr (assoc index (cadr (assoc letter copyState :test #'string=)))) player ret copyState))
    )
    
)

;Function that returns all possible states for the next player and the sent state
(defun returnPossibleStates (state)
    (let*
        (
            (nextPlayer *currentPlayer*)
            (possibleStates (reverse (newStatesRows state nextPlayer 65 *numberOfCells* 0 (1- *numberOfCells*))))
        )
        possibleStates
    )
)

;Calls newStatesColumns with all letters from the board
(defun newStatesRows (state player currentLetter numberOfCells start stop)
    (cond
        ( (> currentLetter (+ 65 (* 2 (1- numberOfCells)))) '() )
        ( (not (= stop (* 2 (1- numberOfCells)))) (append (newStatesRows state player (1+ currentLetter) numberOfCells start (1+ stop)) (newStatesColumns state player (string (code-char currentLetter)) start stop) )  )
        ( t (append (newStatesRows state player (1+ currentLetter) numberOfCells (1+ start) stop) (newStatesColumns state player (string (code-char currentLetter)) start stop) )  )
    )
)

;For every sent letter and available number creates a new state and playes a move on the letter and number
(defun newStatesColumns (state player currentLetter start stop)
    (cond
        ( (> start stop) '() )
        ( t
            (let*
                (
                    (newState (playMoveOnStateForPlayer currentLetter start state player))
                )
                (if (null newState) (newStatesColumns state player currentLetter (1+ start) stop ) (append (list (playMoveOnStateForPlayer currentLetter start state player)) (newStatesColumns state player currentLetter (1+ start) stop ) ) )
            )
        )
        
    )
)

;Helper function used for printing all the possible states
(defun printStates (states)
    (cond
        ( (null states) '())
        ( t (printGame (car states) *numberOfCells*) (printStates (cdr states)))
    )
)

;Helper function used for appending the new generated state from the played move to the global states list
(defun appendNewStateOnGlobalStates (newState)
    (setq *states* (append *states* (list newState)))
)

;Helper function that returns the last game state
(defun returnLatestState ()
    (car (last *states*))
)

;Function that reads a move play it create a new state then the state is appended to the global state list and the it prints the last game board
(defun enterMovePrintBoard()
    (appendNewStateOnGlobalStates (enterMoveReturnNewState (returnLatestState)) )
    (printGame (returnLatestState) *numberOfCells*)
)

;Game over?
(defun testGameOver()
    (setq *gameOver* '())
)