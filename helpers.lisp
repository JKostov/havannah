(load "helpersPhase2.lisp")

;Function that check if the move is valid for the sent state
(defun checkIfValid (state letter index)
    (cond 
        ( (string/= (cadr (assoc index (cadr (assoc letter state :test #'string=)))) "-" ) '() )
        ( t t )
    )
)

;Insert move, check if it is a valid one, creates a copy of the sent state and plays the move on the copy state and then it changes the current player
(defun enterMoveReturnNewState (state currentPlayer)
    (format t "~%~a is on the move. Insert your move (A 1):" currentPlayer)
    (setq move (read))
    (cond
        ( (not (listp move)) (format t "~%Invalid move! Please insert the move in list such as (B 2):") (enterMoveReturnNewState state currentPlayer) )
        ( (not (= (length move) 2)) (format t "~%Invalid move! The move list should contain 2 elements (C 3):") (enterMoveReturnNewState state currentPlayer) )
        ( (null (checkIfValid state (car move) (cadr move))) (format t "~%Invalid move!") (enterMoveReturnNewState state currentPlayer) )
        ( t 
            (let*
                (
                    (letter (car move))
                    (num (cadr move))
                    (stringMove (list (string letter) num))
                    (newState (playMoveOnStateForPlayer letter num state currentPlayer) )
                    (tmp (prepareAndAddToMoveGraph stringMove currentPlayer state))
                    (gameOver (testEndGame stringMove currentPlayer newState (getMovesGraphForPlayer currentPlayer)))
                )
                (if (null gameOver) (changePlayer))
                newState 
            )
        )
    )
)

(defun testEndGame (move currentPlayer state movesGraph)
    (let
        (
            (bridge (checkBridgeEndGame move currentPlayer movesGraph))
            (fork (checkForkEndGame move currentPlayer state))
            (ring (checkRingEndGame move currentPlayer state movesGraph))
        )
        (cond
            ( (or bridge fork ring) (setGameOver) )
            (t '())
        )
    )
)

;Creates a copy of the sent state and plays move on the copy state for the sent player(X or O)
(defun playMoveOnStateForPlayer (letter index state player)
    (let
        (
            (copyState (copy-tree state))
            (valid (checkIfValid state letter index))
        )
        (if (null valid) '() (setf (cadr (assoc index (cadr (assoc letter copyState :test #'string=)))) player ret copyState))
    )
)

;Function that returns all possible states for the next player and the sent state
(defun returnPossibleStates (state currentPlayer numberOfCells)
    (let*
        (
            (nextPlayer currentPlayer)
            (possibleStates (reverse (newStatesRows state nextPlayer 65 numberOfCells 0 (1- numberOfCells))))
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

;Function that reads a move play it create a new state then the state is appended to the global state list and the it prints the last game board
(defun enterMovePrintBoard(currentPlayer numberOfCells)
    (let*
        (
            (latestState (returnLatestState))
            (newStates (appendNewStateOnGlobalStates (enterMoveReturnNewState latestState currentPlayer) ))
            (newState (returnLatestState))
        )
        (printGame newState numberOfCells)
    )
)
