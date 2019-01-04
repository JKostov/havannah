(defun findPlayedMoveFromState (prevState newState)
    (cond
        ( (null prevState) '() )
        ( (equal (car prevState) (car newState)) (findPlayedMoveFromState (cdr prevState) (cdr newState)) )
        ( t (list (caar prevState) (findNumberDifferentMove (cadar prevState) (cadar newState) )) )
    )
)

(defun findNumberDifferentMove (prevNumber newNumber)
    (cond
        ( (null prevNumber) '() )
        ( (equal (car prevNumber) (car newNumber)) (findNumberDifferentMove (cdr prevNumber) (cdr newNumber)) )
        ( t (caar prevNumber) )
    )
)

(defun enterMoveForComputer (state move currentPlayer)
    (let*
        (
            (letter (car move))
            (num (cadr move))
            (newState (playMoveOnStateForPlayer letter num state currentPlayer))
            (tmp (prepareAndAddToMoveGraph move currentPlayer state))
            (gameOver (testEndGame move currentPlayer newState (getMovesGraphForPlayer currentPlayer)))
        ) 
        (if (null gameOver) (changePlayer))
    )
)

(defun getDepth ()
    3
)

(defun enterMovePrintBoardComputer (currentPlayer numberOfCells)
    (format t "~%~a computer is on the move..." currentPlayer)
    (let*
        (
            (latestState (returnLatestState))
            (depth (getDepth))
            (newState (car (minimax latestState depth -100 100 currentPlayer numberOfCells)))
            (playedMove (findPlayedMoveFromState latestState newState))
        )
        (enterMoveForComputer latestState playedMove currentPlayer)
        (appendNewStateOnGlobalStates newState)
        (printGame newState numberOfCells)
    )
)