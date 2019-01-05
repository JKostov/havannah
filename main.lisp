(load "gameConfig.lisp")
(load "game.lisp")
(load "helpers.lisp")
(load "edge.lisp")
(load "dfs.lisp")
(load "helpersPhase2.lisp")
(load "minimax.lisp")
(load "helpersPhase3.lisp")
(load "helpersPhase4.lisp")

(gameConfig) ;Initialize game configuration
(initGame *numberOfCells*) ;Initialize game state
(printGame (returnLatestState) *numberOfCells*) ;Print game board

(generateEdges *numberOfCells*) ;Initialize list with edge elements
(generateSides *numberOfCells* ) ;Initialize list with side elements

(do
    ((go *gameOver* (setq go *gameOver*)))
    ( (not (null go)) (format t "Game over. ~%~a has won!" *currentPlayer*))
    (cond   
        ( (and (string= *firstPlayer* "H") (string= *currentPlayer* "X")) (enterMovePrintBoard *currentPlayer* *numberOfCells*) )
        ( (and (string= *firstPlayer* "H") (string= *currentPlayer* "O")) (enterMovePrintBoardComputer *currentPlayer* *numberOfCells*) )
        ( (and (string= *firstPlayer* "C") (string= *currentPlayer* "X")) (enterMovePrintBoardComputer *currentPlayer* *numberOfCells*) )
        ( t (string= *currentPlayer* "X") (enterMovePrintBoard *currentPlayer* *numberOfCells*) )
    )
)
