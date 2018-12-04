(load "gameConfig.lisp")
(load "game.lisp")
(load "helpers.lisp")
(load "edge.lisp")
(load "dfs.lisp")
(load "helpersPhase2.lisp")

(gameConfig) ;Initialize game configuration
(initGame *numberOfCells*) ;Initialize game state
(printGame (returnLatestState) *numberOfCells*) ;Print game board

(generateEdges *numberOfCells*)
(generateSides *numberOfCells* )
;; (print *sides*)
;; (trace removeSidesForMoves)

;; (trace checkBridgeEndGame)
;; (trace checkForkEndGame)
;; (trace checkRingEndGame)


(do
    ((go *gameOver* (setq go *gameOver*)))
    ( (not (null go)) (format t "Game over. ~%~a has won!" *currentPlayer*))
    (progn (testGameOver) (print *xMovesGraph*) (enterMovePrintBoard)) ;(printStates (returnPossibleStates (returnLatestState))))
)
