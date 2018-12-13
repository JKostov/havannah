(load "gameConfig.lisp")
(load "game.lisp")
(load "helpers.lisp")
(load "edge.lisp")
(load "dfs.lisp")
(load "helpersPhase2.lisp")
(load "minimax.lisp")
(load "helpersPhase3.lisp")

(gameConfig) ;Initialize game configuration
(initGame *numberOfCells*) ;Initialize game state
(printGame (returnLatestState) *numberOfCells*) ;Print game board

(generateEdges *numberOfCells*)
(generateSides *numberOfCells* )
(progn (testGameOver) (enterMovePrintBoard))
(setq asd (car (minimax (returnLatestState) 3 -100 100 *currentPlayer*)))
(print (findPlayedMoveFromState (returnLatestState) asd ))
(printGame asd *numberOfCells*)

;; (do
;;     ((go *gameOver* (setq go *gameOver*)))
;;     ( (not (null go)) (format t "Game over. ~%~a has won!" *currentPlayer*))
;;     (progn (testGameOver) (enterMovePrintBoard)) ;(printStates (returnPossibleStates (returnLatestState))))
;; )
