(load "gameConfig.lisp")
(load "game.lisp")
(load "helpers.lisp")

(gameConfig) ;Initialize game configuration
(initGame *numberOfCells*) ;Initialize game state
(printGame (returnLatestState) *numberOfCells*) ;Print game board

(do
    ((go *gameOver* (setq go *gameOver*)))
    ( (not (null go)) )
    (progn (testGameOver) (enterMovePrintBoard)) ;(printStates (returnPossibleStates (returnLatestState))))
)
