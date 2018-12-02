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
(generateSides (returnLatestState) *numberOfCells* )
(print *sides*)

(do
    ((go *gameOver* (setq go *gameOver*)))
    ( (not (null go)) )
    (progn (testGameOver) (enterMovePrintBoard)) ;(printStates (returnPossibleStates (returnLatestState))))
)

