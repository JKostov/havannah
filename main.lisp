(load "gameConfig.lisp")
(load "game.lisp")
(load "helpers.lisp")

(gameConfig) ;Initialize game configuration
(initGame *numberOfCells*) ;Initialize game state
(printGame *state* *numberOfCells*) ;Print game board

(enterMove)
(printGame *state* *numberOfCells*)
(enterMove)
(printGame *state* *numberOfCells*)
(enterMove)
(printGame *state* *numberOfCells*)
(enterMove)
(printGame *state* *numberOfCells*)