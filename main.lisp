(load "gameConfig.lisp")
(load "game.lisp")
(load "helpers.lisp")

(gameConfig)
(initGame *numberOfCells*)
(printGame *state* *numberOfCells*)
(enterMove)
(printGame *state* *numberOfCells*)
(enterMove)
(printGame *state* *numberOfCells*)