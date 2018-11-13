(load "gameConfig.lisp")
(load "game.lisp")

(gameConfig)
(initGame *numberOfCells*)
;; (print *state*)
(printState *state* *numberOfCells*)