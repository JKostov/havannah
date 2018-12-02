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

;; (setq *xMovesGraph* '(
;;     (("A" 1) (("B" 0) ("B" 1) ("B" 2)))
;;     (("B" 0) (("A" 1) ("B" 1)))
;;     (("B" 1) (("A" 1) ("B" 0) ("B" 2)))
;;     (("B" 2) (("A" 1) ("B" 1) ("B" 3)))
;;     (("B" 3) (("B" 2) ("B" 4)))
;;     (("B" 4) (("B" 3) ("B" 5)))
;;     (("B" 5) (("B" 4) ("B" 6)))
;;     (("B" 6) (("B" 5)))
;; ))

;; (print (checkForkEndGame '("B" 6) '"X"))

;; (do
;;     ((go *gameOver* (setq go *gameOver*)))
;;     ( (not (null go)) )
;;     (progn (testGameOver) (enterMovePrintBoard)) ;(printStates (returnPossibleStates (returnLatestState))))
;; )

