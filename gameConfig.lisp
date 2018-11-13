
(defvar *numberOfCells*) ;Table size
(defvar *firstPlayer*) ;Human or computer

(defun gameConfig ()
    (print "Insert number of cells (6-12):")
    (setq *numberOfCells* (read))
    (if (or (< *numberOfCells* 6) (> *numberOfCells* 12)) (setq *numberOfCells* 6) )
    (print "Insert who will be playing first H for human and C for computer:")
    (setq *firstPlayer* (read))
    (if (not (or (equal *firstPlayer* 'H) (equal *firstPlayer* 'C))) (setq *firstPlayer* "H"))
    (format t "Number ~a ~%" *numberOfCells*)
    (format t "First ~a" *firstPlayer*)
)
