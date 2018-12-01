(defvar *edges* '())
(defvar *sides* '())

(defun generateEdges (numberOfCells)
    (setq *edges* 
        (append
            (list (list "A" 0))
            (list (list "A" (1- numberOfCells)))
            (list (list (string (code-char (+ 65 (1- numberOfCells)))) 0))
            (list (list (string (code-char (+ 65 (1- numberOfCells)))) (* 2 (1- numberOfCells)) ))
            (list (list (string (code-char (+ 65 (* 2 (1- numberOfCells))))) (1- numberOfCells) ))
            (list (list (string (code-char (+ 65 (* 2 (1- numberOfCells))))) (* 2 (1- numberOfCells)) ))
        )
    )
)

(defun generateSideList (numberOfLetter start stop)
    (cond
        ( (> start stop) '() )
        ( t (append (list (list (string (code-char numberOfLetter)) start)) (generateSideList numberOfLetter (1+ start) stop ) ) )
    )
)

(defun generateSides (state numberOfCells)
    (setq *edges* 
        (append
            (generateSideList 65 0 (1- numberOfCells))
            (generateSideList (+ 65 (* 2 (1- numberOfCells))) (1- numberOfCells) (* 2 (1- numberOfCells)) )
        )
    )
)

(print (generateSides '() 6))