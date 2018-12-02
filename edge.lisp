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
    (setq *sides* 
        (removeAllElements *edges*
            (append
                (list (generateSideList 65 1 (- numberOfCells 2)))
                (list (getFirstAndLastElementFromState state))
                (list (generateSideList (+ 65 (* 2 (1- numberOfCells))) numberOfCells (1- (* 2 (1- numberOfCells))) ))
            )
        )
    )
)

(defun getFirstAndLastElementFromState (state)
    (cond
        ( (null state) '() )
        ( t (append (list (list (caar state) (car (caadar state)))) (list (list (caar state) (caar (last (cadar state))))) (getFirstAndLastElementFromState (cdr state))))
    )
)

(defun removeAllElements (elements list)
    (cond
        ( (null list) list )
        ( (member (car list) elements :test 'equal) (removeAllElements elements (cdr list)) )
        ( t (cons (car list) (removeAllElements elements (cdr list)) ))
    )
)


;; (print (generateEdges 6))
;; (print (generateSides '() 6))