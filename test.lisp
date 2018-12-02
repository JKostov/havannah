
(defvar *grafTest* '(
            ("A" ( (0 "X") (1 "X") (2 "-") (3 "-"))) 
         ("B" ( (0 "-") (1 "X") (2 "-") (3 "-") (4 "-"))) 
      ("C" ( (0 "X") (1 "-") (2 "-") (3 "X") (4 "X") (5 "-"))) 
    ("D" ( (0 "-") (1 "-") (2 "-") (3 "-") (4 "-") (5 "-") (6 "-"))) 
      ("E" ( (1 "-") (2 "-") (3 "-") (4 "-") (5 "-") (6 "-"))) 
         ("F" ( (2 "-") (3 "-") (4 "-") (5 "-") (6 "-"))) 
            ("G" ( (3 "-") (4 "-") (5 "-") (6 "-"))) 
))
(defvar *neighbours* '())
(defvar *xMovesGraph* '(
    (("C" 0) ())
    (("C" 3) (("C" 4)))
    (("C" 4) (("C" 3)))
    (("B" 1) (("A" 0) ("A" 1)))
    (("A" 1) (("A" 0) ("B" 1)))
    (("A" 0) (("A" 1) ("B" 1)))
))


(defun checkIfValidAndPlay (letter index)
    (cond 
        ((string/= (cadr (assoc index (cadr (assoc letter *grafTest* :test #'string=)))) "-" ) '())
        (t (setf (cadr (assoc index (cadr (assoc letter *grafTest* :test #'string=)))) "X"))
    )
)
;; helper function for findingNeighbours - checks if current element is valid neighbour
(defun checkIfValidPosition (letter index)
    (cadr (assoc index (cadr (assoc letter *grafTest* :test #'string=)))))

;;finds all the neighbours of passed move
(defun findNeighbours (letter index)
    ;; reset global neighbours list if it was previously used
    (cond ((not (null *neighbours*)) (setq *neighbours* '())))
    ;; letter    num - 1
    (cond
        ( (not (null (checkIfValidPosition letter (1- index)))) (setq *neighbours* (cons (list letter (1- index)) *neighbours*)))
    )
    ;; letter    num + 1
    (cond
        ( (not (null (checkIfValidPosition letter (1+ index)))) (setq *neighbours* (cons (list letter (1+ index)) *neighbours*)))
    )
    ;; letter-1  num - 1
    (cond
        ( (not (null (checkIfValidPosition (string (code-char (1- (char-code (char letter 0))))) (1- index)))) (setq *neighbours* (cons (list (string (code-char (1- (char-code (char letter 0))))) (1- index)) *neighbours*)))
    )
    ;; letter-1  num
    (cond
        ( (not (null (checkIfValidPosition (string (code-char (1- (char-code (char letter 0))))) index))) (setq *neighbours* (cons (list (string (code-char (1- (char-code (char letter 0))))) index) *neighbours*)))
    )
    ;; letter+1  num + 1
    (cond
        ( (not (null (checkIfValidPosition (string (code-char (1+ (char-code (char letter 0))))) (1+ index)))) (setq *neighbours* (cons (list (string (code-char (1+ (char-code (char letter 0))))) (1+ index)) *neighbours*)))
    )
    ;; letter+1  num
    (cond
        ( (not (null (checkIfValidPosition (string (code-char (1+ (char-code (char letter 0))))) index))) (setq *neighbours* (cons (list (string (code-char (1+ (char-code (char letter 0))))) index) *neighbours*)))
    ))


;; filters neighbours that have same sign   
(defun filterMyNeighbours (list sign)
    (cond
        ( (null list) '())
        ( (if (string= (checkIfValidPosition (caar list ) (cadar list)) sign)
            (cons (car list) (filterMyNeighbours (cdr list) sign))
            (filterMyNeighbours (cdr list) sign)))))

;; prepares current move for graph of moves
(defun prepareNodeAndNeighbours (node sign)
    (findNeighbours (car node) (cadr node))
    (setq *neighbours* (filterMyNeighbours *neighbours* sign))
    (cons (cons node '()) (cons *neighbours* '())))

;;checks if list is member of list of lists
(defun clanp (el l)
    (cond ((null l) '())
          ((equalp el (car l)) t)
          (t (clanp el (cdr l)))))

(defun addBackwardRelationship (list graph node)
    (let ((currentRow (car graph))
          (currentNode (caar graph))
          (neighbours (cadar graph)))
        (cond
            ((null graph) '())
            ((not (clanp currentNode list)) (cons currentRow (addBackwardRelationship list (cdr graph) node)))
            (t(cons (cons currentNode (cons (cons node neighbours) '())) (addBackwardRelationship list (cdr graph) node)))
        )
    )
)

;;adds move to move graph
(defun addToMoveGraph (move sign)
    (let ((preparedMove (prepareNodeAndNeighbours move sign)))
       (setq *xMovesGraph* (cons preparedMove *xMovesGraph*))
       (setq *xMovesGraph* (addBackwardRelationship (cadr preparedMove) *xMovesGraph* move))
    )
)

;; adds move to the graph of moves
(defun prepareAndAddToMoveGraph (move sign)
    (findNeighbours (car move) (cadr move))
    (filterMyNeighbours *neighbours* sign)
    (addToMoveGraph move sign)
)
