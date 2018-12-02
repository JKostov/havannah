
(defvar *neighbours* '())
(defvar *xMovesGraph* '())
(defvar *oMovesGraph* '())

;; helper function for findingNeighbours - checks if current element is valid neighbour
(defun checkIfValidPosition (letter index state)
    (cadr (assoc index (cadr (assoc letter state :test #'string=)))))

;;finds all the neighbours of passed move
(defun findNeighbours (letter index state)
    ;; reset global neighbours list if it was previously used
    (cond ((not (null *neighbours*)) (setq *neighbours* '())))
    ;; letter    num - 1
    (cond
        ( (not (null (checkIfValidPosition letter (1- index) state))) (setq *neighbours* (cons (list letter (1- index)) *neighbours*)))
    )
    ;; letter    num + 1
    (cond
        ( (not (null (checkIfValidPosition letter (1+ index) state))) (setq *neighbours* (cons (list letter (1+ index)) *neighbours*)))
    )
    ;; letter-1  num - 1
    (cond
        ( (not (null (checkIfValidPosition (string (code-char (1- (char-code (char letter 0))))) (1- index) state))) (setq *neighbours* (cons (list (string (code-char (1- (char-code (char letter 0))))) (1- index)) *neighbours*)))
    )
    ;; letter-1  num
    (cond
        ( (not (null (checkIfValidPosition (string (code-char (1- (char-code (char letter 0))))) index state))) (setq *neighbours* (cons (list (string (code-char (1- (char-code (char letter 0))))) index) *neighbours*)))
    )
    ;; letter+1  num + 1
    (cond
        ( (not (null (checkIfValidPosition (string (code-char (1+ (char-code (char letter 0))))) (1+ index) state))) (setq *neighbours* (cons (list (string (code-char (1+ (char-code (char letter 0))))) (1+ index)) *neighbours*)))
    )
    ;; letter+1  num
    (cond
        ( (not (null (checkIfValidPosition (string (code-char (1+ (char-code (char letter 0))))) index state))) (setq *neighbours* (cons (list (string (code-char (1+ (char-code (char letter 0))))) index) *neighbours*)))
    ))


;; filters neighbours that have same sign   
(defun filterMyNeighbours (list sign state)
    (cond
        ( (null list) '())
        ( (if (string= (checkIfValidPosition (caar list ) (cadar list) state) sign)
            (cons (car list) (filterMyNeighbours (cdr list) sign state))
            (filterMyNeighbours (cdr list) sign state)))))

;; prepares current move for graph of moves
(defun prepareNodeAndNeighbours (node sign state)
    (findNeighbours (car node) (cadr node) state)
    (setq *neighbours* (filterMyNeighbours *neighbours* sign state))
    (cons node (list *neighbours*)))

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
(defun addToMoveGraph (move sign state)
    (let ((preparedMove (prepareNodeAndNeighbours move sign state)))
        (if (string= sign "X") 
            (setq *xMovesGraph* (cons preparedMove *xMovesGraph*)) 
            (setq *oMovesGraph* (cons preparedMove *oMovesGraph*)))
        (if (string= sign "X") 
            (setq *xMovesGraph* (addBackwardRelationship (cadr preparedMove) *xMovesGraph* move)) 
            (setq *oMovesGraph* (addBackwardRelationship (cadr preparedMove) *oMovesGraph* move)))
    )
)

;; adds move to the graph of moves
(defun prepareAndAddToMoveGraph (move sign state)
    (findNeighbours (car move) (cadr move) state)
    (filterMyNeighbours *neighbours* sign state)
    (addToMoveGraph move sign state)
)
