
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


;------------------------------------------------------------------------------------------------------------------
;;end game

;; checks how many elements of connected are in list
(defun checkCount (connected list)
    (cond ((null connected) '0)
          ((clanp (car connected) list) (1+ (checkCount (cdr connected) list)))
          (t (checkCount (cdr connected) list)))
)

;; returns T or nill for bridge condition
(defun bridgeCondition (connected)
    (cond ((>= (checkCount connected *edges*) 2) t)
          (t '()))
)

;; returns T or nill for fork condition
(defun forkCondition (connected)
    (cond ((>= (checkCount connected *sides*) 3) t)
          (t '()))
)


;;nadji-put (graf l cilj cvorovi)

;;bridge
;------------------------------------------------------------------------------------
(defun checkBridgeEndGame (move sign)
    (if (string= sign "X") 
        (if (eql 2 (checkBridge move *xMovesGraph* *edges*)) t '())
    )
    (if (string= sign "O") 
        (if (eql 2 (checkBridge move *xMovesGraph* *edges*)) t '())
    )
)

(defun checkBridge (move graph edges)
     (cond ((null edges) '0)
           ((not (null  (nadji-put graph (list move) (car edges) '()))) (1+ (checkBridge move graph (cdr edges))))
           (t (checkBridge move graph (cdr edges)))
    )
)

;;fork
;------------------------------------------------------------------------------------

(defun checkForkEndGame(move sign state)
    (if (string= sign "X") 
        (checkFork move *xMovesGraph* *newSides* sign state)
    )
    (if (string= sign "O") 
        (checkFork move *oMovesGraph* *newSides* sign state)
    )
)

(defun checkFork (move graph newSides sign state)
    (findNeighbours (car move) (cadr move) state)
    (let ((validNeighbours (filterMyNeighbours *neighbours* sign state)))
        (cond ((null validNeighbours) '())
            ;; za svakog od suseda proveri da li je side,
                ;; ako jeste onda izbaci tu stranu iz novi sides
            ;; ako new sides ima count 3 elementa posle toga onda je kraj
            ;; ide se u rekurziju i oruje rezultat
        )   
    )
)

(defun checkIfSide (neighbours new Sides)

)