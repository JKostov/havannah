
(defvar *neighbours* '())
(defvar *xMovesGraph* '())
(defvar *oMovesGraph* '())
(defvar *currentSides* '())
(defvar *ring* '())

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
    )
)


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

;; adds backward relationship to current move, connecting already played moves that are his neighbours with him
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
    (cond
        ( (string= sign "X") (= 2 (checkBridge move *xMovesGraph* *edges*)) )
        ( (string= sign "O") (= 2 (checkBridge move *oMovesGraph* *edges*)) )
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
    (setq *currentSides* (copy-tree *sides*))
    (if (string= sign "X") 
        (checkFork move sign state '())
    )
    (if (string= sign "O") 
        (checkFork move sign state '())
    )
    (cond 
        ( (< (length *currentSides*) 4) t)
        ( t '())
    )
)

(defun checkFork (move sign state nodes)
    (cond
        ( (member move nodes :test 'equal) '() )
        (t
            (progn
                (findNeighbours (car move) (cadr move) state)
                (let ((validNeighbours (filterMyNeighbours *neighbours* sign state)))
                    (setq *currentSides* (removeSidesForMoves *currentSides* (cons move validNeighbours)))
                    (cond 
                        ((null validNeighbours) '())
                        ( (< (length *currentSides*) 4) '())
                        ( t (dolist (n validNeighbours) (checkFork n sign state (cons move nodes)) ))
                    )   
                )
            )
        )
    )
    
)

;;ring
;------------------------------------------------------------------------------------
(defun checkRingEndGame (move sign state)
    (if (string= sign "X") 
        (checkRing move sign *xMovesGraph* state)
    )
    (if (string= sign "O") 
        (checkRing move sign *oMovesGraph* state)
    )
    *ring*
)

(defun checkRing (move sign graph state)
    (findNeighbours (car move) (cadr move) state)
    (let 
        ((validNeighbours (filterMyNeighbours *neighbours* sign state)))
        (mapcar #'(lambda (n)
            (cond
                ( (equal *ring* t) '() )
                ( (not (null (nadji-put (removeRelationShipFromGraph graph move n) (list move) n (findSameNeighbours move n state) ))) (setq *ring* t) )
                (t '())
            )
        ) validNeighbours)  
    )
)

(defun findSameNeighbours (n1 n2 state)
    (findNeighbours (car n1) (cadr n1) state)
    (setq neighbours1 (copy-tree *neighbours*))
    (findNeighbours (car n2) (cadr n2) state)
    (setq neighbours2 (copy-tree *neighbours*))
    (findSameElementsInLists neighbours1 neighbours2)
)

(defun findSameElementsInLists (l1 l2)
    (cond
        ( (null l1) '() )
        ( (member (car l1) l2 :test 'equal) (cons (car l1) (findSameElementsInLists (cdr l1) l2)) )
        ( t (findSameElementsInLists (cdr l1) l2))
    )
)

(defun removeRelationShipFromGraph (graph node relationship)
    (cond
        ( (null graph) '() )
        ( (equal (caar graph) node) (append (list (list (caar graph) (remove relationship (cadar graph) :test 'equal))) (cdr graph)) )
        ( t (cons (car graph) (removeRelationShipFromGraph (cdr graph) node relationship)) )
    )
)