
(defvar *xMovesGraph* '())
(defvar *oMovesGraph* '())
(defvar *currentSides* '())
(defvar *ring* '())

;; helper function for findingNeighbours - checks if current element is valid neighbour
(defun checkIfValidPosition (letter index state)
    (cadr (assoc index (cadr (assoc letter state :test #'string=))))
)

;;finds all the neighbours of passed move
(defun findNeighbours2 (letter index state)
    (append
    ;; letter    num - 1
        (if (not (null (checkIfValidPosition (string letter) (1- index) state))) (list (list (string letter) (1- index))) '() )
        ;; letter    num + 1
        (if (not (null (checkIfValidPosition (string letter) (1+ index) state))) (list (list (string letter) (1+ index))) '() )
        ;; letter-1  num - 1
        (if (not (null (checkIfValidPosition (string (code-char (1- (char-code (char (string letter) 0))))) (1- index) state)))  (list (list (string (code-char (1- (char-code (char (string letter) 0))))) (1- index))) '() )
        ;; letter-1  num
        (if (not (null (checkIfValidPosition (string (code-char (1- (char-code (char (string letter) 0))))) index state)))  (list (list (string (code-char (1- (char-code (char (string letter) 0))))) index)) '() )
        ;; letter+1  num + 1
        (if (not (null (checkIfValidPosition (string (code-char (1+ (char-code (char (string letter) 0))))) (1+ index) state))) (list (list (string (code-char (1+ (char-code (char (string letter) 0))))) (1+ index))) '() )
        ;; letter+1  num
        (if (not (null (checkIfValidPosition (string (code-char (1+ (char-code (char (string letter) 0))))) index state)))  (list (list (string (code-char (1+ (char-code (char (string letter) 0))))) index)) '() )
    )
)

;; filters neighbours that have same sign   
(defun filterMyNeighbours (list sign state)
    (cond
        ( (null list) '())
        ( (if (string= (checkIfValidPosition (caar list ) (cadar list) state) sign)
            (cons (car list) (filterMyNeighbours (cdr list) sign state))
            (filterMyNeighbours (cdr list) sign state)))
    )
)

;; prepares current move for graph of moves
(defun prepareNodeAndNeighbours (node sign state)
    (cons node (list (filterMyNeighbours (findNeighbours2 (car node) (cadr node) state) sign state))))

;;checks if list is member of list of lists
(defun clanp (el l)
    (cond 
        ((null l) '())
        ((equalp el (car l)) t)
        (t (clanp el (cdr l)))
    )
)

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
    ;; (filterMyNeighbours (findNeighbours2 (car move) (cadr move) state) sign state)
    (addToMoveGraph move sign state)
)


;------------------------------------------------------------------------------------------------------------------
;;end game

;;bridge
;------------------------------------------------------------------------------------
(defun checkBridgeEndGame (move sign)
    (cond
        ( (string= sign "X") (= 2 (checkBridge move *xMovesGraph* *edges*)) )
        ( (string= sign "O") (= 2 (checkBridge move *oMovesGraph* *edges*)) )
    )
)

;Returns number of path from the current move the the edges
(defun checkBridge (move graph edges)
    (cond 
        ((null edges) '0)
        ((not (null  (nadji-put graph (list move) (car edges) '()))) (1+ (checkBridge move graph (cdr edges))))
        (t (checkBridge move graph (cdr edges)))
    )
)

;;fork
;------------------------------------------------------------------------------------

;Sets global sides array calls checkFork and then it checks if the length of the global side array is less then 4
(defun checkForkEndGame(move sign state)
    (setq *currentSides* (copy-tree *sides*))
    (if (string= sign "X") 
        (checkFork move sign state '())
    )
    (if (string= sign "O") 
        (checkFork move sign state '())
    )
    (< (length *currentSides*) 4)
)

;Goes from the current move through his neighbours to the sides and if the neighbour is a side remove one side list from the global sides array
(defun checkFork (move sign state nodes)
    (cond
        ( (member move nodes :test 'equal) '() )
        (t
            (progn
                (let ((validNeighbours (filterMyNeighbours (findNeighbours2 (car move) (cadr move) state) sign state)))
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

;This function is trying to make connection from the current move to his valid neighbours without going on the same neighbours for the start and the end node
;It is called with modified graph that doesnt have a direct connection from the start node to the end node
(defun checkRing (move sign graph state)
    (let 
        ((validNeighbours (filterMyNeighbours (findNeighbours2 (car move) (cadr move) state) sign state)))
        (mapcar #'(lambda (n)
            (cond
                ( (equal *ring* t) '() )
                ( (not (null (nadji-put (removeRelationShipFromGraph graph move n) (list move) n (findSameNeighbours move n state) ))) (setq *ring* t) )
                (t '())
            )
        ) validNeighbours)  
    )
)

;This function returns the same neighbours for nodes n1 and n2
(defun findSameNeighbours (n1 n2 state)
    (findSameElementsInLists (findNeighbours2 (car n1) (cadr n1) state) (findNeighbours2 (car n2) (cadr n2) state))
)

;Helper function that is finding same elements for the lists l1 and l2
(defun findSameElementsInLists (l1 l2)
    (cond
        ( (null l1) '() )
        ( (member (car l1) l2 :test 'equal) (cons (car l1) (findSameElementsInLists (cdr l1) l2)) )
        ( t (findSameElementsInLists (cdr l1) l2))
    )
)

;This function is removing the relationship from the node the the relationship node
(defun removeRelationShipFromGraph (graph node relationship)
    (cond
        ( (null graph) '() )
        ( (equal (caar graph) node) (append (list (list (caar graph) (remove relationship (cadar graph) :test 'equal))) (cdr graph)) )
        ( t (cons (car graph) (removeRelationShipFromGraph (cdr graph) node relationship)) )
    )
)