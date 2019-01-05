
(defvar *xMovesGraph* '()) ;Global variable that contains the X moves graph - when X plays move, the move is added to the graph and its neighbours relationships
(defvar *oMovesGraph* '()) ;Global variable that contains the O moves graph - when X plays move, the move is added to the graph and its neighbours relationships

;; helper function for findingNeighbours - checks if current element is valid neighbour
(defun checkIfValidPosition (letter index state)
    (cadr (assoc index (cadr (assoc letter state :test #'string=))))
)

;Returns the moves graph for the sent player
(defun getMovesGraphForPlayer (player)
    (cond
        ( (string= player "X") *xMovesGraph* )
        ( t *oMovesGraph* )
    )
)

;Sets the moves graph with the new one for the sent player
(defun setMovesGraphForPlayer (player newGraph)
    (cond
        ( (string= player "X") (setq *xMovesGraph* newGraph) )
        ( t (setq *oMovesGraph* newGraph) )
    )
)

;;finds all the neighbours of passed move
(defun findNeighbours (letter index state)
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
        ( (string= (checkIfValidPosition (caar list ) (cadar list) state) sign) (cons (car list) (filterMyNeighbours (cdr list) sign state)) )
        (t (filterMyNeighbours (cdr list) sign state) )
    )
)

;; prepares current move for graph of moves
(defun prepareNodeAndNeighbours (node sign state)
    (cons node (list (filterMyNeighbours (findNeighbours (car node) (cadr node) state) sign state)))
)

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
    (let 
        (
            (currentRow (car graph))
            (currentNode (caar graph))
            (neighbours (cadar graph))
        )
        (cond
            ( (null graph) '() )
            ( (not (clanp currentNode list)) (cons currentRow (addBackwardRelationship list (cdr graph) node)) )
            ( t (cons (cons currentNode (cons (cons node neighbours) '())) (addBackwardRelationship list (cdr graph) node)) )
        )
    )
)

;;adds move to move graph
(defun addToMoveGraph (move sign state)
    (let*
        (
            (preparedMove (prepareNodeAndNeighbours move sign state))
            (movesGraph (getMovesGraphForPlayer sign))
            (newMovesGraph (setMovesGraphForPlayer sign (cons preparedMove movesGraph)))
        )
        (setMovesGraphForPlayer sign (addBackwardRelationship (cadr preparedMove) newMovesGraph move) )
    )
)

(defun getMovesGraphForNewMove (move sign state)
    (let*
        (
            (preparedMove (prepareNodeAndNeighbours move sign state))
            (movesGraph (getMovesGraphForPlayer sign))
            (newMovesGraph (cons preparedMove movesGraph))
        )
        (addBackwardRelationship (cadr preparedMove) newMovesGraph move)
    )
)

;; adds move to the graph of moves
(defun prepareAndAddToMoveGraph (move sign state)
    (addToMoveGraph move sign state)
)


;------------------------------------------------------------------------------------------------------------------
;;end game

;;bridge
;------------------------------------------------------------------------------------
(defun checkBridgeEndGame (move sign movesGraph)
    (= 2 (checkBridge move movesGraph *edges*))
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

;Calls findAllConnectedMoves then it removes the sides and checks if the sides list has less then 4 elements
; *sides* helper global variable that is initialized when the game begins for the game parameters
(defun checkForkEndGame(move sign state)
    (< (length (removeSidesForMoves *sides* (findAllConnectedMoves (list move) sign state '()))) 4)
)

;Finds all connected moves for the current move
(defun findAllConnectedMoves (moves sign state visited)
    (cond
        ( (null moves) '())
        ( (member (car moves) visited :test 'equal) (findAllConnectedMoves (cdr moves) sign state visited))
        (t (cons (car moves) (findAllConnectedMoves (append (cdr moves) (filterMyNeighbours (findNeighbours (caar moves) (cadar moves) state) sign state)) sign state (cons (car moves) visited) )))
    )
)

;;ring
;------------------------------------------------------------------------------------
(defun checkRingEndGame (move sign state movesGraph)
    (checkRing move sign movesGraph state)
)

;This function is trying to make connection from the current move to his valid neighbours without going on the same neighbours for the start and the end node
;It is called with modified graph that doesnt have a direct connection from the start node to the end node
(defun checkRing (move sign graph state)
    (let 
        ((validNeighbours (filterMyNeighbours (findNeighbours (car move) (cadr move) state) sign state)))
        (some #'(lambda (n)
            (cond
                ( (not (null (nadji-put (removeRelationShipFromGraph graph move n) (list move) n (findSameNeighbours move n state) ))) t )
                (t '())
            )
        ) validNeighbours)
    )
)

;This function returns the same neighbours for nodes n1 and n2
(defun findSameNeighbours (n1 n2 state)
    (findSameElementsInLists (findNeighbours (car n1) (cadr n1) state) (findNeighbours (car n2) (cadr n2) state))
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