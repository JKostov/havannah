(load "InferenceEngine.lisp")

(defun createStateFactsFromState (state)
    (cond
        ((null state) '())
        (t
            (let*
                (
                    (row (convertStringToNumber (caar state)))
                    (rowFacts (createRowFactsFromRow row (cadar state)))
                )
                (append rowFacts (createStateFactsFromState (cdr state)))
            )
        )
    )
)

(defun createRowFactsFromRow (row elements)
    (cond
        ( (null elements) '() )
        ( (string/= (cadar elements) "-") (cons (list (cadar elements) row (caar elements)) (createRowFactsFromRow row (cdr elements))) )
        (t (createRowFactsFromRow row (cdr elements)) )
    )
)

(defun convertStringToNumber (string)
    (- (char-code (char string 0)) 65 )
)

(defun convertNumberToString (number)
    (string (code-char (+ 65 number)))
)

(defun getAssessment(state currentPlayer)
    (let*
        (
            (numberOfCells *numberOfCells*)
            (facts (createStateFactsFromState state))
            (rules (returnRules numberOfCells) )
        )
        (prepare-knowledge rules facts 1)
        (cond
            ( (and (string/= currentPlayer "X") (= (mod (getDepth) 2 ) 1) )
                (cond
                    ( (> (count-results '(finish_x)) 0) 2000 )
                    ( (> (count-results '(finish_x_back)) 0) 1000 )
                    ( (> (- (count-results '(middle_up_x)) (count-results '(counter_x))) -1) (+ 100 (count-results '(middle_up_x))))
                    ( (> (- (count-results '(middle_down_x)) (count-results '(counter_x))) -3) (+ 50 (count-results '(middle_down_x))))
                    ( (> (- (+ 9 (1- (* 2 numberOfCells))) (+ (count-results '(counter_x)) (count-results '(counter_o)) )) 0) (+ 30 (count-results '(middle_x))))
                    ( (and (> (- (+ 11 (1- (* 2 numberOfCells))) (+ (count-results '(counter_x)) (count-results '(counter_o)) )) 0) (= (count-results '(middle_up2_x)) 1) ) 25)
                    ( (and (> (- (+ 12 (1- (* 2 numberOfCells))) (+ (count-results '(counter_x)) (count-results '(counter_o)) )) 0) (= (count-results '(middle_down2_x)) 1) ) 20)
                    ( (> (count-results '(middledown_x)) 0) (+ 10 (count-results '(middledown_x)) ))
                    (t 1)
                )
            )
            (t
                (cond
                    ( (> (count-results '(finish_o)) 0) -2000 )
                    ( (> (count-results '(finish_o_back)) 0) -1000 )
                    ( (> (- (count-results '(middle_up_o)) (count-results '(counter_o))) -1) (- -100 (count-results '(middle_up_o))))
                    ( (> (- (count-results '(middle_down_o)) (count-results '(counter_o))) -3) (- -50 (count-results '(middle_down_o))))
                    ( (> (- (+ 9 (1- (* 2 numberOfCells))) (+ (count-results '(counter_x)) (count-results '(counter_o)) )) 0) (- -30 (count-results '(middle_o))))
                    ( (and (> (- (+ 11 (1- (* 2 numberOfCells))) (+ (count-results '(counter_x)) (count-results '(counter_o)) )) 0) (= (count-results '(middle_up2_o)) 1) )  -25)
                    ( (and (> (- (+ 12 (1- (* 2 numberOfCells))) (+ (count-results '(counter_x)) (count-results '(counter_o)) )) 0) (= (count-results '(middle_down2_o)) 1) ) -20)
                    ( (> (count-results '(middledown_o)) 0) (- -10 (count-results '(middledown_o)) ))
                    (t -1)
                )
            )
        )
    )
) 

(defun findFinishingMove (currentPlayer numberOfCells)
    (let*
        (
            (lastState (returnLatestState))
            (possibleStates (returnPossibleStates lastState currentPlayer numberOfCells))
            (endGameState (findEndGameState lastState possibleStates currentPlayer))
            (finishMove (if endGameState (findPlayedMoveFromState lastState endGameState) '()))
        )
        (if finishMove (list (convertStringToNumber (car finishMove)) (cadr finishMove)) '())
    )
)

(defun findEndGameState (state states currentPlayer)
    (cond
        ((null states) '())
        (t 
            (let*
                (
                    (futureState (car states))
                    (playedMove (findPlayedMoveFromState state futureState))
                    (endGame (testEndGame playedMove currentPlayer futureState (getMovesGraphForNewMove playedMove currentPlayer futureState)))
                )
                (cond
                    ( (not (null endGame)) futureState )
                    (t (findEndGameState state (cdr states) currentPlayer) )
                )
            )
        )
    )
)

(defun replaceElement (list elToReplace replaceWith)
    (cond
        ( (null list) '() )
        ( (atom list) (if (eql list elToReplace) replaceWith list) )
        (t 
            (mapcar #'(lambda (x) (replaceElement x elToReplace replaceWith)) list)
        )
    )
)

(defun returnRules (numberOfCells)
    (replaceElement 
        '(	
            (if (and ("X" ?a ?b)  (!eq ?a (1- numberOfCells)) ) (middle_x))
            (if (and ("O" ?c ?d)  (!eq ?c (1- numberOfCells)) ) (middle_o))

            (if (and ("X" ?a ?b)  (!eq ?a numberOfCells) ) (middledown_x))
            (if (and ("O" ?c ?d)  (!eq ?c numberOfCells) ) (middledown_o))

            (if (and ("X" ?a ?b)  (!eq ?a (- numberOfCells 2)) (!eq ?b 0)) (middle_up_x))
            (if (and ("O" ?c ?d)  (!eq ?c (- numberOfCells 2)) (!eq ?d 0)) (middle_up_o))

            (if (and ("X" ?a ?b)  (!eq ?a (- numberOfCells 2)) (!eq ?b 1)) (middle_up2_x))
            (if (and ("O" ?c ?d)  (!eq ?c (- numberOfCells 2)) (!eq ?d 1)) (middle_up2_o))

            (if (and ("X" ?a ?b)  (!eq ?a (- numberOfCells 2)) (!eq ?b (- (* 2 numberOfCells) 3) )) (middle_up_x))
            (if (and ("O" ?c ?d)  (!eq ?c (- numberOfCells 2)) (!eq ?d (- (* 2 numberOfCells) 3) )) (middle_up_o))

            (if (and ("X" ?a ?b)  (!eq ?a numberOfCells) (!eq ?b 1)) (middle_down_x))
            (if (and ("O" ?c ?d)  (!eq ?c numberOfCells) (!eq ?d 1)) (middle_down_o))

            (if (and ("X" ?a ?b)  (!eq ?a numberOfCells) (!eq ?b 2)) (middle_down2_x))
            (if (and ("O" ?c ?d)  (!eq ?c numberOfCells) (!eq ?d 2)) (middle_down2_o))

            (if (and ("X" ?a ?b)  (!eq ?a numberOfCells) (!eq ?b (- (* 2 numberOfCells) 2) )) (middle_down_x))
            (if (and ("O" ?c ?d)  (!eq ?c numberOfCells) (!eq ?d (- (* 2 numberOfCells) 2) )) (middle_down_o))

            ;Counters
            (if ("X" ?a ?b) (counter_x))
            (if ("O" ?c ?d) (counter_o))

            ;FinishMove
            (if (and ("X" ?a ?b) (!eq (list ?a ?b) (findFinishingMove "O" numberOfCells))) (finish_x_back))
            (if (and ("O" ?c ?d) (!eq (list ?c ?d) (findFinishingMove "X" numberOfCells))) (finish_o_back))

            (if (and ("X" ?a ?b) (!eq (list ?a ?b) (findFinishingMove "X" numberOfCells))) (finish_x))
            (if (and ("O" ?c ?d) (!eq (list ?c ?d) (findFinishingMove "O" numberOfCells))) (finish_o))
        )
    'numberOfCells numberOfCells)
)

(defun !eq (a b)
    (equal a b)
)
