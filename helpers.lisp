;Function that check if the move is valid and if it is updates the game state with that move
(defun checkIfValidAndPlay (letter index)
    (cond 
        ((string/= (cadr (assoc index (cadr (assoc letter *state* :test #'string=)))) "-" ) '())
        (t (setf (cadr (assoc index (cadr (assoc letter *state* :test #'string=)))) *currentPlayer*))
    )
)

;Insert move, check if it is a valid one and then it changes the current player
(defun enterMove ()
    (format t "~%~a is on the move. Insert your move (A 1):" *currentPlayer*)
    (setq move (read))
    (cond
        ( (not (listp move)) (format t "~%Invalid move! Please insert the move in list such as (B 2):") (enterMove) )
        ( (not (= (length move) 2)) (format t "~%Invalid move! The move list should contain 2 elements (C 3):") (enterMove) )
        ( (null (checkIfValidAndPlay (car move) (cadr move))) (format t "~%Invalid move!") (enterMove) )
        ( t (changePlayer) )
    )
)

;Function used for changing the current player after a valid move
(defun changePlayer ()
    (cond
        ( (equal *currentPlayer* "X") (setq *currentPlayer* "O"))
        (t (setq *currentPlayer* "X"))
    )
)
