(defun findPlayedMoveFromState (prevState newState)
    (cond
        ( (null prevState) '() )
        ( (equal (car prevState) (car newState)) (findPlayedMoveFromState (cdr prevState) (cdr newState)) )
        ( t (list (caar prevState) (findNumberDifferentMove (cadar prevState) (cadar newState) )) )
    )
)

(defun findNumberDifferentMove (prevNumber newNumber)
    (cond
        ( (null prevNumber) '() )
        ( (equal (car prevNumber) (car newNumber)) (findNumberDifferentMove (cdr prevNumber) (cdr newNumber)) )
        ( t (caar prevNumber) )
    )
)