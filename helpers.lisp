(defun checkIfValidAndPlay (letter index)
    (cond 
        ((string/= (cadr (assoc index (cadr (assoc letter *state* :test #'string=)))) "-" ) '())
        (t (setf (cadr (assoc index (cadr (assoc letter *state* :test #'string=)))) *currentPlayer*))
    )
)

