(defun checkIfValidAndPlay(letter index value)
    (cond ((string/= (cadr (assoc index (cadr (assoc letter lista :test #'string=)))) "-" ) '())
        (t (setf (cadr (assoc index (cadr (assoc letter lista :test #'string=)))) "X"))))

