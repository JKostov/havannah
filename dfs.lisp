(defun nadji-put (graf l cilj cvorovi)
;; Ako nije ostalo čvorova za obradu onda nema rešenja i funkcija vraća praznu listu
    (cond ((null l) '())
;; Ako je prvi čvor jednak cilju onda je put nađen i vraća se ovaj čvor
    ((equal (car l) cilj) (list cilj))
    (t (let* ((cvorovi1 (append cvorovi (list (car l))))
;; Izdvajaju se potomci prvog čvora iz liste
        (potomci1 (dodaj-potomke graf (car l) (append (cdr l) cvorovi1)))
;; TRAŽENJE PO DUBINI: potomci se dodaju KRAJ liste neobrađenih čvorova
        (l1 (append potomci1 (cdr l)))

;; poziv funkcije nadji-put sa dodatim potomcima prvog čvora liste neobrađenih
        (nadjeni-put (nadji-put graf l1 cilj cvorovi1)))
    (cond ((null nadjeni-put) '())
;; ako je neki od potomaka čvor prvi član puta dodati čvor roditelja u put
        ((member (car nadjeni-put) potomci1 :test #'equal) (cons (car l) nadjeni-put))
        (t nadjeni-put))))))

(defun dodaj-potomke (graf cvor cvorovi)
 (cond ((null graf) '())
    ((equal (caar graf) cvor)
        (novi-cvorovi (cadar graf) cvorovi))
    (t (dodaj-potomke (cdr graf) cvor cvorovi))))

(defun novi-cvorovi (potomci cvorovi)
    (cond ((null potomci) '())
        ((member (car potomci) cvorovi :test #'equal)
            (novi-cvorovi (cdr potomci) cvorovi))
    (t (cons (car potomci)
        (novi-cvorovi (cdr potomci) cvorovi)))))