(defun nadji-put (graf l cilj cvorovi)
;; Ako nije ostalo čvorova za obradu onda nema rešenja i funkcija vraća praznu listu
    (cond ((null l) '())
;; Ako je prvi čvor jednak cilju onda je put nađen i vraća se ovaj čvor
    ((equal (car l) cilj) (list cilj))
    (t (let* ((cvorovi1 (append cvorovi (list (car l))))
;; Izdvajaju se potomci prvog čvora iz liste
        (potomci1 (dodaj-potomke graf (car l) (append (cdr l) cvorovi1)))
;; TRAŽENJE PO SIRINI: potomci se dodaju KRAJ liste neobrađenih čvorova
        (l1 (append (cdr l) potomci1))

;; poziv funkcije nadji-put sa dodatim potomcima prvog čvora liste neobrađenih
        (nadjeni-put (nadji-put graf l1 cilj cvorovi1)))
    (cond ((null nadjeni-put) '())
;; ako je neki od potomaka čvor prvi član puta dodati čvor roditelja u put
        ((member (car nadjeni-put) potomci1) (cons (car l) nadjeni-put))
        (t nadjeni-put))))))

(defun dodaj-potomke (graf cvor cvorovi)
 (cond ((null graf) '())
    ((equal (caar graf) cvor)
        (novi-cvorovi (cadar graf) cvorovi))
    (t (dodaj-potomke (cdr graf) cvor cvorovi))))

(defun novi-cvorovi (potomci cvorovi)
    (cond ((null potomci) '())
        ((member (car potomci) cvorovi)
            (novi-cvorovi (cdr potomci) cvorovi))
    (t (cons (car potomci)
        (novi-cvorovi (cdr potomci) cvorovi)))))

(setq graf1 '((a (a b)) (b (c e)) (c (a b))(d (e f)) (e (d)) (f (a))))
(setq graf2 '((a (b)) (b (c f)) (c (d)) (d (e))(e (d)) (f (a))))
    ;;(print (nadji-put graf2 '(a) 'f '())) ;;  => (a b f)
 ;;(print (nadji-put graf1 '(a) 'f '())) ;; => (a b e d f)