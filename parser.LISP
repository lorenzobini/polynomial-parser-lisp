
; varpowers (Monomial)
; INPUT: a Monomial object (traditional form; parsed form)
; RETURNS: the list of the Monomial's variable powers

(defun varpowers (monomial)
  (cond ((null monomial) nil)
        ((is-trad-poly monomial)
         (varpowers (as-monomial monomial)))
        ((is-monomial monomial)
         (let ((vps (fourth monomial)))
           (let ((vpSorted (sort vps #'string-lessp :key #'third)))
             (let ((vpNorm (normalize-mons vpSorted))) vpNorm))))
        (T (error "Input varpowers invalid." monomial))))



; vars-of (Monomial)
; INPUT: a Monomial object (traditional form; parsed form)
; RETURNS: the list of the Monomial's variables ordered in lexicographical order


(defun vars-of (monomial)
  (cond ((null monomial) nil)
        ((is-trad-poly monomial)
         (vars-of (as-monomial monomial)))
        ((is-monomial monomial)
         (find-variables (varpowers monomial)))
        (T (error "Input vars-of invalid." monomial))))

(defun find-variables (vps)
  (cond ((null vps) nil)
        ((mapcar 'is-varpower vps)
         (cons (third (first vps)) (find-variables (cdr vps))))
        (T (error "Input find-variables invalid." vps))))


; monomial-degree (Monomial)
; INPUT: a Monomial object (traditional form; parsed form)
; RETURNS: the Monomial's degree

(defun monomial-degree (monomial)
  (cond ((null monomial ) nil)
        ((is-trad-poly monomial)
         (monomial-degree (as-monomial monomial)))
        ((is-monomial monomial)
         (third monomial))
        (T (error "Input monomial-degree invalid" monomial))))

; monomial-coefficient (Monomial)
; INPUT: a Monomial object (traditional form; parsed form)
; RETURNS: the Monomial's coefficient

(defun monomial-coefficient (monomial)
  (cond ((null monomial ) nil)
        ((is-trad-poly monomial)
         (monomial-coefficient (as-monomial monomial)))
        ((is-monomial monomial)
         (second monomial))
        (T (error "Input monomial-coefficient invalid" monomial))))


; coefficients (Poly)
; INPUT: a Monomial or Polynomial object (traditional form; parsed form)
; RETURNS: the order-sensitive list of the Polynomial's coefficients 

(defun coefficients (poly)
  (cond ((null poly) nil)
        ((is-trad-poly poly)
         (coefficients (as-polynomial poly)))
        ((is-monomial poly)
         (monomial-coefficient poly))
        ((is-polynomial poly)
         (if (null (second poly)) nil
           (let ((polySort (merge-sort (second poly))))
             (let ((polyNor (normalize-poly polySort)))
               (mapcar 'monomial-coefficient polyNor)))))
        (T (error "Input coefficients invalid." poly))))

; variables (Poly)
; INPUT: a Monomial or Polynomial object (traditional form; parsed form)
; RETURNS: the list of unique symbols of the Polynomial in lexicographical order

(defun variables (poly)
  (cond ((null poly) nil)
        ((is-trad-poly poly)
         (variables (as-polynomial poly)))
        ((is-monomial poly)
         (vars-of poly))
        ((is-polynomial poly)
         (if (null (second poly)) nil
           (let ((monsSort (merge-sort (second poly))))
             (let ((monsNor (normalize-poly monsSort)))
               (let ((listVar 
                      (accumula (mapcar 'vars-of monsNor) nil 'append)))
                 (let ((vars (remove-duplicates listVar)))
                   (sort vars #'string-lessp)))))))
        (T (error "Input variables invalid." poly))))



; monomials (Poly)
; INPUT: a Monomial or Polynomial object (traditional form; parsed form)
; RETURNS: the order-sensitive list of the Polynomial's monomials 

(defun monomials (poly)
  (cond ((null poly) nil)
        ((is-trad-poly poly)
         (monomials (as-polynomial poly)))
        ((is-monomial poly) poly)
        ((is-polynomial poly)
         (if (null (second poly)) nil
           (let ((monsSort (merge-sort (second poly))))
             (normalize-poly monsSort))))
        (T (error "Input monomials invalid." poly))))


; maxdegree (Poly)
; INPUT: a Monomial or Polynomial object (traditional form; parsed form)
; RETURNS: the maximum degree amongst the Polynomial's monomials

(defun maxdegree (poly)
  (cond ((null poly) nil)
        ((is-trad-poly poly)
         (maxdegree (as-polynomial poly)))
        ((is-monomial poly) 
         (monomial-degree poly))
        ((is-polynomial poly)
         (let ((mons (monomials poly)))
           (let ((degrees (mapcar 'maxdegree mons)))
             (let ((degrSort (sort degrees #'<)))
               (first (last degrSort))))))
        (T (error "Input maxdegree invalid." poly))))


; mindegree (Poly)
; INPUT: a Monomial or Polynomial object (traditional form; parsed form)
; RETURNS: the minimum degree amongst the Polynomial's monomials

(defun mindegree (poly)
  (cond ((null poly) nil)
        ((is-trad-poly poly)
         (mindegree (as-polynomial poly)))
        ((is-monomial poly) 
         (monomial-degree poly))
        ((is-polynomial poly)
         (let ((mons (monomials poly)))
           (let ((degrees (mapcar 'mindegree mons)))
             (let ((degrSort (sort degrees #'<)))
               (first degrSort)))))
        (T (error "Input mindegree invalid." poly))))
         


; polyplus (Poly1 Poly2)
; INPUT: two objects of the type Monomial or Polynomial (traditional form; parsed form)
; RETURNS: the result of the sum between the two polynomials

(defun polyplus (poly1 poly2)
  (cond ((and (null poly1) 
              (null poly2)) (list 'poly (list (list 'm 0 0 nil))))
        ((and (null poly1)
              (not (null poly2))) 
         (cond ((is-monomial poly2)
                (polyplus (list 'm 0 0 nil) poly2))
               ((is-polynomial poly2)
                (polyplus (list 'poly (list (list 'm 0 0 nil))) poly2))
               ((is-trad-poly poly2)
                (polyplus (list 0) poly2))))
        ((and (not (null poly1))
              (null poly2)) 
         (cond ((is-monomial poly1)
                (polyplus poly1 (list 'm 0 0 nil)))
               ((is-polynomial poly1)
                (polyplus poly1 (list 'poly (list (list 'm 0 0 nil)))))
               ((is-trad-poly poly1)
                (polyplus poly1 (list 0)))))
        ; termine gestione casi con input nullo
        ((and (is-trad-poly poly1)
              (is-trad-poly poly2))
         (polyplus (as-polynomial poly1) (as-polynomial poly2)))
        ((and (is-monomial poly1)
              (is-monomial poly2))
         (let ((x1 (list 'poly (list poly1)))
               (x2 (list 'poly (list poly2))))
           (polyplus x1 x2)))
        ((and (is-polynomial poly1)
              (is-polynomial poly2))
         (let ((mons (append (second poly1) (second poly2))))
           (let ((monsSorted (merge-sort mons)))
             (let ((monsNorm (normalize-poly monsSorted)))
               (list 'poly monsNorm)))))
        (T (error "Input polyplus invalid" poly1))))


; polyminus (Poly1 Poly2)
; INPUT: two objects of the type Monomial or Polynomial (traditional form; parsed form)
; RETURNS: the result of the subtraction between the two polynomials

(defun polyminus (poly1 poly2)
  (cond ((and (null poly1) 
              (null poly2)) (list 'poly (list (list 'm 0 0 nil))))
        ((and (null poly1)
              (not (null poly2))) 
         (cond ((is-monomial poly2)
                (polyminus (list 'm 0 0 nil) poly2))
               ((is-polynomial poly2)
                (polyminus (list 'poly (list (list 'm 0 0 nil))) poly2))
               ((is-trad-poly poly2)
                (polyminus (list 0) poly2))))
        ((and (not (null poly1))
              (null poly2)) 
         (cond ((is-monomial poly1)
                (polyminus poly1 (list 'm 0 0 nil)))
               ((is-polynomial poly1)
                (polyminus poly1 (list 'poly (list (list 'm 0 0 nil)))))
               ((is-trad-poly poly1)
                (polyminus poly1 (list 0)))))
        ; termine gestione casi con input nullo
        ((and (is-trad-poly poly1)
              (is-trad-poly poly2))
         (polyminus (as-polynomial poly1) (as-polynomial poly2)))
        ((and (is-monomial poly1)
              (is-monomial poly2))
         (let ((x1 (list 'poly (list poly1)))
               (x2 (list 'poly (list poly2))))
           (polyminus x1 x2)))
        ((and (is-polynomial poly1)
              (is-polynomial poly2))
         (let ((mons2inv (mapcar 'invert-coeff (second poly2))))
           (let ((mons (append (second poly1) mons2inv)))
             (let ((monsSorted (merge-sort mons)))
               (let ((monsNorm (normalize-poly monsSorted)))
                 (list 'poly monsNorm))))))
        (T (error "Input polyminus invalid"))))

(defun invert-coeff (mon)
  (let ((inv (- 0 (second mon))))
    (cons 'm (rplaca (cdr mon) inv))))


; polytimes (Poly1 Poly2)
; INPUT: two objects of the type Monomial or Polynomial (traditional form; parsed form)
; RETURNS: the result of the product between the two polynomials

(defun polytimes (poly1 poly2)
  (cond ((or (null poly1)
             (null poly2)) (list 'poly (list (list 'm 0 0 nil))))
        ; nil viene considerato zero, azzerando anche il prodotto finale
        ; termine gestione casi con input nullo
        ((and (is-trad-poly poly1)
              (is-trad-poly poly2))
         (polytimes (as-polynomial poly1) (as-polynomial poly2)))
        ((and (is-monomial poly1)
              (is-monomial poly2))
         (let ((x1 (list 'poly (list poly1)))
               (x2 (list 'poly (list poly2))))
           (polytimes x1 x2)))
        ((and (is-polynomial poly1)
              (is-polynomial poly2))
         (let ((mons (multiply-poly (second poly1) (second poly2))))
           (let ((monsSorted (merge-sort mons)))
             (let ((monsNorm (normalize-poly monsSorted)))
               (list 'poly monsNorm)))))
        (T (error "Input polytimes invalid."))))


(defun multiply-poly (mons1 mons2)
  (cond ((or (null mons1)
             (null mons2)) nil)
        (T (let ((mp1 (multiply-mons (first mons1) mons2))
                 (mp2 (multiply-poly (cdr mons1) mons2)))
             (append mp1 mp2)))))

(defun multiply-mons (mon1 mons2)
  (cond ((null mons2) nil)
        (T (let ((c1 (second mon1))
                 (c2 (second (first mons2)))
                 (td1 (third mon1))
                 (td2 (third (first mons2)))
                 (vp1 (fourth mon1))
                 (vp2 (fourth (first mons2))))
             (let ((cf (* c1 c2))
                   (tdf (+ td1 td2))
                   (vpf (append vp1 vp2)))
               (if (zerop cf) (multiply-mons mon1 (cdr mons2))
                 (let ((vpSort (sort vpf #'string-lessp :key #'third)))
                   (let ((vpNorm (normalize-mons vpSort)))
                     (cons (list 'm cf tdf vpNorm) 
                           (multiply-mons mon1 (cdr mons2)))))))))))
                 

; as-monomial (Expression) 
; INPUT: a Monomial object (traditional form)
; RETURNS: the parsed monomial

(defun as-monomial (mon)
  (cond ((null mon) (list 'm 0 0 nil))
        ((numberp mon) (list 'm mon 0 nil))
        ((atom mon) (list 'm 1 1 (list (list 'v 1 mon))))
        ((listp mon)
         (cond ((numberp (first mon)) (list 'm (first mon) 0 nil))
               ((and (eql 'expt (first mon))
                     (atom (second mon))
                     (integerp (third mon))) 
                (if  (zerop (third mon)) (list 'm 1 0 nil)
                  (list 'm 1 (third mon) 
                        (list (list 'v (third mon) (second mon))))))
               ((eql '* (first mon))
                (if (> 3 (list-length mon))
                    (error "Error. The operation requires two objects."
                           mon)
                  (let ((listmon (mapcar 'as-monomial (cdr mon))))
                    (let ((c (accumula (isolate listmon 'second) 1 '*))
                          (td (accumula (isolate listmon 'third) 0 '+))
                          (vps (remove
                                nil 
                                (accumula 
                                 (isolate listmon 'fourth) nil 'append))))
                      (if (zerop c) (list 'm 0 0 nil)
                        (let ((vpsSort (sort vps #'string-lessp :key #'third)))
                          (let ((vpsNor (normalize-mons vpsSort)))
                            (list 'm c td vpsNor))))))))
               ((atom (first mon)) (list 'm 1 1 (list 'v 1 (first mon))))
               (T (error "Input as-monomial invalid" mon))))
        (T (error "Input as-monomial invalid" mon))))


(defun isolate (list position)
  (if (null list) nil
    (cons (funcall position (first list)) (isolate (cdr list) position))))
                
                     
(defun normalize-mons (list) 
  (cond ((null list) nil)
        ((eql 1 (list-length list)) list)
        (T (let ((vs1 (first list))
                 (vs2 (second list)))
             (let ((p1 (second vs1))
                   (t1 (third vs1))
                   (p2 (second vs2))
                   (t2 (third vs2)))
               (cond ((not(eql t1 t2)) 
                      (cons (first list)
                            (normalize-mons (cdr list))))
                     ((eql t1 t2) 
                      (let ((p (+ p1 p2)))
                        (if (eql 0 p) 
                            (normalize-mons (cddr list))
                          (normalize-mons 
                           (cons (list 'v p t1) (cddr list)))))))))))) 
                

; as-polynomial (Expression)
; INPUT: a Monomial or Polynomial object (traditional form)
; RETURNS: the parsed polynomial

(defun as-polynomial (x) 
  (cond ((or (null x) 
             (atom x)
             (numberp x)) (list 'poly (list (as-monomial x))))
        ((listp x) 
         (cond ((or (numberp (first x))
                    (eql 'expt (first x))
                    (eql '* (first x)))
                (list 'poly (list (as-monomial  x))))
               ((eql '+ (first x)) 
                (let ((mons (mapcar 'as-monomial (cdr x))))
                  (let ((monsSort (merge-sort mons)))
                    (let ((monsNor (normalize-poly monsSort)))
                      (list 'poly  monsNor)))))
               ((atom (first x)) (list 'poly (as-monomial x)))))))


(defun normalize-poly (list)
  (cond ((null list) nil)
        ((and (eql 1 (list-length list))
              (eql 0 (second (first list)))
              (not (eql 0 (third (first list))))
              (not (null (fourth (first list)))))
         (list 'm 0 0 nil))
        ((eql 1(list-length list)) list)
        ((eql 0 (second (first list)))
         (normalize-poly (cdr list)))
        (T (let ((vp1 (fourth (first list)))
                 (vp2 (fourth (second list)))
                 (td1 (third (first list)))
                 (td2 (third (second list)))
                 (c1 (second (first list)))
                 (c2 (second (second list))))
             (let ((vps1 (sort vp1 #'string-lessp :key #'third))
                   (vps2 (sort vp2 #'string-lessp :key #'third)))
               (let ((vpn1 (normalize-mons vps1))
                     (vpn2 (normalize-mons vps2)))
                 (cond ((not (equal vpn1 vpn2))
                        (cons (first list) (normalize-poly (rest list))))
                       ((and (equal vpn1 vpn2)
                             (eql td1 td2)
                             (eql 0 (+ c1 c2)))
                        (normalize-poly (cons (list 'm 0 0 nil) (cddr list))))
                       (T (let ((cf (+ c1 c2)))
                            (normalize-poly (cons
                                             (list 'm cf td1 vpn1)
                                             (cddr list))))))))))))
                        
        

; polyval (Poly Values)
; INPUT: a Monomial or Polynomial object (traditional form; parsed form) 
;        a List of integers for each variable in the polynomial
; RETURNS: the value of the polynomial computed in the n-dimentional point represented
;          by the list of values

(defun polyval (poly1 values)
  (cond ((and (null poly1) (null values)) 0)
        ((is-trad-poly poly1)
         (polyval (as-polynomial poly1) values))
        ((is-monomial poly1)
         (polyval (list 'poly (list poly1)) values))
        ((is-polynomial poly1)
         (let ((monsSorted (merge-sort (second poly1))))
           (let ((monsNorm (normalize-poly monsSorted)))
             (let ((vars (variables (list 'poly monsNorm))))
               (if (eql (list-length vars) (list-length values))
                   (evaluate-poly monsNorm vars values)
                 (error "Values do not coincide"))))))
        (T (error "Input polyval invalid" poly1))))

(defun evaluate-poly (mons vars values)
  (if (null mons) 0
    (let ((c (second (first mons)))
          (vps (fourth (first mons))))
      (let ((vm1 (* c (evaluate-varp vps vars values)))
            (vmr (evaluate-poly (cdr mons) vars values)))
        (+ vm1 vmr)))))

(defun evaluate-varp (vpowers vars values)
  (if (or (null vpowers) (null vars)) 1
    (let ((sym (third (first vpowers)))
          (pow (second (first vpowers))))
      (cond ((eql sym (first vars))
             (* (expt (first values) pow) 
                (evaluate-varp (cdr vpowers) (cdr vars) (cdr values))))
            ((not (eql sym (first vars))) 
             (evaluate-varp vpowers (cdr vars) (cdr values)))))))
           
        


; pprint-polynomial (Poly)
; INPUT: a Monomial or Polynomial object (traditional form; parsed form)
; RETURNS: the polynomial in human-readable format

(defun pprint-polynomial (poly)
  (cond ((null poly) (format t "~D " 0))
        ((is-trad-poly poly)
         (pprint-polynomial (as-polynomial poly)))
        ((is-monomial poly)
         (let ((c (second poly))
               (vps (fourth poly)))
           (cond ((eql 1 c)
                  (first (mapcar 'pprint-polynomial vps)))
                 ((< 0 c)
                  (and (not (format t "~A ~D " "+" c))
                       (first (mapcar 'pprint-polynomial vps))))
                 ((>= 0 c) 
                  (and (not (format t "~D " c))
                       (first (mapcar 'pprint-polynomial vps)))))))
        ((is-polynomial poly)
         (let ((mons (second poly)))
           (let ((monsSorted (merge-sort mons)))
             (let ((monsNorm (normalize-poly monsSorted)))
               (first (mapcar 'pprint-polynomial monsNorm))))))
        ((is-varpower poly)
         (let ((sym (third poly))
               (pow (second poly)))
           (if (eql 1 pow) (format t "~S " sym)
             (format t "~S^~D " sym pow))))))
       
        



; Merge Sort algorithm
              

(defun merge-sort (mons)
  (cond ((null mons) nil)
        ((eql 1 (list-length mons)) mons)
        (T (let ((M1 (first (split mons)))
                 (M2 (second (split mons))))
             (let ((Mr1 (merge-sort M1))
                   (Mr2 (merge-sort M2)))
               (merge-s Mr1 Mr2))))))


(defun merge-s (M1 M2)
  (cond ((null M1) M2)
        ((and (null M2)
              (not (null M1))) M1)
        ((comes-first (first M1) (first M2))
         (cons (first M1) (merge-s (rest M1) M2)))
        ((comes-first (first M2) (first M1))
         (cons (first M2) (merge-s M1 (rest M2))))))


(defun comes-first (m1 m2)
  (cond ((or (null m1)
             (null m2)) T)
        ((and (is-monomial m1)
              (is-monomial m2))
         (cond ((< (third m1) (third m2)) T)
               ((> (third m1) (third m2)) nil)
               ((eql (third m1) (third m2))
                (if  (equal (fourth m1) (fourth m2))
                    T
                  (comes-first (fourth m1) (fourth m2))))))
        ((and (every 'is-varpower m1)
              (every 'is-varpower m2))
         (cond ((string-lessp (third (first m1)) (third (first m2)))
                T)
               ((and (string-equal (third (first m1)) (third (first m2)))
                     (< (second (first m1)) (second (first m2))))
                T)
               ((equal (first m1) (first m2))
                     (comes-first (cdr m1) (cdr m2)))))
        (T nil)))


(defun split (mons)
  (cond ((null mons) '(()()))
        ((eql 1 (list-length mons)) (list (first mons) '()))
        (T  (let ((mid (floor (/ (list-length mons) 2)))) 
                  (list (subseq mons 0 mid)
                        (subseq mons mid)))))) 
  
         

; Other functions for support

(defun accumula (list init function)
  (if (null list) init
    (funcall function (first list) (accumula (cdr list) init function))))



(defun is-trad-poly (poly)
  (cond ((null poly) nil)
        ((numberp poly) T)
        ((listp poly)
         (cond ((numberp (first poly)) T)
               ((and (eql 'expt (first poly))
                     (atom (second poly))
                     (integerp (third poly))) T)
               ((eql '* (first poly))
                (and (is-trad-poly (second poly))
                     (is-trad-poly (cddr poly))) T)
               ((eql '+ (first poly))
                (and (is-trad-poly (second poly))
                     (is-trad-poly (cddr poly))) T)
               ((and (eql 1 (list-length poly))
                     (atom (first poly))) T)))
        (T nil)))


(defun is-polynomial (p)
  (and (listp p)
       (eq 'poly (first p))
       (let ((ms (second p)))
         (and (listp ms)
              (every #'is-monomial ms)))))


(defun is-monomial (m)
  (and (listp m)
       (eq 'm (first m))
       (let ((coe (second m))
             (mtd (third m))
             (vps (fourth m))
             )
         (and (integerp mtd)
              (numberp coe)
              (>= mtd 0)
              (listp vps)
              (every #'is-varpower vps)))))


(defun is-varpower (vp)
  (and (listp vp)
       (eq 'v (first vp))
       (let ((p (second vp))
             (v (third vp))
             )
         (and (integerp p)
              (>= p 0)
              (symbolp v)))))