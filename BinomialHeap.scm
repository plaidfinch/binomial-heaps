; This is the reference implementation of a binomial heap I used as inspiration for the Haskell one. It comes from my advisor's lecture notes for Structure and Interpretation of Computer Programs.

(define (make-queue) '())

(define (tack x lst)
  (if (null? lst)
      (list x)
      (cons (car lst) (tack x (cdr lst)))))

(define (root e)
  (if (number? e)
      e
      (car e)))

(define (meld e1 e2)
;; for combining two equal-sized binomial trees
  (if (and (number? e1) (number? e2))
      (list (max e1 e2) (min e1 e2))
      (if (< (root e1) (root e2))
        (tack e1 e2)
        (tack e2 e1))))

(define (insert e q)
  (if (null? q)
      (list e)
      (if (null? (car q))
          (cons e (cdr q))
          (cons '() 
            (insert (meld e (car q)) 
                    (cdr q))))))

(define (merge q1 q2)
  (cond ((null? q1) q2)
        ((null? q2) q1)
        ((null? (car q1))
         (cons (car q2) 
               (merge (cdr q1) 
                      (cdr q2))))
        ((null? (car q2))
         (cons (car q1) 
               (merge (cdr q1) 
                      (cdr q2))))
        (else
         (cons '()
               (mergecarry (cdr q1)
                           (cdr q2)
                           (meld (car q1) (car q2)))))))

(define (mergecarry q1 q2 carry)
  (cond ((null? q1) (insert carry q2))
        ((null? q2) (insert carry q1))
        ((null? (car q1))
         (merge (cons carry (cdr q1)) q2))
        ((null? (car q2))
         (merge q1 (cons carry (cdr q2))))
        (else
         (cons carry
               (mergecarry (cdr q1) 
                           (cdr q2)
                           (meld (car q1) (car q2)))))))

(define (max-elt e)
  (if (null? e) 0 (if (number? e) e (car e))))

(define (find-max q)
  (apply max (map max-elt q)))

(define (slinkyleft leftlist rightlist)
  (if (null? rightlist)
      leftlist
      (slinkyleft 
        (cons (car rightlist) leftlist) 
        (cdr rightlist))))

(define (clean lst)
  (if (null? lst)
      '()
      (if (null? (car lst))
          (clean (cdr lst))
          lst)))

(define (cleanup q)
  (slinkyleft 
    '()
    (clean (slinkyleft '() q))))
    
(define (select q maxval)
; returns (tree with max root . rest of queue
;            with () in place of selected tree)
  (if (null? q)
      (error 
        "Cannot select from empty queue")
      (if (= maxval (max-elt (car q)))
          (cons (car q) 
                (cleanup 
                  (cons '() (cdr q))))
          (let ((v (select (cdr q) maxval)))
            (cons (car v) 
                  (cleanup 
                    (cons (car q)
                          (cdr v))))))))

(define (remove-max q)
  (let ((q (select q (find-max q))))
    (if (number? (car q))
        (cdr q)
        (merge (cdar q) (cdr q)))))

(define (removes q n)
  (if (= n 0)
      q
      (removes (remove-max q) (- n 1))))
