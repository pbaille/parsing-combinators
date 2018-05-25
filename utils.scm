;; syntax ------------------------------------------------------------------------

(define-syntax comment
  (lambda _ `(begin '())))

(define-syntax λ
  (syntax-rules ()
    ((_ args . body)
     (lambda args . body))))

;; utils --------------------------------------------------------------------------

(define (compose f g)
  (λ args (g (apply f args))))

(define (cdrr x)
  (if (pair? x)
      (cdrr (cdr x))
      x))

(define (proper! x)
  (cond
   ((proper-list? x) x)
   ((pair? x) (cons (car x) (proper! (cdr x))))
   (else (list x))))

(define (args-rest x)
  (and (not (proper-list? x))
       (cdrr x)))

(define (take xs n)
  (if (or (null? xs)
          (zero? n))
      '()
      (cons (car xs)
            (take (cdr xs) (- n 1)))))

(define reduce
  (case-lambda
    ((f xs)
     (reduce f (car xs) (cdr xs)))
    ((f init xs)
     (or (and (null? xs) init)
         (reduce f (f init (car xs)) (cdr xs))))))

(define (drop xs n)
  (if (or (zero? n) (null? xs))
      xs
      (drop (cdr xs) (- n 1))))

(define (min-len xs n)
  (>= (length xs) n))

(define (take! xs n)
  (and (min-len xs n)
       (take xs n)))

(define (partition xs n)
  (if (null? xs) xs
      (cons (take xs n)
            (partition (drop xs n) n))))

(define (interleave xs ys)
  (if (or (null? xs) (null? ys))
      '()
      (cons (car xs)
            (cons (car ys)
                  (interleave (cdr xs) (cdr ys))))))

(begin
  (interleave '(1 2 3 4 5) '(1 2 3 4)))
