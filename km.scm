(load-relative "utils.scm")

(define (kmentry? x)
  (and (pair? x)
       (symbol? (car x))))

(define (km? x)
  (or (null? x)
      (and (kmentry? (car x))
           (km? (cdr x)))))

(define (kmjoin a b)
  (cond
   ((null? a) b)
   ((null? b) a)
   ((km? b)
    (let ((fst (car b)))
      (kmjoin
       (alist-update (car fst) (cdr fst) a)
       (cdr b))))))

(define (km . xs)
  (cond
   ((null? xs) '())
   ((symbol? (car xs))
    (cons (cons (car xs) (cadr xs))
          (apply km (cddr xs))))
   ((km? (car xs))
    (kmjoin (car xs) (apply km (cdr xs))))))

(define (kmget target req)
  (cond

   ((null? req) target)

   ((symbol? req)
    (let ((e (assq req target)))
      (and e (cdr e))))

   ((km? req)
    (map (λ (x)
           (cons (car x)
                 ((cdr x)
                  (kmget target (car x)))))
         req))

   ((and (pair? req)
         (symbol? (car req)))
    (if (null? (cdr req))
        (km (car req)
            (kmget target (car req)))
        (km (car req)
            (kmget target (car req))
            (kmget target (cdr req)))))))

(define (kmget> target . reqs)
  (reduce kmget target reqs))

(begin
  (km? (km 'a 1))
  (km? (km 'a 1 'b 2))
  (km? (km 'c 5 (km 'a 1 'b 2)))
  (let ((k (km 'a 1 'b 2 'c 3)))
   (list
    (kmget k 'a)
    (kmget k '())
    (kmget k (km 'a identity 'b (λ (x) (+ x 1))))
    (kmget k '(a b))
    )))
