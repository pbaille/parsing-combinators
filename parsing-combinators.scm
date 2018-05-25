(load-relative "utils.scm")
(load-relative "km.scm")

(define (parser . opts)
  (km 'parser? #t (apply km opts)))

(define (parser? x)
  (and (km? x)
       (kmget x 'parser?)))

;; accessessors -------

(define (parser-id x)
  (and (parser? x)
       (kmget x 'parser-id)))

(define (parser-id= p id)
  (equal? id (parser-id p)))

(define (get-pparam p k)
  (kmget> p 'params k))

;; utils -------------

(define (dop p x match fail)
  ((kmget p 'parser-impl) x match fail))

(define (testp p . xs)
  (map (λ (x) (dop p x identity (λ _ 'fail)))
       xs))

;; syntax ------------

(define-syntax λp
  (syntax-rules ()
    ((_ . form)
     (parser 'parser-impl (λ . form)))))

(define-syntax parametric-parser
  (er-macro-transformer
   (lambda (exp _ _)
     (let* ((all (cdr exp))
            (form (car all))
            (id (car form))
            (body (cadr all))
            (opts (cddr all))
            (args (cdr form))
            (argsyms (cdr (proper! form))))
       `(lambda ,args
          (km ,body
              'params
              (km ,@(interleave (map (lambda (x) `(quote ,x)) argsyms) argsyms))
              'parser-id ',id
              ,@opts))))))

(define-syntax defparser
  (er-macro-transformer
   (lambda (exp _ _)
     (let* ((form (cadr exp))
            (parametric? (pair? form))
            (id (if parametric? (car form) form))
            (predsym
             (string->symbol
              (string-append (symbol->string id) "?"))))
       `(begin
          (define (,predsym p) (parser-id= p ',id))
          ,(if parametric?
              `(define ,id (parametric-parser ,@(cdr exp)))
              `(define ,id (km ,(caddr exp) 'parser-id ',id ,@(cdddr exp)))))))))

(comment
 "another way"

 (define (mk-pparser id args body opts)
   (let ((argsyms (proper! args)))
     `(lambda ,args
        (km ,body
            'params
            (km ,@(interleave (map (lambda (x) `(quote ,x)) argsyms) argsyms))
            'parser-id ,id
            ,@opts))))

 (define-syntax pparser
   (syntax-rules ()
     ((_ (id . args) body . opts)
      (mk-pparser id args body opts))))

 ;; doesn't work
 (pparser
  (mapp fn parser)
  (λp (x match fail)
      (dop parser x
           (compose fn match)
           fail))
  'foo 'bar))

;; primitives --------

(defparser idp
  (λp (x match _) (match x)))

(defparser neverp
  (λp (x _ fail) (fail x)))

(defparser (mapp fn parser)
  (λp (x match fail)
      (dop parser x
           (compose fn match)
           fail)))

(defparser (constp value)
  (λp (x match fail)
    (if (equal? x value)
        (match x)
        (fail x))))

(defparser (predp pred)
  (λp (x match fail)
    (if (pred x)
        (match x)
        (fail x))))

(begin 
       (define pairp (predp pair?))
       (define nump (predp number?))
       (define symp (predp symbol?))
       (define stringp (predp string?)))

(defparser (orp . ps)
  (if (null? ps)
      neverp
      (λp (x match fail)
          (dop (car ps) x
               match
               (λ (_)
                 (dop (apply orp (cdr ps))
                      x match fail))))))

(defparser (andp . ps)
  (if (null? ps)
      idp
      (λp (x match fail)
        (dop (car ps) x
             (λ (_)
               (dop (apply andp (cdr ps))
                    x match fail))
         fail))))

(defparser (consp p1 p2)
  (andp
   pairp
   (λp (x match fail)
       (dop p1 (car x)
            (λ (m)
              (dop p2 (cdr x)
                  (λ (n) (match (cons m n)))
                  fail))
            fail))))

(defparser nilp (constp '()))

(defparser (ifp p1 p2 p3)
  (λp (x match fail)
      (dop p1 x
           (λ _ (dop p2 x match fail))
           (λ _ (dop p3 x match fail)))))

;; others ----------

(define (carp p)
  (and (parser-id= p 'consp)
       (get-pparam p 'p1)))

(define (cdrp p)
  (and (parser-id= p 'consp)
       (get-pparam p 'p2)))

(define (listp . ps)
  (if (null? ps)
      nilp
      (consp (car ps) (apply listp (cdr ps)))))

(define (cons.p . ps)
  (cond
   ((null? ps) nilp)
   ((null? (cdr ps)) (car ps))
   (else (consp (car ps) (apply cons.p (cdr ps))))))

(define (catp2 l1 l2)
  (cond
   ((nilp? l1) l2)
   ((nilp? l2) l1)
   (else
    (consp (carp l1) (catp2 (cdrp l1) l2)))))

(define (catp . ps)
  (reduce catp2 ps))

(define (->p . xs)
  (if (null? xs)
      idp
      (λp (x match fail)
          (dop (car xs) x
               (λ (y) (dop (apply ->p (cdr xs)) y match fail))
               fail))))

(define-syntax condp
  (syntax-rules ()
    ((_) neverp)
    ((_ b1 . bs)
     (orp (andp . b1) (condp . bs)))))

(defparser (rep p n)
  (cond
   ((zero? n) idp)
   ((positive? n)
    (consp p (rep p (- n 1))))))

(begin

  (testp (constp "42") "42")

  (testp nump 'a 1)

  (define gt10p (predp (λ (x) (> x 10))))
  (define forty2p (constp 42))
  (testp forty2p 41 42)

  (define forty2p_
    (mapp (λ _ 'forty-two) forty2p))
  (testp forty2p_ 41 42)

  (testp (andp nump gt10p) 1 10 34 67)

  (testp (orp nump (predp string?))
         1 "aze" 'zer)

  (testp (consp forty2p_ gt10p)
         (cons 42 11) (cons 42 1))

  (testp (->p nump forty2p_ (predp symbol?))
         42)

  (testp
   (condp
    (nump forty2p_)
    (nump gt10p)
    ((predp string?) (constp "42") (λp _ "mtf42!!!")))
   42 11 2 "23" "42")

  (testp
   (cons.p
    nump
    nump
    (predp pair?))
   '(1 2 3))

  (testp
   (ifp nump
        forty2p_
        (->p (predp symbol?) (constp 'foo)))
   'foo 42 'a 1)

  (testp nilp '())
  (testp (listp) '())
  (testp (listp (predp number?)) '(1))
  (testp (listp (predp number?) (predp number?)) '(1 2))

  (testp
   (listp (predp number?) (predp symbol?))
   '(1 foo) '(42 35) '(aze 1) '(qsd dfg))

  (testp
   (condp
    (nump forty2p_)
    (nump gt10p)
    ((predp string?) (constp "42")))
   42 10 11 "42" 'aze)

  (testp
   (catp
    (listp nump nump)
    (listp gt10p gt10p)
    (listp symp))
   '(1 2 11 12 aze))

  (comment
   (testp (rep nump 3)
          '(1 2 'aze 4 5)))
  )







