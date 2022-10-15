#lang racket

(require 2htdp/batch-io)
(define regex-eq #rx"^:=")
(define regex-addop #rx"^[+-]")
(define regex-mulop #rx"^[*/]")
(define regex-keyword #rx"^read|write")
(define regex-id #rx"^[A-Za-z](?>[A-Za-z]|[0-9])*")
(define regex-number #rx"^[0-9]+")
(define regex-end #rx"$?")



(define (scan input)
  
  (define listofchar '())
  (define each "")
  (define token "")
  (define chartostring "")
  (set! listofchar (string->list input))
  (let looping ([i 0])
    (set! token "")
    

    (define (create_token li)
      (when (> i 0)
        (for ([b i]
              #:break (equal? b i))
          (set! li (remove (list-ref li 0) li))))
      (for ([a li]
            #:break (or (equal? a #\space) (equal? a #\$) (equal? a #\() (equal? a #\))))
        
        (set! each a)
        (set! chartostring (make-string 1 each)) 
        (set! token (string-append token chartostring))
        (if  (equal? (list-ref listofchar (add1 i)) #\))
            (set! i i)
            (set! i (add1 i)))
        
        )
      token)

    (cond[(equal? (list-ref listofchar i) #\$) (set! token "$")]
         [(equal? (list-ref listofchar i) #\() (set! token "(")]
         [(equal? (list-ref listofchar i) #\)) (set! token ")")]
         
         [else (create_token listofchar)])
    
    (define (check regex)
      (let ([v (regexp-match regex token)])
        (if (equal? v #f)  
            (set! v #f)
            (set! v v))
        v))
    (define (check-kw regex)
      (let ([v (regexp-match regex token)])
        (if (equal? v #f)
            (set! v #f)
            (if (equal? (list-ref v 0) token)
                (set! v v)
                (set! v #f)))
        v))

    (define (assign type)
          (if (equal? type 'number)
              (cons type (string->number token))
              (cons type token)))

    
    (let ([tokenize
             (cond
                  [(check-kw regex-keyword) 
                      (assign 'keyword)]
                  [(check regex-id) 
                      (assign 'id)]
                  [(check regex-addop) 
                      (assign 'addop)]
                  [(check regex-mulop) 
                      (assign 'mulop)]
                  [(check regex-number) 
                      (assign 'number)]
                  [(check regex-eq) 
                      (assign 'eq)]
                  [(equal? #\( (list-ref listofchar i))
                   (when (equal? (list-ref listofchar (add1 i)) #\space)
                     (set! i (add1 i)))
                   (cons 'openpar "(")]
                  [(and (equal? #\) (list-ref listofchar i)) (equal? #\) (list-ref listofchar (add1 i))))
                   
                   (cons 'closepar ")")]

                  [(and (equal? #\) (list-ref listofchar i)))
                   (set! i (add1 i))
                   (cons 'closepar ")")]
                  
                  [(equal? #\$ (list-ref listofchar i))
                   '()]
                  
                   
                           
                  
                  [else #f])])


      
      
      (cond [(equal? tokenize '()) '()]
            
            [(equal? (add1 i)  (length listofchar))
             tokenize (cons tokenize (looping i))]
            [tokenize (cons tokenize (looping (add1 i)))]
               
            
            [else (error (format "Lexer error at ~a." i))]))))


(define tlist '())
(define tval "")
(define tsym "")
(define e "")
(define ex '())
(define a "")
(define ind 0)



(define (comparesymbol type)
  (if (equal? tsym type)
      #t
      #f))


(define (lookahead)
  
  (set! ind (add1 ind))
  (unless (null? tlist)
      (set! tval (cdar tlist))
      (set! tsym (caar tlist))
      (set! tlist (cdr tlist))))

(define (stmt)
  (let ([i tval])
    (if (equal? tval "read")
      (begin
        (lookahead)
        (if (equal? (comparesymbol 'id) #t)
            (cons i tval) 
            (error "syntax error:" tval " is not an identifier"))
        )
      (if (equal? tval "write")
        (cons tval (expr))
        
        (if (equal? (comparesymbol 'id) #t)
          (begin
            
            (set! a tval)
            (lookahead)
            (if (equal? tval ":=")
                (cons a (cons tval (expr)))
                (error "syntax error: ~a not an assign character" tval)))
          (when (equal? tval ")")
            (error (format "syntax error: unbalancing parentheses at character ~a " ind))))))))
    
  

(define (stmt_list)
  (lookahead)
  (if (null? tlist)
  '()
  (cons(stmt) (stmt_list))))
  


(define (expr)
  (cons (term) (term_tail)))

(define (term_tail)
  (define checkvalid "")
  (define checkvalid2 "")
  (when (not(null? tlist))
    (set! checkvalid (caar tlist))
    (set! checkvalid2 (cdar tlist)))
  (if (or (equal? tval "+") (equal? tval "-"))
      (if (or (equal? checkvalid 'id)(equal? checkvalid 'number)(equal? checkvalid 'openpar))
          (cons tval (cons (term) (term_tail)))
          (error (format "syntax error: unexpected character at character ~a " ind)))
      '()))

(define (term)
  (cons (factor) (factor_tail)))

(define (factor)
  (lookahead)
  (let ([i tsym])
    (if (equal? tval "(")
        (begin
          (set! e tval)
          (set! ex (expr))
          (if (equal? tval ")")
                (cons e (cons ex tval)) 
                ((error (format "syntax error: unexpected character at character ~a " ind)))))
        (if (equal? (comparesymbol 'number) #t)
          tval
          (when (equal? (comparesymbol 'id) #t)
            tval)))))

(define (factor_tail)
  (lookahead)
  (define checkvalid "")
  (define checkvalid2 "")
  (when (not(null? tlist))
    (set! checkvalid (caar tlist))
    (set! checkvalid2 (cdar tlist)))
  (if (or (equal? tval "*") (equal? tval "/"))
      (if (or (equal? checkvalid 'id)(equal? checkvalid 'number)(equal? checkvalid 'openpar))
          (cons tval (cons (factor) (factor_tail)))
          (error (format "syntax error: unexpected character at character ~a " ind)))
      '()))
  

(define (addop)
  (if (equal? tval "+")
      tval
      (when (equal? tval "-")
      tval)))

(define (mulop)
  (if (equal? tval "*")
      tval
      (when (equal? tval "/")
      tval)))



(define (program)
      (cons(stmt_list) "$$"))
      
(define (space character i lst)
    (cond
    [(null? lst) character]
    [(zero? i) (cons character lst)]
    [else (cons (car lst) (space character (- i 1) (cdr lst)))]))
(define result '())


(define (check$ listchar)
  (define count 0)
  (for ([b (sub1 (length listchar))] #:break (equal? b (sub1 (length listchar))))
      (when (equal?(list-ref listchar b) #\$)
        (set! count (add1 count))))
  (if (> count 0)
      #t
      #f))

(define (parse input)
  (define getfile (read-file input))
  (set! getfile (string->list getfile))
  (for ([b (sub1 (length getfile))] #:break (equal? b (sub1 (length getfile))))
      (when (and (equal?(list-ref getfile b) #\newline) (not(equal? (list-ref getfile (sub1 b)) #\space) ))
        (set! getfile (space #\space b getfile))))
  (set! getfile (remove* (list #\newline) getfile))
  
  
  (if (equal? (check$ getfile) #t)
      (begin
      (set! getfile (list->string getfile))
      (set! tlist (scan getfile))
      (set! result (program))
      (if (equal? tlist '())
        (begin
          (println "Accept. Successfully Parsing!!")
          result)
        (error "Syntax error.")))
      (error "Input string is lack of end-of-string sign:$$")))

   