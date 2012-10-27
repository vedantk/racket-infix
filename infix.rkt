#lang racket

(define (infix-eval str)   
  (define (setup-stacks tokens numstack opstack)
    (if (empty? tokens)
        (values numstack opstack)
        (let ([cur-tok (car tokens)])
          (if (number? cur-tok)
              ;; Push numbers onto the numstack.
              (setup-stacks (cdr tokens)
                            (cons cur-tok numstack)
                            opstack)
              
              ;; Consider non-numeric tokens as operators.
              (if (and (>= (length numstack) 2)
                       (or (empty? opstack)
                           (> (operator-priority (car opstack))
                              (operator-priority cur-tok))))
                  (setup-stacks (cdr tokens)
                                ;; The new numstack uses an old operator.
                                (compute! numstack opstack)
                                (cons cur-tok (cdr opstack)))
                  ;; At this point, just keep the new operator.
                  (setup-stacks (cdr tokens) numstack (cons cur-tok opstack)))))))

  (define (process-stacks numstack opstack)
    (if (empty? opstack)
        (car numstack)
        (process-stacks (compute! numstack opstack) (cdr opstack))))

  (define (compute! numstack opstack)
    (cons 
     (apply (eval (car opstack))
            (list (cadr numstack) (car numstack)))
     (cddr numstack)))    
  
  (define-values (numstack opstack)
    (setup-stacks (infix->tokens str) '() '()))
  (process-stacks numstack opstack))
  
(define (infix->tokens str)
  (define (iter substr tokens)
    (if (= (string-length substr) 0)
      tokens
      (let ([cur-char (string-ref substr 0)])
        (cond
          [(operator? cur-char)
           (iter (substring substr 1)
                 (cons (char->symbol cur-char) tokens))]
          
          [(char-numeric? cur-char)
           ;; Extract the string containing the number...
           (let* ([end-pos (str-scan substr char-numeric?)]
                  [numeric-str (substring substr 0 end-pos)])
             (iter (substring substr end-pos)
                   (cons (string->number numeric-str) tokens)))]
          
          ;; Ignore whitespace and strange characters for now.
          [else (iter (substring substr 1) tokens)]))))

  (iter str '()))

(define (operator? char)
  (member char (list #\+ #\- #\* #\/)))

(define (char->symbol char)
  (string->symbol (string char)))

(define (str-scan str predicate)
  ;; Scan for the position in the string where the predicate fails.
  (define (iter k)
    (cond
      [(>= k (string-length str)) k]
      [(predicate (string-ref str k)) (iter (+ k 1))]
      [else k]))

  (iter 0))

(define (operator-priority op)
  (if (member op '(+ -)) 1 2))