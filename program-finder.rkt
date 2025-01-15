#lang racket
(require racket/stream)
(require "interpreter.rkt")
(require rebellion/concurrency/lock)


(define (concatmap func lst)
  (apply append (map func lst)
  )
  )

(define (all? pred? lst)
  (cond
    [(null? lst) #t]
    [(not (pred? (car lst))) #f]
    [else
     (all? (cdr pred? lst))
     ]
    ))

(define valid-symbols '("." "," ">" "<" "+" "-"))
; [ and ] removed for more programs generation

(define (generate-next-level lst)
  (concatmap  (lambda (current-string)
                (map (lambda (char) (string-append current-string char)) valid-symbols)
                
              )
  lst))


(define (generate-solution-levels-stream)
  (define (iter lst)
    (let
        ([level-result (generate-next-level lst)])
      (stream-cons level-result (iter level-result))
      )
  )
  (stream-cons '("") (stream-cons valid-symbols (iter valid-symbols)))
  )



(define (program-found threads)
    (define (iter key-val-pairs)
      (cond
        [(null? key-val-pairs) #f]
        [(all? (lambda (pair) (not (thread? (cdr pair)))) (cdr key-val-pairs)) (caar key-val-pairs)]
        [else
         (iter (cdr key-val-pairs))
         ]
        )
      )
    (iter (hash->list threads))
  )
           
      
(define (program-finder pairs)
  (define threads (make-hash)) ; code -> [(input, thread|result)]
  (define keys (map car pairs))
  (define sync-lock (make-lock))

  (define (add-thread key-hash-map input thread)
    (lock! sync-lock
           (lambda () (if (hash-has-key? threads key-hash-map)
               (hash-set! threads key-hash-map (cons (cons input thread) (hash-ref threads key-hash-map)))
               (hash-set! threads key-hash-map (cons (cons input thread) '()))
               ))
    ))

  (define (resolve-thread-in-hash key-hash-map input result)
    (define (recurse assoc)
      (cond
        [(null? assoc) '()]
        [(equal? (caar assoc) input) (cons (cons input result) (recurse (cdr assoc)))]
        [else
         (cons (car assoc) (recurse (cdr assoc)))
         ]
      ))
    
    (when (hash-has-key? threads key-hash-map)
      (lock! sync-lock
             (lambda ()
      (hash-set! threads key-hash-map (recurse (hash-ref threads key-hash-map)))
      ))
    ))
  

  (define (run-thread input code)
    (with-handlers
        (
         [exn:fail? (lambda (exception) (lock! sync-lock (lambda () (hash-remove! threads code))))]
         )
      
      (let
          ([result (apply brainfuck-decoder (brainfuck-state-build code input))])
      
        (if (not (equal? result (cadr (assoc input pairs))))
                                    
          (lock! sync-lock (lambda () (hash-remove threads code)))
          (resolve-thread-in-hash code input result)
          )
    )))
    
  
  (define (iter solution-stream)
    
    (define (inner-iter codes)
      (let
          ([program-result (program-found threads)])
        (cond
          [program-result program-result]
          [(null? codes) (iter (stream-rest solution-stream))]
          [else (begin
                  (for-each (lambda (key)
                              (let
                                  ([current-thread (thread (lambda () (run-thread key (car codes))))])
                                (begin
                                  (add-thread (car codes) key current-thread)
                                  (thread-wait current-thread)
                                  )
                                )) keys)
                            (inner-iter (cdr codes)))
                ]
          )
      ))
    (inner-iter (stream-first solution-stream))
    )

  (iter (generate-solution-levels-stream))
)


(provide program-finder)
