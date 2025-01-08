#lang racket
(require "interpreter.rkt")
(require rebellion/concurrency/lock)

(define (concatenate-programs p-filename q-filename p-args)
   (brainfuck q-filename (brainfuck p-filename p-args))
  )

(define (set-list-element lst index value)
  (cond
    [(null? lst) (error "Index out of bounds")]
    [(= index 0) (cons value (cdr lst))]
    [else (cons (car lst) (set-list-element (cdr lst) (- index 1) value))]))


(define (alternate-programs p-filename q-filename input)
  (define (alternate-programs-recurse p-args q-args input output is-p-turn)
    (let*
          
          (
           [p-program-args (set-list-element p-args 1 input)]
           [q-program-args (set-list-element q-args 1 input)]
           [p-program-args (set-list-element p-program-args 5 '())]
           [q-program-args (set-list-element q-program-args 5 '())]
           [p-program-ip (cadddr p-program-args)]
           [q-program-ip (cadddr q-program-args)]
           [p-program-len (caddr (cddddr p-program-args))]
           [q-program-len (caddr (cddddr q-program-args))]
           )
          
      (cond
        [(and (>= p-program-ip p-program-len) (>= q-program-ip q-program-len)) output]
        [(or (>= p-program-ip p-program-len) (not is-p-turn)) (let*
                                             (
                                              (executed-program (apply brainfuck-state-update-after-input-read q-program-args))
                                              (new-input (cadr executed-program))
                                              (new-output (cadr (cddddr executed-program)))
                                              )
                                              (alternate-programs-recurse p-program-args executed-program new-input (append output (map integer->char (reverse new-output))) (if (>= p-program-ip p-program-len)
                                                                                                                                                                                 #f
                                                                                                                                                                                 (not is-p-turn))
                                                                          ))]
        [else (let*
                  (
                   (executed-program (apply brainfuck-state-update-after-input-read p-program-args))
                   (new-input (cadr executed-program))
                   (new-output (cadr (cddddr executed-program)))
                   )
                                           
                (alternate-programs-recurse executed-program q-program-args new-input (append output (map integer->char (reverse new-output))) (if (>= q-program-ip q-program-len)
                                                                                                                                                                                 #t
                                                                                                                                                                                 (not is-p-turn))
                                            ))]
        )
    ))
  
  (let
      (
       [p-code (file->string p-filename)]
       [q-code (file->string q-filename)]
       )
    (alternate-programs-recurse (brainfuck-state-build p-code input) (brainfuck-state-build q-code input) input '() #t)
  ))

(define (parallel-programs p-filename q-filename input)
  (define sync-lock (make-lock))
  (define common-output '())
    
  (define (write-shared-output value)
  (lock! sync-lock
    (lambda () (set! common-output (append common-output value)))))
  
  (define (brainfuck-thread thread-state)
    (if (>= (list-ref thread-state 3) (list-ref thread-state 6))
        #f
        (let*
            ([synced-state (set-list-element thread-state 5 '())]
             [executed-program (apply brainfuck-state-update-after-output-read synced-state)]
             )
          (begin
            (write-shared-output (map integer->char (reverse (list-ref executed-program 5))))
            (brainfuck-thread executed-program)
            )
          )
    ))
  (let*
      (
       [p-code (file->string p-filename)]
       [q-code (file->string q-filename)]
       [p-state (brainfuck-state-build p-code input)]
       [q-state (brainfuck-state-build q-code input)]
       [thread1 (thread (lambda () (brainfuck-thread p-state)))]
       [thread2 (thread (lambda () (brainfuck-thread q-state)))]
       )
    (begin
     (thread-wait thread1)
     (thread-wait thread2)
     common-output
  )))

