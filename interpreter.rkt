#lang racket

;DISCLAIMER
; I use vector and hashmap because I need O(1) search because the tape is too long for linear search (instead of using list and assoc)

(define (brainfuck-decoder code input tape ip dp output code-len brackets-hash)
  (if (>= ip code-len)
        (map integer->char (reverse output))
        (apply brainfuck-decoder (brainfuck-state-update-after-input-read code input tape ip dp output code-len brackets-hash))
        )
  )

(define (brainfuck filename input)
  (apply brainfuck-decoder (brainfuck-state-build (file->string filename) input))
  )
 

(define TAPE-MAX-LENGTH 30000)

(define (brainfuck-state-build code input)
  (let
      ([brackets-hash (get-brackets-hash code)])
    (if brackets-hash
        (list
         code
         input
         (make-vector TAPE-MAX-LENGTH 0)
         0
         0
         '()
         (string-length code)
         (get-brackets-hash code)
         )
        (error "Not valid brackets")
        )))

(define (brainfuck-state-update-after-input-read code input tape ip dp output code-len brackets-hash)
  (append (list code) (read-brainfuck-until code input tape ip dp output code-len brackets-hash #t) (list code-len brackets-hash))
  )

(define (brainfuck-state-update-after-output-read code input tape ip dp output code-len brackets-hash)
  (append (list code) (read-brainfuck-until code input tape ip dp output code-len brackets-hash #f ) (list code-len brackets-hash))
  )

(define (tape-increase-at-position tape pos)
  (begin
    (vector-set! tape pos (+ 1 (vector-ref tape pos)))
    tape
    )
  )

(define (tape-decrease-at-position tape pos)
  (begin
    (vector-set! tape pos (- (vector-ref tape pos) 1))
    tape
    )
  )

(define (tape-set-value-at-pos tape pos value)
  (begin
    (vector-set! tape pos (char->integer value))
    tape
    )
  )




(define (read-brainfuck-until code input tape ip dp output code-len brackets-hash stop-at-input?)
  (if (>= ip code-len)
      (list input tape ip dp output)
   
  (let*
          (
           [command (string-ref code ip)]
           )
    
        (cond
          [(and (char=? command #\>) (= (+ 1 dp) code-len)) (error "dp moved out of bonds")]
          [(char=? command #\>) (read-brainfuck-until code input tape (+ 1 ip) (+ 1 dp) output code-len brackets-hash stop-at-input?)]
          [(and (char=? command #\<) (= dp 0)) (error "dp moved out of bonds")]
          [(char=? command #\<)  (read-brainfuck-until code input tape (+ 1 ip) (- dp 1) output code-len brackets-hash stop-at-input?)]
          [(char=? command #\-) (read-brainfuck-until code input (tape-decrease-at-position tape dp) (+ 1 ip) dp output code-len brackets-hash stop-at-input?)]
          [(char=? command #\+) (read-brainfuck-until code input (tape-increase-at-position tape dp) (+ 1 ip) dp output code-len brackets-hash stop-at-input?)]
          [(char=? command #\.)
           (if stop-at-input?
               (read-brainfuck-until code input tape (+ 1 ip) dp (cons (vector-ref tape dp) output) code-len brackets-hash stop-at-input?)
               (list input tape (+ 1 ip) dp (cons (vector-ref tape dp) output))
               )
           ]
          [(and (char=? command #\,) (null? input)) (error "Cannot use , when input stream already exhausted")]
          [(char=? command #\,)
           (if stop-at-input?
               (list (cdr input) (tape-set-value-at-pos tape dp (car input)) (+ 1 ip) dp output)
                (read-brainfuck-until code (cdr input) (tape-set-value-at-pos tape dp (car input)) (+ 1 ip) dp output  code-len brackets-hash stop-at-input?))
           ]
          [(and (char=? command #\[) (= (vector-ref tape dp) 0)) (read-brainfuck-until code input tape (+ 1 (hash-ref brackets-hash ip)) dp output code-len brackets-hash stop-at-input?)]
          [(and (char=? command #\]) (not (= (vector-ref tape dp) 0))) (read-brainfuck-until code input tape (+ 1 (hash-ref brackets-hash ip)) dp output code-len brackets-hash stop-at-input?)]
          [else
           (read-brainfuck-until code input tape (+ 1 ip) dp output code-len brackets-hash stop-at-input?)
           ]
          )
        )
  ))


(define (get-brackets-hash code)
  (define (add-brackets-to-hash hash a b)
    (begin
      (hash-set! hash a b)
      (hash-set! hash b a)
      hash
    ))
  
  (define (loop code stack bracket-hash current-index)
    (cond
      [(and (null? code) (null? stack)) bracket-hash]
      [(and (null? code) (not (null? stack))) #f]
      [(and (char=? (car code) #\]) (null? stack)) #f]
      [(char=? (car code) #\]) (loop (cdr code) (cdr stack) (add-brackets-to-hash bracket-hash current-index (caar stack)) (+ 1 current-index))]
      [(char=? (car code) #\[)  (loop (cdr code) (cons (cons current-index #\[) stack) bracket-hash (+ 1 current-index))]
      [else
       (loop (cdr code) stack bracket-hash (+ 1 current-index))
       ]
    )
  )
  (loop (string->list code) '() (make-hash) 0)
  )
(provide brainfuck)
(provide brainfuck-state-update-after-input-read)
(provide brainfuck-state-update-after-output-read)
(provide brainfuck-state-build)
(provide brainfuck-decoder)




