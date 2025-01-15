#lang racket
(require "interpreter.rkt")

(define (bynary-to-brainfuck filename input)
  (apply brainfuck-decoder (brainfuck-state-build (brainfuck-bynary filename) input))
  )

(define (to-brainfuck-encoding str)
  (cond
    [(string=? str "000") #\>]
    [(string=? str "001") #\<]
    [(string=? str "010") #\+]
    [(string=? str "011") #\-]
    [(string=? str "100") #\.]
    [(string=? str "101") #\,]
    [(string=? str "110") #\[]
    [(string=? str "111") #\] ]
  ))
 
(define (get-bits byte)
  (let* ([bin-str (number->string byte 2)]
         [left-pad (make-string (- 8 (string-length bin-str)) #\0)]
         )
    (string-append left-pad bin-str)))

(define (brainfuck-bynary filename)
  (define file (open-input-file filename #:mode 'binary))

  (define (get-bits-array output)
    (let
        ([current-byte (read-byte file)])
      
      (if  (eof-object? current-byte)
           (begin
             (close-input-port file)
             output
             )
           (get-bits-array (string-append output (get-bits current-byte)))
            )
      )
    )
  (list->string (map to-brainfuck-encoding (group (get-bits-array "") 3)))
  )

(define (group str n)
  (define (helper remaining output current-len)
    (if (< current-len n)
        (reverse output)
        (helper (substring remaining n) (cons (substring remaining 0 n) output) (- current-len 3))
        )
    )
  (helper str '() (string-length str)))

(provide bynary-to-brainfuck)