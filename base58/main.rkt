#lang typed/racket

(require "typed-sha.rkt"
         "typed-binaryio.rkt")

(provide base58-encode
         base58-decode)

(define alphabet
    (for/hash : (Immutable-HashTable Char Nonnegative-Integer)
      ([c "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"]
       [i : Nonnegative-Integer (in-naturals)])
      (values c i)))

(define backward-alphabet
  (for/hash : (Immutable-HashTable Integer Char)
    ([(k v) alphabet])
    (values v k)))

(: base58-decode (-> String Bytes))
(define (base58-decode str)
  (define-values (bigval leading-zeroes)
    (for/fold ([n : Nonnegative-Integer 0]
               [l : Nonnegative-Integer 0])
              ([c : Char str])
      (define m (hash-ref alphabet c (thunk (raise "Invalid char in base58 string"))))
      (if (and (= n 0) (= m 0))
          (values n (add1 l))
          (values (+ (* n 58) m) l))))
  (define bytes
    (integer->bytes bigval (+ (integer-bytes-length bigval #f) leading-zeroes) #f #t))
  (define check-bytes (subbytes bytes (- (bytes-length bytes) 4) (bytes-length bytes)))
  (define non-check-bytes (subbytes bytes 0 (- (bytes-length bytes) 4)))
  (define hash (sha256 (sha256 non-check-bytes)))
  (unless (equal? check-bytes (subbytes hash 0 4))
    (raise "Invalid base58 string checksum"))
  non-check-bytes)

(: base58-encode (-> Bytes String))
(define (base58-encode bs)
  (define bs-check (subbytes (sha256 (sha256 bs)) 0 4))
  (define out-chars
    (let loop : (Listof Char)
      ([n : Nonnegative-Integer (bytes->integer (bytes-append bs bs-check) #f #t)])
      (if (= n 0)
          empty
          (cons
           (hash-ref backward-alphabet (remainder n 58))
           (loop (quotient n 58))))))
  (list->string (reverse out-chars)))