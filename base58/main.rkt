#lang typed/racket

(require/typed
 sha
 [hmac-sha512 (-> Bytes Bytes Bytes)]
 [sha256 (-> Bytes Bytes)])

(require/typed
 binaryio
 [integer->bytes (-> Integer Nonnegative-Integer Boolean Boolean Bytes)]
 [bytes->integer (-> Bytes #f Boolean Nonnegative-Integer)]
 [integer-bytes-length (-> Nonnegative-Integer #f Nonnegative-Integer)])

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

(: base58-decode (-> String [#:check? Any] Bytes))
(define (base58-decode str #:check? [check? #t])
  (define-values (bigval leading-zeroes)
    (for/fold ([n : Nonnegative-Integer 0]
               [l : Nonnegative-Integer 0])
              ([c : Char str])
      (define m (hash-ref alphabet c (thunk (error "Invalid character in base58 string"))))
      (if (and (= n 0) (= m 0))
          (values n (add1 l))
          (values (+ (* n 58) m) l))))
  
  (define bytes
    (if (> bigval 0)
        (integer->bytes bigval (+ (integer-bytes-length bigval #f) leading-zeroes) #f #t)
        (make-bytes leading-zeroes 0)))
  
  (if check?
      (begin
        (unless (>= (bytes-length bytes) 4)
          (error "Check bytes are missing (decoded byte string is too short)"))
        (let* ([check-bytes (subbytes bytes (- (bytes-length bytes) 4) (bytes-length bytes))]
               [non-check-bytes (subbytes bytes 0 (- (bytes-length bytes) 4))]
               [hash (sha256 (sha256 non-check-bytes))])
          (unless (equal? check-bytes (subbytes hash 0 4))
            (error "Invalid base58 string checksum"))
          non-check-bytes))
      bytes))

(: base58-encode (-> Bytes [#:check? Any] String))
(define (base58-encode bs #:check? [check? #t])
  (define bs-maybe-check
    (if check?
        (bytes-append bs (subbytes (sha256 (sha256 bs)) 0 4))
        bs))
  
  (: encode-nonzero (-> Bytes (Listof Char)))
  (define (encode-nonzero bs)
    (reverse
     (let loop : (Listof Char)
       ([n : Nonnegative-Integer (bytes->integer bs #f #t)])
       (if (= n 0)
           empty
           (cons
            (hash-ref backward-alphabet (remainder n 58))
            (loop (quotient n 58)))))))

  (define-values (ones idx-first-nonzero)
    (for/fold ([ones : (Listof Char) empty]
               [idx-first-nonzero : Nonnegative-Integer 0])
              ([b bs-maybe-check])
      #:break (> b 0)
      (values (cons #\1 ones) (add1 idx-first-nonzero))))

  (list->string
   (append
    ones
    (if (< idx-first-nonzero (bytes-length bs-maybe-check))
        (encode-nonzero (subbytes bs-maybe-check idx-first-nonzero))
        empty))))

(module+ test
  (require typed/rackunit)

  (define tqbf #"The quick brown fox jumps over the lazy dog.")
  (define strs
    `(,tqbf
      #""
      #"\230F3x\316\323U\24\373[B\301\204\243#!\347\342Bl"
      #"\0\0\0\0\0"
      #"\0\0\0asdf"))
  (for* ([s strs]
         [check? (list #f #t)])
    (check-equal? (base58-decode (base58-encode s #:check? check?) #:check? check?) s))

  (check-equal? (base58-decode "2NEpo7TZRRrLZSi2U" #:check? #f) #"Hello World!")
  (check-equal? (base58-encode tqbf #:check? #f) "USm3fpXnKG5EUBx2ndxBDMPVciP5hGey2Jh4NDv6gmeo1LkMeiKrLJUUBk6Z")

  (define fixed #"\200\355\333\334\21h\361\332\352\333\323\344L\36?\217Z(L )\367\212\322j\371\205\203\244\231\336[\31")
  (define fixed-b58 "5Kd3NBUAdUnhyzenEwVLy9pBKxSwXvE9FMPyR4UKZvpe6E3AgLr")
  
  (check-equal? (base58-decode fixed-b58) fixed)
  (check-equal? (base58-encode fixed) fixed-b58))