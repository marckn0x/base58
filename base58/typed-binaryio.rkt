#lang typed/racket

(require/typed/provide
 binaryio
 [integer->bytes (-> Integer Nonnegative-Integer Boolean Boolean Bytes)]
 [bytes->integer (-> Bytes #f Boolean Nonnegative-Integer)]
 [integer-bytes-length (-> Nonnegative-Integer #f Nonnegative-Integer)])