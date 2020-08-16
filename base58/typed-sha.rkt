#lang typed/racket

(require/typed/provide
 sha
 [hmac-sha512 (-> Bytes Bytes Bytes)]
 [sha256 (-> Bytes Bytes)])