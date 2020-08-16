#lang scribble/manual

@(require (for-label racket))

@title{Base58}

@defmodule[base58]

Provides Base58 encoding and decoding functions with optional Base58Check.

@defproc[(base58-encode [bs bytes?]) string?]{
  Encode @racket[bs] to a base58 string.
}

@defproc[(base58-decode [str string?]) bytes?]{
  Decode base58 string @racket[str] to bytes.
}
