#lang scribble/manual

@(require (for-label racket))

@title{Base58}

@author[(author+email "Marc Burns" "marc@kn0x.io")]

@defmodule[base58]

Provides
@hyperlink["https://en.bitcoin.it/wiki/Base58Check_encoding"]{Base58Check}
encoding and decoding functions.

@defproc[(base58-encode [bs bytes?] [#:check? check? any/c #t]) string?]{
  Encodes @racketfont{bs} to a base58 string.
  When @racket[check?] is not @racket[#f], appends check bytes.
}

@defproc[(base58-decode [str string?] [#:check? check? any/c #t]) bytes?]{
  Decodes base58 string @racketfont{str} to bytes.
  When @racket[check?] is not @racket[#f], validates and removes check bytes.
}