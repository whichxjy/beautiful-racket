#lang scribble/manual
@(require (for-label br/conditional))

@title[#:style 'toc]{Beautiful Racket}

@author[(author+email "Matthew Butterick" "mb@mbtype.com")]


Beautiful Racket @link["http://beautifulracket.com"]{is a book} about making programming languages with Racket.

This library provides the @tt{#lang br} teaching language used in the book, as well as supporting modules that can be used in other programs.


@;defmodulelang[br]
@defmodule[br]

@tt{#lang br} is a teaching language designed to smooth over some of the small idiosyncrasies and inconsistencies in Racket, so that those new to Racket will say ``ah, that makes sense'' rather than ``huh? what?'' @tt{#lang br} is not meant to hide the true nature of Racket, but rather defer certain parts of the learning curve.

To that end, this documentation not only explains the functions and forms in the Beautiful Racket library, but also how they depart from traditional or idiomatic Racket. (BTW ``Beautiful Racket'' is the name of the book, not an implication that the rest of Racket is less than beautiful. It is! But one thing at a time.)

@section{Conditionals}

@defmodule[br/conditional]

@defform[(while cond body ...)]
Loop over @racket[_body] expressions as long as @racket[_cond] is not @racket[#f]. If @racket[_cond] starts out @racket[#f], @racket[_body] expressions are not evaluated.

@defform[(until cond body ...)]
Loop over @racket[_body] expressions until @racket[_cond] is not @racket[#f]. If @racket[_cond] starts out @racket[#f], @racket[_body] expressions are not evaluated.


@section{Datums}

@defmodule[br/datum]

@defproc[
(format-datum
[datum-template symbol?]
[arg any/c?] ...)
datum?]
tk


@section{Debugging}

@defmodule[br/debug]

TK

@section{Define}

@defmodule[br/define]

@defform[
(define-macro
(tag-id attr-id elem-id) body ...)]
Hello


@defform[
(define-macro-cases
(tag-id attr-id elem-id) body ...)]
Hello


@section{Reader utilities}

@defmodule[br/reader-utils]

TK

@section{Syntax}

@defmodule[br/syntax]

TK