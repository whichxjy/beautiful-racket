#lang scribble/manual
@(require (for-label racket/base racket/contract br/cond br/datum))

@(require scribble/eval)

@(define my-eval (make-base-eval))
@(my-eval `(require br/cond br/datum))


@title[#:style 'toc]{Beautiful Racket}

@author[(author+email "Matthew Butterick" "mb@mbtype.com")]


@defmodule[br]

@link["http://beautifulracket.com"]{@italic{Beautiful Racket}} is a book about making programming languages with Racket.

This library provides the @tt{#lang br} teaching language used in the book, as well as supporting modules that can be used in other programs.

This library is designed to smooth over some of the small idiosyncrasies and inconsistencies in Racket, so that those new to Racket are more likely to say ``ah, that makes sense'' rather than ``huh? what?''

@section{The @tt{br} language(s)}

@defmodulelang[br]


@defmodulelang[br/quicklang]



@section{Conditionals}

@defmodule[br/cond]

@defform[(while cond body ...)]
Loop over @racket[_body] as long as @racket[_cond] is not @racket[#f]. If @racket[_cond] starts out @racket[#f], @racket[_body] is never evaluated.

@examples[#:eval my-eval
(let ([x 42])
  (while (positive? x)
         (set! x (- x 1)))
  x)
(let ([x 42])
  (while (negative? x)
         (unleash-zombie-army))
  x)
]


@defform[(until cond body ...)]
Loop over @racket[_body] until @racket[_cond] is not @racket[#f]. If @racket[_cond] starts out not @racket[#f], @racket[_body] is never evaluated.

@examples[#:eval my-eval
(let ([x 42])
  (until (zero? x)
         (set! x (- x 1)))
  x)
(let ([x 42])
  (until (= 42 x)
         (destroy-galaxy))
  x)
]

@section{Datums}

@defmodule[br/datum]

A @defterm{datum} is a literal representation of Racket code that describes an S-expression. Unlike a string, a datum preserves the internal structure of the S-expression. Meaning, if the S-expression is a single value, or list-shaped, or tree-shaped, so is its corresponding datum. 

Datums are made with @racket[quote] or its equivalent notation, the @litchar{'} prefix (see @secref["quote" #:doc '(lib "scribblings/guide/guide.scrbl")]).

When I use ``datum'' in its specific Racket sense, I use ``datums'' as its plural rather than ``data'' because that term has an existing, more generic meaning. 

@defproc[
(format-datum
[datum-form datum?]
[val any/c?] ...)
datum?]
Similar to @racket[format], but the template @racket[_datum-form] is a datum, rather than a string, and the function returns a datum, rather than a string. Otherwise, the same formatting escapes can be used in the template (see @racket[fprintf]).

Two special cases. First, a string that describes a list of datums is parenthesized so the result is a single datum. Second, an empty string returns @racket[void] (not @racket[#f], because that's a legitimate datum).

@examples[#:eval my-eval
(format-datum '42)
(format-datum '~a "foo")
(format-datum '(~a ~a) "foo" 42)
(format-datum '~a "foo bar zam")
(void? (format-datum '~a ""))
(format-datum '~a #f)
]

@defproc[
(format-datums
[datum-form datum?]
[vals (listof any/c?)] ...)
(listof datum?)]
Like @racket[format-datum], but applies @racket[_datum-form] to the lists of @racket[_vals] in similar way to @racket[map], where values for the format string are taken from the lists of @racket[_vals] in parallel. This means that a) @racket[_datum-form] must accept as many arguments as there are lists of @racket[_vals], and b) the lists of @racket[_vals] must all have the same number of items.

@examples[#:eval my-eval
(format-datums '~a '("foo" "bar" "zam"))
(format-datums '(~a 42) '("foo" "bar" "zam"))
(format-datums '(~a ~a) '("foo" "bar" "zam") '(42 43 44))
(format-datums '42 '("foo" "bar" "zam"))
(format-datums '(~a ~a) '("foo" "bar" "zam") '(42))
]

@section{Debugging}

@defmodule[br/debug]


@defform*[((report expr) (report expr maybe-name))]
Print the name and value of @racket[_expr] to @racket[current-error-port], but also return the evaluated result of @racket[_expr] as usual. This lets you see the value of an expression or variable at runtime without disrupting any of the surrounding code. Optionally, you can use @racket[_maybe-name] to change the name shown in @racket[current-error-port].

For instance, suppose you wanted to see how @racket[first-condition?] was being evaluted in this expression:

@racketblock[
(if (and (first-condition? x) (second-condition? x))
  (one-thing)
  (other-thing))]

You can wrap it in @racket[report] and find out:

@racketblock[
(if (and (report (first-condition? x)) (second-condition? x))
  (one-thing)
  (other-thing))]

This code will run the same way as before. But when it reaches @racket[first-condition?], you willl see in @racket[current-error-port]:

@racketerror{(first-condition? x) = #t}

You can also add standalone calls to @racket[report] as a debugging aid at points where the return value will be irrelevant, for instance:

@racketblock[
(report x x-before-function)
(if (and (report (first-condition? x)) (second-condition? x))
  (one-thing)
  (other-thing))]

@racketerror{x-before-function = 42
@(linebreak)(first-condition? x) = #t}

But be careful — in the example below, the result of the @racket[if] expression will be skipped in favor of the last expression, which will be the value of @racket[_x]:

@racketblock[
(if (and (report (first-condition? x)) (second-condition? x))
  (one-thing)
  (other-thing))
  (report x)]


@defform[(report* expr ...)]
Apply @racket[report] separately to each @racket[_expr] in the list.


@defform*[((report-datum stx-expr) (report-datum stx-expr maybe-name))]
A variant of @racket[report] for use with @secref["stx-obj" #:doc '(lib "scribblings/guide/guide.scrbl")]. Rather than print the whole object (as @racket[report] would), @racket[report-datum] prints only the datum inside the syntax object, but the return value is the whole syntax object.


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