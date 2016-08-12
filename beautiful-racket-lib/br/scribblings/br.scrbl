#lang scribble/manual
@(require (for-label racket/base racket/contract br))

@(require scribble/eval)

@(define my-eval (make-base-eval))
@(my-eval `(require br))


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

A @defterm{datum} is a literal representation of a single unit of Racket code, also known as an @defterm{S-expression}. Unlike a string, a datum preserves the internal structure of the S-expression. Meaning, if the S-expression is a single value, or list-shaped, or tree-shaped, so is its corresponding datum. 

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


@defform*[[
(report expr) 
(report expr maybe-name)
]]
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
(define-cases id 
  [pat body ...+] ...+)
]
Define a function that behaves differently depending on how many arguments are supplied (also known as @seclink["Evaluation_Order_and_Arity" #:doc '(lib "scribblings/guide/guide.scrbl")]{@italic{arity}}). Like @racket[cond], you can have any number of branches. Each branch starts with a @racket[_pat] that accepts a certain number of arguments. If the current invocation of the function matches the number of arguments in @racket[_pat], then the @racket[_body] on the right-hand side is evaluated. If there is no matching case, an arity error arises.

Derived from @racket[case-lambda], which you might prefer.

@examples[#:eval my-eval
(define-cases f
  [(f arg1) (* arg1 arg1)]
  [(f arg1 arg2) (* arg1 arg2)]
  [(f arg1 arg2 arg3 arg4) (* arg1 arg2 arg3 arg4)])

(f 4)
(f 6 7)
(f 1 2 3 4)
(f "three" "arguments" "will-trigger-an-error")

(define-cases f2
  [(f2) "got zero args"]
  [(f2 . args) (format "got ~a args" (length args))])

(f2)
(f2 6 7)
(f2 1 2 3 4)
(f2 "three" "arguments" "will-not-trigger-an-error-this-time")

]


@defform*[
#:literals (syntax lambda stx)
[
(define-macro id (syntax other-id)) 
(define-macro id (lambda (arg-id) result-expr ...+))
(define-macro id (syntax result-expr)) 
(define-macro (id pat-arg ...) expr ...+) 
]]
Create a macro using one of the subforms above, which are explained below:

@specsubform[#:literals (define-macro syntax lambda stx) 
(define-macro id (syntax other-id))]{
If the first argument is an identifier @racket[_id] and the second a syntaxed identifier that looks like @racket[(syntax other-id)], create a @tech{rename transformer}, which is a fancy term for ``macro that replaces @racket[_id] with @racket[_other-id].'' (This subform is equivalent to @racket[make-rename-transformer].)

Why do we need rename transformers? Because an ordinary macro operates on its whole calling expression, like @racket[(macro-name this-arg that-arg . and-so-on)]. Whereas a rename transformer operates only on the identifier itself (regardless of where it appears in the code). It's like creating an alias for an identifier.

Below, notice how the rename transformer, operating in the macro realm, approximates the behavior of a run-time assignment. 

@examples[#:eval my-eval
(define foo 'foo-value)
(define bar foo)
bar
(define-macro zam-macro #'foo)
zam-macro
(define add +)
(add 20 22)
(define-macro sum-macro #'+)
(sum-macro 20 22)
]
}


@specsubform[#:literals (define-macro lambda stx) 
(define-macro id (lambda (arg-id) result-expr ...+))]{
If the first argument is an @racket[_id] and the second a single-argument procedure, create a macro called @racket[_id] that uses the procedure as a @tech{syntax transformer}. This transformer must return a @tech{syntax object}, otherwise you'll trigger an error. Beyond that, the transformer can do whatever you like. (This subform is equivalent to @racket[define-syntax].)


@examples[#:eval my-eval
(define-macro nice-sum (lambda (stx) #'(+ 2 2)))
nice-sum
(define-macro not-nice (lambda (stx) '(+ 2 2)))
not-nice
]

}

@specsubform[#:literals (define-macro) 
(define-macro id syntax-object)
#:contracts ([syntax-object syntax?])]{
If the first argument is an @racket[_id] and the second a @racket[_syntax-object], create a @tech{syntax transformer} that returns @racket[_syntax-object]. This is just alternate notation for the previous subform, wrapping @racket[_syntax-object] inside a syntax-transformer body. The effect is to create a macro from @racket[_id] that always returns @racket[_syntax-object], regardless of how it's invoked. Not especially useful within programs. Mostly handy for making quick macros at the @tech{REPL}.

@examples[#:eval my-eval
(define-macro bad-listener #'"did you say something?")
bad-listener
(bad-listener)
(bad-listener "hello")
(bad-listener 1 2 3 4)
]

}

@specsubform[#:literals (define-macro) 
(define-macro (id pat-arg ...) result-expr ...+)]{
If the first argument is a @tech{syntax pattern} starting with @racket[_id], then create a @tech{syntax transformer} for this pattern using @racket[_result-expr ...] as the return value. As usual, @racket[_result-expr ...] needs to return a @tech{syntax object} or you'll get an error.

By convention, if a @racket[pat-arg] has a @tt{CAPITALIZED-NAME}, it's treated as a named wildcard match (meaning, it will match any expression in that position, and can be subsequently referred to by that name). Otherwise, @racket[pat-arg] is treated as a literal matcher (meaning, it will only match the same expression). 

For instance, the @racket[sandwich] macro below requires three arguments, and the third must be @racket[please], but the other two are wildcards:

@examples[#:eval my-eval
(define-macro (sandwich TOPPING FILLING please)
  #'(format "I love ~a with ~a." 'FILLING 'TOPPING))

(sandwich brie ham)
(sandwich brie ham now)
(sandwich brie ham please)
(sandwich banana bacon please)

]

The ellipsis @racket[...] can be used with a wildcard matcher to match a list of arguments. Please note: though a wildcard matcher standing alone must match one argument, once you add an ellipsis, it's allowed to match zero:

@examples[#:eval my-eval
(define-macro (pizza TOPPING ...)
  #'(string-join (cons "Waiter!"
                       (list (format "More ~a!" 'TOPPING) ...))
                 " "))

(pizza mushroom)
(pizza mushroom pepperoni)
(pizza)
]

The capitalization convention for a wildcard @racket[pat-arg] makes it easy to mix literal and wildcard matchers in one pattern. But it also makes it easy to mistype a pattern and not get the wildcard matcher you were expecting. Below, @racket[bad-squarer] doesn't work because @racket[any-number] is meant to be a wildcard. But it's not capitalized, so it's considered a literal matcher, and it triggers an error:

@examples[#:eval my-eval
(define-macro (bad-squarer any-number)
  #'(* any-number any-number))
(bad-squarer +10i)
]

The error is cleared when the argument is capitalized, thus making it a wilcard matcher:

@examples[#:eval my-eval
(define-macro (good-squarer ANY-NUMBER)
  #'(* ANY-NUMBER ANY-NUMBER))
(good-squarer +10i)
]

This subform of @racket[define-macro] is useful for macros that have one calling pattern. To make a macro with multiple calling patterns, see @racket[define-macro-cases].
}


@defform*[
[
(define-macro-cases id [(_ pat-arg ...) result-expr ...+] ...+) 
]]
TBD

@section{Reader utilities}

@defmodule[br/reader-utils]

TK

@section{Syntax}

@defmodule[br/syntax]

TK