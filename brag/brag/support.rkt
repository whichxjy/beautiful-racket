#lang racket/base
(require parser-tools/lex
         racket/string
         (prefix-in : parser-tools/lex-sre)
         (for-syntax racket/base))
(provide (all-from-out parser-tools/lex)
         (all-from-out parser-tools/lex-sre)
         [struct-out token-struct]
         token
         [struct-out exn:fail:parsing])


(struct token-struct (type val offset line column span skip?) 
  #:transparent)


;; Token constructor.
;; This is intended to be a general token structure constructor that's nice
;; to work with.
;; It should cooperate with the tokenizers constructed with make-permissive-tokenizer.
(define token
  (lambda (type                 ;; (U symbol string)
           [val #f]  ;; any
           #:position [position #f] ;; (U #f number)
           #:line [line #f]     ;; (U #f number)
           #:column [column #f] ;; (U #f number)
           #:span [span #f]     ;; boolean
           #:skip? [skip? #f])
    (token-struct (if (string? type) (string->symbol type) type)
                  val
                  position line column span skip?)))


;; When bad things happen, we need to emit errors with source location.
(struct exn:fail:parsing exn:fail (srclocs)
  #:transparent
  #:property prop:exn:srclocs (lambda (instance)
                                (exn:fail:parsing-srclocs instance)))


(provide apply-tokenizer)
(define (apply-tokenizer tokenize in)
  (define input-port (if (string? in)
                         (open-input-string in)
                         in))
  (define token-producer (tokenize input-port))
  (for/list ([token (in-producer token-producer (λ(token)
                                                  ;; position-tokens are produced by lexer-src-pos
                                                  (eq? eof (if (position-token? token)
                                                               (position-token-token token)
                                                               token))))])
            token))

(provide trim-ends)
(define (trim-ends left lexeme right)
  (string-trim (string-trim lexeme left #:right? #f) right #:left? #f))

(provide from/to)
(define-lex-trans from/to
  (λ(stx)
    (syntax-case stx ()
      [(_ OPEN CLOSE)
       #'(:seq OPEN (complement (:seq any-string CLOSE any-string)) CLOSE)])))

;; change names of lexer abbreviations to be consistent with Racket srcloc conventions

(provide start-loc)
(define-syntax start-loc (make-rename-transformer #'start-pos))

(provide end-loc)
(define-syntax end-loc (make-rename-transformer #'end-pos))

(provide loc-line)
(define-syntax loc-line (make-rename-transformer #'position-line))

(provide loc-column)
(define-syntax loc-column (make-rename-transformer #'position-col))

(provide loc-position)
(define-syntax loc-position (make-rename-transformer #'position-offset))

(provide loc-span)
(define (loc-span start-loc end-loc)
  (- (loc-position end-loc)
     (loc-position start-loc)))