#lang racket/base
(require br-parser-tools/lex
         racket/string
         racket/struct
         (prefix-in : br-parser-tools/lex-sre)
         (for-syntax racket/base))
(provide (all-from-out br-parser-tools/lex)
         (all-from-out br-parser-tools/lex-sre)
         [struct-out token-struct]
         token
         [struct-out exn:fail:parsing])



(define (token-print token port mode)
  (write-string (format "~a"
                        (cons 'token-struct
                              (map (λ(proc) (format "~v" (proc token)))
                                   (list
                                    token-struct-type
                                    token-struct-val
                                    token-struct-line
                                    token-struct-column
                                    token-struct-offset
                                    token-struct-span
                                    token-struct-skip?)))) port))


(struct token-struct (type val offset line column span skip?)
  #:auto-value #f
  #:transparent)


;; Token constructor.
;; This is intended to be a general token structure constructor that's nice
;; to work with.
;; It should cooperate with the tokenizers constructed with make-permissive-tokenizer.
(define (token type                 ;; (U symbol string)
               [val #f]  ;; any
               [srcloc #f]
               #:position [position #f] ;; (U #f number)
               #:line [line #f]     ;; (U #f number)
               #:column [column #f] ;; (U #f number)
               #:span [span #f]     ;; boolean
               #:skip? [skip? #f])
  (token-struct (if (string? type) (string->symbol type) type)
                val
                ;; keyword values take precedence over srcloc values
                (or position (and srcloc (srcloc-position srcloc)))
                (or line (and srcloc (srcloc-line srcloc)))
                (or column (and srcloc (srcloc-column srcloc)))
                (or span (and srcloc (srcloc-span srcloc)))
                skip?))


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
  (for/list ([token (in-producer token-producer (λ(tok)
                                                  (eq? eof (cond
                                                             ;; position-tokens are produced by lexer-src-pos,
                                                             [(position-token? tok)
                                                              (position-token-token tok)]
                                                             ;; and srcloc-tokens by lexer-srcloc
                                                             [(srcloc-token? tok)
                                                              (srcloc-token-token tok)]
                                                             [else tok]))))])
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

(provide uc+lc)
(define-lex-trans uc+lc
  (λ(stx)
    (syntax-case stx ()
      [(_ . STRS)
       (with-syntax ([(UCSTR ...) (map (compose1 string-upcase syntax->datum) (syntax->list #'STRS))]
                     [(LCSTR ...) (map (compose1 string-downcase syntax->datum) (syntax->list #'STRS))])
         #'(union (union UCSTR ...) (union LCSTR ...)))])))

;; change names of lexer abbreviations to be consistent with Racket srcloc conventions

(define-syntax-rule (dprt ID-IN ID-OUT)
  (begin
    (provide ID-IN)
    (define-syntax ID-IN (make-rename-transformer (syntax ID-OUT)))))

(dprt lexeme-start start-pos)
(dprt lexeme-end end-pos)
(dprt line position-line)
(dprt col position-col)
(dprt pos position-offset)

(provide span)
(define (span lexeme-start lexeme-end)
  (abs ; thus same result in reverse order
   (- (pos lexeme-end)
      (pos lexeme-start))))