#lang br
(require (for-syntax racket/string))

(provide define-verbs
         define-thing
         define-place
         define-everywhere
         
         show-current-place
         show-inventory
         
         have-thing?
         take-thing!
         drop-thing!
         thing-state
         set-thing-state!
         
         (except-out (all-from-out br) #%module-begin)
         (rename-out [module-begin #%module-begin]))

;; ============================================================
;; Overall module:

(define #'(module-begin (txtadv-program _section ...))
  #'(#%module-begin
     _section ...
     
     (provide do-verb do-place)
     (module+ main
       (parameterize ([cmd-line-mode? #t])
         (do-place)))))

(provide verb-section)
(define-inverting #'(verb-section _heading _verb-entry ...)
  (inject-syntax ([#'all-verbs (shared-syntax 'all-verbs)])
                 #'(define-verbs all-verbs
                     _verb-entry ...)))

(provide verb-item)
(define-inverting #'(verb-item (_name0 _transitive0?) (_name _transitive?) ... _desc)
  #`[_name0 #,@(if (syntax->datum #'_transitive0?) #'(_) #'()) (= _name ...) _desc])

(provide verb-name)
(define-cases #'verb-name
  ;; cases with literals go first, so they're not caught by wildcards
  [#'(_ "," _id) #'(_id #f)]
  [#'(_ "," _id _underscore) #'(_id #t)]
  [#'(_ _id) #'(_id #f)]
  [#'(_ _id _underscore) #'(_id #t)])

(provide everywhere-section)
(define-inverting #'(everywhere-section _heading [_name _desc] ...)
  #'(define-everywhere everywhere-actions
      ([_name _desc] ...)))

(provide everywhere-item)
(define-inverting #'(everywhere-item _name _desc)
  #'(_name _desc))

(provide things-section)
(define-inverting #'(things-section _heading _thing ...)
  #'(begin _thing ...))

(provide thing-item)
(define-inverting #'(thing-item (thing-id _thingname) (_actionname _actiondesc) ...)
  #'(define-thing _thingname [_actionname _actiondesc] ...))

(provide thing-action)
(define-inverting #'(thing-action _actionname _actiondesc)
  #'(_actionname _actiondesc))

(provide places-section)
(define-inverting #'(places-section _heading _placeitem ...)
  #'(begin _placeitem ...))

(provide place-item)
(define-inverting #'(place-item _place-id _place-desc [_place-item ...] [_actionname _actiondesc] ...)
  #'(define-place _place-id _place-desc [_place-item ...] ([_actionname _actiondesc] ...)))

(provide place-id)
(define #'(place-id _id) #'_id)

(provide place-descrip)
(define #'(place-descrip _desc) #'_desc)

(provide place-items)
(define-inverting #'(place-items "[" _id ... "]") #'(_id ...))

(provide place-name)
(define-cases #'place-name
  [#'(_ "," _id) #'_id]
  [#'(_ _id) #'_id])

(provide place-action)
(define-inverting #'(place-action _id _desc) #'(_id _desc))

(provide desc)
(define #'(desc _d) #'_d)

(provide s-exp)
(define-cases-inverting #'s-exp
  [#'(_ "(" _sx ... ")") #'(_sx ...)]
  [#'(_ _sx) #'_sx])


(provide start-section)
(define #'(start-section _heading _where)
  (inject-syntax ([#'all-verbs (shared-syntax 'all-verbs)])
                 #'(init-game _where
                              all-verbs
                              everywhere-actions)))

;; ============================================================
;; Model:

;; Elements of the world:
(struct verb (aliases       ; list of symbols
              desc          ; string
              transitive?) #:transparent) ; boolean
(struct thing (name         ; symbol
               [state #:mutable] ; any value
               actions) #:transparent)    ; list of verb--thunk pairs
(struct place (desc         ; string
               [things #:mutable] ; list of things
               actions) #:transparent)    ; list of verb--thunk pairs

;; Tables mapping names<->things for save and load
(define names (make-hash))
(define elements (make-hash))

(define (record-element! name val)
  (hash-set! names name val)
  (hash-set! elements val name))

(define (name->element name) (hash-ref names name #f))
(define (element->name obj) (hash-ref elements obj #f))

;; ============================================================
;; Macros for constructing and registering elements:

(define #'(define-verbs _all-id [_id _spec ...] ...)
  #'(begin
      (define-one-verb _id _spec ...) ...
      (record-element! '_id _id) ...
      (define _all-id (list _id ...))))


;; todo: the underscore arguments in cases 2 & 4 should be literal underscores, not wildcards
(define-syntax define-one-verb
  (syntax-rules (= _)
    [(define-one-verb id (= alias ...) desc)
     (define id (verb (list 'id 'alias ...) desc #f))]
    [(define-one-verb id _ (= alias ...) desc)
     (define id (verb (list 'id 'alias ...) desc #t))]
    [(define-one-verb id)
     (define id (verb (list 'id) (symbol->string 'id) #f))]
    [(define-one-verb id _)
     (define id (verb (list 'id) (symbol->string 'id) #t))]))


(define-syntax-rule (define-thing id 
                      [vrb expr] ...)
  (begin
    (define id 
      (thing 'id #f (list (cons vrb (lambda () expr)) ...)))
    (record-element! 'id id)))


(define-syntax-rule (define-place id 
                      desc 
                      (thng ...) 
                      ([vrb expr] ...))
  (begin
    (define id (place desc
                      (list thng ...)
                      (list (cons vrb (lambda () expr)) ...)))
    (record-element! 'id id)))


(define-syntax-rule (define-everywhere id ([vrb expr] ...))
  (define id (list (cons vrb (lambda () expr)) ...)))

;; ============================================================
;; Game state

(define cmd-line-mode? (make-parameter #f))

;; Initialized on startup:
(define all-verbs null)          ; list of verbs
(define everywhere-actions null) ; list of verb--thunk pairs

;; Things carried by the player:
(define player-inventory null) ; list of things

;; Current location:
(define current-place #f) ; place (or #f until started)

;; Fuctions to be used by verb responses:
(define (have-thing? thing)
  (memq thing player-inventory))
(define (take-thing! thing) 
  (set-place-things! current-place (remq thing (place-things current-place)))
  (set! player-inventory (cons thing player-inventory)))
(define (drop-thing! thing) 
  (set-place-things! current-place (cons thing (place-things current-place)))
  (set! player-inventory (remq thing player-inventory)))

;; ============================================================
;; Game execution

;; Show the player the current place, then get a command:
(define (do-place)
  (show-current-place)
  (when (cmd-line-mode?) (do-verb)))

;; Show the current place:
(define (show-current-place)
  (printf "~a\n" (place-desc current-place))
  (for-each
   (λ (thing) (printf "There is a ~a here.\n" (thing-name thing)))
   (place-things current-place)))

;; Get and handle a command:

(define (get-line)
  (printf "> ")
  (flush-output)
  (read-line))

(define (do-verb [line (and (cmd-line-mode?) (get-line))])
  (define input (if (eof-object? line)
                    '(quit)
                    (let ([port (open-input-string line)])
                      (for/list ([v (in-port read port)]) v))))  
  (if (and (list? input)
           (andmap symbol? input)
           (<= 1 (length input) 2))
      (let* ([verb (car input)]
             [response
              (cond
                [(= 2 (length input))
                 (handle-transitive-verb verb (cadr input))]
                [(= 1 (length input))
                 (handle-intransitive-verb verb)])]
             [result (response)])
        (cond
          [(place? result)
           (set! current-place result)
           (do-place)]
          [(string? result)
           (printf "~a\n" result)
           (when (cmd-line-mode?) (do-verb))]
          [else (when (cmd-line-mode?) (do-verb))]))
      (begin
        (printf "I don't undertand what you mean.\n")
        (when (cmd-line-mode?) (do-verb)))))

;; Handle an intransitive-verb command:
(define (handle-intransitive-verb verb)
  (or
   (find-verb verb (place-actions current-place))
   (find-verb verb everywhere-actions)
   (using-verb 
    verb all-verbs
    (λ (verb)
      (λ () (if (verb-transitive? verb)
                (format "~a what?" (string-titlecase (verb-desc verb)))
                (format "Can't ~a here." (verb-desc verb))))))
   (λ () (format "I don't know how to ~a." verb))))

;; Handle a transitive-verb command:
(define (handle-transitive-verb verb-in obj)
  (or (using-verb 
       verb-in all-verbs
       (λ (verb)
         (and 
          (verb-transitive? verb)
          (cond
            [(ormap (λ (thing)
                      (and (eq? (thing-name thing) obj)
                           thing))
                    (append (place-things current-place)
                            player-inventory))
             => (λ (thing)
                  (or (find-verb verb-in (thing-actions thing))
                      (λ ()
                        (format "Don't know how to ~a ~a."
                                (verb-desc verb) obj))))]
            [else
             (λ () (format "There's no ~a here to ~a." obj 
                           (verb-desc verb)))]))))
      (λ () (format "I don't know how to ~a ~a." verb-in obj))))

;; Show what the player is carrying:
(define (show-inventory)
  (printf "You have")
  (if (null? player-inventory)
      (printf " no items.")
      (for-each (λ (thing)
                  (printf "\n  a ~a" (thing-name thing)))
                player-inventory))
  (printf "\n"))

;; Look for a command match in a list of verb--response pairs,
;; and returns the response thunk if a match is found:
(define (find-verb cmd actions)
  (ormap (λ (a)
           (and (memq cmd (verb-aliases (car a)))
                (cdr a)))
         actions))

;; Looks for a command in a list of verbs, and
;; applies `success-k' to the verb if one is found:
(define (using-verb cmd verbs success-k)
  (ormap (λ (vrb)
           (and (memq cmd (verb-aliases vrb))
                (success-k vrb)))
         verbs))


;; ============================================================
;; To go:

(define (init-game in-place
                   in-all-verbs
                   in-everywhere-actions)
  (set! current-place in-place)
  (set! all-verbs in-all-verbs)
  (set! everywhere-actions in-everywhere-actions))
