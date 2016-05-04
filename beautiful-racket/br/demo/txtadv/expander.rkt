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

(define #'(module-begin _lines ...)
  #'(#%module-begin
     _lines ...
     
     (provide do-verb do-place)
     (module+ main
       (parameterize ([cmd-line-mode? #t])
         (do-place)))))

(provide txtadv-program)
(define #'(txtadv-program _section ...)
  #'(module-begin _section ...))

(provide verb-section)
(define-inverting #'(verb-section _heading _verb-item ...)
  (inject-syntax ([#'in-verbs (shared-syntax 'in-verbs)])
                 #'(define-verbs in-verbs
                     _verb-item ...)))


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

(provide id-desc)
(define-inverting #'(id-desc _id _desc)
  #'(_id _desc))

(provide things-section)
(define-inverting #'(things-section _heading _thing ...)
  #'(begin _thing ...))

(provide thing-item)
(define-inverting #'(thing-item _thingname (_actionname _actiondesc) ...)
  #'(define-thing _thingname [_actionname _actiondesc] ...))

(provide places-section)
(define-inverting #'(places-section _heading _placeitem ...)
  #'(begin _placeitem ...))

(provide place-item)
(define-inverting #'(place-item _place-id _place-desc [_place-item ...] [_actionname _actiondesc] ...)
  #'(define-place _place-id _place-desc [_place-item ...] ([_actionname _actiondesc] ...)))

(provide place-descrip)
(define #'(place-descrip _desc) #'_desc)

(provide place-items)
(define-inverting #'(place-items "[" _id ... "]") #'(_id ...))

(provide place-name)
(define-cases #'place-name
  [#'(_ "," _id) #'_id]
  [#'(_ _id) #'_id])


(provide s-exp)
(define-cases-inverting #'s-exp
  [#'(_ "(" _sx ... ")") #'(_sx ...)]
  [#'(_ _sx) #'_sx])


;; todo: consolidate the game-starters.
;; `start-game-at` works with s-exp language,
;; `start-section` works with text lang. 
(provide start-game-at)
(define #'(start-game-at _where)
  (inject-syntax ([#'in-verbs (shared-syntax 'in-verbs)])
                 #'(init-game _where
                              in-verbs
                              everywhere-actions)))

(provide start-section)
(define #'(start-section _heading _where)
  (inject-syntax ([#'in-verbs (shared-syntax 'in-verbs)])
                 #'(init-game _where
                              in-verbs
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

(define action-verb car)
(define action-response cdr)

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
(define-cases #'define-one-verb
  [#'(_ _id (= _alias ...) _desc)
   #'(define _id (verb (list '_id '_alias ...) _desc #f))]
  [#'(_ _id _ (= _alias ...) _desc)
   #'(define _id (verb (list '_id '_alias ...) _desc #t))]
  [#'(_ _id)
   #'(define _id (verb (list '_id) (symbol->string '_id) #f))]
  [#'(_ _id _)
   #'(define _id (verb (list '_id) (symbol->string '_id) #t))])


(define #'(define-thing _id [_vrb _expr] ...)
  #'(begin
      (define _id
        (thing '_id #f (list (cons _vrb (λ () _expr)) ...)))
      (record-element! '_id _id)))


(define #'(define-place _id _desc (_thng ...) ([_vrb _expr] ...))
  #'(begin
      (define _id (place _desc
                         (list _thng ...)
                         (list (cons _vrb (λ () _expr)) ...)))
      (record-element! '_id _id)))


(define #'(define-everywhere _id ([_vrb _expr] ...))
  #'(define _id (list (cons _vrb (λ () _expr)) ...)))

;; ============================================================
;; Game state

(define cmd-line-mode? (make-parameter #f))

;; Initialized on startup:
(define game-verbs null)          ; list of verbs
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
              (case (length input)
                [(2) (handle-transitive-verb verb (cadr input))]
                [(1) (handle-intransitive-verb verb)])]
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
    verb game-verbs
    (λ (verb)
      (λ () (if (verb-transitive? verb)
                (format "~a what?" (string-titlecase (verb-desc verb)))
                (format "Can't ~a here." (verb-desc verb))))))
   (λ () (format "I don't know how to ~a." verb))))

;; Handle a transitive-verb command:
(define (handle-transitive-verb verb-in obj)
  (or (using-verb 
       verb-in game-verbs
       (λ (verb)
         (and 
          (verb-transitive? verb)
          (cond
            [(for/first ([thing (in-list (append (place-things current-place)
                                                 player-inventory))]
                         #:when (eq? (thing-name thing) obj))
                        thing)
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
  (if (zero? (length player-inventory))
      (printf " no items.")
      (for-each (λ (thing)
                  (printf "\n  a ~a" (thing-name thing)))
                player-inventory))
  (printf "\n"))

;; Look for a command match in a list of verb--response pairs,
;; and returns the response thunk if a match is found:
(define (find-verb cmd actions)
  (for/first ([action (in-list actions)]
              #:when (memq cmd (verb-aliases (action-verb action))))
             (action-response action)))

;; Looks for a command in a list of verbs, and
;; applies `success-func' to the verb if one is found:
(define (using-verb cmd verbs success-func)
  (for/first ([verb (in-list verbs)]
              #:when (memq cmd (verb-aliases verb)))
             (success-func verb)))


;; ============================================================
;; To go:

(define (init-game in-place
                   in-verbs
                   in-everywhere-actions)
  (set! current-place in-place)
  (set! game-verbs in-verbs)
  (set! everywhere-actions in-everywhere-actions))
