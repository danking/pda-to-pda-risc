#lang racket
(require rackunit
         "../pdarisc-data.rkt"
         "../unparse-pdarisc.rkt")

(check-equal? (unparse-pdarisc (make-pdarisc
                                (list (make-assign (make-named-reg 'foo)
                                                   (make-pop))
                                      (make-assign (make-named-reg 'one)
                                                   (make-named-reg 'two))
                                      (make-assign (make-named-reg '我)
                                                   (make-state 'state-1))
                                      (make-assign (make-named-reg 'eat)
                                                   (make-nterm 'program))
                                      (make-assign (make-named-reg 'pie)
                                                   (make-curr-token #f))
                                      (make-assign (make-nameless-reg)
                                                   (make-pop))
                                      (make-accept '()))))
              '((:= foo (pop))
                (:= one two)
                (:= 我 (state state-1))
                (:= eat (nterm program))
                (:= pie (current-token))
                (:= <nameless-register> (pop))
                (accept)))

(check-equal? (unparse-pdarisc (make-pdarisc
                                (list (make-push (make-named-reg 'me))
                                      (make-push (make-state 'around))
                                      (make-accept '()))))
              '((push me)
                (push (state around))
                (accept)))

(check-equal? (unparse-pdarisc (make-pdarisc
                                (list (make-accept (list
                                                    (make-named-reg 'foo)
                                                    (make-named-reg 'bar))))))
              '((accept foo bar)))

(check-equal? (unparse-pdarisc (make-pdarisc
                                (list
                                 (make-sem-act (list (make-named-reg 'exp)
                                                     (make-named-reg 'exps))
                                               (list (make-named-reg 'yahoo)
                                                     #f)
                                               '(values (cons exp exps)
                                                        'nothin-to-see-here))
                                 (make-stack-ensure 3)
                                 (make-accept '()))))
              '((semantic-action (exp exps)
                                 (yahoo #f)
                                 (values (cons exp exps)
                                         'nothin-to-see-here))
                (stack-ensure 3)
                (accept)))

(check-equal? (unparse-pdarisc (make-pdarisc
                                (list (make-block
                                       (list
                                        (make-assign (make-named-reg 'foo)
                                                     (make-pop))
                                        (make-drop-token)
                                        (make-get-token)))
                                      (make-accept '()))))
              '((block (:= foo (pop))
                       drop-token
                       get-token)
                (accept)))

(check-equal? (unparse-pdarisc (make-pdarisc
                                (list (make-block*
                                       (list
                                        (make-assign (make-named-reg 'foo)
                                                     (make-pop))
                                        (make-drop-token)
                                        (make-get-token)
                                        (make-accept '()))))))
              '((block (:= foo (pop))
                       drop-token
                       get-token
                       (accept))))

(check-equal? (unparse-pdarisc (make-pdarisc
                                (list (make-label
                                       (list (make-label-name 'hiphoppop)
                                             (make-label-name 'indirection))
                                       '(((NTERM id) tok (STATE foo)) ())
                                       '(#f plus-sign)
                                       `((,(make-named-reg 'foo)
                                          ,(make-named-reg 'bar))
                                         ())
                                       (list
                                        (list
                                         (make-assign (make-named-reg 'hiphop)
                                                      (make-pop))
                                         (make-push (make-named-reg 'foo))
                                         (make-push (make-named-reg 'bar))
                                         (make-accept '()))
                                        (list
                                         (make-go
                                          (make-label-name 'hiphoppop)
                                          (list
                                           (make-nterm 'kanye)
                                           (make-nterm 'jay-z)))))
                                       (list (make-go
                                              (make-label-name 'indirection)
                                              (list)))))))
              '((label ((hiphoppop : ((NTERM id) tok (STATE foo)) #f
                                   (foo bar)
                                   (:= hiphop (pop))
                                   (push foo)
                                   (push bar)
                                   (accept))
                        (indirection : () plus-sign
                                     ()
                                     (go hiphoppop
                                         (nterm kanye)
                                         (nterm jay-z))))
                       (go indirection))))

(check-equal? (unparse-pdarisc (make-pdarisc
                                (list (make-if-eos
                                       (make-go (make-label-name 'secret-stuff)
                                                '())
                                       (make-block*
                                        (list (make-get-token)
                                              (make-drop-token)
                                              (make-go
                                               (make-label-name 'foo)
                                               '())))))))
              '((if-eos (go secret-stuff)
                        (block get-token drop-token (go foo)))))

(check-equal? (unparse-pdarisc (make-pdarisc
                                (list (make-state-case
                                       (make-named-reg 'what-am-i)
                                       (list (make-state 'pink)
                                             (make-state 'blue)
                                             (make-state 'green))
                                       (list
                                        (list (make-go
                                               (make-label-name 'pink-stuff)
                                               '()))
                                        (list (make-drop-token)
                                              (make-go
                                               (make-label-name 'azure)
                                               '()))
                                        (list (make-go
                                               (make-label-name 'soylent!)
                                               '())))))))
              '((state-case what-am-i
                            (pink (go pink-stuff))
                            (blue drop-token (go azure))
                            (green (go soylent!)))))

(check-equal? (unparse-pdarisc (make-pdarisc
                                (list (make-token-case
                                       '(small big home)
                                       (list (list (make-push
                                                    (make-curr-token #f))
                                                   (make-go
                                                    (make-label-name 'tiny)
                                                    (list
                                                     (make-named-reg 'teeny)
                                                     (make-named-reg 'weeny))))
                                             (list (make-drop-token)
                                                   (make-go
                                                    (make-label-name 'big!)
                                                    '()))
                                             (list (make-assign
                                                    (make-named-reg 'you)
                                                    (make-named-reg 'out))
                                                   (make-go
                                                    (make-label-name 'home)
                                                    '())))))))
              '((token-case
                 (small (push (current-token)) (go tiny teeny weeny))
                 (big drop-token (go big!))
                 (home (:= you out) (go home)))))
