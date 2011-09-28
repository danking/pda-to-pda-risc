#lang racket
(require rackunit
         "../pdarisc-data.rkt"
         "../parse-pdarisc.rkt")

(check-equal? (parse-pdarisc '((:= foo (pop))
                              (:= one two)
                              (:= 我 (state state-1))
                              (:= eat (nterm program))
                              (:= pie (current-token))
                              (accept)))
              (make-pdarisc
               (list (make-assign 'foo
                                  (make-pop))
                     (make-assign 'one
                                  (make-var-ref 'two))
                     (make-assign '我
                                  (make-state 'state-1))
                     (make-assign 'eat
                                  (make-nterm 'program))
                     (make-assign 'pie
                                  (make-curr-token #f))
                     (make-accept '()))))

(check-equal? (parse-pdarisc '((push me)
                              (push (state around))
                              (accept)))
              (make-pdarisc
               (list (make-push (make-var-ref 'me))
                     (make-push (make-state 'around))
                     (make-accept '()))))

(check-equal? (parse-pdarisc '((semantic-action (exp exps)
                                               (yahoo #f)
                                               (values (cons exp exps)
                                                       'nothin-to-see-here))
                              (stack-ensure 3)
                              (accept)))
              (make-pdarisc
               (list (make-sem-act (list 'exp 'exps)
                                   (list 'yahoo #f)
                                   '(values (cons exp exps)
                                            'nothin-to-see-here))
                     (make-stack-ensure 3)
                     (make-accept '()))))

(check-equal? (parse-pdarisc '((block (:= foo (pop))
                                     drop-token
                                     get-token)
                              (accept)))
              (make-pdarisc
               (list (make-block
                      (list
                       (make-assign 'foo (make-pop))
                       (make-drop-token)
                       (make-get-token)))
                     (make-accept '()))))

(check-equal? (parse-pdarisc '((block (:= foo (pop))
                                     drop-token
                                     get-token
                                     (accept))))
              (make-pdarisc
               (list (make-block*
                      (list
                       (make-assign 'foo (make-pop))
                       (make-drop-token)
                       (make-get-token)
                       (make-accept '()))))))

(check-equal? (parse-pdarisc '((label ((hiphoppop : ((STATE foo)) #f
                                                 (foo bar)
                                                 (:= hiphop (pop))
                                                 (push foo)
                                                 (push bar)
                                                 (accept))
                                      (indirection : () #f
                                                   ()
                                                   (go hiphoppop
                                                       (nterm kanye)
                                                       (nterm jay-z))))
                                     (go indirection))))
              (make-pdarisc
               (list (make-label
                      '(hiphoppop indirection)
                      '(((STATE foo)) ())
                      '(#f #f)
                      '((foo bar) ())
                      (list (list
                             (make-assign 'hiphop
                                          (make-pop))
                             (make-push (make-var-ref 'foo))
                             (make-push (make-var-ref 'bar))
                             (make-accept '()))
                            (list
                             (make-go 'hiphoppop
                                      (list
                                       (make-nterm 'kanye)
                                       (make-nterm 'jay-z)))))
                      (list (make-go 'indirection (list)))))))

(check-equal? (parse-pdarisc '((if-eos (go secret-stuff)
                                      (block get-token drop-token (go foo)))))
              (make-pdarisc
               (list (make-if-eos (make-go 'secret-stuff (list))
                                  (make-block*
                                   (list (make-get-token)
                                         (make-drop-token)
                                         (make-go 'foo (list))))))))

(check-equal? (parse-pdarisc '((state-case what-am-i
                                          (pink (go pink-stuff))
                                          (blue drop-token (go azure))
                                          (green (go soylent!)))))
              (make-pdarisc
               (list (make-state-case
                      'what-am-i
                      '(pink blue green)
                      (list (list (make-go 'pink-stuff '()))
                            (list (make-drop-token)
                                  (make-go 'azure '()))
                            (list (make-go 'soylent! '())))))))

(check-equal? (parse-pdarisc '((token-case
                               (small (push (current-token))
                                      (go tiny teeny weeny))
                               (big drop-token (go big!))
                               (home (:= you out) (go home)))))
              (make-pdarisc
               (list (make-token-case
                      '(small big home)
                      (list (list (make-push
                                   (make-curr-token #f))
                                  (make-go
                                   'tiny
                                   (list
                                    (make-var-ref 'teeny)
                                    (make-var-ref 'weeny))))
                            (list (make-drop-token)
                                  (make-go 'big! '()))
                            (list (make-assign
                                   'you
                                   (make-var-ref 'out))
                                  (make-go 'home '())))))))
