#lang racket
(require rackunit
         "../parse-pda.rkt"
         "../compile-pda.rkt"
         "../compile-pdarisc.rkt")

(check-equal? (eval
               `(begin
                  (define-struct nterm (id))
                  (define-struct state (id))
                  (,(compile-pdarisc
                     (compile-pda
                      (parse-pda '((TOKENS A B $eos)
                                   (EOS $eos)
                                   (START s1)
                                   (STATE s1 : (())
                                          (SHIFT (A) s2)
                                          (GOTO start s6))
                                   (STATE s2 : (s : ((A s1 ())
                                                     (A s2 s)))
                                          (SHIFT (A) s2)
                                          (SHIFT (B) s3)
                                          (GOTO start s4))
                                   (STATE s3 : (s : ((B s2 A s1 ())
                                                     (B s2 A s2 s)))
                                          (REDUCE () r2))
                                   (STATE s4 : (s : ((start s4 A s1 ())
                                                     (start s4 A s2 s)))
                                          (SHIFT (B) s5))
                                   (STATE s5 : (s : ((B s5 start s4 A s1 ())
                                                     (B s5 start s4 A s2 s)))
                                          (REDUCE () r1))
                                   (STATE s6 : (s : ((start s1 ())))
                                          (ACCEPT ($eos)))
                                   (RULE r1 : (s : ((B s5 start s4 A s2 s)))
                                         start  (#f v2 #f) (+ 2 v2))
                                   (RULE r2 : (s : ((B s2 A s2 s)))
                                         start  (#f #f) 2)))))
                   '(A A A A A B B B B B)
                   #f
                   (hasheq)
                   '()))
               (module->namespace 'racket))
             '(10))