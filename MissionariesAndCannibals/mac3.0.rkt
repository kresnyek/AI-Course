#lang racket
;--------------------------------------------------------------------------------------;
;       MISSIONARIES AND CANNIBALS                                                     ;
;        BY: Cassie Kresnye                                                            ;
;                                                                                      ;
; This program solves the MissIonaries and Cannibals problem                           ;
; (or at least the hypothetical one) using breadthfirst,                               ;
;  depthfirst, and branch and bound methods.                                           ;
;                                                                                      ;
; QUICK USE: Run MAC with the search you wish to preform                               ;
;                                                                                      ;
;--------------------------------------------------------------------------------------;
;NOTATION
; Full path = '(((a)) ((b)(a)))
; path = '((a) (b))
; state = '(a)

;---------------------MAC-------------------------------------------------------------
;This functions is the 'main', it formats and deligates the program
(define mac(lambda (start goal search)
             (display '(MISSIONARIES AND CANNIBALS) )
             (newline)
             (display '----------------------------------------------------------)
             (newline)
             (display 'START:_)
             (display start)
             (newline)
             (display 'GOAL:_)
             (display goal)
             (newline)
             (display '----------------------------------------------------------)
             (newline)
             (newline)
             
             (define output (search start goal))
             (display 'PATH:_)
             (display output)
             (newline)
             (display 'TOTAL_STATES:_)
             (display (length output))
             (newline)
            )
  )

;-------THE POSSIBLE SEARCHES----------------------------------------------------------
;BFS - Breadth First Search
; This takes in a full path parameter and a goal state
;Treats fullpath as Queue
;returns the goal path in path form 
(define bfs(lambda (lst goal)
             (cond
                ;if first path is goal, we're done!
               ((equal? (car (car lst)) goal) (reverse (car lst)))
               ;Very bad if gets here
               ((null? lst) '(THATS NOT GOOD))
               ;Else, check again till solution is found
               (else
                (bfs (append (cdr lst) (successor (car lst))) goal);this sends in a list that is
                ;the cdr with the newest successors on the back
                )
              )
             
             )
  )
;DFS - Depth First Search
; This takes in a full path parameter and a goal state
;Treats full path as a Stack
;returns the goal path in path form 
(define dfs (lambda (lst goal)
               (cond
                 ;if first path is goal, we're done!
               ((equal? (car (car lst)) goal) (car lst))
               ;Very bad
               ((null? lst) '(THATS NOT GOOD))
               (else
                (dfs (push (successor (car lst)) (cdr lst)) goal);this sends in a list that is
                ;the cdr with the newest successors on the front
                )
              )
              )
  )

;BNB - Branch n Bound (ish)
; This takes in a full path parameter and a goal state
;Sorts the smallest path to always be in the front
;returns the goal path in path form 

(define bnb(lambda (lst goal)

             (cond
               ;if first path is goal, we're done!
               ((equal? (caar lst) goal)(car lst))
               ;again, bad. no path
               ((null? lst) '(THATS NOT GOOD))
               ;Call again, with smallest on the front
               (else
                (bnb (smallestInFront (push (successor (car lst)) (cdr lst))) goal)
                )
               
               )
             )
  )
  
;----SEARCH HELPERS-----------------------------------------------------------------

;Push - pushes lst to front of lst2, and returns
(define push(lambda (lst lst2)
              (reverse(append (reverse lst2) lst))
              )
  )

;smallestInFront - Returns the same list with the smallest path in the front
(define smallestInFront(lambda (lst)
                         (smallestInFrontHelper (cdr lst) (car lst))
                         
                         ) )

;Helper functions for smallestInFront, does the recursive calling to find smallest
(define smallestInFrontHelper (lambda (lst current)
                                (cond
                                  ((null? lst) (list current))
                                  ((< (length (car lst)) (length current))
                                   (append (smallestInFrontHelper (cdr lst) (car lst)) (list current)))
                                  (else
                                   (append (smallestInFrontHelper(cdr lst) current) (list (car lst)))
                                   )
                                  )
                                )
  )


;--------------------SUCCESSOR FUNCTION---------------------------------------------------------------------
;recieves as path, returns all possible new paths branching from that path
;Does not return original path, only a fullpath of versions of the original
(define successor(lambda (path)
                   (cond
                     ((caddr(car path))
                      ;on west side
                      ;If on west, we can only move to the east (sub)
                      
                      (append
                            (successorHelper runRulesWest path subM)
                            (successorHelper runRulesWest path subC)
                            (successorHelper runRulesWest path sub2M)
                            (successorHelper runRulesWest path sub2C)
                            (successorHelper runRulesWest path subMC)
                       )
                      )
                      (else
                       ;on east side
                       ;if on east, we can only move west (add)
                       
                       (append
                            (successorHelper runRulesEast path addM)
                            (successorHelper runRulesEast path addC)
                            (successorHelper runRulesEast path add2M)
                            (successorHelper runRulesEast path add2C)
                            (successorHelper runRulesEast path addMC)
                    )
                   )
                   
                   )
                   )
 )

;This function runs the rules list RULES on the path that (MOVEFUNC PATH) returns.
;If it passes the rules and is currently not a member of the path, it will return
;else, returns '()
(define successorHelper(lambda (rules path moveFunc)
                         (cond ;if it passes the rules, add it
                           ((and (rules (moveFunc (car path)))(not (member (moveFunc (car path)) path)))
                            (list (append (list (moveFunc (car path))) path)))
                           (else ; return nothing
                            '()
                            )
                           )
                          
              )
  )
;-----RULES--------------------------------------------------------------------------------
;Both of these functions run a set of rules depending on what side of the river the current
;state is at

(define runRulesWest(lambda (state)
                      (cond
                        ;sub 1m and 2m check
                        ((> 0 (car state))#f)
                        ;sub 1c and 2c check
                        ((> 0 (cadr state))#F)
                        ;check if legal number of cannibals
                        ((and (< (car state) (cadr state)) (not (equal? (car state) 0))) #f)
                        ;check cannibals across river
                        ((and (< (- 3 (car state)) (- 3 (cadr state))) (not (equal? (car state) 3)))#f)
                        
                        (else #t)
                        )
 
  ))

(define runRulesEast(lambda (state)

                       (cond
                         ;add 1m and 2m check
                         ((< 3 (car state))#F)
                         ;add 1c and 2c check
                         ((< 3 (cadr state))#f)
                         ;cannibal check
                         ((and (< (- 3 (car state)) (- 3 (cadr state))) (not (equal? (car state) 3))) #f)
                         ;check the cannibals on the others ide
                         ((and (< (car state) (cadr state)) (not (equal? (car state) 0)))#f)
                         (else #t)
                         )
    )
  )
                      
  

;--Successor cases--------------------------------------------------------------------
;Each of these functions is a different possible next state for a given state.
;NOTE: these do no check for validity, these merely produce a possible next state
(define addM(lambda (state)
              (cons (+ 1 (car state)) (cons (cadr state) (list (not(caddr state)))))
                ))

(define subM(lambda (state)
              (cons (- (car state) 1) (cons (cadr state) (list (not(caddr state))))))
                )

(define addC(lambda (state)
              (cons (car state) (cons (+ 1 (cadr state)) (list (not (caddr state))))))
                )
 

(define subC(lambda (state)
              (cons (car state) (cons (- (cadr state) 1) (list (not(caddr state))))))
                )

(define add2M(lambda (state)
              (cons (+ 2 (car state)) (cons (cadr state) (list (not(caddr state)))))
                ))

(define sub2M(lambda (state)
              (cons (- (car state) 2) (cons (cadr state) (list (not(caddr state))))))
                )

(define add2C(lambda (state)
              (cons (car state) (cons (+ 2 (cadr state)) (list (not (caddr state))))))
                )
 

(define sub2C(lambda (state)
              (cons (car state) (cons (- (cadr state) 2) (list (not(caddr state))))))
                )

(define addMC(lambda (state)
              (cons (+ 1 (car state)) (cons (+ 1 (cadr state)) (list (not(caddr state)))))
                ))

(define subMC(lambda (state)
              (cons (- (car state) 1) (cons (- (cadr state) 1) (list (not(caddr state)))))
                ))
;---------------TEST------------------------------------------------------------
;TESTING STUFFS

;(bfs '(((3 3 #t))) '(0 0 #f))
;(dfs '(((3 3 #t))) '(0 0 #f))
;(bnb '(((3 3 #t))) '(0 0 #f))
;(define lst '(((c)(b)(a)) ((d)(c)(b)(a)) ((a) (b)) ((a) (b) (c) (d) (e))))
;(smallestInFront lst)

;(mac start goal bfs)
;(mac start goal dfs)
;(mac start goal bnb)

;-------------------RUNNERS------------------------------------------------------
(define start '(((3 3 #t))))
(define goal '(0 0 #f))
(mac start goal SEARCH) ; Replace search with the desired search
