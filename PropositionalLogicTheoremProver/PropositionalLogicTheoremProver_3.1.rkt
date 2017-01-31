#lang racket
;-----------------------------------------------------------------;
;         Propositional Logic Theorem Prover                      ;
;                By: Cassie Kresnye                               ;
;                                                                 ;
;  This program takes in axioms and a negated conclusion, and then;
; produces whether the proof is possible, and if so, gives the    ;
; axioms that it took to get there.                               ;
;                                                                 ;
;-----------------------------------------------------------------;

;NOTES
;NOTE 1:
;  ----My representation in statement form------
;          p -> q : ((#t p)(#t q))
;         ~p -> -q: ((#f p)(#f q))
;
; NOTES: Since only ORs are allowed in CNF, it is assumed in a nexted
;        statement that the 2 statements are being OR'd.
;
;     (p V q) -> s: (((#t p)(#t q))(#t s))
; (p V q) V s -> t: (( ((#t p) (#t q)) (#t s))(#t t))
;
;NOTE 2:
; (car fullpath) -> an axiom

;---------------------PLTP TOP LEVEL-------------------------------;

;--------------------------PLTP FUNCTION---------------------------;
;                       -----PARAMETERS-----                       ;
; path - these are the given axioms to mess with.                  ;
; toProve - This is the pre-negated axiom that is to proven wrong  ;
;                                                                  ;
;                       ------RETURNS-------                       ;
;                             Nothing                              ;
;                                                                  ;
;                       -------ABOUT--------                       ;
; This functions is the main functions of the program that the user;
; can call. It takes in a list of starter statements, and then     ;
; prints out a possible path to prove.                             ;
;                                                                  ;
;                       -------NOTES--------                       ;
; -best of luck!                                                    ;
; -Uses the function pltpHelper.                                    ;
;------------------------------------------------------------------;
(define pltp(lambda(path toProve)
              (cond
                ;no axioms, so can't do anything.
                ((null? path)
                 (display "I need some axioms to start with."))
                (else
                 (pltpHelper (append path toProve))
                 )
                )
              )
)

;------------------------PLTPHELPER FUNCTION-----------------------;
;                       -----PARAMETERS-----                       ;
; path - This is the total axiom list, including the thing to be   ;
;        proven wrong.                                             ;
;                                                                  ;
;                       ------RETURNS-------                       ;
;                             Nothing                              ;
;                                                                  ;
;                       -------ABOUT--------                       ;
; This function is the one that actually iterates through the      ;
; axioms and tries to find a contridiction in them. If none is     ;
; found, it uses the resolution rule to create more axioms and     ;
; tries again. If found, the result is printed. Else, the user will;
; be notified that nothing was found.                              ;
;                                                                  ;
;                       -------NOTES--------                       ;
; -This function uses contridiction?, checkSingles, and            ;
;      printAxiom.                                                 ;
; -This function is the helper function for pltp.                  ;
;                                                                  ;
;------------------------------------------------------------------;

(define pltpHelper(
                   lambda (path)
                    (cond
                      ;This is not very good...
                      ((null? path)
                       (display "I think an error has occured... null path in pltpHelper")
                       )
                      (else
                       ;this is the list of current path plus the newly created axioms, minus duplicates
                       (define possibleNext (remove-duplicates (append path (runThrough path))))
                       (cond
                         
                         ;if no duplicates were added
                         ((equal? path  possibleNext)
                              (display "No path has been found, try some different axioms! :D"))
                         
                         ;if a contridiction was found
                         ((or (contridiction? possibleNext) (checkSingles possibleNext))
                              (display "--SOLUTION--")
                             (newline)
                             (printAxioms 0  possibleNext)
                         )
                         ;Else try again
                          (else
                          (pltpHelper possibleNext))
                          
                          )
                       )
                      )
                    )
  )

;-----------------------PRINTAXIOMS FUNCTION-----------------------;
;                       -----PARAMETERS-----                       ;
; index - This is the starting index to be incremented by one for  ;
;         each axiom in a given list.                              ;
; path - This is the list of axioms to be printed.                 ;
;                                                                  ;
;                       ------RETURNS-------                       ;
;                             Nothing                              ;
;                                                                  ;
;                       -------ABOUT--------                       ;
; This function prints out a list of axioms in an easy to read     ;
; format with an index designated before each axiom.               ;
;                                                                  ;
;                       -------NOTES--------                       ;
;                                                                  ;
;------------------------------------------------------------------;
(define printAxioms(
                    lambda (index path)
                     (cond
                       ;if null, done
                       ((null? path)
                        (newline)
                        (display "Thanks for using my prover!"))
                       (else
                        (display "Axiom ")
                        (display index)
                        (display ": ")
                        (display (car path))
                        (newline)
                        ;go to next axiom
                        (printAxioms (+ index 1) (cdr path))
                       )
                       )
                     
                     )
  )

;--------------------CONTRIDICTION? FUNCTION-----------------------;
;                       -----PARAMETERS-----                       ;
; path - This is the list of axioms to find a controdiction in.    ;
;                                                                  ;
;                       ------RETURNS-------                       ;
;                              #T/#f                               ;
;                                                                  ;
;                       -------ABOUT--------                       ;
; This function takes in a list of axioms, and determines if in    ;
; any of the given axioms there is a contridiction. Will return    ;
; true if a contridiction is found.                                ;
;                                                                  ;
;                       -------NOTES--------                       ;
; -This uses a helper function contridictionHelper?.               ;
;                                                                  ;
;------------------------------------------------------------------;

(define contridiction?(
                       lambda (path)
                        (cond
                          ;if null, nothing found
                          ((null? path) #f)
                          (else
                             (cond
                               ;contridiction found
                               ((contridictionHelper? (car path)) #t)
                               ;else check next axiom
                               (else (contridiction? (cdr path)))
                               )
                           )
                          )
                        
                        )
  )

;------------------CONTRIDICTIONHELPER? FUNCTION-------------------;
;                       -----PARAMETERS-----                       ;
; axiom - This is the axiom to find a contridiction in.            ;
;                                                                  ;
;                       ------RETURNS-------                       ;
;                              #T/#f                               ;
;                                                                  ;
;                       -------ABOUT--------                       ;
; This functions determines if the given axiom has a contridiction ;
; in it. If so, returns true.                                      ;
;                                                                  ;
;                       -------NOTES--------                       ;
; -is a helper function for Contridiction?                         ;
; - Uses function resolverPair?                                    ;
;                                                                  ;
;------------------------------------------------------------------;
(define contridictionHelper?(
                       lambda (axiom)
                        (cond
                          ;empty? then no contridiction found
                          ((null? (cdr axiom)) #f)
                          ; if true, then contridiction!!
                          ((resolverPair? (car axiom) (cdr axiom)) #t)
                          ;else try again
                          (else (contridictionHelper? (cdr axiom)))
                          )
                        
                        )
  )

;----------------------CHECKSINGLES FUNCTION-----------------------;
;                       -----PARAMETERS-----                       ;
; path - This is the list of axioms to check for single            ;
;         contridictions in.                                       ;
;                                                                  ;
;                       ------RETURNS-------                       ;
;                              #T/#f                               ;
;                                                                  ;
;                       -------ABOUT--------                       ;
; This functions checks a given list of axioms for singles that    ;
; contridict each other.                                           ;
;                                                                  ;
;                       -------NOTES--------                       ;
; -So this was fully added due to the time consraint. It is        ;
;    honestly the WORST way to do this, but I don't have time to   ;
;    change Contridiction?, so this will have to do.               ;
; - uses a helper function checkSinglesHelper                      ;
;                                                                  ;
;------------------------------------------------------------------;
(define checkSingles(
                     lambda (path)
                      (cond
                        ;null? then nothing found
                        ((null? path) #f)
                        ; if a single, check it against everything
                        ((equal? 1 (length (car path)))
                         (cond
                           ;if true, found contridiction
                           ((checkSinglesHelper (caar path) (cdr path))
                            #t)
                           (else
                            ;else keep looking
                            (checkSingles (cdr path))
                            )
                           )
                            
                        )
                        (else
                         ;keep looking
                         (checkSingles (cdr path))
                         )
                        )
                      )
  )

;---------------------CHECKSINGLEHELPER FUNCTION-------------------;
;                       -----PARAMETERS-----                       ;
; single - This is the single axiom to compare the rest to.        ;
; path - This is the list of axioms.                               ;
;                                                                  ;
;                       ------RETURNS-------                       ;
;                              #T/#f                               ;
;                                                                  ;
;                       -------ABOUT--------                       ;
; This functions returns true if single contridicts another axiom  ;
; in the path list.                                                ;
;                       -------NOTES--------                       ;
; -This is a helper function for checkSingles                      ;
;                                                                  ;
;------------------------------------------------------------------;
(define checkSinglesHelper(
                       lambda (single path)
                        (cond
                          ;if null, then done
                          ((null? path) #f)
                          ;if equal, then we found a contridiction!
                          ((equal? (append (list (not (car single))) (list (cadr single))) (caar path))
                           #t
                           )
                          ;else, keep looking
                          (else
                           (checkSinglesHelper single (cdr path))
                          )
                          )
                        
                        )
  )

;-----------------------RUNTHROUGH FUNCTION------------------------;
;                       -----PARAMETERS-----                       ;
; path - This is the axioms to runthough and find new ones from.   ;
;                                                                  ;
;                       ------RETURNS-------                       ;
; This function returns a list of possible new axioms to add.      ;
;                                                                  ;
;                       -------ABOUT--------                       ;
; This function iterates through the axioms to find new axioms     ;
; based on resolutions.                                            ;
;                                                                  ;
;                       -------NOTES--------                       ;
; -This function used a helper called runThroughHelper.            ;
;                                                                  ;
;------------------------------------------------------------------;

(define runThrough(
                   lambda (path)
                    (cond
                      ((null? path) '())
                      (else
                       (append (runThroughHelper (car path) (cdr path)) (runThrough (cdr path)))
                       )
                      )
                          )
  )

;--------------------RUNTHROUGHHELPER FUNCTION---------------------;
;                       -----PARAMETERS-----                       ;
; axiom - This is the axiom being compared to the rest.            ;
; path - This is a list of the rest of axioms to compare to.       ;
;                                                                  ;
;                       ------RETURNS-------                       ;
; This function returns a list of possible axioms from the given   ;
; parameters.                                                      ;
;                                                                  ;
;                       -------ABOUT--------                       ;
; This functions creates new axioms based on the given parameters  ;
; through the resolution rule.                                     ;
;                                                                  ;
;                       -------NOTES--------                       ;
; -This uses the function resolvable?                              ;
; -This is a helper function for runThrough.                       ;
;                                                                  ;
;------------------------------------------------------------------;

(define runThroughHelper(
                         lambda (axiom path)
                          (cond
                            ((null? path)'())
                            (else
                             (cond
                               ;if it is resolvable
                               ((resolvable? axiom (car path))
                                (define resolvedBit (resolver axiom (car path)))
                                
                                (cond
                                  ((null? resolvedBit)
                                   (runThroughHelper axiom (cdr path)))
                                  (else
                                   (append
                                    (list resolvedBit)
                                    (runThroughHelper axiom (cdr path))
                                    )
                                   )
                                )
                                )
                               ;else keep looking
                               (else
                                (runThroughHelper axiom (cdr path))
                               )
                            )
                          
                          )
                        )
                   )
  )

;-------------------------RESOLVER FUNCTION------------------------; 
;                       -----PARAMETERS-----                       ;
; axiom1 - This is the first axiom that will be used to resolve.   ;
; axiom2 - This is the second axiom that will be used to resolve.  ;
;                                                                  ;
;                       ------RETURNS-------                       ;
; This function returns a list of possible new axioms.             ;
;                                                                  ;
;                       -------ABOUT--------                       ;
; This function creates new axioms based on the given axioms.      ;
;                                                                  ;
;                       -------NOTES--------                       ;
; - This function has a helper function ResolverHelper.            ;
;                                                                  ;
;------------------------------------------------------------------;

(define resolver(lambda (axiom1 axiom2)
                 ; (display "in Resolver: ")
                  ;(newline)
                   (resolverHelper (remove-duplicates (append axiom1 axiom2)))
                  
              )
  )

;-----------------------RESOLVERHELPER FUNCTION--------------------; 
;                       -----PARAMETERS-----                       ;
; dirtyList - This is the list of mashed axioms to search through. ;
;                                                                  ;
;                       ------RETURNS-------                       ;
; This function returns a list of possible new axioms.             ;
;                                                                  ;
;                       -------ABOUT--------                       ;
; This function creates new axioms based on the given axioms.      ;
;                                                                  ;
;                       -------NOTES--------                       ;
; - This funciton used resolverPair?.                              ;
; - This function is a helper function for Resolver.               ;
;                                                                  ;
;------------------------------------------------------------------;

(define resolverHelper(lambda (dirtyList)
                        ;(display "in ResolverHelper: ")
                       ; (newline)
                        (cond
                          ;if null, done checking
                          ((null? dirtyList) '())
                          (else
                           (cond
                             ((resolverPair? (car dirtyList) (cdr dirtyList))
                              (remove (append (list (not (caar dirtyList))) (list (cadar dirtyList))) (cdr dirtyList))
                              )
                             (else
                              (append (list (car dirtyList)) (resolverHelper (cdr dirtyList)))
                              )
                             )
                           )
                          )
                        )
  )

;----------------------RESOLVERPAIR FUNCTION-----------------------; 
;                       -----PARAMETERS-----                       ;
; compare - The statement to compare the other statements to.      ;
; dirtyList - the rest of the statements to be compared.           ;
;                                                                  ;
;                       ------RETURNS-------                       ;
;                              #t/#f                               ;
;                                                                  ;
;                       -------ABOUT--------                       ;
; This function determines if 2 statments are negated versions of  ;
; each other.                                                      ;
;                                                                  ;
;                       -------NOTES--------                       ;
;                                                                  ;
;------------------------------------------------------------------;

(define resolverPair?(lambda (compare dirtyList)
                         (cond
                           ((null? dirtyList) #f)
                           ((equal? (append (list (not (car compare))) (list (cadr compare))) (car dirtyList))
                            #t
                            )
                           (else
                            (resolverPair? compare (cdr dirtyList)))
                          )
                         )
  )
  

;----------------------RESOLVABLE? FUNCTION------------------------;
;                       -----PARAMETERS-----                       ;
; axiom 1 - an Axoim that is compared.                             ;
; axiom 2 - an Axoim that is compared.                             ;
;                                                                  ;
;                       ------RETURNS-------                       ;
;                              #T/#F                               ;
;                                                                  ;
;                       -------ABOUT--------                       ;
; This functions compares to see if the 2 given axioms are         ;
; resolvable or not. If they are, it returns true. else, false.    ;
;                                                                  ;
;                       -------NOTES--------                       ;
; -This function uses helper resolvableHelper in determining if a  ;
;  list of values has a matching set recursively.                  ;
;------------------------------------------------------------------;

(define resolvable?(lambda (axiom1 axiom2)
                     (resolvableHelper (product axiom1 axiom2))
                        )
  )

;---------------------RESOLVABLEHELPER FUNCTION--------------------;
;                       -----PARAMETERS-----                       ;
; crossProduct - This is the crossproduct to find negations in.    ;
;                                                                  ;
;                       ------RETURNS-------                       ;
;                              #T/#F                               ;
;                                                                  ;
;                       -------ABOUT--------                       ;
; This is a helper function for the resolvable? function. This     ;
; takes in the crossproduct from resolvable? and iterates through  ;
; the pairs to see if a negated pair exists.                       ;
;                                                                  ;
;                       -------NOTES--------                       ;
; - This function uses the function negatedPair.                   ;
;------------------------------------------------------------------;

(define resolvableHelper(lambda (crossProduct)
                          (cond
                            ((null? crossProduct) #f)
                            ((negatedPair? (car crossProduct)) #t)
                            (else
                             (resolvableHelper (cdr crossProduct)))
                            )
                          )
  )

;-----------------------NEGATEDPAIR? FUNCTION----------------------;
;                       -----PARAMETERS-----                       ;
; Pairs - This is a pair of 2 variables of the axioms from         ;
;         resolvableHelper that will be looked at.                 ;
;                                                                  ;
;                       ------RETURNS-------                       ;
;                              #T/#F                               ;
;                                                                  ;
;                       -------ABOUT--------                       ;
; This functions takes in a pair of variables, and determines if   ;
; they are the negated version of each other. If they are, returns ;
; true, else will return false.                                    ;
;                                                                  ;
;                       -------NOTES--------                       ;
;                                                                  ;
;------------------------------------------------------------------;
(define negatedPair?(lambda (pairs)
              (cond
                ((equal? (cadr pairs) (append (list (not (caar pairs))) (cdar pairs)))
                 #t)
                (else  #f)
                
                )
              )
  )

;-------------------------PRODUCT FUNCTION-------------------------;
;                       -----PARAMETERS-----                       ;
; los1 - first list of atoms to cross to the second.               ;
; los2 - second list of atoms to cross to the first.               ;
;                                                                  ;
;                       ------RETURNS-------                       ;
; This function returns the cross product of the inputed lists.    ;
;                                                                  ;
;                       -------ABOUT--------                       ;
; This functions pairs each element of one list to every element in;
; the other list, then returns the result.                         ;
;                                                                  ;
;                       -------NOTES--------                       ;
; -This function uses the helper productHelper.                    ;
; -In this implementation, this is used to cross axioms.           ;
;                                                                  ;
;------------------------------------------------------------------;

(define product(lambda (los1 los2)
                (cond
                  ((null? los1) '());if null,then we're done
                  (else ;else, for each s in los1, run it against los2 in the helper
                   (append (productHelper (car los1) los2) (product (cdr los1) los2))
                   )
 )
  )
  )

;----------------------PRODUCTHELPER FUNCTION----------------------;
;                       -----PARAMETERS-----                       ;
; atm - This is the atom that will be paired with each item of lst.;
; lst - This is the list atm is matched with.                      ;
;                                                                  ;
;                       ------RETURNS-------                       ;
; This function returns a list of atm being paired to each element ;
; in lst.                                                          ;
;                                                                  ;
;                       -------ABOUT--------                       ;
; This functions takes in atm and list, then pairs each element of ;
; lst to atm. This result is stored into a list denoting each pair,;
; and is returned.                                                 ;
;                       -------NOTES--------                       ;
;                                                                  ;
;------------------------------------------------------------------;

(define productHelper(lambda(atm lst)
                (cond
                  ((null? lst) '())
                  (else
                   (cons (cons atm (list (car lst))) (productHelper atm (cdr lst)))
                   )
                  )        
             )
  )
;TESTS-------------------------------------------------------------------------
;This says:
; 1 - T or ~P
; 2 - P or ~G
;(define x '((#t f) (#f p) (#t p) (#t g)))
;(define axiomA '((#t t) (#f p)))
;(define axiomB '((#t p) (#t g)))
;(define axiomC '((#f g) (#f t)))
;(define testAxiom (append (list axiomA) (list axiomB) (list axiomC)))

;TEST SET 1 - 3.1 PASSED!
(define testPath '(((#f r) (#t u)) ((#f u) (#f w)) ((#t r) (#f w))))
(define testContra '(((#t w))))

;TEST SET 2 - 3.1 PASSED!
;(define test2-axioms '(((#f p) (#t q) (#t r)) ((#f  p) (#t q) (#f r))))
;(define test2-contra '(((#t p)) ((#f q))))

;TEST SET 3 - 3.1 PASSED!
;(define test3-axioms '(((#F p) (#T q)) ((#F q) (#F r) (#T s))))
;(define test3-contra '(((#T p)) ((#T r)) ((#F s))))

;TEST SET 4
;(define epp-axiom '(((#F p) (#T q)) ((#T r) (#T s)) ((#T s) (#F t)) ((#F q) (#T s)) ((#F s)) ((#T w) (#T t)) ((#T p) (#F  r) (#T u))))
;(define epp-contra '(((#F u) (#F w))))

;------------------------------------------------------------------;
;                          USER INPUT                              ;
;------------------------------------------------------------------;
;Input here!
(pltp testPath testContra)