#lang racket
;---------------------------------------------------------------------------;
;                        8 PUZZLE PROBLEM                                   ;
;                         BY: Cassie Kresnye                                ;
;                                                                           ;
;      SUMMARY: This program takes in a start and end state, and produces   ;
;      a possible path between the two                                      ;
;---------------------------------------------------------------------------;

;IMPORTANT NOTES------------------------------------------------------------

;MY REPRESENTATIONS USED '(1 2 3 4 5 6 7 8 0) - where the 0 is the space
;Information for this program is on the readme (which is stapled on top of this paper)


;--------------------------------------------------------------------------;
;                         MAIN Functions                                   ;
;--------------------------------------------------------------------------;
;This section has all the output fun stuff that the user interacts with.


;--------------------------------------------------------------------------;
;                               8PUZZLE                                    ;
;--------------------------------------------------------------------------;
;This is what the user interacts with, and wraps everything up nicely.
;This takes in a start state, goal state, and a search to apply to them.
;Nothing is returned by this function, just outputted to the console.

(define 8Puzzle(lambda (start goal search)
                 (intro start goal)
                 ;The double list is formatting the start state into the path as follows:
                 ; (         (              (START) )      )
                 ;    |            |           |
                 ;  FULLPATH     PATH        STATE
                 
                 (conclusion (run8Puzzle (list (list start)) goal search))
                 )
  )

;--------------------------------------------------------------------------;
;                               INTRO                                      ;
;--------------------------------------------------------------------------;
;This takes in the start state and end state, and prints them nicely to the screen.
;Nothing is returned from this functions, just output formatting.
(define intro(lambda (start goal)
               (newline)
                 (display "8 PUZZLE SOLVER")
                 (newline)
                 (display "By: Cassie Kresnye")
                 (newline)
                 (display "Start: ")
                 (print start)
                 (newline)
                 (display "End Goal: ")
                 (print goal)
                 (newline)
                 )
  )

;--------------------------------------------------------------------------;
;                             CONCLUSION                                   ;
;--------------------------------------------------------------------------;
;This takes the resulting path of the program, and prints out the number of moves the path takes
(define conclusion(lambda (path)
                    (newline)(display "Total Moves: ")
                    (print (- (length path) 1))
                    (newline)
                    (display "Best Solution Found: ")
                    (pathPrinter path);prints out visual tiles
                    (newline)
                    (print (reverse path))
                    (newline)
                    
                    (newline)
                    (newline)
                    (display"Thank you for using this lovely program!")
                    )
  )
;--------------------------------------------------------------------------;
;                             PATHPRINTER                                  ;
;--------------------------------------------------------------------------;
;This takes in a path, and prints out the steps in a 'graphic' (ascii) format
;nothing is returned from this
(define pathPrinter(lambda (path)
                     (cond
                       ((null? (cdr path));if last step, no need for arror
                        (tilePrinter (car path))
                        )
                       ((not(null? path))
                       (newline)
                       (tilePrinter (car path))
                       (newline)
                       (display "      |      ")(newline)
                       (display "      |      ")(newline)
                       (display "     \\ /")(newline)
                       (display "      *")(newline)
                       (pathPrinter (cdr path))
                       )
                     )
  )
  )

;--------------------------------------------------------------------------;
;                           TILE PRINTER                                   ;
;--------------------------------------------------------------------------;
;Given a state, this functions prints out a 'graphic' version to read it a little easier.

(define tilePrinter(lambda (state)
                     (display " ___________")(newline)
                     (display "|_") (display (car state)) (display "_|_") (display (cadr state)) (display "_|_") (display (caddr state))(display "_|")(newline)
                     (display "|_") (display (cadddr state)) (display "_|_") (display (cadddr (cdr state))) (display "_|_") (display (cadddr (cddr state)))(display "_|")(newline)
                     (display "|_") (display (cadddr (cdddr state))) (display "_|_") (display (cadddr (cddddr state))) (display "_|_") (display (cadddr (cddddr (cdr state))))(display "_|")(newline)
                     
                     )
  )

;-----------------------------------------------------------------------;
;                       8 PUZZLE RUNNER                                 ;
;-----------------------------------------------------------------------;
;This is the nitty gritty part that actually runs the algorithm. User does not interact with these portions


;--------------------------------------------------------------------------;
;                             RUN8PUZZLE                                   ;
;--------------------------------------------------------------------------;
;This is the wrapper that gets everything set up for the general search

(define run8Puzzle (lambda (path goal search)
                     ;(display "path: ")
                     ;(print path)
                     ;(newline)
                     ;(display "Goal: ")
                     ;(print goal)
                     ;------ACTUAL STUFF---------------------------

                     (generalSearch path goal search)
                     )
  )

;--------------------------------------------------------------------------;
;                          GENERALSEARCH                                   ;
;--------------------------------------------------------------------------;
; This is the function that does the search work.
;goal is in state form.
;This function returns the path that leads to the solution given in goal.

(define generalSearch(lambda (fullPath goal search)
                 ;(newline)
                 ;(newline)
                 ;(print "Next path: ")
                 ;(print fullPath)
                 ;(newline)
                 (cond
                   ;No paths left, so sadly no answer :(
                   ((null? fullPath)
                    '(OH NO))
                   ;Path found! return the front of the list
                   ((equal? goal (caar fullPath))
                    (car fullPath))
                   ;else keep looking
                   (else
                    (generalSearch (search fullPath goal) goal search))
                    )
                   )
                 )

;--------------------------------------------------------------------------;
;                                SEARCHES                                  ;
;--------------------------------------------------------------------------;
;These are the possible searches to use.
;NOTE: in DFS and BFS, the goal parameter is not used. This is just to allow
;the transfer of this parameter for the other two searches.

;--------------------------------------------------------------------------;
;                                   BFS                                    ;
;--------------------------------------------------------------------------;
;Breadth First Search
;This will attach the new paths to the end of the full path
;This returns the new full path

(define bfs(lambda (fullPath goal)
             (append (cdr fullPath) (successors (car fullPath)))
             )
  )
;--------------------------------------------------------------------------;
;                                   DFS                                    ;
;--------------------------------------------------------------------------;
;LONG RUN TIME WARNING!!
;Depth First Search
;This will attach the new paths to the beginning of the full path
;This returns the new fullpath

(define dfs(lambda (fullPath goal)
             (append (successors (car fullPath)) (cdr fullPath))
             )
  )

;--------------------------------------------------------------------------;
;                              BESTFIRST                                   ;
;--------------------------------------------------------------------------;
;Best First Search
;This sorts the list based on the Total Out of Place Heuristic
;This will return the new full path

(define bestFirst(lambda (fullPath goal)
                (putMostlyBestInFront (append (cdr fullPath) (successors (car fullPath))) goal)
                 
                 )
  )

;--------------------------------------------------------------------------;
;                                BABPLUS                                   ;
;--------------------------------------------------------------------------;
;Branch and Bound Plus
;This sorts the list based on the Total Out of Place Heuristic added to the current length of the path.
;This will return the new full path

(define babPlus(lambda (fullPath goal)
                  (putBestInFront (append (cdr fullPath) (successors (car fullPath))) goal)
                   )
  )

;------------------------------------------------------------------;
;                        SUCCESSOR FUNCTIONS                       ;
;------------------------------------------------------------------;
;To calculate the successor functions, I first rotate the 'board' until it is one of 3 positions (TOP LEFT, TOP MIDDLE, OR CENTER)

;next, I find all next moves for that states. These are then rotated out, and sent back out as a list of option
;finally, each possible state that is valid is added to the path, and returned from successors in full path form

;--------------------------------------------------------------------------;
;                             SUCCESSORS                                   ;
;--------------------------------------------------------------------------;
;This takes in a single path, and returns a fullpath format of the next possible states that are legal.

(define successors(lambda (path)
                    ;Get next possible states from first state,
                    ;Then check if any of the returned members are already in path
                    ;if not, make a path to that state
                    (addToPath path (findSpace (car path)))
  )
)

;--------------------------------------------------------------------------;
;                              ADDTOPATH                                   ;
;--------------------------------------------------------------------------;
; This takes in the path being manipulated, and a list of possible next states to be added to that path.
; It does this by crating a fullpath list of each state with the path, as long as it is not already in the path.

(define addToPath(lambda (path states)
                   
                   (cond
                     ;If states is null, we are done
                     ((null?  states) '())
                     (else
                      (cond
                        ;if it is already a member, don't add it
                        ((not(member (car states) path))
                         ;(print "Adding a path")
                         (append
                           (list (append (list (car states))  path))
                          (addToPath path (cdr states))
                          )
                          )
                        (else;else just keep going through the list
                         (addToPath path (cdr states)))
                        )
                     )
                     )
                   )
  )

;--------------------------------------------------------------------------;
;                             FINDSPACE                                    ;
;--------------------------------------------------------------------------;
;This functions finds where the space is, along with getting the possible next states for that space.
;This returns a list of possible next states.

(define findSpace(lambda (state)
                   (cond
                     ;If the top left is a blank
                       ((equal? 0 (car state))
                        (topLeftOptions state)
                        )
                       ;if the top middle is blank
                       ((equal? 0 (cadr state))
                        (topMiddleOptions state)
                        )
                       ;if the center is blank
                       ((equal? 0 (cadddr (cdr state)))
                        (centerOptions state)
                        )
                       (else ;rotate again
                        (turnAllCounterClockwise (findSpace (clockwiseRotation state)))
                        )
                       )
                   )
  )

;--------------------------------------------------------------------------;
;                            TILE OPTIONS                                  ;
;--------------------------------------------------------------------------;
;Takes in a state, and returns possible next states in the following format:
; given: (a b c)
;returned ((a b c d) (a b c e))
;NOTE: simply is a list of possible next, NOT a formated path

;--------------------------------------------------------------------------;
;                            TOPLEFTOPTION                                 ;
;--------------------------------------------------------------------------;
;This one iterates all moves for top left option, returning them in a list.
;(2 options)

(define topLeftOptions(lambda (state)
                        (append
                             ;swap with right tile
                            (list
                             (append
                             (list (cadr state))
                             (list (car state))
                             (cddr state)
                             ))
                            
                            ;swap with tile below
                            (list
                             (append
                                (list (cadddr state))
                                (list (cadr state))
                                (list (caddr state))
                                (list (car state))
                                (cddddr state)
                                     ))
                          )
                        )
  )

;--------------------------------------------------------------------------;
;                           TOPMIDDLEOPTION                                ;
;--------------------------------------------------------------------------;
;This one iterates all moves for top middle option
; (3 options)

(define topMiddleOptions(lambda (state)
                        (append
                             ;swap with left tile
                            (list
                             (append
                             (list (cadr state));2
                             (list (car state));1
                             (cddr state);rest of state
                             ))
                            
                            ;swap with tile below
                            (list
                             (append
                                (list (car state)) ;1
                                (list (cadddr (cdr state)));5
                                (list (caddr state));3
                                (list (cadddr state));4
                                (list (cadr state));2
                                (cddddr (cdr state));rest of state
                                     ))
                            ;swap with right tile
                            (list
                             (append
                                (list (car state))
                                (list (caddr state))
                                (list (cadr state))
                                (cdddr state)
                              )
                             )
                          )
                        )
  )

;--------------------------------------------------------------------------;
;                             CENTEROPTION                                 ;
;--------------------------------------------------------------------------;
;This one iterates all moves for center option
; (4 options)

(define centerOptions(lambda (state)
                        (append
                             
                            ;swap with tile above
                            (list
                             (append
                                (list (car state))
                                (list (cadddr (cdr state)))
                                (list (caddr state))
                                (list (cadddr state))
                                (list (cadr state))
                                (cddddr (cdr state))
                                     ))
                            ;swap with left tile
                            (list
                             (append
                              (list (car state))
                              (list (cadr state))
                              (list (caddr state))
                              (list (cadddr(cdr state)))
                              (list (cadddr state))
                              (cddddr (cdr state))
                              )
                             )
                            ;swap with right tile
                            (list
                             (append
                              (list (car state))
                              (list (cadr state))
                              (list (caddr state))
                              (list (cadddr state))
                              (list (cadddr(cddr state)))
                              (list (cadddr(cdr state)))
                              (cddddr (cddr state))
                              )
                             )
                            ;swap with tile below
                            (list
                             (append
                              (list (car state))
                              (list (cadr state))
                              (list (caddr state))
                              (list (cadddr state))
                              (list (cadddr (cddddr state)))
                              (list (cadddr(cddr state)))
                              (list (cadddr (cdddr state)))
                              (list (cadddr (cdr state)))
                              (cddddr (cddddr state))
                              
                              
                              )
                             )
                          )
                        )
  )

;--------------------------------------------------------------------------;
;                            OTHER FUNCTIONS                               ;
;--------------------------------------------------------------------------;
;This is other functions that do not fit into other sections

;--------------------------------------------------------------------------;
;                           CLOCKWISEROTATION                              ;
;--------------------------------------------------------------------------;
;This takes in a state, and returns that state 'rotated' clockwise.

(define clockwiseRotation(lambda (state)
                         (append
                          ;top row
                          (list (cadddr (cdddr state))) ;bottom left to top left
                          (list (cadddr state)) ; middle left to top middle
                          (list (car state)) ;top left to top right

                          ;middle row
                          (list (cadddr (cddddr state)));bottom middle to middle left
                          (list (cadddr (cdr state))); center stays the same
                          (list (cadr state)) ;top middle to middle right

                          ;bottom rom
                          (list (cadddr (cddddr (cdr state))));bottom right to bottom left
                          (list (cadddr (cddr state))) ; middle right to bottom middle
                          (list (caddr state)) ; top right to bottom right
                          )
                         )
  )

;--------------------------------------------------------------------------;
;                       TURNALLCOUNTERCLOCKWISE                            ;
;--------------------------------------------------------------------------;
;takes in a list of states that need to be rotated
;returns same list that is in the same order, but rotated

(define turnAllCounterClockwise(lambda (lst)
                                 (cond
                                   ((null? (cdr lst)) (list (counterClockwiseRotation (car lst))))
                                   (else (append (list (counterClockwiseRotation (car lst))) (turnAllCounterClockwise (cdr lst))))
                                   )
                                 )
  )

;--------------------------------------------------------------------------;
;                     COUNTERCLOCKWISEROTATION                             ;
;--------------------------------------------------------------------------;
;Takes in a state, and returns that state rotated counterclockwise once.

(define counterClockwiseRotation(lambda (state)

                                  (append
                                   ;top row
                                   (list (caddr state)) ; top right to top left
                                   (list (cadddr (cddr state))); middle right to top middle
                                   (list (cadddr (cddddr (cdr state)))) ; bottom right to top right
                                   
                                   ;middle row
                                   (list (cadr state)) ;top middle to middle left
                                   (list (cadddr (cdr state))); center stays the same
                                   (list (cadddr (cddddr state))); bottom middle to middle right
                                   
                                   ;bottom row
                                   (list (car state));top left to bottom left
                                   (list (cadddr state)) ; middle left to bottom middle
                                   (list (cadddr (cdddr state))) ; bottom left to bottom right
                                   )
                                  )
  )

;--------------------------------------------------------------------------;
;                           PUTBESTINFRONT                                 ;
;--------------------------------------------------------------------------;
;orders the inputted full path based on length of path AND number of tiles out of place

(define putBestInFront(lambda (fullPath goal)
                         (smallestInFrontHelper (cdr fullPath) (car fullPath) goal)
                         
                         ) )

;--------------------------------------------------------------------------;
;                        PUTMOSTLYBESTINFRONT                              ;
;--------------------------------------------------------------------------;
;h(x) is based on number of tiles out of place only

(define putMostlyBestInFront(lambda (fullPath goal)
                         (smallestInFrontMostlyHelper (cdr fullPath) (car fullPath) goal)
                         
                         ) )

;--------------------------------------------------------------------------;
;                      SMALLESTINFRONTHELPER                               ;
;--------------------------------------------------------------------------;
;Helper functions for putbestinfront.
;compares each path, saving the smallest costing path to be placed in the front of the returned list

(define smallestInFrontHelper (lambda (lst current goal)
                                (cond
                                  ;if null, current is smallest
                                  ((null? lst) (list current))
                                  ;if new is smaller than current, use new instead
                                  ((< (+ (length (car lst)) (- 9 (numInPlace (caar lst) goal))) (+ (length current) (- 9 (numInPlace (car current) goal))))
                                   (append (smallestInFrontHelper (cdr lst) (car lst) goal) (list current)))
                                  ;else keep current as smallest
                                  (else
                                   (append (smallestInFrontHelper(cdr lst) current goal) (list (car lst)))
                                   )
                                  )
                                )
  )
;--------------------------------------------------------------------------;
;                     SMALLESTINFRONTMOSTLYHELPER                          ;
;--------------------------------------------------------------------------;
;Helper functions for putmostlybestinfront.
;compares each path, then places the smallest costing on infront of the returned list

(define smallestInFrontMostlyHelper (lambda (lst current goal)
                                (cond
                                  ;if null, current is smallest
                                  ((null? lst) (list current))
                                  ;if new is smaller than current, use new instead
                                  ((< (- 9 (numInPlace (caar lst) goal)) (- 9 (numInPlace (car current) goal)))
                                   (append (smallestInFrontHelper (cdr lst) (car lst) goal) (list current)))
                                  ;else keep current as smallest
                                  (else
                                   (append (smallestInFrontHelper(cdr lst) current goal) (list (car lst)))
                                   )
                                  )
                                )
  )

;--------------------------------------------------------------------------;
;                               NUMINPLACE                                 ;
;--------------------------------------------------------------------------;
;returns the number of tiles in place
;compares the first in each list, then the second, and so on, and keeps track of those matched
(define numInPlace(lambda (state goal)
                       (cond
                         ((null? state) 0)
                         ((equal? (car state) (car goal))
                          (+ 1 (numInPlace (cdr state) (cdr goal))))
                         (else
                          (numInPlace (cdr state) (cdr goal)))
                         
                         )
                       )
  )                      
;------------------------------------------------------------------;
;                           TEST DATA                              ;
;------------------------------------------------------------------;
;My testing data taken from the HW page
(define ss '(1 2 3 8 4 5 0 7 6))
(define ss2 '(2 3 8 1 7 4 6 5 0)) 
(define ss3 '(8 1 0 7 5 3 6 4 2))
(define gg '(1 2 3 8 0 4 7 6 5))
(define goal '(1 2 3 4 5 6 7 8 0))
(define test '(2 3 4 1 8 5 7 6 0))

;(8Puzzle test gg bfs)
;(8Puzzle ss3 gg babPlus)
;(8Puzzle test gg bestFirst)
;(8Puzzle ss gg dfs)
;-------------------------------------------------------------------;
;                            USE                                    ;
;-------------------------------------------------------------------;

(8Puzzle START GOAL SEARCH)
