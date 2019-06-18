;;;;;;;;;;;;;;
; Homework 2 ;
;;;;;;;;;;;;;;

;;;;;;;;;;;;;;
; Question 1 ;
;;;;;;;;;;;;;;

; BFS takes a tree list FRINGE and returns a list of leaf nodes ordered by
; when they get traversed in a DFS
; To do so the function reads a list left to right.
; If the head is an atom (i.e. a leaf node) it is removed from the list and
; BFS is called on the res of the list.
; If the head is another list (i.e. the head is a branch) it is moved to the
; end and evaluated later
(defun BFS (FRINGE)
    (cond 
        ; In case it is just one element return the element
        ((and (atom FRINGE) (NOT (listp FRINGE))) FRINGE)
        ; If head is not an atom move to end of list
        ((not (atom (car FRINGE))) (BFS (append (cdr FRINGE) (car FRINGE))))
        ; If head is atom separate it from calculation of the rest
        ((and (atom (car FRINGE)) (not (listp (car FRINGE)))) (cons (car FRINGE) (BFS (cdr FRINGE))))
        ; Otherwise just return NIL
        (t nil)
    )
)

;;;;;;;;;;;;;;
; Question 2 ;
;;;;;;;;;;;;;;


; These functions implement a depth-first solver for the homer-baby-dog-poison
; problem. In this implementation, a state is represented by a single list
; (homer baby dog poison), where each variable is T if the respective entity is
; on the west side of the river, and NIL if it is on the east side.
; Thus, the initial state for this problem is (NIL NIL NIL NIL) (everybody 
; is on the east side) and the goal state is (T T T T).

; The main entry point for this solver is the function DFS, which is called
; with (a) the state to search from and (b) the path to this state. It returns
; the complete path from the initial state to the goal state: this path is a
; list of intermediate problem states. The first element of the path is the
; initial state and the last element is the goal state. Each intermediate state
; is the state that results from applying the appropriate operator to the
; preceding state. If there is no solution, DFS returns NIL.
; To call DFS to solve the original problem, one would call 
; (DFS '(NIL NIL NIL NIL) NIL) 
; However, it should be possible to call DFS with a different initial
; state or with an initial path.

; First, we define the helper functions of DFS.

; FINAL-STATE takes a single argument S, the current state, and returns T if it
; is the goal state (T T T T) and NIL otherwise.
(defun FINAL-STATE (S)
    (equal '(T T T T) S)
)

; NEXT-STATE returns the state that results from applying an operator to the
; current state. It takes three arguments: the current state (S), and which entity
; to move (A, equal to h for homer only, b for homer with baby, d for homer 
; with dog, and p for homer with poison). 
; It returns a list containing the state that results from that move.
; If applying this operator results in an invalid state (because the dog and baby,
; or poisoin and baby are left unsupervised on one side of the river), or when the
; action is impossible (homer is not on the same side as the entity) it returns NIL.
; NOTE that next-state returns a list containing the successor state (which is
; itself a list); the return should look something like ((NIL NIL T T)).
(defun NEXT-STATE (S A)
    ; Save the states for each item for easier access
    (let ((homer (car S))
          (baby (cadr S))
          (dog (caddr S))
          (poison (cadddr S)))

    ; baby, dog, and poison must be on the same side as homer!!
    ; checking if baby, poison, dog are left alone only matters when homer moves alone
    ; only then would homer not be on the same side as them
    (cond
        ; Case baby
        ((and (equal A 'b) (equal homer baby))
            (cons (append (append (cons (not homer) '()) (cons (not baby) '())) (cddr S)) NIL))
        ; Case dog
        ((and (equal A 'd) (equal homer dog))
            (cons (append (append (cons (not homer) '()) (cons baby (cons (not dog) '())) (cdddr S))) NIL))
        ; Case poison
        ((and (equal A 'p) (equal homer poison))
            (cons (append (cons (not homer) '()) (cons baby (cons dog (cons (not poison) '())))) NIL))
        ; Case homer only
        ((and (equal A 'h)
         ; If homer is on the same side as (baby and dog) or (baby and poison)
         ; then he cannot leave otherwise the two will be left alone, which is not 
         ; allowed
         (not (or (and homer baby dog)
                  (and homer baby poison))))
            (cons (cons (not homer) (cdr S)) NIL))
        (t NIL)
    )
    )
)

; SUCC-FN returns all of the possible legal successor states to the current
; state. It takes a single argument (s), which encodes the current state, and
; returns a list of each state that can be reached by applying legal operators
; to the current state.
(defun SUCC-FN (S)
    ; there are 4 possible moves that can be done
    ; Check if it is possible to do with NEXT-STATE
    ; and combine the results into one list
    (append
        (NEXT-STATE S 'b)
        (NEXT-STATE S 'd)
        (NEXT-STATE S 'p)
        (NEXT-STATE S 'h))
)

; ON-PATH checks whether the current state is on the stack of states visited by
; this depth-first search. It takes two arguments: the current state (S) and the
; stack of states visited by DFS (STATES). It returns T if s is a member of
; states and NIL otherwise.
(defun ON-PATH (S STATES)
    (cond
        ; If reaches end of states not yet visited, so NIL
        ((NULL STATES) NIL)
        ; If first element is S, in list so T
        ((equal S (car STATES)) T)
        ; Otherwise check the rest of the list
        (T (ON-PATH S (cdr STATES)))
    )
)

; MULT-DFS is a helper function for DFS. It takes two arguments: a list of
; states from the initial state to the current state (PATH), and the legal
; successor states to the last, current state in the PATH (STATES). PATH is a
; first-in first-out list of states; that is, the first element is the initial
; state for the current search and the last element is the most recent state
; explored. MULT-DFS does a depth-first search on each element of STATES in
; turn. If any of those searches reaches the final state, MULT-DFS returns the
; complete path from the initial state to the goal state. Otherwise, it returns
; NIL.
(defun MULT-DFS (STATES PATH)
    (cond
        ; No more states to try, return NIL
        ((null STATES) NIL)

        ; Try the first option
        ; Return it if it doesn't give NIL (found a solution)
        ((not (NULL (DFS (car STATES) PATH))) (DFS (car STATES) PATH))

        ; Otherwise if no solution try again on the next one 
        ((NULL (DFS (car STATES) PATH)) (MULT-DFS (cdr STATES) PATH))
    )
)

; DFS does a depth first search from a given state to the goal state. It
; takes two arguments: a state (S) and the path from the initial state to S
; (PATH). If S is the initial state in our search, PATH is set to NIL. DFS
; performs a depth-first search starting at the given state. It returns the path
; from the initial state to the goal state, if any, or NIL otherwise. DFS is
; responsible for checking if S is already the goal state, as well as for
; ensuring that the depth-first search does not revisit a node already on the
; search path.
(defun DFS (S PATH)
    (cond
        ; If S is final state append to PATH and return
        ((FINAL-STATE S) (append PATH (cons S '())))

        ; If S already tried, can't do anything so NIL
        ((ON-PATH S PATH) NIL)

        ; Else try all variants and add best option to end
        (T (MULT-DFS (SUCC-FN S) (append PATH (cons S '()))))
    )
)
