;
; CS161 Hw3: Sokoban
; 
; *********************
;    READ THIS FIRST
; ********************* 
;
; All functions that you need to modify are marked with 'EXERCISE' in their header comments.
; Do not modify a-star.lsp.
; This file also contains many helper functions. You may call any of them in your functions.
;
; *Warning*: The provided A* code only supports the maximum cost of 4999 for any node.
; That is f(n)=g(n)+h(n) < 5000. So, be careful when you write your heuristic functions.
; Do not make them return anything too large.
;
; For Allegro Common Lisp users: The free version of Allegro puts a limit on memory.
; So, it may crash on some hard sokoban problems and there is no easy fix (unless you buy 
; Allegro). 
; Of course, other versions of Lisp may also crash if the problem is too hard, but the amount
; of memory available will be relatively more relaxed.
; Improving the quality of the heuristic will mitigate this problem, as it will allow A* to
; solve hard problems with fewer node expansions.
; 
; In either case, this limitation should not significantly affect your grade.
; 
; Remember that most functions are not graded on efficiency (only correctness).
; Efficiency can only influence your heuristic performance in the competition (which will
; affect your score).
;  
;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; General utility functions
; They are not necessary for this homework.
; Use/modify them for your own convenience.
;

;
; For reloading modified code.
; I found this easier than typing (load "filename") every time. 
;
(defun reload()
  (load "hw3.lsp")
  )

;
; For loading a-star.lsp.
;
(defun load-a-star()
  (load "a-star.lsp"))

;
; Reloads hw3.lsp and a-star.lsp
;
(defun reload-all()
  (reload)
  (load-a-star)
  )

;
; A shortcut function.
; goal-test and next-states stay the same throughout the assignment.
; So, you can just call (sokoban <init-state> #'<heuristic-name>).
; 
;
(defun sokoban (s h)
  (a* s #'goal-test #'next-states h)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; end general utility functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; We now begin actual Sokoban code
;

; Define some global variables
(setq blank 0)
(setq wall 1)
(setq box 2)
(setq keeper 3)
(setq star 4)       ; Goal
(setq boxstar 5)    ; Box on goal
(setq keeperstar 6) ; Keeper on goal

; Some helper functions for checking the content of a square
(defun isBlank (v)
  (= v blank)
  )

(defun isWall (v)
  (= v wall)
  )

(defun isBox (v)
  (= v box)
  )

(defun isKeeper (v)
  (= v keeper)
  )

(defun isStar (v)
  (= v star)
  )

(defun isBoxStar (v)
  (= v boxstar)
  )

(defun isKeeperStar (v)
  (= v keeperstar)
  )

;
; Helper function of getKeeperPosition
;
(defun getKeeperColumn (r col)
  (cond ((null r) nil)
    (t (if (or (isKeeper (car r)) (isKeeperStar (car r)))
           col
         (getKeeperColumn (cdr r) (+ col 1))
         );end if
       );end t
    );end cond
  )

;
; getKeeperPosition (s firstRow)
; Returns a list indicating the position of the keeper (c r).
; 
; Assumes that the keeper is in row >= firstRow.
; The top row is the zeroth row.
; The first (right) column is the zeroth column.
;
(defun getKeeperPosition (s row)
  (cond ((null s) nil)
    (t (let ((x (getKeeperColumn (car s) 0)))
         (if x
         ;keeper is in this row
         (list x row)
         ;otherwise move on
         (getKeeperPosition (cdr s) (+ row 1))
         );end if
           );end let
     );end t
    );end cond
  );end defun

;
; cleanUpList (l)
; returns l with any NIL element removed.
; For example, if l is '(1 2 NIL 3 NIL), returns '(1 2 3).
;
(defun cleanUpList (L)
  (cond ((null L) nil)
    (t (let ((cur (car L))
         (res (cleanUpList (cdr L)))
         )
         (if cur 
         (cons cur res)
          res
         )
         );end let
       );end t
    );end cond
  );end 

; EXERCISE: Modify this function to return true (t)
; if and only if s is a goal state of a Sokoban game.
; (no box is on a non-goal square)
;
; Currently, it always returns NIL. If A* is called with
; this function as the goal testing function, A* will never
; terminate until the whole search space is exhausted.
;

; Helper function for goal-test
; The checks to see if there are any boxes that are not
; on a goal state. In other words, it checks to ensure that
; there is no value 2
(defun no_boxes (row)
    (cond
        ; If the row is empty, no boxes so true
        ((NULL row) t)
        ; If there is a row with a box then NIL
        ((isBox (car row)) NIL)
        ; Otherwise check the rest of the row
        (t (no_boxes (cdr row)))
    )
)

; goal-test will check each row and return true if each row
; has no boxes not on goals (no values of 2)
(defun goal-test (s)
    (cond
        ; If there are no more rows to check it's true
        ((NULL s) t)
        ; Otherwise check to make sure the row is ok
        ; If the row has boxes, return NIL
        ((not (no_boxes (car s))) NIL)
        ; Otherwise check the next row
        (t (goal-test (cdr s)))
    )
);end defun

; EXERCISE: Modify this function to return the list of 
; sucessor states of s.
;
; This is the top-level next-states (successor) function.
; Some skeleton code is provided below.
; You may delete them totally, depending on your approach.
; 
; If you want to use it, you will need to set 'result' to be 
; the set of states after moving the keeper in each of the 4 directions.
; A pseudo-code for this is:
; 
; ...
; (result (list (try-move s UP) (try-move s DOWN) (try-move s LEFT) (try-move s RIGHT)))
; ...
; 
; You will need to define the function try-move and decide how to represent UP,DOWN,LEFT,RIGHT.
; Any NIL result returned from try-move can be removed by cleanUpList.
; 
;

; get_column gets given column for s
; Helper for get_column that gets value for a row
(defun col_row (c l)
    ; Take the head of the nthcdr
    (car (nthcdr c l))
)

(defun get_column (c s)
    (cond
        ; Return an empty list if empty
        ((null s) nil)
        ; Otherwise append the nth value of a row to the rest of the rows
        (t (cons (col_row c (car s))
                (get_column c (cdr s))))
    )
)

; get_row gets the row for s
(defun get_row (r s)
    (car (nthcdr r s))
)

; reverse_list reverses a list order
(defun reverse_list (l)
    (cond
        ; If the list is empty return the empty list
        ((NULL l) NIL)
        ; Otherwise take the head and append to end
        (t (append (reverse_list (cdr l))
                (list (car l))))
    )
)


; create_move_list takes a grid s and given left, right, up, or down
; creates a list oriented in that direction given keeper position
; dir = direction to move, x = col position, y = row position
(defun create_move_list (dir x y s)
    (cond
        ; Looking left. Reverse row
        ((equal dir 'l) (reverse_list (get_row y s)))
        ; get_row is left to right
        ((equal dir 'r) (get_row y s))
        ; Looking up. Reverse column
        ((equal dir 'u) (reverse_list (get_column x s)))
        ; get_col is top to bottom
        ((equal dir 'd) (get_column x s))
    )
)

; swap_col swaps the column for gird s at c with the given list l
; helper function swaps out one element from a list at index c
(defun swap_val (val c l)
    (cond
        ; If the first value is index, remove and swap
        ((= c 0) (cons val (cdr l)))
        ; Otherwise try next one
        (t (cons (car l) (swap_val val (- c 1) (cdr l))))
    )
)

(defun swap_col (l c s)
    (cond
        ((null s) nil)
        (t (cons (swap_val (car l) c (car s)) (swap_col (cdr l) c (cdr s))))
    )
)

; swap_row swaps the row for grid s at r with list l
(defun swap_row (l r s)
    (cond
        ((= r 0) (cons l (cdr s)))
        (t (cons (car s) (swap_row l (- r 1) (cdr s))))
    )
)

; Tries to move the keeper one over if possible, replacing the previous
; spot with blank if it's keeper and star if it's keeperstar
; Returns NIL if can't move

; Helper. Assumes that keeper is tail end of list
; i.e. the head of the list is what is immediately after
; Returns NIL if can't move
(defun move_one (l)
    (cond 
        ; If end of list(NIL) return NIL
        ((null l) NIL)
        ; If not, check if wall
        ((isWall (car l)) NIL)
        ; If box or boxstar, check if can move forward
        ((and (isBox (car l)) (move_box (cdr l)))
            (cons keeper (move_box (cdr l))))

        ((and (isBoxStar (car l)) (move_box (cdr l)))
            (cons keeperstar (move_box (cdr l))))
        ; If next is box or boxstar but can't move, NIL
        ((and 
            (or (isBoxStar (car l)) (isBox (car l))) 
            (not (move_box (cdr l))))
            NIL)

        ; Check if goal, then combine with keeperstar
        ((isStar (car l)) (cons keeperstar (cdr l)))
        ; Otherwise can move forward, so replace with keeper
        (t (cons keeper (cdr l)))
    )
)

; Helper. Given a box can it move forward?
; Similar system to player movement
(defun move_box (l)
    (cond 
        ; If end of list(NIL) return NIL
        ((null l) NIL)
        ; If not, check if wall
        ((isWall (car l)) NIL)
        ; If box or boxstar, can't move
        ((or (isBoxStar (car l)) (isBox (car l))) NIL)
        ; Check if goal, then combine with keeperstar
        ((isStar (car l)) (cons boxstar (cdr l)))
        ; Otherwise can move forward, so replace with keeper
        (t (cons box (cdr l)))
    )
)

(defun try_move_forward (l)
    (cond
        ((null l) nil)
        ; If the first value is the Keeper and can move forward, do it
        ; Replacing old position with blank
        ((and (isKeeper (car l)) (move_one (cdr l)))
            (cons blank (move_one (cdr l))))
        ; If the first is KeeperStar and can move forward, do it
        ; Replacing old position with star
        ((and (isKeeperStar (car l)) (move_one (cdr l)))
            (cons star (move_one (cdr l))))
        ; Otherwise check the rest of the list
        ((try_move_forward (cdr l)) (cons (car l) (try_move_forward (cdr l))))
        ; If can't, NIL
        (t NIL)
    )
)

; Helper for next states.
; Tries to move in the given direction if possible. If it is,
; returns the resulting move.
; If not return NIL
; x and y are the position of the keeper
(defun try_move (dir x y s)
    (let
    ; Create a list based on the direction and keeper position
    ; Try to move forward on list if possible
    ((line (try_move_forward (create_move_list dir x y s))))
        (cond
            ; If can't move forward, stop there
            ((null line) NIL)
            ; Otherwise swap the line back into the correct position
            (t (cond
                ((equal dir 'l)
                    (swap_row (reverse_list line) y s))
                ((equal dir 'r)
                    (swap_row line y s))
                ((equal dir 'u)
                    (swap_col (reverse_list line) x s))
                ((equal dir 'd)
                    (swap_col line x s))
            ))
        )
    )
)

(defun next-states (s)
    (let* 
    ((pos (getKeeperPosition s 0))
    (x (car pos))
    (y (cadr pos))
       ;x and y are now the coordinate of the keeper in s.
       (result (list 
            (try_move 'u x y s)
            (try_move 'd x y s)
            (try_move 'l x y s)
            (try_move 'r x y s)))
    )
    (cleanUpList result);end
   );end let
);

; EXERCISE: Modify this function to compute the trivial 
; admissible heuristic.
; All values return 0
(defun h0 (s)
    0
  )

; EXERCISE: Modify this function to compute the 
; number of misplaced boxes in s.
;
; ANSWER: YES
; This heuristic is admissible because the amount of boxes not yet
; on goal states does not actually help us put boxes on goal states

(defun h1 (s)
    (cond
        ; No rows = 0
        ((null s) 0)
        ; If there's a row add number of boxes to total
        (t (+ (count_boxes (car s)) (h1 (cdr s))))
    )
)
; Helper count_boxes counts the number of boxes in a given row
(defun count_boxes (row)
    (cond
        ; No boxes = 0
        ((null row) 0)
        ; If there's a box add 1 to total
        ((isBox (car row)) (+ 1 (count_boxes (cdr row))))
        ; Otherwise don't add anything
        (t (count_boxes (cdr row)))
    )
)

; EXERCISE: Change the name of this function to h<UID> where
; <UID> is your actual student ID number. Then, modify this 
; function to compute an admissible heuristic value of s. 
; 
; This function will be entered in the competition.
; Objective: make A* solve problems as fast as possible.
; The Lisp 'time' function can be used to measure the 
; running time of a function call.
;

; Calculates manhattan distance between 2 coordinates
(defun distance (coord1 coord2)
    (+ 
        (abs (- (car coord1) (car coord2))) 
        (abs (- (cadr coord1) (cadr coord2)))
    )
)

; Helper to find_boxes. Gives full coordinates of boxes
(defun box_col (l col row)
    (cond
        ((null l) NIL)
        ; If the first value is a box create a pair
        ; using current col index and row index.
        ; Then try to find more in list if possible
        ((isBox (car l)) 
            (cons (list col row) 
                (box_col (cdr l) (+ col 1) row)))
        ; If it isn't try with the rest of the values
        (t (box_col (cdr l) (+ col 1) row))
    )
)
; Helper finds coordinates boxes and makes a list of them
(defun find_boxes (s row)
    (cond
        ((null s) NIL)
        (t (append (box_col (car s) 0 row) (find_boxes (cdr s) (+ row 1))))
    )
)

; Creates a list of boxes and the distance from the player each one is
;(defun box_distance (boxes keeper)
;    (cond
;        ((null boxes) NIL)
        ; create a new pair, the left being the box coordinates and
        ; the right being the distance from the player
;        (t (cons 
;            (list (car boxes) (distance (car boxes) keeper))
            ; Try and Calculate the other distances next
;            (box_distance (cdr boxes) keeper)))
;    )
;)

;(defun box_pairs (s)
;    (box_distance (find_boxes s 0) (getKeeperPosition s 0))
;)

;(defun closest_box (pairs shortest)
;    (cond
;        ((null pairs) shortest)
;        ((< (cadr (car pairs)) shortest) (closest_box (cdr pairs) (cadr (car pairs))))
;        (t (closest_box (cdr pairs) shortest))
;    )
;)

; Takes a list of coordinates for boxes and the coordinates of a keeper
; and finds the distances between them. The function returns a list of all
; the distances
(defun box_distances (boxes keeper)
    (cond
        ; There are no boxes so return the empty list
        ((null boxes) NIL)
        (t (cons (distance (car boxes) keeper)
                (box_distances (cdr boxes) keeper)))
    )
)

; My heuristic takes the manhattan distance of all the boxes to the keeper
; The heuristic will make it so that boxes closest to the pklayer will be moved first.
(defun h004951425 (s)
    (let
    ; distances is the list of distances of every box (not on a goal) to the player
    ((distances (box_distances (find_boxes s 0) (getKeeperPosition s 0))))
        (cond
            ; All boxes are on goal states, so the list will be empty
            ((null distances) 0)
            ; Take the minimum of the given list to find the distance of the closest box
            (t (eval (cons 'min distances)))
        )
    )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Some predefined problems.
 | Each problem can be visualized by calling (printstate <problem>). For example, (printstate p1).
 | Problems are roughly ordered by their difficulties.
 | For most problems, we also privide 2 additional number per problem:
 |    1) # of nodes expanded by A* using our next-states and h0 heuristic.
 |    2) the depth of the optimal solution.
 | These numbers are located at the comments of the problems. For example, the first problem below 
 | was solved by 80 nodes expansion of A* and its optimal solution depth is 7.
 | 
 | Your implementation may not result in the same number of nodes expanded, but it should probably
 | give something in the same ballpark. As for the solution depth, any admissible heuristic must 
 | make A* return an optimal solution. So, the depths of the optimal solutions provided could be used
 | for checking whether your heuristic is admissible.
 |
 | Warning: some problems toward the end are quite hard and could be impossible to solve without a good heuristic!
 | 
 |#

;(80,7)
(setq p1 '((1 1 1 1 1 1)
       (1 0 3 0 0 1)
       (1 0 2 0 0 1)
       (1 1 0 1 1 1)
       (1 0 0 0 0 1)
       (1 0 0 0 4 1)
       (1 1 1 1 1 1)))

;(110,10)
(setq p2 '((1 1 1 1 1 1 1)
       (1 0 0 0 0 0 1) 
       (1 0 0 0 0 0 1) 
       (1 0 0 2 1 4 1) 
       (1 3 0 0 1 0 1)
       (1 1 1 1 1 1 1)))

;(211,12)
(setq p3 '((1 1 1 1 1 1 1 1 1)
       (1 0 0 0 1 0 0 0 1)
       (1 0 0 0 2 0 3 4 1)
       (1 0 0 0 1 0 0 0 1)
       (1 0 0 0 1 0 0 0 1)
       (1 1 1 1 1 1 1 1 1)))

;(300,13)
(setq p4 '((1 1 1 1 1 1 1)
       (0 0 0 0 0 1 4)
       (0 0 0 0 0 0 0)
       (0 0 1 1 1 0 0)
       (0 0 1 0 0 0 0)
       (0 2 1 0 0 0 0)
       (0 3 1 0 0 0 0)))

;(551,10)
(setq p5 '((1 1 1 1 1 1)
       (1 1 0 0 1 1)
       (1 0 0 0 0 1)
       (1 4 2 2 4 1)
       (1 0 0 0 0 1)
       (1 1 3 1 1 1)
       (1 1 1 1 1 1)))

;(722,12)
(setq p6 '((1 1 1 1 1 1 1 1)
       (1 0 0 0 0 0 4 1)
       (1 0 0 0 2 2 3 1)
       (1 0 0 1 0 0 4 1)
       (1 1 1 1 1 1 1 1)))

;(1738,50)
(setq p7 '((1 1 1 1 1 1 1 1 1 1)
       (0 0 1 1 1 1 0 0 0 3)
       (0 0 0 0 0 1 0 0 0 0)
       (0 0 0 0 0 1 0 0 1 0)
       (0 0 1 0 0 1 0 0 1 0)
       (0 2 1 0 0 0 0 0 1 0)
       (0 0 1 0 0 0 0 0 1 4)))

;(1763,22)
(setq p8 '((1 1 1 1 1 1)
       (1 4 0 0 4 1)
       (1 0 2 2 0 1)
       (1 2 0 1 0 1)
       (1 3 0 0 4 1)
       (1 1 1 1 1 1)))

;(1806,41)
(setq p9 '((1 1 1 1 1 1 1 1 1) 
       (1 1 1 0 0 1 1 1 1) 
       (1 0 0 0 0 0 2 0 1) 
       (1 0 1 0 0 1 2 0 1) 
       (1 0 4 0 4 1 3 0 1) 
       (1 1 1 1 1 1 1 1 1)))

;(10082,51)
(setq p10 '((1 1 1 1 1 0 0)
        (1 0 0 0 1 1 0)
        (1 3 2 0 0 1 1)
        (1 1 0 2 0 0 1)
        (0 1 1 0 2 0 1)
        (0 0 1 1 0 0 1)
        (0 0 0 1 1 4 1)
        (0 0 0 0 1 4 1)
        (0 0 0 0 1 4 1)
        (0 0 0 0 1 1 1)))

;(16517,48)
(setq p11 '((1 1 1 1 1 1 1)
        (1 4 0 0 0 4 1)
        (1 0 2 2 1 0 1)
        (1 0 2 0 1 3 1)
        (1 1 2 0 1 0 1)
        (1 4 0 0 4 0 1)
        (1 1 1 1 1 1 1)))

;(22035,38)
(setq p12 '((0 0 0 0 1 1 1 1 1 0 0 0)
        (1 1 1 1 1 0 0 0 1 1 1 1)
        (1 0 0 0 2 0 0 0 0 0 0 1)
        (1 3 0 0 0 0 0 0 0 0 0 1)
        (1 0 0 0 2 1 1 1 0 0 0 1)
        (1 0 0 0 0 1 0 1 4 0 4 1)
        (1 1 1 1 1 1 0 1 1 1 1 1)))

;(26905,28)
(setq p13 '((1 1 1 1 1 1 1 1 1 1)
        (1 4 0 0 0 0 0 2 0 1)
        (1 0 2 0 0 0 0 0 4 1)
        (1 0 3 0 0 0 0 0 2 1)
        (1 0 0 0 0 0 0 0 0 1)
        (1 0 0 0 0 0 0 0 4 1)
        (1 1 1 1 1 1 1 1 1 1)))

;(41715,53)
(setq p14 '((0 0 1 0 0 0 0)
        (0 2 1 4 0 0 0)
        (0 2 0 4 0 0 0)    
        (3 2 1 1 1 0 0)
        (0 0 1 4 0 0 0)))

;(48695,44)
(setq p15 '((1 1 1 1 1 1 1)
        (1 0 0 0 0 0 1)
        (1 0 0 2 2 0 1)
        (1 0 2 0 2 3 1)
        (1 4 4 1 1 1 1)
        (1 4 4 1 0 0 0)
        (1 1 1 1 0 0 0)
        ))

;(91344,111)
(setq p16 '((1 1 1 1 1 0 0 0)
        (1 0 0 0 1 0 0 0)
        (1 2 1 0 1 1 1 1)
        (1 4 0 0 0 0 0 1)
        (1 0 0 5 0 5 0 1)
        (1 0 5 0 1 0 1 1)
        (1 1 1 0 3 0 1 0)
        (0 0 1 1 1 1 1 0)))

;(3301278,76)
(setq p17 '((1 1 1 1 1 1 1 1 1 1)
        (1 3 0 0 1 0 0 0 4 1)
        (1 0 2 0 2 0 0 4 4 1)
        (1 0 2 2 2 1 1 4 4 1)
        (1 0 0 0 0 1 1 4 4 1)
        (1 1 1 1 1 1 0 0 0 0)))

;(??,25)
(setq p18 '((0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
        (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
        (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
        (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
        (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
        (0 0 0 0 0 0 0 0 3 0 0 0 0 0 0 0)
        (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
        (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
        (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
        (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
        (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
        (0 0 0 0 1 0 0 0 0 0 4 1 0 0 0 0)
        (0 0 0 0 1 0 2 0 0 0 0 1 0 0 0 0)       
        (0 0 0 0 1 0 2 0 0 0 4 1 0 0 0 0)
        ))
;(??,21)
(setq p19 '((0 0 0 1 0 0 0 0 1 0 0 0)
        (0 0 0 1 0 0 0 0 1 0 0 0)
        (0 0 0 1 0 0 0 0 1 0 0 0)
        (1 1 1 1 0 0 0 0 1 1 1 1)
        (0 0 0 0 1 0 0 1 0 0 0 0)
        (0 0 0 0 0 0 3 0 0 0 2 0)
        (0 0 0 0 1 0 0 1 0 0 0 4)
        (1 1 1 1 0 0 0 0 1 1 1 1)
        (0 0 0 1 0 0 0 0 1 0 0 0)
        (0 0 0 1 0 0 0 0 1 0 0 0)
        (0 0 0 1 0 2 0 4 1 0 0 0)))

;(??,??)
(setq p20 '((0 0 0 1 1 1 1 0 0)
        (1 1 1 1 0 0 1 1 0)
        (1 0 0 0 2 0 0 1 0)
        (1 0 0 5 5 5 0 1 0)
        (1 0 0 4 0 4 0 1 1)
        (1 1 0 5 0 5 0 0 1)
        (0 1 1 5 5 5 0 0 1)
        (0 0 1 0 2 0 1 1 1)
        (0 0 1 0 3 0 1 0 0)
        (0 0 1 1 1 1 1 0 0)))

;(??,??)
(setq p21 '((0 0 1 1 1 1 1 1 1 0)
        (1 1 1 0 0 1 1 1 1 0)
        (1 0 0 2 0 0 0 1 1 0)
        (1 3 2 0 2 0 0 0 1 0)
        (1 1 0 2 0 2 0 0 1 0)
        (0 1 1 0 2 0 2 0 1 0)
        (0 0 1 1 0 2 0 0 1 0)
        (0 0 0 1 1 1 1 0 1 0)
        (0 0 0 0 1 4 1 0 0 1)
        (0 0 0 0 1 4 4 4 0 1)
        (0 0 0 0 1 0 1 4 0 1)
        (0 0 0 0 1 4 4 4 0 1)
        (0 0 0 0 1 1 1 1 1 1)))

;(??,??)
(setq p22 '((0 0 0 0 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0)
        (0 0 0 0 1 0 0 0 1 0 0 0 0 0 0 0 0 0 0)
        (0 0 0 0 1 2 0 0 1 0 0 0 0 0 0 0 0 0 0)
        (0 0 1 1 1 0 0 2 1 1 0 0 0 0 0 0 0 0 0)
        (0 0 1 0 0 2 0 2 0 1 0 0 0 0 0 0 0 0 0)
        (1 1 1 0 1 0 1 1 0 1 0 0 0 1 1 1 1 1 1)
        (1 0 0 0 1 0 1 1 0 1 1 1 1 1 0 0 4 4 1)
        (1 0 2 0 0 2 0 0 0 0 0 0 0 0 0 0 4 4 1)
        (1 1 1 1 1 0 1 1 1 0 1 3 1 1 0 0 4 4 1)
        (0 0 0 0 1 0 0 0 0 0 1 1 1 1 1 1 1 1 1)
        (0 0 0 0 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Utility functions for printing states and moves.
 | You do not need to understand any of the functions below this point.
 |#

;
; Helper function of prettyMoves
; from s1 --> s2
;
(defun detectDiff (s1 s2)
  (let* ((k1 (getKeeperPosition s1 0))
     (k2 (getKeeperPosition s2 0))
     (deltaX (- (car k2) (car k1)))
     (deltaY (- (cadr k2) (cadr k1)))
     )
    (cond ((= deltaX 0) (if (> deltaY 0) 'DOWN 'UP))
      (t (if (> deltaX 0) 'RIGHT 'LEFT))
      );end cond
    );end let
  );end defun

;
; Translates a list of states into a list of moves.
; Usage: (prettyMoves (a* <problem> #'goal-test #'next-states #'heuristic))
;
(defun prettyMoves (m)
  (cond ((null m) nil)
    ((= 1 (length m)) (list 'END))
    (t (cons (detectDiff (car m) (cadr m)) (prettyMoves (cdr m))))
    );end cond
  );

;
; Print the content of the square to stdout.
;
(defun printSquare (s)
  (cond ((= s blank) (format t " "))
    ((= s wall) (format t "#"))
    ((= s box) (format t "$"))
    ((= s keeper) (format t "@"))
    ((= s star) (format t "."))
    ((= s boxstar) (format t "*"))
    ((= s keeperstar) (format t "+"))
    (t (format t "|"))
    );end cond
  )

;
; Print a row
;
(defun printRow (r)
  (dolist (cur r)
    (printSquare cur)    
    )
  );

;
; Print a state
;
(defun printState (s)
  (progn    
    (dolist (cur s)
      (printRow cur)
      (format t "~%")
      )
    );end progn
  )

;
; Print a list of states with delay.
;
(defun printStates (sl delay)
  (dolist (cur sl)
    (printState cur)
    (sleep delay)
    );end dolist
  );end defun
