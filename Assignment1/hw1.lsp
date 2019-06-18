; Determines value of Nth number in Padovan sequence
; Takes in an int N
; If N is between 0-2 return 1
; Otherwise it is necessary to break PAD down and add the two parts together.
(defun PAD (N)
    (cond 
        ; If N is 0-2, value is 1
        ((or (= 0 N) (= 1 N) (= 2 N)) 1) 
        ; Otherwise value is sum of the 2nd and 3rd Padovan values from the end
        (T (+ (PAD (- N 2)) (PAD (- N 3)))) 
    )
)

; Determines number of additions done when calling PAD for int N
; This is done similarly to PAD but the base case returns 0
; and the alternate case adds 1 and calls SUMS again
(defun SUMS (N)
    (cond 
        ; No addition done on values 0-2
        ; Therefore 0
        ((or (= 0 N) (= 1 N) (= 2 N)) 0)
        ; Adds one and calls "PAD" again every value above 2
        (T (+ 1 (+ (SUMS (- N 2)) (SUMS (- N 3)))))
    )
)

; Takes one argument TREE (a tree given as a list)
; and rewrites it by replacing every leaf with a '?
; The function breaks down the tree into a left and right branch
; and recursively breaks it down until it reaches an atom
; Once it is an atom the atom is replaced by '? and gets recombined
(defun ANON (TREE)
    (cond
        ; If the result is an empty list return an empty list
        ((and (listp TREE) (atom TREE)) '())
        ; If the tree is just one element return '?
        ((atom TREE) '?)
        ; Otherwise split into a left and right hand side to convert into question
        ; And combine the two halves together again
        (T (cons (ANON (car TREE)) (ANON (cdr TREE))))
    )
)