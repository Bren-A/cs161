;
; Graph coloring to SAT conversion
;
; All functions you need to write are marked with 'EXERCISE' in their header comments.
; Same rules apply regarding Lisp functions you are allowed to use.
; In fact, you do not need a lot of Lisp functions to finish this assignment.
;

;;;;;;;;;;;;;;;;;;;;;;
; General util.
;
(defun reload()
  (load "hw4.lsp")
  );end defun

; EXERCISE: Fill this function.
; returns the index of the variable
; that corresponds to the fact that 
; "node n gets color c" (when there are k possible colors).
;
; variable index = (n-1)*k + c
(defun node2var (n c k)
  (+ (* (- n 1) k) c)
  )

; EXERCISE: Fill this function
; returns *a clause* for the constraint:
; "node n gets at least one color from the set {c,c+1,...,k}."
;
; i.e. "node n has var v based on colors [c..k]"
; In order to do this, must take a list of colors and 
; the given node then use node2var to convert to the correct
; propositional variable.
;
; Uses colors_to_vars helper in order to accomplish this
; Changed to tail recursion to help optimize
(defun colors_to_vars (n c current k vars)
    (cond
        ; Once the current index goes below c,
        ; there are no variables to add so return current vars list
        ((< current c) vars)
        ; Otherwise there are still variables.
        ; Add the variable to the current list
        (t (colors_to_vars n c (- current 1) k (cons (node2var n current k) vars)))
    )
)

(defun at-least-one-color (n c k)
    (colors_to_vars n c k k NIL)
)

; EXERCISE: Fill this function
; returns *a list of clauses* for the constraint:
; "node n gets at most one color from the set {c,c+1,...,k}."
;
; In other words, exclusive or,
; where "node n gets color c and not the inverse of c"
;
; Must create helper function inverse(list) to invert a list of
; values and make them negative
; Implemented tail recursively for optimization
(defun inverse (l current)
    (cond
        ; If there are no longer vaues to negate return the current
        ((null l) current)
        ; Otherwise inverse the rest of the list and append to
        ; the current current
        (t (inverse (cdr l) (append current (list (- (car l))))))
    )
)
; Next, we must create a list which contains the negation of all the
; variables except v 
; create_pair takes the list and recombines it into the
; desired inverse of the clause

(defun create_pair (n c curr k current)
  (cond 
    ((= c curr) current)
    ; Otherwise invert the current color/node combination and add it to the current list
    (t (create_pair n c (- curr 1) k  (append (list (inverse (list (node2var n c k) (node2var n curr k)) NIL)) current)))
  )
)

(defun create_list (n c curr k current)
  (cond
    ; Once the current index is below c, return the current list
    ((< curr c) current)
    ; Otherwise create another pair for the next color
    (t (create_list n c (- curr 1) k (append (create_pair n curr k k NIL) current)))
  )
)

; EXERCISE: Fill this function
; returns *a list of clauses* for the constraint:
; "node n gets at most one color from the set {c,c+1,...,k}."
;
(defun at-most-one-color (n c k)
  (create_list n c k k NIL)
)

; EXERCISE: Fill this function
; returns *a list of clauses* to ensure that
; "node n gets exactly one color from the set {1,2,...,k}."
;
; node n gets at least one color from [1..k] 
; and
; node n gets at most one color from [1..k]
; ( (1 v ... v k) ^ ( (1 ^ (~2 v ... v ~k)) ^ (k ^ (~1 v ... v ~k-1)) ) )
(defun generate-node-clauses (n k)
    ; Combine the two clauses into one list
    (append (list (at-least-one-color n 1 k)) (at-most-one-color n 1 k))
)

; EXERCISE: Fill this function
; returns *a list of clauses* to ensure that
; "the nodes at both ends of edge e cannot have the same color from the set {1,2,...,k}."
;
; For every color combination for the given two nodes, the created
; variable for said two nodes cannot be the variables created
; when the same color is used.
;
; Each node cannot have the same color at the same time.
; combine_lists takes the list of possible colors for two nodes
; connnected by an edge and adds a clause that prevents the colors
; of each node from being equal
(defun combine_lists (l1 l2)
    (cond
        ((null l1) '())
        (t (cons (list (- (car l1)) (- (car l2))) (combine_lists (cdr l1) (cdr l2))))
    )
)
; Creates the edge clause
(defun generate-edge-clauses (e k)
    (combine_lists (at-least-one-color (car e) 1 k) (at-least-one-color (cadr e) 1 k))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Your exercises end here. Below are top-level
; and utility functions that you do not need to understand.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; 
; Top-level function for converting the graph coloring problem
; of the graph defined in 'fname' using k colors into a SAT problem.
; The resulting SAT problem is written to 'out-name' in a simplified DIMACS format.
; (http://www.satcompetition.org/2004/format-solvers2004.html)
;
; This function also returns the cnf written to file.
; 
; *works only for k>0*
;
(defun graph-coloring-to-sat (fname out-name k)
  (progn
    (setf in-path (make-pathname :name fname))
    (setf in (open in-path :direction :input))
    (setq info (get-number-pair-from-string (read-line in) #\ ))
    (setq cnf nil)
    (do ((node 1
	       (+ node 1)
	       ))
	((> node (car info)))
      (setq cnf (append (generate-node-clauses node k) cnf))
      );end do
    (do ((line (read-line in nil 'eof)
	       (read-line in nil 'eof)))
	((eql line 'eof) (close in))
      (let ((edge (get-number-pair-from-string line #\ )))
	(setq cnf (append (generate-edge-clauses edge k) cnf))
	);end let
      );end do
    (close in)
    (write-cnf-to-file out-name (* (car info) k) cnf)
    (return-from graph-coloring-to-sat cnf)
    );end progn  
  );end defun

;
; A utility function for parsing a pair of integers.
; 
(defun get-number-pair-from-string (string token)
  (if (and string token)
      (do* ((delim-list (if (and token (listp token)) token (list token)))
            (char-list (coerce string 'list))
            (limit (list-length char-list))
            (char-count 0 (+ 1 char-count))
            (char (car char-list) (nth char-count char-list))
            )
           ((or (member char delim-list)
                (= char-count limit))
            (return
               (if (= char-count limit)
                   (list string nil)
                   (list (parse-integer (coerce (butlast char-list (- limit char-count))
                                 'string))
                         (parse-integer (coerce (nthcdr (+ char-count 1) char-list) 'string))
			 )))))))

;
; Writes clause to file handle 'out'.
;
(defun write-clause-to-file (out clause)
  (cond ((null clause) (format out "0~%"))
	(t (progn 
	     (format out "~A " (car clause))
	     (write-clause-to-file out (cdr clause))
	     );end progn
	   );end t
	);end cond
  );end defun

;
; Writes the formula cnf with vc variables to 'fname'.
;
(defun write-cnf-to-file (fname vc cnf)
  (progn
    (setf path (make-pathname :name fname))
    (setf out (open path :direction :output))
    (setq cc (length cnf))  
    (format out "p cnf ~A ~A~%" vc cc)
    (dolist (clause cnf)
      (write-clause-to-file out clause)
      );end dolist
    (close out)
    );end progn
  );end defun
