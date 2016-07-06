;;0. (3p extra credit) Write a wrapper function that executes all your
;;    code with sufficent examples (at least 5 per function). 

(defun wrapperFunction ()
  ;; 1a iterative
  (princ "---- NUMBER 1A ----")
  (format t "~%")

  (iterativeDotGenerator 0)  
  (format t "~%") 
  (iterativeDotGenerator 1)  
  (format t "~%") 
  (iterativeDotGenerator 2)  
  (format t "~%") 
  (iterativeDotGenerator 3)  
  (format t "~%") 
  (iterativeDotGenerator 4)  
  (format t "~%")

  (format t "-------------------------------------------~%")

  ;; 1a recursive
  (princ "---- NUMBER 1A ----")
  (format t "~%")

  (recursiveDotGenerator 0)  
  (format t "~%") 
  (recursiveDotGenerator 1)  
  (format t "~%") 
  (recursiveDotGenerator 2)  
  (format t "~%") 
  (recursiveDotGenerator 3)  
  (format t "~%") 
  (recursiveDotGenerator 4)  
  (format t "~%")
  
  (format t "-------------------------------------------~%")

  ;; 1b iterative
  (princ "---- NUMBER 1B ----")
  (format t "~%")

  (let ((a '(a b c d a)) (b '(a a a a a) ) (c '() )  (d '(k) ) (e '(a b c a a a c b a)))
    (iterativeLetterCount a)
    (format t "~%")
    (iterativeLetterCount b)
    (format t "~%")
    (iterativeLetterCount c)
    (format t "~%")
    (iterativeLetterCount d)
    (format t "~%")
    (iterativeLetterCount e)
    (format t "~%")
  )

  (format t "-------------------------------------------~%")

    ;; 1b recursive
    (princ "---- NUMBER 1B ----")
    (format t "~%")

    (let ((a '(a b c a))) 
      (princ (recursiveCountLetter a) ) )
    (format t "~%")
    (let ((b '(a a a a a)))
      (princ (recursiveCountLetter b) ) )
    (format t "~%")
    (let ((c '()))
      (princ (recursiveCountLetter c) ) )
    (format t "~%")
    (let ((d '(k t y u i o)))
      (princ (recursiveCountLetter d) ) )
    (format t "~%")
    (let ((e '(a b c a a a c b a) ))
      (princ (recursiveCountLetter e) ) )
    (format t "~%")

  (format t "-------------------------------------------~%")

  ;; 2a
  (princ "---- NUMBER 2A ----")
  (format t "~%")

  (princ (summit1 '(1 2 3 4 5))) ;; 15     
  (format t "~%")
  (princ (summit1 '(0))) ;; 0     
  (format t "~%")
  (princ (summit1 '())) ;; 0  
  (format t "~%")
  (princ (summit1 '(1))) ;; 1    
  (format t "~%")
  (princ (summit1 '(3 3 3 3 3))) ;; 15     
  (format t "~%")

(format t "-------------------------------------------~%")

  ;; 2b
  (princ "---- NUMBER 2B ----")
  (format t "~%")

  (princ (summit2 '(1 2 3 4 5))) ;; 15     
  (format t "~%")
  (princ (summit2 '(0))) ;; 0     
  (format t "~%")
  (princ (summit2 '())) ;; 0  
  (format t "~%")
  (princ (summit2 '(1))) ;; 1    
  (format t "~%")
  (princ (summit2 '(3 3 3 3 3))) ;; 15     
  (format t "~%")

  (format t "-------------------------------------------~%")
  
  ;; number three modifies actual data
  ;; 3a
  (princ "---- NUMBER 3A ----")
  (format t "~%")

  (princ (pos_rec+ '(1 2 3 4 5))) 
  (format t "~%") 
  (princ (pos_rec+ '(5 4 3 2 1))) 
  (format t "~%")
  (princ (pos_rec+ '(1 1 1 1 1 1 1 1))) 
  (format t "~%")
  (princ (pos_rec+ '()) )
  (format t "~%")
  (princ(pos_rec+ '(1 0 -1 -2))) 
  (format t "~%")

  (format t "-------------------------------------------~%")

  ;; 3b
  (princ "---- NUMBER 3B ----")
  (format t "~%")

  (let ((a '(1 2 3 4)))
    (pos+_iterative a)) 
  (format t "~%")
  (let ((b '(5 4 3 2 1 0))) 
    (pos+_iterative b)) 
  (format t "~%")
  (let ((c '(1 1 1 1)))
    (pos+_iterative c)) 
  (format t "~%")
  (let ((d '(0 0 0 0 0 0)))
    (pos+_iterative d) )
  (format t "~%")
  (let ((e '(9 5 7 1)))
    (pos+_iterative e)) 
  (format t "~%")

  (format t "-------------------------------------------~%")

  (princ (pos+_mapcar '( 1 2 5 6)))  
  (format t "~%")
  (princ (pos+_mapcar '( 1 0 10 10)))  
  (format t "~%")
  (princ (pos+_mapcar '( 10 9 8 7 6)))  
  (format t "~%")
  (princ (pos+_mapcar '( 2 2 2 2 2 2 2)))  
  (format t "~%")
  (princ (pos+_mapcar '( 0 )))  
  (format t "~%")

  (format t "-------------------------------------------~%")

  ;; 4
  (princ "---- NUMBER 4 ----")
  (format t "~%")

  (princ (f 0)) 
  (format t "~%")
  (princ (f 2)) 
  (format t "~%")
  (princ (f 3)) 
  (format t "~%")
  (princ (f 10)) 
  (format t "~%")
  (princ (f 5)) 
  (format t "~%")
  (princ (f 10000)) 
  (format t "~%")
  (princ (f 4)) 
  (format t "~%")

  (format t "-------------------------------------------~%")

  ;;5
  (princ "---- NUMBER 5 ----")
  (format t "~%")

  (let ((prefix '(S E T)) (dictionary '((S E T Q) (S E T F) (R E V E R S E) (C A T) )) )
    (princ (lookup prefix dictionary))
    (format t "~%")
  )

  (let ((prefix '(D E)) (dictionary '((S E T Q) (D E F U N) (D E M) (S E T F) (D) (R E V E R S E) (C A T) )) )
    (princ (lookup prefix dictionary))
    (format t "~%")
  )

  (let ((prefix '(S)) (dictionary '((S E T Q) (S E T F) (S) (S E A) (R E V E R S E) (C A T) (S E T) (R E A D))) )
    (princ (lookup prefix dictionary))
    (format t "~%")
  )

  (let ((prefix '(S E T)) (dictionary '((R E A D) (S E F) (R E V E R S E) (C A T) )) )
    (princ (lookup prefix dictionary))
    (format t "~%")
  )

  (let ((prefix '(S I M P L E)) (dictionary '((S E T Q) (S E T F) (S I M P L E) (R E V E R S E) (C A T) )) )
    (princ (lookup prefix dictionary))
    (format t "~%")
  )

  (format t "-------------------------------------------~%")

  ;; 6
  (princ "---- NUMBER 6 ----")
  (format t "~%")

  (princ (occurrences '(a b c a d d)))
  (format t "~%")

  (princ (occurrences '(a b c a d d a a d e t h c)))
  (format t "~%")

  (princ (occurrences '()))
  (format t "~%")

  (princ (occurrences '(d d d d d d d d d)))
  (format t "~%")

  (princ (occurrences '(a b c a b c a a b a b c)))
  (format t "~%")

  (format t "-------------------------------------------~%")

  ;; 7
  (princ "---- NUMBER 7 ----")
  (format t "~%")

  (princ (find-fun '( (D E F U N) (M O R E F U N) (U P) (F U N))))
  (format t "~%")

  (princ (find-fun '( () () () ())))
  (format t "~%")

  (princ (find-fun '( (D E F U N) (M O R E F U N) (U P) (F U N) (D O W N) (FUN) (N O T F U N) )))
  (format t "~%")

  (princ (find-fun '( (D O W N) (D E F U N) (L E F T) (U P) (R I G H T) (Sometest F U N other text))))
  (format t "~%")

  (princ (find-fun '( (D E F U N) (F U N) (D E F U N) (F U N) (R U N))))
  (format t "~%")

  (format t "-------------------------------------------~%")

  ;; 8
  (princ "---- NUMBER 8 ----")
  (format t "~%")
  (princ "7dd = ")
  (princ (conv16 '(7 d d)))
  (format t "~%")

  (princ "0 = ")
  (princ (conv16 '(0)))
  (format t "~%")

  (princ "f = ")
  (princ (conv16 '(f)))
  (format t "~%")

  (princ "7ddf = ")
  (princ (conv16 '(7 d d f)))
  (format t "~%")

  (princ "ffffff = ")
  (princ (conv16 '(f f f f f f)))
  (format t "~%")

  (format t "-------------------------------------------~%")
)

;;1. (4p) Give iterative and recursive definitions of a function that

;;   (a) takes a positive integer and prints that many dots.
(defun iterativeDotGenerator (x) 
  (loop
    ;; loop from 1 to the positive integer
    for m from 1 to x
      do (princ ".")
  )
)

(defun recursiveDotGenerator (x)
    (if  (= x 0)
      ;; base case just return 1
      1
      (progn
        ;; reduce the subproblem 
        (recursiveDotGenerator (- x 1))
        ;; build the answer
        (princ ".") 
      )
    )
  t
)
;; (b) takes a list and returns the number of times the symbol a
;;       occurs in it.
    
(defun iterativeLetterCount (list)
  ;; set the counter variable 
  (let ((count 0))  
    (loop for sym in list do
      ;; if the symbol equals a increment the counter 
      (if (eq sym 'a) 
        (setq count (+ count 1) ) 
      ) 
    ) 
    (princ count)
  ) 
)

(defun recursiveCountLetter (lis) 
  (let ((counter 0))
  ;; base case if list is empty return 0 
  (if (null lis) 0 
    (if (eq (first lis) 'a) 
      ;; if the symbol is a increment the counter based on the return of the function
      (progn (setq counter (+ 1 (recursiveCountLetter (rest lis)))) counter)
      ;; otherwise we don't increment the counter just set and return in. 
      (progn (setq counter (recursiveCountLetter (rest lis))) counter) ) )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 2. (3p) A friend is trying to write a function that returns the sum of all
;;   the non-nil elements in a list. He has written two versions of this 
;;   function and neither of them work. Explain what is wrong with each,
;;   and give a correct version:

;;(A) (defun summit (lst)
;;      (remove nil lst)
;;        (apply #'+ lst))

;; ***** This function fails due to the remove function not modifying
;;       the actual lst variable, so when the nil is removed it is not 
;;       removed from the same list that gets the + operator applied to it
;;       The working version is:

;; correct first version
(defun summit1 (lst) 
  (setq lst (remove nil lst))
    (apply #'+ lst))

;;   (b) (defun summit (lst)
;;          (let ((x (car lst)))
;;	     (if (null x)
;;		 (summit (car lst))
;;		 (+ x (summit (cdr lst))))))

;; ***** This function causes an infinite loop because
;;       the recursive function does not have a base case
;;       i.e. the if clause does not contain a call that 
;;       doesn't call the summit function again. 
;; A corrected version can be seen below:

;; correct second version
(defun summit2 (lst) 
  (let ((x (car lst))) 
  (progn (if (null lst) 
    0 
    (if (null x) 
      (summit2 (cdr lst)) 
      (+ x (summit2 (cdr lst))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;3. (3p) Suppose the function pos+ takes a list and returns a list of each
;;   element plus its position:
;;        > (pos+ '(7 5 1 4))
;;        (7 6 3 7)
;;Define this function using (a) recursion (b) iteration (c) mapcar

;; (a) recursive
(defun pos_rec+  (lst &optional (counter 0) ) 
  ;; if the list is empty we'll just return it
  (if (eq (length lst)  counter) 
    lst  
    ;; otherwise we will increment the value at index 
    ;; by it's index
    (progn (setf (nth counter lst) 
      (+ counter (nth counter lst)) )
        ;; we will call the function on the counter
        ;; and the rest of the list 
        (pos_rec+ lst (+ counter 1))) ))

;;(b) iteration
(defun pos+_iterative (lst) 
  (let ((counter 0)) 
  (loop for x in lst do 
    ;; increment the value at the index by the counter
    (setf (nth counter lst) (+ x counter)) (setq counter (+ counter 1)))) 
    (princ lst)
)

;; (c) mapcar
(defun pos+_mapcar (lst) 
  (let ((applist () ))
    ;; build a list of the index we want to apply 
    (loop for x from 0 to (length lst) do 
      (setq applist (append applist (list x)) ))
        ;; use mapcar to add the list to the passed in list  
        (mapcar #'+ lst applist)  ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;4. (3p) Define a function f that takes one numeric argument, and returns the
;;   greatest argument passed to it so far:
;;        >(f 5)
;;        5
;;        >(f 2)
;;        5
;;        >(f 10)
;;        10

(let ((max 0)) 
(defun f (number) 
  (if (> number max)
    ;; the variable is static it will
    ;; always be the max accross runs 
    (setq max number)) 
    max
)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;5. Dictionary (3p):
;;    You are given a dictionary in the form  of "exploded" symbols (e.g. 
;;    (d e f u n), (s e t q), (s e t f)). Write a recursive function,
;;    lookup, that takes a prefix of an exploded symbol (e.g., (s e t))
;;    and a dictionary and returns the list of all items in the
;;    dictionary that match the prefix. 

;; this is a last ditch effort since it uses the static variable and cheets a little
;;(let ((returnList () )) (defun lookup (pre dict) (setq returnList () ) (if (null dict) 0  
;;    (progn (lookup pre (rest dict)) (if (equal pre (extractprefix (first dict) (length pre ) ) ) 
;;      (setf returnList (append (list (first dict)) returnList)) )  ) ) ) )


(defun lookup (pre dict) 
  (let ((returnList () ))
  ;; base case just return the list 
  (if (null dict) returnList
    ;; check to see if the prefix matches  
    (if (equal pre (extractprefix (first dict) (length pre) ) )
      ;; if the prefix matches return the list plus the appended dictionary value 
      (progn (setf returnList (append (lookup pre (rest dict)) (list (first dict)) ) )  returnList )
      ;; otherwise just return the list 
      (progn (setf returnList (lookup pre (rest dict))) returnList) 
    )
  )
  )
)

;; extract the prefix from the exploded characters 					      
(defun extractPrefix (element length) (let ((pre () )) 
  (loop for x from 0 to (- length 1) do (setq pre (append pre (list (nth x element)))) ) pre))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;6. Occurrences (3p):
;;    Define a function that takes a list and returns a list indicating
;;    the number of times each (eql) element appears, sorted from most
;;    common element to least common:
    
;;    > (occurrences '(a b a d a c d c a))
;;    ((A 4) (C 2) (D 2) (B 1))
(defun occurrences (lst) 
  (let ((count 0) (appList () ))
    ;; while the list isn't empty
    (loop while (not (null lst) ) do
      ;; for each element in the list 
      (loop for x in lst do
        ;; if the elements are equal 
        (if (eql (first lst) x )
          ;; increment the count 
          (setq count (+ count 1) ) ))
        ;; append the result to the list
        (setq appList (append appList (list (list (first lst) count)) ) )
        ;; remove the elements we just counted 
        (setq lst (remove (first lst) lst) ) 
        ;; reset the count
        (setq count 0)
    )
    ;; sort the list based on the number of counts 
    (sort appList #'(lambda (x y) (> (second x) (second y) ) ) )
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;7. Find-fun (3p):
;;    Write a lisp function find-fun that takes a dictionary (a list of exploded words) 
;;    and returns all words that contain (f u n) as a sublist.

;;    > (find-fun '((d e f u n) (h e l l o) (s e t f) (f u n d)))  
;;    ((D E F U N) (F U N D))

(defun find-fun (seq) 
  (let ((returnList () ))
    ;; base case if null just return the list 
    (if (null seq) returnList
      ;; if the first element has F U N in it
      (if (search '(F U N) (first seq) )
        ;; if the result contains F U N append append it to the list recursivly 
        (progn (setf returnList (append (find-fun (rest seq)) (list (first seq) ) ) ) returnList)
        ;; otherwise just return the list retursivly 
	(progn (setf returnList (find-fun (rest seq))))
      )
    ) 
  ) 
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;8. Conv16: (3p)
;;    Write a function conv16 that takes an exploded hexadecimal
;;    number-i.e. a list in which hexadecimal digits (0-9,a-f) are separated by
;;    spaces-and returns its corresponding value. For example,

;;    > (conv16 '(7 d d)) 
;;    2013

;;    Note that 7dd in a hexadecimal form can be converted into a decimal form
;;    by the following rule (7*16 + 13)*16 + 13, i.e. multiply the first digit 
;;    by 16, add the second digit,  multiply the result by 16, add the third 
;;    digit, and so on. Note that you do not need a calculator to do this problem.

;; Start by mapping the hex non-numerics to their number representation
(defun getNumber(l)
  ;; map the symbols to numbers 
  (let ( (numberVal l) (temp (mapcar #'(lambda (x y) (list x y)) '(A B C D E F) '(10 11 12 13 14 15) ) )) 
  (loop for x in temp do
    ;; search for the characters number representation and return it. 
    (if (eq (first x) l ) 
      (setq numberVal (nth 1 x)) 
    ) 
  )
  numberVal 
  ) 
)

(defun conv16 (lst &optional (value 0))
  ;; if the list length is equal to one
  ;; we don't need to multiply by 16 since it's
  ;; the ones place  
  (if (eq 1 (length lst)) 
    (+ value (getNumber (nth 0 lst)))
    ;; otherwisewe recurse and convert using the formula
    (progn (setq value (+ value (getNumber (first lst) ) ) ) 
      (setq value (* value 16)) (conv16 (rest lst) value) ) 
  )
)
