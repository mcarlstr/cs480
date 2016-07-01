
;;1. (4p) Give iterative and recursive definitions of a function that

;;   (a) takes a positive integer and prints that many dots.
(defun iterativeDotGenerator (x) 
  (loop
    for m from 1 to x
      do (princ ".")
  ) 
)

(defun recursiveDotGenerator (x)
    (if (= x 1)
      ;; base case just return 1
      1
      (
        recursiveDotGenerator (- x 1) 
      )
    )
    (princ ".")
  t
)
;; (b) takes a list and returns the number of times the symbol a
;;       occurs in it.;;    
(defun iterativeLetterCount (list) 
  (let ((count 0)) 
    (loop for sym in list do 
      (if (eq sym 'a) 
        (setq count (+ count 1) ) 
      ) 
    ) 
    (princ count)
  ) 
)

(defun recursiveCountLetter (lis) 
  (let ((counter 0)) 
  (if (null lis) 0 
    (if (eq (first lis) 'a) 
      (setq counter (+ 1 (recursiveCountLetter (rest lis)))) 
      (recursiveCountLetter (rest lis))) )))

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

(defun summit2 (lst) 
  (let ((x (car lst))) 
  (progn (if (null lst) 
    0 
    (if (null x) 
      (summit (cdr lst)) 
      (+ x (summit (cdr lst))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;3. (3p) Suppose the function pos+ takes a list and returns a list of each
;;   element plus its position:
;;        > (pos+ '(7 5 1 4))
;;        (7 6 3 7)
;;Define this function using (a) recursion (b) iteration (c) mapcar

;; (a) recursive
(defun pos+  (lst &optional (counter 0) ) 
  (if (eq (length lst)  counter) 
    lst  
    (progn (setf (nth counter lst) 
      (+ counter (nth counter lst)) ) 
        (pos+ lst (+ counter 1))) ))

;;(b) iteration
(defun pos+_iterative (lst) 
  (let ((counter 0)) 
  (loop for x in lst do 
    (setf (nth counter lst) (+ x counter)) (setq counter (+ counter 1)))) 
    (princ lst)
)

;; (c) mapcar
(defun pos_mapcar+ (lst) 
  (let ((applist () )) 
    (loop for x from 0 to (length lst) do 
      (setq applist (append applist (list x)) ))  
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
;;(defun lookup (pre dict) () )
;;(defun lookup (pre dict) () )
;;  (defun extractPrefix (pre element) (loop for x in element for 0 to (length pre) do (princ x )) )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;6. Occurrences (3p):
;;    Define a function that takes a list and returns a list indicating
;;    the number of times each (eql) element appears, sorted from most
;;    common element to least common:
    
;;    > (occurrences '(a b a d a c d c a))
;;    ((A 4) (C 2) (D 2) (B 1))
(defun occurrences1 (lst) (let ((count 0) (appList () ))
    (loop for x in lst do (if (eq (first lst) x ) (setq count (+ count 1) ) ) ) 
(setq appList (append appList (list (first lst) count)) ) (setq lst (remove (first lst) lst) ) 
(setq count 0) (princ applist) (princ count) ) )

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

;;(defun conv16 (lst &optional (value 0)) (if (null lst) value 
;;    (progn (setq value (+ value (getNumber (first lst) ) ) ) (setq value (* value 16)) (conv16 (rest lst) value) ) ) )

;; Start by mapping the hex non-numerics to their number representation
(defun getNumber(l) (let ( (numberVal l) (temp (mapcar #'(lambda (x y) (list x y)) '(A B C D E F) '(10 11 12 13 14 15) ) )) 
  (loop for x in temp do 
    (if (eq (first x) l ) 
      (setq numberVal (nth 1 x)) 
    ) 
  )
  numberVal ) 
)

;; right one?
(defun conv16 (lst &optional (value 0)) (if (eq 1 (length lst)) (+ value (getNumber (nth 0 lst))) 
     (progn (setq value (+ value (getNumber (first lst) ) ) ) (setq value (* value 16)) (conv16 (rest lst) value) ) ) )
