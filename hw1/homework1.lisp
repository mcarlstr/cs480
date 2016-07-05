
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
  (if (eq (length lst)  counter) 
    lst  
    (progn (setf (nth counter lst) 
      (+ counter (nth counter lst)) ) 
        (pos_rec+ lst (+ counter 1))) ))

;;(b) iteration
(defun pos+_iterative (lst) 
  (let ((counter 0)) 
  (loop for x in lst do 
    (setf (nth counter lst) (+ x counter)) (setq counter (+ counter 1)))) 
    (princ lst)
)

;; (c) mapcar
(defun pos+_mapcar (lst) 
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

;; this is a last ditch effort since it uses the static variable and cheets a little
;;(let ((returnList () )) (defun lookup (pre dict) (setq returnList () ) (if (null dict) 0  
;;    (progn (lookup pre (rest dict)) (if (equal pre (extractprefix (first dict) (length pre ) ) ) 
;;      (setf returnList (append (list (first dict)) returnList)) )  ) ) ) )


(defun lookup (pre dict) 
  (let ((returnList () )) 
  (if (null dict) returnList  
    (if (equal pre (extractprefix (first dict) (length pre) ) ) 
      (progn (setf returnList (append (lookup pre (rest dict)) (list (first dict)) ) )  returnList ) 
      (progn (setf returnList (lookup pre (rest dict))) returnList) 
    )
  )
  )
)
 					      
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
    (loop while (not (null lst) ) do 
      (loop for x in lst do 
        (if (eql (first lst) x ) 
          (setq count (+ count 1) ) ))
        (setq appList (append appList (list (list (first lst) count)) ) ) 
        (setq lst (remove (first lst) lst) ) 
        (setq count 0)
    ) 
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
    (if (null seq) returnList
      (if (search '(F U N) (first seq) ) 
        (progn (setf returnList (append (find-fun (rest seq)) (list (first seq) ) ) ) returnList) 
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
  (let ( (numberVal l) (temp (mapcar #'(lambda (x y) (list x y)) '(A B C D E F) '(10 11 12 13 14 15) ) )) 
  (loop for x in temp do 
    (if (eq (first x) l ) 
      (setq numberVal (nth 1 x)) 
    ) 
  )
  numberVal 
  ) 
)

(defun conv16 (lst &optional (value 0)) 
  (if (eq 1 (length lst)) 
    (+ value (getNumber (nth 0 lst))) 
    (progn (setq value (+ value (getNumber (first lst) ) ) ) 
      (setq value (* value 16)) (conv16 (rest lst) value) ) 
  )
)
