
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
      1
      (
        recursiveDotGenerator (- x 1) 
      )
    )
    (princ ".")
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




;;(let ((counter 0)) 
;;(defun countLetterperm (lis) 
;;  (if (null lis) (setf counter nil) 
;;    (if (eq (first lis) 'a) (progn (countLetter (rest lis)) (setq counter (+ counter 1))) (countLetter (rest lis))) )))

(defun recursiveCountLetter (lis) 
  (let ((counter 0)) 
  (if (null lis) 0 
    (if (eq (first lis) 'a) 
      (setq counter (+ 1 (countLetter (rest lis)))) 
      (countLetter (rest lis))) )))
