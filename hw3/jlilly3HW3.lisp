;;(setq colors '(R G B))

;;(setq map '((A (B C E)) (B (A E F)) (C (A E F)) (D (F)) (E (A B C F)) (F (B C D E))))

;; (defun pair (element lst) (let ((result () )) (setq result (mapcar #'(lambda(x) (list element x)) lst))))

(defun generate-permutations (colorList len)
  (let ((retList () ) (tempList () ) (temp ()) (counter 0) (permutations 0))
  (setq colorList (mapcar #'(lambda (x) (list x)) colorList))
  (setq retList colorList)
  (setq permutations (expt (length colorList) len))
  (loop while (not (eq (length retList) permutations)) do
    (loop for x in retList do
      (loop for y in colorList do
           (setq temp (list (append x y)) )
           (setq tempList (append tempList temp))
           (setq temp ())
      )
    )
    (setq retList tempList)
    (setq tempList () )
  )
  (format t "Generated ~D permutations~%" counter)
  retList
  )
)
