;;(setq colors '(R G B))

;;(setq map '((A (B C E)) (B (A E F)) (C (A E F)) (D (F)) (E (A B C F)) (F (B C D E))))

;; (defun pair (element lst) (let ((result () )) (setq result (mapcar #'(lambda(x) (list element x)) lst))))

;;(setq australia '(   ))
(defun generate-permutations (colorList len)
  (let ((retList () ) (tempList () ) (temp ()) (permutations 0))
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
  retList
  )
)

(defun do-remove (permutations indexList currentIndex)
  (break)
  (loop for x in permutations do
    (loop for y in indexList do
      (if (eq (nth currentIndex x) (nth y x))
        ;; remove the permutation
        (setf permutations (remove x permutations))
      )
    )
  )
  (format t "--------------------------------------------------------------------------~%")
  permutations
  ;;(princ permutations)
)

(defun remove-illigal-permutations (permutations assocList)
  (let ((countryMap ()) (associations ()) (temp ()) )
    (setq countryMap (mapcar #'(lambda(x)(first x) ) assocList)) ;; The index is it's position in the permutation
    (loop for x in assocList do
      (setq temp (second x))
      (loop for y in temp do
        ;; associations will have the indicies to check 
        (setq associations (append associations (list (position y countryMap :test #'equal))))
      )
      (break)
      ;; check the permutations and do the removal
      (setf permutations (do-remove permutations associations (position (first x) countryMap :test #'equal )))
      (setq associations ())
    )
  )
  permutations
)

(defun find-colors (assocList colorList)
  (let ((len (length assocList)) (permutations ()) )
    (setq permutations (generate-permutations colorList len))
    (setq permutations (remove-illigal-permutations permutations assocList))
    permutations
  ) 
)
