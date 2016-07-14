;; Joshua Lilly
;; CS 480 
;; 7/14/16

 (let ((map '((A (B C E)) (B (A E F)) (C (A E F)) (D (F)) (E (A B C F)) (F (B C D E))) )

 (australia '( (WA (NT SA)) (NT (WA SA Q)) (SA (WA NT Q SW V)) (Q (NT SA SW)) (SW (SA Q V)) (V (SA SW)) (TS  () ) )))


;; generates all possible permutations of length len over an 
;; alphabet colorList
(defun generate-permutations (colorList len)
  (let ((retList () ) (tempList () ) (temp ()) (permutations 0))
  (setq colorList (mapcar #'(lambda (x) (list x)) colorList))
  (setq retList colorList)
  (setq permutations (expt (length colorList) len))
  ;; we will always have a total of number of color choices ^ # of cities
  (loop while (not (eq (length retList) permutations)) do
    (loop for x in retList do
      (loop for y in colorList do
           (setq temp (list (append x y)) )  ;; this just appends all symbols in the alphabet
           (setq tempList (append tempList temp)) ;; to the end of everything in the list
           (setq temp ())
      )
    )
    (setq retList tempList)
    (setq tempList () )
  )
  retList
  )
)

;; do the actual checking and removal of illigal states
(defun do-remove (permutations indexList currentIndex)
  (loop for x in permutations do
    (loop for y in indexList do
      (if (eq (nth currentIndex x) (nth y x))
        ;; remove the permutation
        (setf permutations (remove x permutations))
      )
    )
  )
  permutations
)

;; Given a permutation of all possible color assignments removes 
;; the illigal formulations using the rules in the association list
;; this is done by mapping the city/state to it's index in the map
(defun remove-illigal-permutations (permutations assocList)
  (let ((countryMap ()) (associations ()) (temp ()) (retList ()) )
    (setq countryMap (mapcar #'(lambda(x)(first x) ) assocList)) ;; The index is it's position in the permutation
    (loop for x in assocList do
      (setq temp (second x))
      (if (not (null temp))
      (progn
      (loop for y in temp do
        ;; associations will have the indicies to check 
        (setq associations (append associations (list (position y countryMap :test #'equal))))
      )
      ;; check the permutations and do the removal
      (setf permutations (do-remove permutations associations (position (first x) countryMap :test #'equal )))
      (setq associations ())) 0)
    )
    (setq temp ())
    ;; format the output in the form (provance map_color)
    (loop for x in permutations do
      (setf temp (mapcar #'(lambda (x y) (list x y) ) countryMap x ))
      (setf retList (append retList (list temp)))
      (setf temp ())
    )
    retList
  )
)

;; finds the possible colors to color the map given
;; the map and an alphabet, which represents colors
(defun find-colors (assocList colorList)
  (let ((len (length assocList)) (permutations ()) )
    (setq permutations (generate-permutations colorList len))
    (setq permutations (remove-illigal-permutations permutations assocList))
    permutations
  ) 
)

;; The "main" function takes the map and determines 
;; the number of colors needed to color the map and
;; the possible combinations to color the map squares
(defun color-map (assocList)
  ;; because the most colors we need is 4
  (let ((colorList '( (R G) (R G B) (R G B Y) ) ) (permutations ()))
    (if (eq (length assocList) 1)
      (format t "1 color needed to color map~%")
      (loop for x in colorList do
        (setq permutations (find-colors assocList x))
        (if (not (null permutations))
           (progn
           (format t "Colored the map with ~D colors" (+ (position x colorList :test #'equal) 2))
           (return permutations)
           ) 
        )
      )
    )
  )
)
)
