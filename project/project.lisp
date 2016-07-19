
(defvar *australia* '( (WA (NT SA)) (NT (WA SA Q)) (SA (WA NT Q SW V)) (Q (NT SA SW)) (SW (SA Q V)) (V (SA SW)) (TS  () ) ))

;; mutate the list so the elements with degree 0 or 1 are removed. 
(defun remove-zero-one (list)
  (let ((sym1 () ) (sym2 () ) (temp () ) )
    (loop for x in list do
      ;; if it has no associations just delete it
      (if (eq 0 (length (second x)))
        (setf list (remove x list :test #'equal)) 
      )

      ;; if it has one association we need to delete the element
      ;; and remove it from the list it is associated with i.e.
      ;; the edge between u - v
      (if (eq 1 (length (second x)))
        (progn
          ;; store the symbol in the association list
          ;; that we need to find and delete.
          (setq sym1 (first (second x)))
          (setq sym2 (first x))
          ;; remove the element with one edge.
          (setf list (remove x list :test #'equal))
          ;; find the list that contains a reference to
          ;; the element we are about to delete and remove 
          ;; the element we are deleting from that elements list.
          (loop for y in list do
            (if (eq (first y) sym1)
              (progn 
                ;; this is the list with the edge we want to remove
                ;; which means the second element of y is the list which
                ;; contains that element.
                (setq temp (second y))
                (setq temp (remove sym2 (second y) :test #'equal))
                (setf (second y) temp)
              )
            )
          )
        )
      )
    )
  )
  list  
)

(defun MGA (countryList) 
  (let ((F ()) (i 1) )
   
  )
)

