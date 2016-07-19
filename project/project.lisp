
;; Maybe we can add the weights to the association list like this.
;; so I am going to write code assuming weights are represented like this.
(defvar *australia* '( (WA ((NT 1) (SA 1))) (NT ((WA 1) (SA 1) (Q 1) )) 
  (SA ((WA 1) (NT 1) (Q 1) (SW 1) (V 1))) (Q ((NT 1) (SA 1) (SW 1))) (SW ((SA 1) (Q 1) (V 1))) (V ((SA 1) (SW 1))) (TS  () ) ))

;; mutate the list so the elements with degree 0 or 1 are removed. 

;; corrisponds to repeatedly remove all verticies with deg 0 
;; or 1 from V and their adjacent edges from E and insert the
;; resulting graph into Gi

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

(defun find-largest-degree (lst)
  (let ( (i 0) (largest () ) )
    (loop while (< i (length lst)) do
      (if (> (length (second (nth i lst))) (length (second largest)))
        (setq largest (nth i lst)) 
      )
      (setq i (+ 1 i))
    )
    largest
  )
)

;; TODO implement weights
(defun MGA (countryList) 
  (let ((F () ) (i 1) (Gi () ) (largest () ) )
    (setq Gi (remove-zero-one countryList))
    (loop while (not (null Gi)) do
      ;; At this point in the algorithm weight is going to
      ;; always be 1 so the smallest weight to degree ration
      ;; will be the node with the largest degree.
      (setq largest (find-largest-degree countryList))
      (setq F (append F largest))
      (setq countryList (remove largest countryList))
      (+ i 1)
      ;; merge the two list together since the call to
      ;; remove-zero-one will most likely return a list
      ;; where some of the elements are already in G1

      ;; TODO
    )
  )
)

