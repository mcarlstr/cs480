
;; Maybe we can add the weights to the association list like this.
;; so I am going to write code assuming weights are represented like this.
(defvar *australia* '( (WA ((NT 1) (SA 1))) 
                       (NT ((WA 1) (SA 1) (Q 1) )) 
                       (SA ((WA 1) (NT 1) (Q 1) (SW 1) (V 1))) 
                       (Q ((NT 1) (SA 1) (SW 1))) 
                       (SW ((SA 1) (Q 1) (V 1))) 
                       (V ((SA 1) (SW 1))) 
                       (TS  () ) ))

(defun remove-zero-one (list)

  (format t "list is ")
  (princ list)
  (format t "~%")

  (setq list (sort list #'(lambda(x y)(< (length (second x)) (length (second y))))))
  (loop while (and (not (null list)) (or (eq 0 (length (second (first list)))) 
    (eq 1 (length (second (first list)))))) do
    (remove-from-neighbors list (second (first list)) (first (first list)))
    (setq list (remove (first list) list))
    (setq list (sort list #'(lambda(x y)(< (length (second x)) (length (second y))))))
    (format t "list after deleting ")
    (princ list)
    (format t "~%")
  )
  list
)

;; example call  (remove-from-neighbors *australia* (second (first *australia*)) 'WA)
  ;; will remove wa from the list portion of all elements in *australia*
;; assocList - the list to cheack
;; associationsList - the list of associations our target symbol
  ;; is associated with
;; symbol the symbol to remove from the nodes
(defun remove-from-neighbors (map associationsList symbol)
  (let ((sym1 () ) )
    (progn
      (loop for x in associationsList do
        ;; store the symbol in the association list
        ;; that we need to find and delete.
        (setq sym1 (first x))
 
        ;; find the list that contains a reference to
        ;; the element we are about to delete and remove 
        ;; the element we are deleting from that elements list.
        (loop for y in map do
          (if (eq (first y) sym1)
                 
            ;; this is the list with the edge we want to remove
            ;; which means the second element of y is the list which
            ;; contains that element.
            (loop for z in (second y) do
              (if (eq (first z) symbol)
                (progn
                  (format t "removing ")
                  (princ z)
                  (format t " from ")
                  (princ y)
                  (format t "~%") 
 
                  ;; this setf screws up the countryList... *BUG*
                  (setf (second y) (remove z (second y)))
                )              
              )
            )
          )
        )
      )
    )
  )
)

;; return the largest degree in the given lst
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

(defun copy (lst)
  (let ((cop () ) (temp1 ()) (temp2 ()) )
    (loop for x in lst do
      (setq temp1 (first x))
      (setq temp2 (copy-list (second x)))
      (setf cop (append cop (list (list temp1 temp2))))
    )
    cop
  )
)

;; This function returns the cutset and mutates countryList 
;; into the desired tree
;; *** Note the first entry in the adjancency list after th
;; the tree has been created is the back edge for the given node.
(defun MGA (countryList) 
  (let ((F () ) (Gi () ) (largest () ) (copy ()) )
    ;; make a deep copy for the functions to mutate
    ;; since we don't want to destry the origional just yet
    (setf copy (copy countryList))
    (setf (second copy) (copy-alist (second countryList)))

    (setq Gi (remove-zero-one copy))
    (loop while (not (null Gi)) do
      ;; At this point in the algorithm weight is going to
      ;; always be 1 so the smallest weight to degree ration
      ;; will be the node with the largest degree.
      (setq largest (find-largest-degree copy))
      
      (format t "Removing ")
      (princ largest)
      (format t "~%")
       
      (setq F (append F (list largest)))
      (break)
      (format t "The cutset is now ")
      (princ F)
      (format t "~%")

      (setf copy (delete largest copy))
      (remove-from-neighbors copy (second largest) (first largest))

      ;; G1 at this point should be === to countryList \ all verticies
      ;; with 0 or one as their degree and without the largest
      ;; node we just found in this iteration. Since the difference
      ;; between country list is such. We should be able to run the
      ;; remove-zero-one algorithm on the countryList graph and receive
      ;; a new G1 which is equivalent to countryList without any nodes with
      ;; degree 0 or 1 (without the node we extracted in this iteration) and
      ;; that should be what we want... I think... that still needs to be tested
      (setq Gi (remove-zero-one copy))
    )

    ;;make tree
    (loop for m in F do
      (princ m)
      (break)
      (remove-from-neighbors countryList (second m) (first m))
      (setf countryList (delete m countryList :test #'equal))
    ) 
    (princ countryList)   
    ;; return the cut set the list passed in
    ;; is now the tree portion.
    F
  )
)

