
(defvar *australia* '( (WA (NT SA)) 
                       (NT (WA SA Q )) 
                       (SA (WA NT Q SW V)) 
                       (Q (NT SA SW)) 
                       (SW (SA Q V)) 
                       (V (SA SW)) 
                       (TS  () ) ))

(defvar *50-states* '(
                    (AL (GA FL MS TN))             ; AL = Alabama
                    (AK ())                        ; AK = Alaska
                    (AZ (CA NV UT CO NM))          ; AZ = Arizona
                    (AR (TX OK MO TN MS LA))       ; AR = Arkansas
                    (CA (OR NV AZ))                ; CA = California
                    (CO (NM AZ UT WY NE KS OK))    ; CO = Colorado
                    (CT (RI NY MA))                ; CT = Conneticut
                    (DE (MD PA NJ))                ; DE = Delaware
                    (DC (MD VA))                   ; DC = D.C.
                    (FL (GA AL))                   ; FL = Florida
                    (GA (SC NC TN AL FL))          ; GA = Georgia
                    (HI ())                        ; HI = Hawaii
                    (ID (WA OR NV UT WY MT))       ; ID = Idaho
                    (IL (WI IA MO KY IN))          ; IL = Illinois
                    (IN (IL KY OH MI))             ; IN = Indiana
                    (IA (MN SD NE MO IL WI))       ; IA = Iowa
                    (KS (CO OK MO NE))             ; KS = Kansas
                    (KY (MO TN VA WV OH IN IL))    ; KY = Kentucky
                    (LA (TX AR MS))                ; LA = Lousiana
                    (ME (NH))                      ; ME = Maine
                    (MD (DE PA WV DC VA))          ; MD = Maryland
                    (MA (RI CT NY VT NH))          ; MA = Mass
                    (MI (OH IN WI))                ; MI = Michigan
                    (MN (WI IA SD ND))             ; MN = Minnesota
                    (MS (LA AR TN AL))             ; MS = Mississippi
                    (MO (KS NE IA IL KY TN AR OK)) ; MO = Missouri
                    (MT (ID WY SD ND))             ; MT = Montana
                    (NE (WY SD IA MO KS CO))       ; NE = Nebraska
                    (NV (CA OR ID UT AZ))          ; NV = Nevada
                    (NH (ME MA VT))                ; NH = New Hampshire
                    (NJ (NY PA DE))                ; NJ = New Jersey
                    (NM (AZ UT CO OK TX))          ; NM = New Mexico
                    (NY (PA NJ CT MA VT))          ; NY = New York
                    (NC (VA TN GA SC))             ; NC = North Carolina
                    (ND (MT SD MN))                ; ND = North Dakota
                    (OH (PA WV KY IN MI))          ; OH = Ohio
                    (OK (TX NM CO KS MO AR))       ; OK = Oklahoma
                    (OR (WA ID NV CA))             ; OR = Oregon
                    (PA (NY NJ DE MD WV OH))       ; PA = Pennsylvania
                    (RI (CT MA))                   ; RI = Rhode Island
                    (SC (GA NC))                   ; SC = South Carolina
                    (SD (WY MT ND MN IA NE))       ; SD = South Dakota
                    (TN (AR MO KY VA NC GA AL MS)) ; TN = Tennessee
                    (TX (NM OK AR LA))             ; TX = Texas
                    (UT (CO NM AZ NV ID WY))       ; UT = Utah
                    (VT (NY MA NH))                ; VT = Vermont
                    (VA (NC TN KY WV MD DC))       ; VA = Virginia
                    (WA (ID OR))                   ; WA = Washington
                    (WV (KY OH PA MD VA))          ; WV = West Virginia
                    (WI (MN IA  IL MI))            ; WI = Wisconsin
                    (WY (ID MT SD NE CO UT))))     ; WY = Wyoming

(defun remove-zero-one (list)

  (format t "list is ")
  (princ list)
  (format t "~%")

  (setq list (sort list #'(lambda(x y)(< (length (second x)) (length (second y))))))
  (loop while (and (not (null list)) (or (eq 0 (length (second (first list)))) 
    (eq 1 (length (second (first list)))))) do
    (remove-from-neighbors list (second (first list)) (first (first list)))
    (setf list (delete (first list) list))
    (setf list (sort list #'(lambda(x y)(< (length (second x)) (length (second y))))))
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
        (setq sym1 x)
        ;; find the list that contains a reference to
        ;; the element we are about to delete and remove 
        ;; the element we are deleting from that elements list.
        (loop for y in map do
          (if (eq (first y) sym1)        
            ;; this is the list with the edge we want to remove
            ;; which means the second element of y is the list which
            ;; contains that element.
            (loop for z in (second y) do
              (if (eq z symbol)
                (progn
                  (format t "removing ")
                  (princ z)
                  (format t " from ")
                  (princ y)
                  (format t "~%") 
 
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

    (setf copy (remove-zero-one copy))
    (setq Gi copy)
    (loop while (not (null Gi)) do
      ;; At this point in the algorithm weight is going to
      ;; always be 1 so the smallest weight to degree ration
      ;; will be the node with the largest degree. Which is at
      ;; the end of the list
      (setq largest (last copy))

      (format t "Removing ")
      (princ largest)
      (format t "~%")
       
      (setq F (append F  largest))
      (format t "The cutset is now ")
      (princ F)
      (format t "~%")

      (setf copy (delete (first (last copy)) copy))
      (remove-from-neighbors copy (second (first largest)) (first (first largest)))

      ;; G1 at this point should be === to countryList \ all verticies
      ;; with 0 or one as their degree and without the largest
      ;; node we just found in this iteration. Since the difference
      ;; between country list is such. We should be able to run the
      ;; remove-zero-one algorithm on the countryList graph and receive
      ;; a new G1 which is equivalent to countryList without any nodes with
      ;; degree 0 or 1 (without the node we extracted in this iteration) and
      ;; that should be what we want... I think... that still needs to be tested
      (setf copy (remove-zero-one copy))
      (setq Gi copy)
    )
    ;; return the cut set
    F
  )
)

;; makes the tree portion on the list
(defun get-tree (countryList minCut)
  ;;make tree
  (loop for m in minCut do
    (setf countryList (delete m countryList :test #'equal))
    (remove-from-neighbors countryList (second minCut) (first minCut))
  )   
)

(let ((explored '()))
(defun has-cycle-rec (map parent)
  
  ;set children to the list of nodes connected to parent
  (let ((children (second (find-if #'(lambda (x) (equal (first x) parent)) map))))
    ;add parent to the list of explored nodes
	(setf explored (cons parent explored))
	(cond ((null map) nil) ;if map is nil, then we've explored everything
          (t (let ((cycle-found nil));otherwise
             ;perform has-cycle-rec on each of the children
			 ;until we've reached a node we have already explored (return false)
			 (dolist (e children cycle-found)
				(if (and (not (equal parent e)) (find e explored))
					(setf cycle-found t)
					(has-cycle-rec map e)
				)
			 )
			)
		  )
	)
  )
)
 
 ;wrapper function that starts out has-cycle-rec
(defun has-cycle (map)
	;start out parent as the very first node that will be explored
  (let ((return_val (has-cycle-rec map (car (car map)))))
    (setf explored '())
    return_val
  )
)
)








