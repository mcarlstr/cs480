
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


;;; DD *******************************************************************
;;; Everything added between here and the next DD *********** line
;;; added by David DiMenna originally.
;;;
;;; Just doing this to make it easier to note and so that I don't mess up any
;;; other code.

;;; Takes generated cutset and creates an assoc list by referencing
;;; original assoc-list.  This new assoc list is self-contained, the only edges 
;;; included are ones which connect to other vertices in the cutset.

(defun gen-cutset-assoc (cutset assoc-list)
  (let ((cutset-assoc))
    (dolist (current cutset)
      (setf cutset-assoc (cons (find current assoc-list :key #'car) 
        cutset-assoc))
    )

    ;; scrub new assoc-list, removing edges to vertices outside of cutset
    (dolist (current cutset-assoc)
      (dolist (x (first(rest current)))
        (if (not (member x cutset))
          (setf (first(rest current)) (remove x (first(rest current))))
        )
      )
    )
    (reverse cutset-assoc)
  )
)


;;; helper to allow sorting by degree (length of cdr in assoc list)
(defun deg-sort (x y)
  (< (length (cadr x)) (length (cadr y)))
)


;;; helper, sort alphabetically by vertex name in assoc-list

(defun alp-sort (x y)
  (string-lessp (car x) (car y))
)


;;; New coloring function. Attempts to find legal color assignment for entire
;;; assoc list by starting at high degree vertices first.
;;;
;;; This function is extremely greedy and will simply ignore states that dont
;;; have any legal color options, so don't feed it a big assoc-list that hasnt
;;; been cutsetted (is that a word?)
;;;
;;; This will naturally color with the fewest possible colors.
;;;
;;; Requires an assoc-list, color-list, and any predetermined colorings (used
;;; when integrating a cutset coloring with the rest of the map, otherwise 3rd
;;; argument is just nil)
;;;
;;; Returns list of cons pairs of (VERTEX . COLOR), sorted by VERTEX

(defun color-greedy (assoc-list color-list coloring)
  (let ((color-assignment)
      (sorted-list))

    (setf color-assignment coloring)
    (setf sorted-list (reverse (sort (copy-seq assoc-list) #'deg-sort)))

    (dolist (current sorted-list)
      (let ((red-flag)  ; t if a neighbor is red (or whatever 1st color is)
          (green-flag)  ; t if a neighbor is green (2nd)
          (blue-flag) ; t if a neighbor is blue (3rd)
          (yellow-flag)); t if a neighbor is yellow (4th in color list)

        (setf red-flag nil)
        (setf blue-flag nil)
        (setf green-flag nil)
        (setf yellow-flag nil)

        (dolist (x (cadr current))
          (let ((temp-check))
            (setf temp-check (find x color-assignment :key #'car))
            (if temp-check
              (cond
                ((eql (cdr temp-check) (nth 0 color-list)) 
                  (setf red-flag t))
                ((eql (cdr temp-check) (nth 1 color-list)) 
                  (setf green-flag t))
                ((eql (cdr temp-check) (nth 2 color-list)) 
                  (setf blue-flag t))
                ((eql (cdr temp-check) (nth 3 color-list)) 
                  (setf yellow-flag t))
                (t nil)
              )
            )
          )
        )

        ;; color according to flags if the vertex isnt already in list
        ;; of color assignments
        (if (not (find (car current) color-assignment :key #'car))
          (cond 
            ((not red-flag)
              (setf color-assignment (cons (cons (car current) (nth 0 color-list))
                color-assignment)))
            ((not green-flag)
              (setf color-assignment (cons (cons (car current) (nth 1 color-list))
                color-assignment)))
            ((not blue-flag)
              (setf color-assignment (cons (cons (car current) (nth 2 color-list))
                color-assignment)))
            ((not yellow-flag)
              (setf color-assignment (cons (cons (car current) (nth 3 color-list))
                color-assignment)))
            (t (format t "~% NO LEGAL COLOR ASSIGNMENT ~%"))
          )
        )
      )
    )
    color-assignment
  )
)


;;; Simple function to scrub output from MGA function to a list of vertices only
(defun scrub-mga (mga-out)
  (let ((new-out))
    (dolist (current mga-out)
      (setf new-out (cons (car current) new-out))
    )
    new-out
  )
)



;;; Wrapper function to handle coloring of map in several steps.
;;; Requires the original assoc-list, the cutset of just vertices, and the color
;;; list.
;;; Example function call: (color-map *50-states* '(AK HI ME CO) '(R G B Y))
;;;
;;; Returns a list of cons pairs indicating color assignments in 
;;; alphabetical order according to vertex name
;;; Example output: ((AK R) (CA B) (MD R) (NM G) (WY B))

(defun color-map (assoc-list color-list)
  (let ((cutset-assoc)
      (cutset-coloring)
      (tree-coloring)
      (cutset))

    (setf cutset (scrub-mga (mga assoc-list)))
    (setf cutset-assoc (gen-cutset-assoc cutset assoc-list))
    (setf cutset-coloring (color-greedy cutset-assoc color-list nil))
    (setf tree-coloring (color-greedy assoc-list color-list cutset-coloring))
    (sort tree-coloring #'alp-sort)
  )
)



;;; Function just to show an example run

(defmacro pr (form)
  `(format t "~%~a~%~%~a~%" ',form ,form)
)

(defun run-proj ()

  (format t "~%-------------------------------------~%*50-states* with 4 colors:~%")
  (pr (color-map *50-states* '(R G B Y)))
  
  (format t "~%-------------------------------------~%Australia Map with 4 colors (note only 3 used):~%")
  (pr (color-map *australia* '(R G B Y)))

)


;;; end of DD contribution
;;; DD *********************************************************************
