;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: GILT; Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;         The Garnet User Interface Development Environment.      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code was written as part of the Garnet project at          ;;;
;;; Carnegie Mellon University, and has been placed in the public   ;;;
;;; domain.  If you are using this code or any part of Garnet,      ;;;
;;; please contact garnet@cs.cmu.edu to be put on the mailing list. ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; The "Value Filter" extension to Gilt is a tool for assigning dependencies
;;; among objects.  With the Value Control module, the values returned by
;;; gadgets can be defined by a user-supplied expression.  The Enable Control
;;; module allows the user to define expressions that regulate whether gadgets
;;; are enabled and may be operated.  The Error Checking module allows the
;;; definition of an error handling routine, complete with a customized
;;; error dialog box.
;;;
;;; Designed by Brad Myers
;;; Implemented by Andrew Mickish

;;; CHANGE LOG:
;;; 12/31/92 Andrew Mickish - Removed an update call in Remove-Panel-If-Unused
;;;            


;;
;;  Error Control module
;;

(in-package "GILT")

(declaim (special ERROR-CHECK))

;;; *** NOTES on the implementation ***
;;;   This menu gets error value/error message pairs.  That is, when
;;; the object has a particular value, then the Gilt error gadget
;;; should pop up and display the message corresponding to that value.
;;; The value/message pairs are kept in an association list in the
;;; :error-check-alist slot of the object itself.  The formula in the
;;; :filtered-value slot (which is generated by the Value Control menu) 
;;; then takes care of popping up the appropriate message when the
;;; :value slot has an error value.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Utility functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Searches through each labeled-box-panel in the menu and finds
;; the labeled box that the user is currently editing
;;
(defun Get-Active-Box (ec)
  (do ((e-gadgets (g-value (g-value ec :panel-list) :components)
		  (cdr e-gadgets)))
      ((when e-gadgets
	 (let* ((e-gadget (car e-gadgets))
		(text-inter (g-value e-gadget :error-string
				     :field-text :text-edit)))
	   (eq (g-value text-inter :current-state) :running)))
       (when e-gadgets
	 (g-value (car e-gadgets) :error-string)))))
	
;; Insert a "(gvl ... :filtered-value)" call into the given labeled box
;;
(defun EC-Insert-the-Ref (src-obj dest-obj l-box)
  (let ((ref `(gvl ,@(make-path src-obj dest-obj) :filtered-value)))
    (gg:insert-text-into-box l-box (write-to-string ref :case :downcase))))

;; Selection function for the "Use Value of Object" button
;;
(defun EC-Insert-Ref-Obj-Into-Str (gadget val)
  (declare (ignore val))
  (let* ((ec (g-value gadget :parent))
	 (sel-obj (g-value *Selection-Obj* :value))
	 (current-box (get-active-box ec))
	 (for-obj (g-value ec :for-object)))
    (cond ((null sel-obj)(gilt-error "Nothing selected"))
	  ((listp sel-obj) (dolist (o (reverse sel-obj))
			     (EC-Insert-The-Ref for-obj o current-box)))
	  ((null current-box)(gilt-error "No error string is
currently being edited."))
	  (t (EC-Insert-The-Ref for-obj sel-obj current-box)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Final functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Called by the "Another Error Check" button.
;; Adds a new labeled-box-panel to the menu.
;;
(defun Add-Panel (gadget val)
  (declare (ignore val))
  (let* ((main-gadget (g-value gadget :parent))
	 (error-list (g-value main-gadget :panel-list)))
    (opal:add-item error-list)
    (s-value (g-value error-list :window)
	     :height
	     (+ 10 (opal:bottom (g-value main-gadget :another-panel))))
    (opal:update (g-value error-list :window))))


;; Called by the Error Value labeled box.
;; Removes the labeled-box-panel from the aggrelist when the user erases
;; the strings from the scrolling labeled boxes.
;;
(defun Remove-Panel-If-Unused (box val)
  ; If this labeled box is empty...
  (when (equal "" val)
    (let* ((e-gadget (g-value box :parent))
	   (other-box
	    (car (remove box (list (g-value e-gadget :error-value)
				   (g-value e-gadget :error-string)))))
	   (error-list (g-value e-gadget :parent))
	   (win (g-value error-list :window)))
      ; And the other labeled box is empty...
      (when (and (equal "" (g-value other-box :value))
		 (> (g-value error-list :items) 1))
	; Then remove this panel of labeled boxes from the aggrelist.
	(opal::remove-nth-item error-list (g-value e-gadget :rank))
	;(opal:update win)
	(s-value win :height (+ 10 (opal:bottom (g-value error-list :parent
							 :another-panel)))))
      (opal:update win))))


;; Called by the OK-Cancel buttons in the Error-Check menu.
;; Sets the :error-check-alist slot in the actual object with the error
;; values and strings (the strings are really format statement expressions --
;; see documentation for Set-Format-Expr).  The VC menu is responsible
;; for creating the formula that makes use of these values.
;;
(defun Set-Error-Val-Alist (ec-menu vals)
  (declare (ignore vals))
  (let* ((box-panels (g-value ec-menu :panel-list :components))
	 (for-obj (g-value ec-menu :for-object))
	 (alist NIL))
    (dolist (bp box-panels)
      (let ((error-value (g-value bp :error-value :value))
	    (error-string (g-value bp :error-string :format-expr)))
	(unless (equal "" error-value)
	  (multiple-value-bind (val errorp)
	      (gg:Careful-Read-From-String error-value *error-gadget*)
	    (unless errorp
	      (push (cons val error-string) alist))))))
    (s-value for-obj :error-check-alist alist)))


;; This function takes the value of the error-string labeled box and
;; puts the strings and object references together into a format statement
;; that can be evaluated and given to the Gilt error gadget.  The format
;; statement (in expression form, not in string form) is set in the
;; :format-expr slot of the labeled box.
;;
;; The variable *format-args* is referenced in Set-Format-Expr but only
;; set in subroutines.  *format-args* is a list of references to objects that
;; must be evaluated at the time that the message appears (i.e., we can't
;; just evaluate the g-values now because the objects will have different
;; values later).  So, we have to create a format statement that contains
;; the error message with ~A's in it, e.g.,
;;   (format NIL "Object now has value ~A" (g-value object :value))
;; This format statement will be evaluated just before the error gadget
;; is displayed, so the message will contain the right values.
;;
(defvar *format-args* NIL)

(defun Set-Format-Expr (gadget value)
  (setf *format-args* NIL)  ; Set in decompose-aux
  (s-value gadget
	   :format-expr
	   (if (equal "" value)
	       ""
	       (append `(format NIL ,(decompose-string value))
		       (reverse *format-args*)))))

(defun decompose-string (str)
  (case (char str 0)
    (#\space (decompose-string (subseq str 1)))
    (t (decompose-aux str))))

(defun decompose-aux (str)
  (let (substr substr-len remainder)
    (multiple-value-setq
	(substr substr-len) (read-from-string str))
    (unless (stringp substr)
      (push substr *format-args*)
      (setq substr "~A"))
    (setq remainder (subseq str substr-len))
    (if (equal "" remainder)
	substr
	(concatenate 'string substr (decompose-string remainder)))))

;; The functions below, which compose strings from format statements, undo
;; the work of the functions above.  The input to compose-string is a
;; format expression, like (format NIL "value = ~A" (g-value obj :value)).
;;
(defun compose-string (expr)
  (let* ((format-string (caddr expr))
	 (format-args (cdddr expr))
	 (str-len (length format-string))
	 (leading-arg (string= "~A" (subseq format-string 0 2)))
	 (trailing-arg (string= "~A" (subseq format-string (- str-len 2)))))
    ;; Add quote to head of string unless it begins with a format arg.
    (unless leading-arg
      (setq format-string (concatenate 'string "\"" format-string))
      (incf str-len))
    ;; Likewise with the tail.
    (unless trailing-arg
      (setq format-string (concatenate 'string format-string "\""))
      (incf str-len))
    ;; Insert the args into the string, along with quotes as delimiters
    (do* ((i 0 (1+ i))
	  (c (char format-string i) (char format-string i)))
	 ((= i (- str-len 1))
	  (let* ((right-trimmed-string 
		  (if trailing-arg
		      (string-right-trim '(#\" #\space) format-string)
		      format-string))
		 (left-trimmed-string
		  (if leading-arg
		      (string-left-trim '(#\" #\space) right-trimmed-string)
		      right-trimmed-string)))
	    left-trimmed-string))
      (when (and (char= #\~ c)
		 (char= #\A (char format-string (1+ i))))
	; found a ~A, replace with an arg
	(let* ((arg-string (prin1-to-string (car format-args)))
	       (arg-string-len (length arg-string)))
	  (setq format-string (concatenate 'string
				(subseq format-string 0 i)
				"\" " arg-string " \""
				(subseq format-string (+ 2 i))))
	  (incf i (+ 2 arg-string-len))
	  (incf str-len (+ 2 arg-string-len))
	  (setq format-args (cdr format-args)))))))
  

(defun Error-Check-OK-Fn (ec vals)
  (Set-Error-Val-Alist ec vals)
  (Close-EC-Win ec))

(defun Error-Check-Cancel-Fn (ec vals)
  (declare (ignore vals))
  (Close-EC-Win ec))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Top-level function to display an error check menu for an object
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defparameter *Used-EC-Wins* NIL)

(defun Close-EC-Win (ec)
  (setf *vc-position-list* (remove (g-value ec :position-index)
				   *vc-position-list*))
  (push (g-value ec :window) *Used-EC-Wins*))

;; Use the *vc-position-list* defined in motif-value-control.lisp so that
;; the error-check menus are aligned with the value control menus.
;;
(defun Pop-Used-EC-Win (for-obj title)
  (let ((ec-win (pop *Used-EC-Wins*)))
    (when ec-win
      (let ((ec (car (g-value ec-win :aggregate :components)))
	    (i (next-vc-position-index)))
	(push i *vc-position-list*)
	(s-value ec-win :left (next-vc-left i))
	(s-value ec-win :top (next-vc-top i))
	(s-value ec :position-index i)
	(s-value ec-win :title title)
	(s-value ec :for-object for-obj)
	(s-value ec-win :visible T)
	ec))))

(defun Create-EC-Win (for-obj title)
  (let* ((i (next-vc-position-index))
	 (ec (create-instance NIL ERROR-CHECK
		(:position-index i)
		(:for-object for-obj)
		(:window-title title))))
    (show-in-window ec (next-vc-left i) (next-vc-top i))
    ec))


(defun Destroy-Error-Check-Wins ()
  (dolist (ec (g-value ERROR-CHECK :is-a-inv))
    (opal:destroy (g-value ec :window)))
  (setf *Used-EC-Wins* NIL))


;; Set up the labeled-box panels with the error values that have already
;; been defined for the object.  The error values are in an association
;; list, i.e., ((val . string) ...).  Iterate over the list of pairs in
;; the association list and set the strings of the labeled boxes in the
;; error check panel.
;;
(defun Init-Error-Check (ec ec-vals)
  (let* ((panel-agglist (g-value ec :panel-list))
	 (old-items-length (g-value panel-agglist :items))
	 (new-items-length (max 1 (length ec-vals))))

    (unless (eql old-items-length new-items-length)
      (s-value panel-agglist :items new-items-length))

    (if ec-vals
	;; Install error check values and strings
	(do* ((panels (g-value panel-agglist :components) (cdr panels))
	      (panel (car panels) (car panels))
	      (panel-vals ec-vals (cdr panel-vals))
	      (panel-val (car panel-vals) (car panel-vals)))
	     ((null panels) T)
	  (let ((error-value (g-value panel :error-value))
		(error-string (g-value panel :error-string)))
	    (s-value error-value :value (prin1-to-string (car panel-val)))
	    (s-value error-string :value (compose-string (cdr panel-val)))
	    ;; Initialize :format-expr value, also
	    (Set-Format-Expr error-string (g-value error-string :value))))
	;; Clear the panel
	(let* ((panel (car (g-value panel-agglist :components)))
	       (error-value (g-value panel :error-value))
	       (error-string (g-value panel :error-string)))
	  (s-value error-value :value "")
	  (s-value error-string :value "")
	  (Set-Format-Expr error-string "")))
    (s-value (g-value ec :window)
	     :height
	     (+ 5 (opal:bottom (g-value ec :another-panel))))))


(defun Show-Error-Check (ec-button value)
  (declare (ignore value))
  (let* ((for-obj (g-value ec-button :parent :for-object))
	 (ec-vals (g-value for-obj :error-check-alist))
	 (title (concatenate 'string
			     "Error Check:  " (name-for-schema for-obj)))
	 (ec (or (Pop-Used-EC-Win for-obj title)
		 (Create-EC-Win for-obj title))))
    (Init-Error-Check ec ec-vals)
    (opal:update (g-value ec :window))
    ec))




;;
;;  Motif Error Control objects
;;


;; Make this an instance of a Motif-gadget-prototype, so it will get
;; the appropriate colors automatically.
;;
(create-instance 'LABELED-BOX-PANEL garnet-gadgets::MOTIF-GADGET-PROTOTYPE
  (:width (o-formula (gvl :parent :width)))
  (:height 54)
  (:right (o-formula (+ (gvl :left)(gvl :width) -3)))
  (:bottom (o-formula (+ (gvl :top)(gvl :height) -3)))
  (:parts `(
    (:top-left ,opal:polyline
      (:line-style ,(o-formula (gvl :parent :shadow-line-style)))
      (:point-list ,(o-formula (let* ((x1 (gvl :parent :left))
				      (y1 (gvl :parent :top))
				      (x2 (gvl :parent :right))
				      (y2 (gvl :parent :bottom)))
				 (list x1 y2 x1 y1 x2 y1)))))
    (:bot-right ,opal:polyline
      (:line-style ,(o-formula (gvl :parent :highlight-line-style)))
      (:point-list ,(o-formula (let* ((x1 (gvl :parent :left))
				      (y1 (gvl :parent :top))
				      (x2 (gvl :parent :right))
				      (y2 (gvl :parent :bottom)))
				 (list (1- x1) y2 x2 y2 x2 (1- y1))))))
    (:error-value ,GARNET-GADGETS:MOTIF-SCROLLING-LABELED-BOX
      (:left ,(o-formula (+ 4 (gvl :parent :left))))
      (:top ,(o-formula (+ 4 (gvl :parent :top))))
      (:width ,(o-formula (- (gvl :parent :width) 13)))
      (:label-string "If value is")
      (:value "")
      (:selection-function Remove-Panel-If-Unused))
    (:error-string ,GARNET-GADGETS:MOTIF-SCROLLING-LABELED-BOX
      (:left ,(o-formula (+ (gvl :parent :left) 18)))
      (:top ,(o-formula (+ (gvl :parent :top) 25)))
      (:width ,(o-formula (- (gvl :parent :width) 27)))
      (:label-string "Error String or Function")
      (:value "")
      (:active-p ,(o-formula (not (string-equal
				   (gvl :parent :error-value :value) ""))))
      (:selection-function ,#'(lambda (g v)
				(Set-Format-Expr g v)
				(Remove-Panel-If-Unused g v)))))))


(create-instance 'ERROR-CHECK opal:aggregadget
   (:window-left 400) (:window-top 400)
   (:window-width 490) (:window-height 161)
   (:window-title "Error Check") (:package-name "GILT")
   (:window-background-color opal:motif-gray)
   (:left 4) (:top 10)
   (:width 505)
   (:height (o-formula (- (opal:gv-bottom (gvl :another-panel)) (gvl :top))))
   (:For-Object NIL)
   (:function-for-ok 'Error-Check-OK-Fn)
   (:function-for-cancel 'Error-Check-Cancel-Fn)
   (:parts
    `((:title ,opal:text
       (:left ,(o-formula (gvl :parent :left)))
       (:top ,(o-formula (gvl :parent :top)))
       (:string ,(o-formula (concatenate 'simple-string
			     "Error Check for \""
			     (Get-User-Name (gvl :parent :for-object))
			     "\"")))
       (:font ,(opal:get-standard-font NIL :bold-italic :large)))
      (:ok-cancel ,garnet-gadgets:motif-text-button-panel
       (:left 300) ;(:left 360)
       (:top ,(o-formula (+ 5 (opal:gv-bottom (gvl :parent :title)))))
       (:direction :horizontal)
       (:items ("OK" "Apply" "Cancel"))
       (:selection-function gilt:okcancel-function))
      (:insert-ref ,GARNET-GADGETS:MOTIF-TEXT-BUTTON
       (:selection-function EC-Insert-Ref-Obj-Into-Str)
       (:string "Use Value of Object")
       (:left ,(o-formula (+ 10 (gvl :parent :left))))
       (:top ,(o-formula (+ 5 (opal:gv-bottom (gvl :parent :title)))))
       (:interactors
	((:press :modify
		 (:waiting-priority ,inter:running-priority-level)))))
      (:panel-list ,opal:aggrelist
       (:left ,(o-formula (+ 10 (gvl :parent :left))))
       (:top ,(o-formula (+ 10 (opal:gv-bottom (gvl :parent :insert-ref)))))
       (:width 465)
       (:direction :vertical)
       (:v-spacing 7)
       (:items 1)
       (:item-prototype ,LABELED-BOX-PANEL))
      (:another-panel ,GARNET-GADGETS:MOTIF-TEXT-BUTTON
       (:left ,(o-formula (+ 10 (gvl :parent :left))))
       (:top ,(o-formula (+ 5 (opal:gv-bottom (gvl :parent :panel-list)))))
       (:string "Another Error Check")
       (:selection-function ,#'Add-Panel)))))

