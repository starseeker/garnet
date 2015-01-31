;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: GARNET-GADGETS; Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;         The Garnet User Interface Development Environment.      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code was written as part of the Garnet project at          ;;;
;;; Carnegie Mellon University, and has been placed in the public   ;;;
;;; domain.  If you are using this code or any part of Garnet,      ;;;
;;; please contact garnet@cs.cmu.edu to be put on the mailing list. ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; This file contains the load gadget.  The functions for this gadget
;;; can be found in the save-load-functions.lisp file.
;;;
;;; Designed and Implemented by Rajan Parthasarathy
;;;

;;; Change log:
;;;
;;; 03/25/93  Andrew Mickish - Removed extra quote from Default-Load-Function
;;; 01/30/93  Rajan Parthasarathy - s-value'd the :initialize slot
;;; 12/14/92  Andrew Mickish - Added type and parameter declarations
;;; 06/26/92  Rajan Parthasarathy - Created
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "GARNET-GADGETS")

(eval-when (eval load compile)
  (export '(LOAD-GADGET)))

(create-instance 'LOAD-GADGET opal:aggregadget
  :declare ((:parameters :parent-window :window-title :window-left :window-top
			 :min-gadget-width :initial-directory :message-string
			 :button-panel-items :button-panel-h-spacing
			 :num-visible :check-filenames-p :modal-p
			 :dir-input-field-font :dir-input-label-font
			 :file-input-field-font :file-input-label-font
			 :message-font :button-panel-font :file-menu-font
			 :selection-function)
	    (:type ((or (is-a-p inter:interactor-window) null) :parent-window)
		   ((or null string) :window-title)
		   (integer :window-left :window-top :button-panel-h-spacing)
		   ((integer 0) :min-gadget-width :num-visible)
		   (string :message-string)
		   (filename-type  :initial-directory)
		   (list :button-panel-items)
		   (kr-boolean :check-filenames-p :modal-p)
		   ((or null function symbol) :selection-function)
		   ((or (is-a-p opal:font) (is-a-p opal:font-from-file))
		    :dir-input-field-font :dir-input-label-font
		    :file-input-field-font :file-input-label-font
		    :message-font :button-panel-font :file-menu-font))
	   (:maybe-constant :left :top :parent-window :window-title :window-left
		     :window-top :dir-input-field-font :dir-input-label-font
		     :message-font :message-string :num-visible :file-menu-font
		     :initial-directory :file-input-field-font
		     :file-input-label-font :button-panel-items
		     :button-panel-font :button-panel-h-spacing
		     :min-gadget-width :modal-p :check-filenames-p))
;;; Customizable slots
  (:LEFT 10)
  (:TOP 24)
  (:PARENT-WINDOW NIL)
  
  ;; Slots for window customization
  (:WINDOW-TITLE "Load Window")
  (:WINDOW-TOP (o-formula (if (gvl :parent-window)
			       (floor (- (gvl :parent-window :height)
					 (gvl :window :height)) 2)
			       0)))
  (:WINDOW-LEFT (o-formula (if (gvl :parent-window)
			      (floor (- (gvl :parent-window :width)
					(gvl :window :width)) 2)
			      0)))

  ;; Slots for dir-input customization
  (:DIR-INPUT-FIELD-FONT (opal:get-standard-font NIL NIL :small))
  (:DIR-INPUT-LABEL-FONT (opal:get-standard-font NIL :BOLD NIL))
   
  ;; Slots for message customization
  (:MESSAGE-FONT (OPAL:GET-STANDARD-FONT :FIXED :ITALIC :SMALL))
  (:MESSAGE-STRING "Fetching directory...")

  ;; Slots for file-menu customization
  (:NUM-VISIBLE 6)
  (:FILE-MENU-FONT (opal:get-standard-font NIL :BOLD NIL))
  (:INITIAL-DIRECTORY #-apple "./" #+apple ":")
   
  ;; Slots for file-input customization
  (:FILE-INPUT-FIELD-FONT (opal:get-standard-font NIL NIL :small))
  (:FILE-INPUT-LABEL-FONT (opal:get-standard-font NIL :BOLD NIL))
   
  ;; Slots for button customization
  (:BUTTON-PANEL-ITEMS '("Load" "Cancel"))
  (:BUTTON-PANEL-FONT OPAL:DEFAULT-FONT)
  (:BUTTON-PANEL-H-SPACING 25)

  ;; Other slots
  (:MIN-GADGET-WIDTH 240)
  (:MODAL-P NIL)
  (:SELECTION-FUNCTION NIL)
  (:CHECK-FILENAMES-P T)
  
;;; Non customizable slots
  (:destroy #'Save-Load-Gadget-Destroy)
  (:parts `(

;;; This is the box labeled "Directory"
    (:DIR-INPUT ,GARNET-GADGETS:SCROLLING-LABELED-BOX
      (:LEFT ,(o-formula (gvl :parent :left) 10))
      (:TOP ,(o-formula (gvl :parent :top) 24))
      (:WIDTH ,(o-formula (gvl :parent :min-gadget-width) 240))
      (:MIN-FRAME-WIDTH NIL)
      (:FIELD-FONT ,(o-formula (gvl :parent :dir-input-field-font)))
      (:FIELD-OFFSET 0)
      (:LABEL-OFFSET 5)
      (:LABEL-FONT ,(o-formula (gvl :parent :dir-input-label-font)))
      (:GROW-P T)
      (:LABEL-STRING "Directory:")
      (:VALUE ,(o-formula (directory-namestring
			   (truename (gvl :parent :initial-directory)))))
      (:SELECTION-FUNCTION Update-File-Menu))

;;;  This is the scrolling menu with a list of files in it
	    
    (:FILE-MENU ,GARNET-GADGETS:SCROLLING-MENU
      (:TOGGLE-P NIL)
      (:FINAL-FEEDBACK-P NIL)
      (:MAX-ITEM-WIDTH ,(o-formula (gvl :min-frame-width)))
      (:LEFT ,(o-formula (+ (gvl :parent :left) 22) 32))
      (:TOP ,(o-formula (+ (gvl :parent :dir-input :top)
			   (gvl :parent :dir-input :height)
			   20)))
      (:INDICATOR-FONT ,(create-instance nil OPAL:FONT
            (:SIZE :SMALL)))
      (:MULTIPLE-P T)
      (:PAGE-INCR 5)
      (:SCR-INCR 1)
      (:SCROLL-SELECTION-FUNCTION NIL)
      (:MENU-SELECTION-FUNCTION file-menu-selection)
      (:H-ALIGN :LEFT)
      (:SCROLL-ON-LEFT-P T)
      (:MIN-FRAME-WIDTH ,(o-formula (- (gvl :parent :min-gadget-width) 60)))
      (:TEXT-OFFSET 4)
      (:INT-MENU-FEEDBACk-P T)
      (:ITEM-FONT ,(o-formula (gvl :parent :file-menu-font)))
      (:V-SPACING 3)
      (:MIN-SCROLL-BAR-WIDTH 20)
      (:INDICATOR-TEXT-P NIL)
      (:PAGE-TRILL-P NIL)
      (:SCR-TRILL-P T)
      (:NUM-VISIBLE ,(o-formula (gvl :parent :num-visible)))
      (:INT-SCROLL-FEEDBACK-P NIL)
      (:TITLE NIL)
      (:SELECTED-RANKS NIL))

;;; This is the box that says "Filename"
	    
    (:FILE-INPUT ,GARNET-GADGETS:SCROLLING-LABELED-BOX
      (:LEFT ,(o-formula (gvl :parent :left)))
      (:TOP ,(o-formula (+ (gvl :parent :file-menu :top)
			   (gvl :parent :file-menu :height)
			   20)))
      (:WIDTH ,(o-formula (gvl :parent :min-gadget-width) 240))
      (:CONSTANT (:KNOWN-AS :COMPONENTS :FIELD-TEXT :FRAME :LABEL-TEXT))
      (:MIN-FRAME-WIDTH NIL)
      (:FIELD-FONT ,(o-formula (gvl :parent :file-input-field-font)))
      (:FIELD-OFFSET 0)
      (:LABEL-OFFSET 5)
      (:LABEL-FONT ,(o-formula (gvl :parent :file-input-label-font)))
      (:GROW-P T)
      (:SELECTION-FUNCTION Check-Load-Filename)
      (:LABEL-STRING "Filename:")
      (:VALUE ""))

;;; This is the little message that appears everytime a directory is being
;;; fetched	    
	   
    (:MESSAGE ,OPAL:TEXT
      (:LEFT ,(o-formula (+ (gvl :parent :left) 20) 30))
      (:TOP ,(o-formula (+ (gvl :parent :top) 20) 44))
      (:FONT ,(o-formula (gvl :parent :message-font))))

;;; These are the two buttons for load and cancel
	    
    (:OK-CANCEL-BUTTONS ,GARNET-GADGETS:TEXT-BUTTON-PANEL
     (:CONSTANT (:KNOWN-AS :COMPONENTS :TEXT-BUTTON-PRESS :FINAL-FEEDBACK
			    :TEXT-BUTTON-LIST))
     (:left ,(o-formula (floor (- (gvl :parent :min-gadget-width)
				  (gvl :width)) 2)))
     (:top ,(o-formula
	     (let ((comps (gvl :parent :components))
		   (me (gvl :parent :ok-cancel-buttons))
		   (maxbot 0))
	       (dolist (ob comps)
		 (when (and (not (equal ob me))
			    (> (opal:bottom ob)
			       maxbot))
		   (setf maxbot (opal:bottom ob))))
	       (+ maxbot 25))))
	   
     (:SELECTION-FUNCTION default-load-function)
     (:FONT ,(o-formula (gvl :parent :button-panel-font)))
     (:H-ALIGN :CENTER)
     (:PIXEL-MARGIN NIL)
     (:RANK-MARGIN NIL)
     (:FIXED-HEIGHT-P T)
     (:FIXED-WIDTH-P T)
     (:INDENT 0)
     (:V-SPACING 5)
     (:H-SPACING ,(o-formula (gvl :parent :button-panel-h-spacing)))
     (:DIRECTION :HORIZONTAL)
     (:SHADOW-OFFSET 5)
     (:TEXT-OFFSET 2)
     (:FINAL-FEEDBACK-P NIL)
     (:GRAY-WIDTH 3)
     (:ITEMS ,(o-formula (gvl :parent :button-panel-items)))))))

(s-value load-gadget :initialize #'Save-Load-Gadget-Initialize)
