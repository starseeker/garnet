;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: OPAL; Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;         The Garnet User Interface Development Environment.      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code was written as part of the Garnet project at          ;;;
;;; Carnegie Mellon University, and has been placed in the public   ;;;
;;; domain.  If you are using this code or any part of Garnet,      ;;;
;;; please contact garnet@cs.cmu.edu to be put on the mailing list. ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Opal:Macros.Lisp
;;;

;;; This file contains most of the defmacros which are used by Opal.
;;  Others are found in new-defs.lisp and multifont.lisp.

;;  FMG I've tried to change macros to inline functions wherever
;;  possible.


(in-package "KR")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(gv-fixnum gvl-fixnum g-value-fixnum)))

;; FMG Some common idioms supporting optimization.
(defmacro gv-fixnum (object &rest slots)
  `(the fixnum (gv ,object ,@slots)))

(defmacro gvl-fixnum (&rest slots)
  `(the fixnum (gvl ,@slots)))

(defmacro g-value-fixnum (object &rest slots)
  `(the fixnum (g-value ,object ,@slots)))



(in-package "OPAL")

;;; General Use 


;; Wrappers for KR-SEND.
(defmacro add-component (schema &rest args)
  `(let ((the-schema ,schema))
     (kr-send the-schema :add-component the-schema ,@args)))

(defmacro remove-component (schema &rest args)
  `(let ((the-schema ,schema))
     (kr-send the-schema :remove-component the-schema ,@args)))

(defmacro move-component (schema &rest args)
  `(let ((the-schema ,schema))
     (kr-send the-schema :move-component the-schema ,@args)))

(defmacro do-all-components (schema &rest args)
  `(let ((the-schema ,schema))
     (kr-send the-schema :do-all-components the-schema ,@args)))

(defmacro do-components (schema &rest args)
  `(let ((the-schema ,schema))
     (kr-send the-schema :do-components the-schema ,@args)))

;; Added do-items because it would be very helpful to operate over the
;; items of a virtual-aggregate or an aggrelist. [2003/09/16:rpg]

(defmacro do-items (schema &rest args)
  `(let ((the-schema ,schema))
     (kr-send the-schema :do-items the-schema ,@args)))

(defmacro point-to-component (schema &rest args)
  `(let ((the-schema ,schema))
     (kr-send the-schema :point-to-component the-schema ,@args)))

(defmacro point-to-leaf (schema &rest args)
  `(let ((the-schema ,schema))
     (kr-send the-schema :point-to-leaf the-schema ,@args)))

(defmacro fix-update-slots (schema &rest args)
  `(let ((the-schema ,schema))
    (kr-send the-schema :fix-update-slots the-schema ,@args)))

(defmacro initialize (schema &rest args)
  `(let ((the-schema ,schema))
    (kr-send the-schema :initialize the-schema ,@args)))

(defmacro destroy-me (schema &rest args)
  `(let ((the-schema ,schema))
    (kr-send the-schema :destroy-me the-schema ,@args)))

(defmacro destroy (schema &rest args)
  `(let ((the-schema ,schema))
    (kr-send the-schema :destroy the-schema ,@args)))

(defmacro rotate (schema &rest args)
  `(let ((the-schema ,schema))
    (kr-send the-schema :rotate the-schema ,@args)))

(defmacro update (schema &rest args)
  `(let ((the-schema ,schema))
    (kr-send the-schema :update the-schema ,@args)))

(defmacro draw (schema &rest args)
  `(let ((the-schema ,schema))
    (kr-send the-schema :draw the-schema ,@args)))

(defmacro point-in-gob (schema &rest args)
  `(let ((the-schema ,schema))
    (kr-send the-schema :point-in-gob the-schema ,@args)))

(defmacro set-styles (schema &rest args)
  `(let ((the-schema ,schema))
    (kr-send the-schema :set-styles the-schema ,@args)))

(defmacro set-frr-bbox (schema &rest args)
  `(let ((the-schema ,schema))
    (kr-send the-schema :set-frr-bbox the-schema ,@args)))

(defmacro dothings ((varname &rest things) &body body)
 "Same as 'dolist', except 'things' are not a list.  Does not cons."
 (let ((count (length things))
       (tagname   (gensym "TOP-TAG"))
       (countname (gensym "COUNT"))
       case-entries)
   (declare (fixnum count))
   (dolist (thing things)
     (push (list (decf count) thing) case-entries))
   (setq case-entries (nreverse case-entries))
   `(let ((,countname ,(length things))
	  ,varname)
      (declare (fixnum ,countname))
      (tagbody
	 ,tagname
	 (unless (zerop ,countname)
	   (setq ,varname (case (decf ,countname) ,@case-entries))
	   ,@body
	   (go ,tagname))))))


;;; For "objects.lisp"
;;

(declaim (inline get-thickness))	   
(defun get-thickness (gob)
  (let* ((line-style (g-value gob :line-style))
	 (thickness  (and line-style (g-value-fixnum line-style :line-thickness))))
    (if thickness (max thickness 1) 0)))

;; This version of get-thickness AREFs the update-vals array for the
;; line thickness, rather than g-valuing the :line-style slot. Thus,
;; we get the "old" line thickness.
(declaim (inline get-old-thickness))
(defun get-old-thickness (gob line-style-index update-vals)
  (declare (ignore gob))
  (let* ((line-style (aref update-vals line-style-index))
	 (thickness  (and line-style (g-value-fixnum line-style :line-thickness))))
    (declare (type (or fixnum null) thickness))
    (if thickness (max thickness 1) 0)))

(declaim (inline point-in-rectangle))
(defun point-in-rectangle (x y left top right bottom)
  (declare (fixnum left top right bottom))
  (and (<= left x right)
       (<= top y bottom)))


;;  TEXT MACROS

(declaim (inline the-width))
(defun the-width (text-extents)
  (first text-extents))

(declaim (inline the-actual-ascent))
(defun the-actual-ascent (text-extents)
  (second text-extents))

(declaim (inline the-actual-descent))
(defun the-actual-descent (text-extents)
  (third text-extents))

(declaim (inline the-left-bearing))
(defun the-left-bearing (text-extents)
  (fourth text-extents))

(declaim (inline the-right-bearing))
(defun the-right-bearing (text-extents)
  (fifth text-extents))

(declaim (inline the-font-ascent))
(defun the-font-ascent (text-extents)
  (sixth text-extents))

(declaim (inline the-font-descent))
(defun the-font-descent (text-extents)
  (seventh text-extents))


;;   IMAGE MACROS
(declaim (inline read-image))
(defun read-image (pathname &optional root-window)
  (gem:read-an-image (or root-window
			 (g-value gem:device-info :current-root))
		     pathname))

(declaim (inline write-image))
(defun write-image (pathname image &optional root-window)
  (gem:write-an-image (or root-window
			  (g-value gem:device-info :current-root))
		      pathname image))


;; For "basics.lisp"

;;; The accessors for the sides of the gob adjust both the dimensions,
;;  and position of the gob based on the given value.

(declaim (inline left-side))
(defun left-side (gob)
  (g-value gob :left))

(declaim (inline right-side))
(defun right-side (gob)
  (right gob))

(declaim (inline top-side))
(defun top-side (gob)
  (g-value gob :top))

(declaim (inline bottom-side))
(defun bottom-side (gob)
  (bottom gob))


;;; For "text-fonts.lisp"

;; Font-From-File

(declaim (inline extract-dir))
(defun extract-dir (font-name)
  (subseq font-name 0 (1+ (position #\/ font-name :from-end t))))

(declaim (inline extract-font-name))
(defun extract-font-name (font-name)
  (subseq  font-name
	   (1+ (position #\/ font-name :from-end t))
	   (position #\. font-name :from-end t)))


;; For "windows.lisp"

(declaim (inline get-parent-win))
(defun get-parent-win (a-window display-info)
  (let ((win-parent (g-value a-window :parent)))
    (if win-parent
	(g-value win-parent :drawable)
	(display-info-root-window display-info))))

(defmacro With-Cursor (cursor &body body)
  `(unwind-protect
	(progn
	  (change-cursors ,cursor)
	  ,@body)
     (restore-cursors)))

(defmacro With-HourGlass-Cursor (&body body)
  `(unwind-protect
	(progn
	  (change-cursors HourGlass-Pair)
	  ,@body)
     (restore-cursors)))



;; For "clean-up.lisp"

(defmacro opal-window (window-pair)
  `(cdr ,window-pair))

(defmacro clx-window (window-pair)
  `(car ,window-pair))

(defmacro already-been-destroyed (a-window)
  `(not (kr:schema-p ,a-window)))


;; For aggregadgets, aggrelists, etc.

(defmacro add-item (schema &rest args)
 `(let ((the-schema ,schema))
   (kr-send the-schema :add-item the-schema ,@args)))

(defmacro change-item (schema &rest args)
  "Change-Item puts the specified item in the :items list, replacing
the item that was previously in the specified position.
    agg  - the aggrelist or gadget to be changed
    item - the new item to put in the :items list
    n    - the position of the old item to be replaced"
  `(let ((the-schema ,schema))
    (kr-send the-schema :change-item the-schema ,@args)))

(defmacro remove-item (schema &rest args)
 `(let ((the-schema ,schema))
   (kr-send the-schema :remove-item the-schema ,@args)))


;; Virtual aggregates.

(defmacro point-to-rank (schema &rest args)
 `(let ((the-schema ,schema))
   (kr-send the-schema :point-to-rank the-schema ,@args)))

(defmacro do-in-clip-rect ((m n agg rect) &body body)
  `(let* ((agg* ,agg)
	  (p-to-r (g-value agg* :point-to-rank))
	  (r* ,rect)
	  (array-size* (g-value agg* :array-length)) ; list
	  (max-x2* (1- (first array-size*)))
	  (max-y2* (1- (second array-size*)))
	  (first* (first r*))
	  (second* (second r*)))
     (declare (fixnum max-x2* max-y2* first* second*))
     (multiple-value-bind (x1* y1*)
       		          (funcall p-to-r agg* first* second*)
       (declare (fixnum x1* y1*))
       (multiple-value-bind (x2* y2*)
			    (funcall p-to-r agg* (+ first* (third r*) -1)
						 (+ second* (fourth r*) -1))
	 (declare (fixnum x2* y2*))
	 (setq x1* (if x1* (max 0 x1*) 0))
	 (setq y1* (if y1* (max 0 y1*) 0))
	 (setq x2* (if x2* (min x2* max-x2*) max-x2*))
	 (setq y2* (if y2* (min y2* max-y2*) max-y2*))
	 (when (and (<= x1* x2*) (<= y1* y2*))
	   (do ((,m x1* (1+ ,m)))
	       ((> ,m x2*))
	     (declare (fixnum ,m))
	     (do ((,n y1* (1+ ,n)))
	         ((> ,n y2*))
	       (declare (fixnum ,n))
	       ,@body)))))))


;; Utility

(defmacro swap(a b) `(rotatef ,a ,b))

;; This seems useful....
(defmacro half(n)
  `(round (/ ,n 2.0)))

;;; Some premature optimization.
(declaim (inline q-min))
(defun q-min (x y)
  "Two-argument fixnum version of min."
  #+cmu
  (declare (values fixnum))
  (declare (fixnum x y))
  (if (< x y) x y))


(declaim (inline q-max))
(defun q-max (x y)
  "Two-argument fixnum version of max."
  #+cmu 
  (declare (values fixnum))
  (declare (fixnum x y))
  (if (< x y) y x))


(declaim (inline q-abs))
(defun q-abs (x)
  "Fixnum version of abs."
  #+cmu
  (declare (values fixnum))
  (declare (fixnum x))
  (if (< x 0) (- x) x))
