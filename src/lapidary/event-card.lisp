;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: LAPIDARY; Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;         The Garnet User Interface Development Environment.      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code was written as part of the Garnet project at          ;;;
;;; Carnegie Mellon University, and has been placed in the public   ;;;
;;; domain.  If you are using this code or any part of Garnet,      ;;;
;;; please contact garnet@cs.cmu.edu to be put on the mailing list. ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This file created by GILT V1.1: The Garnet Interface Builder
;;; on Apr 8, 1992, 11:43 AM
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "LAPIDARY")

(eval-when (:execute :load-toplevel :compile-toplevel)
  (export '(EVENT-CARD)))

(create-instance 'EVENT-CARD OPAL:AGGREGADGET
  (:LEFT 0)
  (:TOP 0)
  (:WIDTH (o-formula (KR:GVL :WINDOW :WIDTH) 595))
  (:HEIGHT (o-formula (KR:GVL :WINDOW :HEIGHT) 300))
  (:FUNCTION-FOR-OK 'event-ok)
  (:function-for-cancel 'event-cancel)
  (:EXPORT-P T)
  (:WINDOW-TITLE "event-window")
  (:PACKAGE-NAME "LAPIDARY")
  (:WINDOW-HEIGHT 289)
  (:WINDOW-WIDTH 632)
  (:WINDOW-TOP 419)
  (:WINDOW-LEFT 25)
  (:parts `(
    (NIL ,OPAL:RECTANGLE
      (:LEFT 21)
      (:TOP 38)
      (:WIDTH 587)
      (:HEIGHT 239)
      (:CONSTANT (T )))
    (NIL ,OPAL:MULTI-TEXT
      (:LEFT 35)
      (:TOP 56)
      (:CONSTANT (T ))
      (:STRING "Modifiers:")
      (:FONT ,(create-instance nil OPAL:FONT
            (:FACE :BOLD))))
    (NIL ,OPAL:MULTI-TEXT
      (:LEFT 35)
      (:TOP 118)
      (:CONSTANT (T ))
      (:STRING "Mouse Action:")
      (:FONT ,(create-instance nil OPAL:FONT
            (:FACE :BOLD))))
    (NIL ,OPAL:MULTI-TEXT
      (:LEFT 35)
      (:TOP 180)
      (:CONSTANT (T ))
      (:STRING "Keyboard:")
      (:FONT ,(create-instance nil OPAL:FONT
            (:FACE :BOLD))))
    (:modifiers ,GARNET-GADGETS:MOTIF-CHECK-BUTTON-PANEL
      (:LEFT 156)
      (:TOP 51)
      (:CONSTANT (T ))
      (:selection-function modifiers-handler)
      (:ITEMS ("shift" "control" "meta" "any modifier" ))
      (:H-SPACING 10)
      (:V-SPACING 5)
      (:INDENT 0)
      (:DIRECTION :HORIZONTAL)
      (:FIXED-HEIGHT-P NIL)
      (:FONT ,OPAL:DEFAULT-FONT)
      (:BUTTON-HEIGHT 20)
      (:FIXED-WIDTH-P NIL)
      (:BUTTON-WIDTH 20)
      (:RANK-MARGIN NIL)
      (:PIXEL-MARGIN NIL)
      (:TEXT-ON-LEFT-P T))
    (:mouse ,GARNET-GADGETS:MOTIF-RADIO-BUTTON-PANEL
      (:LEFT 156)
      (:TOP 95)
      (:CONSTANT (T ))
      (:selection-function mouse-handler)
      (:ITEMS ("leftdown" "middledown" "rightdown" "any-mousedown" "leftup" "middleup" "rightup" "any-mouseup" ))
      (:H-SPACING 10)
      (:V-SPACING 5)
      (:INDENT 0)
      (:DIRECTION :HORIZONTAL)
      (:FIXED-HEIGHT-P T)
      (:FONT ,OPAL:DEFAULT-FONT)
      (:BUTTON-DIAMETER 23)
      (:FIXED-WIDTH-P NIL)
      (:RANK-MARGIN 4)
      (:PIXEL-MARGIN NIL)
      (:TEXT-ON-LEFT-P T))
    (:key ,GARNET-GADGETS:LABELED-BOX
      (:LEFT 292)
      (:TOP 178)
      (:CONSTANT (T ))
      (:LABEL-STRING "Specific keypress:")
      (:selection-function key-handler)
      (:value "")
      (:MIN-WIDTH 120)
      (:FIELD-OFFSET 6)
      (:FIELD-FONT ,*labeled-box-field-font*)
      (:MIN-FRAME-WIDTH 10)
      (:LABEL-OFFSET 5)
      (:LABEL-FONT ,*labeled-box-label-font*))
    (:any-key ,GARNET-GADGETS:MOTIF-RADIO-BUTTON
      (:LEFT 156)
      (:TOP 173)
      (:CONSTANT (T ))
      (:string "Any keypress:")
      (:selection-function any-key-handler)
      (:INDENT 0)
      (:FONT ,OPAL:DEFAULT-FONT)
      (:BUTTON-DIAMETER 23)
      (:FIXED-WIDTH-P T)
      (:TEXT-ON-LEFT-P T))
    (NIL ,GARNET-GADGETS:MOTIF-TEXT-BUTTON-PANEL
      (:LEFT 489)
      (:TOP 3)
      (:CONSTANT (T ))
      (:ITEMS ("OK" "Cancel" ))
      (:FINAL-FEEDBACK-P NIL)
      (:DIRECTION :HORIZONTAL)
      (:SELECTION-FUNCTION GILT:OKCANCEL-FUNCTION))
    (NIL ,GARNET-GADGETS:MOTIF-TEXT-BUTTON-PANEL
      (:LEFT 35)
      (:TOP 220)
      (:CONSTANT (T ))
      (:selection-function add-del-event)
      (:ITEMS ("Add an event" "Delete this event" ))
      (:FINAL-FEEDBACK-P nil)
      (:H-SPACING 5)
      (:V-SPACING 5)
      (:INDENT 0)
      (:DIRECTION :HORIZONTAL)
      (:FIXED-HEIGHT-P T)
      (:FONT ,OPAL:DEFAULT-FONT)
      (:FIXED-WIDTH-P T)
      (:RANK-MARGIN NIL)
      (:PIXEL-MARGIN NIL)
      (:H-ALIGN :CENTER)))))



