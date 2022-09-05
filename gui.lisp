;; This is an attempt at a GUI node editor, using LTK.
;;
;; Goals:
;; - Edit patches with a Node-based editor
;; - Easily edit and create DSP blocks in sexp assembly
;; - Create simple file format for saving and recalling patches
;;
;; (not necessarily GUI only)
;; - Upload via serial port & wireless link
;; - Quickly update (<2s) a running patch
;; - Emulate FV-1 and run patch on PC
;;
(require 'asdf)
(require 'ltk)
(use-package :ltk)

(defun make-buttons (text-list master)
  (loop for text in text-list
        counting t into i do
          (grid
           (make-instance 'button :master master :text text :width 10)
           0 i :padx 5)))

(defun rect-coords (x y width height)
  (let ((ret-x (max (- x (/ width 2)) 0))
        (ret-y (max (- y (/ height 2)) 0)))
    (list
     ret-x ret-y
     (+ width ret-x) (+ height ret-y))))

;; (exit-wish)
;; (make-node-editor node-editor-frame)
(defun make-node-editor (master)
  (grid
   (let ((canvas (make-instance 'canvas :master master)))

     ;; Spawn node / square
     (let ((rect-handle (create-rectangle canvas 50 50 100 100)))
       ;; Move square to mouse position on click
       (bind canvas "<B1-Motion>" #'(lambda (evt)
                              (set-coords canvas rect-handle
                                          (rect-coords (event-x evt) (event-y evt) 50 50)))))
     ;;  do stuff
     ) 0 0 :sticky "nsew"))

(defun some-text ()
  (format nil "Did you know ? This is a helpful tooltip, blah blah blah~%Linefeed haha"))

;; Start LTK
(start-wish)

(wm-title *tk* "FV-1 studio")
(ltk::use-theme "clam")

(defparameter toolbar-frame       (make-instance 'frame :borderwidth 2 :relief :solid))
(defparameter file-buttons-frame  (make-instance 'frame :master toolbar-frame :borderwidth 2))
(defparameter block-buttons-frame (make-instance 'frame :master toolbar-frame :borderwidth 2))
(defparameter block-params-frame  (make-instance 'frame :master toolbar-frame :borderwidth 2))
(defparameter tooltip-frame       (make-instance 'labelframe :master toolbar-frame :text "Tooltips"))
(defparameter node-editor-frame   (make-instance 'frame :borderwidth 2 :relief :solid))

;; Main split: node editor and toolbars
;; The two frames fill the window when resized
(grid toolbar-frame     0 0 :padx 10 :pady 10 :sticky "nsew")
(grid node-editor-frame 1 0 :padx 10 :pady 10 :sticky "nsew")

;; Place button frames in the toolbar frame
(grid file-buttons-frame  0 0 :padx 0 :pady 0)
(grid block-buttons-frame 1 0 :padx 0 :pady 0)
(grid block-params-frame  2 0 :padx 0 :pady 0)
(grid tooltip-frame       0 1 :padx 10 :pady 10 :rowspan 2 :sticky "nsew")

;; Add some padding (might remove later)
(configure file-buttons-frame :padding "5 5 5 0")
(configure block-buttons-frame :padding "5 0 5 5")
(configure tooltip-frame :padding "10 5 10 10")

;; Allow resizing of the two main frames
(grid-columnconfigure *tk* 0 :weight 1)
(grid-rowconfigure *tk* 1 :weight 1)
;; Prevent toolbar from being resized
(grid-rowconfigure *tk* 0 :weight 0)

;; Allow resizing of widgets inside toolbar frame
(grid-columnconfigure toolbar-frame 0 :weight 1)
(grid-columnconfigure toolbar-frame 1 :weight 1)
(grid-rowconfigure toolbar-frame 0 :weight 1)
(grid-rowconfigure toolbar-frame 1 :weight 1)

;; Set minimum size for tooltip text
(grid-columnconfigure toolbar-frame 1 :minsize 350)

;; Spawn file buttons
(make-buttons (list "save" "spinoff" "upload" "upload-next") file-buttons-frame)
;; Spawn DSP block buttons
(make-buttons (list "insert" "configure" "connect" "disconnect") block-buttons-frame)
;; Spawn DSP block parameter lists
(defparameter block-select (make-instance 'combobox :master block-params-frame :values '(filter limiter distortion) :state 'readonly))
(configure block-select :state 'readonly)
(slot-value block-select :state )
(grid block-select 1 0 :pady 5)
(bind block-select "<<ComboboxSelected>>"
      (lambda (evt) (format t "block is now ~a~% (evt ~a)" (text block-select) evt)))

;; Dummy tooltip
(grid (make-instance 'label :master tooltip-frame :wraplength 300 :text (some-text)) 0 0 :sticky "nw")

;; Make node editor widget
(make-node-editor node-editor-frame)
;; Make it expand to fill frame width
(grid-rowconfigure node-editor-frame 0 :weight 1)
(grid-columnconfigure node-editor-frame 0 :weight 1)

;; Stop window from getting too small
(minsize *tk* 800 500)

;; Run main event loop
(mainloop)

;; Close TK connection when window is closed
(on-focus toplevel (lambda (evt) (exit-wish)))
;; (exit-wish)
