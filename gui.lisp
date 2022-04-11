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
           0 i)))

(with-ltk ()
  (wm-title *tk* "FV-1 studio")
  (ltk::use-theme "clam")

  (let* ((toolbar-frame      (make-instance 'frame :borderwidth 2 :relief :solid))
        (file-buttons-frame  (make-instance 'frame :master toolbar-frame :borderwidth 2 :relief :groove))
        (block-buttons-frame (make-instance 'frame :master toolbar-frame :borderwidth 2 :relief :groove))
        (tooltip-frame       (make-instance 'labelframe :master toolbar-frame :text "Details"))
        (node-editor-frame   (make-instance 'frame :borderwidth 2 :relief :solid)))

    ;; Main split: node editor and toolbars
    ;; The two frames fill the window when resized
    (grid toolbar-frame     0 0 :padx 10 :pady 10 :sticky "nsew")
    (grid node-editor-frame 1 0 :padx 10 :pady 10 :sticky "nsew")

    ;; Place button frames in the toolbar frame
    (grid file-buttons-frame  0 0 :padx 10 :pady 5)
    (grid block-buttons-frame 1 0 :padx 10 :pady 5)
    (grid tooltip-frame       0 1 :padx 10 :pady 10 :rowspan 2 :sticky "nsew")

    ;; Add some padding (might remove later)
    (configure file-buttons-frame :padding "5 5 5 5")
    (configure block-buttons-frame :padding "5 5 5 5")
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
    ;; Spawm DSP block buttons
    (make-buttons (list "insert" "configure" "connect" "disconnect") block-buttons-frame)
    ;; Dummy tooltip
    (grid (make-instance 'label :master tooltip-frame :wraplength 300 :text (some-text)) 0 0 :sticky "nw")
    ;; Make node editor widget
    (grid (make-instance 'canvas :master node-editor-frame) 0 0)

    ;; Stop window from getting too small
    (minsize *tk* 800 500)
    ))

(defun some-text ()
  (format nil "Did you know ? This is a helpful tooltip, blah blah blah~%Linefeed haha"))
