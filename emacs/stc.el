;;; stc.el --- C-Smalltalk SuperCollider

;;; Commentary:
; .stc files contain Smalltalk code written in C-Smalltalk (SuperCollider) notation.

(require 'comint)
(require 'thingatpt)
(require 'find-lisp)
(require 'stsc3)
(require 'sclang-mode)

;;; Code:

(defun stc-process-region ()
  "Translate region from C-Smalltalk to Smalltalk notation, return result as string."
  (with-output-to-string
    (shell-command-on-region
     (region-beginning)
     (region-end)
     "stc-to-st"
     standard-output)))

(defun stc-eval-region ()
  "Evaluate region."
  (interactive)
  (stsc3-send-string (stc-process-region)))

(defun stc-send-region-msg (msg)
  "Send region to Smalltalk, appending MSG."
  (let ((str (stc-process-region)))
    (stsc3-send-string (format "[%s] value %s " str msg))))

(defun stc-play-region ()
  "Play region at scsynth."
  (interactive)
  (stc-send-region-msg "play"))

(defun stc-draw-region ()
  "Draw region."
  (interactive)
  (stc-send-region-msg "draw"))

(defun stc-fork-region ()
  "Fork region."
  (interactive)
  (stc-send-region-msg "fork"))

(defvar stc-mode-map nil
  "Keymap for stc mode.")

(defun stc-mode-keybindings (map)
  "Install stc keybindings into MAP.

The mnemonics are: a = audition (play), e = evaluate, g = graph (draw), s = stop.
"
  (define-key map (kbd "C-c C-a") 'stc-play-region)
  (define-key map (kbd "C-c C-e") 'stc-eval-region)
  (define-key map (kbd "C-c C-g") 'stc-draw-region)
  (define-key map (kbd "C-c C-s") 'stc-stop))

(if stc-mode-map
    ()
  (let ((map (make-sparse-keymap "stc")))
    (stsc3-mode-keybindings map)
    (stc-mode-keybindings map) ; overwrite C-cC-a and C-cC-g
    (setq stc-mode-map map)))

(define-derived-mode
  stc-mode
  sclang-mode
  "stc"
  "Major mode for working with .stc files."
  (turn-on-font-lock))

(add-to-list 'auto-mode-alist '("\\.stc$" . stc-mode))

(provide 'stc)

;;; stc.el ends here
