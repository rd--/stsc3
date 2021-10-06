; Indentation and font locking is courtesy `smalltalk-mode' mode.
; Inter-process communication is courtesy `comint'.
; Symbol at point acquisition is courtesy `thingatpt'.
; Directory search facilities are courtesy `find-lisp'.

(require 'smalltalk-mode)
(require 'comint)
(require 'thingatpt)
(require 'find-lisp)

(defcustom stsc3-buffer "*stsc3*"
  "*The name of the stsc3 Smalltalk process buffer."
  :type 'string)

(defvar stsc3-interpreter (list "gst")
  "*The name of the Smalltalk interpreter (default=\"gst\").")

(defvar stsc3-directory nil
  "*The stsc3 directory (default=nil).")

(defun stsc3-send-string (s)
  "Send string and newline to Smalltalk."
  (if (comint-check-proc stsc3-buffer)
      (comint-send-string stsc3-buffer (concat s "\n"))
    (error "no stsc3 process?")))

(defun stsc3-send-quit ()
  "Quit Smalltalk."
  (interactive)
  (stsc3-send-string "ObjectMemory quit"))

(defun stsc3-find-files (dir rgx)
  (mapc (lambda (filename)
          (find-file-other-window filename))
        (find-lisp-find-files dir rgx)))

(defun stsc3-help ()
  "Lookup up the name at point in the stsc3 help files."
  (interactive)
  (let ((rgx (concat "^" (thing-at-point 'symbol) "\\.help\\.st$")))
    (stsc3-find-files (concat stsc3-directory "help/") rgx)))

(defun stsc3-remove-trailing-newline (s)
  "Delete trailing newlines from string."
  (replace-regexp-in-string "\n\\'" "" s))

(defun stsc3-ugen-list ()
  "List of SuperCollider UGen names."
  (split-string (shell-command-to-string "hsc3-db list ugen all st")))

(defvar stsc3-ugen-history nil
  "List of recently selected UGens.")

(defun stsc3-ugen-exemplar (ugen-name)
  "Insert an exemplar UGen instance at <point>."
  (interactive (list (completing-read "UGen: " (stsc3-ugen-list) nil t nil 'stsc3-ugen-list)))
  (let ((p (format "hsc3-help ugen exemplar st %s" ugen-name)))
    (insert (stsc3-remove-trailing-newline (shell-command-to-string p)))))

(defun stsc3-filein-current-file ()
  "Filein the current buffer file name to Smalltalk."
  (interactive)
  (save-buffer)
  (stsc3-send-string (format "FileStream fileIn: '%s'" buffer-file-name)))

(defun stsc3-save-image-file ()
  "Save the current state of the Smalltalk image to file."
  (interactive)
  (stsc3-send-string "ObjectMemory snapshot."))

(defun stsc3-send-current-line ()
  "Send the current line to Smalltalk."
  (interactive)
  (stsc3-send-string
   (buffer-substring-no-properties
    (line-beginning-position)
    (line-end-position))))

(defun stsc3-region-string ()
  "Get current region as string."
  (buffer-substring-no-properties (region-beginning) (region-end)))

(defun stsc3-send-region ()
  "Send region to Smalltalk."
  (interactive)
  (stsc3-send-string (format "[%s] value" (stsc3-region-string))))

(defun stsc3-send-region-msg (msg)
  "Send region to Smalltalk, appending msg."
  (let ((str (stsc3-region-string)))
    (stsc3-send-string (format "[%s] value %s " str msg))))

(defun stsc3-play-region ()
  "Play region at scsynth."
  (interactive)
  (stsc3-send-region-msg "play"))

(defun stsc3-draw-region ()
  "Draw region."
  (interactive)
  (stsc3-send-region-msg "draw"))

(defun stsc3-print-ugens-region ()
  "Print region."
  (interactive)
  (stsc3-send-region-msg "printUGens"))

(defun stsc3-reset-scsynth ()
  "Send SC3 reset instruction to Smalltalk."
  (interactive)
  (stsc3-send-string "SC3 reset"))

(defun stsc3-start-smalltalk ()
  "Start the stsc3 Smalltalk process.

If `stsc3-interpreter' is not already a subprocess it is
started and a new window is created to display the results of
evaluating stsc3 expressions.  Input and output is via `stsc3-buffer'."
  (interactive)
  (if (comint-check-proc stsc3-buffer)
      (stsc3-see-smalltalk)
    (apply
     'make-comint
     "stsc3"
     (car stsc3-interpreter)
     nil
     (cdr stsc3-interpreter))
    (stsc3-see-smalltalk)))

(defun stsc3-interrupt-smalltalk ()
  "Interupt Smalltalk."
  (interactive)
  (interrupt-process stsc3-buffer comint-ptyp))

(defun stsc3-stop ()
  "Interrupt Smalltalk & reset scsynth."
  (interactive)
  (progn
    (stsc3-interrupt-smalltalk)
    (sleep-for 0.15)
    (stsc3-reset-scsynth)))

(defun stsc3-see-smalltalk ()
 "Show Smalltalk output."
 (interactive)
  (if (not (comint-check-proc stsc3-buffer))
      (stsc3-start-smalltalk)
   (delete-other-windows)
   (split-window-vertically)
   (with-current-buffer stsc3-buffer
     (let ((window (display-buffer (current-buffer))))
       (goto-char (point-max))
       (save-selected-window
         (set-window-point window (point-max)))))))

(defvar stsc3-mode-map nil
  "Smalltalk SuperCollider keymap.")

(defun stsc3-mode-keybindings (map)
  "Smalltalk SuperCollider keybindings."
  (define-key map (kbd "C-c <") 'stsc3-filein-current-file)
  (define-key map (kbd "C-c >") 'stsc3-see-smalltalk)
  (define-key map (kbd "C-c C-c") 'stsc3-send-current-line)
  (define-key map (kbd "C-c C-h") 'stsc3-help)
  (define-key map (kbd "C-c C-u") 'stsc3-ugen-exemplar)
  (define-key map (kbd "C-c C-a") 'stsc3-play-region)
  (define-key map (kbd "C-c C-g") 'stsc3-draw-region)
  (define-key map (kbd "C-c C-i") 'stsc3-interrupt-smalltalk)
  (define-key map (kbd "C-c C-k") 'stsc3-reset-scsynth)
  (define-key map (kbd "C-c C-q") 'stsc3-send-quit)
  (define-key map (kbd "C-c C-.") 'stsc3-stop))

(defun stsc3-mode-menu (map)
  "Smalltalk SuperCollider Menu"
  (define-key map [menu-bar stsc3] (cons "Smalltalk-SuperCollider" (make-sparse-keymap "Smalltalk-SuperCollider")))
  (define-key map [menu-bar stsc3 help] (cons "Help" (make-sparse-keymap "Help")))
  (define-key map [menu-bar stsc3 help stsc3] '("StSc3 Help" . stsc3-help))
  (define-key map [menu-bar stsc3 help ugen] '("UGen Exemplar" . stsc3-ugen-exemplar))
  (define-key map [menu-bar stsc3 expression] (cons "Expression" (make-sparse-keymap "Expression")))
  (define-key map [menu-bar stsc3 expression stop] '("Stop (interrupt and reset)" . stsc3-stop))
  (define-key map [menu-bar stsc3 expression send-current-line] '("Send current line" . stsc3-send-current-line))
  (define-key map [menu-bar stsc3 expression draw-region] '("Draw region" . stsc3-draw-region))
  (define-key map [menu-bar stsc3 expression play-region] '("Play region" . stsc3-play-region))
  (define-key map [menu-bar stsc3 scsynth] (cons "ScSynth" (make-sparse-keymap "SCSynth")))
  (define-key map [menu-bar stsc3 scsynth quit] '("Quit scsynth" . stsc3-quit-scsynth))
  (define-key map [menu-bar stsc3 scsynth reset] '("Reset scsynth" . stsc3-reset-scsynth))
  (define-key map [menu-bar stsc3 Smalltalk] (cons "Smalltalk" (make-sparse-keymap "Smalltalk")))
  (define-key map [menu-bar stsc3 Smalltalk quit-smalltalk] '("Quit Smalltalk" . stsc3-quit-smalltalk))
  (define-key map [menu-bar stsc3 Smalltalk interrupt-smalltalk] '("Interrupt Smalltalk" . stsc3-interrupt-smalltalk))
  (define-key map [menu-bar stsc3 Smalltalk save-image-file] '("Save image file" . stsc3-save-image-file))
  (define-key map [menu-bar stsc3 Smalltalk filein-current-file] '("Filein current file" . stsc3-filein-current-file))
  (define-key map [menu-bar stsc3 Smalltalk see-smalltalk] '("See Smalltalk" . stsc3-see-smalltalk)))

(if stsc3-mode-map
    ()
  (let ((map (make-sparse-keymap "Smalltalk-SuperCollider")))
    (stsc3-mode-keybindings map)
    (stsc3-mode-menu map)
    (setq stsc3-mode-map map)))

(define-derived-mode
  stsc3-mode
  smalltalk-mode
  "Smalltalk SuperCollider"
  "Major mode for interacting with an inferior stsc3 process."
  (setq stsc3-literate-p nil)
  (turn-on-font-lock))

(add-to-list 'auto-mode-alist '("\\.st$" . stsc3-mode))

(provide 'stsc3)
