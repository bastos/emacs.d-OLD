;; Some imports
(setq load-path (cons "~/.emacs.d" load-path))

(setq load-path (cons "~/.emacs.d/color-theme" load-path))

(setq load-path (cons "~/.emacs.d/gist.el" load-path))

(setq load-path (cons "~/.emacs.d/cheat.el" load-path))

(setq load-path (cons "~/.emacs.d/icicles" load-path))

(setq load-path (cons "~/.emacs.d/textmate.el" load-path))

(setq load-path (cons "~/.emacs.d/jdee/lisp" load-path))

(setq load-path (cons "~/.emacs.d/powerline" load-path))
;; (setq load-path (cons "~/.emacs.d/android-mode" load-path))

(setq load-path (cons "~/.emacs.d/markdown-mode" load-path))

;; Javascript
;; emacs --batch --eval '(byte-compile-file "js2-mode.el")'
(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;; Python
(autoload 'python-mode "python-mode.el" "Python mode." t)

(setq auto-mode-alist (append '(("/*.\.py$" . python-mode)) 
			      auto-mode-alist))

;; Ruby
(setq load-path (cons "~/.emacs.d/emacs-rails" load-path))
(setq load-path (cons "~/.emacs.d/ruby-mode" load-path))

(autoload 'run-ruby "inf-ruby"
  "Run an inferior Ruby process")
(autoload 'inf-ruby-keys "inf-ruby"
  "Set local key defs for inf-ruby in ruby-mode")
(add-hook 'ruby-mode-hook
	  '(lambda ()
	     (inf-ruby-keys)
	     ))

(require 'rails)

;; (require 'ido)
;; (ido-mode t)

;; Rinari
;; (add-to-list 'load-path "~/.emacs.d/rinari")
;; (require 'rinari)

;; Fuzzy Find in Project
(add-to-list 'load-path "~/.emacs.d/fuzzy-find-in-project")
(require 'fuzzy-find-in-project)

;; Example: (set-key "<f11>" 'gdb)
(defun set-key (kbd funct)
  (global-set-key (read-kbd-macro kbd) funct))

;;(load-file "/usr/share/emacs/site-lisp/cedet/common/cedet.el")
;;(require 'cedet)
;; Some requires
;; (require 'ecb-autoloads)
(require 'snippet)
(require 'find-recursive) 
(require 'psvn)  
(require 'color-theme)
(require 'gist)
(require 'cheat)
;; (require 'erlang-start)
;; (require 'twittering-mode)
;; (require 'icicles)
(require 'lacarte)
(require 'textmate)

;; (require 'android-mode)

(require 'markdown-mode)

(require 'powerline)
(powerline-default)

;; Theme
(require 'tomorrow-theme)
(load-theme 'tomorrow t)

;; Android Mode
(defcustom android-mode-sdk-dir "~/Apps/android-sdk-linux_86"
  "Android SDK path"
  :group 'android-mode
  :type '(string))

;; JS Mode
;; (autoload 'js2-mode "js2" nil t)
;; (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;; ido-mode
(ido-mode 1)
(setq ido-enable-flex-matching t)
(ido-everywhere 1)

;; Turning textmate on
(textmate-mode)

(setq color-theme-is-global t)

;;(load-file "~/.emacs.d/twilight-emacs/color-theme-twilight.el")
;;(color-theme-twilight)
; pick your font
; M-x describe-font
(setq default-frame-alist '((font . "-unknown-Monaco-normal-normal-normal-*-14-*-*-*-m-0-iso10646-1")))
;;(cua-mode)

;; (ecb-activate)

;; Comment region
(defalias 'cr 'comment-region)
(defalias 'ucr 'uncomment-region)

;; Set up recentf so I can get a list of recent files when I start
(recentf-mode 1)

;;(recentf-open-files nil "*Recent Files*")
(setq recentf-max-saved-items 1200)

(defun yes-or-no-p (prompt)
  "replace tedious yes/no+enter with y/n keypress"
  (ding t)
  (y-or-n-p prompt))

;; go to line
(global-set-key (kbd "C-M-g") 'goto-line)

;; remove toolbar
(tool-bar-mode 0)

;; Count TODO's: http://gist.github.com/20152
(defun count-todos-in-buffer ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (message "Number of TODO's in the current buffer: %d" (count-matches "TODO"))))
    
(set-key "<C-f7>" 'count-todos-in-buffer)

;; turn off the annoying alarm bell
(setq ring-bell-function 'ignore)

;; Aliases

(defalias 'qrr 'query-replace-regexp)

;; Window resize
(defun win-resize-top-or-bot ()
  "Figure out if the current window is on top, bottom or in the
middle"
  (let* ((win-edges (window-edges))
	 (this-window-y-min (nth 1 win-edges))
	 (this-window-y-max (nth 3 win-edges))
	 (fr-height (frame-height)))
    (cond
     ((eq 0 this-window-y-min) "top")
     ((eq (- fr-height 1) this-window-y-max) "bot")
     (t "mid"))))

(defun win-resize-left-or-right ()
  "Figure out if the current window is to the left, right or in the
middle"
  (let* ((win-edges (window-edges))
	 (this-window-x-min (nth 0 win-edges))
	 (this-window-x-max (nth 2 win-edges))
	 (fr-width (frame-width)))
    (cond
     ((eq 0 this-window-x-min) "left")
     ((eq (+ fr-width 4) this-window-x-max) "right")
     (t "mid"))))

(defun win-resize-enlarge-horiz ()
  (interactive)
  (cond
   ((equal "top" (win-resize-top-or-bot)) (enlarge-window -1))
   ((equal "bot" (win-resize-top-or-bot)) (enlarge-window 1))
   ((equal "mid" (win-resize-top-or-bot)) (enlarge-window -1))
   (t (message "nil"))))

(defun win-resize-minimize-horiz ()
  (interactive)
  (cond
   ((equal "top" (win-resize-top-or-bot)) (enlarge-window 1))
   ((equal "bot" (win-resize-top-or-bot)) (enlarge-window -1))
   ((equal "mid" (win-resize-top-or-bot)) (enlarge-window 1))
   (t (message "nil"))))

(defun win-resize-enlarge-vert ()
  (interactive)
  (cond
   ((equal "left" (win-resize-left-or-right)) (enlarge-window-horizontally -1))
   ((equal "right" (win-resize-left-or-right)) (enlarge-window-horizontally 1))
   ((equal "mid" (win-resize-left-or-right)) (enlarge-window-horizontally -1))))

(defun win-resize-minimize-vert ()
  (interactive)
  (cond
   ((equal "left" (win-resize-left-or-right)) (enlarge-window-horizontally 1))
   ((equal "right" (win-resize-left-or-right)) (enlarge-window-horizontally -1))
   ((equal "mid" (win-resize-left-or-right)) (enlarge-window-horizontally 1))))

(set-key "<f11>"  'menu-bar-mode)
(global-set-key [C-tab] 'other-window) ;; vimy window switching
(server-start)

;; See http://www.emacswiki.org/cgi-bin/wiki/InteractivelyDoThings#toc5
(defun ido-execute ()
  (interactive)
  (call-interactively
   (intern
    (ido-completing-read
     "M-x "
     (let (cmd-list)
       (mapatoms (lambda (S) (when (commandp S) (setq cmd-list (cons (format "%S" S) cmd-list)))))
       cmd-list)))))

(global-set-key "\M-x" 'ido-execute)

;; Powerline
(setq powerline-color1 "#ffffff")
(setq powerline-color2 "#333333")

(set-face-attribute 'mode-line nil
                    :foreground "#ffffff"
                    :background "#666666"
                    :box nil)
(set-face-attribute 'mode-line-inactive nil
                    :box nil)

;; Move lines around
(defun move-line (n)
  "Move the current line up or down by N lines."
  (interactive "p")
  (setq col (current-column))
  (beginning-of-line) (setq start (point))
  (end-of-line) (forward-char) (setq end (point))
  (let ((line-text (delete-and-extract-region start end)))
    (forward-line n)
    (insert line-text)
    ;; restore point to original column in moved line
    (forward-line -1)
    (forward-char col)))

(defun move-line-up (n)
  "Move the current line up by N lines."
  (interactive "p")
  (move-line (if (null n) -1 (- n))))

(defun move-line-down (n)
  "Move the current line down by N lines."
  (interactive "p")
  (move-line (if (null n) 1 n)))

(defun move-region (start end n)
  "Move the current region up or down by N lines."
  (interactive "r\np")
  (let ((line-text (delete-and-extract-region start end)))
    (forward-line n)
    (let ((start (point)))
      (insert line-text)
      (setq deactivate-mark nil)
      (set-mark start))))

(defun move-region-up (start end n)
  "Move the current line up by N lines."
  (interactive "r\np")
  (move-region start end (if (null n) -1 (- n))))

(defun move-region-down (start end n)
  "Move the current line down by N lines."
  (interactive "r\np")
  (move-region start end (if (null n) 1 n)))

;;(global-set-key (kbd "M-<up>") 'move-region-up)
;;(global-set-key (kbd "M-<down>") 'move-region-down)

(global-set-key (kbd "M-<up>") 'move-line-up)
(global-set-key (kbd "M-<down>") 'move-line-down)
