;; Some imports
(setq load-path (cons "~/.emacs.d" load-path))

(setq load-path (cons "~/.emacs.d/gist.el" load-path))

(setq load-path (cons "~/.emacs.d/cheat.el" load-path))

(setq load-path (cons "~/.emacs.d/textmate.el" load-path))

(setq load-path (cons "~/.emacs.d/jdee/lisp" load-path))

(setq load-path (cons "~/.emacs.d/powerline" load-path))

(setq load-path (cons "~/.emacs.d/markdown-mode" load-path))

(add-to-list 'load-path "~/.emacs.d/fuzzy-find-in-project")

(autoload 'python-mode "python-mode.el" "Python mode." t)
			    
(setq load-path (cons "~/.emacs.d/emacs-rails" load-path))

(setq load-path (cons "~/.emacs.d/ruby-mode" load-path))

(require 'rails)
(require 'snippet)
(require 'find-recursive) 
(require 'gist)
(require 'cheat)
(require 'textmate)
(require 'markdown-mode)
(require 'powerline)
(require 'fuzzy-find-in-project)
(require 'tomorrow-night-eighties-theme)

;; Configurations

(autoload 'run-ruby "inf-ruby" 
"Run an inferior Ruby process")

(autoload 'inf-ruby-keys "inf-ruby" 
"Set local key defs for inf-ruby in ruby-mode")

(add-hook 'ruby-mode-hook
	  '(lambda ()
	     (inf-ruby-keys)
	     ))

;; Example: (set-key "<f11>" 'gdb)
(defun set-key (kbd funct)
  (global-set-key (read-kbd-macro kbd) funct))

(powerline-default)

;; Theme
(load-theme 'tomorrow-night-eighties t)
(setq color-theme-is-global t)

;; ido-mode
(ido-mode 1)
(setq ido-enable-flex-matching t)
(ido-everywhere 1)

;; Turning textmate on
(textmate-mode)

; pick your font
; M-x describe-font
(setq default-frame-alist '((font . "-unknown-Monaco-normal-normal-normal-*-14-*-*-*-m-0-iso10646-1")))

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
