;; Some imports
(setq load-path (cons "~/.emacs.d" load-path))

(setq load-path (cons "~/.emacs.d/color-theme" load-path))

(setq load-path (cons "~/.emacs.d/gist.el" load-path))

(setq load-path (cons "~/.emacs.d/cheat.el" load-path))

(setq load-path (cons "~/.emacs.d/icicles" load-path))

(setq load-path (cons "~/.emacs.d/textmate.el" load-path))

(setq load-path (cons "~/.emacs.d/jdee/lisp" load-path))

(setq load-path (cons "~/.emacs.d/android-mode" load-path))

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
(require 'twittering-mode)
;; (require 'icicles)
(require 'lacarte)
(require 'textmate)
;;(require 'jde)
;; Turn on textmate mode http://github.com/defunkt/textmate.el/tree/master
(textmate-mode)
(require 'android-mode)

(require 'markdown-mode)

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

(setq color-theme-is-global t)

;;(load-file "~/.emacs.d/twilight-emacs/color-theme-twilight.el")
;;(color-theme-twilight)
; pick your font
; M-x describe-font
(setq default-frame-alist '((font . "-unknown-Monaco-normal-normal-normal-*-*-*-*-*-m-0-iso10646-1")))
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
(tool-bar-mode nil)

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
