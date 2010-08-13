(custom-set-variables
 '(column-number-mode t)
 '(ecb-options-version "2.32")
 '(show-paren-mode t)
 '(speedbar-frame-parameters (quote ((minibuffer) (width . 20) (border-width . 0) (menu-bar-lines . 0) (tool-bar-lines . 0) (unsplittable . t) (set-background-color "black"))))
 '(transient-mark-mode t))

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

;;(add-to-list 'load-path (expand-file-name "~/emacs/site/elib"))


;; Erlang
;; (setq load-path (cons  "/usr/lib/erlang/lib/tools-2.6.1/emacs"
;;      load-path))
;;      (setq erlang-root-dir "/usr/lib/erlang")
;;      (setq exec-path (cons "/usr/lib/erlang/bin" exec-path))

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
;; GEM: sudo gem install --source http://gems.github.com jamis-fuzzy_file_finder
;; Example:  (fuzzy-find-project-root "~/path/to/project")
(add-to-list 'load-path "~/.emacs.d/fuzzy-find-in-project")
(require 'fuzzy-find-in-project)

;; Example: (set-key "<f11>" 'gdb)
(defun set-key (kbd funct)
  (global-set-key (read-kbd-macro kbd) funct))

;;(load-file "/usr/share/emacs/site-lisp/cedet/common/cedet.el")
(require 'cedet)
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
(require 'jde)
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

;; icicles
;; (icicle-mode 1) ; Turn on Icicle mode.

;; ido-mode
(ido-mode 1)
(setq ido-enable-flex-matching t)
(ido-everywhere 1)

;; iswitchb
;;(iswitchb-mode t)
;; (global-set-key "\C-x\C-b" 'iswitchb-buffer)

;; Load local values 
(load "local")

;; Erlang config
;; (defvar inferior-erlang-prompt-timeout t)

;; Disable ecb tip
;; (setq ecb-tip-of-the-day nil)

(setq color-theme-is-global t)

;; (set-face-background 'flymake-errline "red4")
;; (set-face-background 'flymake-warnline "dark slate blue")

;; (set-default-font "Bitstream Vera Sans Mono-11")
;; (set-fontset-font (frame-parameter nil 'font)
;; 'han '("cwTeXHeiBold" . "unicode-bmp"))

;; (require 'color-theme)
;; (color-theme-initialize)
;; (setq color-theme-is-global t)

(load-file "~/.emacs.d/twilight-emacs/color-theme-twilight.el")
(color-theme-twilight)
; pick your font
; M-x describe-font
(setq default-frame-alist '((font . "-unknown-Monaco-normal-normal-normal-*-*-*-*-*-m-0-iso10646-1")))
(cua-mode)

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

;; Set keys
;; (set-key "<f12>"  'ecb-toggle-ecb-windows)
(set-key "<f11>"  'menu-bar-mode)
(global-set-key [C-tab] 'other-window) ;; vimy window switching
(server-start)
;;(set-key "<f12>"  'win-resize-enlarge-vert)
;;(set-key "<f11>"  'win-resize-minimize-vert)

;; (global-set-key [C-M-down] 'win-resize-minimize-vert)
;; (global-set-key [C-M-up] 'win-resize-enlarge-vert)
;; (global-set-key [C-M-left] 'win-resize-minimize-horiz)
;; (global-set-key [C-M-right] 'win-resize-enlarge-horiz)
;; (global-set-key [C-M-up] 'win-resize-enlarge-horiz)
;; (global-set-key [C-M-down] 'win-resize-minimize-horiz)
;; (global-set-key [C-M-left] 'win-resize-enlarge-vert)
;; (global-set-key [C-M-right] 'win-resize-minimize-vert)

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
