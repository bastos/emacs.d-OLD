(custom-set-variables
 '(column-number-mode t)
 '(ecb-options-version "2.32")
 '(show-paren-mode t)
 '(speedbar-frame-parameters (quote ((minibuffer) (width . 20) (border-width . 0) (menu-bar-lines . 0) (tool-bar-lines . 0) (unsplittable . t) (set-background-color "black"))))
 '(transient-mark-mode t))

;; Some imports
(setq load-path (cons "~/.emacs.d" load-path))
(setq load-path (cons "~/.emacs.d/rails" load-path))
(setq load-path (cons "~/.emacs.d/ruby-mode" load-path))
(setq load-path (cons "~/.emacs.d/color-theme" load-path))
(setq load-path (cons "~/.emacs.d/gist.el" load-path))
(setq load-path (cons "~/.emacs.d/cheat.el" load-path))
(setq load-path (cons "~/.emacs.d/icicles" load-path))

;; Erlang
(setq load-path (cons  "/usr/lib/erlang/lib/tools-2.6.1/emacs"
      load-path))
      (setq erlang-root-dir "/usr/lib/erlang")
      (setq exec-path (cons "/usr/lib/erlang/bin" exec-path))

;; Python
(autoload 'python-mode "python-mode.el" "Python mode." t)

(setq auto-mode-alist (append '(("/*.\.py$" . python-mode)) 
			      auto-mode-alist))

;; Ruby
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

(load-file "/usr/share/emacs/site-lisp/cedet/common/cedet.el")

;; Some requires
(require 'ecb-autoloads)
(require 'snippet)
(require 'find-recursive) 
(require 'rails)
(require 'psvn)  
(require 'color-theme)
(require 'gist)
(require 'cheat)
(require 'erlang-start)
(require 'twittering-mode)
(require 'icicles)
(require 'lacarte)

;; icicles
(icicle-mode 1) ; Turn on Icicle mode.

;; iswitchb
(iswitchb-mode t)
;; (global-set-key "\C-x\C-b" 'iswitchb-buffer)

;; Twitter mode. Just moved to local.el
;; (twittering-mode)
;; (twittering-icon-mode t)

;; Load local values 
(load "local")

;; Erlang config
(defvar inferior-erlang-prompt-timeout t)

;; Disable ecp tip
(setq ecb-tip-of-the-day nil)

(setq color-theme-is-global t)

(set-face-background 'flymake-errline "red4")
(set-face-background 'flymake-warnline "dark slate blue")

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
(setq default-frame-alist '((font . " -unknown-DejaVu Sans-normal-normal-normal-*-16-*-*-*-*-0-iso8859-1")))

(cua-mode)
(ecb-activate)

;; remove toolbar
(tool-bar-mode nil)

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
(set-key "<f12>"  'ecb-toggle-ecb-windows)
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

