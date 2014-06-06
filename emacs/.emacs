;;;; .emacs

;;; package management system
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

;;; uncomment this line to disable loading of "default.el" at startup
(setq inhibit-startup-message t)

;; enable visual feedback on selections and parens
(tool-bar-mode -1)
(fringe-mode 0)
(transient-mark-mode t)
(show-paren-mode t)
(setq show-paren-style 'mixed)
(column-number-mode t)
(global-subword-mode t)
(eldoc-mode t)
(hl-line-mode t)
(setq fill-column 100)

;; 'yes' is too long to type
(fset 'yes-or-no-p 'y-or-n-p)

;; default to better frame titles
(setq frame-title-format
      (concat  "%b - emacs@" (system-name)))

;;; enable ido mode by default
(require 'ido-hacks)
(ido-mode t)
(flx-ido-mode)
(ido-everywhere t)
(ido-vertical-mode t)

(setq ido-enable-flex-matching t
      ido-use-filename-at-point 'guess
      ido-vertical-define-keys 'C-n-C-p-up-down-left-right)

;;; smex gets its global bind for execute-extended-command
(global-set-key (kbd "M-x") 'smex)

;;; python mode setup
(autoload 'python-mode "python-mode" "Python Mode." t)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))
(add-to-list 'load-path "~/.emacs.d")
;(elpy-enable)

;;; magit support
(require 'magit)
(global-set-key (kbd "\C-x g") 'magit-status)

;; full screen magit-status
(defadvice magit-status (around magit-fullscreen activate)
  (window-configuration-to-register :magit-fullscreen)
  ad-do-it
  (delete-other-windows))

(defun magit-quit-session ()
  "Restores the previous window configuration and kills the magit buffer"
  (interactive)
  (kill-buffer)
  (jump-to-register :magit-fullscreen))

(define-key magit-status-mode-map (kbd "q") 'magit-quit-session)

;;; utility functions and key bindings
(defun eval-and-replace (value)
  "Evaluate the sexp at point and replace it with its return value."
  (interactive (list (eval-last-sexp nil)))
  (kill-sexp -1)
  (insert (format "%S" value)))

(global-set-key (kbd "\C-c e") 'eval-and-replace)

;; access 'occur' from inside isearch
(define-key isearch-mode-map (kbd "C-o")
  (lambda () (interactive)
    (let ((case-fold-search isearch-case-fold-search))
      (occur (if isearch-regexp isearch-string (regexp-quote isearch-string))))))

;;; Jump to last change
(global-set-key [(control .)] 'goto-last-change)

;;; Jump between BOL and non-white char on line
(defun smart-beginning-of-line ()
  "Move point to first non-whitespace character or beginning-of-line.

Move point to the first non-whitespace character on this line.
If point was already at that position, move point to beginning of line."
  (interactive)
  (let ((oldpos (point)))
    (back-to-indentation)
    (and (= oldpos (point))
         (beginning-of-line))))

(global-set-key (kbd "C-a") 'smart-beginning-of-line)

;;; key chords + ace
(key-chord-mode 1)
(key-chord-define-global "xo" 'ace-window) 
(key-chord-define-global "jl" 'join-line)

(defun buffer-switch (arg)
  "Run ace-jump-buffer unless called with argument"
  (interactive "p")
  (if (> arg 1)
      (list-buffers)
    (ace-jump-buffer)))
(global-set-key (kbd "C-x C-b") 'buffer-switch)
(global-set-key (kbd "s-a") 'ace-jump-mode)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(custom-safe-themes (quote ("c2cfe2f1440d9ef4bfd3ef4cf15bfe35ff40e6d431264b1e24af64f145cffb11" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" "e16a771a13a202ee6e276d06098bc77f008b73bbac4d526f160faa2d76c1dd0e" "1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" "dd4db38519d2ad7eb9e2f30bc03fba61a7af49a185edfd44e020aa5345e3dca7" default)))
 '(fill-column 100)
 '(font-use-system-font t)
 '(virtualenv-root "~/work/python/")
 '(whitespace-line-column 100)
 '(whitespace-style (quote (face tabs trailing space-before-tab indentation empty space-after-tab space-mark tab-mark lines-tail))))
(setq whitespace-style (quote (spaces tabs newline space-mark tab-mark newline-mark)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(kdbp-mode-continuation-whitespace-face ((t (:background "light salmon")))))
(load-theme 'zenburn)

;;; load kdb+ mode
(require 'kdbp-mode)
(add-to-list 'auto-mode-alist '("\\.qsd$" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.[qk]$" . kdbp-mode))
(add-hook 'kdbp-mode-hook (lambda ()
                            (setq tab-width 2)))
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(setq js2-basic-offset 2)

;;; popwin 
(require 'popwin)
(popwin-mode 1)
(global-set-key (kbd "C-z") popwin:keymap)

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;;; misc settings
;; handle backups and visit places
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (concat user-emacs-directory "places"))
(setq backup-directory-alist '(("." . "~/.emacs.backups")))
(setq-default indent-tabs-mode nil)

(setq erc-nick "bartiosze")

;;; and finally start emacs server
(server-start)
