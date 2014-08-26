;;;; .emacs

;;; Initial setup
;; I keep custom elisp code in dedicated folder. I want this folder to
;; be added to load-path.

(add-to-list 'load-path "~/.emacs.d/lisp")
;; Package management system
(require 'package)
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ;; ("marmalade" . "http://marmalade-repo.org/packages/")
        ("melpa" . "http://melpa.milkbox.net/packages/")))
       ;; ("melpa" . "http://melpa-stable.milkbox.net/packages/")))
(package-initialize)

;; Use `use-package' for packages setup
(require 'use-package)

;; Own code:
;; Enable visuals
(use-package visual)

;; Load elisp
(use-package utilities)

;;; Packages
;; 
(use-package diminish
  :config
  (progn
    (eval-after-load "yasnippet" '(diminish 'yas-minor-mode))
    (eval-after-load "undo-tree" '(diminish 'undo-tree-mode))
    (eval-after-load "guide-key" '(diminish 'guide-key-mode))
    (eval-after-load "eldoc" '(diminish 'eldoc-mode))
    (diminish 'subword-mode)
    (diminish 'visual-line-mode)))

;; enable ido mode by default
(use-package ido
  :config
  (progn
    (use-package ido)
    (use-package ido-hacks)
    (use-package flx-ido)
    (use-package ido-vertical-mode)

    (ido-mode 1)
    (flx-ido-mode 1)
    (ido-hacks-mode 1)
    (ido-everywhere 1)
    (ido-vertical-mode 1)

    (setq ido-enable-flex-matching t
          ido-use-filename-at-point 'guess
          ido-vertical-define-keys 'C-n-C-p-up-down-left-right)))

;;; `smex' gets its global bind for execute-extended-command.
;;; `ido-hacks' likes to setup itself for M-x is it is better to load
;;; `smex' immediatelly after ido.
(use-package smex
  :bind ("M-x" . smex))


;;; python mode setup
;; (autoload 'python-mode "python-mode" "Python Mode." t)
;; (add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
;; (add-to-list 'interpreter-mode-alist '("python" . python-mode))

;(elpy-enable)

;;; Magit support
;; It is nice to have full screen magit status and still be able to
;; get back to previous window configuration. `magit-status' is
;; adviced to store window configuration to register and rester it on
;; quit. This requires `utilities.el' to be loaded.

(use-package magit
  :bind ("C-x g" . magit-status)
  :config (bind-key "q" 'b/magit-quit-session magit-status-mode-map))

;; replace elisp code with evaluation results (requires `utiliteis.el')
(bind-key "C-c e" 'b/eval-and-replace)

;; Access 'occur' from inside isearch
(bind-key "C-o" 'isearch-occur isearch-mode-map)

;;; Jump to last change
(use-package goto-chg
  :bind ("C-." . goto-last-change))

(bind-key "C-a" 'b/smart-beginning-of-line)

;;; key chords + ace
(use-package ace-window
  :config
  (progn
    (use-package key-chord
      :config (key-chord-mode 1))
    (key-chord-define-global "so" 'ace-window)
    (key-chord-define-global "jl" 'join-line)
    (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))))

(use-package ace-jump-mode
  :config
  (progn
    (defun b/buffer-switch (arg)
      "Run ace-jump-buffer unless called with argument"
      (interactive "p")
      (if (> arg 1)
          (list-buffers)
        (ace-jump-buffer))))
  :bind (("C-x C-b" . b/buffer-switch)
         ("s-a" . ace-jump-mode)))

(use-package main-line
  :config
  (setq main-line-separator-style 'zigzag))

;;; load kdb+ mode
(require 'kdbp-mode)
(add-to-list 'auto-mode-alist '("\\.qsd$" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.[qk]$" . kdbp-mode))
(add-hook 'kdbp-mode-hook (lambda ()
                            (setq tab-width 2)))
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(setq js2-basic-offset 2)
(setq sentence-end-double-space nil)

;;; popwin 
(use-package popwin
  :config
  (progn (popwin-mode 1)
         (bind-key "C-z" popwin:keymap)))

(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'forward))

;; Sometimes it is hard to remember all those key combinations.
;; Especially for rectangles, other windows and mode commands
(use-package guide-key
  :init
  (setq guide-key/guide-key-sequence '("C-x r" "C-x 4" "C-c"))
  (setq guide-key/idle-delay 0.5)
  (setq guide-key/align-command-by-space-flag t)
  (guide-key-mode 1))

;;; misc settings
;; handle backups and visit places
(use-package saveplace
  :config
  (progn
    (setq-default save-place t)
    (setq save-place-file (concat user-emacs-directory "places"))
    (setq backup-directory-alist '(("." . "~/.emacs.backups")))))

(setq-default indent-tabs-mode nil)
;; (setq fill-column 100)

(setq erc-nick "bartiosze")

;;; xcape keycode remap Return->Control (when pressed)
(global-set-key (kbd "<key-4660>") 'ignore)

;; whitespace style
(setq whitespace-style (quote (spaces tabs newline space-mark tab-mark newline-mark)))

;;; and finally start emacs server
(server-start)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("3a727bdc09a7a141e58925258b6e873c65ccf393b2240c51553098ca93957723" "756597b162f1be60a12dbd52bab71d40d6a2845a3e3c2584c6573ee9c332a66e" "6a37be365d1d95fad2f4d185e51928c789ef7a4ccf17e7ca13ad63a8bf5b922f" "146d24de1bb61ddfa64062c29b5ff57065552a7c4019bee5d869e938782dfc2a" default)))
 '(org-agenda-files (quote ("~/org/bka.org")))
 '(paradox-github-token t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
