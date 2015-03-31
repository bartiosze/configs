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

;;; Own code:
;; Enable visuals
(use-package visual)

;; setup powerline
(use-package powerline
  :config
  (progn
    (powerline-default-theme)))

;; Load elisp
(use-package utilities)

;;; Packages
;;
(use-package diminish
  :config
  (progn
    (eval-after-load "yasnippet" '(diminish 'yas-minor-mode))
    (eval-after-load "guide-key" '(diminish 'guide-key-mode))
    (eval-after-load "eldoc" '(diminish 'eldoc-mode))
    (eval-after-load "paredit" '(diminish 'paredit-mode "𝝥"))
    (eval-after-load "paredit-everywhere" '(diminish 'paredit-everywhere-mode "𝞟"))
    (eval-after-load "helm-mode" '(diminish 'helm-mode "𝝜"))
    (eval-after-load "flymake" '(diminish 'flymake-mode "𝝫"))
    (eval-after-load "auto-complete" '(diminish 'auto-complete-mode "⏦"))
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
  :diminish magit-auto-revert-mode
  :config (bind-key "q" 'b/magit-quit-session magit-status-mode-map))

;; replace elisp code with evaluation results (requires `utiliteis.el')
(bind-key "C-c e" 'b/eval-and-replace)

;; Access 'occur' from inside isearch
(bind-key "C-o" 'helm-occur isearch-mode-map)

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
    (key-chord-define-global "jj" 'ace-window)
    (key-chord-define-global "jl" 'join-line)
    (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))))

(use-package ace-jump-mode
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

;;; web devel (html+js)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-hook 'js2-mode 'hs-minor-mode)
(add-to-list 'auto-mode-alist '("\\.html$" . web-mode))
             
(setq js2-basic-offset 2)
(setq sentence-end-double-space nil)

;;; popwin 
(use-package popwin
  :config
  (progn
    (popwin-mode 1)
    (bind-key "C-z" popwin:keymap)))

(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'forward))

;; fancy undo visualization. `undo-tree-visualize` is bound to `C-x u`
(use-package undo-tree
  :defer t
  :ensure t
  :diminish (undo-tree-mode . "𝗨")
  :idle
  (progn
    (global-undo-tree-mode)
    (setq undo-tree-visualizer-timestamps t)
    (setq undo-tree-visualizer-diff t)))

;; Sometimes it is hard to remember all those key combinations.
;; Especially for rectangles, other windows and mode commands
(use-package guide-key
  :init
  (setq guide-key/guide-key-sequence '("C-x r" "C-x 4" "C-c" "C-c h" "C-c p"))
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
    ("3a727bdc09a7a141e58925258b6e873c65ccf393b2240c51553098ca93957723"
     "756597b162f1be60a12dbd52bab71d40d6a2845a3e3c2584c6573ee9c332a66e"
     "6a37be365d1d95fad2f4d185e51928c789ef7a4ccf17e7ca13ad63a8bf5b922f"
     "146d24de1bb61ddfa64062c29b5ff57065552a7c4019bee5d869e938782dfc2a"
     default)))
 '(org-agenda-files (quote ("~/org/bka.org")))
 '(paradox-github-token t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(require 'helm)
(require 'helm-config)

;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))

(setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
      helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
      helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf t)

(helm-mode 1)
(global-set-key (kbd "M-x") 'helm-M-x)
(setq helm-M-x-fuzzy-match t)
(setq helm-lisp-fuzzy-completion t)

(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-f") 'helm-for-files)
(global-set-key (kbd "M-s o") 'helm-occur)
(global-set-key (kbd "C-c h g") 'helm-google-suggest)
(global-set-key (kbd "C-x r b") 'helm-bookmarks)

(put 'narrow-to-region 'disabled nil)

(setq org-capture-templates
      `(("l" "Ledger entries")
        ("lm" "Expenses" plain
         (file "~/personal/ledger")
         "%(org-read-date) %^{Payee}
    Assets:PKO
    Expenses:%^{Account}  %^{Amount} PLN
  " :immediate-finish)
        ("ln" "No Frills" plain
         (file "~/personal/ledger")
         "%(let ((org-read-date-prefer-future nil)) (org-read-date)) * No Frills
    Liabilities:MBNA  
    Assets:Wayne:Groceries  $%^{Amount}
  " :immediate-finish)    
        ("lc" "Cash" plain
         (file "~/personal/ledger")
         "%(org-read-date) * %^{Payee}
    Expenses:Cash 
    Expenses:%^{Account}  %^{Amount}
  ")))
