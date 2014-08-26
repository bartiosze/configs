;;; Visuals, UI, feedback
;; I don't like the default welcome screen. Turn it off. 
(setq inhibit-startup-message t)

;;; enable visual feedback on selections and parens
;; I don't use toolbar that much. Turn it off by default.
(tool-bar-mode -1)

;; Disable frame borders also. It fits awesomewm window borders.
(fringe-mode 0)

;; I like to see curent selection.
(transient-mark-mode t)
;; Pairs of parentheses should be always on display. Matching
;; parenthesis is highlighted when point is after closing or on
;; opening parenthesis.
(show-paren-mode t)
;; When the matching paren is not visible hightlight the expression
;; enclosed. Otherwise highlight the paren (as usual)
(setq show-paren-style 'mixed)

;; Display column numer where we are.
;; (column-number-mode t)

;; It is useful to be able to navigate camelCasedWords like those
;; where hyphenated.
(global-subword-mode t)

;; When documentation is available (for typed function), display help
;; line in the message area. Enable wverywhere by default.
(eldoc-mode 1)

;; Always highlight current line so that we know where we are in a blink.
(global-hl-line-mode t)

;; In most situations 'yes' is too long to type
(fset 'yes-or-no-p 'y-or-n-p)

;; When entering commands I like to see keystrokes echoed instantly.
(setq echo-keystrokes 0.01)
;; default to better frame titles
(setq frame-title-format (concat  "%b - emacs@" (system-name)))

;; Load zenburn theme.
(load-theme 'zenburn t)

;; Thus I have basic visual setting. There are some tweeks in my
;; `.emacs' but it is ok to leave that there if it's configured via
;; external packages.

;; Let's provide the setup!

(provide 'visual)
