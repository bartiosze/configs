;; full screen magit-status
(defadvice magit-status (around magit-fullscreen activate)
  (window-configuration-to-register :magit-fullscreen)
  ad-do-it
  (delete-other-windows))

(defun b/magit-quit-session ()
  "Restores the previous window configuration and kills the magit buffer"
  (interactive)
  (kill-buffer)
  (jump-to-register :magit-fullscreen))

;;; utility functions and key bindings
(defun b/eval-and-replace (value)
  "Evaluate the sexp at point and replace it with its return value."
  (interactive (list (eval-last-sexp nil)))
  (kill-sexp -1)
  (insert (format "%S" value)))

(defun b/isearch-occur ()
  (let ((case-fold-search isearch-case-fold-search))
    (occur (if isearch-regexp isearch-string (regexp-quote isearch-string)))))

;;; Jump between BOL and non-white char on line
(defun b/smart-beginning-of-line ()
  "Move point to first non-whitespace character or beginning-of-line.

Move point to the first non-whitespace character on this line.
If point was already at that position, move point to beginning of line."
  (interactive)
  (let ((oldpos (point)))
    (back-to-indentation)
    (and (= oldpos (point))
         (beginning-of-line))))

(provide 'utilities)
