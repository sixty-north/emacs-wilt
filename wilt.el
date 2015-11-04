;;; wilt.el --- An extensions for calculating WILT in a buffer.
;;
;; Copyright (c) 2015 Austin Bingham
;;
;; Author: Austin Bingham <austin@sixty-north.com>
;; Version: 0.1
;; URL: https://github.com/sixty-north/emacs-wilt
;; Package-Requires: ((dash "X.Y.Z") (s "X.Y.Z"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Description:
;;
;; WILT (Whitespace Integrated Over Lines-of-text) is a simple, though
;; surprisingly robust, measure of code complexity.
;;
;;; License:
;;
;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.
;;
;;; Code:

(require 'dash)
(require 's)

(defvar-local wilt--current 0
  "The most recently calculated WILT value for a buffer.")

(defcustom wilt-update-conditions '(save new-line mode-enabled)
  "When the WILT metric should be recalculated for a buffer.

The variable is a list of events that may trigger parsing the
buffer for new completion:

`save'
      Set buffer-needs-parse flag after the buffer was saved.

`new-line'
      Set buffer-needs-parse flag immediately after a new
      line was inserted into the buffer.

`mode-enabled'
      Set buffer-needs-parse flag after `wilt-mode' has been
      enabled.

If nil, never automatically recalculate WILT."

  :group 'wilt
  :type '(set (const :tag "After the buffer was saved" save)
              (const :tag "After a new line was inserted" new-line)
              (const :tag "After `wilt-mode' was enabled" mode-enabled))
  :safe #'listp)

(defun wilt--conditional-update (condition)
  "Update the WILT if CONDITION is configured for update.

See `wilt-update-conditions' for more information on update
conditions.
"
  (if (memq condition wilt-update-conditions)
      (wilt--update-current)))

(defun wilt--line-length ()
  "Calculate length of current line."
  (save-excursion
    (-
     (progn
       (end-of-line)
       (current-column))
     (progn
       (beginning-of-line)
       (current-column)))))

(defun wilt--count-whitespace ()
  "Count the leading whitespace in the current line."
  (save-excursion
    (beginning-of-line)
    (let ((start-col (current-column)))
      (forward-to-indentation 0)
      (- (current-column) start-col))))

(defun wilt--leading-whitespaces ()
  "Return a list of leading whitespaces in a buffer.

Note that this list is reversed relative to the lines in the
buffer."
  (let ((counts '()))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
	(if (> (wilt--line-length) 0)
	  (setq counts (cons (wilt--count-whitespace) counts)))
  	(forward-line 1)))
    counts))

(defun wilt-calculate-wilt ()
  "Calculate WILT for the current buffer."
  (let* ((ws (wilt--leading-whitespaces))
	 (total-ws (-sum ws)))
    (/ (float total-ws) (length ws))))

(defun wilt-display-wilt ()
  "Show WILT in the message buffer."
  (interactive)
  (message "%s" (wilt-calculate-wilt)))

(defun wilt--mode-line-status-text ()
  "Get text for the mode line."
  (format " WILT: %.2f" wilt--current))

(defun wilt--update-current ()
  "Update the current WILT calculation."
  (setq wilt--current (wilt-calculate-wilt))

(defun wilt--on-save ()
  "Hook run after a save."
  (wilt--conditional-update 'save)))

(defun wilt--on-command ()
  "Hook run after a command is executed."
  (if (eq this-command 'newline)
      (wilt--conditional-update 'new-line)))

(defconst wilt-hooks-alist
  '((after-save-hook . wilt--on-save)
    (post-command-hook . wilt--on-command))
  "Hooks which wilt hooks into.")

;;;###autoload
(define-minor-mode wilt-mode
  "Minor mode for calculating WILT metrics on your code.

Just displays WILT metric in status line whenever it's configured
to do so.

When called interactively, toggle `wilt-mode'.  With prefix ARG,
enable `wilt-mode' if ARG is positive, otherwise disable it.

When called from Lisp, enable `wilt-mode' if ARG is omitted,
nil or positive.  If ARG is `toggle', toggle `wilt-mode'.
Otherwise behave as if called interactively.

\\{wilt-mode-map}"
  :init-value nil
  :lighter (:eval (wilt--mode-line-status-text))
  :group 'wilt
  :require 'wilt
  :after-hook (wilt--conditional-update 'mode-enabled)
  (cond
   (wilt-mode
    (dolist (hook wilt-hooks-alist)
      (add-hook (car hook) (cdr hook) nil 'local)))
   (t
    (dolist (hook wilt-hooks-alist)
      (remove-hook (car hook) (cdr hook) 'local)))))

(provide 'wilt)

;;; wilt.el ends here
