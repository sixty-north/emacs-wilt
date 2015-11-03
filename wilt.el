;;; wilt.el -- An extensions for calculating WILT in a buffer.
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

(defun wilt--count-leading-whitespace (line)
  "Count the leading whitespace in LINE."
  (string-match "^\\([[:space:]]*\\).*$" line)
  (length (match-string 1 line)))

(defun wilt--buffer-lines ()
  "Get list of lines in buffer."
  (save-excursion
    (goto-char (point-min))
    (mapcar
     (lambda (lineno)
       (let* ((start-pos (line-beginning-position lineno))
	      (stop-pos (line-end-position lineno)))
	 (buffer-substring start-pos stop-pos)))
     (number-sequence 1 (count-lines (buffer-end 0) (buffer-end 1))))))

(defun wilt--leading-whitespaces ()
  "TODO."
  (let* ((lines (-remove 'string-empty-p (wilt--buffer-lines))))
    (mapcar 'wilt--count-leading-whitespace lines)))

(defun wilt-calculate-buffer ()
  "TODO."
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (dotimes (lineno (count-lines (buffer-end 0) (buffer-end 1)))
      (let ((lineno (+ 1 lineno)))
	(message
	 (buffer-substring
	  (line-beginning-position lineno)
	  (line-end-position lineno)))))))

;;; wilt.el ends here
