(require 'dash)
(require 's)

(defun wilt--count-leading-whitespace (line)
  "TODO."
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
