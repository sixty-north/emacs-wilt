;;; wilt-test.el --- tests for wilt.el -*- lexical-binding: t -*-
;;
;; Copyright (c) 2015 Austin Bingham
;;
;; Author: Austin Bingham <austin@sixty-north.com>
;; Version: 0.1
;; URL: https://github.com/sixty-north/emacs-wilt
;; Package-Requires: ((emacs "24.3") (wilt "0.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Description:
;;
;; These are some tests for wilt.el.
;;
;; The tests are built using the standard `ert' module, so you can run
;; them using a number of techniques. For example:
;;
;;     (require 'wilt-test)
;;     (ert-run-tests-interactively "wilt-test")
;;
;; For more details, see the project page at
;; https://github.com/sixty-north/emacs-wilt.
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

;;; Code:


(require 'ert)
(require 'f)
(require 'wilt)

(ert-deftest wilt-test-line-length-with-content ()
  "Length of line with actual content."
  (with-temp-buffer
    (insert "asdfasdf\n")
    (forward-line -1)
    (should (= (wilt--line-length) 8))))

(ert-deftest wilt-test-line-length-empty-line ()
  "Length of line with no content."
  (with-temp-buffer
    (should (= (wilt--line-length) 0))))

(ert-deftest wilt-test-count-whitespace-none ()
  "Unindented lines should have no leading whitespace."
  (with-temp-buffer
    (insert "asdf")
    (should (= (wilt--count-whitespace) 0))))

(ert-deftest wilt-test-count-whitespace-some ()
  "Indnted lines should have some leading whitespace."
  (with-temp-buffer
    (insert "    asdf")
    (should (= (wilt--count-whitespace) 4))))

(ert-deftest wilt-test-leading-whitespaces-only-empty-lines ()
  (with-temp-buffer
    (insert "\n")
    (insert "\n")
    (insert "\n")
    (should (equal (wilt--leading-whitespaces) '()))))

(ert-deftest wilt-test-leading-whitespaces-only-unindented-lines ()
  (with-temp-buffer
    (insert "asdf\n")
    (insert "qwer\n")
    (insert "zxcv\n")
    (should (equal (wilt--leading-whitespaces) '(0 0 0)))))

(ert-deftest wilt-test-leading-whitespaces-some-indented-lines ()
  (with-temp-buffer
    (insert "  asdf\n")
    (insert "qwer\n")
    (insert "    zxcv\n")
    (insert " llamas")
    (should (equal (wilt--leading-whitespaces) (reverse '(2 0 4 1))))))

(ert-deftest wilt-test-calculate-wilt-empty-buffer ()
  (with-temp-buffer
    (should (= (wilt-calculate-wilt) 0.0))))

(ert-deftest wilt-test-calculate-wilt-no-indentation ()
  (with-temp-buffer
    (insert "I\n")
    (insert "am\n")
    (insert "not\n")
    (insert "a\n")
    (insert "number!\n")
    (should (= (wilt-calculate-wilt) 0.0))))

(ert-deftest wilt-test-calculate-wilt-some-indentation ()
  (with-temp-buffer
    (insert "I\n")
    (insert " am\n")
    (insert "      a\n")
    (insert "  number!\n")
    (should (= (wilt-calculate-wilt) 2.25))))

(ert-deftest wilt-test-status-text ()
  (let ((wilt-mode-line-template "llamas %.2f")
	(wilt--current 1.2345))
    (should (equal (wilt--mode-line-status-text) "llamas 1.23")))
  (let ((wilt-mode-line-template "[%s]")
	(wilt--current 2.34567))
    (should (equal (wilt--mode-line-status-text) "[2.34567]"))))

;; TODO: Tests for hooks and update conditions?

(provide 'wilt-test)

;;; wilt-test.el ends here
