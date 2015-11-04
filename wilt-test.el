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

(ert-deftest wilt-test-example ()
  "A dummy test."
  (should (= 1 1)))

(provide 'wilt-test)

;;; wilt-test.el ends here
