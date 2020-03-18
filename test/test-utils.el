;;; test-utils.el --- ESUP: Tests for utils.el -*- lexical-binding: t -*-

;; Copyright (C) 2014-2017 Joe Schafer

;; Author: Joe Schafer <joe@jschaf.com>
;; Maintainer: Serghei Iakovlev <egrep@protonmail.ch>
;; Version: 0.7.
;; URL: http://github.com/jschaf/esup

;; This file is NOT part of GNU Emacs.

;;;; License

;; This file is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Tests for utils.el functionality using `buttercup'.

;;; Code:

(require 'buttercup)
(load (concat (file-name-directory (or load-file-name (buffer-file-name)
                                       default-directory))
              "utils.el") nil 'nomessage 'nosuffix)

;;;; Tests:

(describe "Calling esup-results-equal-p to compare esup-result objects"
  (it "equal for empty lists"
    (expect (esup-results-equal-p '() '() '()) :to-equal t))

  (it "equal for ojects with a single element"
    (should
     (esup-results-equal-p
      '()
      (list (make-esup-result "file" "sexp"))
      (list (make-esup-result "file" "sexp")))))

  (it "equal for objects with three elements"
    (should
     (esup-results-equal-p
      '()
      (list
       (make-esup-result "file1" "sexp1" :start-point 10)
       (make-esup-result "file2" "sexp2" :gc-time 20)
       (make-esup-result "file3" "sexp3"))
      (list
       (make-esup-result "file1" "sexp1" :start-point 10)
       (make-esup-result "file2" "sexp2" :gc-time 20)
       (make-esup-result "file3" "sexp3")))))

  (it "equal when ignoring :gc-time"
    (should
     (esup-results-equal-p
      '(:gc-time)
      (list (make-esup-result "file" "sexp" :gc-time 30))
      (list (make-esup-result "file" "sexp" :gc-time 50)))))

  (it "NOT equal when :gc-time are mismatch"
    (let ((result (esup-results-equal-p
                   '()
                   (list (make-esup-result "file" "sexp" :gc-time 30))
                   (list (make-esup-result "file" "sexp" :gc-time 50)))))
      (expect result :to-be nil)))

  (it "NOT equal for objects with different elements"
    (let ((result (esup-results-equal-p
                   '()
                   (list (make-esup-result "file" "sexp1"))
                   (list (make-esup-result "file" "sexp2")))))
      (expect result :to-be nil))))

;;; test-utils.el ends here
