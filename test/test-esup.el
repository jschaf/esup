;;; test-esup.el --- ESUP: Tests for esup.el and esup-child.el -*- lexical-binding: t -*-

;; Copyright (C) 2014, 2015, 2016, 2017, 2018, 2019, 2020 Joe Schafer

;; Author: Joe Schafer <joe@jschaf.com>
;; Maintainer: Serghei Iakovlev <egrep@protonmail.ch>
;; Version: 0.7.1
;; URL: https://github.com/jschaf/esup
;; Package-Requires: ((emacs "25.1"))

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

;; Tests for esup-child.el and esup.el functionality using `buttercup'.

;;; Code:

(require 'buttercup)

;; Load undercover at early stage to improve code coverage.
(when (require 'undercover nil t)
  ;; Track coverage, but don't send to coverage serivice.  Save in parent
  ;; directory as undercover saves paths relative to the repository root.
  (undercover "*.el" "test/util.el"
              (:report-file "coverage-final.json")
              (:send-report nil)))

(load (concat (file-name-directory (or load-file-name (buffer-file-name)
                                       default-directory))
              "utils.el") nil 'nomessage 'nosuffix)

(defconst esup-test/fake-port -1)

;;;; Tests:

;; TODO(jschaf): There's a bug when using the same mock directory that
;; causes it to bleed into other tests.  For a quick fix, don't use
;; the same directory.

(describe "The esup-child-run during performing"
  (it "loads file"
    (with-esup-mock
     '(:load-path ("/fake")
       :files (("/fake/foof.el" . "(progn 'qux)")))

     (should
      (esup-results-equal-p
       '(:gc-time :exec-time)
       (esup-child-run "/fake/foof.el" esup-test/fake-port)
       (list (esup-result
              :file "/fake/foof.el"
              :expression-string "(progn 'qux)"
              :start-point 1
              :end-point 13))))))

  (it "handles empty file"
    (with-esup-mock
     '(:load-path ("/fake")
       :files (("/fake/foo-bar.el" . "")))

     (should
      (esup-results-equal-p
       '(:gc-time :exec-time)
       (esup-child-run "/fake/foo-bar.el" -1)
       (list)))))

  (it "handles whitespace-only file"
    (with-esup-mock
     '(:load-path ("/fake")
       :files (("/fake/foo-bar.el" . "  ")))

     (should
      (esup-results-equal-p
       '(:gc-time :exec-time)
       (esup-child-run "/fake/foo-bar.el" -1)
       (list)))))

  (it "counts gc"
    (with-esup-mock
     '(:load-path ("/fake")
       :files (("/fake/bar-qux.el" .
                "(progn (garbage-collect) (garbage-collect))")))

     (should
      (esup-results-equal-p
       '(:gc-time :exec-time)
       (esup-child-run "/fake/bar-qux.el" esup-test/fake-port)
       (list (make-esup-result
              "/fake/bar-qux.el"
              "(progn (garbage-collect) (garbage-collect))"
              :gc-number 2))))))

  (it "uses load-path"
    (with-esup-mock
     '(:load-path ("/fake1" "/fake2")
       :files (("/fake2/qux.el" . "(require 'baz) (progn 'qux)")
               ("/fake2/baz.el" . "(progn 'baz)")))

     (should
      (esup-results-equal-p
       '(:gc-time :exec-time)
       (esup-child-run "qux.el" esup-test/fake-port)
       (list
        (make-esup-result "/fake2/baz.el" "(progn 'baz)")
        (make-esup-result "/fake2/qux.el" "(progn 'qux)"
                          :start-point 16 :end-point 28))))))

  (it "steps into requires"
      (with-esup-mock
       '(:load-path ("/fake3")
         :files (("/fake3/qux.el" . "(require 'baz)")
                 ("/fake3/baz.el" . "(progn 'baz)")))

       (should
        (esup-results-equal-p
         '(:gc-time :exec-time)
         (esup-child-run "qux.el" esup-test/fake-port)
         (list
          (make-esup-result "/fake3/baz.el" "(progn 'baz)"))))))

  (it "handles dynamic docstring"
      (with-esup-mock
       '(:load-path ("/fake1")
         :files (("/fake1/qux.el" . "#@2 A\n(defvar var 1)")))

       (should
        (esup-results-equal-p
         '(:gc-time :exec-time)
         (esup-child-run "qux.el" esup-test/fake-port)
         (list
          (make-esup-result "/fake1/qux.el" "(defvar var 1)"
                            :start-point 7 :end-point 21 :line-number 2))))))

  (it "respects require level of 1"
      (with-esup-mock
       '(:load-path ("/fake8")
         :files (("/fake8/a.el" . "(require 'c)")
                 ("/fake8/c.el" . "(require 'd)")
                 ("/fake8/d.el" . "(progn 'd)")))

       (should
        (esup-results-equal-p
         '(:gc-time :exec-time)
         (let ((depth 1))
           (esup-child-run "a.el" esup-test/fake-port depth))
         (list
          (make-esup-result "/fake8/c.el" "(require 'd)"))))))

  (it "respects require level of 2"
      (with-esup-mock
       '(:load-path ("/fake9")
         :files (("/fake9/a.el" . "(require 'c)")
                 ("/fake9/c.el" . "(require 'd)")
                 ("/fake9/d.el" . "(progn 'd)")))

       (should
        (esup-results-equal-p
         '(:gc-time :exec-time)
         (let ((depth 2))
           (esup-child-run "a.el" esup-test/fake-port depth))
         (list
          (make-esup-result "/fake9/d.el" "(progn 'd)"))))))

  (it "handles require with sexp filename"
      (with-esup-mock
       '(:load-path ("/fake10")
         :files
         (("/fake10/bar.el" .
           "(require 'core (concat \"/specified/qux/\" \"core\"))")
          ("/specified/qux/core.el" . "(progn 'core)")))

       (should
        (esup-results-equal-p
         '(:gc-time :exec-time)
         (esup-child-run "/fake10/bar.el" esup-test/fake-port)
         (list (make-esup-result "/specified/qux/core.el" "(progn 'core)"))))))

  (it "doesn't step into already required feature"
      (with-esup-mock
       '(:load-path ("/fake12")
         :files (("/fake12/qux.el" . "(require 'baz) (require 'baz)")
                 ("/fake12/baz.el" . "(progn 'baz) (provide 'baz)")))

       (should
        (esup-results-equal-p
         '(:gc-time :exec-time)
         (esup-child-run "qux.el" esup-test/fake-port)
         (list
          (make-esup-result "/fake12/baz.el" "(progn 'baz)")
          (make-esup-result "/fake12/baz.el" "(provide 'baz)"
                            :start-point 14 :end-point 28)
          (make-esup-result "/fake12/qux.el" "(require 'baz)"
                            :start-point 16 :end-point 30))))))

  (it "advises require"
      (with-esup-mock
       '(:load-path ("/fake13")
         :files (("/fake13/qux.el" .
                  "(defun my-require (feat) (require feat))(my-require 'baz)")
                 ("/fake13/baz.el" . "(progn 'baz) (provide 'baz)")))

       (should
        (esup-results-equal-p
         '(:gc-time :exec-time)
         (esup-child-run "qux.el" esup-test/fake-port)
         (list
          (make-esup-result "/fake13/qux.el"
                            "(defun my-require (feat) (require feat))")
          (make-esup-result "/fake13/baz.el" "(progn 'baz)")
          (make-esup-result "/fake13/baz.el" "(provide 'baz)"
                            :start-point 14 :end-point 28))))))

  (it "advises load"
      (with-esup-mock
       '(:load-path ("/fake14")
         :files
         (("/fake14/qux.el" .
           "(defun my-load (file) (load file)) (my-load \"baz\")")
          ("/fake14/baz.el" . "(progn 'baz) (provide 'baz)")))

       (should
        (esup-results-equal-p
         '(:gc-time :exec-time)
         (esup-child-run "qux.el" esup-test/fake-port)
         (list
          (make-esup-result "/fake14/qux.el"
                            "(defun my-load (file) (load file))")
          (make-esup-result "/fake14/baz.el" "(progn 'baz)")
          (make-esup-result "/fake14/baz.el" "(provide 'baz)"
                            :start-point 14 :end-point 28)))))))

;;; test-esup.el ends here
