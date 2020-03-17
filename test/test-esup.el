;;; test-esup.el --- Tests for esup -*- lexical-binding: t -*-

;;; Commentary:

;; Tests for esup-child.el and esup.el functionality using `buttercup'.

;;; Code:

(require 'buttercup)
(load (concat (file-name-directory (or load-file-name (buffer-file-name)
                                       default-directory))
              "utils.el") nil 'nomessage 'nosuffix)

(defconst esup-test/fake-port -1)

;;;; Tests:

;; TODO(jschaf): There's a bug when using the same mock directory that
;; causes it to bleed into other tests.  For a quick fix, don't use
;; the same directory.

(describe "Child run"
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

  (it "counts gc"
    (with-esup-mock
     '(:load-path ("/fake")
       :files (("/fake/bar-qux.el" . "(progn (garbage-collect) (garbage-collect))")))

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
                          :start-point 16 :end-point 28)))))))

;; (ert-deftest esup-child-run__steps-into-requires()
;;   (with-esup-mock
;;    '(:load-path ("/fake3")
;;      :files (("/fake3/qux.el" . "(require 'baz)")
;;              ("/fake3/baz.el" . "(progn 'baz)")))

;;    (should
;;     (esup-results-equal-p
;;      '(:gc-time :exec-time)
;;      (esup-child-run "qux.el" esup-test/fake-port)
;;      (list
;;       (make-esup-result "/fake3/baz.el" "(progn 'baz)"))))))

;; (ert-deftest esup-child-run__handles-dynamic-docstring()
;;   (with-esup-mock
;;    '(:load-path ("/fake1")
;;      :files (("/fake1/qux.el" . "#@2 A\n(defvar var 1)")))

;;    (should
;;     (esup-results-equal-p
;;      '(:gc-time :exec-time)
;;      (esup-child-run "qux.el" esup-test/fake-port)
;;      (list
;;       (make-esup-result "/fake1/qux.el" "(defvar var 1)"
;;                         :start-point 7 :end-point 21 :line-number 2))))))

;; (ert-deftest esup-child-run__respects-require-level-of-1 ()
;;   (with-esup-mock
;;    '(:load-path ("/fake8")
;;      :files (("/fake8/a.el" . "(require 'c)")
;;              ("/fake8/c.el" . "(require 'd)")
;;              ("/fake8/d.el" . "(progn 'd)")))

;;    (should
;;     (esup-results-equal-p
;;      '(:gc-time :exec-time)
;;      (let ((depth 1))
;;        (esup-child-run "a.el" esup-test/fake-port depth))
;;      (list
;;       (make-esup-result "/fake8/c.el" "(require 'd)"))))))

;; (ert-deftest esup-child-run__respects-require-level-of-2 ()
;;   (with-esup-mock
;;    '(:load-path ("/fake9")
;;      :files (("/fake9/a.el" . "(require 'c)")
;;              ("/fake9/c.el" . "(require 'd)")
;;              ("/fake9/d.el" . "(progn 'd)")))

;;    (should
;;     (esup-results-equal-p
;;      '(:gc-time :exec-time)
;;      (let ((depth 2))
;;        (esup-child-run "a.el" esup-test/fake-port depth))
;;      (list
;;       (make-esup-result "/fake9/d.el" "(progn 'd)"))))))

;; (ert-deftest esup-child-run__handles_require_with_sexp_filename ()
;;   (with-esup-mock
;;    '(:load-path ("/fake10")
;;      :files
;;      (("/fake10/bar.el" . "(require 'core (concat \"/specified/qux/\" \"core\"))")
;;       ("/specified/qux/core.el" . "(progn 'core)")))

;;    (should
;;     (esup-results-equal-p
;;      '(:gc-time :exec-time)
;;      (esup-child-run "/fake10/bar.el" esup-test/fake-port)
;;      (list (make-esup-result "/specified/qux/core.el" "(progn 'core)"))))))

;; (ert-deftest esup-child-run__doesnt_step_into_already_required_feature()
;;   (with-esup-mock
;;    '(:load-path ("/fake12")
;;      :files (("/fake12/qux.el" . "(require 'baz) (require 'baz)")
;;              ("/fake12/baz.el" . "(progn 'baz) (provide 'baz)")))

;;    (should
;;     (esup-results-equal-p
;;      '(:gc-time :exec-time)
;;      (esup-child-run "qux.el" esup-test/fake-port)
;;      (list
;;       (make-esup-result "/fake12/baz.el" "(progn 'baz)")
;;       (make-esup-result "/fake12/baz.el" "(provide 'baz)"
;;                         :start-point 14 :end-point 28)
;;       (make-esup-result "/fake12/qux.el" "(require 'baz)"
;;                         :start-point 16 :end-point 30))))))

;; (ert-deftest esup-child-run__advises_require()
;;   (with-esup-mock
;;    '(:load-path ("/fake13")
;;      :files (("/fake13/qux.el" . "(defun my-require (feat) (require feat)) (my-require 'baz)")
;;              ("/fake13/baz.el" . "(progn 'baz) (provide 'baz)")))

;;    (should
;;     (esup-results-equal-p
;;      '(:gc-time :exec-time)
;;      (esup-child-run "qux.el" esup-test/fake-port)
;;      (list
;;       (make-esup-result "/fake13/qux.el" "(defun my-require (feat) (require feat))")
;;       (make-esup-result "/fake13/baz.el" "(progn 'baz)")
;;       (make-esup-result "/fake13/baz.el" "(provide 'baz)"
;;                         :start-point 14 :end-point 28))))))

;; (ert-deftest esup-child-run__advises_load()
;;   (with-esup-mock
;;    '(:load-path ("/fake14")
;;      :files
;;      (("/fake14/qux.el" . "(defun my-load (file) (load file)) (my-load \"baz\")")
;;       ("/fake14/baz.el" . "(progn 'baz) (provide 'baz)")))

;;    (should
;;     (esup-results-equal-p
;;      '(:gc-time :exec-time)
;;      (esup-child-run "qux.el" esup-test/fake-port)
;;      (list
;;       (make-esup-result "/fake14/qux.el" "(defun my-load (file) (load file))")
;;       (make-esup-result "/fake14/baz.el" "(progn 'baz)")
;;       (make-esup-result "/fake14/baz.el" "(provide 'baz)"
;;                         :start-point 14 :end-point 28))))))

;; 
;; ;; Test Utilities

;; (defun esup-test--explain-esup-results-equal-p (ignoring-slots a b)
;;   "Explain why `esup-results-equal-p' returned t or nil."
;;   (pcase a
;;     ;; Actual is nil, but expected is not.
;;     ((guard (and (null a) (not (null b))))
;;      `(actual is nil but expected ,b))

;;     ;; Different types.
;;     ((guard (not (equal (type-of a) (type-of b))))
;;      `(different-types ,a ,b))

;;     ;; A list of esup-results.
;;     ((pred listp)
;;      (esup-test--explain-list-of-esup-results ignoring-slots a b))

;;     ;; A single esup-result.
;;     ((app eieio-object-class esup-result)
;;      (esup-test--explain-single-esup-result ignoring-slots a b))

;;     ;; Unknown types.
;;     (_ `(unknown-types ,a ,b))))
;; (put 'esup-results-equal-p 'ert-explainer
;;      'esup-test--explain-esup-results-equal-p)

;; (defun esup-test--explain-list-of-esup-results (ignoring-slots a b)
;;   (cond
;;    ((not (eq (length a) (length b)))
;;     `(different-lengths ,a ,b))
;;    (t
;;     ;; Compare each index
;;     (cl-loop
;;      for (actual . expected) in (-zip-pair a b)
;;      for i = 0 then (1+ i)
;;      collect
;;      `(index ,i ,@(esup-test--explain-single-esup-result
;;                    ignoring-slots actual expected))))))

;; (defun esup-test--explain-single-esup-result (ignoring-slots a b)
;;   (if (esup-results-single-equal-p ignoring-slots a b)
;;       'MATCH
;;     ;; Compare each slot
;;     (-non-nil
;;      (cl-loop for slot in (esup-test--all-slots)
;;               collect
;;               (cond
;;                ((-contains? ignoring-slots slot)
;;                 `(,slot IGNORED))
;;                ;; Got a match
;;                ((equal (eieio-oref a slot) (eieio-oref b slot))
;;                 `(,slot MATCHED on ,(eieio-oref a slot)))
;;                ;; Explain the mismatch
;;                (t
;;                 `(,slot MISMATCH
;;                         actual was ,(eieio-oref b slot)
;;                         but expected ,(eieio-oref a slot))))))))
;; (put 'esup-results-single-equal-p 'ert-explainer
;;      'esup-test--explain-single-esup-result)

;; 
;; ;; Test Utility Tests

;; (ert-deftest esup-results-equal-p__empty-list__is-equal ()
;;   (should
;;    (esup-results-equal-p '() '() '())))

;; (ert-deftest esup-results-equal-p__single-elem__is-equal ()
;;   (should
;;    (esup-results-equal-p
;;     '()
;;     (list (make-esup-result "file" "sexp"))
;;     (list (make-esup-result "file" "sexp")))))

;; (ert-deftest esup-results-equal-p__three-elem__is-equal ()
;;   (should
;;    (esup-results-equal-p
;;     '()
;;     (list
;;      (make-esup-result "file1" "sexp1" :start-point 10)
;;      (make-esup-result "file2" "sexp2" :gc-time 20)
;;      (make-esup-result "file3" "sexp3"))
;;     (list
;;      (make-esup-result "file1" "sexp1" :start-point 10)
;;      (make-esup-result "file2" "sexp2" :gc-time 20)
;;      (make-esup-result "file3" "sexp3")))))

;; (ert-deftest esup-results-equal-p__ignoring-gc-time__is-equal ()
;;   (should
;;    (esup-results-equal-p
;;     '(:gc-time)
;;     (list (make-esup-result "file" "sexp" :gc-time 30))
;;     (list (make-esup-result "file" "sexp" :gc-time 50)))))

;; (ert-deftest esup-results-equal-p__gc-time-mismatch__is-false ()
;;   (should
;;    (not
;;     (esup-results-equal-p
;;      '()
;;      (list (make-esup-result "file" "sexp" :gc-time 30))
;;      (list (make-esup-result "file" "sexp" :gc-time 50))))))

;; (ert-deftest esup-results-equal-p__single-elem-mismatch__is-false ()
;;   (should
;;    (not
;;     (esup-results-equal-p
;;      '()
;;      (list (make-esup-result "file" "sexp1"))
;;      (list (make-esup-result "file" "sexp2"))))))

;; (ert-deftest esup-results-single-equal-p__ignoring-no-slots__is-equal ()
;;   (should
;;    (esup-results-single-equal-p
;;     '()
;;     (make-esup-result "/fake/file-1.el" "(progn 'file-1)")
;;     (make-esup-result "/fake/file-1.el" "(progn 'file-1)"))))

;; (ert-deftest esup-results-single-equal-p__sexp-mismatch__is-false ()
;;   (should
;;    (not
;;     (esup-results-single-equal-p
;;      '()
;;      (make-esup-result "/fake/file-1.el" "(progn 'file-1)")
;;      (make-esup-result "/fake/file-1.el" "(progn 'file-2)")))))

;; (ert-deftest esup-results-single-equal-p__ignoring-gc-time__is-equal ()
;;   (should
;;    (esup-results-single-equal-p
;;     '(:gc-time)
;;     (esup-result
;;      :file "file" :expression-string "sexp" :end-point 20 :gc-time 444)
;;     (esup-result
;;      :file "file" :expression-string "sexp" :end-point 20 :gc-time 555))))

;; (ert-deftest esup-results-single-equal-p__gc-time-mismatch__is-false ()
;;   (should
;;    (not
;;     (esup-results-single-equal-p
;;      '()
;;      (esup-result
;;       :file "file" :expression-string "sexp" :end-point 20 :gc-time 444)
;;      (esup-result
;;       :file "file" :expression-string "sexp" :end-point 20 :gc-time 555)))))

;; (ert-deftest esup-results-single-equal-p__sexp-mismatch__is-false ()
;;   (should
;;    (not
;;     (esup-results-single-equal-p
;;      '()
;;      (esup-result
;;       :file "file" :expression-string "sexp")
;;      (esup-result
;;       :file "file" :expression-string "sexp2")))))

;; (ert-deftest make-esup-result__no-extra-args__is-same ()
;;   (should
;;    (equal
;;     (make-esup-result "file" "sexp")
;;     (esup-result
;;      :file "file"
;;      :expression-string "sexp"
;;      :end-point 5))))

;; (ert-deftest make-esup-result__with-extra-args__is-same ()
;;   (should
;;    (equal
;;     (make-esup-result "file" "sexp" :gc-time 20 :exec-time 40)
;;     (esup-result
;;      :file "file"
;;      :expression-string "sexp"
;;      :end-point 5
;;      :gc-time 20
;;      :exec-time 40))))

;;; test-esup.el ends here
