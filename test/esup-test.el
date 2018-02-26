;;; esup-tests.el --- tests for esup -*- lexical-binding: t -*-

;;; Commentary:

;; Tests for esup-child.el and esup.el functionality.

;;; Code:

(eval-when-compile
  (require 'cl))

(require 'esup-child)
(require 'noflet)
(require 'el-mock)

(defun esup/stub-init-stream (port init-message)
  (let ((buffer (get-buffer-create (format "esup-test-%s" init-message))))
    (with-current-buffer buffer
      (erase-buffer)
      (insert init-message "\n"))
    buffer))

(defvar esup/logstream-buffer "esup-test-LOGSTREAM")
(defvar esup/results-buffer "esup-test-RESULTSSTREAM")

(defun esup/stub-process-send-string (process string)
  (with-current-buffer (get-buffer process)
    (goto-char (point-max))
    (insert string)))

(defun esup/stub-process-send-eof (process)
  (with-current-buffer (get-buffer process)
    (goto-char (point-max))
    (insert "\n<EOF>")))

(defmacro esup/with-mock-buffer (str &rest body)
  "Create buffer with STR and run BODY."
  (declare (indent 1))
  `(with-temp-buffer
     ;; We don't want to send anything over the network
     (noflet (
              (esup-child-init-stream
               (port init-message)
               (esup/stub-init-stream port init-message))

              (process-send-string
               (process string)
               (esup/stub-process-send-string
                process string))

              (process-send-eof (process)
                                (esup/stub-process-send-eof process)))
             ;; Create buffer-file-name because esup-child collects it.
             (esup-child-init-streams 6666)
             (let ((buffer-file-name "*esup-ert-test*"))
               (insert ,str)
               (goto-char (point-min))
               ,@body))))

;; Use a macro so ERT expands needle and the buffer contents.
(defmacro esup/buffer-contains-p (buffer needle)
  `(s-contains-p
    ,needle
    (with-current-buffer ,buffer
      (buffer-substring (point-min) (point-max)))))

(defmacro esup/profile-sexp (sexp-str)
  "Run `esup-child-profile-sexp' on SEXP-STR and return the result."
  `(esup/with-mock-buffer
       ,sexp-str
     (esup-child-profile-sexp (point-min) (point-max))))

(defun esup/profile-single-sexp (sexp-str)
  "Run `esup-child-profile-sexp' on SEXP-STR and return the first result."
  (car (esup/profile-sexp sexp-str)))

(defun esup/points-eq-p (esup-result start end)
  "Return t if ESUP-RESULT match START and END."
  (and (eq start (oref esup-result :start-point))
       (eq end (oref esup-result :end-point))))

(ert-deftest esup/start-end ()
  "Test that `esup-child-profile-sexp' has the right start and end points."
  (loop for (input start end) in
        '(("()" 1 3)
          ("'(1)" 1 5))
        do
        (should (esup/points-eq-p (esup/profile-single-sexp input)
                                  start end))))

(ert-deftest esup/logs-profiling-info ()
  "Test `esup-child-profile-sexp' sends logging info."
  (let ((result
         (esup/with-mock-buffer ""
           (esup-child-profile-sexp (point-min) (point-max)))))
    (should (esup/buffer-contains-p
             esup/logstream-buffer
             "LOG: profiling sexp *esup-ert-test*:1"))))

(defun esup-test-make-locate-file-fn (mock-fs) 
  (lambda (filename path &optional suffixes predicate)
    (car-safe
     (-non-nil
      ;; Find files that exist in the mock-fs
      (-map
       (lambda (path) (car-safe (assoc path mock-fs)))
       ;; Add .el and .elc suffixes
       (-table-flat
        'concat
        ;; Build all possible file paths.
        (-table-flat 'esup--join-paths (cons "" path) (list filename))
        (cons "" load-suffixes)))))))

(defun esup--join-paths (dir file)
  (cond
   ((file-name-absolute-p file) file)
   ((string= " "dir) file)
   (t (concat (file-name-as-directory dir) file))))

(defmacro with-esup-mock (props &rest body)
  `(let* ((load-path (plist-get ,props :load-path))
          (mock-fs (plist-get ,props :files))
          (locate-fn (esup-test-make-locate-file-fn mock-fs)))
     (noflet
       ((find-file-noselect
         (filename &optional nowarn rawfile wildcards)
         (let ((mock-file-exists (assoc filename mock-fs))
               (contents (alist-get filename mock-fs)))
           (if mock-file-exists
               (with-current-buffer (get-buffer-create filename)
                 (setq-local buffer-file-name filename)
                 (insert contents)
                 (current-buffer))
             (error "Unknown file %s not in mock-fs" filename))))

        (locate-file
         (filename path &optional suffixes predicate)
         (funcall locate-fn filename path suffixes predicate))

        ;; Stub out network calls.
        (esup-child-init-streams (port))
        (kill-emacs (&optional arg))
        (process-send-string (process string))
        (process-send-eof (&optional process)))
       
       (message "load-path: %s" load-path)
       (message "mock-fs: %s" mock-fs)
       ,@body)))


(ert-deftest esup-child-run__loads-file ()
  (with-esup-mock
   '(:load-path ("/fake")
     :files (("/fake/foo.el" . "(progn 'qux)")))

   (assert-esup-results-equal-ignoring-time
    (esup-child-run "/fake/foo.el" -1)
    (list (esup-result
           :file "/fake/foo.el"
           :expression-string "(progn 'qux)"
           :start-point 1
           :end-point 13)))))


(ert-deftest esup-child-run__handles-empty-file()
  (with-esup-mock
   '(:load-path ("/fake")
     :files (("/fake/foo.el" . "")))

   (assert-esup-results-equal-ignoring-time
    (esup-child-run "/fake/foo.el" -1)
    (list))))

(ert-deftest esup-child-run__counts-gc()
  (with-esup-mock
   '(:load-path ("/fake")
     :files (("/fake/bar.el" . "(progn (garbage-collect) (garbage-collect))")))

   (assert-esup-results-equal-ignoring-time
    (esup-child-run "/fake/bar.el" -1)
    (list (make-esup-result
           "/fake/bar.el"
           "(progn (garbage-collect) (garbage-collect))"
           :gc-number 2)))))

(ert-deftest esup-child-run__uses-load-path()
  (with-esup-mock
   '(:load-path ("/fake1" "/fake2")
     :files (("/fake1/qux.el" . "(progn 'qux)")
             ("/fake1/baz.el" . "(progn 'baz)")))

   (assert-esup-results-equal-ignoring-time
    (esup-child-run "qux.el" -1)
    (list
     (make-esup-result "/fake1/qux.el" "(progn 'qux)")
     (make-esup-result "/fake1/baz.el" "(progn 'baz)")
     (make-esup-result "/fake1/baz.el" "(progn 'baz)")))))

(ert-deftest esup-child-run__steps-into-requires()
  (with-esup-mock
   '(:load-path ("/fake1" "/fake2")
     :files (("/fake1/qux.el" . "(require 'baz)")
             ("/fake1/baz.el" . "(progn 'baz)")))

   (assert-esup-results-equal-ignoring-time
    (esup-child-run "qux.el" -1)
    (list
     (make-esup-result "/fake1/baz.el" "(progn 'baz)")))))

(defun assert-esup-results-equal-ignoring-time (actuals expecteds)
  (should (eq (length actuals) (length expecteds)))
  (cl-loop for (actual . expected) in (-zip-with #'cons actuals expecteds)
           do
           (should (equal (oref actual :file)
                          (oref expected :file)))
           (should (equal (oref actual :expression-string)
                          (oref expected :expression-string)))
           (should (equal (oref actual :start-point)
                          (oref expected :start-point)))
           (should (equal (oref actual :end-point)
                          (oref expected :end-point)))
           (should (equal (oref actual :line-number)
                          (oref expected :line-number)))
           (should (equal (oref actual :gc-number)
                          (oref expected :gc-number)))))

(defun make-esup-result (file sexp &rest args)
  (apply
   #'esup-result
   :file file
   :expression-string sexp
   :end-point (1+ (length sexp))
    args))

(ert-deftest esup/require-to-load ()
  "Test that `esup-child-require-to-load' works."
  (cl-loop for (input expected) in
           '(("(require 'a)" "a")
             ("(require 'b \"bFilename\")" "bFilename"))
           do
           (should (equal
                    (esup-child-require-to-load (read input))
                    expected))))

(ert-deftest esup/profile-require-level1 ()
  "Test that `esup-child-profile-sexp' steps into require statments."
  (let ((esup-child-profile-require-level 1))
    (with-mock
     (stub esup-child-profile-file => (list 1 2))
     (cl-loop for (input expected) in
              '(("(require 'e)" 2))
              do
              (should (eq
                       (length (esup/profile-sexp input))
                       expected))))))

(ert-deftest esup/profile-leading-dynamic-docstring ()
  "Test that `esup-child-profile-buffer' handles dynamic docstrings."
  ;; If it doesn't error, we're good.
  (esup/with-mock-buffer
      "#@2 A\n(defvar var 1)"
    (esup-child-profile-buffer (current-buffer))))


;; To test:
;; (load custom-file) variable for file name
;; level is incremented

(provide 'esup-tests)
;;; esup-test.el ends here
