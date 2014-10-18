(require 'esup)
(require 'esup-child)

(defmacro esup/with-mock-buffer (str &rest body)
  "Run BODY in a fake buffer filled by STR."
  (declare (indent 1))
  `(with-temp-buffer
     (let ((buffer-file-name "*esup-ert-test*"))
       (insert ,str)
       (goto-char (point-min))
       ,@body)))

(defmacro esup/profile-single-sexp (sexp-str)
  "Run `esup-profile-sexp' on SEXP-STR and return the result."
  `(esup/with-mock-buffer ,sexp-str
     (car (esup-child-profile-sexp (point-min) (point-max)))))

(defun esup/points-eq-p (esup-result start end)
  (and (eq start (oref esup-result :start-point))
       (eq end (oref esup-result :end-point))))

(ert-deftest esup/start-end ()
  "Test that `esup-profile-sexp' has the right start and end points."
  (loop for (input start end) in
           '(("()" 1 3)
             ("'(1)" 1 5))
           do
           (should (esup/points-eq-p (esup/profile-single-sexp input)
                                     start end))))


(ert-deftest esup/empty-file ()
  "Test `esup-profile-sexp' with an empty string.
This is known to fail."
  :expected-result :failed
  (let ((result
         (esup/with-mock-buffer ""
           (esup-child-profile-sexp (point-min) (point-max)))))
    (should result)))


(ert-deftest esup/garbage-collections ()
    "Test that we count garbage collection properly."
    (should
     (eq 2
         (oref
          (esup/profile-single-sexp
           "(progn (garbage-collect) (garbage-collect))")
          :gc-number))))
