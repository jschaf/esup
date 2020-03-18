;;; utils.el --- Esup: Non-interactive unit-test setup -*- lexical-binding: t; -*-

;; Copyright (C) 2014, 2015, 2016, 2017, 2018, 2019, 2020 Joe Schafer

;; Author: Joe Schafer <joe@jschaf.com>
;; Maintainer: Serghei Iakovlev <egrep@protonmail.ch>
;; Version: 0.7.1
;; URL: https://github.com/jschaf/esup

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

;; Esup's non-interactive test suite setup tp use `buttercup'.

;;; Code:

(require 'buttercup)

(require 'time-date) ; TODO(serghei): W/o this all test fails (add to esup.el?)
(require 'dash)      ; `-clone', `-table-flat', `-non-nil', `-map', etc
(require 'cl-lib)    ; `cl-defmacro'
(require 'noflet)    ; `noflet'

(defvar esup-debug-enabled nil
  "Enable debug messages for the test utilities.
Also sends all esup-child log messages to stdout.")

(let* ((current-file (if load-in-progress load-file-name (buffer-file-name)))
       (source-directory (locate-dominating-file current-file "Cask"))
       ;; Don't load old byte-compiled versions
       (load-prefer-newer t))
  ;; Load the file under test
  (add-to-list 'load-path source-directory)
  (load (expand-file-name "esup") nil 'nomessage)
  (load (expand-file-name "esup-child") nil 'nomessage))

;;;; Utulity:

(defun make-esup-result (file expression-string &rest args)
  "Create `esup-result' with desired FILE and EXPRESSION-STRING.
In addition apply rest ARGS if any."
  (apply
   #'esup-result
   :file file
   :expression-string expression-string
   :end-point (1+ (length expression-string))
   args))

(defun esup-test--all-slots ()
  "Return a list of all possible slots for an `esup-result'."
  (--map (intern (concat ":" (symbol-name it)))
         (object-slots (make-instance 'esup-result))))

(defun esup-test--slots-to-compare (ignoring-slots)
  "Return a list of slots to compare for an `esup-result'.
Create a new list with only the members of IGNORING-SLOTS that are not in
`esup-test--all-slots' result."
  (-difference (esup-test--all-slots) ignoring-slots))

(defun esup-results-single-equal-p (ignoring-slots a b)
  "Compare `esup-result' objects with an IGNORING-SLOTS.
A test for equality of A and B objects is performed by using `eq' defun."
  (--all? (not (null it))
          (--map (equal (eieio-oref a it) (eieio-oref b it))
                 (esup-test--slots-to-compare ignoring-slots))))

(defun esup-results-equal-p (ignoring-slots a b)
  "Compare a list of `esup-result' objects with an IGNORING-SLOTS.
A test for equality of A and B objects is performed by using `eq' defun."
  (and
   (eq (length a) (length b))
   (--all? (not (null it))
           (--zip-with (esup-results-single-equal-p
                        ignoring-slots it other)
                       a b))))

(defun esup-debug-test (str &rest format-args)
  "Output STR with FORMAT-ARGS if debug-mode is t."
  (when esup-debug-enabled
    (apply 'message str format-args)))

(defun esup--join-paths (dir file)
  "Ensure FILE is abolute file name, otherwise use DIR as a base path."
  (cond
   ((file-name-absolute-p file) file)
   ((string= " " dir) file)
   (t (concat (file-name-as-directory dir) file))))

(defun esup-test-make-locate-file-fn (mock-fs)
  "Create locate file defun using MOCK-FS."
  (lambda (filename path &optional suffixes predicate)
    (esup-debug-test
     (concat "starting generated locate-file-fn: "
             "filename=%s path=%s suffixes=%s predicate=%s")
     filename path suffixes predicate)
    (let* ((all-files-no-suffix
            (-table-flat 'esup--join-paths (cons "" path) (list filename)))
           (all-files
            (-table-flat 'concat all-files-no-suffix (cons "" load-suffixes)))
           (matching-files-in-mock-fs
            (-non-nil
             ;; Find files that exist in the mock-fs
             (-map
              (lambda (path)
                (car-safe
                 (or (assoc path mock-fs) (assoc (concat "./" path) mock-fs))))
              all-files))))
      (esup-debug-test "searching for file match: matching-files=%s all-files=%s"
                       matching-files-in-mock-fs all-files)
      (car-safe matching-files-in-mock-fs))))

(defmacro with-esup-mock (props &rest body)
   "Evaluate BODY with local esup state variables.
Use PROPS as a property list to create mock filesystem."
  (let ((old-features (-clone features)))
    `(let* ((load-path (plist-get ,props :load-path))
            (mock-fs (plist-get ,props :files))
            (locate-fn (esup-test-make-locate-file-fn mock-fs)))
       (esup-debug-test "starting with-esup-mock: load-path=%s mock-fs=%s"
                        load-path mock-fs)
       (noflet
        ((find-file-noselect
          (filename &optional nowarn rawfile wildcards)
          (esup-debug-test
           (concat
            "starting mock find-file-no-select: "
            "filename=%s nowarn=%s rawfile=%s wildcards=%s")
           filename nowarn rawfile wildcards)

          (let ((mock-file-exists (assoc filename mock-fs))
                (contents (alist-get filename mock-fs)))
            (if mock-file-exists
                (with-current-buffer (get-buffer-create filename)
                  (setq-local buffer-file-name filename)
                  (setq-local buffer-read-only nil)
                  (insert contents)
                  (current-buffer))
              (error "Unknown file %s not in mock-fs" filename))))

         (locate-file
          (filename path &optional suffixes predicate)
          (esup-debug-test
           "starting mock locate-file: filename=%s path=%s suffixes=%s pred=%s"
           filename path suffixes predicate)

          (let ((results (funcall locate-fn filename path suffixes predicate)))
            (esup-debug-test "locate-file mock returned '%s'" results)
            results))

         (require (feature &optional filename noerror)
                  (esup-debug-test
                   "starting mock require: feature=%s filename=%s noerror=%s"
                   feature filename noerror)
                  (if filename
                      (funcall locate-fn filename load-path)
                    (funcall locate-fn (symbol-name feature) load-path)))

         ;; Stub out network calls.
         (esup-child-init-streams (port))
         (kill-emacs (&optional arg))
         (process-send-string (process string)
                              (when esup-debug-enabled (message string)))
         (process-send-eof (&optional process)))

        ,@body

        (esup-debug-test "test added features %s"
                         (-difference features ',old-features))
        ;; Reset the features list in case any tests provided features.
        (setq features ',old-features)

        ;; Reset the max depth since the tests re-use the same environment.
        (when (and (boundp 'esup-child-max-depth)
                   (not (eq esup-child-max-depth 2)))
          (esup-debug-test "Resetting esup-child-max-depth back to 2 from %d"
                           esup-child-max-depth)
          (setq esup-child-max-depth 2))))))

;;; utils.el ends here
