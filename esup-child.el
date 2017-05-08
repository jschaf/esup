;;; esup-child.el --- lisp file for child Emacs to run. -*- lexical-binding: t -*-

;; Copyright (C) 2014-2017 Joe Schafer

;; Author: Joe Schafer <joe@jschaf.com>
;; Version: 0.5
;; Keywords:  convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; The Emacs invoked to be timed will load this file.
;;
;; See documentation on https://github.com/jschaf/esup

;;; Code:

(require 'benchmark)
(require 'eieio)

;; We don't use :accesssor for class slots because it cause a
;; byte-compiler error even if we use the accessor.  This is fixed in
;; Emacs 25.  The error text is below:
;;
;; Unused lexical variable `scoped-class'
(defclass esup-result ()
  ((file :initarg :file
         :initform ""
         :type string
         :documentation "The file location for the result.")
   (start-point :initarg :start-point
                :initform 0
                :type number
                :documentation
                "The start position of the benchmarked expression.")
   (line-number :initarg :line-number
                :initform 0
                :type number
                :documentation "The beginning line number of the expression.")
   (expression-string :initarg :expression-string
                      :initform ""
                      :type string
                      :documentation
                      "A string representation of the benchmarked expression.")
   (end-point :initarg :end-point
              :initform 0
              :type number
              :documentation "The end position of the benchmarked expression.")
   (exec-time :initarg :exec-time
              :initform 0
              :type number
              :documentation)
   (gc-number :initarg :gc-number
              :initform 0
              :type number
              :documentation "The number of garbage collections that ran.")
   (gc-time :initarg :gc-time
            :initform 0
            :type number
            :documentation "The time taken by garbage collection.")
   (percentage :initarg :percentage
               :initform 0
               :type number
               :documentation "The percentage of time taken by expression."))
  "A record of benchmarked results.")

(defvar esup-child-profile-require-level 1
  "How deep to profile (require) statements.
0, don't step into any require statements.
1, step into require statements in `esup-init-file'.
n, step into up to n levels of require statements.")

(defvar esup-child-parent-log-process nil
  "The network process that connects to the parent Emacs.
We send our log information back to the parent Emacs via this
network process.")

(defvar esup-child-parent-results-process nil
  "The network process that connects to the parent Emacs.
We send our results back to the parent Emacs via this network
process.")

(defun esup-child-connect-to-parent (port)
  "Connect to the parent process at PORT."
  (let ((port-num (if (stringp port) (string-to-number port) port)))
    (open-network-stream
     "*esup-child-connection*"
     "*esup-child-connection*"
     "localhost"
     port-num
     :type 'plain)))

(defun esup-child-init-stream (port init-message)
  "Create process on PORT, send INIT-MESSAGE, and return the process."
  (let ((proc (esup-child-connect-to-parent port)))
    (set-process-query-on-exit-flag proc nil)
    (process-send-string proc init-message)
    proc))

(defun esup-child-send-log (format-str &rest args)
  "Send FORMAT-STR formatted with ARGS as a log message."
  (process-send-string esup-child-parent-log-process
                       (apply 'format (concat "LOG: " format-str) args)))

(defun esup-child-send-result (result &optional no-serialize)
  "Send RESULT to the parent process.
If NO-SERIALIZE is non-nil then don't serialize RESULT with
`prin1-to-string'."
  (process-send-string esup-child-parent-results-process
                       (if no-serialize
                           result
                         (prin1-to-string result))))

(defun esup-child-send-eof ()
  "Make process see end-of-file in its input."
  (process-send-eof esup-child-parent-log-process))

(defvar esup-child-result-separator "\n;;ESUP-RESULT-SEPARATOR;;\n"
  "The separator between results.
The parent Emacs uses the separator to know when the child has
sent a full result.  Emacs accepts network input only when it's
not busy and in bunches of about 500 bytes.  So, we might not get
a complete result.")

(defun esup-child-run (init-file port)
  "Profile INIT-FILE and send results to localhost:PORT."

  (setq esup-child-parent-log-process
        (esup-child-init-stream port "LOGSTREAM"))
  (setq esup-child-parent-results-process
        (esup-child-init-stream port "RESULTSSTREAM"))

  (setq enable-local-variables :safe)
  (prog1
      (esup-child-profile-file init-file 0)
    (kill-emacs)))

(defun esup-child-chomp (str)
  "Chomp leading and tailing whitespace from STR."
  (while (string-match "\\`\n+\\|^\\s-+\\|\\s-+$\\|\n+\\'"
                       str)
    (setq str (replace-match "" t t str)))
  str)

(defun esup-child-profile-file (file-name &optional level)
  "Profile FILE-NAME and return the benchmarked expressions.
LEVEL is the number of `load's or `require's we've stepped into."
  (unless level (setq level 0))
  (let* ((clean-file (esup-child-chomp file-name))
         (abs-file-path
          (locate-file clean-file load-path
                       ;; Add empty string in case the user has (load
                       ;; "file.el"), otherwise we'll look for file.el.el
                       (cons "" load-suffixes))))
    (if abs-file-path
        (progn
          ;; TODO: A file with no sexps (either nothing or comments) will
          ;; cause an error.
          (message "esup: loading %s" abs-file-path)
          (esup-child-send-log (format "loading %s\n" abs-file-path))
          (esup-child-profile-buffer (find-file-noselect abs-file-path) level))
      ;; The file doesn't exist, return an empty list of `esup-result'
      '())))

(defun esup-child-skip-byte-code-dynamic-docstrings ()
  "Skip dynamic docstrings generated by byte compilation."
  (while (looking-at "[\s\t\n\r]*#@\\([0-9]+\\) ")
    (goto-char (+ (match-end 0) (string-to-number (match-string 1))))))

(defun esup-child-profile-buffer (buffer &optional level)
  "Profile BUFFER and return the benchmarked expressions.
LEVEL is the number of `load's or `require's we've stepped into."
  (unless level (setq level 0))
  (condition-case error-message
      (with-current-buffer buffer
        (goto-char (point-min))
        (forward-comment (buffer-size))
        (esup-child-skip-byte-code-dynamic-docstrings)
        ;; The only way to reliably figure out if we're done is to compare
        ;; sexp positions.  `forward-sexp' handles all the complexities of
        ;; white-space and comments.
        (let ((buffer-read-only t)
              (last-start -1)
              (end (progn (forward-sexp 1)
                          (point)))
              (start (progn (forward-sexp -1)
                            (point)))
              results
              (after-init-time nil))
          (while (> start last-start)
            (setq results (append results
                                  (esup-child-profile-sexp start end level)))
            (setq last-start start)
            (goto-char end)
            (esup-child-skip-byte-code-dynamic-docstrings)
            (forward-sexp 1)
            (setq end (point))
            (forward-sexp -1)
            (setq start (point)))
          results))
    (error
     (message "ERROR(profile-buffer): %s" error-message)
     (esup-child-send-log "ERROR(profile-buffer) at %s %s" buffer error-message)
     (esup-child-send-eof))))

(defun esup-child-profile-sexp (start end &optional level)
  "Profile the sexp between START and END in the current buffer.
Returns a list of class `esup-result'.
LEVEL is the number of `load's or `require's we've stepped into."
  (unless level (setq level 0))
  (let* ((sexp-string (esup-child-chomp (buffer-substring start end)))
         (line-number (line-number-at-pos start))
         (file-name (buffer-file-name))
         (location-information
          (format "%s:%s %d-%d" file-name line-number start end))
         sexp
         esup--profile-results)
    (condition-case error-message
        (progn
          (esup-child-send-log
           "profiling sexp %s %s\n"
           location-information
           (buffer-substring-no-properties start (min end (+ 30 start))))

          (setq sexp (if (string-equal sexp-string "")
                         ""
                       (car-safe (read-from-string sexp-string))))

          (cond
           ((string-equal sexp-string "")
            '())
           ;; Recursively profile loaded files.
           ((looking-at "(load ")
            (goto-char (match-end 0))
            (esup-child-profile-file (eval (nth 1 sexp)) (1+ level)))

           ((and (< level esup-child-profile-require-level)
                 (looking-at "(require "))
            ;; TODO: See if symbol already provided.  #38
            (esup-child-profile-file (esup-child-require-to-load sexp) (1+ level)))

           (t
            (setq esup--profile-results
                  (list (esup-child-profile-string
                         sexp-string file-name line-number start end)))
            (esup-child-send-result esup--profile-results)
            (esup-child-send-result esup-child-result-separator 'no-serialize)
            esup--profile-results)))
      (error
       (message "ERROR: %s" error-message)
       (esup-child-send-log "ERROR(profile-sexp) at %s: %s"
                            location-information error-message)
       (esup-child-send-eof)))))

(defun esup-child-profile-string (sexp-string
                                  &optional file-name line-number
                                  start-point end-point)
  "Profile SEXP-STRING.
Returns an `esup-reusult'.  FILE-NAME is the file that
SEXP-STRING was `eval'ed in.  LINE-NUMBER is the line number of
the string.  START-POINT and END-POINT are the points at which
SEXP-STRING appears in FILE-NAME."
  (let ((sexp (if (string-equal sexp-string "")
                  ""
                (car-safe (read-from-string sexp-string))))
        benchmark)
    (setq benchmark (benchmark-run (eval sexp)))
    (esup-result "esup-result"
                 :file file-name
                 :expression-string sexp-string
                 :start-point start-point :end-point end-point
                 :line-number line-number
                 :exec-time (nth 0 benchmark)
                 :gc-number (nth 1 benchmark)
                 :gc-time (nth 2 benchmark))))


(defun esup-child-require-to-load (sexp)
  "Given a require SEXP, return the corresponding file-name."
  (let ((library (symbol-name (eval (nth 1 sexp))))
        (filename (when (>= (length sexp) 2)
                    (nth 2 sexp))))
    (or filename library)))

(provide 'esup-child)
;;; esup-child.el ends here
