;;; esup-child.el --- lisp file for child Emacs to run. -*- lexical-binding: t -*-

;; Copyright (C) 2014, 2015, 2016, 2017, 2018, 2019, 2020 Joe Schafer

;; Author: Joe Schafer <joe@jschaf.com>
;; Maintainer: Serghei Iakovlev <egrep@protonmail.ch>
;; Version: 0.7.1
;; URL: https://github.com/jschaf/esup
;; Keywords: convenience, processes
;; Package-Requires: ((cl-lib "0.5") (emacs "25.1"))

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

;; The Emacs invoked to be timed will load this file.
;;
;; See documentation on https://github.com/jschaf/esup

;;; Code:

(require 'benchmark)
(require 'eieio)
(require 'seq)
(require 'subr-x)

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
                :initform 1
                :type number
                :documentation
     "The start position of the benchmarked expression.")
   (line-number :initarg :line-number
                :initform 1
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

(defvar esup-child-max-depth 1
  "How deep to profile (require) statements.
0, don't step into any require statements.
1, step into require statements in `esup-init-file'.
n, step into up to n levels of require statements.")

(defvar esup-child-current-depth 0
  "The current depth of require forms we've stepped into.")

(defvar esup-child-last-call-intercept-results nil
  "The results of an intercepted call, if any.
This is set when eval'ing an esup-advised `require' or `load'
call before reaching the max depth.  The profile information of
the advice is used instead of the whole benchmark of the
require.")

(defvar esup-child-parent-log-process nil
  "The network process that connects to the parent Emacs.
We send our log information back to the parent Emacs via this
network process.")

(defvar esup-child-parent-results-process nil
  "The network process that connects to the parent Emacs.
We send our results back to the parent Emacs via this network
process.")

(defvar esup-child-result-separator "\n;;ESUP-RESULT-SEPARATOR;;\n"
  "The separator between results.
The parent Emacs uses the separator to know when the child has
sent a full result.  Emacs accepts network input only when it's
not busy and in bunches of about 500 bytes.  So, we might not get
a complete result.")

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
                       (apply 'format (concat "LOG: " format-str "\n") args)))

(defun esup-child-send-result-separator ()
  "Send the result separator to the parent process."
  (process-send-string esup-child-parent-results-process
                       esup-child-result-separator))

(defun esup-child-send-results (results)
  "Send RESULTS to the parent process."
  (process-send-string esup-child-parent-results-process
                       (esup-child-serialize-results results)))

(defun esup-child-send-eof ()
  "Make process see end-of-file in its input."
  (process-send-eof esup-child-parent-log-process))

(defun esup-child-log-invocation-options ()
  "Log the invocation options that esup-child was started with."
  (let ((invocation-binary (concat invocation-directory invocation-name)))
    (esup-child-send-log "binary: %s" invocation-binary)))

(defun esup-child-init-streams (port)
  "Initialize the streams for logging and results on PORT."
  (setq esup-child-parent-log-process
        (esup-child-init-stream port "LOGSTREAM"))
  (setq esup-child-parent-results-process
        (esup-child-init-stream port "RESULTSSTREAM")))

(defun esup-child-run (init-file port &optional max-depth)
  "Profile INIT-FILE and send results to localhost:PORT."
  (esup-child-init-streams port)
  (setq esup-child-max-depth (or max-depth esup-child-max-depth))
  (esup-child-send-log "starting esup-child on '%s' port=%s max-depth=%s"
                       init-file port esup-child-max-depth)
  (advice-add 'require :around 'esup-child-require-advice)
  (advice-add 'load :around 'esup-child-load-advice)
  (setq enable-local-variables :safe)
  (esup-child-log-invocation-options)
  (prog1
      (esup-child-profile-file init-file)
    (advice-remove 'require 'esup-child-require-advice)
    (advice-remove 'load 'esup-child-load-advice)
    (kill-emacs)))

(defun esup-child-chomp (str)
  "Chomp leading and tailing whitespace from STR."
  (while (string-match "\\`\n+\\|^\\s-+\\|\\s-+$\\|\n+\\'"
                       str)
    (setq str (replace-match "" t t str)))
  str)

(defun esup-child-s-pad-left (len padding s)
  "If S is shorter than LEN, pad it with PADDING on the left."
  (let ((extra (max 0 (- len (length s)))))
    (concat (make-string extra (string-to-char padding))
            s)))

(defun esup-child-unindent (str)
  "Remove common leading whitespace from each line of STR.
If STR contains only whitespace, return an empty string."
  (let* ((lines (split-string str "\\(\r\n\\|[\n\r]\\)"))
         (non-whitespace-lines (seq-filter (lambda (s) (< 0 (length (string-trim-left s))))
                                           lines))
         (n-to-trim (apply #'min (mapcar (lambda (s) (- (length s) (length (string-trim-left s))))
                                         (or non-whitespace-lines [""]))))
         (result (string-join (mapcar (lambda (s) (substring (esup-child-s-pad-left n-to-trim " " s) n-to-trim))
                                      lines)
                              "\n")))
    (if (= 0 (length (esup-child-chomp result))) "" result)))

(defmacro with-esup-child-increasing-depth (&rest body)
  "Run BODY and with an incremented depth level.
Decrement the depth level after complete."
  `(progn
     (setq esup-child-current-depth (1+ esup-child-current-depth))
     (setq esup-child-last-call-intercept-results '())
     (prog1
         ;; This is cleared after `esup-child-profile-string' completes.
         (setq esup-child-last-call-intercept-results
               (progn ,@body))
       (setq esup-child-current-depth
             (1- esup-child-current-depth)))))

(defun esup-child-require-advice
    (old-require-fn feature &optional filename noerror)
  "Advice to `require' to profile sexps with esup if max depth isn't exceeded."
  (esup-child-send-log
   "intercepted require call feature=%s filename=%s current-depth=%d  max-depth=%d"
   feature filename esup-child-current-depth esup-child-max-depth)
  (cond
   ;; We've exceed the depth limit, call old require.
   ((>= esup-child-current-depth esup-child-max-depth)
    (progn
      (esup-child-send-log
       "using old require because depth %s >= max-depth %d"
       esup-child-current-depth esup-child-max-depth)
      (funcall old-require-fn feature filename noerror)))

   ;; Feature already loaded.
   ((featurep feature)
    (esup-child-send-log "intercepted require call but feature already loaded")
    (funcall old-require-fn feature filename noerror))

   ;; Max depth not exceeded, so profile the file with esup.
   (t
    (with-esup-child-increasing-depth
     (esup-child-send-log "stepping into require call" feature filename noerror)
     (esup-child-profile-file
      (esup-child-require-feature-to-filename feature filename))))))

(defun esup-child-load-advice
    (old-load-fn file &optional noerror nomessage nosuffix must-suffix)
  "Advice around `load' to profile a file with esup.
Only profiles if `esup-child-max-depth' isn't reached."
  (cond
   ;; We've exceed the depth limit, call old load.
   ((>= esup-child-current-depth esup-child-max-depth)
    (progn
      (esup-child-send-log
       "intercepted load call but depth %d exceeds max-depth %d"
       esup-child-current-depth esup-child-max-depth)
      (funcall old-load-fn file noerror nomessage nosuffix must-suffix)))

   ;; Max depth not exceeded, so profile the file with esup.
   (t
    (with-esup-child-increasing-depth
     (esup-child-send-log
      "intercepted load call file=%s noerror=%s" file noerror)
     (esup-child-profile-file file)))))

(defun esup-child-profile-file (file-name)
  "Profile FILE-NAME and return the benchmarked expressions."
  (esup-child-send-log "profiling file='%s'" file-name)
  (let* ((clean-file (esup-child-chomp file-name))
         (abs-file-path
          (locate-file clean-file load-path
                       ;; Add empty string in case the user has (load
                       ;; "file.el"), otherwise we'll look for file.el.el
                       (cons "" load-suffixes))))
    (if abs-file-path
        (progn
          (esup-child-send-log "loading %s" abs-file-path)
          (esup-child-profile-buffer (find-file-noselect abs-file-path)))
      ;; The file doesn't exist, return an empty list of `esup-result'
      (esup-child-send-log "found no matching files for %s" abs-file-path)
      '())))

(defun esup-child-skip-byte-code-dynamic-docstrings ()
  "Skip dynamic docstrings generated by byte compilation."
  (while (looking-at "[\s\t\n\r]*#@\\([0-9]+\\) ")
    (goto-char (+ (match-end 0) (string-to-number (match-string 1))))))

(defun esup-child-create-location-info-string (&optional buffer)
  "Create a string of the location info for BUFFER.
BUFFER defaults to the current buffer."
  (unless buffer (setq buffer (current-buffer)))
  (let* ((line-number (line-number-at-pos (point)))
         (file-name (with-current-buffer buffer (buffer-file-name)))
         (location-information
          (format "%s:%d" file-name line-number)))
    location-information))

(defun esup-child-profile-buffer (buffer)
  "Profile BUFFER and return the benchmarked expressions."
  (condition-case-unless-debug error-message
      (with-current-buffer buffer
        (goto-char (point-min))
        (forward-comment (buffer-size))
        (esup-child-skip-byte-code-dynamic-docstrings)
        ;; The only way to reliably figure out if we're done is to compare
        ;; sexp positions.  `forward-sexp' handles all the complexities of
        ;; white-space and comments.
        (let ((buffer-read-only t)
              (last-start -1)
              (end (progn (forward-sexp 1) (point)))
              (start (progn (forward-sexp -1) (point)))
              results
              (after-init-time nil))
          (while (> start last-start)
            (setq results
                  (append results (esup-child-profile-sexp start end)))
            (setq last-start start)
            (goto-char end)
            (esup-child-skip-byte-code-dynamic-docstrings)
            (forward-sexp 1)
            (setq end (point))
            (forward-sexp -1)
            (setq start (point)))
          results))
    (error
     (esup-child-send-log "ERROR(profile-buffer) at %s %s"
                          (esup-child-create-location-info-string buffer)
                          error-message)
     (esup-child-send-eof))))

(defun esup-child-profile-sexp (start end)
  "Profile the sexp between START and END in the current buffer.
Returns a list of class `esup-result'."
  (let* ((sexp-string (esup-child-unindent (buffer-substring start end)))
         (line-number (line-number-at-pos start))
         (file-name (buffer-file-name))
         sexp
         esup--profile-results)
    (esup-child-send-log
     "profiling sexp at %s: %s"
     (esup-child-create-location-info-string)
     (buffer-substring-no-properties start (min end (+ 30 start))))

    (condition-case-unless-debug error-message
        (progn
          (setq sexp (if (string-equal sexp-string "")
                         ""
                       (car-safe (read-from-string sexp-string))))

          (cond
           ((string-equal sexp-string "") '())

           (t
            (setq esup--profile-results
                  (esup-child-profile-string sexp-string file-name line-number
                                             start end))
            (esup-child-send-results esup--profile-results)
            (esup-child-send-result-separator)
            esup--profile-results)))
      (error
       (esup-child-send-log "ERROR(profile-sexp) at %s with sexp %s: error=%s"
                            (esup-child-create-location-info-string)
                            sexp
                            error-message)
       (esup-child-send-eof)))))

(defun esup-child-profile-string
    (sexp-string &optional file-name line-number start-point end-point)
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
    (prog1
        (if esup-child-last-call-intercept-results
            ;; We intercepted the last call with advice on load or
            ;; require.  That means the we profiled the file by sexp,
            ;; so use that instead of the load or require call.
            (progn
              (esup-child-send-log
               "using intercepted results for string %s: %s"
               sexp-string esup-child-last-call-intercept-results)
              esup-child-last-call-intercept-results)

          ;; Otherwise, use the normal profile results.
          (list
           (esup-result (when (<= emacs-major-version 25) "esup-result")
                        :file file-name
                        :expression-string sexp-string
                        :start-point start-point :end-point end-point
                        :line-number line-number
                        :exec-time (nth 0 benchmark)
                        :gc-number (nth 1 benchmark)
                        :gc-time (nth 2 benchmark))))
      ;; Reset for the next invocation.
      (setq esup-child-last-call-intercept-results nil))))


(defun esup-child-require-feature-to-filename (feature &optional filename)
  "Given a require FEATURE, return the corresponding FILENAME."
  (esup-child-send-log
   "converting require to file-name feature='%s' filename='%s'"
   feature filename)

  (if (not filename)
      ;; Filename wasn't provided so use the feature.
      (pcase (type-of feature)
        ('symbol (symbol-name feature))
        ('cons (symbol-name (eval feature))))

    ;; Filename was provided so it overrides the feature.
    (pcase (type-of filename)
      ('string filename)
      ('cons (eval filename)))))

(defun esup-child-serialize-result (esup-result)
  "Serialize an ESUP-RESULT into a `read'able string.
We need this because `prin1-to-string' isn't stable between Emacs 25 and 26."
  (concat
   "(esup-result (when (<= emacs-major-version 25) \"esup-result\") "
   (format ":file %s "
           (prin1-to-string (slot-value esup-result 'file)))
   (format ":start-point %d " (slot-value esup-result 'start-point))
   (format ":line-number %d " (slot-value esup-result 'line-number))
   (format ":expression-string %s "
           (prin1-to-string (slot-value esup-result 'expression-string)))
   (format ":end-point %d " (slot-value esup-result 'end-point))
   (format ":exec-time %f " (slot-value esup-result 'exec-time))
   (format ":gc-number %d " (slot-value esup-result 'gc-number))
   (format ":gc-time %f" (slot-value esup-result 'gc-time))
   ")"))

(defun esup-child-serialize-results (esup-results)
  "Serialize a list of ESUP-RESULTS into a `read'able string."
  (format "(list\n  %s)"
          (mapconcat 'identity
                     (cl-loop for result in esup-results
                              collect (esup-child-serialize-result result))
                     "\n  ")))

(provide 'esup-child)
;;; esup-child.el ends here
