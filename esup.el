;;; esup.el --- the Emacs StartUp Profiler (ESUP)

;; Copyright (C) 2013 Joe Schafer

;; Author: Joe Schafer <joe@jchaf.com>
;; Maintainer:  Joe Schafer <joe@jschaf.com>
;; Created: 19 May 2013
;; URL: http://github.com/jschaf/esup
;; Version:  0.3
;; Keywords:  emacs-lisp, elisp, profile, startup

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;;; Installation:
;;
;; Place esup.el on your `load-path' by adding this to your
;; `user-init-file', usually ~/.emacs or ~/.emacs.d/init.el
;;
;; (add-to-list 'load-path "~/dir/to-esup")
;;
;; Load the code:
;;
;; (autoload 'esup "esup" "Emacs Start Up Profiler." nil)
;;
;; M-x `esup' to profile your Emacs startup and display the results.

;;; Commentary:
;;
;; The most recent code is always at http://github.com/jschaf/esup
;;
;; esup profiles your Emacs startup time by examining all top-level
;; S-expressions (sexps).  esup starts a new Emacs process from Emacs
;; to profile each SEXP.  After the profiled Emacs is complete, it
;; will exit and your Emacs will display the results.

(require 'benchmark)
(require 'eieio)
(eval-when-compile
 (require 'cl-lib))

;;; Code:


;; User variables

(defgroup esup nil
  "A major mode for the Emacs Start Up Profiler."
  :prefix "esup-"
  :version "0.4"
  :group 'languages)

;; TODO: find out how emacs actually loads the files
;; `locate-user-emacs-file' and `user-init-file' look like starting
;; points.
(defcustom esup-user-init-files '("~/.emacs"
                                  "~/.emacs.el"
                                  "~/.emacs.d/init.el")
  "Possible user init files to profile.")

(defcustom esup-run-as-batch-p nil
  "If non-nil, run the profiled Emacs as batch.
This option is off by default because batch runs faster than
regular Emacs, so the timing information is not as realistic.  If
you don't want to the benchmarked Emacs frame to appear when
running `esup', set this to t.")

(defcustom esup-results-file "~/.esup-results.el"
  "Where to save the results of profiling.")

(defcustom esup-insignificant-time 0.02
  "Only show expressions that take longer than this time.")

(defface esup-timing-information
  '((t :inherit font-lock-type-face))
  "Face for displaying timing information.
Includes execution time, gc time and number of gc pauses."
  :group 'esup
  :version "24.3")

(defface esup-line-number
  '((t :inherit font-lock-constant-face))
  "Face for displaying line numbers in the *esup* buffer."
  :group 'esup
  :version "24.3")

(defface esup-column-number
  '((t :inherit font-lock-doc-face))
  "Face for displaying column numbers in the *esup* buffer."
  :group 'esup
  :version "24.3")

(defface esup-file
  '((t :inherit font-lock-function-name-face))
  "Face for displaying the file name in the *esup* buffer."
  :group 'esup
  :version "24.3")

(defvar esup-process nil
  "The current esup process.")

(defvar esup-emacs-path (concat invocation-directory invocation-name)
  "Path to the Emacs binary used for profiling.")

(defvar esup-esup-path
  (or (and load-in-progress
	   load-file-name)
      (progn
        ;; Prevent byte-compiler from complaining
        (declare-function find-library-name "find-func")
        (require 'find-func)
        (find-library-name "esup")))
  "Full path to esup.el.")


;;; Model - functions for collecting and manipulating data.

;; We don't use :accesssor for class slots because it cause a
;; byte-compiler error even if we use the accessor.  The error text is
;; below:
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

(defun esup-profile-file (file-name)
  "Profile FILE-NAME and return the benchmarked expressions."
  (let ((clean-file (esup-chomp file-name))
        abs-file-path)
    ;; Either look up the variable or remove the quotes
    (setq clean-file
          (or (symbol-value (intern-soft clean-file))
              (replace-regexp-in-string "\"" "" clean-file)))

    (setq abs-file-path
          (locate-file clean-file load-path
                       ;; Add empty string in case the user has (load
                       ;; "file.el"), otherwise we'll look for file.el.el
                       (cons "" load-suffixes)))
    ;; TODO: A file with no sexps (either nothing or comments) will
    ;; cause an error.
    (message "esup: loading %s" abs-file-path)
    (esup-profile-buffer (find-file-noselect abs-file-path))))

(defun esup-profile-buffer (buffer)
  "Profile BUFFER and return the benchmarked expressions."
  (with-current-buffer buffer
    (goto-char (point-min))
    ;; The only way to reliably figure out if we're done is to compare
    ;; sexp positions.  `forward-sexp' handles all the complexities of
    ;; white-space and comments.
    (let ((buffer-read-only t)
          (last-start -1)
          (end (progn (forward-sexp 1) (point)))
          (start (progn (forward-sexp -1) (point)))
          results)

      (while (> start last-start)
        (setq results (append results (esup-profile-sexp start end)))
        (setq last-start start)
        (goto-char end)
        (forward-sexp 1)
        (setq end (point))
        (forward-sexp -1)
        (setq start (point)))
      results)))

(defun esup-profile-sexp (start end)
  "Profile the sexp between START and END in the current buffer.
Returns a list of class `esup-result'."
  (let* ((sexp-string (buffer-substring start end))
         (sexp (car (read-from-string
                     sexp-string)))
         (line-number (line-number-at-pos start))
         (benchmark (benchmark-run (eval sexp)))
         (file-name (buffer-file-name))
        load-file-name)
    ;; Recursively profile loaded files.
    (if (looking-at "(load ")
        (progn
          (goto-char (match-end 0))
          (setq load-file-name (buffer-substring
                                (point)
                                (progn (forward-sexp 1) (point))))
          (esup-profile-file load-file-name))
      ;; Have this function always return a list of `esup-result' to
      ;; simplify processing because a loaded file will return a list
      ;; of results.
      (list (esup-result "esup-result"
               :file file-name
               :expression-string sexp-string
               :start-point start :end-point end
               :line-number line-number
               :exec-time (nth 0 benchmark)
               :gc-number (nth 1 benchmark) :gc-time (nth 2 benchmark))))))

(defun esup-total-exec-time (results)
  "Calculate the total execution time of RESULTS."
  (loop for result in results
        sum (oref result :exec-time) into total-exec-time
        finally return total-exec-time))

(defun esup-total-gc-number (results)
  "Calculate the total number of GC pauses of RESULTS."
  (loop for result in results
        sum (oref result :gc-number) into total-gc-number
        finally return total-gc-number))

(defun esup-total-gc-time (results)
  "Calculate the total time spent in GC of RESULTS."
  (loop for result in results
        sum (oref result :gc-time) into total-gc-time
        finally return total-gc-time))

(defun esup-drop-insignificant-times (results)
  "Remove inconsequential entries and sort RESULTS."
    (cl-delete-if (lambda (a) (< a esup-insignificant-time))
                  results
                  :key #'(lambda (obj) (oref obj :exec-time)))
    (cl-sort results '> :key #'(lambda (obj) (oref obj :exec-time))))

(defun esup-update-percentages (results)
  "Add the percentage of exec-time to each item in RESULTS."
  (loop for result in results
        with total-time = (esup-total-exec-time results)
        do
        (oset result :percentage (* 100 (/ (oref result :exec-time)
                                           total-time)))))


;;; Controller - the entry points

(defun esup-visit-item ()
  "Visit current item."
  (interactive)
  (let ((file (get-text-property (point) 'full-file))
        (start-point (get-text-property (point) 'start-point)))
    (if file
        (progn
          (find-file-other-window file)
          (goto-char start-point))
      (message "Not at a file."))))

(define-derived-mode esup-mode
  special-mode "esup"
  (font-lock-mode 1))

(define-key esup-mode-map (kbd "<return>") 'esup-visit-item)
(define-key esup-mode-map "n" 'esup-next-result)
(define-key esup-mode-map "p" 'esup-previous-result)

(defun esup-next-result (&optional arg)
  "Move down the next ARG results."
  ;; This function and its counterpart `esup-previous-result' rely on
  ;; the text-property `result-break' that we added to the newline
  ;; between each result.  The text-property is inserted in the
  ;; function `esup-display-results'.
  (interactive "p")
  (setq arg (or arg 1))

  ;; Move off of the result-break text-property because otherwise the
  ;; movement will be off by one character.
  (when (get-text-property (point) 'result-break)
    (backward-char))

  (let ((next-point (point)))
    (while (> arg 0)
      (setq next-point (next-single-property-change next-point 'result-break))
      (if next-point
          (progn
            (setq arg (1- arg))
            (setq next-point (1+ next-point)))
        (setq arg 0)
        (setq next-point (point-max))))
    (goto-char next-point)))

(defun esup-previous-result (&optional arg)
  "Move up the previous ARG results."
  (interactive "p")
  ;; Add one to arg because we have to go up 2 results then down one
  ;; character to be at the start of a new result.
  (setq arg (+ 2 (or arg 1)))
  ;; Get off the result-break because the movements will be off by one
  ;; character.
  (when (get-text-property (point) 'result-break)
    (forward-char))
  (let ((prev-point (point)))
    (while (> arg 0)
      (setq prev-point (previous-single-property-change prev-point
                                                        'result-break))
      (if prev-point
          (setq arg (1- arg))
        ;; break out of the loop because we couldn't find a previous
        ;; text-property of result-break, so we're at the beginning of
        ;; the buffer.
        (setq arg 0)
        (setq prev-point (point-min))))
    (goto-char prev-point)
    (when (get-text-property (point) 'result-break)
      (forward-char))))

(defun esup-batch ()
  "Function for the profiled Emacs to run."
  (require 'cl)
  (let ((init-file (car (cl-remove-if-not #'file-exists-p
                                          esup-user-init-files)))
        results)
    (ignore-errors
      (add-to-list 'load-path (file-name-directory init-file))
      (setq results (esup-profile-file init-file))
      (find-file esup-results-file)
      (erase-buffer)
      (prin1 results (current-buffer))
      (basic-save-buffer)
      (setq desktop-save-mode nil))
    (kill-emacs)))

(defun esup-process-sentinel (process status)
  "Monitor PROCESS for change in STATUS."
  (cond ((string= status "finished\n") (esup-display-results))
        (t (insert (format "Process %s %s" process status)))))

;;;###autoload
(defun esup ()
  "Profile the startup time of Emacs in the background."
  (interactive)
  (message "Starting esup...")
  (with-current-buffer (get-buffer-create "*esup-log*")
    (erase-buffer))
  ;; TODO: have the emacs frame run in the background.
  (setq esup-process
        (start-process "*esup*" "*esup-log*"
                       esup-emacs-path
                       ;; The option -q is combined with --batch
                       ;; because this `start-process' errors if we
                       ;; pass either an empty string or nil
                       (if esup-run-as-batch-p
                           "-q --batch"
                         "-q")
                       "-l" esup-esup-path
                       "-f" "esup-batch"))
  (set-process-sentinel esup-process 'esup-process-sentinel))

(defun esup-follow-link (pos)
  "Follow the link that was clicked at point POS."
  (let ((file (get-text-property pos 'full-file))
        (start-point (get-text-property pos 'start-point)))
    (message "Opening link to %s" file)
    (find-file-other-window file)
    (goto-char start-point)))


;;; Utilities

(defsubst esup-propertize-string (str &rest properties)
  "Replace all properties of STR with PROPERTIES."
  (set-text-properties 0 (length str) properties str)
  str)

(defsubst esup-fontify-string (str face)
  "Modify STR's font-lock-face property to FACE and return STR."
  (esup-propertize-string str 'font-lock-face face))

(defun esup-chomp (str)
  "Chomp leading and tailing whitespace from STR."
  (while (string-match "\\`\n+\\|^\\s-+\\|\\s-+$\\|\n+\\'"
                       str)
    (setq str (replace-match "" t t str)))
  str)


;;; View - rendering functions

(defvar esup-display-buffer "*esup*"
  "The buffer in which to display benchmark results.")

(defun esup-buffer ()
  "Initialize and return the *esup* buffer."
  (let ((buf (get-buffer esup-display-buffer)))
    (if buf
        buf
      (setq buf (generate-new-buffer esup-display-buffer))
      (with-current-buffer buf
        (esup-mode)))
    buf))

(defun esup-display-results ()
  "Display the results of the benchmarking."
  (interactive)
  (let* ((results (esup-fontify-results
                   (esup-drop-insignificant-times
                    (esup-read-results))))
         (result-break (esup-propertize-string "\n" 'result-break t))
         ;; Needed since the buffer is in `view-mode'.
         (inhibit-read-only t))
    (with-current-buffer (esup-buffer)
      (erase-buffer)
      (esup-update-percentages results)
      (insert (esup-render-summary results) result-break)
      (loop for result in results
            do (insert (render result) result-break))
      ;; We want the user to be at the top because it's disorienting
      ;; to start at the bottom.
      (goto-char (point-min))
      (pop-to-buffer (current-buffer))))
  (message "esup finished"))

(defun esup-render-summary (results)
  "Return a summary string for RESULTS."
  (let ((total-exec-time (esup-total-exec-time results))
        (total-gc-number (esup-total-gc-number results))
        (total-gc-time (esup-total-gc-time results)))
    (concat
     "Total User Startup Time: "
     (esup-fontify-string (format "%.3fsec     " total-exec-time)
                          'esup-timing-information)
     "Total Number of GC Pauses: "
     (esup-fontify-string (format "%d     " total-gc-number)
                          'esup-timing-information)
     "Total GC Time: "
     (esup-fontify-string (format "%.3fsec" total-gc-time)
                          'esup-timing-information)
     "\n")))

(defmethod render ((obj esup-result))
  "Render fields with ESUP-RESULT and return the string."
  (with-slots (file expression-string start-point end-point line-number
                    exec-time percentage)
      obj
    (let* ((short-file (file-name-nondirectory file)))
      ;; TODO: make mouse clicking work on goto file
      (esup-propertize-string
       short-file
       'font-lock-face 'esup-file
       'mouse-face 'highlight
       'full-file file
       'follow-link 'esup-open-link
       'start-point start-point
       'keymap 'esup-open-link)

      (concat
       short-file
       (esup-fontify-string (format ":%d  " line-number)
                            'esup-line-number)
       (esup-fontify-string (format "%.3fsec" exec-time)
                            'esup-timing-information)
       "   "
       (esup-fontify-string (format "%d%%" percentage)
                            'esup-timing-information)
       "\n"
       expression-string
       "\n"))))

(defun esup-fontify-results (results)
  "Add Emacs-Lisp font-lock to each expression in RESULTS."
  (with-temp-buffer
    (emacs-lisp-mode)
    (loop for result in results
          do
          (erase-buffer)
          (insert (oref result :expression-string))
          (font-lock-fontify-buffer)
          (oset result :expression-string (buffer-string)))
    results))

(defun esup-read-results ()
  "Read results from `esup-results-file' and return the list."
  (let (results)
    (with-current-buffer (find-file-noselect esup-results-file)
      (goto-char (point-min))
      (setq results (read (current-buffer)))
      (kill-buffer (current-buffer)))
    results))


;; Enable lexical binding.  Shouldn't affect Emacsen without lexbind
;; support.
;;
;; Local Variables:
;; lexical-binding: t
;; End:

(provide 'esup)

;;; esup.el ends here
