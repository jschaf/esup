;;; esup.el --- the Emacs StartUp Profiler (ESUP)

;; Copyright (C) 2013 Joe Schafer

;; Author: Joe Schafer <joe@jchaf.com>
;; Maintainer:  Joe Schafer <joe@jschaf.com>
;; Created: 19 May 2013
;; Version:  0.2
;; Keywords:  emacs-lisp, elisp, profile

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
;; This is to easily profile your Emacs init file (or any other
;; script-like Emacs Lisp file, for that matter).

;; It will go over all sexp's (balanced expressions) in the file and
;; run them through `benchmark-run'.  It will then show the file with
;; overlays applied in a way that let you easily find out which sexp's
;; take the most time.  Since time is relative, it's not the absolute
;; value that counts but the percentage of the total running time.
;;
;; * All other sexp's with a percentage greater than
;;   `profile-dotemacs-low-percentage' will be preceded by a
;;   highlighted line, showing the results from `benchmark-run'.
;;   Also, the more 'reddish' the background of the sexp, the more
;;   time it needs.

;; * All other sexp's will be grayed out to indicate that their
;;   running time is miniscule.  You can still see the benchmark
;;   results in the minibuffer by hovering over the sexp with the
;;   mouse.

;; You can only benchmark full sexp's, so if you wrapped large parts
;; of your init file in some conditional clause, you'll have to remove
;; that for getting finer granularity.


(require 'benchmark)
(require 'eieio)
(eval-when-compile
 (require 'cl-lib))

;;; Code:


;; User variables

(defgroup esup nil
  "A major mode for the Emacs Start Up Profiler."
  :prefix "esup-"
  :group 'languages)

(defcustom esup-mode-hook nil
  "Hook to run when starting esup mode."
  :type 'hook
  :group 'esup)

(defcustom esup-user-init-files '("~/.emacs"
                                 "~/.emacs.el"
                                 "~/.emacs.d/init.el")
  "Possible user init files to be profiled.")

(defcustom esup-results-file "~/.esup-results.el"
  "Where to save the results of profiling")

(defcustom esup-low-percentage 3
  "Percentage which should be considered low.
All sexp's with a running time below this percentage will be
grayed out.")

(defface esup-line-number
  '((t :inherit font-lock-keyword-face))
  "Face for displaying line numbers in the *esup* buffer."
  :group 'esup
  :version "22.1")

(defface esup-column-number
  '((t :inherit font-lock-doc-face))
  "Face for displaying column numbers in the *esup* buffer."
  :group 'esup
  :version "22.1")

(defface esup-file
  '((t :inherit font-lock-function-name-face))
  "Face for displaying the file name in the *esup* buffer."
  :group 'esup
  :version "22.1")

(defvar esup-process nil
  "The current esup process.")

;;; Model - all the data

(defclass esup-result ()
  ((file :initarg :file
         :initform ""
         :type string
         :custom string
         :accessor get-file-name
         :documentation "The file location for the result.")
   (start-point :initarg :start-point
                :initform 0
                :type number
                :accessor get-start-point
                :documentation "The start position of the benchmarked expression.")
   (end-point :initarg :end-point
              :initform 0
              :type number
              :accessor get-end-point
              :documentation "The end position of the benchmarked expression.")
   (exec-time :initarg :exec-time
              :initform 0
              :type number
              :accessor get-exec-time
              :documentation)
   (gc-number :initarg :gc-number
              :initform 0
              :type number
              :accessor get-gc-number
              :documentation "The number of garbage collections that ran.")
   (gc-time :initarg :gc-time
            :initform 0
            :type number
            :accessor get-gc-time
            :documentation "The time taken by garbage collection.")
   (percentage :initarg :percentage
               :initform 0
               :type number
               :accessor get-percentage
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
                       ;; Add empty string in case the user has
                       ;; file.el, otherwise we look for
                       ;; file.el.el
                       (cons "" load-suffixes)))

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

(defun esup-result-from-benchmark (file start end benchmark)
  "Build an `esup-result' from FILE, START, END and the list
BENCHMARK."
  (esup-result "esup-result"
               :file file :start-point start :end-point end :exec-time (nth 0 benchmark)
               :gc-number (nth 1 benchmark) :gc-time (nth 2 benchmark)))

(defun esup-profile-sexp (start end)
  "Profile the sexp between START and END in the current buffer.
Returns a list of `esup-result'."
  (let* ((sexp (car (read-from-string
                     (buffer-substring-no-properties start end))))
         (file-name (buffer-file-name))
         load-file-name)
    (if (looking-at "(load ")
        (progn
          (goto-char (match-end 0))
          (setq load-file-name (buffer-substring
                                (point)
                                (progn (forward-sexp 1) (point))))
          (esup-profile-file load-file-name))
      ;; Have this function always return a list of results to
      ;; simplify processing because a loaded file will return a list
      ;; of results.
      (list (esup-result-from-benchmark file-name start end (benchmark-run (eval sexp)))))))

(defun esup-total-exec-time (results)
  "Calculate the total execution time of RESULTS."
  (loop for result in results
        sum (get-exec-time result) into total-time
        finally return total-time))

(defun esup-update-percentages (results)
  "Add the percentage of exec-time to each item in RESULTS."
  ;; (dolist (result results)
  ;;   (oset result :percentage 2)
  ;;   )
  (loop for result in results
        with total-time = (esup-total-exec-time results)
        do
        (oset result :percentage (* 100 (/ (get-exec-time result) 
                                           total-time)))))


(defvar esup-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map button-buffer-map)

    ;; (define-key map [mouse-2] 'esup-follow-mouse)
    ;; (define-key map "\C-c\C-b" 'esup-go-back)
    ;; (define-key map "\C-c\C-f" 'esup-go-forward)
    ;; (define-key map "\C-c\C-c" 'esup-follow-symbol)
    ;; ;; Documentation only, since we use minor-mode-overriding-map-alist.
    ;; (define-key map "\r" 'esup-follow)
    map)
  "Keymap for `esup-mode'.")

;;; Controller - the entry points
(defun esup-mode ()
  "Major mode for controlling the *esup* buffer.

Commands:
\\{esup-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (use-local-map esup-mode-map)
  (setq mode-name "esup")
  (setq major-mode 'esup-mode)

  (view-mode)

  (set (make-local-variable 'word-wrap) t)
  (set (make-local-variable 'view-no-disable-on-exit) t)
  ;; With Emacs 22 `view-exit-action' could delete the selected window
  ;; disregarding whether the help buffer was shown in that window at
  ;; all.  Since `view-exit-action' is called with the help buffer as
  ;; argument it seems more appropriate to have it work on the buffer
  ;; only and leave it to `view-mode-exit' to delete any associated
  ;; window(s).
  (setq view-exit-action
	(lambda (buffer)
	  ;; Use `with-current-buffer' to make sure that `bury-buffer'
	  ;; also removes BUFFER from the selected window.
	  (with-current-buffer buffer
	    (bury-buffer))))

  ;; (set (make-local-variable 'revert-buffer-function)
  ;;      'help-mode-revert-buffer)

  (run-mode-hooks 'esup-mode-hook))

(defun esup-batch ()
  "Function for the profiled Emacs to run."
  (let ((init-file (car (cl-remove-if-not #'file-exists-p
                                          esup-user-init-files)))
        results)
    (add-to-list 'load-path (file-name-directory init-file))
    (setq results (esup-profile-file init-file))
    (find-file esup-results-file)
    (erase-buffer)
    (prin1 results (current-buffer))
    (basic-save-buffer)
    (kill-emacs)))

(defun esup-process-sentinel (process status)
  "Monitor changes in the *esup* process."
  (cond ((string= status "finished\n") (esup-display-results))
        (t (insert (format "Process %s %s" process status)))))

;;;###autoload
(defun esup ()
  "Start a new emacs in the background and profile its startup
time."
  (interactive)
  (with-current-buffer (get-buffer-create "*esup-log*")
    (erase-buffer)
    (switch-to-buffer-other-window (current-buffer)))
  ;; TODO: use full path to emacs
  (setq esup-process
        (start-process "*esup*" "*esup-log*"
                       "emacs"
                       "-q"
                       "--debug-init"
                       "--batch"
                       "-l" "~/.emacs.d/el-get/esup/esup.el"
                       "-f" "esup-batch"))
  (set-process-sentinel esup-process 'esup-process-sentinel))

(defun esup-buffer ()
  "Initialize and return the *esup* buffer."
  (let ((buf (get-buffer esup-display-buffer)))
    (if buf
        buf
      (setq buf (generate-new-buffer esup-display-buffer))
      (with-current-buffer buf
        (esup-mode)))
    buf))

(defun esup-follow-link (pos)
  "Follow the link that was clicked at point POS."
  (let ((file (get-text-property pos 'full-file))
        (start-point (get-text-property pos 'start-point)))
    (message "Opening link to %s" file)
    (find-file-other-window file)
    (goto-char start-point)))


;;; View - rendering functions

(defvar esup-display-buffer "*esup*"
  "The buffer in which to display benchmark results.")

(defun esup-display-results ()
  "Display the results of the benchmarking."
  (interactive)
  (let ((results (esup-massage-results (esup-read-results)))
        (inhibit-read-only t))
    (with-current-buffer (esup-buffer)
      (erase-buffer)
      (esup-update-percentages results)
      (loop for result in results
            do (insert (render result) "\n"))
      (pop-to-buffer (current-buffer))))) 

(defmethod render ((obj esup-result))
  "Render fields with ESUP-RESULT and return the string."
  (with-slots (file start-point end-point exec-time percentage)
      obj
    (let* ((short-file (file-name-nondirectory file)))
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

       ":n  "
       (format "%.3fsec" exec-time)
       "   "
       (format "%d%%" percentage)
       "\n"))))

(defun esup-massage-results (results)
  "Remove all results that took 0 time and sort the remainder."
    ;; (cl-delete-if (lambda (a) (< a 0.01)) results :key 'get-exec-time)
    (cl-sort results '> :key 'get-exec-time))

(defun esup-read-results ()
  "Read results from `esup-results-file' and return the list."
  (let (results)
    (with-current-buffer (find-file-noselect esup-results-file)
      (goto-char (point-min))
      (setq results (read (current-buffer)))
      (kill-buffer (current-buffer)))
    results))



;; (defun esup-show-results (results)
;;   "Show timings from RESULTS in current buffer."
;;   (let ((totaltime (esup-totaltime results))
;; 	current percentage ov)
;;     (while results
;;       (let* ((current (pop results))
;; 	     (ov (make-overlay (car current) (cadr current)))
;; 	     (current (car (last current)))
;; 	     (percentage (/ (+ (car current) (nth 2 current))
;; 			    totaltime))
;; 	     col benchstr lowface)
;; 	(setq col
;; 	      (esup-percentage-color
;; 	       percentage
;; 	       (if (color-defined-p (face-background 'default))
;; 		   (face-background 'default)
;; 		   "black")
;; 	       (face-background 'esup-time-face)))
;; 	(setq percentage (round (* 100 percentage)))
;; 	(setq benchstr (esup-make-benchstr current))
;; 	(overlay-put ov 'help-echo benchstr)
;; 	(if (and (numberp esup-low-percentage)
;; 		 (< percentage esup-low-percentage))
;; 	    (overlay-put ov 'face 'esup-low-percentage-face)
;; 	  (overlay-put ov 'before-string
;; 		       (propertize benchstr
;; 				   'face 'esup-highlight-face))
;; 	  (overlay-put ov 'face
;; 		       `(:background ,col)))))
;;     (setq ov (make-overlay (1- (point-max)) (point-max)))
;;     (overlay-put ov 'after-string
;; 		 (propertize
;; 		  (format "\n-----------------\nTotal time: %.2fs\n"
;; 			  totaltime)
;; 		  'face 'esup-highlight-face))))



;; (defun esup-percentage-color (percent col-begin col-end)
;;   "Calculate color according to PERCENT between COL-BEGIN and COL-END."
;;   (let* ((col1 (color-values col-begin))
;; 	 (col2 (color-values col-end))
;; 	 (col
;; 	  (mapcar (lambda (c)
;; 		    (round
;; 		     (+ (* (- 1 percent) (nth c col1))
;; 			(* percent (nth c col2)))))
;; 		  '(0 1 2))))
;;     (format "RGB:%04x/%04x/%04x"
;; 	    (car col)
;; 	    (nth 1 col)
;; 	    (nth 2 col))))


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

;; (defun esup-make-benchstr (timings)
;;   "Create descriptive benchmark string from TIMINGS."
;;   (format
;;    (concat
;;     "<Percentage: %d ; "
;;     "Time: %.2f ; "
;;     "Number of GC: %d ; "
;;     "Time for GC: %.2f>\n")
;;    percentage
;;    (car timings) (nth 1 timings) (nth 2 timings)))


;; Enable lexical binding.  Shouldn't affect Emacsen without lexbind
;; support.
;;
;; Local Variables:
;; lexical-binding: t
;; End:

(provide 'esup)

;; esup.el ends here
