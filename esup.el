;;; esup.el --- the Emacs StartUp Profiler (ESUP)

;; Copyright (C) 2013 Joe Schafer

;; Author: Joe Schafer <joe@jchaf.com>

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
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

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

;;; Usage:

;; Start emacs as follows:
;;
;;    emacs -Q -l <PATH>/esup.el -f esup
;;
;; with <PATH> being the path to where this file resides.

;;; Download:

;;  You can always get the latest version from
;;       http://randomsample.de/profile-dotemacs.el


;;; Code:

(require 'benchmark)
(eval-when-compile
 (require 'cl-lib))

;; User variables

(defconst esup-user-init-files '("~/.emacs"
                                 "~/.emacs.el"
                                 "~/.emacs.d/init.el")
  "Possible user init files to be profiled.")

(defvar esup-low-percentage 3
  "Percentage which should be considered low.
All sexp's with a running time below this percentage will be
grayed out.")

(defface esup-time-face
  '((((background dark)) (:background "OrangeRed1"))
    (t (:background "red3")))
  "Background color to indicate percentage of total time.")

(defface esup-low-percentage-face
  '((((background dark)) (:foreground "gray25"))
    (t (:foreground "gray75")))
  "Face for sexps below `esup-low-percentage'.")

(defface esup-highlight-face
  '((((background dark)) (:background "blue"))
    (t (:background "yellow")))
  "Highlight face for benchmark results.")

(defun esup-trim-left (s)
  "Remove whitespace at the beginning of S."
  (if (string-match "\\`[ \t\n\r]+" s)
      (replace-match "" t t s)
    s))

(defun esup-trim-right (s)
  "Remove whitespace at the end of S."
  (if (string-match "[ \t\n\r]+\\'" s)
      (replace-match "" t t s)
    s))

(defun esup-trim (s)
  "Remove whitespace at the beginning and end of S."
  (esup-trim-left (esup-trim-right s)))

;; Main function
(defun esup-profile-file (file-name)
  "Profile FILE-NAME and return the benchmarked expressions."
  (let ((clean-file (esup-trim file-name))
        abs-file-path)
    (message "esup-profile-file dirty %s" clean-file)
    ;; Either clean up the string or look up the variable
    (if (intern-soft clean-file)
        (progn
          (message "esup-profile-file interning file %s %s" (symbol-value (intern-soft clean-file)) (intern-soft clean-file))
          (setq clean-file (symbol-value (intern-soft clean-file))))
      (message "Don't have an intern")
      (setq clean-file (replace-regexp-in-string "\"" "" clean-file)))

    (message "esup-profile-file clean %s" clean-file)
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
    (let ((file-name (buffer-file-name))
          (buffer-read-only t)
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
  "Profile the sexp between points START and END in the current buffer.

Returns a list of benchmarked expressions."
  (let* ((sexp (progn (goto-char start) (sexp-at-point)))
         (sexp-str (format "%s" sexp))
         (eval (eval sexp))
         load-file-name)
    (if (looking-at "(load ")
        (progn
          (goto-char (match-end 0))
          (setq load-file-name (buffer-substring
                                (point)
                                (progn (forward-sexp 1) (point))))
          (esup-profile-file load-file-name))
      (list (list file-name start end (benchmark-run (eval sexp)))))))

(defun esup-start-profile ()
  (let ((init-file (car (cl-remove-if-not #'file-exists-p
                                          esup-user-init-files)))
        results)
    (message "esup profiling init file: %s" init-file)
    (add-to-list 'load-path (file-name-directory init-file))
    (setq results (esup-profile-file init-file))
    (princ results)
    (kill-emacs)))

(defun esup ()
  "Load first existent init file from `esup-files' and benchmark its sexps."
  (interactive)
  (if noninteractive (esup-start-profile) (esup-start-process)))

(defun esup-start-process ()
  "Start a new emacs and profile its startup time."
  (with-current-buffer (get-buffer-create "*esup*")
    (erase-buffer))
  (start-process "*esup*" "*esup*"
                 "emacs"
                 "-q"
                 "--batch"
                 "-l" "~/.emacs.d/el-get/esup/esup.el"
                 "-f" "esup"))
;; Helper functions

(defun esup-show-results (results)
  "Show timings from RESULTS in current buffer."
  (let ((totaltime (esup-totaltime results))
	current percentage ov)
    (while results
      (let* ((current (pop results))
	     (ov (make-overlay (car current) (cadr current)))
	     (current (car (last current)))
	     (percentage (/ (+ (car current) (nth 2 current))
			    totaltime))
	     col benchstr lowface)
	(setq col
	      (esup-percentage-color
	       percentage
	       (if (color-defined-p (face-background 'default))
		   (face-background 'default)
		   "black")
	       (face-background 'esup-time-face)))
	(setq percentage (round (* 100 percentage)))
	(setq benchstr (esup-make-benchstr current))
	(overlay-put ov 'help-echo benchstr)
	(if (and (numberp esup-low-percentage)
		 (< percentage esup-low-percentage))
	    (overlay-put ov 'face 'esup-low-percentage-face)
	  (overlay-put ov 'before-string
		       (propertize benchstr
				   'face 'esup-highlight-face))
	  (overlay-put ov 'face
		       `(:background ,col)))))
    (setq ov (make-overlay (1- (point-max)) (point-max)))
    (overlay-put ov 'after-string
		 (propertize
		  (format "\n-----------------\nTotal time: %.2fs\n"
			  totaltime)
		  'face 'esup-highlight-face))))

(defun esup-totaltime (results)
  "Calculate total time of RESULTS."
  (let ((totaltime 0))
    (mapc (lambda (x)
	    (let ((cur (car (last x))))
	      (setq totaltime (+ totaltime (car cur) (nth 2 cur)))))
	  results)
    totaltime))

(defun esup-percentage-color (percent col-begin col-end)
  "Calculate color according to PERCENT between COL-BEGIN and COL-END."
  (let* ((col1 (color-values col-begin))
	 (col2 (color-values col-end))
	 (col
	  (mapcar (lambda (c)
		    (round
		     (+ (* (- 1 percent) (nth c col1))
			(* percent (nth c col2)))))
		  '(0 1 2))))
    (format "RGB:%04x/%04x/%04x"
	    (car col)
	    (nth 1 col)
	    (nth 2 col))))

(defun esup-make-benchstr (timings)
  "Create descriptive benchmark string from TIMINGS."
  (format
   (concat
    "<Percentage: %d ; "
    "Time: %.2f ; "
    "Number of GC: %d ; "
    "Time for GC: %.2f>\n")
   percentage
   (car timings) (nth 1 timings) (nth 2 timings)))


;; esup.el ends here
