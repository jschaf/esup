;;; esup.el --- The Emacs StartUp Profiler (ESUP) -*- lexical-binding: t -*-

;; Copyright (C) 2014, 2015, 2016, 2017, 2018, 2019, 2020 Joe Schafero

;; Author: Joe Schafer <joe@jschaf.com>
;; Maintainer: Serghei Iakovlev <egrep@protonmail.ch>
;; Version: 0.7.1
;; URL: https://github.com/jschaf/esup
;; Keywords: convenience, processes
;; Package-Requires: ((cl-lib "0.5") (s "1.2") (emacs "25.1"))

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

;; Benchmark Emacs Startup time without ever leaving your Emacs.
;;
;; Esup profiles your Emacs startup time by examining all top-level
;; S-expressions (sexps).  Esup starts a new Emacs process from
;; Emacs to profile each SEXP.  After the profiled Emacs is complete,
;; it will exit and your Emacs will display the results.
;;
;; Esup will step into `require' and `load' forms at the top level
;; of a file, but not if they're enclosed in any other statement.
;;
;; Installation:
;;
;; Place esup.el and esup-child.el on your `load-path' by adding this
;; to your `user-init-file', usually ~/.emacs or ~/.emacs.d/init.el
;;
;;   (add-to-list 'load-path "~/dir/to-esup")
;;
;; Load the code:
;;
;;   (autoload 'esup "esup" "Emacs Start Up Profiler." nil)
;;
;; M-x `esup' to profile your Emacs startup and display the results.
;;
;; The master of all the material is the GitHub repository
;; (see URL `https://github.com/jschaf/esup').
;;
;; Bugs:
;;
;; Bug tracking is currently handled using the GitHub issue tracker
;; (see URL `https://github.com/jschaf/esup/issues').

;;; Code:


;;; Requirements

(require 'eieio)
(require 'esup-child)

(eval-when-compile
  (require 'cl-lib))


;;; Esup internals

(defvar esup-load-path
  ;; Emacs doesn't visit a file when loading it, meaning
  ;; `buffer-file-name' returns nil.
  (file-name-directory (file-truename
                        (if load-in-progress
                            load-file-name
                          buffer-file-name)))
  "Full directory path to esup.el and esup-child.el.")


;;; User variables

(defgroup esup nil
  "A major mode for the Emacs Start Up Profiler."
  :prefix "esup-"
  :version "0.6"
  :group 'languages)

(defcustom esup-user-init-file user-init-file
  "The user init files to profile."
  :group 'esup
  :type 'string)

(defcustom esup-depth 1
  "How deep to profile require and load expressions.
0, don't step into any require statements.
1, step into require statements in `esup-init-file'.
n, step into up to n levels of require statements."
  :group 'esup
  :type 'integer)

(defcustom esup-run-as-batch-p nil
  "If non-nil, run the profiled Emacs as batch.
This option is off by default because batch runs faster than
regular Emacs, so the timing information is not as realistic.  If
you don't want to the benchmarked Emacs frame to appear when
running `esup', set this to t."
  :group 'esup
  :type 'boolean)

(defcustom esup-insignificant-time 0.009
  "Only show expressions that take longer than this time."
  :group 'esup
  :type 'float)

(defcustom esup-server-port nil
  "The port for esup to communicate with the child Emacs.
If value is nil, Emacs selects an unused port."
  :group 'esup
  :type 'integer)

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

(defface esup-error-face
  '((t :inherit font-lock-warning-face))
  "Face for displaying errors in the *esup* buffer."
  :group 'esup
  :version "25.1")

(defvar esup-child-process nil
  "The current esup child process, i.e the Emacs being timed.")

(defvar esup-emacs-path (concat invocation-directory invocation-name)
  "Path to the Emacs binary used for profiling.")

(defvar esup-errors '()
  "A list of error messages from the child Emacs.")



(defun esup-total-exec-time (results)
  "Calculate the total execution time of RESULTS."
  (cl-loop for result in results
           sum (slot-value result 'exec-time) into total-exec-time
           finally return total-exec-time))

(defun esup-total-gc-number (results)
  "Calculate the total number of GC pauses of RESULTS."
  (cl-loop for result in results
           sum (slot-value result 'gc-number) into total-gc-number
           finally return total-gc-number))

(defun esup-total-gc-time (results)
  "Calculate the total time spent in GC of RESULTS."
  (cl-loop for result in results
           sum (slot-value result 'gc-time) into total-gc-time
           finally return total-gc-time))

(defun esup-drop-insignificant-times (results)
  "Remove inconsequential entries and sort RESULTS."
  (cl-delete-if (lambda (a) (< a esup-insignificant-time))
                results
                :key #'(lambda (obj) (slot-value obj 'exec-time)))
  (cl-sort results '> :key #'(lambda (obj) (slot-value obj 'exec-time))))

(defun esup-update-percentages (results)
  "Add the percentage of exec-time to each item in RESULTS."
  (cl-loop for result in results
           with total-time = (esup-total-exec-time results)
           do
           (oset result :percentage (* 100 (/ (slot-value result 'exec-time)
                                              total-time)))))


;;; Controller - the entry points.

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
  (buffer-disable-undo)
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
        ;; Break out of the loop because we couldn't find a previous
        ;; text-property of result-break, so we're at the beginning of
        ;; the buffer.
        (setq arg 0)
        (setq prev-point (point-min))))
    (goto-char prev-point)
    (when (get-text-property (point) 'result-break)
      (forward-char))))

(defun esup-child-process-sentinel (process status)
  "Monitor PROCESS for change in STATUS."
  (cond ((string= status "finished\n") (esup-display-results))
        (t (insert (format "Process %s %s" process status)))))

(defvar esup-server-process nil
  "The parent Emacs' server process.
The child Emacs send data to this process on
`esup-child-results-port' and `esup-child-log-port'.")

(defvar esup-child-results-port nil
  "The port by which the child Emacs sends profile results.")

(defvar esup-child-log-port nil
  "The port by which the child Emacs sends log information.")

(defvar esup-server-log-buffer "*esup-log*"
  "The log buffer for esup server messages.")

(defun esup-server-log (format-str &rest args)
  "Log FORMAT-STR with format ARGS to `esup-server-log-buffer'."
  (unless (string-equal format-str "")
    (with-current-buffer esup-server-log-buffer
      (unless (bobp) (insert "\n"))
      (goto-char (point-max))
      (if args
          (insert (apply 'format  format-str args))
        (insert format-str)))))

(defvar esup-incoming-results-buffer "*esup-results*"
  "The buffer to hold incoming information from the child Emacs.")

(defun esup-store-partial-result (result-str)
  "Write RESULT-STR to `esup-incoming-results-buffer'."
  (with-current-buffer (get-buffer-create esup-incoming-results-buffer)
    (goto-char (point-max))
    (insert result-str)))

(defun esup-select-port ()
  "Select a port for the esup server process.
If `esup-server-port' is nil, then let the OS select an unused
port."
  ;; The value `t' instructs Emacs to pick an unused port.
  (or esup-server-port t))

(defun esup-server-create (port)
  "Create the esup parent server at localhost:PORT."
  (interactive)
  (make-network-process
   :name "*esup-server*"
   :type nil ; stream
   :server t
   :host 'local
   :service port
   :family nil
   ;; Broken on Emacs 25
   ;; :nowait t
   :stop nil
   :buffer esup-server-log-buffer
   :coding 'utf-8
   :noquery t
   :filter 'esup--server-filter
   :sentinel 'esup--server-sentinel
   :log 'esup--server-logger))

(defun esup--server-filter (proc string)
  "Filter the log and result entries recieved at the parent process.
PROC is the process and STRING is the message.  `esup-child'
starts messages with LOGSTREAM or RESULTSSTREAM to indicate the
type of message."
  (cond
   ((string-prefix-p "LOGSTREAM" string)
    (setq esup-child-log-port (process-contact proc :service))
    (esup-server-log "Set information from port %s to be the log process"
                     esup-child-log-port)
    ;; There might be information that tagged along with LOGSTREAM
    (esup-server-log (substring string
                                (length "LOGSTREAM")
                                (length string))))

   ((string-prefix-p "RESULTSSTREAM" string)
    (setq esup-child-results-port (process-contact proc :service))
    (esup-server-log "Set information from port %s to be the results process"
                     esup-child-results-port)
    ;; There might be information that tagged along with RESULTSSTREAM
    (esup-store-partial-result (substring string
                                          (length "RESULTSSTREAM")
                                          (length string))))

   ((eq esup-child-results-port (process-contact proc :service))
    (esup-store-partial-result string))

   ((eq esup-child-log-port (process-contact proc :service))
    (when (string-prefix-p "LOG: ERROR" string)
      (push (substring string (length "LOG: ")) esup-errors))
    (esup-server-log string))

   (t
    (error "Recieved unknown message type"))))

(defun esup--server-sentinel (proc event)
  "Listen for PROC EVENTs."
  (esup-server-log "name: %s, sentinel: proc: %s, event %s"
                   (process-name proc) proc event))

(defun esup--server-logger (server connection message)
  "Log adapter for `make-network-process'.
Provides a useful default for SERVER, CONNECTION and MESSAGE."
  (esup-server-log "logged: server %s, connection %s, message %s"
                   server connection message))

(defvar esup-last-result-start-point 1
  "The end point of the last read result from `esup-incoming-results-buffer'.")

(defun esup-reset ()
  "Reset all variables and buffers for another run of `esup'."
  (setq esup-last-result-start-point 1)
  (with-current-buffer (get-buffer-create esup-server-log-buffer)
    (buffer-disable-undo)
    (erase-buffer))
  (with-current-buffer (get-buffer-create esup-incoming-results-buffer)
    (buffer-disable-undo)
    (erase-buffer))
  (setq esup-errors '())
  (when esup-server-process
    (delete-process esup-server-process)))

;;;###autoload
(defun esup (&optional init-file &rest args)
  "Profile the startup time of Emacs in the background.
If INIT-FILE is non-nil, profile that instead of USER-INIT-FILE.
ARGS is a list of extra command line arguments to pass to Emacs."
  (interactive "P")
  (setq init-file
        (cond
         ;; Universal prefix arg, so prompt
         ((equal init-file '(4)) (read-file-name "Profile a file with esup: "))
         ((stringp init-file) init-file)
         (t esup-user-init-file)))

  (message "Starting esup...")
  (esup-reset)

  (setq esup-server-process (esup-server-create (esup-select-port)))
  (setq esup-server-port (process-contact esup-server-process :service))
  (message "esup process started on port %s" esup-server-port)

  (let ((process-args `("*esup-child*"
                        "*esup-child*"
                        ,esup-emacs-path
                        ,@args
                        "-q"
                        "-L" ,esup-load-path
                        "-l" "esup-child"
                        ,(format "--eval=(esup-child-run \"%s\" \"%s\" %d)"
                                 init-file
                                 esup-server-port
                                 esup-depth))))

    ;; The option -q is set by itself because this `start-process' errors if we
    ;; pass either an empty string or nil as an argument.
    (when esup-run-as-batch-p
      (setq process-args (append process-args '("--batch"))))
    (setq esup-child-process (apply #'start-process process-args)))

  (set-process-sentinel esup-child-process 'esup-child-process-sentinel))

(defun esup-follow-link (pos)
  "Follow the link that was clicked at point POS."
  (let ((file (get-text-property pos 'full-file))
        (start-point (get-text-property pos 'start-point)))
    (message "Opening link to %s" file)
    (find-file-other-window file)
    (goto-char start-point)))


;;; Utilities.

(defsubst esup-propertize-string (str &rest properties)
  "Replace all properties of STR with PROPERTIES."
  (set-text-properties 0 (length str) properties str)
  str)

(defsubst esup-fontify-string (str face)
  "Modify STR's font-lock-face property to FACE and return STR."
  (esup-propertize-string str 'font-lock-face face))


;;; View - rendering functions.

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
  (let* ((all-results (esup-fontify-results
                       (esup-read-results)))
         (results (esup-drop-insignificant-times all-results))
         (result-break (esup-propertize-string "\n" 'result-break t))
         ;; Needed since the buffer is in `view-mode'.
         (inhibit-read-only t))
    (with-current-buffer (esup-buffer)
      (erase-buffer)
      (esup-update-percentages results)
      (when esup-errors
        (insert (esup-render-errors esup-errors) result-break))
      (insert (esup-render-summary results) result-break)
      (cl-loop for result in results
               do (insert (render result) result-break))
      ;; We want the user to be at the top because it's disorienting
      ;; to start at the bottom.
      (goto-char (point-min))
      (pop-to-buffer (current-buffer))))
  (message "esup finished"))

(defun esup-render-errors (errors)
  "Return a fontified string of ERRORS."
  (if esup-errors
      (concat
       (esup-fontify-string
        "ERROR: the child emacs had the following errors:\n"
        'esup-error-face)
       (mapconcat 'identity
                  (cl-loop for error-string in errors
                           collect (format "  %s" error-string))
                  "\n")
       "\n\n"
       (esup-fontify-string "Results will be incomplete due to errors.\n\n"
                            'esup-error-face))
    ""))

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

(cl-defmethod render ((obj esup-result))
  "Render fields with OBJ and return the string."
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
    (cl-loop for result in results
             do
             (erase-buffer)
             (insert (slot-value result 'expression-string))
         (font-lock-ensure)
         (setf (slot-value result 'expression-string) (buffer-string)))
    results))

(defun esup-read-result (start-point)
  "Return one `esup-result' object from the current buffer.
Begins reading at START-POINT.
Returns either a class `esup-result' or nil."
  (goto-char start-point)
  (eval (read (current-buffer))))

(defun esup-next-separator-end-point ()
  "Return the end point of the next `esup-child-result-separator'.
Returns either an point or nil if `esup-child-result-separator' isn't bounded in
current lexical context."
  (when (boundp 'esup-child-result-separator)
    (save-excursion (search-forward esup-child-result-separator
                                    (point-max) 'noerror))))

(defun esup-read-results ()
  "Read all `esup-result' objects from `esup-incoming-results-buffer'."
  (let (results sep-end-point)
    (with-current-buffer (get-buffer esup-incoming-results-buffer)
      (goto-char esup-last-result-start-point)
      (message "at %s" esup-last-result-start-point)
      (unless (eobp)
        (while (setq sep-end-point (esup-next-separator-end-point))
          (setq results (cons (car (esup-read-result (point))) results))
          (setq esup-last-result-start-point sep-end-point)
          (goto-char esup-last-result-start-point))))
    (nreverse results)))


(provide 'esup)
;;; esup.el ends here
