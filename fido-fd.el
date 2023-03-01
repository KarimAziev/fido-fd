;;; fido-fd.el --- Fido interface for fd -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Karim Aziiev <karim.aziiev@gmail.com>

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/fido-fd
;; Version: 0.1.0
;; Keywords: files
;; Package-Requires: ((emacs "28.1"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Fido interface for fd

;;; Code:


(defcustom fido-fd-multi-command-flags '("--changed-within 1d"
                                         "--changed-before 1d")
  "Flags to pass on every search."
  :type '(repeat string)
  :group 'fido-fd)

(defcustom fido-fd-exec-path (or (executable-find "fdfind")
                                 (executable-find "fd"))
  "Path to fd program."
  :group 'fido-fd
  :type 'string)

(defcustom fido-fd-resolve-project-root-fn 'fido-fd-resolve-project-root
  "Function to find project directory."
  :group 'fido-fd
  :type 'function)

(defmacro fido-fd--pipe (&rest functions)
  "Return left-to-right composition from FUNCTIONS."
  (declare (debug t) (pure t) (side-effect-free t))
  `(lambda (&rest args)
     ,@(let ((init-fn (pop functions)))
         (list
          (seq-reduce
           (lambda (acc fn)
             (if (symbolp fn)
                 `(funcall #',fn ,acc)
               `(funcall ,fn ,acc)))
           functions
           (if (symbolp init-fn)
               `(apply #',init-fn args)
             `(apply ,init-fn args)))))))

(defmacro fido-fd--compose (&rest functions)
  "Return right-to-left composition from FUNCTIONS."
  (declare (debug t) (pure t) (side-effect-free t))
  `(fido-fd--pipe ,@(reverse functions)))


(defmacro fido-fd--rpartial (fn &rest args)
  "Return a partial application of FN to right-hand ARGS.

ARGS is a list of the last N arguments to pass to FN. The result is a new
function which does the same as FN, except that the last N arguments are fixed
at the values with which this function was called."
  (declare (side-effect-free t))
  `(lambda (&rest pre-args)
     ,(car (list (if (symbolp fn)
                     `(apply #',fn (append pre-args (list ,@args)))
                   `(apply ,fn (append pre-args (list ,@args))))))))

(defvar fido-fd-current-switch-idx nil)
(defcustom fido-fd-switches nil
  "Fido switches."
  :group 'fido-fd
  :type '(repeat (cons directory
                       (repeat :tag "Fdind options" string))))

(defun fido-fd-index-switcher (step current-index switch-list)
  "Increase or decrease CURRENT-INDEX depending on STEP value and SWITCH-LIST."
  (cond ((> step 0)
         (if (>= (+ step current-index)
                 (length switch-list))
             0
           (+ step current-index)))
        ((< step 0)
         (if (or (<= 0 (+ step current-index)))
             (+ step current-index)
           (1- (length switch-list))))))

(defcustom fido-fd-root-flags  '("-E 'run'"
                                 "-E" "'jest_rs'"
                                 "-E" "'home'"
                                 "--follow")
  "Flags to pass fdind when searching in root directory."
  :type '(repeat string)
  :group 'fido-fd)


(defvar fido-fd-args nil)
(defvar fido-fd-current-dir nil)
(defvar fido-fd-last-input nil)

;;;###autoload
(defun fido-fd-next-switch ()
  "Select next element from `fido-fd-switches'."
  (interactive)
  (setq fido-fd-current-switch-idx
        (if fido-fd-current-switch-idx
            (fido-fd-index-switcher 1
                                    fido-fd-current-switch-idx
                                    fido-fd-switches)
          0))
  (when-let* ((switches (nth fido-fd-current-switch-idx fido-fd-switches)))
    (setq fido-fd-current-dir (car switches))
    (setq fido-fd-args (cdr switches))
    (if (not (minibufferp))
        (apply #'fido-fd-async fido-fd-current-dir switches
               fido-fd-last-input)
      (setq fido-fd-last-input
            (minibuffer-contents-no-properties))
      (run-with-timer 0.2 nil 'fido-fd-async fido-fd-current-dir fido-fd-args
                      fido-fd-last-input)
      (abort-minibuffers))))

(defun fido-fd-parent (path)
  "Return the parent directory to PATH without slash."
  (let ((parent (file-name-directory
                 (directory-file-name
                  (expand-file-name path default-directory)))))
    (when (and (file-exists-p path)
               (file-exists-p parent)
               (not (equal
                     (file-truename (directory-file-name
                                     (expand-file-name path)))
                     (file-truename (directory-file-name
                                     (expand-file-name parent))))))
      (if (file-name-absolute-p path)
          (directory-file-name parent)
        (file-relative-name parent)))))

(defun fido-fd-slash (dir)
  "Add slash to DIR if none."
  (when dir
    (if (string-match-p "/$" dir)
        dir
      (setq dir (concat dir "/")))))

(defun fido-fd-parent-dir (path)
  "Return the parent directory to PATH with slash."
  (when-let ((path (fido-fd-parent path)))
    (fido-fd-slash path)))

(defun fido-fd-generic-list-to-string (&rest flags)
  "Flattenize and join FLAGS using spaces."
  (setq flags (delete nil (flatten-list flags)))
  (when flags
    (string-join flags "\s")))

(defun fido-fd-map-ignored (ignored)
  "Generate fd flag to exclude IGNORED from search."
  (let ((re "^\\(-E\\|--exclude\\)"))
    (if (stringp ignored)
        (if (string-match-p re ignored)
            ignored
          (concat "-E " ignored))
      (if (and ignored (listp ignored))
          (fido-fd-generic-list-to-string
           (mapcar (lambda (it) (if (and (stringp it)
                                    (not (string-match-p re it)))
                               (concat "-E " it)
                             it))
                   ignored))))))

(defun fido-fd-make-sortable-pre-part (&optional dir flags ignores)
  "Return string with fdfind command to search in DIR.

FLAGS and IGNORES should be string or list or alist.

IGNORES may omit --exclude flag."
  (setq flags (fido-fd-generic-list-to-string flags))
  (setq ignores (fido-fd-generic-list-to-string
                 (fido-fd-map-ignored ignores)))
  (let* ((dir-flag (when dir (fido-fd-generic-list-to-string "." dir)))
         (parts (mapcar
                 (fido-fd--compose
                  (fido-fd--rpartial concat ";")
                  'fido-fd-generic-list-to-string
                  (apply-partially #'append '("fdfind" "-0" "--color=never"))
                  (fido-fd--rpartial append (list ignores flags dir-flag))
                  'list)
                 (if (equal dir "/")
                     (list (fido-fd-generic-list-to-string
                            fido-fd-root-flags))
                   fido-fd-multi-command-flags))))
    (fido-fd-generic-list-to-string parts)))

;;;###autoload
(defun fido-fd-make-sortable-command (place &optional common-flags ignores)
  "Return string with fdfind command to search in PLACE.
PLACE can be a string of directory, list of directories,or alist of directories
with extra flags.

COMMON-FLAGS and IGNORES should be string or list or alist.

IGNORES may omit --exclude flag."
  (if (stringp place)
      (fido-fd-make-sortable-pre-part place common-flags ignores)
    (mapconcat (lambda (it)
                 (if (stringp it)
                     (fido-fd-make-sortable-pre-part
                      it
                      common-flags
                      ignores)
                   (let ((dir (car it))
                         (flags (cdr it)))
                     (fido-fd-make-sortable-pre-part
                      dir
                      (append (if (listp flags)
                                  flags
                                (list flags))
                              common-flags)
                      ignores))))
               place
               "\s")))

;;;###autoload
(defun fido-fd-make-sortable-tr-command (place &optional common-flags ignores)
  "Return combined `fdfind' and `tr' command to search in PLACE.

PLACE can be a string of directory, list of directories,or alist of directories
 with extra flags.

COMMON-FLAGS and IGNORES should be string or list or alist.
IGNORES may omit --exclude flag."
  (let ((command (fido-fd-make-sortable-command place common-flags ignores)))
    (string-join (list "{" command "}" "|"  "tr '\n' ' '") "\s")))

;;;###autoload
(defun fido-fd-multi-dir (place &optional flags ignored)
  "Return list of files  in PLACE.
PLACE can be a string of directory, list of directories, or alist of directories
with extra flags.

FLAGS and IGNORES should be string, list or alist of strings.

IGNORED may omit --exclude flag."
  (split-string (shell-command-to-string
                 (fido-fd-make-sortable-tr-command place flags ignored))
                "\0" t))

(defun fido-fd-make-command (&optional dir flags)
  "Generate string with fd command in DIR from FLAGS."
  (fido-fd-generic-list-to-string
   "fdfind" "--color=never"
   flags
   (fido-fd-generic-list-to-string "." dir)))

(defun fido-fd-find (place &optional flags)
  "Return list of files  in PLACE.
PLACE can be a string of directory, list of directories, or alist of directories
with extra flags.

FLAGS should be string, list or alist of strings."
  (split-string (shell-command-to-string
                 (fido-fd-make-command place flags))
                "\n" t))

(defvar fido-fd-async-command nil)

(defun fido-fd-count-matches-by-re (re str &optional start end)
  "Count occurrences of RE in STR.
START, inclusive, and END, exclusive, delimit the part of s to
match.  START and END are both indexed starting at 1; the initial
character in s is index 1."
  (save-match-data
    (with-temp-buffer
      (insert str)
      (goto-char (point-min))
      (count-matches re (or start 1) (or end (point-max))))))

(defcustom fido-fd-async-filter-update-time 500000
  "The amount of microseconds to wait until updating `fido-fd--async-filter'."
  :type 'integer
  :group 'fido-fd)

(defcustom fido-fd-async-command-delay 0
  "Number of seconds to wait before spawning another async command."
  :type 'number
  :group 'fido-fd)

(defvar fido-fd--async-timer nil
  "Timer used to dispose `fido-fd--async-command.")


(defvar fido-fd--async-time nil
  "Store the time when a new process was started.
Or the time of the last minibuffer update.")


(defvar fido-fd--async-start nil
  "Store the time when a new process was started.")

(defvar fido-fd--async-duration nil
  "Store the time a process takes to gather all its candidates.
The time is measured in seconds.")

(defvar fido-fd--async-last-error-string nil
  "When the process returned non-0, store the output here.")

(defvar fido-fd--async-last-command nil
  "Store the last fd command.")

(defvar fido-fd-cands nil)
(defun fido-fd-icomplete-mode-p ()
  "Return non nil if fido related modes enabled."
  (or (bound-and-true-p fido-mode)
      (bound-and-true-p icomplete-mode)
      (bound-and-true-p fido-vertical-mode)))



(defun fido-fd-exhibit ()
  "Invoke `icomplete-exhibit' or `minibuffer-completion-help'."
  (when (minibufferp)
    (when (and
         (fido-fd-icomplete-mode-p)
         (fboundp 'icomplete-exhibit))
        (icomplete-exhibit))))

(defun fido-fd--sync-sentinel-on-exit (process)
  "Synchronize PROCESS sentinel."
  (if (zerop (process-exit-status process))
      (progn (setq fido-fd-cands (with-current-buffer (process-buffer process)
                                   (fido-fd--split-string)))
             (when fido-fd--async-start
               (setq fido-fd--async-duration
                     (time-to-seconds (time-since fido-fd--async-start))))
             (fido-fd-exhibit))
    (setq fido-fd--async-last-error-string
          (with-current-buffer (process-buffer process)
            (buffer-string)))
    (setq fido-fd-cands nil)
    (fido-fd-exhibit)))

(defun fido-fd--split-string (&optional str)
  "Split STR or buffer string."
  (split-string
   (or str (buffer-string))
   "\0"
   t))

(defun fido-fd-delete-process (&optional name)
  "Delete current `fido-fd' process or that with NAME."
  (let ((process (get-process (or name " *fido-fd*"))))
    (when process
      (delete-process process))))

(defun fido-fd--async-sentinel (process _msg)
  "Sentinel function for an asynchronous `fido-fd' PROCESS."
  (when (eq (process-status process) 'exit)
    (fido-fd--sync-sentinel-on-exit process)))

(defun fido-fd--async-filter (process str)
  "Receive from PROCESS the output STR.
Update the minibuffer with the amount of lines collected every
`fido-fd-async-filter-update-time' microseconds since the last update."
  (condition-case err
      (progn (with-current-buffer (process-buffer process)
               (insert str))
             (when (time-less-p (list 0 0 fido-fd-async-filter-update-time)
                                (time-since fido-fd--async-time))
               (setq fido-fd-cands (with-current-buffer (process-buffer process)
                                     (fido-fd--split-string)))
               (fido-fd-exhibit)))
    (error (message "FIDO-FD ERROR %s" err))))

(defun fido-fd--async-command-1 (cmd &optional sentinel filter name)
  "Start and return new `fido-fd' process by calling CMD.
CMD can be either a shell command as a string, or a list of the
program name to be called directly, followed by its arguments.
If the default `fido-fd' process or one with NAME already exists,
kill it and its associated buffer before starting a new one.
Give the process the functions SENTINEL and FILTER, which default
to `fido-fd--async-sentinel' and `fido-fd--async-filter',
respectively."
  (fido-fd-delete-process name)
  (setq name (or name " *fido-fd*"))
  (when (get-buffer name)
    (kill-buffer name))
  (setq fido-fd--async-last-command cmd)
  (let* ((buf (get-buffer-create name))
         (proc (if (listp cmd)
                   (apply #'start-file-process name buf cmd)
                 (start-file-process-shell-command name buf cmd))))
    (setq fido-fd--async-time (current-time))
    (setq fido-fd--async-start fido-fd--async-time)
    (set-process-sentinel proc (or sentinel #'fido-fd--async-sentinel))
    (set-process-filter proc (or filter #'fido-fd--async-filter))
    proc))

(defun fido-fd--elisp-to-pcre (regex)
  "Convert REGEX from Elisp format to PCRE format, on best-effort basis.
REGEX may be of any format returned by an Fido regex function,
namely a string or a list.  The return value is always a string.

Note that incorrect results may be returned for sufficiently
complex regexes."
  (if (consp regex)
      (mapconcat
       (lambda (pair)
         (let ((subexp (fido-fd--elisp-to-pcre (car pair))))
           (if (string-match-p "|" subexp)
               (format "(?:%s)" subexp)
             subexp)))
       (seq-filter #'cdr regex)
       ".*")
    (replace-regexp-in-string
     "\\\\[(){}|`']\\|[()]"
     (lambda (s)
       (or (cdr (assoc s '(("\\(" . "(")
                           ("\\)" . ")")
                           ("(" . "\\(")
                           (")" . "\\)")
                           ("\\{" . "{")
                           ("\\}" . "}")
                           ("\\|" . "|")
                           ("\\`" . "^")
                           ("\\'" . "$"))))
           (error
            "Unexpected error in `fido-fd--elisp-to-pcre' (got match %S)" s)))
     regex t t)))

(defun fido-fd--async-command (&rest args)
  "Like `fido-fd--async-command-1', with same ARGS, but debounced.
Calls to `fido-fd--async-command-1' are separated by at least
`fido-fd-async-command-delay' seconds, so as to avoid issues
caused by spawning too many subprocesses too quickly."
  (if (zerop fido-fd-async-command-delay)
      (apply #'fido-fd--async-command-1 args)
    (when fido-fd--async-timer
      (cancel-timer fido-fd--async-timer))
    (setq fido-fd--async-timer
          (apply #'run-with-timer
                 fido-fd-async-command-delay
                 nil
                 #'fido-fd--async-command-1
                 args))))
(defvar fido-fd--subexps 0
  "Number of groups in the current `fido-fd--regex'.")

(defvar fido-fd--regex-hash
  (make-hash-table :test #'equal)
  "Store pre-computed regex.")
(defvar fido-fd--input-garbage nil)
(defun fido-fd-match-regex-brackets (str)
  "Match STR."
  (let ((len (length str))
        (i 1)
        (open-count 1)
        c)
    (while (and (< i len)
                (> open-count 0))
      (setq c (aref str i))
      (cond ((= c ?\[)
             (cl-incf open-count))
            ((= c ?\])
             (cl-decf open-count)))
      (cl-incf i))
    (when (= open-count 0)
      (if (eq (string-match "[+*?]" str i) i)
          (match-end 0)
        i))))

(defun fido-fd--regex-p (object)
  "Return OBJECT if it is a valid regular expression, else nil."
  (ignore-errors (string-match-p object "") object))

(defun fido-fd--regex-or-literal (str)
  "If STR isn't a legal regexp, escape it."
  (or (fido-fd--regex-p str) (regexp-quote str)))

(defun fido-fd--split (str)
  "Split STR into list of substrings bounded by spaces.
Single spaces act as splitting points.  Consecutive spaces
\"quote\" their preceding spaces, i.e., guard them from being
split.  This allows the literal interpretation of N spaces by
inputting N+1 spaces.  Any substring not constituting a valid
regexp is passed to `regexp-quote'."
  (let ((len (length str))
        (i 0)
        (start 0)
        (res nil)
        match-len
        end
        c)
    (catch 'break
      (while (< i len)
        (setq c (aref str i))
        (cond ((= ?\[ c)
               (if (setq end (fido-fd-match-regex-brackets
                              (substring str i)))
                   (cl-incf i end)
                 (setq fido-fd--input-garbage (substring str i))
                 (throw 'break nil)))
              ((= ?\\ c)
               (if (and (< (1+ i) len)
                        (= ?\((aref str (1+ i))))
                   (progn
                     (when (> i start)
                       (push (substring str start i) res))
                     (if (eq (string-match "\\\\([^\0]*?\\\\)" str i) i)
                         (progn
                           (push (match-string 0 str) res)
                           (setq i (match-end 0))
                           (setq start i))
                       (setq fido-fd--input-garbage (substring str i))
                       (throw 'break nil)))
                 (cl-incf i)))
              ((= ?\  c)
               (string-match " +" str i)
               (setq match-len (- (match-end 0)
                                  (match-beginning 0)))
               (if (= match-len 1)
                   (progn
                     (when (> i start)
                       (push (substring str start i) res))
                     (setq start (1+ i)))
                 (setq str (replace-match
                            (make-string (1- match-len) ?\ )
                            nil nil str))
                 (setq len (length str))
                 (cl-incf i (1- match-len)))
               (cl-incf i))
              (t
               (cl-incf i)))))
    (when (< start i)
      (push (substring str start) res))
    (mapcar #'fido-fd--regex-or-literal (nreverse res))))

(defun fido-fd--regex (str &optional greedy)
  "Re-build regex pattern from STR in case it has a space.
When GREEDY is non-nil, join words in a greedy way."
  (let ((hashed (unless greedy
                  (gethash str fido-fd--regex-hash))))
    (if hashed
        (progn
          (setq fido-fd--subexps (car hashed))
          (cdr hashed))
      (when (string-match-p "\\(?:[^\\]\\|^\\)\\\\\\'" str)
        (setq str (substring str 0 -1)))
      (setq str   (if (string-match "\\`\\(.*\\)[\\]|\\'" str)
                      (match-string 1 str)
                    str))
      (cdr (puthash str
                    (let ((subs (fido-fd--split str)))
                      (if (= (length subs) 1)
                          (cons
                           (setq fido-fd--subexps 0)
                           (if (string-match-p "\\`\\.[^.]" (car subs))
                               (concat "\\." (substring (car subs) 1))
                             (car subs)))
                        (cons
                         (setq fido-fd--subexps (length subs))
                         (replace-regexp-in-string
                          "\\.\\*\\??\\\\( "
                          "\\( "
                          (mapconcat
                           (lambda (x)
                             (if (string-match-p "\\`\\\\([^?][^\0]*\\\\)\\'" x)
                                 x
                               (format "\\(%s\\)" x)))
                           subs
                           (if greedy ".*" ".*?"))
                          nil t))))
                    fido-fd--regex-hash)))))

(defun fido-fd-async-cmd (input)
  "Return a `mdfind' shell command based on INPUT."
  (let* ((regex (shell-quote-argument (fido-fd--elisp-to-pcre
                                       (fido-fd--regex (file-name-nondirectory
                                                        (or input ""))))))
         (cmd (apply #'format fido-fd-async-command
                     (append
                      (make-vector
                       (fido-fd-count-matches-by-re "%s"
                                                    fido-fd-async-command)
                       regex)
                      nil))))
    cmd))

(defvar fido-fd-async-history nil
  "History for `fido-fd-async'.")

(defun fido-fd-async-function (input &rest _)
  "Execute fd command with INPUT."
  (or
   (progn
     (fido-fd--async-command
      (funcall #'fido-fd-async-cmd input))
     fido-fd-cands)))

;;;###autoload
(defun fido-fd-preview-file (file)
  "Momentarily display content of the FILE in popup window.

Display remains until next event is input."
  (if (and
       file
       (file-directory-p file))
      (fido-fd-visit-dir file)
    (when-let ((filename (and
                          file
                          (file-readable-p file)
                          (file-exists-p file)
                          file))
               (buffer (get-buffer-create
                        "*fido-fd-preview*")))
      (with-current-buffer buffer
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert-file-contents-literally file)
          (let ((size (buffer-size)))
            (if (>= size large-file-warning-threshold)
                (kill-buffer (current-buffer))
              (ignore-errors
                (delay-mode-hooks
                  (let ((buffer-file-name filename))
                    (set-auto-mode)
                    (font-lock-ensure))))))))
      (when (buffer-live-p buffer)
        (unless (get-buffer-window buffer)
          (with-minibuffer-selected-window
            (pop-to-buffer-same-window
             buffer)))))))

(defun fido-fd-read-date (&optional prompt default-time _history)
  "Read fdfind date options with PROMPT, DEFAULT-TIME and HISTORY."
  (let* ((actions '((?h "hours" "%dh")
                    (?m "minutes" "%dmin")
                    (?w "weeks" "%dweeks")
                    (?d "days" "%dd")
                    (?t "time")
                    (?o "other")
                    (?n "none")))
         (answer (read-multiple-choice "Type: " actions)))
    (if (nth 2 answer)
        (format (nth 2 answer)
                (read-number (format "%s " (nth 1 answer))))
      (pcase (car answer)
        (?t
         (read-string (require 'org)
                      (when (fboundp 'org-read-date)
                        (org-read-date t nil))))
        (?o (read-string (or prompt "Other: ") default-time))))))

(defun fido-fd-read-size (&rest _args)
  "Read fdfind size options."
  (let* ((actions '((?b "bytes" "b")
                    (?k "kilobytes" "k")
                    (?m "megabytes" "m")
                    (?g "gigabytes" "g")
                    (?t "terabytes" "t")
                    (?K "kibibytes" "ki")
                    (?G "gibibytes" "gi")
                    (?T "tebibytes" "ti")
                    (?n "none")))
         (answer (read-multiple-choice "Type: " actions)))
    (when (nth 2 answer)
      (let* ((value (concat (format "%d"
                                    (read-number (format "%s "
                                                         (nth 1 answer))))
                            (nth 2 answer)))
             (prefix (nth 2 (read-multiple-choice
                             ""
                             `((?g ,(format "greater then %s" value) "+")
                               (?l ,(format "less then %s" value) "-")
                               (?e ,(format "equal then %s" value) ""))))))
        (concat prefix value)))))



(defun fido-fd-get-flags ()
  "Return string with fd flags from `fido-fd-hydra-state'."
  (string-join
   fido-fd-args
   "\s"))


;;;###autoload
(defun fido-fd-read-flags ()
  "Invoke hydra to configure fd flags."
  (interactive)
  (if (active-minibuffer-window)
      (progn
        (setq fido-fd-last-input (minibuffer-contents-no-properties))
        (setq fido-fd-args fido-fd-args)
        (setq fido-fd-current-dir fido-fd-current-dir)
        (run-with-timer 0.2 nil #'fido-fd-transient)
        (abort-minibuffers))
    (fido-fd-transient)))

;;;###autoload
(defun fido-fd-find-directory-up ()
  "Change `fido-fd-current-dir' to parent directory."
  (interactive)
  (if (active-minibuffer-window)
      (progn (setq fido-fd-last-input (minibuffer-contents-no-properties))
             (fido-fd-delete-process)
             (run-with-timer 0.2 nil
                             (lambda nil
                                 (put 'quit 'error-message "Quit")
                                 (funcall-interactively #'fido-fd-async
                                                        (fido-fd-parent-dir
                                                         fido-fd-current-dir)
                                                        fido-fd-args
                                                        fido-fd-last-input)))
             (abort-minibuffers))
    (fido-fd-delete-process)
    (funcall-interactively #'fido-fd-async
                           (fido-fd-parent-dir fido-fd-current-dir)
                           fido-fd-args
                           fido-fd-last-input)))

(defun fido-fd-visit-dir (dir)
  "Change `fido-fd-current-dir' to DIR."
  (when (and dir
             (file-directory-p dir)
             (file-exists-p dir)
             (file-readable-p dir))
    (if
        (active-minibuffer-window)
        (progn
          (setq fido-fd-last-input (minibuffer-contents-no-properties))
          (fido-fd-async dir fido-fd-args
                         fido-fd-last-input)
          (abort-minibuffers))
      (fido-fd-async dir fido-fd-args
                     nil))))

(defun fido-fd-expand-file (filename)
  "Expand not absolute FILENAME to `fido-fd-current-dir'.
If FILENAME is absolute just return it."
  (if (and filename
           (file-name-absolute-p filename))
      filename
    (expand-file-name filename (or fido-fd-current-dir default-directory))))

;;;###autoload
(defun fido-fd-find-file-or-preview ()
  "Find or preview FILE."
  (interactive)
  (condition-case err
      (let ((content (minibuffer-contents-no-properties)))
        (when (and content)
          (setq content
                (fido-fd-expand-file (if (car-safe
                                          completion-all-sorted-completions)
                                         (car-safe
                                          completion-all-sorted-completions)
                                       content))))
        (when (and
               content
               (file-exists-p content)
               (file-readable-p content)
               (file-name-absolute-p content))
          (fido-fd-preview-file content)))
    (error
     (message "fido-fd-find-file-or-preview %s" err)
     (abort-minibuffers))))

;;;###autoload
(defun fido-fd-change-dir ()
  "Read directory for fd in minibuffer."
  (interactive)
  (if (active-minibuffer-window)
      (progn (setq fido-fd-last-input (minibuffer-contents-no-properties))
             (run-with-timer 0.2 nil
                             (lambda (input)
                               (let ((directory (read-directory-name
                                                 "Search in:\s")))
                                 (funcall-interactively
                                  #'fido-fd-async
                                  directory
                                  fido-fd-args
                                  input)))
                             fido-fd-last-input)
             (abort-minibuffers))
    (let ((directory (read-directory-name "Search in:\s")))
      (funcall-interactively #'fido-fd-async
                             directory
                             fido-fd-args
                             fido-fd-last-input))))

(defun fido-fd-make-shell-command ()
  "Return string fd shell command."
  (concat fido-fd-exec-path " -0 --color=never " (string-join fido-fd-args)
          (concat " %s ")))

;;;###autoload
(defun fido-fd-toggle-hidden ()
  "Toggle --hidden flag."
  (interactive)
  (let ((flag (if (vc-root-dir) "--no-ignore-vcs" "--hidden")))
    (setq fido-fd-args (if
                           (member flag fido-fd-args)
                           (delete flag fido-fd-args)
                         (push flag fido-fd-args))))
  (setq fido-fd-args (if
                         (member "--hidden" fido-fd-args)
                         (delete "--hidden" fido-fd-args)
                       (push "--hidden" fido-fd-args)))
  (if (active-minibuffer-window)
      (progn (setq fido-fd-last-input (minibuffer-contents-no-properties))
             (run-with-timer 0.2 nil
                             (lambda (input)
                               (fido-fd-async
                                fido-fd-current-dir
                                fido-fd-args
                                input))
                             fido-fd-last-input)
             (abort-minibuffers))
    (fido-fd-async
     (read-directory-name "Search in:\s")
     fido-fd-args
     fido-fd-last-input)))

(require 'transient)


(transient-define-argument fido-fd-read-file-ext ()
  "Read file type for fd."
  :argument "-e "
  :description "File extension"
  :multi-value 'repeat
  :class 'transient-option)

(transient-define-argument fido-fd-exclude-argument ()
  "Read file type for fd."
  :argument "--exclude "
  :shortarg "--E "
  :multi-value 'repeat
  :class 'transient-option)

;;;###autoload
(defun fido-fd-async-exit-and-run ()
  "Run `fido-fd-async' from `fido-fd-transient'."
  (interactive)
  (let ((args (transient-args transient-current-command)))
    (run-with-timer 0.5 nil 'fido-fd-async fido-fd-current-dir args
                    fido-fd-last-input)))

;;;###autoload
(defun fido-fd-save-switches ()
  "Save transient args to `fido-fd-switches'."
  (interactive)
  (let ((args (transient-args transient-current-command)))
    (add-to-list 'fido-fd-switches (cons
                                    (read-directory-name "Directory:"
                                                         (or
                                                          fido-fd-current-dir
                                                          default-directory))
                                    args))
    (when (yes-or-no-p (format "Save %s?" args))
      (customize-save-variable 'fido-fd-switches fido-fd-switches))))

(transient-define-argument fido-fd-ignore-type-argument ()
  "Argument for ignore switches."
  :class 'transient-switches
  :argument-format "%s"
  :argument-regexp
  "\\(--no-ignore\\)\\|\\(--no-ignore-parent\\)\\|\\(--no-ignore-vcs\\)"
  :choices '("--no-ignore-vcs" "--no-ignore" "--no-ignore-parent"))

(transient-define-argument fido-fd-file-type-argument ()
  "Argument for toggle directory or file."
  :class 'transient-switches
  :argument-format "%s"
  :argument-regexp "\\(--type=file\\)\\|\\(--type=directory\\)"
  :choices '("--type=file" "--type=directory"))

(transient-define-argument fido-fd-case-type-argument ()
  "Argument for toggle case options."
  :class 'transient-switches
  :argument-format "%s"
  :argument-regexp "\\(--ignore-case\\)\\|\\(--case-sensitive\\)"
  :choices '("--ignore-case" "--case-sensitive"))


;;;###autoload (autoload 'fido-fd-transient "fido-fd.el" nil t)
(transient-define-prefix fido-fd-transient ()
  "Fd 8.3.1.
USAGE:
fd [FLAGS/OPTIONS] [<pattern>] [<path>...]

FLAGS:"
  :value (lambda () fido-fd-args)
  :man-page "fdfind"
  ["Flags"
   ("H" " Search hidden files and directories" "--hidden")
   ("I" "Ignore options" fido-fd-ignore-type-argument)
   ("c" " Case-sensitive search " fido-fd-case-type-argument)
   ("g" " Glob-based search (default: regular expression)" "--glob")
   ("a" " Show absolute instead of relative" "--absolute-path")
   ("L" " Follow symbolic links" "--follow")
   ("p" "Search full abs" "--full-path")]
  ["File type"
   ("f" "File" fido-fd-file-type-argument)
   ("l" "Symlink" "--type=l")
   ("x" "Executable" "--type=x")
   ("e" "Empty" "--type=e")
   ("S" "Socket" "--type=s")
   ("P" "Pipe" "--type=p")]
  ["Depth"
   ("d" "Maximum search depth " "--max-depth "
    :class transient-option
    :reader transient-read-number-N0)
   ("=" "Exact search depth " "--exact-depth "
    :class transient-option
    :reader transient-read-number-N0)
   ("0" "Minimum search depth " "--min-depth "
    :class transient-option
    :reader transient-read-number-N0)]
  ["Options"
   ("-e" fido-fd-read-file-ext)
   ("E" "Exclude" fido-fd-exclude-argument)
   ("w" "Filter by file modification time (newer than) " "--changed-within "
    :class transient-option
    :reader fido-fd-read-date)
   ("b" "Filter by file modification time (older than) " "--changed-before "
    :class transient-option
    :reader fido-fd-read-date)
   ("s" "Size" "--size "
    :class transient-option
    :reader fido-fd-read-size)]
  ["Actions"
   ("RET" "Run" fido-fd-async-exit-and-run :transient nil)
   ("<return>" "Run" fido-fd-async-exit-and-run :transient nil)
   ("C-c C-s" "Save" fido-fd-save-switches)
   ("q" "Quit" transient-quit-all)])

;;;###autoload
(defun fido-fd-change-max-depth ()
  "Change --max-depth flag."
  (interactive)
  (read-string "Max-depth" ))


(defun fido-fd-preview-get-file ()
  "Return FILE from `minibuffer-contents'."
  (let ((content (minibuffer-contents)))
    (when (and content)
      (setq content
            (fido-fd-expand-file (if (car completion-all-sorted-completions)
                                     (car completion-all-sorted-completions)
                                   content))))
    (when (and
           content
           (file-exists-p content)
           (file-readable-p content)
           (file-name-absolute-p content))
      content)))



(defun fido-fd--find-file-other-window ()
  "Find file in other window and abort the current minibuffer.
File is detected from `minibuffer-contents'."
  (interactive)
  (when-let ((file (fido-fd-preview-get-file)))
    (run-with-timer 0.2 nil #'find-file-other-window file)
    (abort-minibuffers)))

(defun fido-fd--change-dir (dir)
  "Change fido directory DIR."
  (setq fido-fd-last-input nil)
  (fido-fd-delete-process)
  (run-with-timer 0.2 nil 'fido-fd-async
                  dir fido-fd-args fido-fd-last-input)
  (abort-minibuffers))

;;;###autoload
(defun fido-fd-home-dir ()
  "Change fd home dir."
  (interactive)
  (if (active-minibuffer-window)
      (fido-fd--change-dir (expand-file-name "~/"))
    (fido-fd-async (expand-file-name "~/"))))

;;;###autoload
(defun fido-fd-vc-dir ()
  "Change fd home dir."
  (interactive)
  (let ((proj (fido-fd-resolve-project-root)))
    (when (and proj
               (equal (expand-file-name proj)
                      fido-fd-current-dir)
               (fido-fd-parent-dir fido-fd-current-dir))
      (let ((default-directory (fido-fd-parent-dir fido-fd-current-dir)))
        (setq proj (or (fido-fd-resolve-project-root) proj))))
    (if (active-minibuffer-window)
        (fido-fd--change-dir (expand-file-name (or proj "~/")))
      (fido-fd-async (expand-file-name (or proj "~/"))))))

;;;###autoload
(defun fido-fd-abs-dir ()
  "Change fd home dir."
  (interactive)
  (add-to-list 'fido-fd-args "--exclude 'home'")
  (if (active-minibuffer-window)
      (fido-fd--change-dir (expand-file-name "/"))
    (fido-fd-async "/" fido-fd-args)))

(defvar fido-fd-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-<backspace>") #'fido-fd-find-directory-up)
    (define-key map (kbd "C-l") #'fido-fd-find-directory-up)
    (define-key map (kbd "C-x d")  #'fido-fd-change-dir)
    (define-key map (kbd "C-j") #'fido-fd-find-file-or-preview)
    (define-key map (kbd "C-.")  #'fido-fd-read-flags)
    (define-key map (kbd "C-M-.")  #'fido-fd-read-flags)
    (define-key map (kbd "C-c C-o") #'fido-fd--find-file-other-window)
    (define-key map (kbd "~") #'fido-fd-home-dir)
    (define-key map (kbd "M-.") #'fido-fd-toggle-hidden)
    (define-key map (kbd "C->") #'fido-fd-next-switch)
    (define-key map (kbd "M-<up>") #'fido-fd-change-max-depth)
    map))

(defun fido-fd-resolve-project-root ()
  "Resolve project root by searching git directory."
  (locate-dominating-file default-directory ".git" ))


(defun fido-fd-update-directory ()
  "Fp update directory."
  (when (eq this-command 'icomplete-fido-backward-updir)
    (remove-hook 'post-command-hook #'fido-fd-update-directory t)
    (fido-fd-find-directory-up)))

(defun fido-fd-async-minibuffer-setup ()
  "Hook function for `icomplete-minibuffer-setup-hook'."
  (when (minibufferp)
    (use-local-map (make-composed-keymap fido-fd-map (current-local-map)))
    (insert (or fido-fd-last-input ""))))

;;;###autoload
(defun fido-fd-async-resume ()
  "Search in DIRECTORY or `default-directory' with INPUT, ARGS and ACTION-FN.
Default value for ACTION is find file."
  (interactive)
  (fido-fd-async fido-fd-current-dir fido-fd-args fido-fd-last-input nil t))


;;;###autoload
(defun fido-fd-async (&optional directory args input action-fn resume)
  "Search in DIRECTORY or `default-directory' with INPUT, ARGS and ACTION-FN.
Default value for ACTION is find file."
  (interactive)
  (unless resume
    (setq fido-fd-current-dir (fido-fd-slash
                               (expand-file-name
                                (or directory
                                    default-directory))))
    (setq fido-fd-last-input input)
    (setq fido-fd-args (or args fido-fd-args))
    (setq fido-fd-cands nil)
    (setq fido-fd-async-command (concat fido-fd-exec-path " -0 --color=never "
                                        (string-join fido-fd-args " ")
                                        (concat " %s ")))
    (unless fido-fd-current-dir
      (setq fido-fd-current-dir default-directory)))
  (unwind-protect
      (let ((default-directory fido-fd-current-dir))
        (when (bound-and-true-p icomplete-mode)
          (add-hook 'icomplete-minibuffer-setup-hook
                    #'fido-fd-async-minibuffer-setup))
        (when (bound-and-true-p icomplete-vertical-mode)
          (add-hook 'icomplete--vertical-minibuffer-setup
                    #'fido-fd-async-minibuffer-setup))
        (unless (bound-and-true-p icomplete-mode)
          (add-hook 'minibuffer-setup-hook
                    #'fido-fd-async-minibuffer-setup))
        (let ((file (completing-read-default
                     (concat
                      "fd "
                      (string-join fido-fd-args " ")
                      " "
                      (abbreviate-file-name
                       fido-fd-current-dir)
                      ": ")
                     (lambda (string pred action)
                       (if (eq action 'metadata)
                           `(metadata
                             (category . file))
                         (complete-with-action
                          action
                          (if resume
                              fido-fd-cands
                            (fido-fd-async-function string))
                          string
                          pred)))
                     nil nil nil fido-fd-async-history)))
          (if action-fn
              (funcall action-fn (fido-fd-expand-file file))
            (find-file file))))
    (fido-fd-delete-process)
    (remove-hook 'icomplete-minibuffer-setup-hook
                 #'fido-fd-async-minibuffer-setup)
    (remove-hook 'icomplete--vertical-minibuffer-setup
                 #'fido-fd-async-minibuffer-setup)
    (remove-hook 'minibuffer-setup-hook
                 #'fido-fd-async-minibuffer-setup)))

(provide 'fido-fd)
;;; fido-fd.el ends here