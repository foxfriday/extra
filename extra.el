;;; extra.el --- A collection of unrelated functions         -*- lexical-binding: t; -*-

;; Copyright (C) 2023 M. Rincón

;; Author: M. Rincón
;; Keywords: functions
;; Version: 0.0.2

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
;; This is a small collections of functions the author finds useful, but too small to
;; merit a dedicated library. Some offer just a small amount of the functionality or
;; larger library. Basically, this is a way for me to avoid installing a larger package
;; when I only intend to use one function.

;;; Code:
(defvar extra-open-out-extensions (list "pdf" "epub")
  "List of extensions to open externally.")

(defvar extra-open-out-cmnd (if (eq system-type 'darwin) (message "open") "xdg-open")
  "Command used to open a file externally.")

(defvar extra-surround-characters
  '(("<" . ">") ("(" . ")") ("{" . "}") ("[" . "]") ("\"" . "\"") ("~" . "~") ("=" . "="))
  "Default surround character.")

(defvar extra-search-cmnd "rg --color=auto -i -nH --no-heading --null"
  "Default command for grep searches.")

;;;###autoload
(defun extra-surround (&optional surr)
  "Surround selection with the string SURR on the left and right."
  (interactive)
  (let* ((surr (if surr surr (completing-read "Surround:" extra-surround-characters)))
         (match (assoc surr extra-surround-characters))
         (right (if match (cdr match) surr))
         (rgn (use-region-p))
         (bds (if rgn nil (bounds-of-thing-at-point 'symbol)))
         (start (if rgn (region-beginning) (car bds)))
         (end (if rgn (region-end) (cdr bds))))
    (goto-char start)
    (insert surr)
    (goto-char end)
    (forward-char 1)
    (insert-before-markers right)))

;;;###autoload
(defun extra-open-out (file &optional cmnd)
  "Execute command CMND on FILE."
  (let* ((ext (file-name-extension file))
         (app (cond (cmnd cmnd)
                    ((member ext extra-open-out-extensions) extra-open-out-cmnd)
                    (t nil)))
         (log-buffer (get-buffer-create "*Messages*")))
    (if app (make-process :name "ext-open"
                          :buffer log-buffer
                          :command (list app file)
                          :stderr log-buffer)
      (find-file file))
    app))

;;;###autoload
(defun extra-unfill-paragraph (&optional region)
  "From multi-line paragraph to a long single line in the REGION."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max))
        (emacs-lisp-docstring-fill-column t))
    (fill-paragraph nil region)))

;;;###autoload
(defun extra-toggle-line-numbering ()
  "Toggle line numbering between absolute and relative."
  (interactive)
  (cond ((eq display-line-numbers 'relative)
         (setq display-line-numbers nil))
        (display-line-numbers
         (setq display-line-numbers 'relative))
        (t
         (setq display-line-numbers t))))

(defun extra-switch-window (&optional direction)
  "Switch DIRECTION up, down, left, right or other."
  (cond ((string= direction "k")
         (windmove-up))
        ((string= direction "j")
         (windmove-down))
        ((string= direction "h")
         (windmove-left))
        ((string= direction "l")
         (windmove-right))
        (t (other-window 1))))

;;;###autoload
(defun extra-close-other-buffer (&optional direction)
  "Close buffer at DIRECTION up, down, left, or right."
  (interactive)
  (unless (one-window-p)
    (extra-switch-window direction)
    (kill-this-buffer)
    (if (not (one-window-p))
        (other-window 1))))

;;;###autoload
(defun extra-kill-other-buffer (&optional direction)
  "Kill window and its buffer at DIRECTION up, down, left, or right."
  (interactive)
  (unless (one-window-p)
    (extra-switch-window direction)
    (kill-this-buffer)
    (if (not (one-window-p))
        (delete-window))))

;;;###autoload
(defun extra-kill-buffers-in-mode (mode)
  "Closes all open buffers with MODE."
  (interactive)
  (mapc (lambda (buffer)
          (when (eq mode (buffer-local-value 'major-mode buffer))
            (kill-buffer buffer)))
        (buffer-list)))

;;;###autoload
(defun extra-open-in(dir regex)
  "Open a file in DIR matching the REGEX."
  (interactive)
  (let* ((files (list)))
    (dolist (f (directory-files-recursively dir regex))
      (push (cons (file-name-nondirectory f) f) files))
    (if files
        (find-file (cdr (assoc (completing-read "Open File" files) files)))
      (warn "No files found."))))

(defun extra-getlast-regexp ()
  "Find the last used regular expression."
  (require 'isearch)
  (cond ((functionp isearch-regexp-function) (funcall isearch-regexp-function isearch-string))
         (isearch-regexp-function (word-search-regexp isearch-string))
         (isearch-regexp isearch-string)
         (isearch-string (regexp-quote isearch-string))
         (t nil)))

;;;###autoload
(defun extra-search-dir (&optional dir last)
  "Show matches in DIR, by default use LAST search."
  (interactive)
  (require 'grep)
  (let* ((last (if last last (extra-getlast-regexp)))
         (ddir (if dir dir (locate-dominating-file default-directory ".git")))
         (ask (if last (concat "Regex (Default: " last "):") "Search:"))
         (regexp (read-string ask nil nil last))
         (dir (read-directory-name "Directory: " ddir))
         (default-directory dir)
         (cmnd (format "find . -type f -exec grep --color=auto -nH --null -e %s \\{\\} +" regexp)))
    (grep-find cmnd)))

;;;###autoload
(defun extra-search-to-dired ()
  "Search a string and start Dired on output."
  (interactive)
  (let* ((last (extra-getlast-regexp))
         (ask (if last (concat "Regex (Default: " last "):") "Search:"))
         (regexp (read-string ask nil nil last))
         (dir (read-directory-name "Directory:")))
    (find-dired dir (concat "-type f -not -path '*/.git*' -exec rg -q -e "
		            (shell-quote-argument regexp) " "
		            (shell-quote-argument "{}") " "
		            (shell-quote-argument ";")))))

;;; Dired + Git
(defun extra-dired-git (flags dir bfr)
  "Show a `dired` buffer with files in DIR using git FLAGS named BFR."
  (switch-to-buffer (get-buffer-create bfr))
  (cd dir)
  (shell-command (concat "git " flags " | xargs ls -lah") (current-buffer))
  (dired-mode dir)
  (set (make-local-variable 'dired-subdir-alist)
       (list (cons default-directory (point-min-marker)))))

;;;###autoload
(defun extra-dired-untracked (dir)
  "Show a `dired` buffer with untracked files in DIR."
  (interactive "DUntracked in directory: ")
  (extra-dired-git "ls-files --others" dir "*untracked*"))

;;;###autoload
(defun extra-dired-tracked (dir)
  "Show a `dired` buffer with tracked files in DIR."
  (interactive "DUntracked in directory: ")
  (extra-dired-git "ls-files" dir "*tracked*"))

;;;###autoload
(defun extra-dired-conf ()
  "Show a `dired` buffer with tracked configuration files."
  (interactive)
  (extra-dired-git
   "--git-dir=${HOME}/.dots.git/ --work-tree=${HOME} ls-files" "~"
   "*tracked*"))

;;;###autoload
(defun extra-print-to-pdf ()
  "Print current buffer to PDF."
  (interactive)
  (let* ((bname (file-name-nondirectory (buffer-file-name)))
         (fname (file-name-sans-extension bname))
         (temp-ps (expand-file-name (concat "~/Downloads/" fname ".ps")))
         (out-pdf (expand-file-name (concat "~/Downloads/" fname ".pdf"))))
    (ps-print-buffer-with-faces temp-ps)
    (shell-command (concat "ps2pdf " temp-ps " " out-pdf))
    (delete-file temp-ps)
    (message (concat "Saved " out-pdf))))

;;;###autoload
(defun extra-narrow ()
  "Try to narrow the frame to `fill-column` or revert a prior action.

To remove the fringe band when running in a GUI environment, set
a fringe face that matches the background. The same can be done
for the line number band."
  (interactive)
  (let* ((window (car (get-buffer-window-list (current-buffer) nil t)))
         (width (window-total-width window t))
         (frame (max (round (/ (- width (+ fill-column 2)) 2)) 0))
         (now (window-margins window))
         (new (cond ((not (cdr now)) frame)
                    ((= frame (car now) (cdr now)) 0)
                    (t frame))))
    (set-window-margins window new new)))

;;;###autoload
(defun extra-sudoedit (&optional arg)
  "Edit buffer as root, with ARG reopen current buffer."
  (interactive "p")
  (if (and (= 4 arg) buffer-file-name)
      (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))
    (find-file (concat "/sudo:root@localhost:"
                       (expand-file-name (read-file-name "File: "))))))

;;;###autoload
(defun extra-format-buffer (formatter)
  "Format the current buffer using the FORMATTER."
  (interactive)
  (let* ((input-buffer (buffer-name))
         (in-point (point))
         (proj (locate-dominating-file default-directory ".git"))
         (default-directory (if proj proj default-directory))
         (temp-file (make-temp-file "format"))
         (log-buffer (get-buffer-create "*format-log*"))
         (out-buffer (get-buffer-create " *format-output*" t))
         (exit-code (call-process-region nil nil formatter nil (list out-buffer temp-file) nil "-")))
    (with-current-buffer log-buffer
      (insert-file-contents temp-file))
    (unless (zerop exit-code)
      (kill-buffer out-buffer)
      (error "Formatting failed with error code %s" exit-code))
    (with-current-buffer out-buffer
      (copy-to-buffer input-buffer (point-min) (point-max)))
    (kill-buffer out-buffer)
    (goto-char (min in-point (point-max)))))

(provide 'extra)
;;; extra.el ends here
