;;; extra.el --- A collection of unrelated functions         -*- lexical-binding: t; -*-

;; Copyright (C) 2023 M. Rincón

;; Author: M. Rincón
;; Keywords: functions
;; Version: 0.1.0

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
(defvar extra-open-out-extensions (list "pdf" "epub" "xlsx" "docx" "pptx")
  "List of extensions to open externally.")

(defvar extra-open-out-cmnd (if (eq system-type 'darwin) "open" "xdg-open")
  "Command used to open a file externally.")

(defvar extra-surround-characters
  '(("<" . ">") ("(" . ")") ("{" . "}") ("[" . "]") ("\"" . "\"") ("~" . "~") ("=" . "="))
  "Default surround character.")

(defvar extra-search-cmnd "rg --color=auto -i -nH --no-heading --null"
  "Default command for grep searches.")

(defvar extra-search-exclude (list ".git" ".venv" ".mypy_cache" "__pycache__")
  "Default list of excluded directories.")

(defvar extra-alert-buffer "*Extra Alert*"
  "Default name for the alert buffer.")

(defvar extra-narrow-padding 10
  "Additional padding added to `fill-column` with `extra-narrow` mode.")

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
  (cond ((functionp isearch-regexp-function) (funcall isearch-regexp-function
                                                      isearch-string))
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
         (excld (mapconcat (lambda (d) (format " -not -path \"*/%s/*\"" d))
                           extra-search-exclude))
         (cmnd
          (format "find . -type f %s -exec grep --color=auto -nH --null -e %s \\{\\} +"
                  excld regexp)))
    (grep-find cmnd)))

;;;###autoload
(defun extra-search-to-dired ()
  "Search a string and start Dired on output."
  (interactive)
  (let* ((last (extra-getlast-regexp))
         (ask (if last (concat "Regex (Default: " last "):") "Search:"))
         (regexp (read-string ask nil nil last))
         (dir (read-directory-name "Directory:"))
         (excld (mapconcat (lambda (d) (format " -not -path \"*/%s/*\"" d))
                           extra-search-exclude)))
    (find-dired dir (concat "-type f " excld " -exec rg -q -e "
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
   "--git-dir=${HOME}/.conf.git/ --work-tree=${HOME} ls-files"
   "~"
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

;;; Alert
;; This code is mostly based on `appt.el`
(defun extra--select-lowest-window ()
  "Select the lowest window on the frame."
  (let ((lowest-window (selected-window))
        (bottom-edge (nth 3 (window-edges)))
        next-bottom-edge)
    (walk-windows (lambda (w)
                    (when (< bottom-edge (setq next-bottom-edge
                                               (nth 3 (window-edges w))))
                      (setq bottom-edge next-bottom-edge
                            lowest-window w))) 'nomini)
    (select-window lowest-window)))

;;;###autoload
(defun extra-alert (msg)
  "Create a window with the MSG."
  (let ((this-window (selected-window))
        (alert-window (get-buffer-create extra-alert-buffer)))
    (when (minibufferp)
      (other-window 1)
      (and (minibufferp) (display-multi-frame-p) (other-frame 1)))
    (if (cdr (assq 'unsplittable (frame-parameters)))
        ;; In an unsplittable frame, use something somewhere else.
        (progn
	  (set-buffer alert-window)
	  (display-buffer alert-window))
      (unless (or (special-display-p (buffer-name alert-window))
                  (same-window-p (buffer-name alert-window)))
        ;; By default, split the bottom window and use the lower part.
        (extra--select-lowest-window)
        ;; Split the window, unless it's too small to do so.
        (when (>= (window-height) (* 2 window-min-height))
          (select-window (split-window))))
      (switch-to-buffer alert-window))
    (setq buffer-read-only nil
          buffer-undo-list t)
    (erase-buffer)
    (insert "\n" msg)
    (center-paragraph)
    (shrink-window-if-larger-than-buffer (get-buffer-window alert-window t))
    (set-buffer-modified-p nil)
    (setq buffer-read-only t)
    (raise-frame)
    (select-window this-window)))

;;;###autoload
(defun extra-kill-alert ()
  "Close alert buffer."
  (interactive)
  (set-buffer extra-alert-buffer)
  (kill-this-buffer)
  (if (not (one-window-p))
        (delete-window)))

(defun extra--narrow (&optional pad)
  "Try to narrow frame to `fill-column` or by PAD on both sides.

To remove the fringe band when running in a GUI environment, set
a fringe face that matches the background. The same can be done
for the line number band."
  (let* ((window-configuration-change-hook nil)
         (fcolor (face-attribute 'default :background))
         (window (car (get-buffer-window-list (current-buffer) nil t)))
         (width (window-total-width window t))
         (new (if pad
                  pad
                (max (round (/ (- width (+ fill-column extra-narrow-padding)) 2)) 0))))
    (setq-local left-margin-width new)
    (setq-local right-margin-width new)
    (set-window-margins window new new)
    (set-face-attribute 'fringe nil :background fcolor)
    (set-face-attribute 'line-number nil :background fcolor)))

;;;###autoload
(define-minor-mode extra-narrow-mode
  "Try to narrow the frame to `fill-column` or revert a prior action.

To remove the fringe band when running in a GUI environment, set
a fringe face that matches the background. The same can be done
for the line number band."
  :init-value nil
  :lighter " extra-narrow-mode"
  (if extra-narrow-mode
      (extra--narrow)
      (add-hook 'window-configuration-change-hook 'extra--narrow 100 t)
    (progn (remove-hook 'window-configuration-change-hook 'extra--narrow t)
           (extra--narrow 0))))

(provide 'extra)
;;; extra.el ends here
