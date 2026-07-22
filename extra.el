;;; extra.el --- A collection of unrelated functions         -*- lexical-binding: t; -*-

;; Copyright (C) 2024 M. Rincón

;; Author: M. Rincón
;; Keywords: functions
;; Version: 0.1.4

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
  '(("<" . ">") ("(" . ")") ("{" . "}") ("[" . "]") ("\"" . "\"") ("~" . "~")
    ("=" . "=") ("*" . "*") ("_" . "_") ("'" . "'") (":" . ":"))
  "Default surround character.")

(defvar extra-search-exclude (list ".git" ".venv" ".mypy_cache" "__pycache__" ".pytest_cache")
  "Default list of excluded directories.")

(defvar extra-format-mode-cmd
  (list (cons 'python-ts-mode "ruff format -")
        (cons 'bash-ts-mode "shfmt -ln bash -i 4 -ci"))
  "Format command associated with a mode.")

(defvar extra-narrow-padding 10
  "Additional padding added to `fill-column' with `extra-narrow-mode'.")

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
    (unless start (user-error "No region or symbol at point"))
    (let ((end-marker (copy-marker end)))
      (goto-char start)
      (insert surr)
      (goto-char end-marker)
      (insert right))))

;;;###autoload
(defun extra-open-out (file &optional cmnd)
  "Execute command CMND on FILE."
  (let* ((ext (file-name-extension file))
         (app (cond (cmnd cmnd)
                    ((and ext (member (downcase ext) extra-open-out-extensions))
                     extra-open-out-cmnd)
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
(defun extra-close-other-buffer (&optional direction kill)
  "Close or KILL buffer at DIRECTION up (k), down (j), left (h), or right (l)."
  (interactive)
  (unless (one-window-p)
    (extra-switch-window direction)
    (kill-buffer)
    (if (not (one-window-p))
        (if kill (delete-window) (other-window 1)))))

;;;###autoload
(defun extra-kill-buffers-in-mode (mode)
  "Closes all open buffers with MODE."
  (mapc (lambda (buffer)
          (when (eq mode (buffer-local-value 'major-mode buffer))
            (kill-buffer buffer)))
        (buffer-list)))

;;;###autoload
(defun extra-search-dir (&optional dir rgx)
  "Show matches in DIR, by default use RGX search."
  (interactive)
  (require 'grep)
  (let* ((regexp (if rgx rgx (read-string "Regex:" nil nil)))
         (default-directory (if dir dir (read-directory-name "Directory:")))
         (excld (mapconcat (lambda (d) (format " -not -path \"*/%s/*\"" d))
                           extra-search-exclude ""))
         (cmnd (format "find . -type f %s -exec grep --color=auto -nH --null -e %s \\{\\} +"
                       excld
                       (shell-quote-argument regexp))))
    (grep-find cmnd)))

;;;###autoload
(defun extra-search-to-dired (&optional dir rgx)
  "Search RGX in DIR and show matches in Dired."
  (interactive)
  (let* ((regexp (if rgx rgx (read-string "Regex:" nil nil)))
         (dir (if dir dir (read-directory-name "Directory:")))
         (excld (mapconcat (lambda (d) (format " -not -path \"*/%s/*\"" d))
                           extra-search-exclude "")))
    (find-dired dir (concat "-type f " excld " -exec rg -q -e "
		            (shell-quote-argument regexp) " "
		            (shell-quote-argument "{}") " "
		            (shell-quote-argument ";")))))

;;; Dired + Git
(defun extra-dired-git (flags dir bfr)
  "Show a `dired` buffer with files in DIR using git FLAGS named BFR."
  (switch-to-buffer (get-buffer-create bfr))
  (cd dir)
  (shell-command (concat "git " flags " -z | xargs -0 -r ls -lah") (current-buffer))
  (dired-mode dir)
  (setq-local dired-subdir-alist
              (list (cons default-directory (point-min-marker)))))

;;;###autoload
(defun extra-dired-untracked (dir)
  "Show a `dired` buffer with untracked files in DIR."
  (interactive "DUntracked in directory: ")
  (extra-dired-git "ls-files --others" dir "*untracked*"))

;;;###autoload
(defun extra-dired-tracked (dir)
  "Show a `dired` buffer with tracked files in DIR."
  (interactive "DTracked in directory: ")
  (extra-dired-git "ls-files" dir "*tracked*"))

;;;###autoload
(defun extra-print-buffer (file &optional landscape open)
  "Write the current buffer to a FILE and OPEN the file.

If LANDSCAPE is not nil, print PDF and PS files in landscape mode.
The file extension can be plain text (txt), PDF or Postscript (ps).
If OPEN is not nil, open the file afterwards."
  (interactive "FWrite buffer to file: \nP")
  (if (or (not (file-writable-p file))
	  (and (file-exists-p file)
	       (if (called-interactively-p 'any)
		   (not (y-or-n-p (format "Overwrite existing file %s? " file))))))
      (error "Cannot write to file %s" file))
  (save-excursion
    (save-window-excursion
      (let ((bs (copy-sequence (buffer-string)))
            (bn (concat (buffer-name) "_print"))
	    (extension (file-name-extension file))
	    (default-directory (file-name-directory file))
            (ps-print-header nil)
            (ps-landscape-mode landscape))
	(with-temp-buffer
	  (rename-buffer bn t)
	  (set-buffer-modified-p nil)
	  (insert bs)
	  (cond
	   ((string= "ps" extension)
	    (require 'ps-print)
	    (ps-print-buffer-with-faces file)
	    (message "Postscript written to %s" file))
	   ((string= "pdf" extension)
	    (require 'ps-print)
	    (ps-print-buffer-with-faces
	     (concat (file-name-sans-extension file) ".ps"))
	    (call-process "ps2pdf" nil nil nil
			  (expand-file-name
			   (concat (file-name-sans-extension file) ".ps"))
			  (expand-file-name file))
	    (delete-file (concat (file-name-sans-extension file) ".ps"))
	    (message "PDF written to %s" file))
	   (t
            (write-region nil nil file)
            (message "Plain text written to %s" file)))))))
  (when open (find-file file)))

;;;###autoload
(defun extra-sudoedit (&optional arg)
  "Edit buffer as root, with ARG reopen current buffer."
  (interactive "p")
  (if (and (= 4 arg) buffer-file-name)
      (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))
    (find-file (concat "/sudo:root@localhost:"
                       (expand-file-name (read-file-name "File: "))))))

;;;###autoload
(defun extra-format-buffer (&optional formatter)
  "Format the current buffer using the FORMATTER command."
  (interactive)
  (let* ((fmt (if formatter
                  formatter
                (cdr (assoc major-mode extra-format-mode-cmd))))
         (input-buffer (buffer-name))
         (in-point (point))
         (proj (locate-dominating-file default-directory ".git"))
         (default-directory (if proj proj default-directory))
         (log-buffer (get-buffer-create "*extra-format-log*"))
         (out-buffer (get-buffer-create "*extra-format-tmp*" t))
         (exit-code))
    (if fmt
        (setq exit-code (shell-command-on-region (point-min)
                                                 (point-max)
                                                 fmt
                                                 out-buffer
                                                 nil
                                                 log-buffer))
      (error "There's no default format command for the current mode"))
    (unless (zerop exit-code)
      (kill-buffer out-buffer)
      (error "Formatting failed with error code %s" exit-code))
    (with-current-buffer out-buffer
      (copy-to-buffer input-buffer (point-min) (point-max)))
    (kill-buffer out-buffer)
    (if (= (buffer-size log-buffer) 0) (kill-buffer log-buffer))
    (goto-char (min in-point (point-max)))))

;;; Narrow mode
(defun extra--narrow (&optional pad)
  "Try to narrow frame to `fill-column' or by PAD on both sides.

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
  "Try to narrow the frame to `fill-column' or revert a prior action.

To remove the fringe band when running in a GUI environment, set
a fringe face that matches the background. The same can be done
for the line number band."
  :init-value nil
  :lighter " Narrow"
  (if extra-narrow-mode
      (progn
        (extra--narrow)
        (add-hook 'window-configuration-change-hook 'extra--narrow 100 t))
    (remove-hook 'window-configuration-change-hook 'extra--narrow t)
    (extra--narrow 0)))

(provide 'extra)
;;; extra.el ends here
