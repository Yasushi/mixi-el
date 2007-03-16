;; mixi-ticker.el --- ticker for mixi

;; Copyright (C) 2007 OHASHI Akira

;; Author: OHASHI Akira <bg66@koka-in.org>
;; Keywords: hypermedia

;; This file is *NOT* a part of Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, you can either send email to this
;; program's maintainer or write to: The Free Software Foundation,
;; Inc.; 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.

;;; Commentary:

;; To use, add the following line to your ~/.emacs:
;;
;; (autoload 'mixi-ticker-start "mixi-ticker")
;;
;; then M-x mixi-ticker-start will start mixi-ticker.

;; Bug reports:
;;
;; If you have bug reports and/or suggestions for improvement, please
;; send them via <URL:http://mixi.jp/view_community.pl?id=1596390>.

;;; Code:

(require 'mixi)
(require 'mixi-utils)
(require 'timer)

(defcustom mixi-ticker-check-list nil
  "*A list for checking to detect new articles.
Each element looks like URL or FUNCTION.
URL is the URL for mixi access point of the channel.  If URL is friend's, get
his/her diaries as article.  If community's, get its BBSes.  If diary's or
BBS's, get its comments.
FUNCTION is the function which has one `range' argument and returns the list
of mixi object."
  :type '(repeat (radio (string :tag "URL")
			(const :tag "New diaries" mixi-get-new-diaries)
			(const :tag "New comments" mixi-get-new-comments)
			(const :tag "New BBSes" mixi-get-new-bbses)
			(const :tag "Messages" mixi-get-messages)
			(const :tag "Logs" mixi-get-logs)
			(function :tag "Other function")))
  :group 'mixi)

(defcustom mixi-ticker-check-range 3
  "*The number of ranges that should be checked to detect new articles."
  :type 'integer
  :group 'mixi)

(defcustom mixi-ticker-interval 3600
  "*Time interval for checking to detect new articles."
  :type 'integer
  :group 'mixi)

(defcustom mixi-ticker-display-interval 0.3
  "*Time interval for displaying new articles."
  :type 'number
  :group 'mixi)

(defvar mixi-ticker-timer nil)
(defvar mixi-ticker-display-timer nil)
(defvar mixi-ticker-objects nil)
(defvar mixi-ticker-message nil)
(defvar mixi-ticker-last-check nil)

(defun mixi-ticker-display ()
  (when (not (or (active-minibuffer-window)
                 (and (current-message)
                      (not (string= (current-message)
                                    mixi-ticker-message)))))
    (if (and (stringp mixi-ticker-message)
	     (> (length mixi-ticker-message) 1))
	(setq mixi-ticker-message
	      (substring mixi-ticker-message 1))
      (setq mixi-ticker-message nil))
    (unless mixi-ticker-message
      (while mixi-ticker-objects
	(condition-case nil
	    (let ((string (mixi-make-title (car mixi-ticker-objects) t)))
	      (setq mixi-ticker-message (concat mixi-ticker-message " "
						(mixi-message string))))
	  (error nil))
	(setq mixi-ticker-objects (cdr mixi-ticker-objects))))
    (message mixi-ticker-message)))

(defun mixi-ticker-check ()
  "Check to detect new articles."
  (setq mixi-ticker-objects nil)
  (mapc (lambda (url-or-function)
	  (let ((objects (mixi-make-objects url-or-function
					    mixi-ticker-check-range)))
	    (while objects
	      (let ((object (car objects)))
		(when (mixi-parent-p object)
		  (let ((comments (mixi-get-comments
				   object mixi-ticker-check-range)))
		    (while comments
		      (let ((time (mixi-object-time (car comments))))
			(when (mixi-time-less-p mixi-ticker-last-check time)
			  (setq mixi-ticker-objects
				(cons (car comments) mixi-ticker-objects))))
		      (setq comments (cdr comments)))))
		(let ((time (mixi-object-time object)))
		  (when (mixi-time-less-p mixi-ticker-last-check time)
		    (setq mixi-ticker-objects (cons object
						    mixi-ticker-objects)))))
	      (setq objects (cdr objects)))))
	mixi-ticker-check-list)
  (setq mixi-ticker-objects (reverse mixi-ticker-objects))
  (setq mixi-ticker-last-check (current-time)))

;;;###autoload
(defun mixi-ticker-start ()
  (interactive)
  (when mixi-ticker-check-list
    (setq mixi-ticker-timer
	  (run-at-time mixi-ticker-interval mixi-ticker-interval
		       'mixi-ticker-check))
    (setq mixi-ticker-display-timer
	  (run-at-time mixi-ticker-display-interval
		       mixi-ticker-display-interval
		       'mixi-ticker-display))
    (unless mixi-ticker-last-check
      (setq mixi-ticker-last-check (current-time)))))

;;;###autoload
(defun mixi-ticker-stop ()
  (interactive)
  (when (timerp mixi-ticker-timer)
    (cancel-timer mixi-ticker-timer)
    (setq mixi-ticker-timer nil))
  (when (timerp mixi-ticker-display-timer)
    (cancel-timer mixi-ticker-display-timer)
    (setq mixi-ticker-display-timer nil)))

(provide 'mixi-ticker)

;;; mixi-ticker.el ends here
