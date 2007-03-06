;;; riece-mixi.el --- Riece integration for mixi
;; Copyright (C) 2007 OHASHI Akira

;; Author: OHASHI Akira <bg66@koka-in.org>
;; Keywords: IRC, riece

;; This file is *NOT* part of Riece.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; NOTE: This is an add-on module for Riece.

;; If you have bug reports and/or suggestions for improvement, please
;; send them via <URL:http://mixi.jp/view_community.pl?id=1596390>.

;;; Code:

(require 'mixi)
(require 'mixi-utils)
(require 'timer)

;; Functions and variables which should be defined in the other module
;; at run-time.
(eval-when-compile
  (defvar riece-current-channels)
  (autoload 'riece-send-string "riece-server")
  (autoload 'riece-identity-prefix "riece-identity")
  (autoload 'riece-display-message "riece-message")
  (autoload 'riece-make-message "riece-message")
  (autoload 'riece-current-nickname "riece-misc")
  (autoload 'riece-message-own-p "riece-message")
  (autoload 'riece-message-text "riece-message")
  (autoload 'riece-message-target "riece-message")
  (autoload 'riece-parse-identity "riece-identity"))

(defgroup riece-mixi nil
  "Riece integration for mixi."
  :prefix "riece-"
  :group 'riece)

(defcustom riece-mixi-regexp "\\(https?://\\([^.]+.\\)?mixi.jp[^ ]+\\)"
  "*Pattern of string to retrieving to mixi."
  :type 'string
  :group 'riece-mixi)

(defcustom riece-mixi-reply-to-only-me nil
  "*If non-nil, reply to only my messages."
  :type 'boolean
  :group 'riece-mixi)

(defcustom riece-mixi-check-alist nil
  "*An alist for checking to detect new articles.
Each element looks like (CHANNEL . URL) or (CHANNEL . FUNCTION).
CHANNEL is a channel name.
URL is the URL for mixi access point of the channel.  If URL is friend's, get
his/her diaries as article.  If community's, get its BBSes.  If diary's or
BBS's, get its comments.
FUNCTION is the function which has one `range' argument and returns the list
of mixi object."
  :type '(repeat (cons :format "%v"
		       (string :tag "Channel")
		       (radio (string :tag "URL")
			      (function :tag "Other function"))))
  :group 'riece-mixi)

(defcustom riece-mixi-check-range 1
  "*The number of ranges that should be checked to detect new articles."
  :type 'integer
  :group 'riece-mixi)

(defcustom riece-mixi-timer-step 3600
  "*Seconds for checking to detect new articles."
  :type 'integer
  :group 'riece-mixi)

(defvar riece-mixi-timer nil)
(defvar riece-mixi-last-check nil)

(defconst riece-mixi-description
  "Riece integration for mixi.")

(defun riece-mixi-send-notice (target string)
  (riece-send-string
   (format "NOTICE %s :%s\r\n" (riece-identity-prefix target) string))
  (riece-display-message
   (riece-make-message (riece-current-nickname) target string 'notice)))

(defun riece-mixi-send-object (target object)
  (condition-case nil
      (let ((string (concat (mixi-make-title object t) " [AR]")))
	(riece-mixi-send-notice target string))
    (error nil)))

(defun riece-mixi-display-message-function (message)
  (when (and (get 'riece-mixi 'riece-addon-enabled)
	     (or (not riece-mixi-reply-to-only-me)
		 (and riece-mixi-reply-to-only-me
		      (riece-message-own-p message)))
	     (string-match riece-mixi-regexp (riece-message-text message)))
    (let* ((url (match-string 1 (riece-message-text message)))
	   (object (mixi-make-object-from-url url)))
      (when (mixi-object-p object)
	(let ((target (riece-message-target message)))
	  (riece-mixi-send-object target object))))))

(defun riece-mixi-send-object-with-url (target object)
  (condition-case nil
      (let ((string (mixi-make-title object t))
	    (url (mixi-make-url object)))
	(riece-mixi-send-notice target string)
	(riece-mixi-send-notice target url))
    (error nil)))

(defun riece-mixi-check ()
  "Check to detect new articles.
If they exist, send them as notice to the corresponding channel."
  (when (get 'riece-mixi 'riece-addon-enabled)
    (mapc (lambda (list)
	    (let ((target (riece-parse-identity (car list)))
		  (url-or-function (cdr list)))
	      (when (member target riece-current-channels)
		(let ((objects (mixi-make-objects url-or-function
						  riece-mixi-check-range)))
		  (mapc (lambda (object)
			  (when (mixi-parent-p object)
			    (let ((comments (mixi-get-comments
					     object riece-mixi-check-range)))
			      (mapc (lambda (comment)
				      (let ((time (mixi-object-time comment)))
					(when (mixi-time-less-p
					       riece-mixi-last-check time)
					  (riece-mixi-send-object-with-url
					   target comment))))
				    comments)))
			  (let ((time (mixi-object-time object)))
			    (when (mixi-time-less-p riece-mixi-last-check
						    time)
			      (riece-mixi-send-object-with-url target
							       object))))
			objects)))))
	  riece-mixi-check-alist)
    (setq riece-mixi-last-check (current-time))))

(defun riece-mixi-insinuate ()
  (add-hook 'riece-after-display-message-functions
	    'riece-mixi-display-message-function))

(defun riece-mixi-enable ()
  (when riece-mixi-check-alist
    (setq riece-mixi-timer
	  (run-at-time riece-mixi-timer-step riece-mixi-timer-step
		       'riece-mixi-check))
    (setq riece-mixi-last-check (current-time))))

(defun riece-mixi-disable ()
  (when (timerp riece-mixi-timer)
    (cancel-timer riece-mixi-timer)
    (setq riece-mixi-timer nil)))

(provide 'riece-mixi)

;;; riece-mixi.el ends here
