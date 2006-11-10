;;; sb-mixi.el --- shimbun backend for mixi

;; Copyright (C) 2006 OHASHI Akira

;; Author: OHASHI Akira <bg66@koka-in.org>
;; Keywords: news

;; This file is *NOT* a part of shimbun.

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

;;; Code:

(require 'mixi)
(require 'shimbun)

(luna-define-class shimbun-mixi (shimbun) ())

(defcustom shimbun-mixi-group-alist '(("new-diaries" . mixi-get-new-diaries)
				      ("new-comments" . mixi-get-new-comments)
				      ("new-topics" . mixi-get-new-topics)
				      ("my-diaries" . "/home.pl"))
  "*An alist of mixi shimbun group definition.
Each element looks like (NAME . URL) or (NAME . FUNCTION).
NAME is a shimbun group name.
URL is the URL for mixi access point of the group.  When URL is friend's, get
his/her diaries as article.  When community's, get its topics.  When diary's
or topic's, get its comments.
FUNCTION is the function for getting articles."
  :group 'shimbun
  :type '(repeat (cons :fromat "%v"
		       (string :tag "Group name")
		       (radio (string :tag "URL")
			      (const :tag "New diaries" mixi-get-new-diaries)
			      (const :tag "New comments" mixi-get-new-comments)
			      (const :tag "New topics" mixi-get-new-topics)
			      (function :tag "Other function")))))

(defcustom shimbun-mixi-page-articles 10
  "*How many articles are there in one page."
  :group 'shimbun
  :type 'integer)

(luna-define-method shimbun-groups ((shimbun shimbun-mixi))
  (mapcar 'car shimbun-mixi-group-alist))

(defun shimbun-mixi-make-subject (object)
  (let ((class (mixi-object-class object)))
    (if (eq class 'mixi-comment)
	(concat "Re: " (mixi-object-title (mixi-comment-parent object)))
      (mixi-object-title object))))

(defun shimbun-mixi-make-from (object)
  (let ((owner (mixi-object-owner object)))
    (mixi-friend-nick owner)))

(defun shimbun-mixi-make-date (object)
  (let* ((time (mixi-object-time object))
	 (cts (current-time-string time))
	 (day-of-week (substring cts 0 3))
	 (month (substring cts 4 7)))
    (concat day-of-week ", "
	    (format-time-string "%d" time) " "
	    month " "
	    (format-time-string "%Y %H:%M:%S %z" time))))

(defun shimbun-mixi-make-message-id (object)
  (let ((class (mixi-object-class object)))
    (concat "<"
	    (format-time-string "%Y%m%d%H%M" (mixi-object-time object)) "."
	    (if (eq class 'mixi-comment)
		(concat (mixi-friend-id (mixi-comment-owner object)) "@"
			(mixi-object-id (mixi-comment-parent object)) "."
			(mixi-friend-id (mixi-object-owner
					 (mixi-comment-parent object))) ".")
	      (concat (mixi-object-id object) "@"
		      (mixi-object-id (mixi-object-owner object)) "."))
	    (mixi-object-name object) ".mixi.jp"
	    ">")))

(defun shimbun-mixi-make-xref (object)
  (let ((class (mixi-object-class object)))
    (cond ((eq class 'mixi-diary)
	   (mixi-expand-url (mixi-diary-page object)))
	  ((eq class 'mixi-topic)
	   (mixi-expand-url (mixi-topic-page object)))
	  ((eq class 'mixi-comment)
	   (concat (shimbun-mixi-make-xref (mixi-comment-parent object))
		   "#comment")))))

(defun shimbun-mixi-get-headers (shimbun objects &optional range)
  (when objects
    (let (headers)
      (catch 'stop
	(mapc (lambda (object)
		(when (mixi-object-p object)
		  (let ((class (mixi-object-class object))
			(id (shimbun-mixi-make-message-id object)))
		    (when (and (eq class 'mixi-comment)
			       (shimbun-search-id shimbun id))
		      (throw 'stop nil))
		    (push
		     (shimbun-create-header
		      0
		      (shimbun-mixi-make-subject object)
		      (shimbun-mixi-make-from object)
		      (shimbun-mixi-make-date object)
		      id
		      (if (eq class 'mixi-comment)
			  (shimbun-mixi-make-message-id
			   (mixi-comment-parent object))
			"")
		      0 0
		      (shimbun-mixi-make-xref object))
		     headers)
		    (when (or (eq class 'mixi-diary)
			      (eq class 'mixi-topic))
		      (let ((comments (mixi-get-comments object range)))
			(mapc (lambda (header)
				(push header headers))
			      (shimbun-mixi-get-headers shimbun
							comments)))))))
	      objects))
      headers)))

(luna-define-method shimbun-get-headers ((shimbun shimbun-mixi)
					 &optional range)
  (let ((url-or-function (cdr (assoc (shimbun-current-group-internal shimbun)
				     shimbun-mixi-group-alist)))
	(range (when (integerp range) (* range shimbun-mixi-page-articles)))
	objects)
    (if (stringp url-or-function)
	(let* ((object (mixi-make-object-from-url url-or-function))
	       (class (mixi-object-class object)))
	  (cond ((eq class 'mixi-friend)
		 (setq objects (mixi-get-diaries object range)))
		((eq class 'mixi-community)
		 (setq objects (mixi-get-topics object range)))
		((or (eq class 'mixi-diary) (eq class 'mixi-topic))
		 (setq objects (mixi-get-comments object range)))
		(t (error (concat (symbol-name class)
				  " is not supported yet.")))))
      (when (fboundp url-or-function)
	(setq objects (funcall url-or-function range))))
    (shimbun-sort-headers (shimbun-mixi-get-headers shimbun objects range))))

(defun shimbun-comment-article (url header)
  (let ((parent (mixi-make-object-from-url url))
	(date (shimbun-header-date header))
	(from (shimbun-header-from header)))
    (catch 'found
      (mapc (lambda (comment)
	      (let ((nick (mixi-friend-nick (mixi-comment-owner comment)))
		    (time (shimbun-mixi-make-date comment))
		    nick2)
		;; FIXME: How tricky it is.
		(when (string-match "\\(.+\\)����$" nick)
		  (setq nick2 (match-string 1 nick)))
		(when (and
		       (or (string= (shimbun-mime-encode-string nick) from)
			   (string= (shimbun-mime-encode-string nick2) from))
		       (string= time date))
		  ;; FIXME: Concat parent's information?
		  (throw 'found (mixi-comment-content comment)))))
	    ;; FIXME: Limit range?
	    (mixi-get-comments parent)))))

(luna-define-method shimbun-article ((shimbun shimbun-mixi)
				     header &optional outbuf)
  (when (shimbun-current-group-internal shimbun)
    (with-current-buffer (or outbuf (current-buffer))
      (w3m-insert-string
       (or (with-temp-buffer
	     (let* ((url (shimbun-article-url shimbun header))
		    (article (if (string-match "#comment$" url)
				 (shimbun-comment-article url header)
			       ;; FIXME: Concat community information?
			       (mixi-object-content
				(mixi-make-object-from-url url)))))
	       (when (stringp article)
		 (insert article)))
	     (shimbun-message shimbun "shimbun: Make contents...")
	     (goto-char (point-min))
	     (prog1 (shimbun-make-contents shimbun header)
	       (shimbun-message shimbun "shimbun: Make contents...done")))
	   "")))))

(provide 'sb-mixi)

;;; sb-mixi.el ends here