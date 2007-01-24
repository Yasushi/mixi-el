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

;; If you have bug reports and/or suggestions for improvement, please
;; send them via <URL:http://mixi.jp/view_community.pl?id=1596390>.

;;; Code:

(require 'mixi)
(require 'shimbun)

(eval-and-compile
  (luna-define-class shimbun-mixi (shimbun) (comment-cache))
  (luna-define-internal-accessors 'shimbun-mixi))

(defcustom shimbun-mixi-group-alist
  '(("new-diaries" . mixi-get-new-diaries)
    ("new-comments" . mixi-get-new-comments)
    ("new-bbses" . mixi-get-new-bbses)
    ("messages" . mixi-get-messages)
    ("messages.sent" .
     (lambda (range)
       (mixi-get-messages 'outbox range)))
    ("my-diaries" . "/home.pl")
    ("mixi-el" . "/view_community.pl?id=1596390")
    ("news.newest.domestic" .
     (lambda (range)
       (mixi-get-news 'domestic 'newest range)))
    ("news.newest.politics" .
     (lambda (range)
       (mixi-get-news 'politics 'newest range)))
    ("news.newest.economy" .
     (lambda (range)
       (mixi-get-news 'economy 'newest range)))
    ("news.newest.area" .
     (lambda (range)
       (mixi-get-news 'area 'newest range)))
    ("news.newest.abroad" .
     (lambda (range)
       (mixi-get-news 'abroad 'newest range)))
    ("news.newest.sports" .
     (lambda (range)
       (mixi-get-news 'sports 'newest range)))
    ("news.newest.entertainment" .
     (lambda (range)
       (mixi-get-news 'entertainment 'newest range)))
    ("news.newest.it" .
     (lambda (range)
       (mixi-get-news 'IT 'newest range)))
    ("news.pickup.domestic" .
     (lambda (range)
       (mixi-get-news 'domestic 'pickup range)))
    ("news.pickup.politics" .
     (lambda (range)
       (mixi-get-news 'politics 'pickup range)))
    ("news.pickup.economy" .
     (lambda (range)
       (mixi-get-news 'economy 'pickup range)))
    ("news.pickup.area" .
     (lambda (range)
       (mixi-get-news 'area 'pickup range)))
    ("news.pickup.abroad" .
     (lambda (range)
       (mixi-get-news 'abroad 'pickup range)))
    ("news.pickup.sports" .
     (lambda (range)
       (mixi-get-news 'sports 'pickup range)))
    ("news.pickup.entertainment" .
     (lambda (range)
       (mixi-get-news 'entertainment 'pickup range)))
    ("news.pickup.it" .
     (lambda (range)
       (mixi-get-news 'IT 'pickup range))))
  "*An alist of mixi shimbun group definition.
Each element looks like (NAME . URL) or (NAME . FUNCTION).
NAME is a shimbun group name.
URL is the URL for mixi access point of the group.  If URL is friend's, get
his/her diaries as article.  If community's, get its BBSes.  If diary's or
BBS's, get its comments.
FUNCTION is the function which has one `range' argument and returns the list
of mixi object."
  :group 'shimbun
  :type '(repeat (cons :fromat "%v"
		       (string :tag "Group name")
		       (radio (string :tag "URL")
			      (const :tag "New diaries" mixi-get-new-diaries)
			      (const :tag "New comments" mixi-get-new-comments)
			      (const :tag "New BBSes" mixi-get-new-bbses)
			      (const :tag "Messages" mixi-get-messages)
			      (function :tag "Other function")))))

;; FIXME: Don't use this user option.
(defcustom shimbun-mixi-page-articles 10
  "*How many articles are there in one page."
  :group 'shimbun
  :type 'integer)

(defvar shimbun-mixi-x-face-alist
  '(("default" . "X-Face: CY;j#FoBnpK^37`-IoJvN!J^u;GciiPmMQ@T)~RP1]t8iv?v)/bVI:I\"F!JfWJvhM5{zY!=
 h.d+'g\\I{D>Ocy?Rc4uYUyOZj2%2Kl>,x-!MCSsyi3!L}psrrC1jlF,O?Ui>qf)X;sBz`/}\\066X%$
 siG'|4K!2?==|oB&#E'5GGH\\#z[muyQ")))

(defvar shimbun-mixi-reply-to nil)

(luna-define-method initialize-instance :after ((shimbun shimbun-mixi)
						&rest init-args)
  (shimbun-mixi-set-comment-cache-internal shimbun
					   (make-hash-table :test 'equal))
  shimbun)

(luna-define-method shimbun-close :after ((shimbun shimbun-mixi))
  (shimbun-mixi-set-comment-cache-internal shimbun nil)
  (mixi-logout))

(luna-define-method shimbun-groups ((shimbun shimbun-mixi))
  (mapcar 'car shimbun-mixi-group-alist))

(luna-define-method shimbun-reply-to ((shimbun shimbun-mixi))
  shimbun-mixi-reply-to)

(defun shimbun-mixi-make-subject (shimbun object)
  (let ((class (mixi-object-class object)))
    (if (eq class 'mixi-comment)
        (concat "Re: " (shimbun-mixi-make-subject
			shimbun (mixi-comment-parent object)))
      (let ((prefix (when (eq class 'mixi-event) "[イベント]"))
	    (subject (mixi-object-title object))
	    (suffix (when (string-match
			   "^new-" (shimbun-current-group-internal shimbun))
		      (concat " ("
			      (if (eq class 'mixi-diary)
				  (mixi-friend-nick
				   (mixi-diary-owner object))
				(mixi-community-name
				 (mixi-bbs-community object)))
			      ")"))))
	(concat prefix subject suffix)))))

(defun shimbun-mixi-make-from (object)
  (let ((class (mixi-object-class object)))
    (if (eq class 'mixi-news)
	(mixi-news-media object)
      (let ((owner (mixi-object-owner object)))
	(mixi-friend-nick owner)))))

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
		      (if (eq class 'mixi-news)
			  (mixi-news-media-id object)
			(mixi-object-id (mixi-object-owner object))) "."))
	    (mixi-object-name object) ".mixi.jp"
	    ">")))

(defun shimbun-mixi-make-xref (object)
  (let ((class (mixi-object-class object)))
    (cond ((eq class 'mixi-diary)
	   (mixi-expand-url (mixi-diary-page object)))
	  ((eq class 'mixi-topic)
	   (mixi-expand-url (mixi-topic-page object)))
	  ((eq class 'mixi-event)
	   (mixi-expand-url (mixi-event-page object)))
	  ((eq class 'mixi-comment)
	   (concat (shimbun-mixi-make-xref (mixi-comment-parent object))
		   "#comment"))
	  ((eq class 'mixi-message)
	   (mixi-expand-url (mixi-message-page object)))
	  ((eq class 'mixi-news)
	   (mixi-news-page object)))))

(defun shimbun-mixi-make-body (object)
  (let ((class (mixi-object-class object)))
    (cond ((eq class 'mixi-event)
	   (let ((limit (mixi-event-limit object)))
	     (setq limit (if limit
			     (format-time-string "%Y年%m月%d日" limit)
			   "指定なし"))
	     (concat "<dl><dt>開催日時：</dt>"
		     "<dd>" (mixi-event-date object) "</dd>"
		     "<dt>開催場所：</dt>"
		     "<dd>" (mixi-event-place object) "</dd>"
		     "<dt>詳細：</dt>"
		     "<dd>" (mixi-event-detail object) "</dd>"
		     "<dt>募集期限：</dt>"
		     "<dd>" limit "</dd>"
		     "<dt>参加者：</dt>"
		     "<dd>" (mixi-event-members object) "</dd></dl>")))
	  (t (mixi-object-content object)))))

(defun shimbun-mixi-make-reply-to (object)
  (setq shimbun-mixi-reply-to "mixi;")
  (let ((class (mixi-object-class object)))
    (setq shimbun-mixi-reply-to
	  (concat
	   (cond ((eq class 'mixi-diary)
		  (concat shimbun-mixi-reply-to "comment;diary;"
			  (mixi-friend-id (mixi-diary-owner object)) ";"
			  (mixi-diary-id object)))
		 ((mixi-bbs-p object)
		  (concat shimbun-mixi-reply-to "comment;"
			  (mixi-object-name object) ";"
			  (mixi-community-id (mixi-bbs-community object)) ";"
			  (mixi-bbs-id object)))
		 ((eq class 'mixi-community)
		  (concat shimbun-mixi-reply-to "topic;"
			  (mixi-community-id object)))
		 ((or (eq class 'mixi-news) (eq object (mixi-make-me)))
		  (concat shimbun-mixi-reply-to "diary"))
		 ((eq class 'mixi-message)
		  (concat shimbun-mixi-reply-to "message;"
			  (mixi-friend-id (mixi-message-owner object))))
		 ((or (eq class 'mixi-friend) (eq class 'mixi-log))
		  (concat shimbun-mixi-reply-to "message;"
			  (mixi-friend-id object))))))))

(defun shimbun-mixi-get-headers (shimbun objects &optional range)
  (when objects
    (let (headers)
      (catch 'stop
	(mapc (lambda (object)
		(when (mixi-object-p object)
		  (let ((class (mixi-object-class object)))
		    (when (mixi-parent-p object)
		      (let ((comments (mixi-get-comments object range)))
			(mapc (lambda (header)
				(push header headers))
			      (shimbun-mixi-get-headers shimbun
							comments))))
		    (let ((id (shimbun-mixi-make-message-id object)))
		      (when (and (eq class 'mixi-comment)
				 (shimbun-search-id shimbun id))
			(throw 'stop nil))
		      (push
		       (shimbun-create-header
			0
			(shimbun-mixi-make-subject shimbun object)
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
		      (when (eq class 'mixi-comment)
			(puthash id (mixi-comment-content object)
				 (shimbun-mixi-comment-cache-internal
				  shimbun)))))))
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
		 (setq objects (mixi-get-bbses object range)))
		((mixi-parent-p object)
		 (setq objects (mixi-get-comments object range)))
		(t (error (concat (symbol-name class)
				  " is not supported yet.")))))
      (setq objects (funcall url-or-function range)))
    (shimbun-sort-headers (shimbun-mixi-get-headers shimbun objects range))))

(defun shimbun-mixi-comment-article (url shimbun header)
  (let* ((message-id (shimbun-header-id header))
	 (cache (shimbun-mixi-comment-cache-internal shimbun))
	 (article (gethash message-id cache)))
    (unless (stringp article)
      (let ((parent (mixi-make-object-from-url url)))
	(mapc (lambda (comment)
		(let ((id (shimbun-mixi-make-message-id comment))
		      (content (mixi-comment-content comment)))
		  (puthash id content cache)
		  (when (string= id message-id)
		    (setq article content))))
	      (mixi-get-comments parent))))
    article))

(luna-define-method shimbun-article ((shimbun shimbun-mixi)
				     header &optional outbuf)
  (when (shimbun-current-group-internal shimbun)
    (with-current-buffer (or outbuf (current-buffer))
      (w3m-insert-string
       (or (with-temp-buffer
	     (let* ((url (shimbun-article-url shimbun header))
		    (object (mixi-make-object-from-url url))
		    (article (if (string-match "#comment$" url)
				 (shimbun-mixi-comment-article
				  url shimbun header)
			       (shimbun-mixi-make-body object))))
	       (shimbun-mixi-make-reply-to object)
	       (when (stringp article)
		 (insert article)))
	     (shimbun-message shimbun "shimbun: Make contents...")
	     (goto-char (point-min))
	     (prog1 (shimbun-make-contents shimbun header)
	       (shimbun-message shimbun "shimbun: Make contents...done")))
	   "")))))

(defconst shimbun-mixi-to-regexp
  "^mixi;\\([a-z]+\\);?\\([a-z0-9]+\\)?;?\\([0-9]+\\)?;?\\([0-9]+\\)?")

(defun shimbun-mixi-send-mail (to title content)
  (when (string-match shimbun-mixi-to-regexp to)
    (let ((method (match-string 1 to)))
      (cond ((string= method "comment")
	     (let ((parent (match-string 2 to))
		   (owner-id (match-string 3 to))
		   (id (match-string 4 to)))
	       (if (string= parent "diary")
		   (mixi-post-comment
		    (mixi-make-diary (mixi-make-friend owner-id) id) content)
		 (let ((func (intern
			      (concat mixi-object-prefix "make-" parent))))
		   (mixi-post-comment
		    (funcall func (mixi-make-community owner-id) id)
		    content)))))
	    ((string= method "topic")
	     (mixi-post-topic (mixi-make-community (match-string 2 to))
			      title content))
	    ((string= method "diary")
	     (mixi-post-diary title content))
	    ((string= method "message")
	     (mixi-post-message (mixi-make-friend (match-string 2 to))
				title content))))))

(provide 'sb-mixi)

;;; sb-mixi.el ends here
