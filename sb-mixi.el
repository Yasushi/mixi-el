;;; sb-mixi.el --- shimbun backend for mixi

;; Copyright (C) 2006, 2007 OHASHI Akira

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
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; If you have bug reports and/or suggestions for improvement, please
;; send them via <URL:http://mixi.jp/view_community.pl?id=1596390>.

;;; Code:

(require 'mixi)
(require 'mixi-utils)
(require 'shimbun)

(defconst shimbun-mixi-revision "$Revision: 1.61 $")

(eval-and-compile
  (luna-define-class shimbun-mixi (shimbun) (comment-cache))
  (luna-define-internal-accessors 'shimbun-mixi))

(defconst shimbun-mixi-default-group-alist
  '(("new-diaries" . mixi-get-new-diaries)
    ("new-comments" . mixi-get-new-comments)
    ("new-bbses" . mixi-get-new-bbses)
    ("new-bbs-comments" . mixi-get-new-bbs-comments)
    ("messages" . mixi-get-messages)
    ("messages.sent" .
     (lambda (range)
       (mixi-get-messages 'outbox range)))
    ("logs" . mixi-get-logs)
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
    ("news.newest.game-anime" .
     (lambda (range)
       (mixi-get-news 'game-anime 'newest range)))
    ("news.newest.column" .
     (lambda (range)
       (mixi-get-news 'column 'newest range)))
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
       (mixi-get-news 'IT 'pickup range)))
    ("news.pickup.game-anime" .
     (lambda (range)
       (mixi-get-news 'game-anime 'pickup range)))
    ("news.pickup.column" .
     (lambda (range)
       (mixi-get-news 'column 'pickup range))))
  "An alist of mixi shimbun group default definition.")

(defcustom shimbun-mixi-group-alist nil
  "*An alist of mixi shimbun group definition.
Each element looks like (NAME . URL) or (NAME . FUNCTION).
NAME is a shimbun group name that you can name it as you like.
URL is the URL for mixi access point of the group.  If URL is friend's, get
his/her diaries as article.  If community's, get its BBSes.  If diary's or
BBS's, get its comments.
FUNCTION is the function which has one `range' argument and returns the list
of mixi object."
  :group 'shimbun
  :type '(repeat (cons :fromat "%v"
		       (string :tag "Group name")
		       (radio (string :tag "URL")
			      (function :tag "Function")))))

(defcustom shimbun-mixi-get-profile-with-diary t
  "*If non-nil, get his/her profile together with diaries."
  :group 'shimbun
  :type 'boolean)

(defcustom shimbun-mixi-add-comment-count-to-author t
  "*If non-nil, add comment count to author."
  :group 'shimbun
  :type 'boolean)

;; FIXME: Don't use this user option.
(defcustom shimbun-mixi-page-articles 10
  "*How many articles are there in one page."
  :group 'shimbun
  :type 'integer)

(defvar shimbun-mixi-x-face-alist
  '(("default" . "X-Face: CY;j#FoBnpK^37`-IoJvN!J^u;GciiPmMQ@T)~RP1]t8iv?v)/bVI:I\"F!JfWJvhM5{zY!=
 h.d+'g\\I{D>Ocy?Rc4uYUyOZj2%2Kl>,x-!MCSsyi3!L}psrrC1jlF,O?Ui>qf)X;sBz`/}\\066X%$
 siG'|4K!2?==|oB&#E'5GGH\\#z[muyQ")))

(defun shimbun-mixi-group-alist ()
  (append shimbun-mixi-default-group-alist
	  shimbun-mixi-group-alist))

(luna-define-method initialize-instance :after ((shimbun shimbun-mixi)
						&rest init-args)
  (shimbun-mixi-set-comment-cache-internal shimbun
					   (make-hash-table :test 'equal))
  shimbun)

(luna-define-method shimbun-close :after ((shimbun shimbun-mixi))
  (shimbun-mixi-set-comment-cache-internal shimbun nil)
  (mixi-logout))

(luna-define-method shimbun-groups ((shimbun shimbun-mixi))
  (mapcar 'car (shimbun-mixi-group-alist)))

(luna-define-method shimbun-reply-to ((shimbun shimbun-mixi))
  mixi-reply-to)

(defun shimbun-mixi-get-headers (shimbun objects &optional range)
  (let (headers)
    (catch 'stop
      (while objects
	(let ((object (car objects)))
	  (when (and shimbun-mixi-get-profile-with-diary
		     (mixi-diary-p object))
	    (let ((owner (mixi-diary-owner object)))
	      (unless (mixi-object-realized-p owner)
		(mixi-realize-friend owner))))
	  (when (mixi-parent-p object)
	    (let* ((comments (mixi-get-comments object range))
		   (comment-headers (shimbun-mixi-get-headers shimbun
							      comments)))
	      (while comment-headers
		(push (car comment-headers) headers)
		(setq comment-headers (cdr comment-headers)))))
	  (let ((class (mixi-object-class object))
		(id (mixi-make-message-id object)))
	    (when (and (eq class 'mixi-comment)
		       (shimbun-search-id shimbun id))
	      (throw 'stop nil))
	    (push
	     (shimbun-create-header
	      0
	      (mixi-make-title object (string-match
				       "^new-"
				       (shimbun-current-group-internal
					shimbun)))
	      (mixi-make-author object shimbun-mixi-add-comment-count-to-author)
	      (mixi-make-date object)
	      id
	      (if (eq class 'mixi-comment)
		  (mixi-make-message-id (mixi-comment-parent object))
		"")
	      0 0
	      (mixi-make-url object))
	     headers)
	    (when (eq class 'mixi-comment)
	      (puthash id (mixi-comment-content object)
		       (shimbun-mixi-comment-cache-internal shimbun)))))
	(setq objects (cdr objects))))
    headers))

(luna-define-method shimbun-get-headers ((shimbun shimbun-mixi)
					 &optional range)
  (let ((url-or-function (cdr (assoc (shimbun-current-group-internal shimbun)
				     (shimbun-mixi-group-alist))))
	(range (shimbun-header-index-pages range)))
    (when (integerp range)
      (setq range (* range shimbun-mixi-page-articles)))
    (shimbun-sort-headers
     (shimbun-mixi-get-headers shimbun
			       (mixi-make-objects url-or-function range)
			       range))))

(defun shimbun-mixi-comment-article (url shimbun header)
  (let* ((message-id (shimbun-header-id header))
	 (cache (shimbun-mixi-comment-cache-internal shimbun))
	 (article (gethash message-id cache)))
    (unless (stringp article)
      (let* ((parent (mixi-make-object-from-url url))
	     (comments (mixi-get-comments parent)))
	(while comments
	  (let ((id (mixi-make-message-id (car comments)))
		(content (mixi-comment-content (car comments))))
	    (puthash id content cache)
	    (when (string= id message-id)
	      (setq article content)))
	  (setq comments (cdr comments)))))
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
			       (mixi-make-content object))))
	       (mixi-make-reply-to object)
	       (when (stringp article)
		 (insert article)))
	     (shimbun-message shimbun "shimbun: Make contents...")
	     (goto-char (point-min))
	     (prog1 (shimbun-make-contents shimbun header)
	       (shimbun-message shimbun "shimbun: Make contents...done")))
	   "")))))

(provide 'sb-mixi)

;;; sb-mixi.el ends here
