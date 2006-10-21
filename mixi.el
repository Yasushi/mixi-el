;; mixi.el --- API library for accessing to mixi

;; Copyright (C) 2005,2006 OHASHI Akira

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

;; API for getting contents:
;;
;;  * mixi-get-friends
;;  * mixi-get-favorites
;;  * mixi-get-logs
;;  * mixi-get-diaries
;;  * mixi-get-new-diaries
;;  * mixi-get-communities
;;  * mixi-get-topics
;;  * mixi-get-new-topics
;;  * mixi-get-comments
;;  * mixi-get-new-comments

;; Example:
;;
;; Display only the first page of new diaries like a mail format.
;;
;; (let ((mixi-new-diary-max-pages 1)
;;       (buffer (generate-new-buffer "*temp*"))
;;       (format "%Y/%m/%d %H:%M"))
;;   (pop-to-buffer buffer)
;;   (mapc (lambda (diary)
;; 	  (let ((subject (mixi-diary-title diary))
;; 		;; Don't get owner's nick at first for omitting a useless
;; 		;; retrieval.
;; 		(from (mixi-friend-nick (mixi-diary-owner diary)))
;; 		(date (format-time-string format (mixi-diary-time diary)))
;; 		(body (mixi-diary-content diary)))
;; 	    (insert "From: " from "\n"
;; 		    "Subject: " subject "\n"
;; 		    "Date: " date "\n\n"
;; 		    body "\n\n")))
;; 	(mixi-get-new-diaries))
;;   (set-buffer-modified-p nil)
;;   (setq buffer-read-only t)
;;   (goto-char (point-min)))
;;
;; Display only the first page of new diaries including all comments like a
;; mail format.  Comments are displayed like a reply mail.
;;
;; (let ((mixi-new-diary-max-pages 1)
;;       (buffer (generate-new-buffer "*temp*"))
;;       (format "%Y/%m/%d %H:%M"))
;;   (pop-to-buffer buffer)
;;   (mapc (lambda (diary)
;; 	  (let ((subject (mixi-diary-title diary))
;;  		;; Don't get owner's nick at first for omitting a useless
;;  		;; retrieval.
;;  		(from (mixi-friend-nick (mixi-diary-owner diary)))
;; 		(date (format-time-string format (mixi-diary-time diary)))
;; 		(body (mixi-diary-content diary)))
;; 	    (insert "From: " from "\n"
;; 		    "Subject: " subject "\n"
;; 		    "Date: " date "\n\n"
;; 		    body "\n\n")
;; 	    (mapc (lambda (comment)
;; 		    (let ((from (mixi-friend-nick
;; 				 (mixi-comment-owner comment)))
;; 			  (subject (concat "Re: " subject))
;; 			  (date (format-time-string
;; 				 format (mixi-comment-time comment)))
;; 			  (body (mixi-comment-content comment)))
;; 		      (insert "From: " from "\n"
;; 			      "Subject: " subject "\n"
;; 			      "Date: " date "\n\n"
;; 			      body "\n\n")))
;; 		  (mixi-get-comments diary))))
;; 	(mixi-get-new-diaries))
;;   (set-buffer-modified-p nil)
;;   (setq buffer-read-only t)
;;   (goto-char (point-min)))

;;; Code:

(require 'w3m)
(eval-when-compile (require 'cl))

(defgroup mixi nil
  "API library for accessing to mixi."
  :group 'hypermedia)

(defcustom mixi-url "http://mixi.jp"
  "*The URL of mixi."
  :type 'string
  :group 'mixi)

(defcustom mixi-coding-system 'euc-jp
  "*Coding system for mixi."
  :type 'coding-system
  :group 'mixi)

(defcustom mixi-default-email nil
  "*Default E-mail address that is used to login automatically."
  :type '(choice (string :tag "E-mail address")
		 (const :tag "Asked when it is necessary" nil))
  :group 'mixi)

(defcustom mixi-default-password nil
  "*Default password that is used to login automatically."
  :type '(choice (string :tag "Password")
		 (const :tag "Asked when it is necessary" nil))
  :group 'mixi)

(defcustom mixi-accept-adult-contents t
  "*If non-nil, accept to access to the adult contents."
  :type 'boolean
  :group 'mixi)

(defcustom mixi-continuously-access-interval 3.0
  "*Time interval between each mixi access.
Increase this value when unexpected error frequently occurs."
  :type 'number
  :group 'mixi)

(defcustom mixi-cache-expires 3600
  "*Seconds for expiration of a cached object."
  :type '(choice (integer :tag "Expired seconds")
		 (const :tag "Don't expire" nil))
  :group 'mixi)

;; FIXME: Not implemented.
(defcustom mixi-cache-use-file t
  "*If non-nil, caches are saved to files."
  :type 'boolean
  :group 'mixi)

(defcustom mixi-cache-directory (expand-file-name "~/.mixi")
  "*Where to look for cache files."
  :type 'directory
  :group 'mixi)

(defcustom mixi-verbose t
  "*Flag controls whether `mixi' should be verbose.
If it is non-ni, the `w3m-verbose' variable will be bound to nil
while `mixi' is waiting for a server's response."
  :type 'boolean
  :group 'mixi)

(defvar mixi-me nil)

;; Utilities.
(defmacro mixi-message (string)
  `(concat "[mixi] " ,string))

(defconst mixi-message-adult-contents
  "このページから先はアダルト（成人向け）コンテンツが含まれています。<br>
閲覧に同意された方のみ、先へお進みください。")
(defconst mixi-message-continuously-accessing
  "安定してサイトの運営をおこなう為、間隔を空けない連続的なページの遷移・更<br>
新は制限させていただいております。ご迷惑をおかけいたしますが、しばらくお<br>
待ちいただいてから操作をおこなってください。")
(defconst mixi-warning-continuously-accessing
  "間隔を空けない連続的なページの遷移・更新を頻繁におこなわれていることが見<br>
受けられましたので、一時的に操作を停止させていただきます。申し訳ございま<br>
せんが、しばらくの間お待ちください。")

(defun mixi-retrieve (url &optional post-data)
  "Retrieve the URL and return getted strings."
  (let ((url (w3m-expand-url url mixi-url)))
    (with-temp-buffer
      (let ((w3m-verbose (if mixi-verbose nil w3m-verbose)))
	(if (not (string= (w3m-retrieve url nil nil post-data) "text/html"))
	    (error (mixi-message "Cannot retrieve"))
	  (w3m-decode-buffer url)
	  (let ((ret (buffer-substring-no-properties (point-min) (point-max))))
	    (when (string-match mixi-message-adult-contents ret)
	      (if mixi-accept-adult-contents
		  (setq ret (mixi-retrieve url "submit=agree"))
		(setq ret (mixi-retrieve (concat url "?")))))
	    (when (string-match mixi-warning-continuously-accessing ret)
	      (error (mixi-message "Continuously accessing")))
	    (if (not (string-match mixi-message-continuously-accessing ret))
		ret
	      (message (mixi-message "Waiting for continuously accessing..."))
	      (sit-for mixi-continuously-access-interval)
	      (mixi-retrieve url post-data))))))))

(defconst mixi-my-id-regexp
  "<a href=\"add_diary\\.pl\\?id=\\([0-9]+\\)")

(defun mixi-login (&optional email password)
  "Login to mixi."
  (unless w3m-use-cookies
    (error
     (mixi-message
      "Require to accept cookies.  Please set `w3m-use-cookies' to t.")))
  (let ((email (or email mixi-default-email
		   (read-from-minibuffer (mixi-message "Login Email: "))))
	(password (or password mixi-default-password
		      (read-passwd (mixi-message "Login Password: ")))))
    (let ((buffer (mixi-retrieve "/login.pl"
				 (concat "email=" email
					 "&password=" password
					 "&next_url=/home.pl"
					 "&sticky=on"))))
      (unless (string-match "url=/check\\.pl\\?n=" buffer)
	(error (mixi-message "Cannot login")))
      (setq buffer (mixi-retrieve "/check.pl?n=home.pl"))
      (if (string-match mixi-my-id-regexp buffer)
	  (setq mixi-me (mixi-make-friend
			 (string-to-number (match-string 1 buffer))))
	(error (mixi-message "Cannot login"))))))

(defun mixi-logout ()
  (mixi-retrieve "/logout.pl"))

(defmacro with-mixi-retrieve (url &rest body)
  `(let (buffer)
     (when ,url
       (setq buffer (mixi-retrieve ,url))
       (when (string-match "login.pl" buffer)
	 (mixi-login)
	 (setq buffer (mixi-retrieve ,url))))
     ,@body))
(put 'with-mixi-retrieve 'lisp-indent-function 'defun)

(defun mixi-get-matched-items (url max-pages regexp)
  "Get matched items to REGEXP in URL."
  (let ((page 1)
	ids)
    (catch 'end
      (while (or (null max-pages) (<= page max-pages))
	(with-mixi-retrieve (format url page)
	  (let ((pos 0))
	    (while (string-match regexp buffer pos)
	      (let ((num 1)
		    list)
		(while (match-string num buffer)
		  (let ((string (match-string num buffer)))
		    (save-match-data
		      (when (string-match "^[0-9]+$" string)
			(setq string (string-to-number string))))
		    (setq list (cons string list)))
		  (incf num))
	      (setq ids (cons (reverse list) ids))
	      (setq pos (match-end (1- num)))))
	    (when (eq pos 0)
	      (throw 'end ids))))
	(incf page)))
    ;; FIXME: Sort? Now order by newest.
    (reverse ids)))

(defun mixi-remove-markup (string)
  "Remove markups from STRING."
  (with-temp-buffer
    (insert string)
    (save-excursion
      (goto-char (point-min))
      (while (search-forward "<!--" nil t)
	(delete-region (match-beginning 0)
		       (or (search-forward "-->" nil t)
			   (point-max))))
      (goto-char (point-min))
      (while (re-search-forward "<[^>]+>" nil t)
	(replace-match "" t t))
      (goto-char (point-min))
      (while (re-search-forward "" nil t)
	(replace-match "\n" t t)))
    (w3m-decode-entities)
    (buffer-string)))

;; Cache.

(defun mixi-cache-expired-p (object)
  "Whether a cache of OBJECT is expired."
  ;; FIXME: Use method instead of `(aref (cdr object) 0)'.
  (let ((timestamp (aref (cdr object) 0)))
    (unless (or (null mixi-cache-expires)
		 (null timestamp))
      (time-less-p (time-add timestamp
			     (seconds-to-time mixi-cache-expires))
		   (current-time)))))

(defun mixi-make-cache (key value table)
  "Make a cache object and return it."
  (let ((cache (gethash key table)))
    (if (and cache (not (mixi-cache-expired-p cache)))
	cache
      (puthash key value table))))

;; Object.
(defconst mixi-object-prefix "mixi-")

(defmacro mixi-object-class (object)
  `(car-safe ,object))

(defmacro mixi-object-p (object)
  `(eq (string-match (concat "^" mixi-object-prefix)
		     (symbol-name (mixi-object-class ,object))) 0))

(defun mixi-object-name (object)
  "Return the name of OBJECT."
  (unless (mixi-object-p object)
    (signal 'wrong-type-argument (list 'mixi-object-p object)))
  (let ((class (mixi-object-class object)))
    (substring (symbol-name class) (length mixi-object-prefix))))

(defun mixi-object-id (object)
  "Return the id of OBJECT."
  (unless (mixi-object-p object)
    (signal 'wrong-type-argument (list 'mixi-object-p object)))
  (let ((func (intern (concat mixi-object-prefix
			      (mixi-object-name object) "-id"))))
    (funcall func object)))

;; Friend object.
(defvar mixi-friend-cache (make-hash-table :test 'equal))
(defun mixi-make-friend (id &optional nick)
  "Return a friend object."
  (mixi-make-cache id (cons 'mixi-friend (vector nil id nick nil))
		   mixi-friend-cache))

(defun mixi-make-me ()
  (unless mixi-me
    (with-mixi-retrieve "/home.pl"
      (if (string-match mixi-my-id-regexp buffer)
	  (setq mixi-me
		(mixi-make-friend (string-to-number (match-string 1 buffer))))
	(signal 'error (list 'who-am-i)))))
  mixi-me)

(defmacro mixi-friend-p (friend)
  `(eq (mixi-object-class ,friend) 'mixi-friend))

(defmacro mixi-friend-page (friend)
  `(concat "/show_friend.pl?id=" (number-to-string (mixi-friend-id ,friend))))

(defconst mixi-friend-nick-regexp
  "<img alt=\"\\*\" src=\"http://img\\.mixi\\.jp/img/dot0\\.gif\" width=\"1\" height=\"5\"><br>\n\\(.*\\)さん([0-9]+)")
(defconst mixi-friend-name-regexp
  "<td BGCOLOR=#F2DDB7 WIDTH=80 NOWRAP><font COLOR=#996600>名&nbsp;前</font></td>\n<td WIDTH=345>\\([^<]+\\)</td>")
(defconst mixi-my-name-regexp
  "<td BGCOLOR=#F2DDB7 WIDTH=80 NOWRAP><font COLOR=#996600>名 前</font></td>\n\n<td WIDTH=345>\\([^<]+\\)</td>")

(defun mixi-friend-realize (friend)
  "Realize a FRIEND."
  ;; FIXME: Check a expiration of cache?
  (unless (mixi-friend-realize-p friend)
    (let (name)
      (with-mixi-retrieve (mixi-friend-page friend)
	(if (string-match mixi-friend-nick-regexp buffer)
	    (mixi-friend-set-nick friend (match-string 1 buffer))
	  (signal 'error (list 'cannot-find-nick friend)))
	(when (string-match mixi-friend-name-regexp buffer)
	  (setq name (match-string 1 buffer))))
      ;; For getting my name.
      (unless name
	(with-mixi-retrieve "/show_profile.pl"
	  (if (string-match mixi-my-name-regexp buffer)
	      (setq name (match-string 1 buffer))
	    (signal 'error (list 'cannot-find-name friend)))))
      (mixi-friend-set-name friend name))
    (mixi-friend-touch friend)))

(defun mixi-friend-realize-p (friend)
  "Return the timestamp of FRIEND."
  (unless (mixi-friend-p friend)
    (signal 'wrong-type-argument (list 'mixi-friend-p friend)))
  (aref (cdr friend) 0))

(defun mixi-friend-id (friend)
  "Return the id of FRIEND."
  (unless (mixi-friend-p friend)
    (signal 'wrong-type-argument (list 'mixi-friend-p friend)))
  (aref (cdr friend) 1))

(defun mixi-friend-nick (friend)
  "Return the nick of FRIEND."
  (unless (mixi-friend-p friend)
    (signal 'wrong-type-argument (list 'mixi-friend-p friend)))
  (unless (aref (cdr friend) 2)
    (mixi-friend-realize friend))
  (aref (cdr friend) 2))

(defun mixi-friend-name (friend)
  "Return the name of FRIEND."
  (unless (mixi-friend-p friend)
    (signal 'wrong-type-argument (list 'mixi-friend-p friend)))
  (mixi-friend-realize friend)
  (aref (cdr friend) 3))

(defun mixi-friend-touch (friend)
  "Set the timestamp of FRIEND."
  (unless (mixi-friend-p friend)
    (signal 'wrong-type-argument (list 'mixi-friend-p friend)))
  (aset (cdr friend) 0 (current-time)))

(defun mixi-friend-set-nick (friend nick)
  "Set the nick of FRIEND."
  (unless (mixi-friend-p friend)
    (signal 'wrong-type-argument (list 'mixi-friend-p friend)))
  (aset (cdr friend) 2 nick))

(defun mixi-friend-set-name (friend name)
  "Set the name of FRIEND."
  (unless (mixi-friend-p friend)
    (signal 'wrong-type-argument (list 'mixi-friend-p friend)))
  (aset (cdr friend) 3 name))

(defmacro mixi-friend-list-page (&optional friend)
  `(concat "/list_friend.pl?page=%d"
	   (when ,friend (concat "&id=" (number-to-string
					 (mixi-friend-id ,friend))))))

(defconst mixi-friend-list-id-regexp
  "<a href=show_friend\\.pl\\?id=\\([0-9]+\\)")
(defconst mixi-friend-list-nick-regexp
  "<td valign=middle>\\(.+\\)さん([0-9]+)<br />")

(defvar mixi-friend-max-pages 10)
(defun mixi-get-friends (&optional friend)
  "Get friends of FRIEND."
  (unless (or (null friend) (mixi-friend-p friend))
    (signal 'wrong-type-argument (list 'mixi-friend-p friend)))
  (let ((ids (mixi-get-matched-items (mixi-friend-list-page friend)
				     mixi-friend-max-pages
				     mixi-friend-list-id-regexp))
	(nicks (mixi-get-matched-items (mixi-friend-list-page friend)
				       mixi-friend-max-pages
				       mixi-friend-list-nick-regexp)))
    (let ((index 0)
	  ret)
      (while (< index (length ids))
	(setq ret (cons (mixi-make-friend (nth 0 (nth index ids))
					  (nth 0 (nth index nicks))) ret))
	(incf index))
      (reverse ret))))

;; Favorite.
(defmacro mixi-favorite-list-page ()
  `(concat "/list_bookmark.pl?page=%d"))

(defconst mixi-favorite-list-id-regexp
  "<td ALIGN=center BGCOLOR=#FDF9F2 width=330><a href=\"show_friend\\.pl\\?id=\\([0-9]+\\)\">")
(defconst mixi-favorite-list-nick-regexp
  "<td BGCOLOR=#FDF9F2><font COLOR=#996600>名&nbsp;&nbsp;前</font></td>
<td COLSPAN=2 BGCOLOR=#FFFFFF>\\(.+\\)</td></tr>")

(defvar mixi-favorite-max-pages nil)
(defun mixi-get-favorites ()
  "Get favorites."
  (let ((ids (mixi-get-matched-items (mixi-favorite-list-page)
				     mixi-favorite-max-pages
				     mixi-favorite-list-id-regexp))
	(nicks (mixi-get-matched-items (mixi-favorite-list-page)
				       mixi-favorite-max-pages
				       mixi-favorite-list-nick-regexp)))
    (let ((index 0)
	  ret)
      (while (< index (length ids))
	(setq ret (cons (mixi-make-friend (nth 0 (nth index ids))
					  (nth 0 (nth index nicks))) ret))
	(incf index))
      (reverse ret))))

;; Log object.
(defun mixi-make-log (friend time)
  "Return a log object."
  (cons 'mixi-log (vector friend time)))

(defmacro mixi-log-p (log)
  `(eq (mixi-object-class ,log) 'mixi-log))

(defun mixi-log-friend (log)
  "Return the friend of LOG."
  (unless (mixi-log-p log)
    (signal 'wrong-type-argument (list 'mixi-log-p log)))
  (aref (cdr log) 0))

(defun mixi-log-time (log)
  "Return the time of LOG."
  (unless (mixi-log-p log)
    (signal 'wrong-type-argument (list 'mixi-log-p log)))
  (aref (cdr log) 1))

(defmacro mixi-log-list-page ()
  `(concat "/show_log.pl"))

(defconst mixi-log-list-regexp
  "\\([0-9]+\\)年\\([0-9]+\\)月\\([0-9]+\\)日 \\([0-9]+\\):\\([0-9]+\\) <a href=\"show_friend\\.pl\\?id=\\([0-9]+\\)\">\\(.+\\)</a><br>")

(defvar mixi-log-max-pages 1)
(defun mixi-get-logs ()
  "Get logs."
  (let ((items (mixi-get-matched-items (mixi-log-list-page)
				       mixi-log-max-pages
				       mixi-log-list-regexp)))
    (mapcar (lambda (item)
	      (mixi-make-log (mixi-make-friend (nth 5 item) (nth 6 item))
			     (encode-time 0 (nth 4 item) (nth 3 item)
					  (nth 2 item) (nth 1 item)
					  (nth 0 item))))
	    items)))

;; Diary object.
(defvar mixi-diary-cache (make-hash-table :test 'equal))
(defun mixi-make-diary (owner id)
  "Return a diary object."
  (let ((owner (or owner (mixi-make-me))))
    (mixi-make-cache (list (mixi-friend-id owner) id)
		     (cons 'mixi-diary (vector nil owner id nil nil nil))
		     mixi-diary-cache)))

(defmacro mixi-diary-p (diary)
  `(eq (mixi-object-class ,diary) 'mixi-diary))

(defmacro mixi-diary-page (diary)
  `(concat "/view_diary.pl?id=" (number-to-string (mixi-diary-id ,diary))
	   "&owner_id=" (number-to-string (mixi-friend-id
					   (mixi-diary-owner ,diary)))))

;; FIXME: Remove `さん'.
(defconst mixi-diary-owner-nick-regexp
  "<td WIDTH=490 background=http://img\\.mixi\\.jp/img/bg_w\\.gif><b><font COLOR=#605048>\\(.+\\)\\(さん\\)?の日記</font></b></td>")
(defconst mixi-diary-time-regexp
  "<td ALIGN=center ROWSPAN=2 NOWRAP WIDTH=95 bgcolor=#FFD8B0>\\([0-9]+\\)年\\([0-9]+\\)月\\([0-9]+\\)日<br>\\([0-9]+\\):\\([0-9]+\\)</td>")
(defconst mixi-diary-title-regexp
  "<td BGCOLOR=#FFF4E0 WIDTH=430>&nbsp;\\([^<]+\\)</td></tr>")
(defconst mixi-diary-content-regexp
  "<td CLASS=h12>\\(.+\\)</td></tr>")

(defun mixi-diary-realize (diary)
  "Realize a DIARY."
  ;; FIXME: Check a expiration of cache?
  (unless (mixi-diary-realize-p diary)
    (with-mixi-retrieve (mixi-diary-page diary)
      (if (string-match mixi-diary-owner-nick-regexp buffer)
	  (mixi-friend-set-nick (mixi-diary-owner diary)
				(match-string 1 buffer))
	(signal 'error (list 'cannot-find-owner-nick diary)))
      (if (string-match mixi-diary-time-regexp buffer)
	  (mixi-diary-set-time
	   diary (encode-time 0 (string-to-number (match-string 5 buffer))
			      (string-to-number (match-string 4 buffer))
			      (string-to-number (match-string 3 buffer))
			      (string-to-number (match-string 2 buffer))
			      (string-to-number (match-string 1 buffer))))
	(signal 'error (list 'cannot-find-time diary)))
      (if (string-match mixi-diary-title-regexp buffer)
	  (mixi-diary-set-title diary (match-string 1 buffer))
	(signal 'error (list 'cannot-find-title diary)))
      (if (string-match mixi-diary-content-regexp buffer)
	  (mixi-diary-set-content diary (mixi-remove-markup
					 (match-string 1 buffer)))
	(signal 'error (list 'cannot-find-content diary))))
    (mixi-diary-touch diary)))

(defun mixi-diary-realize-p (diary)
  "Return the timestamp of DIARY."
  (unless (mixi-diary-p diary)
    (signal 'wrong-type-argument (list 'mixi-diary-p diary)))
  (aref (cdr diary) 0))

(defun mixi-diary-owner (diary)
  "Return the owner of DIARY."
  (unless (mixi-diary-p diary)
    (signal 'wrong-type-argument (list 'mixi-diary-p diary)))
  (aref (cdr diary) 1))

(defun mixi-diary-id (diary)
  "Return the id of DIARY."
  (unless (mixi-diary-p diary)
    (signal 'wrong-type-argument (list 'mixi-diary-p diary)))
  (aref (cdr diary) 2))

(defun mixi-diary-time (diary)
  "Return the time of DIARY."
  (unless (mixi-diary-p diary)
    (signal 'wrong-type-argument (list 'mixi-diary-p diary)))
  (mixi-diary-realize diary)
  (aref (cdr diary) 3))

(defun mixi-diary-title (diary)
  "Return the title of DIARY."
  (unless (mixi-diary-p diary)
    (signal 'wrong-type-argument (list 'mixi-diary-p diary)))
  (mixi-diary-realize diary)
  (aref (cdr diary) 4))

(defun mixi-diary-content (diary)
  "Return the content of DIARY."
  (unless (mixi-diary-p diary)
    (signal 'wrong-type-argument (list 'mixi-diary-p diary)))
  (mixi-diary-realize diary)
  (aref (cdr diary) 5))

(defun mixi-diary-touch (diary)
  "Set the timestamp of DIARY."
  (unless (mixi-diary-p diary)
    (signal 'wrong-type-argument (list 'mixi-diary-p diary)))
  (aset (cdr diary) 0 (current-time)))

(defun mixi-diary-set-time (diary time)
  "Set the time of DIARY."
  (unless (mixi-diary-p diary)
    (signal 'wrong-type-argument (list 'mixi-diary-p diary)))
  (aset (cdr diary) 3 time))

(defun mixi-diary-set-title (diary title)
  "Set the title of DIARY."
  (unless (mixi-diary-p diary)
    (signal 'wrong-type-argument (list 'mixi-diary-p diary)))
  (aset (cdr diary) 4 title))

(defun mixi-diary-set-content (diary content)
  "Set the content of DIARY."
  (unless (mixi-diary-p diary)
    (signal 'wrong-type-argument (list 'mixi-diary-p diary)))
  (aset (cdr diary) 5 content))

(defmacro mixi-diary-list-page (&optional friend)
  `(concat "/list_diary.pl?page=%d"
	   (when ,friend (concat "&id=" (number-to-string
					 (mixi-friend-id ,friend))))))

(defconst mixi-diary-list-regexp
  "<a href=\"view_diary\\.pl\\?id=\\([0-9]+\\)&owner_id=[0-9]+\">")

(defvar mixi-diary-max-pages nil)
(defun mixi-get-diaries (&optional friend)
  "Get diaries of FRIEND."
  (unless (or (null friend) (mixi-friend-p friend))
    (signal 'wrong-type-argument (list 'mixi-friend-p friend)))
  (let ((items (mixi-get-matched-items (mixi-diary-list-page friend)
				       mixi-diary-max-pages
				       mixi-diary-list-regexp)))
    (mapcar (lambda (item)
	      (mixi-make-diary friend (nth 0 item)))
	    items)))

(defmacro mixi-new-diary-list-page ()
  `(concat "/new_friend_diary.pl?page=%d"))

(defconst mixi-new-diary-list-regexp
  "<a class=\"new_link\" href=view_diary\\.pl\\?id=\\([0-9]+\\)&owner_id=\\([0-9]+\\)>")

(defvar mixi-new-diary-max-pages nil)
(defun mixi-get-new-diaries ()
  "Get new diaries."
  (let ((items (mixi-get-matched-items (mixi-new-diary-list-page)
				       mixi-new-diary-max-pages
				       mixi-new-diary-list-regexp)))
    (mapcar (lambda (item)
	      (mixi-make-diary (mixi-make-friend (nth 1 item)) (nth 0 item)))
	    items)))

;; Community object.
(defvar mixi-community-cache (make-hash-table :test 'equal))
(defun mixi-make-community (id &optional name)
  "Return a community object."
  (mixi-make-cache id (cons 'mixi-community (vector nil id name nil nil nil
						    nil))
		   mixi-community-cache))

(defmacro mixi-community-p (community)
  `(eq (mixi-object-class ,community) 'mixi-community))

(defmacro mixi-community-page (community)
  `(concat "/view_community.pl?id=" (number-to-string
				     (mixi-community-id ,community))))

(defconst mixi-community-nodata-regexp
  "^データがありません")
(defconst mixi-community-name-regexp
  "<td WIDTH=345>\\(.*\\)</td></tr>")
(defconst mixi-community-birthday-regexp
  "<td BGCOLOR=#F2DDB7><font COLOR=#996600>開設日</font></td>\n<td>\\([0-9]+\\)年\\([0-9]+\\)月\\([0-9]+\\)日</td>")
;; FIXME: Care when the owner has seceded.
(defconst mixi-community-owner-regexp
  "<td BGCOLOR=#F2DDB7><font COLOR=#996600>管理人</font></td>\n<td>\n\n<a href=\"\\(home\\.pl\\|show_friend\\.pl\\?id=\\([0-9]+\\)\\)\">\n\\(.+\\)</a>")
(defconst mixi-community-category-regexp
  "<td BGCOLOR=#F2DDB7><font COLOR=#996600>カテゴリ</font></td>\n<td>\\([^<]+\\)</td>")
(defconst mixi-community-description-regexp
  "<td CLASS=h120>\\(.+\\)</td>")

(defun mixi-community-realize (community)
  "Realize a COMMUNITY."
  ;; FIXME: Check a expiration of cache?
  (unless (mixi-community-realize-p community)
    (with-mixi-retrieve (mixi-community-page community)
      (if (string-match mixi-community-nodata-regexp buffer)
	  ;; FIXME: Set all members?
	  (mixi-community-set-name community "データがありません")
	(if (string-match mixi-community-name-regexp buffer)
	    (mixi-community-set-name community (match-string 1 buffer))
	  (signal 'error (list 'cannot-find-name community)))
	(if (string-match mixi-community-birthday-regexp buffer)
	    (mixi-community-set-birthday
	     community
	     (encode-time 0 0 0 (string-to-number (match-string 3 buffer))
			  (string-to-number (match-string 2 buffer))
			  (string-to-number (match-string 1 buffer))))
	  (signal 'error (list 'cannot-find-birthday community)))
	(if (string-match mixi-community-owner-regexp buffer)
	    (if (string= (match-string 1 buffer) "home.pl")
		(mixi-community-set-owner community (mixi-make-me))
	      (mixi-community-set-owner
	       community (mixi-make-friend
			  (string-to-number (match-string 2 buffer))
			  (match-string 3 buffer))))
	  (signal 'error (list 'cannot-find-owner community)))
	(if (string-match mixi-community-category-regexp buffer)
	    (mixi-community-set-category community (match-string 1 buffer))
	  (signal 'error (list 'cannot-find-category community)))
	(if (string-match mixi-community-description-regexp buffer)
	    (mixi-community-set-description community (match-string 1 buffer))
	  (signal 'error (list 'cannot-find-description community)))))
    (mixi-community-touch community)))

(defun mixi-community-realize-p (community)
  "Return the timestamp of COMMUNITY."
  (unless (mixi-community-p community)
    (signal 'wrong-type-argument (list 'mixi-community-p community)))
  (aref (cdr community) 0))

(defun mixi-community-id (community)
  "Return the id of COMMUNITY."
  (unless (mixi-community-p community)
    (signal 'wrong-type-argument (list 'mixi-community-p community)))
  (aref (cdr community) 1))

(defun mixi-community-name (community)
  "Return the name of COMMUNITY."
  (unless (mixi-community-p community)
    (signal 'wrong-type-argument (list 'mixi-community-p community)))
  (unless (aref (cdr community) 2)
    (mixi-community-realize community))
  (aref (cdr community) 2))

(defun mixi-community-birthday (community)
  "Return the birthday of COMMUNITY."
  (unless (mixi-community-p community)
    (signal 'wrong-type-argument (list 'mixi-community-p community)))
  (mixi-community-realize community)
  (aref (cdr community) 3))

(defun mixi-community-owner (community)
  "Return the owner of COMMUNITY."
  (unless (mixi-community-p community)
    (signal 'wrong-type-argument (list 'mixi-community-p community)))
  (mixi-community-realize community)
  (aref (cdr community) 4))

(defun mixi-community-category (community)
  "Return the category of COMMUNITY."
  (unless (mixi-community-p community)
    (signal 'wrong-type-argument (list 'mixi-community-p community)))
  (mixi-community-realize community)
  (aref (cdr community) 5))

(defun mixi-community-description (community)
  "Return the description of COMMUNITY."
  (unless (mixi-community-p community)
    (signal 'wrong-type-argument (list 'mixi-community-p community)))
  (mixi-community-realize community)
  (aref (cdr community) 6))

(defun mixi-community-touch (community)
  "Set the timestamp of COMMUNITY."
  (unless (mixi-community-p community)
    (signal 'wrong-type-argument (list 'mixi-community-p community)))
  (aset (cdr community) 0 (current-time)))

(defun mixi-community-set-name (community name)
  "Set the name of COMMUNITY."
  (unless (mixi-community-p community)
    (signal 'wrong-type-argument (list 'mixi-community-p community)))
  (aset (cdr community) 2 name))

(defun mixi-community-set-birthday (community birthday)
  "Set the birthday of COMMUNITY."
  (unless (mixi-community-p community)
    (signal 'wrong-type-argument (list 'mixi-community-p community)))
  (aset (cdr community) 3 birthday))

(defun mixi-community-set-owner (community owner)
  "Set the owner of COMMUNITY."
  (unless (mixi-community-p community)
    (signal 'wrong-type-argument (list 'mixi-community-p community)))
  (unless (mixi-friend-p owner)
    (signal 'wrong-type-argument (list 'mixi-friend-p owner)))
  (aset (cdr community) 4 owner))

(defun mixi-community-set-category (community category)
  "Set the category of COMMUNITY."
  (unless (mixi-community-p community)
    (signal 'wrong-type-argument (list 'mixi-community-p community)))
  (aset (cdr community) 5 category))

(defun mixi-community-set-description (community description)
  "Set the name of COMMUNITY."
  (unless (mixi-community-p community)
    (signal 'wrong-type-argument (list 'mixi-community-p community)))
  (aset (cdr community) 6 description))

(defmacro mixi-community-list-page (&optional friend)
  `(concat "/list_community.pl?page=%d"
	   (when ,friend (concat "&id=" (number-to-string
					 (mixi-friend-id ,friend))))))

(defconst mixi-community-list-id-regexp
  "<a href=view_community\\.pl\\?id=\\([0-9]+\\)")
(defconst mixi-community-list-name-regexp
  "<td valign=middle>\\(.+\\)([0-9]+)</td>")

(defvar mixi-community-max-pages nil)
(defun mixi-get-communities (&optional friend)
  "Get communities of FRIEND."
  (unless (or (null friend) (mixi-friend-p friend))
    (signal 'wrong-type-argument (list 'mixi-friend-p friend)))
  (let ((ids (mixi-get-matched-items (mixi-community-list-page friend)
				     mixi-community-max-pages
				     mixi-community-list-id-regexp))
	(names (mixi-get-matched-items (mixi-community-list-page friend)
				     mixi-community-max-pages
				     mixi-community-list-name-regexp)))
    (let ((index 0)
	  ret)
      (while (< index (length ids))
	(setq ret (cons (mixi-make-community (nth 0 (nth index ids))
					     (nth 0 (nth index names))) ret))
	(incf index))
      (reverse ret))))

;; Topic object.
(defvar mixi-topic-cache (make-hash-table :test 'equal))
(defun mixi-make-topic (community id)
  "Return a topic object."
  (mixi-make-cache (list (mixi-community-id community) id)
		   (cons 'mixi-topic (vector nil community id nil nil nil nil))
		   mixi-topic-cache))

(defmacro mixi-topic-p (topic)
  `(eq (mixi-object-class ,topic) 'mixi-topic))

(defmacro mixi-topic-page (topic)
  `(concat "/view_bbs.pl?id=" (number-to-string (mixi-topic-id ,topic))
	   "&comm_id=" (number-to-string
			(mixi-community-id (mixi-topic-community ,topic)))))

(defconst mixi-topic-time-regexp
  "<td rowspan=\"3\" width=\"110\" bgcolor=\"#ffd8b0\" align=\"center\" valign=\"top\" nowrap>\\([0-9]+\\)年\\([0-9]+\\)月\\([0-9]+\\)日<br>\\([0-9]+\\):\\([0-9]+\\)</td>")
(defconst mixi-topic-title-regexp
  "<td bgcolor=\"#fff4e0\">&nbsp;\\([^<]+\\)</td>")
;; FIXME: Remove `さん'.
(defconst mixi-topic-owner-regexp
  "<td bgcolor=\"#fdf9f2\">&nbsp;<font color=\"#dfb479\"></font>&nbsp;<a href=\"show_friend\\.pl\\?id=\\([0-9]+\\)\">\\(.+\\)\\(さん\\)?</a>")
(defconst mixi-topic-content-regexp
  "<td class=\"h120\"><table><tr>\\(.+\\)?</tr></table>\\(.+\\)</td>")

(defun mixi-topic-realize (topic)
  "Realize a TOPIC."
  ;; FIXME: Check a expiration of cache?
  (unless (mixi-topic-realize-p topic)
    (with-mixi-retrieve (mixi-topic-page topic)
      (if (string-match mixi-topic-time-regexp buffer)
	  (mixi-topic-set-time
	   topic (encode-time 0 (string-to-number (match-string 5 buffer))
			      (string-to-number (match-string 4 buffer))
			      (string-to-number (match-string 3 buffer))
			      (string-to-number (match-string 2 buffer))
			      (string-to-number (match-string 1 buffer))))
	(signal 'error (list 'cannot-find-time topic)))
      (if (string-match mixi-topic-title-regexp buffer)
	  (mixi-topic-set-title topic (match-string 1 buffer))
	(signal 'error (list 'cannot-find-title topic)))
      (if (string-match mixi-topic-owner-regexp buffer)
	  (mixi-topic-set-owner topic
				(mixi-make-friend
				 (string-to-number (match-string 1 buffer))
				 (match-string 2 buffer)))
	(signal 'error (list 'cannot-find-owner topic)))
      (if (string-match mixi-topic-content-regexp buffer)
	  (mixi-topic-set-content topic (mixi-remove-markup
					 (match-string 2 buffer)))
	(signal 'error (list 'cannot-find-content topic))))
    (mixi-topic-touch topic)))

(defun mixi-topic-realize-p (topic)
  "Return the timestamp of TOPIC."
  (unless (mixi-topic-p topic)
    (signal 'wrong-type-argument (list 'mixi-topic-p topic)))
  (aref (cdr topic) 0))

(defun mixi-topic-community (topic)
  "Return the community of TOPIC."
  (unless (mixi-topic-p topic)
    (signal 'wrong-type-argument (list 'mixi-topic-p topic)))
  (aref (cdr topic) 1))

(defun mixi-topic-id (topic)
  "Return the id of TOPIC."
  (unless (mixi-topic-p topic)
    (signal 'wrong-type-argument (list 'mixi-topic-p topic)))
  (aref (cdr topic) 2))

(defun mixi-topic-time (topic)
  "Return the time of TOPIC."
  (unless (mixi-topic-p topic)
    (signal 'wrong-type-argument (list 'mixi-topic-p topic)))
  (mixi-topic-realize topic)
  (aref (cdr topic) 3))

(defun mixi-topic-title (topic)
  "Return the title of TOPIC."
  (unless (mixi-topic-p topic)
    (signal 'wrong-type-argument (list 'mixi-topic-p topic)))
  (mixi-topic-realize topic)
  (aref (cdr topic) 4))

(defun mixi-topic-owner (topic)
  "Return the owner of TOPIC."
  (unless (mixi-topic-p topic)
    (signal 'wrong-type-argument (list 'mixi-topic-p topic)))
  (mixi-topic-realize topic)
  (aref (cdr topic) 5))

(defun mixi-topic-content (topic)
  "Return the content of TOPIC."
  (unless (mixi-topic-p topic)
    (signal 'wrong-type-argument (list 'mixi-topic-p topic)))
  (mixi-topic-realize topic)
  (aref (cdr topic) 6))

(defun mixi-topic-touch (topic)
  "Set the timestamp of TOPIC."
  (unless (mixi-topic-p topic)
    (signal 'wrong-type-argument (list 'mixi-topic-p topic)))
  (aset (cdr topic) 0 (current-time)))

(defun mixi-topic-set-time (topic time)
  "Set the time of TOPIC."
  (unless (mixi-topic-p topic)
    (signal 'wrong-type-argument (list 'mixi-topic-p topic)))
  (aset (cdr topic) 3 time))

(defun mixi-topic-set-title (topic title)
  "Set the title of TOPIC."
  (unless (mixi-topic-p topic)
    (signal 'wrong-type-argument (list 'mixi-topic-p topic)))
  (aset (cdr topic) 4 title))

(defun mixi-topic-set-owner (topic owner)
  "Set the owner of TOPIC."
  (unless (mixi-topic-p topic)
    (signal 'wrong-type-argument (list 'mixi-topic-p topic)))
  (unless (mixi-friend-p owner)
    (signal 'wrong-type-argument (list 'mixi-friend-p owner)))
  (aset (cdr topic) 5 owner))

(defun mixi-topic-set-content (topic content)
  "Set the content of TOPIC."
  (unless (mixi-topic-p topic)
    (signal 'wrong-type-argument (list 'mixi-topic-p topic)))
  (aset (cdr topic) 6 content))

(defmacro mixi-topic-list-page (community)
  `(concat "/list_bbs.pl?page=%d"
	   "&id=" (number-to-string (mixi-community-id ,community))))

(defconst mixi-topic-list-regexp
  "<a href=view_bbs\\.pl\\?id=\\([0-9]+\\)")

(defvar mixi-topic-max-pages nil)
(defun mixi-get-topics (community)
  "Get topics of COMMUNITY."
  (unless (mixi-community-p community)
    (signal 'wrong-type-argument (list 'mixi-community-p community)))
  (let ((items (mixi-get-matched-items (mixi-topic-list-page community)
				       mixi-topic-max-pages
				       mixi-topic-list-regexp)))
    (mapcar (lambda (item)
	      (mixi-make-topic community (nth 0 item)))
	    items)))

(defmacro mixi-new-topic-list-page ()
  `(concat "/new_bbs.pl?page=%d"))

(defconst mixi-new-topic-list-regexp
  "<a href=\"view_bbs\\.pl\\?id=\\([0-9]+\\)&comment_count=[0-9]+&comm_id=\\([0-9]+\\)\" class=\"new_link\">")

(defvar mixi-new-topic-max-pages nil)
(defun mixi-get-new-topics ()
  "Get new topics."
  (let ((items (mixi-get-matched-items (mixi-new-topic-list-page)
				       mixi-new-topic-max-pages
				       mixi-new-topic-list-regexp)))
    (mapcar (lambda (item)
	      (mixi-make-topic (mixi-make-community (nth 1 item))
			       (nth 0 item)))
	    items)))

;; Comment object.
(defun mixi-make-comment (parent owner time content)
  "Return a comment object."
  (cons 'mixi-comment (vector parent owner time content)))

(defmacro mixi-comment-p (comment)
  `(eq (mixi-object-class ,comment) 'mixi-comment))

(defun mixi-comment-parent (comment)
  "Return the parent of COMMENT."
  (unless (mixi-comment-p comment)
    (signal 'wrong-type-argument (list 'mixi-comment-p comment)))
  (aref (cdr comment) 0))

(defun mixi-comment-owner (comment)
  "Return the owner of COMMENT."
  (unless (mixi-comment-p comment)
    (signal 'wrong-type-argument (list 'mixi-comment-p comment)))
  (aref (cdr comment) 1))

(defun mixi-comment-time (comment)
  "Return the time of COMMENT."
  (unless (mixi-comment-p comment)
    (signal 'wrong-type-argument (list 'mixi-comment-p comment)))
  (aref (cdr comment) 2))

(defun mixi-comment-content (comment)
  "Return the content of COMMENT."
  (unless (mixi-comment-p comment)
    (signal 'wrong-type-argument (list 'mixi-comment-p comment)))
  (aref (cdr comment) 3))

(defun mixi-diary-comment-list-page (diary)
  (concat "/view_diary.pl?page=all"
	  "&id=" (number-to-string (mixi-diary-id diary))
	  "&owner_id=" (number-to-string
			(mixi-friend-id (mixi-diary-owner diary)))))

;; FIXME: Split regexp to time, owner(id and nick) and contents.
(defconst mixi-diary-comment-list-regexp
"<td rowspan=\"2\" align=\"center\" width=\"95\" bgcolor=\"#f2ddb7\" nowrap>
\\([0-9]+\\)年\\([0-9]+\\)月\\([0-9]+\\)日<br>\\([0-9]+\\):\\([0-9]+\\)\\(<br>
<input type=checkbox name=comment_id value=\".+\">
\\|\\)
</td>
<td ALIGN=center BGCOLOR=#FDF9F2 WIDTH=430>
<table border=\"0\" cellspacing=\"0\" cellpadding=\"0\" width=\"410\">
<tr>
<td>
<a href=\"show_friend\\.pl\\?id=\\([0-9]+\\)\">\\(.+\\)</a>

</td>
</tr>
</table>
</td>
</tr>
<!-- 本文 : start -->
<tr>
<td bgcolor=\"#ffffff\">
<table BORDER=0 CELLSPACING=0 CELLPADDING=[35] WIDTH=410>
<tr>
<td CLASS=h12>
\\(.+\\)
</td></tr></table>")

(defun mixi-topic-comment-list-page (topic)
  (concat "/view_bbs.pl?page=all"
	  "&id=" (number-to-string (mixi-topic-id topic))
	  "&comm_id=" (number-to-string
			(mixi-community-id (mixi-topic-community topic)))))

;; FIXME: Split regexp to time, owner(id and nick) and contents.
(defconst mixi-topic-comment-list-regexp
  "<tr valign=\"top\">
<td rowspan=\"2\" width=\"110\" bgcolor=\"#f2ddb7\" align=\"center\" nowrap>
\\([0-9]+\\)年\\([0-9]+\\)月\\([0-9]+\\)日<br>
\\([0-9]+\\):\\([0-9]+\\)<br>
\\(</td>\\)
<td bgcolor=\"#fdf9f2\">&nbsp;<font color=\"#f8a448\">
<b>&nbsp;&nbsp;[0-9]+</b>:</font>&nbsp;

<a href=\"show_friend\\.pl\\?id=\\([0-9]+\\)\">\\(.+\\)</a>



</td>
</tr>
<tr>
<td bgcolor=\"#ffffff\" align=\"center\">
<table border=\"0\" cellspacing=\"0\" cellpadding=\"5\" width=\"500\">
<tr>
<td class=\"h120\">

\\(.+\\)
</td>
</tr>
</table>
</td>
</tr>")

(defun mixi-get-comments (parent)
  "Get comments of PARENT."
  (unless (mixi-object-p parent)
    (signal 'wrong-type-argument (list 'mixi-object-p object)))
  (let* ((name (mixi-object-name parent))
	 (list-page (intern (concat mixi-object-prefix name
				    "-comment-list-page")))
	 (regexp (eval (intern (concat mixi-object-prefix name
				       "-comment-list-regexp")))))
    (let ((items (mixi-get-matched-items (funcall list-page parent) 1 regexp)))
      (mapcar (lambda (item)
		(mixi-make-comment parent (mixi-make-friend
					   (nth 6 item) (nth 7 item))
				   (encode-time 0 (nth 4 item)
						(nth 3 item)
						(nth 2 item)
						(nth 1 item)
						(nth 0 item))
				   (mixi-remove-markup (nth 8 item))))
	      items))))

(defmacro mixi-new-comment-list-page ()
  `(concat "/new_comment.pl?page=%d"))

(defconst mixi-new-comment-list-regexp
  "<a href=\"view_diary\\.pl\\?id=\\([0-9]+\\)&owner_id=\\([0-9]+\\)&comment_count=[0-9]+\" class=\"new_link\">")

(defvar mixi-new-comment-max-pages nil)
(defun mixi-get-new-comments ()
  "Get new comments."
  (let ((items (mixi-get-matched-items (mixi-new-comment-list-page)
				       mixi-new-comment-max-pages
				       mixi-new-comment-list-regexp)))
    (mapcar (lambda (item)
	      (let ((diary (mixi-make-diary
			    (mixi-make-friend (nth 1 item))
			    (nth 0 item))))
		(mixi-get-comments diary)))
	    items)))

;;

;; FIXME: Move to the class method.

;; FIXME: When it got some results, this function doesn't work fine.
(defun mixi-friend-to-id (friend)
  (with-mixi-retrieve (concat "/search.pl?submit=main&nickname="
			      (w3m-url-encode-string friend
						     mixi-coding-system))
    (when (string-match "show_friend\\.pl\\?id=\\([0-9]+\\)" buffer)
      (string-to-number (match-string 1 buffer)))))

;; FIXME: When it got some results, this function doesn't work fine.
(defun mixi-community-to-id (community)
  (with-mixi-retrieve (concat "/search_community.pl?sort="
			      "&type=com&submit=main&keyword="
			      (w3m-url-encode-string community
						     mixi-coding-system))
    (when (string-match "view_community\\.pl\\?id=\\([0-9]+\\)" buffer)
      (string-to-number (match-string 1 buffer)))))

(provide 'mixi)

;;; mixi.el ends here
