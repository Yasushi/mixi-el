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
;;  * mixi-search-diaries
;;  * mixi-get-communities
;;  * mixi-search-communities
;;  * mixi-get-bbses
;;  * mixi-get-new-bbses
;;  * mixi-search-bbses
;;  * mixi-get-comments
;;  * mixi-get-new-comments
;;  * mixi-get-messages
;;  * mixi-get-introductions
;;
;; API for posting:
;;
;;  * mixi-post-diary
;;  * mixi-post-topic
;;  * mixi-post-comment
;;  * mixi-post-message
;; 
;; Utilities:
;;
;;  * mixi-remove-markup

;; Example:
;;
;; Display newest 3 diaries like a mail format.
;;
;; (let ((range 3)
;;       (buffer (get-buffer-create "*temp*"))
;;       (format "%Y/%m/%d %H:%M"))
;;   (pop-to-buffer buffer)
;;   (mapc (lambda (diary)
;; 	  (let ((subject (mixi-diary-title diary))
;; 		(from (mixi-friend-nick (mixi-diary-owner diary)))
;; 		(date (format-time-string format (mixi-diary-time diary)))
;; 		(body (mixi-remove-markup (mixi-diary-content diary))))
;; 	    (insert "From: " from "\n"
;; 		    "Subject: " subject "\n"
;; 		    "Date: " date "\n\n"
;; 		    body "\n\n")))
;; 	(mixi-get-new-diaries range))
;;   (set-buffer-modified-p nil)
;;   (setq buffer-read-only t)
;;   (goto-char (point-min)))
;;
;; Display newest 3 diaries including newest 3 comments like a mail format.
;; Comments are displayed like a reply mail.
;;
;; (let ((range 3)
;;       (buffer (get-buffer-create "*temp*"))
;;       (format "%Y/%m/%d %H:%M"))
;;   (pop-to-buffer buffer)
;;   (mapc (lambda (diary)
;; 	  (let ((subject (mixi-diary-title diary))
;;  		(from (mixi-friend-nick (mixi-diary-owner diary)))
;; 		(date (format-time-string format (mixi-diary-time diary)))
;; 		(body (mixi-remove-markup (mixi-diary-content diary))))
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
;; 			  (body (mixi-remove-markup
;; 				 (mixi-comment-content comment))))
;; 		      (insert "From: " from "\n"
;; 			      "Subject: " subject "\n"
;; 			      "Date: " date "\n\n"
;; 			      body "\n\n")))
;; 		  (mixi-get-comments diary range))))
;; 	(mixi-get-new-diaries range))
;;   (set-buffer-modified-p nil)
;;   (setq buffer-read-only t)
;;   (goto-char (point-min)))

;; Bug reports:
;;
;; If you have bug reports and/or suggestions for improvement, please
;; send them via <URL:http://mixi.jp/view_community.pl?id=1596390>.

;;; Code:

(eval-when-compile (require 'cl))

(defgroup mixi nil
  "API library for accessing to mixi."
  :group 'hypermedia)

(defcustom mixi-url "http://mixi.jp"
  "*The URL of mixi."
  :type 'string
  :group 'mixi)

(defcustom mixi-directory (expand-file-name "~/.mixi")
  "*Where to look for mixi files."
  :type 'directory
  :group 'mixi)

(defcustom mixi-coding-system 'euc-jp
  "*Coding system for mixi."
  :type 'coding-system
  :group 'mixi)

(defcustom mixi-curl-program "curl"
  "*The program name of `curl'."
  :type 'file
  :group 'mixi)

(defcustom mixi-curl-cookie-file (expand-file-name "cookies.txt"
						   mixi-directory)
  "*The location of cookie file created by `curl'."
  :type 'file
  :group 'mixi)

(defcustom mixi-backend
  (or (condition-case nil
	  (progn
	    (require 'w3m)
	    'w3m)
	(error))
      (condition-case nil
	  (progn
	    (require 'url)
	    (if (fboundp 'url-retrieve-synchronously)
		'url))
	(error))
      (if (and (fboundp 'executable-find)
	       (executable-find mixi-curl-program))
	  'curl)
      (error "Cannot set `mixi-backend'."))
  "*The backend for accessing to mixi."
  :type '(radio (const :tag "Use emacs-w3m" w3m)
		(const :tag "Use url.el" url)
		(const :tag "Use curl" curl)
		(symbol :tag "The other backend"))
  :group 'mixi)

(defcustom mixi-default-email nil
  "*Default E-mail address that is used to login automatically."
  :type '(radio (string :tag "E-mail address")
		(const :tag "Asked when it is necessary" nil))
  :group 'mixi)

(defcustom mixi-default-password nil
  "*Default password that is used to login automatically."
  :type '(radio (string :tag "Password")
		(const :tag "Asked when it is necessary" nil))
  :group 'mixi)

(defcustom mixi-accept-adult-contents t
  "*If non-nil, accept to access to the adult contents."
  :type 'boolean
  :group 'mixi)

(defcustom mixi-continuously-access-interval 4.0
  "*Time interval between each mixi access.
Increase this value when unexpected error frequently occurs."
  :type 'number
  :group 'mixi)

(defcustom mixi-cache-expires nil
  "*Seconds for expiration of a cached object."
  :type '(radio (integer :tag "Expired seconds")
		(const :tag "Don't expire" nil)
		(const :tag "Don't cache" t))
  :group 'mixi)

;; FIXME: Not implemented.
(defcustom mixi-cache-use-file t
  "*If non-nil, caches are saved to files."
  :type 'boolean
  :group 'mixi)

(defvar mixi-me nil)

;; Utilities.
(defmacro mixi-message (&rest strings)
  `(concat "[mixi] " ,@strings))

(put 'mixi-realization-error
     'error-message (mixi-message "Cannot realize object"))
(put 'mixi-realization-error
     'error-conditions '(mixi-realization-error error))

(put 'mixi-post-error
     'error-message (mixi-message "Cannot post"))
(put 'mixi-post-error
     'error-conditions '(mixi-post-error error))

(defmacro mixi-realization-error (type object)
  `(let ((data (if (and (boundp 'buffer) debug-on-error)
		   (list ,type ,object buffer)
		 (list ,type ,object))))
     (signal 'mixi-realization-error data)))

(defmacro mixi-post-error (type &optional object)
  `(let ((data (when (and (boundp 'buffer) debug-on-error)
		 (list buffer))))
     (if ,object
	 (setq data (cons ,type (cons ,object data)))
       (setq data (cons ,type data)))
     (signal 'mixi-post-error data)))

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

(defmacro mixi-retrieve (url &optional post-data)
  `(funcall (intern (concat "mixi-" (symbol-name mixi-backend) "-retrieve"))
	    ,url ,post-data))

(defmacro mixi-post-form (url fields)
  `(funcall (intern (concat "mixi-" (symbol-name mixi-backend) "-post-form"))
	    ,url ,fields))

(defun mixi-parse-buffer (url buffer &optional post-data)
  (when (string-match mixi-message-adult-contents buffer)
    (if mixi-accept-adult-contents
	(setq buffer (mixi-retrieve url "submit=agree"))
      (setq buffer (mixi-retrieve (concat url "?")))))
  (when (string-match mixi-warning-continuously-accessing buffer)
    (error (mixi-message "Access denied.  Please wait a while and increase "
			 "the value of `mixi-continuously-access-interval'.")))
  (if (not (string-match mixi-message-continuously-accessing buffer))
      buffer
    (message (mixi-message "Waiting for continuously accessing..."))
    (sleep-for mixi-continuously-access-interval)
    (mixi-retrieve url post-data)))

(defmacro mixi-expand-url (url)
  `(if (string-match (concat "^" mixi-url) ,url)
       ,url
     (concat mixi-url ,url)))

;; FIXME: Support file, checkbox and so on.
(defun mixi-make-form-data (fields)
  "Make form data and return (CONTENT-TYPE . FORM-DATA)."
  (let* ((boundary (apply 'format "--_%d_%d_%d" (current-time)))
	 (content-type (concat "multipart/form-data; boundary=" boundary))
	 (form-data
	  (mapconcat
	   (lambda (field)
	     (concat "--" boundary "\r\n"
		     "Content-Disposition: form-data; name=\""
		     (car field) "\"\r\n"
		     "\r\n"
		     (encode-coding-string (cdr field) mixi-coding-system)))
	   fields "\r\n")))
    (cons content-type (concat form-data "\r\n--" boundary "--"))))

(defun mixi-url-retrieve (url &optional post-data extra-headers)
  "Retrieve the URL and return gotten strings."
  (let* ((url-request-method (if post-data "POST" "GET"))
	 (url-request-data post-data)
	 (url-request-extra-headers extra-headers)
	 (url (mixi-expand-url url))
	 (buffer (url-retrieve-synchronously url))
	 ret)
    (unless (bufferp buffer)
      (error (mixi-message "Cannot retrieve")))
    (with-current-buffer buffer
      (goto-char (point-min))
      (while (looking-at "HTTP/[0-9]+\\.[0-9]+ [13][0-9][0-9]")
	(delete-region (point) (re-search-forward "\r?\n\r?\n")))
      (unless (looking-at "HTTP/[0-9]+\\.[0-9]+ 200")
	(error (mixi-message "Cannot retrieve")))
      (delete-region (point) (re-search-forward "\r?\n\r?\n"))
      (setq ret (decode-coding-string (buffer-string) mixi-coding-system))
      (kill-buffer buffer)
      (mixi-parse-buffer url ret post-data))))

(defun mixi-url-post-form (url fields)
  (let* ((form-data (mixi-make-form-data fields))
	 (extra-headers `(("Content-Type" . ,(car form-data)))))
    (mixi-url-retrieve url (cdr form-data) extra-headers)))

(defun mixi-w3m-retrieve (url &optional post-data)
  "Retrieve the URL and return gotten strings."
  (let ((url (mixi-expand-url url)))
    (with-temp-buffer
      (if (not (string= (w3m-retrieve url nil nil post-data) "text/html"))
	  (error (mixi-message "Cannot retrieve"))
	(w3m-decode-buffer url)
	(let ((ret (buffer-substring-no-properties (point-min) (point-max))))
	  (mixi-parse-buffer url ret post-data))))))

(defun mixi-w3m-post-form (url fields)
  (let ((form-data (mixi-make-form-data fields)))
    (mixi-w3m-retrieve url form-data)))

(defun mixi-curl-retrieve (url &optional post-data)
  "Retrieve the URL and return gotten strings."
  (with-temp-buffer
    (if (fboundp 'set-buffer-multibyte)
	(set-buffer-multibyte nil))
    (let ((orig-mode (default-file-modes))
	  (coding-system-for-read 'binary)
	  (coding-system-for-write 'binary)
	  process ret)
      (unwind-protect
	  (progn
	    (set-default-file-modes 448)
	    (setq process
		  (apply #'start-process "curl" (current-buffer)
			 mixi-curl-program
			 (append (if post-data '("-d" "@-"))
				 (list "-i" "-L" "-s"
				       "-b" mixi-curl-cookie-file
				       "-c" mixi-curl-cookie-file
				       (mixi-expand-url url)))))
	    (set-process-sentinel process #'ignore))
	(set-default-file-modes orig-mode))
      (when post-data
	(process-send-string process (concat post-data "\n"))
	(process-send-eof process))
      (while (eq (process-status process) 'run)
	(accept-process-output process 1))
      (goto-char (point-min))
      (while (looking-at "HTTP/[0-9]+\\.[0-9]+ [13][0-9][0-9]")
	(delete-region (point) (re-search-forward "\r?\n\r?\n")))
      (unless (looking-at "HTTP/[0-9]+\\.[0-9]+ 200")
	(error (mixi-message "Cannot retrieve")))
      (delete-region (point) (re-search-forward "\r?\n\r?\n"))
      (setq ret (decode-coding-string (buffer-string) mixi-coding-system))
      (mixi-parse-buffer url ret post-data))))

(defconst mixi-my-id-regexp
  "<a href=\"add_diary\\.pl\\?id=\\([0-9]+\\)")

(defun mixi-login (&optional email password)
  "Login to mixi."
  (when (and (eq mixi-backend 'w3m) (not w3m-use-cookies))
    (error (mixi-message "Require to accept cookies.  Please set "
			 "`w3m-use-cookies' to t.")))
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
	  (setq mixi-me (mixi-make-friend (match-string 1 buffer)))
	(error (mixi-message "Cannot login"))))))

(defun mixi-logout ()
  (mixi-retrieve "/logout.pl"))

(defmacro with-mixi-retrieve (url &rest body)
  `(let (buffer)
     (when ,url
       (setq buffer (mixi-retrieve ,url))
       (when (string-match "<form action=\"login\\.pl\" method=\"post\">"
			   buffer)
	 (mixi-login)
	 (setq buffer (mixi-retrieve ,url))))
     ,@body))
(put 'with-mixi-retrieve 'lisp-indent-function 'defun)
(put 'with-mixi-retrieve 'edebug-form-spec '(form body))

(defmacro with-mixi-post-form (url fields &rest body)
  `(let (buffer)
     (when ,url
       (setq buffer (mixi-post-form ,url ,fields))
       (when (string-match "<form action=\"login\\.pl\" method=\"post\">"
			   buffer)
	 (mixi-login)
	 (setq buffer (mixi-post-form ,url ,fields))))
     ,@body))
(put 'with-mixi-post-form 'lisp-indent-function 'defun)
(put 'with-mixi-post-form 'edebug-form-spec '(form body))

(defun mixi-get-matched-items (url regexp &optional range)
  "Get matched items to REGEXP in URL."
  (let ((page 1)
	ids)
    (catch 'end
      (while (or (null range) (< (length ids) range))
	(with-mixi-retrieve (format url page)
	  (let ((pos 0)
		found)
	    (while (and (string-match regexp buffer pos)
			(or (null range) (< (length ids) range)))
	      (let ((num 1)
		    list)
		(while (match-string num buffer)
		  (setq list (cons (match-string num buffer) list))
		  (incf num))
		(when (not (member (reverse list) ids))
		  (setq found t)
		  (setq ids (cons (reverse list) ids)))
		(setq pos (match-end (1- num)))))
	    (when (not found)
	      (throw 'end ids))))
	(incf page)))
    (reverse ids)))

;; stolen (and modified) from shimbun.el
(defun mixi-remove-markup (string)
  "Remove markups from STRING."
  (with-temp-buffer
    (insert (or string ""))
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
    ;; FIXME: Decode entities.
    (buffer-string)))

;; stolen (and modified) from w3m.el
;; FIXME: Hmm.
(defun mixi-url-encode-and-quote-percent-string (string)
  (apply (function concat)
	 (mapcar
	  (lambda (char)
	    (cond
	     ((eq char ?\n)		; newline
	      "%%0D%%0A")
	     ((string-match "[-a-zA-Z0-9_:/.]" (char-to-string char)) ; xxx?
	      (char-to-string char))	; printable
	     ((char-equal char ?\x20)	; space
	      "+")
	     (t
	      (format "%%%%%02x" char))))	; escape
	  ;; Coerce a string into a list of chars.
	  (append (encode-coding-string (or string "") mixi-coding-system)
		  nil))))

;; Object.
(defconst mixi-object-prefix "mixi-")

(defmacro mixi-object-class (object)
  `(car-safe ,object))

(defmacro mixi-object-p (object)
  `(let ((class (mixi-object-class ,object)))
     (when (symbolp class)
       (eq (string-match (concat "^" mixi-object-prefix)
			 (symbol-name class)) 0))))

(defun mixi-object-name (object)
  "Return the name of OBJECT."
  (unless (mixi-object-p object)
    (signal 'wrong-type-argument (list 'mixi-object-p object)))
  (let ((class (mixi-object-class object)))
    (substring (symbol-name class) (length mixi-object-prefix))))

(defun mixi-read-object (exp)
  "Read one Lisp expression as mixi object."
  (if (mixi-object-p exp)
      (let ((func (intern (concat mixi-object-prefix "make-"
				  (mixi-object-name exp))))
	    (args (mapcar (lambda (arg)
			    (mixi-read-object arg))
			  (cdr exp))))
	(let ((object (apply func (cdr args))))
	  (when (car args)
	    (mixi-object-set-timestamp object (car args)))
	  object))
    exp))

(defun mixi-object-timestamp (object)
  "Return the timestamp of OJBECT."
  (unless (mixi-object-p object)
    (signal 'wrong-type-argument (list 'mixi-object-p object)))
  (aref (cdr object) 0))
(defalias 'mixi-object-realized-p 'mixi-object-timestamp)

(defun mixi-object-owner (object)
  "Return the owner of OBJECT."
  (unless (mixi-object-p object)
    (signal 'wrong-type-argument (list 'mixi-object-p object)))
  (let ((func (intern (concat mixi-object-prefix
			      (mixi-object-name object) "-owner"))))
    (funcall func object)))

(defun mixi-object-id (object)
  "Return the id of OBJECT."
  (unless (mixi-object-p object)
    (signal 'wrong-type-argument (list 'mixi-object-p object)))
  (let ((func (intern (concat mixi-object-prefix
			      (mixi-object-name object) "-id"))))
    (funcall func object)))

(defun mixi-object-time (object)
  "Return the time of OBJECT."
  (unless (mixi-object-p object)
    (signal 'wrong-type-argument (list 'mixi-object-p object)))
  (let ((func (intern (concat mixi-object-prefix
			      (mixi-object-name object) "-time"))))
    (funcall func object)))

(defun mixi-object-title (object)
  "Return the title of OBJECT."
  (unless (mixi-object-p object)
    (signal 'wrong-type-argument (list 'mixi-object-p object)))
  (let ((func (intern (concat mixi-object-prefix
			      (mixi-object-name object) "-title"))))
    (funcall func object)))

(defun mixi-object-content (object)
  "Return the content of OBJECT."
  (unless (mixi-object-p object)
    (signal 'wrong-type-argument (list 'mixi-object-p object)))
  (let ((func (intern (concat mixi-object-prefix
			      (mixi-object-name object) "-content"))))
    (funcall func object)))

(defun mixi-object-set-timestamp (object timestamp)
  "Set the timestamp of OBJECT."
  (unless (mixi-object-p object)
    (signal 'wrong-type-argument (list 'mixi-object-p object)))
  (aset (cdr object) 0 timestamp))

(defmacro mixi-object-touch (object)
  `(mixi-object-set-timestamp ,object (current-time)))

(defconst mixi-object-url-regexp
  "/\\(show\\|view\\)_\\([a-z]+\\)\\.pl")

(defun mixi-make-object-from-url (url)
  "Return a mixi object from URL."
  (if (string-match mixi-object-url-regexp url)
      (let ((name (match-string 2 url)))
	(when (string= name "bbs")
	  (setq name "topic"))
	(let ((func (intern (concat mixi-object-prefix "make-" name
				    "-from-url"))))
	  (funcall func url)))
    (when (string-match "/home\\.pl" url)
      (mixi-make-friend-from-url url))))

;; Cache.
;; stolen from time-date.el
(defun mixi-time-less-p (t1 t2)
  "Say whether time value T1 is less than time value T2."
  (unless (numberp (cdr t1))
    (setq t1 (cons (car t1) (car (cdr t1)))))
  (unless (numberp (cdr t2))
    (setq t2 (cons (car t2) (car (cdr t2)))))
  (or (< (car t1) (car t2))
      (and (= (car t1) (car t2))
	   (< (cdr t1) (cdr t2)))))

(defun mixi-time-add (t1 t2)
  "Add two time values.  One should represent a time difference."
  (unless (numberp (cdr t1))
    (setq t1 (cons (car t1) (car (cdr t1)))))
  (unless (numberp (cdr t2))
    (setq t2 (cons (car t2) (car (cdr t2)))))
  (let ((low (+ (cdr t1) (cdr t2))))
    (cons (+ (car t1) (car t2) (lsh low -16)) low)))

;; stolen from time-date.el
(defun mixi-seconds-to-time (seconds)
  "Convert SECONDS (a floating point number) to a time value."
  (cons (floor seconds 65536)
	(floor (mod seconds 65536))))

(defun mixi-cache-expired-p (object)
  "Whether a cache of OBJECT is expired."
  (let ((timestamp (mixi-object-timestamp object)))
    (unless (or (null mixi-cache-expires)
		 (null timestamp))
      (if (numberp mixi-cache-expires)
	  (mixi-time-less-p
	   (mixi-time-add timestamp (mixi-seconds-to-time mixi-cache-expires))
	   (current-time))
	t))))

(defun mixi-make-cache (key value table)
  "Make a cache object and return it."
  (let ((cache (gethash key table)))
    (if (and cache (not (mixi-cache-expired-p cache)))
	cache
      (puthash key value table))))

(defconst mixi-cache-file-regexp "[a-z]+-cache$")
(defconst mixi-cache-regexp (concat mixi-object-prefix
				    mixi-cache-file-regexp))

(defun mixi-save-cache ()
  (let ((cache-directory (expand-file-name "cache" mixi-directory)))
    (unless (file-directory-p cache-directory)
      (make-directory cache-directory t))
    (let ((caches (apropos-internal mixi-cache-regexp 'boundp)))
      (mapc (lambda (symbol)
	      (with-temp-file (expand-file-name
			       (substring (symbol-name symbol)
					  (length mixi-object-prefix))
			       cache-directory)
		(let ((coding-system-for-write mixi-coding-system)
		      (cache (symbol-value symbol)))
		  (insert "#s(hash-table size "
			  (number-to-string (hash-table-count cache))
			  " test equal data (\n")
		  (maphash
		   (lambda (key value)
		     (let (print-level print-length)
		       (insert (prin1-to-string key) " "
			       (prin1-to-string value) "\n")))
		   cache))
		(insert "))")))
	    caches))))

;; stolen (and modified) from lsdb.el
(defun mixi-read-cache (&optional marker)
  "Read one Lisp expression as text from MARKER, return as Lisp object."
  (save-excursion
    (goto-char marker)
    (if (looking-at "^#s(")
	(let ((end-marker
	       (progn
		 (forward-char 2);skip "#s"
		 (forward-sexp);move to the left paren
		 (point-marker))))
	  (with-temp-buffer
	    (buffer-disable-undo)
	    (insert-buffer-substring (marker-buffer marker)
				     marker end-marker)
	    (goto-char (point-min))
	    (delete-char 2)
	    (let ((object (read (current-buffer)))
		  data)
	      (if (eq 'hash-table (car object))
		  (progn
		    (setq data (plist-get (cdr object) 'data))
		    (while data
		      (pop data);throw it away
		      (mixi-read-object (pop data))))
		object))))
      (read marker))))

(defun mixi-load-cache ()
  (let ((cache-directory (expand-file-name "cache" mixi-directory)))
    (when (file-directory-p cache-directory)
      ;; FIXME: Load friend and community first.
      (let ((files (directory-files cache-directory t
				    mixi-cache-file-regexp)))
	(mapc (lambda (file)
		(let ((buffer (find-file-noselect file)))
		  (unwind-protect
		      (save-excursion
			(set-buffer buffer)
			(goto-char (point-min))
			(re-search-forward "^#s(")
			(goto-char (match-beginning 0))
			(mixi-read-cache (point-marker)))
		    (kill-buffer buffer))))
	      files)))))

;; Friend object.
(defvar mixi-friend-cache (make-hash-table :test 'equal))
(defun mixi-make-friend (id &optional nick name sex address age birthday
			    blood-type birthplace hobby job organization
			    profile)
  "Return a friend object."
  (mixi-make-cache id (cons 'mixi-friend (vector nil id nick name sex address
						 age birthday blood-type
						 birthplace hobby job
						 organization profile))
		   mixi-friend-cache))

(defun mixi-make-me ()
  "Return a my object."
  (unless mixi-me
    (with-mixi-retrieve "/home.pl"
      (if (string-match mixi-my-id-regexp buffer)
	  (setq mixi-me (mixi-make-friend (match-string 1 buffer)))
	(signal 'error (list 'who-am-i)))))
  mixi-me)

(defconst mixi-friend-url-regexp
  "/show_friend\\.pl\\?id=\\([0-9]+\\)")

(defun mixi-make-friend-from-url (url)
  "Return a friend object from URL."
  (if (string-match mixi-friend-url-regexp url)
      (let ((id (match-string 1 url)))
	(mixi-make-friend id))
    (when (string-match "/home\\.pl" url)
      (mixi-make-me))))

(defmacro mixi-friend-p (friend)
  `(eq (mixi-object-class ,friend) 'mixi-friend))

(defmacro mixi-friend-page (friend)
  `(concat "/show_friend.pl?id=" (mixi-friend-id ,friend)))

(defconst mixi-friend-nick-regexp
  "<img alt=\"\\*\" src=\"http://img\\.mixi\\.jp/img/dot0\\.gif\" width=\"1\" height=\"5\"><br>?
\\(.*\\)さん([0-9]+)")
(defconst mixi-friend-name-regexp
  "<td BGCOLOR=#F2DDB7 WIDTH=80 NOWRAP><font COLOR=#996600>名\\(&nbsp;\\| \\)前</font></td>

?<td WIDTH=345>\\(.+?\\)\\(</td>\\|<img\\)")
(defconst mixi-friend-sex-regexp
  "<td BGCOLOR=#F2DDB7\\( WIDTH=80 NOWRAP\\)?><font COLOR=#996600>性\\(&nbsp;\\| \\)別</font></td>

?<td WIDTH=345>\\([男女]\\)性\\(</td>\\|<img\\)")
(defconst mixi-friend-address-regexp
  "<td BGCOLOR=#F2DDB7><font COLOR=#996600>現住所</font></td>\n<td>\\(.+\\)\\(\n.+\n\\)?</td></tr>")
(defconst mixi-friend-age-regexp
  "<td BGCOLOR=#F2DDB7><font COLOR=#996600>年\\(&nbsp;\\| \\)齢</font></td>\n<td>\\([0-9]+\\)歳\\(\n.+\n\\)?</td></tr>")
(defconst mixi-friend-birthday-regexp
  "<td BGCOLOR=#F2DDB7><font COLOR=#996600>誕生日</font></td>\n<td>\\([0-9]+\\)月\\([0-9]+\\)日\\(\n.+\n\\)?</td></tr>")
(defconst mixi-friend-blood-type-regexp
  "<td BGCOLOR=#F2DDB7><font COLOR=#996600>血液型</font></td>\n<td>\\([ABO]B?\\)型\\(\n\n\\)?</td></tr>")
(defconst mixi-friend-birthplace-regexp
  "<td BGCOLOR=#F2DDB7><font COLOR=#996600>出身地</font>\n?</td>\n<td>\\(.+\\)\\(\n.+\n\\)?</td></tr>")
(defconst mixi-friend-hobby-regexp
  "<td BGCOLOR=#F2DDB7><font COLOR=#996600>趣\\(&nbsp;\\| \\)味</font></td>\n<td>\\(.+?\\)\\(</td>\\|<img\\)")
(defconst mixi-friend-job-regexp
  "<td BGCOLOR=#F2DDB7><font COLOR=#996600>職\\(&nbsp;\\| \\)業</font></td>\n<td>\\(.+\\)\\(\n.+\n\\)?</td></tr>")
(defconst mixi-friend-organization-regexp
  "<td BGCOLOR=#F2DDB7><font COLOR=#996600>所\\(&nbsp;\\| \\)属</font></td>\n<td[^>]*>\\(.+\\)\\(\n.+\n\\)?</td></tr>")
(defconst mixi-friend-profile-regexp
  "<td BGCOLOR=#F2DDB7><font COLOR=#996600>自己紹介</font></td>\n<td CLASS=h120>\\(.+\\)</td></tr>")

(defun mixi-realize-friend (friend)
  "Realize a FRIEND."
  ;; FIXME: Check a expiration of cache?
  (unless (mixi-object-realized-p friend)
    (let (buf)
      (with-mixi-retrieve (mixi-friend-page friend)
	(setq buf buffer))
      (if (string-match mixi-friend-nick-regexp buf)
	  (mixi-friend-set-nick friend (match-string 1 buf))
	(mixi-realization-error 'cannot-find-nick friend))
      ;; For getting my profile.
      (unless (string-match mixi-friend-name-regexp buf)
	(with-mixi-retrieve (concat "/show_profile.pl?id="
				    (mixi-friend-id friend))
	  (setq buf buffer)))
      (if (string-match mixi-friend-name-regexp buf)
	    (mixi-friend-set-name friend (match-string 2 buf))
	(mixi-realization-error 'cannot-find-name friend))
      (if (string-match mixi-friend-sex-regexp buf)
	  (mixi-friend-set-sex friend
			       (if (string= (match-string 3 buf) "男")
				   'male 'female))
	(mixi-realization-error 'cannot-find-sex friend))
      (when (string-match mixi-friend-address-regexp buf)
	(mixi-friend-set-address friend (match-string 1 buf)))
      (when (string-match mixi-friend-age-regexp buf)
	(mixi-friend-set-age
	 friend (string-to-number (match-string 2 buf))))
      (when (string-match mixi-friend-birthday-regexp buf)
	(mixi-friend-set-birthday
	 friend (list (string-to-number (match-string 1 buf))
		      (string-to-number (match-string 2 buf)))))
      (when (string-match mixi-friend-blood-type-regexp buf)
	(mixi-friend-set-blood-type friend (intern (match-string 1 buf))))
      (when (string-match mixi-friend-birthplace-regexp buf)
	(mixi-friend-set-birthplace friend (match-string 1 buf)))
      (when (string-match mixi-friend-hobby-regexp buf)
	(mixi-friend-set-hobby
	 friend (split-string (match-string 2 buf) ", ")))
      (when (string-match mixi-friend-job-regexp buf)
	(mixi-friend-set-job friend (match-string 2 buf)))
      (when (string-match mixi-friend-organization-regexp buf)
	(mixi-friend-set-organization friend (match-string 2 buf)))
      (when (string-match mixi-friend-profile-regexp buf)
	(mixi-friend-set-profile friend (match-string 1 buf))))
    (mixi-object-touch friend)))

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
    (mixi-realize-friend friend))
  (aref (cdr friend) 2))

(defun mixi-friend-name (friend)
  "Return the name of FRIEND."
  (unless (mixi-friend-p friend)
    (signal 'wrong-type-argument (list 'mixi-friend-p friend)))
  (mixi-realize-friend friend)
  (aref (cdr friend) 3))

(defun mixi-friend-sex (friend)
  "Return the sex of FRIEND."
  (unless (mixi-friend-p friend)
    (signal 'wrong-type-argument (list 'mixi-friend-p friend)))
  (mixi-realize-friend friend)
  (aref (cdr friend) 4))

(defun mixi-friend-address (friend)
  "Return the address of FRIEND."
  (unless (mixi-friend-p friend)
    (signal 'wrong-type-argument (list 'mixi-friend-p friend)))
  (mixi-realize-friend friend)
  (aref (cdr friend) 5))

(defun mixi-friend-age (friend)
  "Return the age of FRIEND."
  (unless (mixi-friend-p friend)
    (signal 'wrong-type-argument (list 'mixi-friend-p friend)))
  (mixi-realize-friend friend)
  (aref (cdr friend) 6))

(defun mixi-friend-birthday (friend)
  "Return the birthday of FRIEND."
  (unless (mixi-friend-p friend)
    (signal 'wrong-type-argument (list 'mixi-friend-p friend)))
  (mixi-realize-friend friend)
  (aref (cdr friend) 7))

(defun mixi-friend-blood-type (friend)
  "Return the blood type of FRIEND."
  (unless (mixi-friend-p friend)
    (signal 'wrong-type-argument (list 'mixi-friend-p friend)))
  (mixi-realize-friend friend)
  (aref (cdr friend) 8))

(defun mixi-friend-birthplace (friend)
  "Return the birthplace of FRIEND."
  (unless (mixi-friend-p friend)
    (signal 'wrong-type-argument (list 'mixi-friend-p friend)))
  (mixi-realize-friend friend)
  (aref (cdr friend) 9))

(defun mixi-friend-hobby (friend)
  "Return the hobby of FRIEND."
  (unless (mixi-friend-p friend)
    (signal 'wrong-type-argument (list 'mixi-friend-p friend)))
  (mixi-realize-friend friend)
  (aref (cdr friend) 10))

(defun mixi-friend-job (friend)
  "Return the job of FRIEND."
  (unless (mixi-friend-p friend)
    (signal 'wrong-type-argument (list 'mixi-friend-p friend)))
  (mixi-realize-friend friend)
  (aref (cdr friend) 11))

(defun mixi-friend-organization (friend)
  "Return the organization of FRIEND."
  (unless (mixi-friend-p friend)
    (signal 'wrong-type-argument (list 'mixi-friend-p friend)))
  (mixi-realize-friend friend)
  (aref (cdr friend) 12))

(defun mixi-friend-profile (friend)
  "Return the pforile of FRIEND."
  (unless (mixi-friend-p friend)
    (signal 'wrong-type-argument (list 'mixi-friend-p friend)))
  (mixi-realize-friend friend)
  (aref (cdr friend) 13))

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

(defun mixi-friend-set-sex (friend sex)
  "Set the sex of FRIEND."
  (unless (mixi-friend-p friend)
    (signal 'wrong-type-argument (list 'mixi-friend-p friend)))
  (aset (cdr friend) 4 sex))

(defun mixi-friend-set-address (friend address)
  "Set the address of FRIEND."
  (unless (mixi-friend-p friend)
    (signal 'wrong-type-argument (list 'mixi-friend-p friend)))
  (aset (cdr friend) 5 address))

(defun mixi-friend-set-age (friend age)
  "Set the age of FRIEND."
  (unless (mixi-friend-p friend)
    (signal 'wrong-type-argument (list 'mixi-friend-p friend)))
  (aset (cdr friend) 6 age))

(defun mixi-friend-set-birthday (friend birthday)
  "Set the birthday of FRIEND."
  (unless (mixi-friend-p friend)
    (signal 'wrong-type-argument (list 'mixi-friend-p friend)))
  (aset (cdr friend) 7 birthday))

(defun mixi-friend-set-blood-type (friend blood-type)
  "Set the blood type of FRIEND."
  (unless (mixi-friend-p friend)
    (signal 'wrong-type-argument (list 'mixi-friend-p friend)))
  (aset (cdr friend) 8 blood-type))

(defun mixi-friend-set-birthplace (friend birthplace)
  "Set the birthplace of FRIEND."
  (unless (mixi-friend-p friend)
    (signal 'wrong-type-argument (list 'mixi-friend-p friend)))
  (aset (cdr friend) 9 birthplace))

(defun mixi-friend-set-hobby (friend hobby)
  "Set the hobby of FRIEND."
  (unless (mixi-friend-p friend)
    (signal 'wrong-type-argument (list 'mixi-friend-p friend)))
  (aset (cdr friend) 10 hobby))

(defun mixi-friend-set-job (friend job)
  "Set the job of FRIEND."
  (unless (mixi-friend-p friend)
    (signal 'wrong-type-argument (list 'mixi-friend-p friend)))
  (aset (cdr friend) 11 job))

(defun mixi-friend-set-organization (friend organization)
  "Set the organization of FRIEND."
  (unless (mixi-friend-p friend)
    (signal 'wrong-type-argument (list 'mixi-friend-p friend)))
  (aset (cdr friend) 12 organization))

(defun mixi-friend-set-profile (friend profile)
  "Set the profile of FRIEND."
  (unless (mixi-friend-p friend)
    (signal 'wrong-type-argument (list 'mixi-friend-p friend)))
  (aset (cdr friend) 13 profile))

(defmacro mixi-friend-list-page (&optional friend)
  `(concat "/list_friend.pl?page=%d"
	   (when ,friend (concat "&id=" (mixi-friend-id ,friend)))))

(defconst mixi-friend-list-id-regexp
  "<a href=show_friend\\.pl\\?id=\\([0-9]+\\)")
(defconst mixi-friend-list-nick-regexp
  "<td valign=middle>\\(.+\\)さん([0-9]+)<br />")

(defun mixi-get-friends (&rest friend-or-range)
  "Get friends of FRIEND."
  (when (> (length friend-or-range) 2)
    (signal 'wrong-number-of-arguments (list 'mixi-get-friends
					     (length friend-or-range))))
  (let ((friend (nth 0 friend-or-range))
	(range (nth 1 friend-or-range)))
    (when (or (not (mixi-friend-p friend)) (mixi-friend-p range))
      (setq friend (nth 1 friend-or-range))
      (setq range (nth 0 friend-or-range)))
    (unless (or (null friend) (mixi-friend-p friend))
      (signal 'wrong-type-argument (list 'mixi-friend-p friend)))
    (let ((ids (mixi-get-matched-items (mixi-friend-list-page friend)
				       mixi-friend-list-id-regexp
				       range))
	  (nicks (mixi-get-matched-items (mixi-friend-list-page friend)
					 mixi-friend-list-nick-regexp
					 range)))
      (let ((index 0)
	    ret)
	(while (< index (length ids))
	  (setq ret (cons (mixi-make-friend (nth 0 (nth index ids))
					    (nth 0 (nth index nicks))) ret))
	  (incf index))
	(reverse ret)))))

;; Favorite.
(defmacro mixi-favorite-list-page ()
  `(concat "/list_bookmark.pl?page=%d"))

(defconst mixi-favorite-list-id-regexp
  "<td ALIGN=center BGCOLOR=#FDF9F2 width=330><a href=\"show_friend\\.pl\\?id=\\([0-9]+\\)\">")
(defconst mixi-favorite-list-nick-regexp
  "<td BGCOLOR=#FDF9F2><font COLOR=#996600>名&nbsp;&nbsp;前</font></td>
<td COLSPAN=2 BGCOLOR=#FFFFFF>\\(.+\\)</td></tr>")

(defun mixi-get-favorites (&optional range)
  "Get favorites."
  (let ((ids (mixi-get-matched-items (mixi-favorite-list-page)
				     mixi-favorite-list-id-regexp
				     range))
	(nicks (mixi-get-matched-items (mixi-favorite-list-page)
				       mixi-favorite-list-nick-regexp
				       range)))
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
  "\\([0-9]+\\)年\\([0-9]+\\)月\\([0-9]+\\)日 \\([0-9]+\\):\\([0-9]+\\) <a href=\"show_friend\\.pl\\?id=\\([0-9]+\\)\">\\(.*\\)</a></li>")

(defun mixi-get-logs (&optional range)
  "Get logs."
  (let ((items (mixi-get-matched-items (mixi-log-list-page)
				       mixi-log-list-regexp
				       range)))
    (mapcar (lambda (item)
	      (mixi-make-log (mixi-make-friend (nth 5 item) (nth 6 item))
			     (encode-time 0
					  (string-to-number (nth 4 item))
					  (string-to-number (nth 3 item))
					  (string-to-number (nth 2 item))
					  (string-to-number (nth 1 item))
					  (string-to-number (nth 0 item)))))
	    items)))

;; Diary object.
(defvar mixi-diary-cache (make-hash-table :test 'equal))
(defun mixi-make-diary (owner id &optional time title content)
  "Return a diary object."
  (let ((owner (or owner (mixi-make-me))))
    (mixi-make-cache (list (mixi-friend-id owner) id)
		     (cons 'mixi-diary (vector nil owner id time title
					       content))
		     mixi-diary-cache)))

(defconst mixi-diary-url-regexp
  "/view_diary\\.pl\\?id=\\([0-9]+\\)&owner_id=\\([0-9]+\\)")

(defun mixi-make-diary-from-url (url)
  "Return a diary object from URL."
  (when (string-match mixi-diary-url-regexp url)
    (let ((id (match-string 1 url))
	  (owner-id (match-string 2 url)))
      (mixi-make-diary (mixi-make-friend owner-id) id))))

(defmacro mixi-diary-p (diary)
  `(eq (mixi-object-class ,diary) 'mixi-diary))

(defmacro mixi-diary-page (diary)
  `(concat "/view_diary.pl?id=" (mixi-diary-id ,diary)
	   "&owner_id=" (mixi-friend-id (mixi-diary-owner ,diary))))

(defconst mixi-diary-closed-regexp
  "<td>友人\\(の友人\\)?まで公開のため読むことが出来ません。</td></tr>")
(defconst mixi-diary-owner-nick-regexp
  "<td WIDTH=490 background=http://img\\.mixi\\.jp/img/bg_w\\.gif><b><font COLOR=#605048>\\(.+?\\)\\(さん\\)?の日記</font></b></td>")
(defconst mixi-diary-time-regexp
  "<td \\(align\\|ALIGN\\)=\"?center\"? \\(rowspan\\|ROWSPAN\\)=\"?[23]\"? \\(nowrap=\"nowrap\"\\|NOWRAP\\) \\(width\\|WIDTH\\)=\"?95\"? bgcolor=\"?#FFD8B0\"?>\\([0-9]+\\)年\\([0-9]+\\)月\\([0-9]+\\)日\\(<br />\\|<br>\\)\\([0-9]+\\):\\([0-9]+\\)</td>")
(defconst mixi-diary-title-regexp
  "<td \\(bgcolor\\|BGCOLOR\\)=\"?#FFF4E0\"? width=\"?430\"?>&nbsp;\\([^<]+\\)</td>")
(defconst mixi-diary-content-regexp
  "<td \\(class\\|CLASS\\)=\"?h12\"?>\\(.*\\)</td>")

(defun mixi-realize-diary (diary)
  "Realize a DIARY."
  ;; FIXME: Check a expiration of cache?
  (unless (mixi-object-realized-p diary)
    (with-mixi-retrieve (mixi-diary-page diary)
      (unless (string-match mixi-diary-closed-regexp buffer)
	(if (string-match mixi-diary-owner-nick-regexp buffer)
	    (mixi-friend-set-nick (mixi-diary-owner diary)
				  (match-string 1 buffer))
	  (mixi-realization-error 'cannot-find-owner-nick diary))
	(if (string-match mixi-diary-time-regexp buffer)
	    (mixi-diary-set-time
	     diary (encode-time 0 (string-to-number (match-string 10 buffer))
				(string-to-number (match-string 9 buffer))
				(string-to-number (match-string 7 buffer))
				(string-to-number (match-string 6 buffer))
				(string-to-number (match-string 5 buffer))))
	  (mixi-realization-error 'cannot-find-time diary))
	(if (string-match mixi-diary-title-regexp buffer)
	    (mixi-diary-set-title diary (match-string 2 buffer))
	  (mixi-realization-error 'cannot-find-title diary))
	(if (string-match mixi-diary-content-regexp buffer)
	    (mixi-diary-set-content diary (match-string 2 buffer))
	  (mixi-realization-error 'cannot-find-content diary))))
    (mixi-object-touch diary)))

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
  (mixi-realize-diary diary)
  (aref (cdr diary) 3))

(defun mixi-diary-title (diary)
  "Return the title of DIARY."
  (unless (mixi-diary-p diary)
    (signal 'wrong-type-argument (list 'mixi-diary-p diary)))
  (mixi-realize-diary diary)
  (aref (cdr diary) 4))

(defun mixi-diary-content (diary)
  "Return the content of DIARY."
  (unless (mixi-diary-p diary)
    (signal 'wrong-type-argument (list 'mixi-diary-p diary)))
  (mixi-realize-diary diary)
  (aref (cdr diary) 5))

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
	   (when ,friend (concat "&id=" (mixi-friend-id ,friend)))))

(defconst mixi-diary-list-regexp
  "<a href=\"view_diary\\.pl\\?id=\\([0-9]+\\)&owner_id=[0-9]+\">")

(defun mixi-get-diaries (&rest friend-or-range)
  "Get diaries of FRIEND."
  (when (> (length friend-or-range) 2)
    (signal 'wrong-number-of-arguments
	    (list 'mixi-get-diaries (length friend-or-range))))
  (let ((friend (nth 0 friend-or-range))
	(range (nth 1 friend-or-range)))
    (when (or (not (mixi-friend-p friend)) (mixi-friend-p range))
      (setq friend (nth 1 friend-or-range))
      (setq range (nth 0 friend-or-range)))
    (unless (or (null friend) (mixi-friend-p friend))
      (signal 'wrong-type-argument (list 'mixi-friend-p friend)))
    (let ((items (mixi-get-matched-items (mixi-diary-list-page friend)
					 mixi-diary-list-regexp
					 range)))
      (mapcar (lambda (item)
		(mixi-make-diary friend (nth 0 item)))
	      items))))

(defmacro mixi-new-diary-list-page ()
  `(concat "/new_friend_diary.pl?page=%d"))

(defconst mixi-new-diary-list-regexp
  "<a class=\"new_link\" href=view_diary\\.pl\\?id=\\([0-9]+\\)&owner_id=\\([0-9]+\\)>")

(defun mixi-get-new-diaries (&optional range)
  "Get new diaries."
  (let ((items (mixi-get-matched-items (mixi-new-diary-list-page)
				       mixi-new-diary-list-regexp
				       range)))
    (mapcar (lambda (item)
	      (mixi-make-diary (mixi-make-friend (nth 1 item)) (nth 0 item)))
	    items)))

(defmacro mixi-search-diary-list-page (keyword)
  `(concat "/search_diary.pl?page=%d&submit=search&keyword="
	     (mixi-url-encode-and-quote-percent-string keyword)))

(defconst mixi-search-diary-list-regexp
  "<a href=\"view_diary\\.pl\\?id=\\([0-9]+\\)&owner_id=\\([0-9]+\\)\">")

(defun mixi-search-diaries (keyword &optional range)
  (let ((items (mixi-get-matched-items (mixi-search-diary-list-page keyword)
				       mixi-search-diary-list-regexp
				       range)))
    (mapcar (lambda (item)
	      (mixi-make-diary (mixi-make-friend (nth 1 item)) (nth 0 item)))
	    items)))

(defmacro mixi-post-diary-page ()
  `(concat "/add_diary.pl"))

(defconst mixi-post-key-regexp
  "<input type=\"?hidden\"? name=\"?post_key\"? value=\"\\([a-z0-9]+\\)\">")
(defconst mixi-post-succeed-regexp
  "<b>\\(作成\\|書き込み\\)が完了しました。反映に時間がかかることがありますので、表示されていない場合は少々お待ちください。</b>")

;; FIXME: Support photos.
(defun mixi-post-diary (title content)
  "Post a diary."
  (unless (stringp title)
    (signal 'wrong-type-argument (list 'stringp title)))
  (unless (stringp content)
    (signal 'wrong-type-argument (list 'stringp content)))
  (let ((fields `(("id" . ,(mixi-friend-id (mixi-make-me)))
		  ("diary_title" . ,title)
		  ("diary_body" . ,content)
		  ("submit" . "main")))
	post-key)
    (with-mixi-post-form (mixi-post-diary-page) fields
      (if (string-match mixi-post-key-regexp buffer)
	  (setq post-key (match-string 1 buffer))
	(mixi-post-error 'cannot-find-key)))
    (setq fields `(("post_key" . ,post-key)
		   ("id" . ,(mixi-friend-id (mixi-make-me)))
		   ("diary_title" . ,title)
		   ("diary_body" . ,content)
		   ("submit" . "confirm")))
    (with-mixi-post-form (mixi-post-diary-page) fields
      (unless (string-match mixi-post-succeed-regexp buffer)
	(mixi-post-error 'cannot-find-succeed)))))

;; Community object.
(defvar mixi-community-cache (make-hash-table :test 'equal))
(defun mixi-make-community (id &optional name birthday owner category members
			       open-level authority description)
  "Return a community object."
  (mixi-make-cache id (cons 'mixi-community (vector nil id name birthday owner
						    category members
						    open-level authority
						    description))
		   mixi-community-cache))

(defconst mixi-community-url-regexp
  "/view_community\\.pl\\?id=\\([0-9]+\\)")

(defun mixi-make-community-from-url (url)
  "Return a community object from URL."
  (when (string-match mixi-community-url-regexp url)
    (let ((id (match-string 1 url)))
      (mixi-make-community id))))

(defmacro mixi-community-p (community)
  `(eq (mixi-object-class ,community) 'mixi-community))

(defmacro mixi-community-page (community)
  `(concat "/view_community.pl?id=" (mixi-community-id ,community)))

;; FIXME: Not community specific.
(defconst mixi-community-nodata-regexp
  "^データがありません")
(defconst mixi-community-name-regexp
  "<td WIDTH=345>\\(.*\\)</td></tr>")
(defconst mixi-community-birthday-regexp
  "<td BGCOLOR=#F2DDB7><font COLOR=#996600>開設日</font></td>
<td>\\([0-9]+\\)年\\([0-9]+\\)月\\([0-9]+\\)日</td>")
;; FIXME: Care when the owner has seceded.
(defconst mixi-community-owner-regexp
  "<td BGCOLOR=#F2DDB7><font COLOR=#996600>管理人</font></td>
<td>

<a href=\"\\(home\\.pl\\|show_friend\\.pl\\?id=\\([0-9]+\\)\\)\">
\\(.*\\)</a>")
(defconst mixi-community-category-regexp
  "<td BGCOLOR=#F2DDB7><font COLOR=#996600>カテゴリ</font></td>
<td>\\([^<]+\\)</td>")
(defconst mixi-community-members-regexp
  "<td BGCOLOR=#F2DDB7><font COLOR=#996600>メンバー数</font></td>
<td>\\([0-9]+\\)人</td></tr>")
(defconst mixi-community-open-level-regexp
  "<td BGCOLOR=#F2DDB7><font COLOR=#996600>参加条件と<br>公開レベル</font></td>
<td>\\(.+\\)</td></tr>")
(defconst mixi-community-authority-regexp
  "<td BGCOLOR=#F2DDB7><font COLOR=#996600>トピック作成の権限</font></td>
<td>\\(.+\\)</td></tr>")
(defconst mixi-community-description-regexp
  "<td CLASS=h120>\\(.+\\)</td>")

(defun mixi-realize-community (community)
  "Realize a COMMUNITY."
  ;; FIXME: Check a expiration of cache?
  (unless (mixi-object-realized-p community)
    (with-mixi-retrieve (mixi-community-page community)
      (if (string-match mixi-community-nodata-regexp buffer)
	  ;; FIXME: Set all members?
	  (mixi-community-set-name community "データがありません")
	(if (string-match mixi-community-name-regexp buffer)
	    (mixi-community-set-name community (match-string 1 buffer))
	  (mixi-realization-error 'cannot-find-name community))
	(if (string-match mixi-community-birthday-regexp buffer)
	    (mixi-community-set-birthday
	     community
	     (encode-time 0 0 0 (string-to-number (match-string 3 buffer))
			  (string-to-number (match-string 2 buffer))
			  (string-to-number (match-string 1 buffer))))
	  (mixi-realization-error 'cannot-find-birthday community))
	(if (string-match mixi-community-owner-regexp buffer)
	    (if (string= (match-string 1 buffer) "home.pl")
		(mixi-community-set-owner community (mixi-make-me))
	      (mixi-community-set-owner
	       community (mixi-make-friend (match-string 2 buffer)
					   (match-string 3 buffer))))
	  (mixi-realization-error 'cannot-find-owner community))
	(if (string-match mixi-community-category-regexp buffer)
	    (mixi-community-set-category community (match-string 1 buffer))
	  (mixi-realization-error 'cannot-find-category community))
	(if (string-match mixi-community-members-regexp buffer)
	    (mixi-community-set-members
	     community (string-to-number (match-string 1 buffer)))
	  (mixi-realization-error 'cannot-find-members community))
	(if (string-match mixi-community-open-level-regexp buffer)
	    (mixi-community-set-open-level community (match-string 1 buffer))
	  (mixi-realization-error 'cannot-find-open-level community))
	(if (string-match mixi-community-authority-regexp buffer)
	    (mixi-community-set-authority community (match-string 1 buffer))
	  (mixi-realization-error 'cannot-find-authority community))
	(if (string-match mixi-community-description-regexp buffer)
	    (mixi-community-set-description community (match-string 1 buffer))
	  (mixi-realization-error 'cannot-find-description community))))
    (mixi-object-touch community)))

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
    (mixi-realize-community community))
  (aref (cdr community) 2))

(defun mixi-community-birthday (community)
  "Return the birthday of COMMUNITY."
  (unless (mixi-community-p community)
    (signal 'wrong-type-argument (list 'mixi-community-p community)))
  (mixi-realize-community community)
  (aref (cdr community) 3))

(defun mixi-community-owner (community)
  "Return the owner of COMMUNITY."
  (unless (mixi-community-p community)
    (signal 'wrong-type-argument (list 'mixi-community-p community)))
  (mixi-realize-community community)
  (aref (cdr community) 4))

(defun mixi-community-category (community)
  "Return the category of COMMUNITY."
  (unless (mixi-community-p community)
    (signal 'wrong-type-argument (list 'mixi-community-p community)))
  (mixi-realize-community community)
  (aref (cdr community) 5))

(defun mixi-community-members (community)
  "Return the members of COMMUNITY."
  (unless (mixi-community-p community)
    (signal 'wrong-type-argument (list 'mixi-community-p community)))
  (mixi-realize-community community)
  (aref (cdr community) 6))

(defun mixi-community-open-level (community)
  "Return the open-level of COMMUNITY."
  (unless (mixi-community-p community)
    (signal 'wrong-type-argument (list 'mixi-community-p community)))
  (mixi-realize-community community)
  (aref (cdr community) 7))

(defun mixi-community-authority (community)
  "Return the authority of COMMUNITY."
  (unless (mixi-community-p community)
    (signal 'wrong-type-argument (list 'mixi-community-p community)))
  (mixi-realize-community community)
  (aref (cdr community) 8))

(defun mixi-community-description (community)
  "Return the description of COMMUNITY."
  (unless (mixi-community-p community)
    (signal 'wrong-type-argument (list 'mixi-community-p community)))
  (mixi-realize-community community)
  (aref (cdr community) 9))

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

(defun mixi-community-set-members (community members)
  "Set the name of COMMUNITY."
  (unless (mixi-community-p community)
    (signal 'wrong-type-argument (list 'mixi-community-p community)))
  (aset (cdr community) 6 members))

(defun mixi-community-set-open-level (community open-level)
  "Set the name of COMMUNITY."
  (unless (mixi-community-p community)
    (signal 'wrong-type-argument (list 'mixi-community-p community)))
  (aset (cdr community) 7 open-level))

(defun mixi-community-set-authority (community authority)
  "Set the name of COMMUNITY."
  (unless (mixi-community-p community)
    (signal 'wrong-type-argument (list 'mixi-community-p community)))
  (aset (cdr community) 8 authority))

(defun mixi-community-set-description (community description)
  "Set the name of COMMUNITY."
  (unless (mixi-community-p community)
    (signal 'wrong-type-argument (list 'mixi-community-p community)))
  (aset (cdr community) 9 description))

(defmacro mixi-community-list-page (&optional friend)
  `(concat "/list_community.pl?page=%d"
	   (when ,friend (concat "&id=" (mixi-friend-id ,friend)))))

(defconst mixi-community-list-id-regexp
  "<a href=view_community\\.pl\\?id=\\([0-9]+\\)")
(defconst mixi-community-list-name-regexp
  "<td valign=middle>\\(.+\\)([0-9]+)</td>")

(defun mixi-get-communities (&rest friend-or-range)
  "Get communities of FRIEND."
  (when (> (length friend-or-range) 2)
    (signal 'wrong-number-of-arguments
	    (list 'mixi-get-communities (length friend-or-range))))
  (let ((friend (nth 0 friend-or-range))
	(range (nth 1 friend-or-range)))
    (when (or (not (mixi-friend-p friend)) (mixi-friend-p range))
      (setq friend (nth 1 friend-or-range))
      (setq range (nth 0 friend-or-range)))
    (unless (or (null friend) (mixi-friend-p friend))
      (signal 'wrong-type-argument (list 'mixi-friend-p friend)))
    (let ((ids (mixi-get-matched-items (mixi-community-list-page friend)
				       mixi-community-list-id-regexp
				       range))
	  (names (mixi-get-matched-items (mixi-community-list-page friend)
					 mixi-community-list-name-regexp
					 range)))
      (let ((index 0)
	    ret)
	(while (< index (length ids))
	  (setq ret (cons (mixi-make-community (nth 0 (nth index ids))
					       (nth 0 (nth index names))) ret))
	  (incf index))
	(reverse ret)))))

(defmacro mixi-search-community-list-page (keyword)
  `(concat "/search_community.pl?page=%d&&sort=date&type=com&submit=main"
	   "&keyword=" (mixi-url-encode-and-quote-percent-string keyword)
	   "&category_id=0"))

(defconst mixi-search-community-list-regexp
  "<td WIDTH=90 VALIGN=top ROWSPAN=4 ALIGN=center background=http://img\\.mixi\\.jp/img/bg_line\\.gif><a href=\"view_community\\.pl\\?id=\\([0-9]+\\)\"><img SRC=\"http://img-c[0-9]+\\.mixi\\.jp/photo/comm/[^.]+\\.jpg\" VSPACE=3 border=0></a></td>
<td NOWRAP WIDTH=90 BGCOLOR=#FDF9F2><font COLOR=#996600>コミュニティ名</font></td>
<td COLSPAN=2 WIDTH=370 BGCOLOR=#FFFFFF>\\([^<]+\\)</td></tr>")

;; FIXME: Support category.
(defun mixi-search-communities (keyword &optional range)
  (let ((items (mixi-get-matched-items (mixi-search-community-list-page
					keyword)
				       mixi-search-community-list-regexp
				       range)))
    (mapcar (lambda (item)
	      (mixi-make-community (nth 0 item) (nth 1 item)))
	    items)))

;; Topic object.
(defvar mixi-topic-cache (make-hash-table :test 'equal))
(defun mixi-make-topic (community id &optional time title owner content)
  "Return a topic object."
  (mixi-make-cache (list (mixi-community-id community) id)
		   (cons 'mixi-topic (vector nil community id time title owner
					     content))
		   mixi-topic-cache))

(defconst mixi-topic-url-regexp
  "/view_bbs\\.pl\\?id=\\([0-9]+\\)\\(&comment_count=[0-9]+\\)?&comm_id=\\([0-9]+\\)")

(defun mixi-make-topic-from-url (url)
  "Return a topic object from URL."
  (when (string-match mixi-topic-url-regexp url)
    (let ((id (match-string 1 url))
	  (community-id (match-string 3 url)))
      (mixi-make-topic (mixi-make-community community-id) id))))

(defmacro mixi-topic-p (topic)
  `(eq (mixi-object-class ,topic) 'mixi-topic))

(defmacro mixi-topic-page (topic)
  `(concat "/view_bbs.pl?id=" (mixi-topic-id ,topic)
	   "&comm_id=" (mixi-community-id (mixi-topic-community ,topic))))

(defconst mixi-topic-community-regexp
  "<td width=\"595\" background=\"http://img\\.mixi\\.jp/img/bg_w\\.gif\"><b>\\[\\(.+\\)\\] トピック</b></td>")
(defconst mixi-topic-time-regexp
  "<td rowspan=\"3\" width=\"110\" bgcolor=\"#ffd8b0\" align=\"center\" valign=\"top\" nowrap>\\([0-9]+\\)年\\([0-9]+\\)月\\([0-9]+\\)日<br>\\([0-9]+\\):\\([0-9]+\\)</td>")
(defconst mixi-topic-title-regexp
  "<td bgcolor=\"#fff4e0\">&nbsp;\\([^<]+\\)</td>")
(defconst mixi-topic-owner-regexp
  "<td bgcolor=\"#fdf9f2\">&nbsp;<font color=\"#dfb479\"></font>&nbsp;<a href=\"show_friend\\.pl\\?id=\\([0-9]+\\)\">\\(.*?\\)\\(さん\\)?</a>")
(defconst mixi-topic-content-regexp
  "<table width=\"500\" border=\"0\" cellspacing=\"0\" cellpadding=\"5\"><tr><td class=\"h120\"><table><tr>\\(<td width=\"130\" height=\"140\" align=\"center\" valign=\"middle\"><a href=\"javascript:void(0)\" onClick=\"MM_openBrWindow('show_bbs_picture\\.pl\\?id=[0-9]+&comm_id=[0-9]+&number=[0-9]+','pict','width=680,height=660,toolbar=no,scrollbars=yes,left=5,top=5')\"><img src=\"http://ic[0-9]+\\.mixi\\.jp/[^.]+\\.jpg\" border=\"0\"></a></td>\n\\)*</tr></table>\\(.+\\)</td></tr></table>")

(defun mixi-realize-topic (topic)
  "Realize a TOPIC."
  ;; FIXME: Check a expiration of cache?
  (unless (mixi-object-realized-p topic)
    (with-mixi-retrieve (mixi-topic-page topic)
      (if (string-match mixi-topic-community-regexp buffer)
	  (mixi-community-set-name (mixi-topic-community topic)
				   (match-string 1 buffer))
	(mixi-realization-error 'cannot-find-community topic))
      (if (string-match mixi-topic-time-regexp buffer)
	  (mixi-topic-set-time
	   topic (encode-time 0 (string-to-number (match-string 5 buffer))
			      (string-to-number (match-string 4 buffer))
			      (string-to-number (match-string 3 buffer))
			      (string-to-number (match-string 2 buffer))
			      (string-to-number (match-string 1 buffer))))
	(mixi-realization-error 'cannot-find-time topic))
      (if (string-match mixi-topic-title-regexp buffer)
	  (mixi-topic-set-title topic (match-string 1 buffer))
	(mixi-realization-error 'cannot-find-title topic))
      (if (string-match mixi-topic-owner-regexp buffer)
	  (mixi-topic-set-owner topic
				(mixi-make-friend (match-string 1 buffer)
						  (match-string 2 buffer)))
	(mixi-realization-error 'cannot-find-owner topic))
      (if (string-match mixi-topic-content-regexp buffer)
	  (mixi-topic-set-content topic (match-string 2 buffer))
	(mixi-realization-error 'cannot-find-content topic)))
    (mixi-object-touch topic)))

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
  (mixi-realize-topic topic)
  (aref (cdr topic) 3))

(defun mixi-topic-title (topic)
  "Return the title of TOPIC."
  (unless (mixi-topic-p topic)
    (signal 'wrong-type-argument (list 'mixi-topic-p topic)))
  (mixi-realize-topic topic)
  (aref (cdr topic) 4))

(defun mixi-topic-owner (topic)
  "Return the owner of TOPIC."
  (unless (mixi-topic-p topic)
    (signal 'wrong-type-argument (list 'mixi-topic-p topic)))
  (mixi-realize-topic topic)
  (aref (cdr topic) 5))

(defun mixi-topic-content (topic)
  "Return the content of TOPIC."
  (unless (mixi-topic-p topic)
    (signal 'wrong-type-argument (list 'mixi-topic-p topic)))
  (mixi-realize-topic topic)
  (aref (cdr topic) 6))

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

(defmacro mixi-post-topic-page (community)
  `(concat "/add_bbs.pl?id=" (mixi-community-id community)))

;; FIXME: Support photos.
(defun mixi-post-topic (community title content)
  "Post a topic to COMMUNITY."
  (unless (mixi-community-p community)
    (signal 'wrong-type-argument (list 'mixi-community-p community)))
  (unless (stringp title)
    (signal 'wrong-type-argument (list 'stringp title)))
  (unless (stringp content)
    (signal 'wrong-type-argument (list 'stringp content)))
  (let ((fields `(("bbs_title" . ,title)
		  ("bbs_body" . ,content)
		  ("submit" . "main")))
	post-key)
    (with-mixi-post-form (mixi-post-topic-page community) fields
      (if (string-match mixi-post-key-regexp buffer)
	  (setq post-key (match-string 1 buffer))
	(mixi-post-error 'cannot-find-key community)))
    (setq fields `(("post_key" . ,post-key)
		   ("bbs_title" . ,title)
		   ("bbs_body" . ,content)
		   ("submit" . "confirm")))
    (with-mixi-post-form (mixi-post-topic-page community) fields
      (unless (string-match mixi-post-succeed-regexp buffer)
	(mixi-post-error 'cannot-find-succeed community)))))

;; Event object.
(defvar mixi-event-cache (make-hash-table :test 'equal))
(defun mixi-make-event (community id &optional time title owner date place
				  detail limit members)
  "Return a event object."
  (mixi-make-cache (list (mixi-community-id community) id)
		   (cons 'mixi-event (vector nil community id time title owner
					     date place detail limit members))
		   mixi-event-cache))

(defconst mixi-event-url-regexp
  "/view_event\\.pl\\?id=\\([0-9]+\\)\\(&comment_count=[0-9]+\\)?&comm_id=\\([0-9]+\\)")

(defun mixi-make-event-from-url (url)
  "Return a event object from URL."
  (when (string-match mixi-event-url-regexp url)
    (let ((id (match-string 1 url))
	  (community-id (match-string 3 url)))
      (mixi-make-event (mixi-make-community community-id) id))))

(defmacro mixi-event-p (event)
  `(eq (mixi-object-class ,event) 'mixi-event))

(defmacro mixi-event-page (event)
  `(concat "/view_event.pl?id=" (mixi-event-id ,event)
	   "&comm_id=" (mixi-community-id (mixi-event-community ,event))))

(defconst mixi-event-community-regexp
  "<td WIDTH=595 background=http://img\\.mixi\\.jp/img/bg_w\\.gif><b>\\[\\(.+\\)\\] イベント</b></td>")
(defconst mixi-event-time-regexp
  "<td ROWSPAN=11 \\(BGCOLOR\\|bgcolor\\)=#FFD8B0 \\(ALIGN\\|align\\)=center \\(VALIGN\\|Valign\\)=top WIDTH=110>
?\\([0-9]+\\)年\\([0-9]+\\)月\\([0-9]+\\)日<br>
?\\([0-9]+\\):\\([0-9]+\\)</td>")
(defconst mixi-event-title-regexp
  "<td bgcolor=#FFF4E0\\( width=410\\)?>&nbsp;\\([^<]+\\)</td>")
(defconst mixi-event-owner-regexp
  "<td \\(BGCOLOR\\|bgcolor\\)=#FDF9F2>&nbsp;<a href=\"show_friend\\.pl\\?id=\\([0-9]+\\)\">\\(.*\\)</a>")
(defconst mixi-event-owner-seceded-regexp
  "<td \\(BGCOLOR\\|bgcolor\\)=#FDF9F2>&nbsp;\\((mixi 退会済)\\)")
(defconst mixi-event-date-regexp
  "<td \\(BGCOLOR\\|bgcolor\\)=#\\(FFFFFF\\|ffffff\\) \\(ALIGN\\|align\\)=center NOWRAP>開催日時</td>
<td \\(BGCOLOR\\|bgcolor\\)=#\\(FFFFFF\\|ffffff\\)>
&nbsp;\\(.+\\)
</td>")
(defconst mixi-event-place-regexp
  "<td \\(BGCOLOR\\|bgcolor\\)=#\\(FFFFFF\\|ffffff\\) \\(ALIGN\\|align\\)=center NOWRAP>開催場所</td>
<td \\(BGCOLOR\\|bgcolor\\)=#\\(FFFFFF\\|ffffff\\)>
&nbsp;\\(.+\\)
</td>")
(defconst mixi-event-detail-regexp
  "<td \\(BGCOLOR\\|bgcolor\\)=#\\(FFFFFF\\|ffffff\\) \\(ALIGN\\|align\\)=center NOWRAP>詳細</td>
<td \\(BGCOLOR\\|bgcolor\\)=#\\(FFFFFF\\|ffffff\\)><table BORDER=0 CELLSPACING=0 CELLPADDING=5><tr><td CLASS=h120>\\(.+\\)</td></tr></table></td>")
(defconst mixi-event-limit-regexp
  "<td \\(BGCOLOR\\|bgcolor\\)=\"?#\\(FFFFFF\\|ffffff\\)\"? \\(ALIGN\\|align\\)=\"?center\"? NOWRAP>募集期限</td>
?<td \\(BGCOLOR\\|bgcolor\\)=#\\(FFFFFF\\|ffffff\\)>&nbsp;\\([0-9]+\\)年\\([0-9]+\\)月\\([0-9]+\\)日</td>")
(defconst mixi-event-members-regexp
  "<td \\(BGCOLOR\\|bgcolor\\)=#\\(FFFFFF\\|ffffff\\) \\(ALIGN\\|align\\)=center NOWRAP>参加者</td>
<td \\(BGCOLOR\\|bgcolor\\)=#\\(FFFFFF\\|ffffff\\)>

?
?<table BORDER=0 CELLSPACING=0 CELLPADDING=0 WIDTH=100%>
<tr>

?<td>&nbsp;\\(.+\\)</td>")

(defun mixi-realize-event (event)
  "Realize a EVENT."
  ;; FIXME: Check a expiration of cache?
  (unless (mixi-object-realized-p event)
    (with-mixi-retrieve (mixi-event-page event)
      (if (string-match mixi-event-community-regexp buffer)
	  (mixi-community-set-name (mixi-event-community event)
				   (match-string 1 buffer))
	(mixi-realization-error 'cannot-find-community event))
      (if (string-match mixi-event-time-regexp buffer)
	  (mixi-event-set-time
	   event (encode-time 0 (string-to-number (match-string 8 buffer))
			      (string-to-number (match-string 7 buffer))
			      (string-to-number (match-string 6 buffer))
			      (string-to-number (match-string 5 buffer))
			      (string-to-number (match-string 4 buffer))))
	(mixi-realization-error 'cannot-find-time event))
      (if (string-match mixi-event-title-regexp buffer)
	  (mixi-event-set-title event (match-string 2 buffer))
	(mixi-realization-error 'cannot-find-title event))
      (if (string-match mixi-event-owner-regexp buffer)
	  (mixi-event-set-owner event
				(mixi-make-friend (match-string 2 buffer)
						  (match-string 3 buffer)))
	(if (string-match mixi-event-owner-seceded-regexp buffer)
	    (mixi-event-set-owner event
				  (mixi-make-friend nil
						    (match-string 2 buffer)))
	  (mixi-realization-error 'cannot-find-owner event)))
      (if (string-match mixi-event-date-regexp buffer)
	  (mixi-event-set-date event (match-string 6 buffer))
	(mixi-realization-error 'cannot-find-date event))
      (if (string-match mixi-event-place-regexp buffer)
	  (mixi-event-set-place event (match-string 6 buffer))
	(mixi-realization-error 'cannot-find-place event))
      (if (string-match mixi-event-detail-regexp buffer)
	  (mixi-event-set-detail event (match-string 6 buffer))
	(mixi-realization-error 'cannot-find-detail event))
      (when (string-match mixi-event-limit-regexp buffer)
	(mixi-event-set-limit
	 event (encode-time 0 0 0 (string-to-number (match-string 8 buffer))
			    (string-to-number (match-string 7 buffer))
			    (string-to-number (match-string 6 buffer)))))
      (if (string-match mixi-event-members-regexp buffer)
	  (mixi-event-set-members event (match-string 6 buffer))
	(mixi-realization-error 'cannot-find-members event)))
    (mixi-object-touch event)))

(defun mixi-event-community (event)
  "Return the community of EVENT."
  (unless (mixi-event-p event)
    (signal 'wrong-type-argument (list 'mixi-event-p event)))
  (aref (cdr event) 1))

(defun mixi-event-id (event)
  "Return the id of EVENT."
  (unless (mixi-event-p event)
    (signal 'wrong-type-argument (list 'mixi-event-p event)))
  (aref (cdr event) 2))

(defun mixi-event-time (event)
  "Return the time of EVENT."
  (unless (mixi-event-p event)
    (signal 'wrong-type-argument (list 'mixi-event-p event)))
  (mixi-realize-event event)
  (aref (cdr event) 3))

(defun mixi-event-title (event)
  "Return the title of EVENT."
  (unless (mixi-event-p event)
    (signal 'wrong-type-argument (list 'mixi-event-p event)))
  (mixi-realize-event event)
  (aref (cdr event) 4))

(defun mixi-event-owner (event)
  "Return the owner of EVENT."
  (unless (mixi-event-p event)
    (signal 'wrong-type-argument (list 'mixi-event-p event)))
  (mixi-realize-event event)
  (aref (cdr event) 5))

(defun mixi-event-date (event)
  "Return the date of EVENT."
  (unless (mixi-event-p event)
    (signal 'wrong-type-argument (list 'mixi-event-p event)))
  (mixi-realize-event event)
  (aref (cdr event) 6))

(defun mixi-event-place (event)
  "Return the place of EVENT."
  (unless (mixi-event-p event)
    (signal 'wrong-type-argument (list 'mixi-event-p event)))
  (mixi-realize-event event)
  (aref (cdr event) 7))

(defun mixi-event-detail (event)
  "Return the detail of EVENT."
  (unless (mixi-event-p event)
    (signal 'wrong-type-argument (list 'mixi-event-p event)))
  (mixi-realize-event event)
  (aref (cdr event) 8))

(defun mixi-event-limit (event)
  "Return the limit of EVENT."
  (unless (mixi-event-p event)
    (signal 'wrong-type-argument (list 'mixi-event-p event)))
  (mixi-realize-event event)
  (aref (cdr event) 9))

(defun mixi-event-members (event)
  "Return the members of EVENT."
  (unless (mixi-event-p event)
    (signal 'wrong-type-argument (list 'mixi-event-p event)))
  (mixi-realize-event event)
  (aref (cdr event) 10))

(defun mixi-event-set-time (event time)
  "Set the time of EVENT."
  (unless (mixi-event-p event)
    (signal 'wrong-type-argument (list 'mixi-event-p event)))
  (aset (cdr event) 3 time))

(defun mixi-event-set-title (event title)
  "Set the title of EVENT."
  (unless (mixi-event-p event)
    (signal 'wrong-type-argument (list 'mixi-event-p event)))
  (aset (cdr event) 4 title))

(defun mixi-event-set-owner (event owner)
  "Set the owner of EVENT."
  (unless (mixi-event-p event)
    (signal 'wrong-type-argument (list 'mixi-event-p event)))
  (unless (mixi-friend-p owner)
    (signal 'wrong-type-argument (list 'mixi-friend-p owner)))
  (aset (cdr event) 5 owner))

(defun mixi-event-set-date (event date)
  "Set the date of EVENT."
  (unless (mixi-event-p event)
    (signal 'wrong-type-argument (list 'mixi-event-p event)))
  (aset (cdr event) 6 date))

(defun mixi-event-set-place (event place)
  "Set the place of EVENT."
  (unless (mixi-event-p event)
    (signal 'wrong-type-argument (list 'mixi-event-p event)))
  (aset (cdr event) 7 place))

(defun mixi-event-set-detail (event detail)
  "Set the detail of EVENT."
  (unless (mixi-event-p event)
    (signal 'wrong-type-argument (list 'mixi-event-p event)))
  (aset (cdr event) 8 detail))

(defun mixi-event-set-limit (event limit)
  "Set the limit of EVENT."
  (unless (mixi-event-p event)
    (signal 'wrong-type-argument (list 'mixi-event-p event)))
  (aset (cdr event) 9 limit))

(defun mixi-event-set-members (event members)
  "Set the members of EVENT."
  (unless (mixi-event-p event)
    (signal 'wrong-type-argument (list 'mixi-event-p event)))
  (aset (cdr event) 10 members))

;; Bbs object.
(defconst mixi-bbs-list '(mixi-topic mixi-event))

(defmacro mixi-bbs-p (object)
  `(when (memq (mixi-object-class ,object) mixi-bbs-list)
     t))

(defun mixi-bbs-community (object)
  "Return the community of OBJECT."
  (unless (mixi-bbs-p object)
    (signal 'wrong-type-argument (list 'mixi-bbs-p object)))
  (let ((func (intern (concat mixi-object-prefix
			      (mixi-object-name object) "-community"))))
    (funcall func object)))

(defalias 'mixi-bbs-id 'mixi-object-id)
(defalias 'mixi-bbs-time 'mixi-object-time)
(defalias 'mixi-bbs-title 'mixi-object-title)
(defalias 'mixi-bbs-owner 'mixi-object-owner)
(defalias 'mixi-bbs-content 'mixi-object-content)

(defmacro mixi-bbs-list-page (community)
  `(concat "/list_bbs.pl?page=%d"
	   "&id=" (mixi-community-id ,community)))

(defconst mixi-bbs-list-regexp
  "<a href=view_\\(bbs\\|event\\)\\.pl\\?id=\\([0-9]+\\)")

(defun mixi-get-bbses (community &optional range)
  "Get bbese of COMMUNITY."
  (unless (mixi-community-p community)
    (signal 'wrong-type-argument (list 'mixi-community-p community)))
  (let ((items (mixi-get-matched-items (mixi-bbs-list-page community)
				       mixi-bbs-list-regexp
				       range)))
    (mapcar (lambda (item)
	      (let ((name (nth 0 item)))
		(when (string= name "bbs")
		  (setq name "topic"))
		(let ((func (intern (concat "mixi-make-" name))))
		  (funcall func community (nth 1 item)))))
	    items)))

(defmacro mixi-new-bbs-list-page ()
  `(concat "/new_bbs.pl?page=%d"))

(defconst mixi-new-bbs-list-regexp
  "<a href=\"view_\\(bbs\\|event\\)\\.pl\\?id=\\([0-9]+\\)&comment_count=[0-9]+&comm_id=\\([0-9]+\\)\" class=\"new_link\">")

(defun mixi-get-new-bbses (&optional range)
  "Get new topics."
  (let ((items (mixi-get-matched-items (mixi-new-bbs-list-page)
				       mixi-new-bbs-list-regexp
				       range)))
    (mapcar (lambda (item)
	      (let ((name (nth 0 item)))
		(when (string= name "bbs")
		  (setq name "topic"))
		(let ((func (intern (concat "mixi-make-" name))))
		  (funcall func (mixi-make-community (nth 2 item))
			   (nth 1 item)))))
	    items)))

(defmacro mixi-search-bbs-list-page (keyword)
  `(concat "/search_topic.pl?page=%d&type=top&submit=search"
	   "&keyword=" (mixi-url-encode-and-quote-percent-string keyword)
	   "&community_id=0&category_id=0"))

(defconst mixi-search-bbs-list-regexp
  "<a href=\"view_\\(bbs\\|event\\)\\.pl\\?id=\\([0-9]+\\)&comm_id=\\([0-9]+\\)\"><img src=http://img\\.mixi\\.jp/img/shbtn\\.gif ALT=詳細を見る BORDER=0 WIDTH=104 HEIGHT=19></a>")

;; FIXME: Support community and category.
(defun mixi-search-bbses (keyword &optional range)
  (let ((items (mixi-get-matched-items (mixi-search-bbs-list-page keyword)
				       mixi-search-bbs-list-regexp
				       range)))
    (mapcar (lambda (item)
	      (let ((name (nth 0 item)))
		(when (string= name "bbs")
		  (setq name "topic"))
		(let ((func (intern (concat "mixi-make-" name))))
		  (funcall func (mixi-make-community (nth 2 item))
			   (nth 1 item)))))
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
  (concat "/view_diary.pl?full=1"
	  "&id=" (mixi-diary-id diary)
	  "&owner_id=" (mixi-friend-id (mixi-diary-owner diary))))

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
\\(<td>\\)
<a href=\"show_friend\\.pl\\?id=\\([0-9]+\\)\">\\(.*\\)</a>

\\(<font color=\"#f2ddb7\">|</font> <a href=[^>]+>削除</a>

\\|\\)</td>
</tr>
</table>
</td>
</tr>
<!-- [^ ]+ : start -->
<tr>
<td bgcolor=\"#ffffff\">
<table BORDER=0 CELLSPACING=0 CELLPADDING=[35] WIDTH=410>
<tr>
<td CLASS=h12>
\\(.+\\)
</td></tr></table>")

(defun mixi-topic-comment-list-page (topic)
  (concat "/view_bbs.pl?page=all"
	  "&id=" (mixi-topic-id topic)
	  "&comm_id=" (mixi-community-id (mixi-topic-community topic))))

;; FIXME: Split regexp to time, owner(id and nick) and contents.
(defconst mixi-topic-comment-list-regexp
  "<tr valign=\"top\">
<td rowspan=\"2\" width=\"110\" bgcolor=\"#f2ddb7\" align=\"center\" nowrap>
\\([0-9]+\\)年\\([0-9]+\\)月\\([0-9]+\\)日<br>
\\([0-9]+\\):\\([0-9]+\\)<br>
\\(<input type=\"checkbox\" name=\"comment_id\" value=\".+\">
\\|\\)</td>
<td bgcolor=\"#fdf9f2\">&nbsp;<font color=\"#f8a448\">
<b>[^<]+</b>:</font>&nbsp;
\\(
\\|\\) *<a href=\"show_friend\\.pl\\?id=\\([0-9]+\\)\">\\(.*\\)</a>

?\\(

\\|<font color=\"#f2ddb7\">|&nbsp;</font><a href=\"delete_bbs_comment\\.pl\\?id=[0-9]+&comm_id=[0-9]+&comment_id=[0-9]+\">削除</a>
\\|\\)</td>
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

(defun mixi-event-comment-list-page (event)
  (concat "/view_event.pl?page=all"
	  "&id=" (mixi-event-id event)
	  "&comm_id=" (mixi-community-id (mixi-event-community event))))

;; FIXME: Split regexp to time, owner(id and nick) and contents.
(defconst mixi-event-comment-list-regexp
  "<tr>
<td ROWSPAN=2 ALIGN=center BGCOLOR=#F2DDB7 WIDTH=110>
\\([0-9]+\\)年\\([0-9]+\\)月\\([0-9]+\\)日<br>
\\([0-9]+\\):\\([0-9]+\\)<br>
\\(</td>\\)
\\(<td BGCOLOR=#FDF9F2>\\)
<font COLOR=#F8A448><b>[^<]+</b> :</font>
<a HREF=\"show_friend\\.pl\\?id=\\([0-9]+\\)\">\\(.*\\)</a>

\\(<font COLOR=#F2DDB7>|</font>
<a href=\"delete_bbs_comment\\.pl\\?id=[0-9]+&comm_id=[0-9]+&comment_id=[0-9]+&type=event\">削除</a>

\\|\\)</td>
</tr>
<tr>
<td ALIGN=center BGCOLOR=#FFFFFF>
<table BORDER=0 CELLSPACING=0 CELLPADDING=5 WIDTH=500>
<tr><td CLASS=h120>\\(.+\\)</td></tr>
</table>
</td>
</tr>")

(defun mixi-get-comments (parent &optional range)
  "Get comments of PARENT."
  (unless (mixi-object-p parent)
    (signal 'wrong-type-argument (list 'mixi-object-p parent)))
  (let* ((name (mixi-object-name parent))
	 (list-page (intern (concat mixi-object-prefix name
				    "-comment-list-page")))
	 (regexp (eval (intern (concat mixi-object-prefix name
				       "-comment-list-regexp")))))
    (let ((items (mixi-get-matched-items
		  (funcall list-page parent) regexp)))
      (let (list)
	(catch 'stop
	  (mapc (lambda (item)
		  (when (and (numberp range)
			     (>= (length list) range))
		    (throw 'stop nil))
		  (setq list (cons item list)))
	        (reverse items)))
	(setq items (reverse list)))
      (mapcar (lambda (item)
		(mixi-make-comment parent (mixi-make-friend
					   (nth 7 item) (nth 8 item))
				   (encode-time
				    0
				    (string-to-number (nth 4 item))
				    (string-to-number (nth 3 item))
				    (string-to-number (nth 2 item))
				    (string-to-number (nth 1 item))
				    (string-to-number (nth 0 item)))
				   (nth 10 item)))
	      items))))

(defmacro mixi-new-comment-list-page ()
  `(concat "/new_comment.pl?page=%d"))

(defconst mixi-new-comment-list-regexp
  "<a href=\"view_diary\\.pl\\?id=\\([0-9]+\\)&owner_id=\\([0-9]+\\)&comment_count=[0-9]+\" class=\"new_link\">")

(defun mixi-get-new-comments (&optional range)
  "Get new comments."
  (let ((items (mixi-get-matched-items (mixi-new-comment-list-page)
				       mixi-new-comment-list-regexp
				       range)))
    (mapcar (lambda (item)
	      (mixi-make-diary (mixi-make-friend (nth 1 item)) (nth 0 item)))
	    items)))

(defun mixi-post-diary-comment-page (diary)
  (concat "/add_comment.pl?&diary_id=" (mixi-diary-id diary)))

(defun mixi-post-topic-comment-page (topic)
  (concat "/add_bbs_comment.pl?id=" (mixi-topic-id topic)
	  "&comm_id=" (mixi-community-id (mixi-topic-community topic))))

(defun mixi-post-event-comment-page (event)
  (concat "/add_event_comment.pl?id=" (mixi-event-id event)
	  "&comm_id=" (mixi-community-id (mixi-event-community event))))

;; FIXME: Support photos.
(defun mixi-post-comment (parent content)
  "Post a comment to PARENT."
  (unless (mixi-object-p parent)
    (signal 'wrong-type-argument (list 'mixi-object-p parent)))
  (unless (stringp content)
    (signal 'wrong-type-argument (list 'stringp content)))
  (let* ((name (mixi-object-name parent))
	 (page (intern (concat mixi-object-prefix "post-" name
			       "-comment-page")))
	 fields post-key)
    (if (mixi-diary-p parent)
	(setq fields
	      `(("owner_id" . ,(mixi-friend-id (mixi-diary-owner parent)))
		("comment_body" . ,content)))
      (setq fields `(("comment" . ,content))))
    (with-mixi-post-form (funcall page parent) fields
      (if (string-match mixi-post-key-regexp buffer)
	  (setq post-key (match-string 1 buffer))
	(mixi-post-error 'cannot-find-key parent)))
    (if (mixi-diary-p parent)
	(setq fields
	      `(("post_key" . ,post-key)
		("owner_id" . ,(mixi-friend-id (mixi-diary-owner parent)))
		("comment_body" . ,content)
		("submit" . "confirm")))
      (setq fields `(("post_key" . ,post-key)
		     ("comment" . ,content)
		     ("submit" . "confirm"))))
    (with-mixi-post-form (funcall page parent) fields
      (unless (string-match mixi-post-succeed-regexp buffer)
	(mixi-post-error 'cannot-find-succeed parent)))))

;; Message object.
(defconst mixi-message-box-list '(inbox outbox savebox thrash)) ; thrash?

(defmacro mixi-message-box-p (box)
  `(when (memq ,box mixi-message-box-list)
     t))

(defun mixi-message-box-name (box)
  "Return the name of BOX."
  (unless (mixi-message-box-p box)
    (signal 'wrong-type-argument (list 'mixi-message-box-p box)))
  (symbol-name box))

(defvar mixi-message-cache (make-hash-table :test 'equal))
(defun mixi-make-message (id box &optional owner title time content)
  "Return a message object."
  (mixi-make-cache (list id box)
		   (cons 'mixi-message (vector nil id box owner title time
					       content))
		   mixi-message-cache))

(defconst mixi-message-url-regexp
  "/view_message\\.pl\\?id=\\([a-z0-9]+\\)&box=\\([a-z]+\\)")

(defun mixi-make-message-from-url (url)
  "Return a message object from URL."
  (when (string-match mixi-message-url-regexp url)
    (let ((id (match-string 1 url))
	  (box (match-string 2 url)))
      (mixi-make-message id box))))

(defmacro mixi-message-p (message)
  `(eq (mixi-object-class ,message) 'mixi-message))

(defmacro mixi-message-page (message)
  `(concat "/view_message.pl?id=" (mixi-message-id ,message)
	   "&box=" (mixi-message-box ,message)))

(defconst mixi-message-owner-regexp
  "<font COLOR=#996600>\\(差出人\\|宛&nbsp;先\\)</font>&nbsp;:&nbsp;<a HREF=\"show_friend\\.pl\\?id=\\([0-9]+\\)\">\\(.*\\)\\(</a>\\|</td>\\)")
(defconst mixi-message-title-regexp
"<font COLOR=#996600>件\\(　\\|&nbsp;\\)名</font>&nbsp;:&nbsp;\\(.+\\)\n?</td>")
(defconst mixi-message-time-regexp
"<font COLOR=#996600>日\\(　\\|&nbsp;\\)付</font>&nbsp;:&nbsp;\\([0-9]+\\)年\\([0-9]+\\)月\\([0-9]+\\)日 \\([0-9]+\\)時\\([0-9]+\\)分&nbsp;&nbsp;")
(defconst mixi-message-content-regexp
  "<tr><td CLASS=h120>\\(.+\\)</td></tr>")

(defun mixi-realize-message (message)
  "Realize a MESSAGE."
  (unless (mixi-object-realized-p message)
    (with-mixi-retrieve (mixi-message-page message)
      (if (string-match mixi-message-owner-regexp buffer)
	  (mixi-message-set-owner message
				  (mixi-make-friend (match-string 2 buffer)
						    (match-string 3 buffer)))
	(mixi-realization-error 'cannot-find-owner message))
      (if (string-match mixi-message-title-regexp buffer)
	  (mixi-message-set-title message (match-string 2 buffer))
	(mixi-realization-error 'cannot-find-title message))
      (if (string-match mixi-message-time-regexp buffer)
	  (mixi-message-set-time
	   message (encode-time 0 (string-to-number (match-string 6 buffer))
				(string-to-number (match-string 5 buffer))
				(string-to-number (match-string 4 buffer))
				(string-to-number (match-string 3 buffer))
				(string-to-number (match-string 2 buffer))))
	(mixi-realization-error 'cannot-find-time message))
      (if (string-match mixi-message-content-regexp buffer)
	  (mixi-message-set-content message (match-string 1 buffer))
	(mixi-realization-error 'cannot-find-content message)))
    (mixi-object-touch message)))

(defun mixi-message-id (message)
  "Return the id of MESSAGE."
  (unless (mixi-message-p message)
    (signal 'wrong-type-argument (list 'mixi-message-p message)))
  (aref (cdr message) 1))

(defun mixi-message-box (message)
  "Return the box of MESSAGE."
  (unless (mixi-message-p message)
    (signal 'wrong-type-argument (list 'mixi-message-p message)))
  (aref (cdr message) 2))

(defun mixi-message-owner (message)
  "Return the owner of MESSAGE."
  (unless (mixi-message-p message)
    (signal 'wrong-type-argument (list 'mixi-message-p message)))
  (mixi-realize-message message)
  (aref (cdr message) 3))

(defun mixi-message-title (message)
  "Return the title of MESSAGE."
  (unless (mixi-message-p message)
    (signal 'wrong-type-argument (list 'mixi-message-p message)))
  (mixi-realize-message message)
  (aref (cdr message) 4))

(defun mixi-message-time (message)
  "Return the date of MESSAGE."
  (unless (mixi-message-p message)
    (signal 'wrong-type-argument (list 'mixi-message-p message)))
  (mixi-realize-message message)
  (aref (cdr message) 5))

(defun mixi-message-content (message)
  "Return the content of MESSAGE."
  (unless (mixi-message-p message)
    (signal 'wrong-type-argument (list 'mixi-message-p message)))
  (mixi-realize-message message)
  (aref (cdr message) 6))

(defun mixi-message-set-owner (message owner)
  "Set the owner of MESSAGE."
  (unless (mixi-message-p message)
    (signal 'wrong-type-argument (list 'mixi-message-p message)))
  (aset (cdr message) 3 owner))

(defun mixi-message-set-title (message title)
  "Set the title of MESSAGE."
  (unless (mixi-message-p message)
    (signal 'wrong-type-argument (list 'mixi-message-p message)))
  (aset (cdr message) 4 title))

(defun mixi-message-set-time (message time)
  "Set the date of MESSAGE."
  (unless (mixi-message-p message)
    (signal 'wrong-type-argument (list 'mixi-message-p message)))
  (aset (cdr message) 5 time))

(defun mixi-message-set-content (message content)
  "Set the content of MESSAGE."
  (unless (mixi-message-p message)
    (signal 'wrong-type-argument (list 'mixi-message-p message)))
  (aset (cdr message) 6 content))

(defmacro mixi-message-list-page (&optional box)
  `(concat "/list_message.pl?page=%d"
	   (when ,box (concat "&box=" ,box))))

(defconst mixi-message-list-regexp
  "<td><a HREF=\"view_message\\.pl\\?id=\\(.+\\)&box=\\(.+\\)\">")

(defun mixi-get-messages (&rest box-or-range)
  "Get messages of BOX."
  (when (> (length box-or-range) 2)
    (signal 'wrong-number-of-arguments
	    (list 'mixi-get-messages (length box-or-range))))
  (let ((box (nth 0 box-or-range))
	(range (nth 1 box-or-range)))
    (when (or (not (mixi-message-box-p box))
	      (mixi-message-box-p range))
      (setq box (nth 1 box-or-range))
      (setq range (nth 0 box-or-range)))
    (let ((items (mixi-get-matched-items
		  (mixi-message-list-page
		   (when box (mixi-message-box-name box)))
		  mixi-message-list-regexp
		  range)))
      (mapcar (lambda (item)
		(mixi-make-message (nth 0 item) (nth 1 item)))
	      items))))

(defmacro mixi-post-message-page (friend)
  `(concat "/send_message.pl?id=" (mixi-friend-id friend)))

(defconst mixi-post-message-key-regexp
  "<input name=post_key type=hidden value=\\([a-z0-9]+\\)>")

(defconst mixi-post-message-succeed-regexp
  "<b>送信完了</b>しました。")

(defun mixi-post-message (friend title content)
  "Post a message to FRIEND."
  (unless (mixi-friend-p friend)
    (signal 'wrong-type-argument (list 'mixi-friend-p friend)))
  (unless (stringp title)
    (signal 'wrong-type-argument (list 'stringp title)))
  (unless (stringp content)
    (signal 'wrong-type-argument (list 'stringp content)))
  (let ((fields `(("subject" . ,title)
		  ("body" . ,content)
		  ("submit" . "main")))
	post-key)
    (with-mixi-post-form (mixi-post-message-page friend) fields
      (if (string-match mixi-post-message-key-regexp buffer)
	  (setq post-key (match-string 1 buffer))
	(mixi-post-error 'cannot-find-key friend)))
    (setq fields `(("post_key" . ,post-key)
		   ("subject" . ,title)
		   ("body" . ,content)
		   ("yes" . "　送　信　")
		   ("submit" . "confirm")))
    (with-mixi-post-form (mixi-post-message-page friend) fields
      (unless (string-match mixi-post-message-succeed-regexp buffer)
	(mixi-post-error 'cannot-find-succeed friend)))))

;; Introduction object.
(defun mixi-make-introduction (parent owner content)
  "Return a introduction object."
  (cons 'mixi-introduction (vector parent owner content)))

(defmacro mixi-introduction-p (introduction)
  `(eq (mixi-object-class ,introduction) 'mixi-introduction))

(defun mixi-introduction-parent (introduction)
  "Return the parent of INTRODUCTION."
  (unless (mixi-introduction-p introduction)
    (signal 'wrong-type-argument (list 'mixi-introduction-p introduction)))
  (aref (cdr introduction) 0))

(defun mixi-introduction-owner (introduction)
  "Return the owner of INTRODUCTION."
  (unless (mixi-introduction-p introduction)
    (signal 'wrong-type-argument (list 'mixi-introduction-p introduction)))
  (aref (cdr introduction) 1))

(defun mixi-introduction-content (introduction)
  "Return the content of INTRODUCTION."
  (unless (mixi-introduction-p introduction)
    (signal 'wrong-type-argument (list 'mixi-introduction-p introduction)))
  (aref (cdr introduction) 3))

(defmacro mixi-introduction-list-page (&optional friend)
  `(concat "/show_intro.pl?page=%d"
	   (when ,friend (concat "&id=" (mixi-friend-id ,friend)))))

(defconst mixi-introduction-list-regexp
  "<tr bgcolor=#FFFFFF>
<td WIDTH=150 background=http://img\\.mixi\\.jp/img/bg_line\\.gif align=\"center\"><a href=\"show_friend\\.pl\\?id=\\([0-9]+\\)\"><img src=\".+\" border=0><br>
\\(.*\\)</td></a>

<td WIDTH=480>
\\(関係：.+<br>


\\(\\(.\\|\n<br>\\)+\\)\\|
\\(\\(.\\|\n<br>\\)+\\)\\)




</td>
</tr>")
(defconst mixi-my-introduction-list-regexp
  "<tr bgcolor=#FFFFFF>
<td WIDTH=150 background=http://img\\.mixi\\.jp/img/bg_line\\.gif align=\"center\"><a href=\"show_friend\\.pl\\?id=\\([0-9]+\\)\"><img src=\".+\" border=0><br>
\\(.*\\)</td></a>


<td WIDTH=480>
\\(関係：.+<br>


\\(\\(.\\|\n<br>\\)+\\)\\|
\\(\\(.\\|\n<br>\\)+\\)\\)


<br>
<a href=\"edit_intro\\.pl\\?id=\\1&type=edit\">この友人を紹介する</a>


<BR>
<a href=\"delete_intro\\.pl\\?id=\\1\">削除</a>

</td>
</tr>")

(defun mixi-get-introductions (&rest friend-or-range)
  "Get introductions of FRIEND."
  (when (> (length friend-or-range) 2)
    (signal 'wrong-number-of-arguments
	    (list 'mixi-get-introduction (length friend-or-range))))
  (let ((friend (nth 0 friend-or-range))
	(range (nth 1 friend-or-range)))
    (when (or (not (mixi-friend-p friend)) (mixi-friend-p range))
      (setq friend (nth 1 friend-or-range))
      (setq range (nth 0 friend-or-range)))
    (unless (or (null friend) (mixi-friend-p friend))
      (signal 'wrong-type-argument (list 'mixi-friend-p friend)))
    (let* ((regexp (if friend mixi-introduction-list-regexp
		     mixi-my-introduction-list-regexp))
	   (items (mixi-get-matched-items (mixi-introduction-list-page friend)
					  regexp
					  range)))
      (mapcar (lambda (item)
		(mixi-make-introduction (or friend (mixi-make-me))
					(mixi-make-friend (nth 0 item)
							  (nth 1 item))
					(nth 2 item)))
	      items))))

(provide 'mixi)

;;; mixi.el ends here
