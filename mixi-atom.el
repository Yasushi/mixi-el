;; mixi-atom.el --- Atom Syndication Format

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

;; Bug reports:
;;
;; If you have bug reports and/or suggestions for improvement, please
;; send them via <URL:http://mixi.jp/view_community.pl?id=1596390>.

;;; Code:

(require 'mixi)
(require 'mixi-utils)

(defcustom mixi-atom-coding-system 'utf-8
  "*Coding system for Atom Syndication Format."
  :type 'coding-system
  :group 'mixi)

(defcustom mixi-atom-namespace "http://www.w3.org/2005/Atom"
  "*Namespace for Atom Syndication Format."
  :type 'string
  :group 'mixi)

(defcustom mixi-atom-title "Mixi Feed"
  "*Title for feed."
  :type 'string
  :group 'mixi)

(defcustom mixi-atom-syndication-list
  '((mixi-get-diaries . 10))
  "*A list of atom syndication definition.
Each element looks like (URL . RANGE) or (FUNCTION . RANGE).
URL is the URL for mixi access point.  If URL is friend's, get his/her diaries
as article.  If community's, get its BBSes.  If diary's or BBS's, get its
comments.
FUNCTION is the function which has one `range' argument and returns the list
of mixi object.
RANGE is the range for getting articles.  If RANGE is nil, get all articles."
  :group 'mixi
  :type '(repeat (cons
		  (radio (string :tag "URL")
			 (const :tag "New diaries" mixi-get-new-diaries)
			 (const :tag "New comments" mixi-get-new-comments)
			 (const :tag "New BBSes" mixi-get-new-bbses)
			 (const :tag "Messages" mixi-get-messages)
			 (const :tag "Logs" mixi-get-logs)
			 (function :tag "Other function"))
		  (radio (integer :tag "Range")
			 (const :tag "All" nil)))))

(defcustom mixi-atom-file "~/atom.xml"
  "*File name for `mixi-make-atom-file'."
  :group 'mixi
  :type 'string)

(defmacro mixi-atom-make-date (time)
  `(let ((date (format-time-string "%Y-%m-%dT%T%z" ,time)))
     (if (string-match "[+-][0-9][0-9][0-9][0-9]$" date)
	 (let ((length (length date)))
	   (format "%s:%s"
		   (substring date 0 (- length 2))
		   (substring date (- length 2) length)))
       date)))

(defmacro mixi-atom-now ()
  `(mixi-atom-make-date (current-time)))

(defun mixi-make-atom-entry (object)
  "Make Atom entry."
  (concat "<entry>\n"
	  " <title>" (mixi-make-title object) "</title>\n"
	  " <link href=\"" (mixi-make-encoded-url object) "\"/>\n"
	  " <id>" (mixi-make-tag-uri object) "</id>\n"
	  " <updated>" (mixi-atom-make-date (mixi-object-time object))
	  "</updated>\n"
	  " <summary>" (mixi-remove-markup (mixi-make-content object))
	  "</summary>\n"
	  "</entry>\n"))

(defun mixi-make-atom-entries (objects &optional range)
  "Make Atom entries."
  (let (entries)
    (mapcar (lambda (object)
	      (setq entries
		    (concat entries (mixi-make-atom-entry object)))
	      (when (mixi-parent-p object)
		(let ((comments (mixi-get-comments object range)))
		  (while comments
		    (setq entries
			  (concat entries
				  (mixi-make-atom-entry (car comments))))
		    (setq comments (cdr comments))))))
	    objects)
    entries))

;;;###autoload
(defun mixi-make-atom ()
  "Make Atom Syndication Format"
  (insert "<?xml version=\"1.0\" encoding=\""
	  (symbol-name mixi-atom-coding-system) "\"?>\n"
	  "<feed xmlns=\"" mixi-atom-namespace "\">\n"
	  "\n"
	  "<title>" mixi-atom-title "</title>\n"
	  "<link href=\"" mixi-url "\"/>\n"
	  "<updated>" (mixi-atom-now) "</updated>\n"
	  "<author>\n"
	  " <name>" (mixi-friend-nick (mixi-make-me)) "</name>\n"
	  "</author>\n"
	  "<id>tag:mixi.jp," (format-time-string "%Y-%m-%d"
						 (current-time))
	  ":" (mixi-friend-nick (mixi-make-me)) "</id>\n"
	  "\n"
	  (mapconcat 'identity
		     (mapcar (lambda (list)
			       (let ((url-or-function (car list))
				     (range (cdr list)))
				 (mixi-make-atom-entries
				  (mixi-make-objects url-or-function
						     range) range)))
			     mixi-atom-syndication-list)
		     "\n")
	  "\n"
	  "</feed>\n"))

;;;###autoload
(defun mixi-make-atom-file ()
  (with-temp-buffer
    (mixi-make-atom)
    (let ((coding-system-for-write mixi-atom-coding-system)
	  (file (expand-file-name mixi-atom-file)))
      (write-region (point-min) (point-max) file))))

(provide 'mixi-atom)

;;; mixi-atom.el ends here
