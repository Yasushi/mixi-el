;; mixi-utils.el --- Utilities for mixi object

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

(defvar mixi-reply-to nil)

(defmacro with-mixi-class (object &rest body)
  `(let ((class (mixi-object-class ,object)))
     ,@body))
(put 'with-mixi-class 'lisp-indent-function 'defun)
(put 'with-mixi-class 'edebug-form-spec '(body))

(defun mixi-make-title (object &optional add-suffix)
  (with-mixi-class object
    (cond ((eq class 'mixi-comment)
	   (concat "Re: " (mixi-make-title
			   (mixi-comment-parent object) add-suffix)))
	  ((eq class 'mixi-log)
	   (mixi-friend-nick (mixi-log-friend object)))
	  (t
	   (let ((prefix (when (eq class 'mixi-event) "[イベント]"))
		 (subject (mixi-object-title object))
		 (suffix (when add-suffix
			   (concat " ("
				   (if (eq class 'mixi-diary)
				       (mixi-friend-nick
					(mixi-diary-owner object))
				     (mixi-community-name
				      (mixi-bbs-community object)))
				   ")"))))
	     (concat prefix subject suffix))))))

(defun mixi-make-author (object)
  (with-mixi-class object
    (if (eq class 'mixi-news)
	(mixi-news-media object)
      (let ((owner (if (eq class 'mixi-log)
		       (mixi-log-friend object)
		     (mixi-object-owner object))))
	(mixi-friend-nick owner)))))

(defun mixi-make-date (object)
  (let* ((time (mixi-object-time object))
	 (cts (current-time-string time))
	 (day-of-week (substring cts 0 3))
	 (month (substring cts 4 7)))
    (concat day-of-week ", "
	    (format-time-string "%d" time) " "
	    month " "
	    (format-time-string "%Y %H:%M:%S %z" time))))

(defun mixi-make-message-id (object)
  (with-mixi-class object
    (concat
     (format-time-string "<%Y%m%d%H%M" (mixi-object-time object)) "."
     (cond ((eq class 'mixi-comment)
	    (concat (mixi-friend-id (mixi-comment-owner object)) "@"
		    (mixi-object-id (mixi-comment-parent object)) "."
		    (mixi-friend-id (mixi-object-owner
				     (mixi-comment-parent object))) "."))
	   ((eq class 'mixi-log)
	    (concat (mixi-friend-id (mixi-log-friend object)) "@"))
	   (t
	    (concat (mixi-object-id object) "@"
		    (if (eq class 'mixi-news)
			(mixi-news-media-id object)
		      (mixi-object-id (mixi-object-owner object))) ".")))
     (mixi-object-name object) ".mixi.jp>")))

(defun mixi-make-url (object)
  (with-mixi-class object
    (cond ((eq class 'mixi-diary)
	   (mixi-expand-url (mixi-diary-page object)))
	  ((eq class 'mixi-topic)
	   (mixi-expand-url (mixi-topic-page object)))
	  ((eq class 'mixi-event)
	   (mixi-expand-url (mixi-event-page object)))
	  ((eq class 'mixi-comment)
	   (concat (mixi-make-url (mixi-comment-parent object))
		   "#comment"))
	  ((eq class 'mixi-message)
	   (mixi-expand-url (mixi-message-page object)))
	  ((eq class 'mixi-news)
	   (mixi-news-page object))
	  ((eq class 'mixi-log)
	   (mixi-expand-url (mixi-friend-page (mixi-log-friend object))))
	  ((eq class 'mixi-friend)
	   (mixi-expand-url (mixi-friend-page object))))))

(defun mixi-make-content (object)
  (with-mixi-class object
    (cond ((eq class 'mixi-event)
	   (let ((limit (mixi-event-limit object)))
	     (setq limit (if limit
			     (format-time-string "%Y年%m月%d日" limit)
			   "指定なし"))
	     (concat "<dl>"
		     "<dt>開催日時：</dt>"
		     "<dd>" (mixi-event-date object) "</dd>\n"
		     "<dt>開催場所：</dt>"
		     "<dd>" (mixi-event-place object) "</dd>\n"
		     "<dt>詳細：</dt>"
		     "<dd>" (mixi-event-detail object) "</dd>\n"
		     "<dt>募集期限：</dt>"
		     "<dd>" limit "</dd>\n"
		     "<dt>参加者：</dt>"
		     "<dd>" (mixi-event-members object) "</dd>\n"
		     "</dl>")))
	  ((eq class 'mixi-friend)
	   (if (mixi-object-realized-p object)
	       (let ((sex (if (eq (mixi-friend-sex object) 'male) "男" "女"))
		     (age (number-to-string (mixi-friend-age object)))
		     (birthday
		      (concat (mapconcat (lambda (number)
					   (number-to-string number))
					 (mixi-friend-birthday object) "月")
			      "日"))
		     (blood-type (symbol-name
				  (mixi-friend-blood-type object)))
		     (hobby (mapconcat 'identity
				       (mixi-friend-hobby object) ", ")))
		 (concat "<dl>"
			 "<dt>名前：</dt>"
			 "<dd>" (mixi-friend-name object) "</dd>\n"
			 "<dt>性別：</dt>"
			 "<dd>" sex "性</dd>\n"
			 "<dt>現住所：</dt>"
			 "<dd>" (mixi-friend-address object) "</dd>\n"
			 "<dt>年齢：</dt>"
			 "<dd>" age "歳</dd>\n"
			 "<dt>誕生日：</dt>"
			 "<dd>" birthday "</dd>\n"
			 "<dt>血液型：</dt>"
			 "<dd>" blood-type "型</dd>\n"
			 "<dt>出身地：</dt>"
			 "<dd>" (mixi-friend-birthplace object) "</dd>\n"
			 "<dt>趣味：</dt>"
			 "<dd>" hobby "</dd>\n"
			 "<dt>職業：</dt>"
			 "<dd>" (mixi-friend-job object) "</dd>\n"
			 "<dt>所属：</dt>"
			 "<dd>" (mixi-friend-organization object) "</dd>\n"
			 "<dt>自己紹介：</dt>"
			 "<dd>" (mixi-friend-profile object) "</dd>\n"
			 "</dl>"))
	     (concat "<a href=\"" (mixi-make-url object)
		     "\">プロフィールを表示する</a>")))
	  (t (mixi-object-content object)))))

(defun mixi-make-reply-to (object)
  (setq mixi-reply-to "mixi;")
  (with-mixi-class object
    (setq mixi-reply-to
	  (concat
	   (cond ((eq class 'mixi-diary)
		  (concat mixi-reply-to "comment;diary;"
			  (mixi-friend-id (mixi-diary-owner object)) ";"
			  (mixi-diary-id object)))
		 ((mixi-bbs-p object)
		  (concat mixi-reply-to "comment;"
			  (mixi-object-name object) ";"
			  (mixi-community-id (mixi-bbs-community object)) ";"
			  (mixi-bbs-id object)))
		 ((eq class 'mixi-community)
		  (concat mixi-reply-to "topic;"
			  (mixi-community-id object)))
		 ((or (eq class 'mixi-news) (eq object (mixi-make-me)))
		  (concat mixi-reply-to "diary"))
		 ((eq class 'mixi-message)
		  (concat mixi-reply-to "message;"
			  (mixi-friend-id (mixi-message-owner object))))
		 ((or (eq class 'mixi-friend) (eq class 'mixi-log))
		  (concat mixi-reply-to "message;"
			  (mixi-friend-id object))))))))

(provide 'mixi-utils)

;;; mixi-utils.el ends here
