;;; mixi-gnus.el --- Gnus integration for mixi

;; Copyright (C) 2007 OHASHI Akira

;; Author: OHASHI Akira <bg66@koka-in.org>
;; Keywords: news

;; This file is *NOT* a part of Gnus.

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

;; To use, add the following lines to your ~/.emacs:
;;
;; (autoload 'mixi-gnus-setup "mixi-gnus")
;; (add-hook 'gnus-startup-hook 'mixi-gnus-setup)

;; If you have bug reports and/or suggestions for improvement, please
;; send them via <URL:http://mixi.jp/view_community.pl?id=1596390>.

;;; Code:

(require 'sb-mixi)
(require 'message)

(defun message-mixi-p ()
  "Say whether the current buffer contains a mixi message."
  (and (not message-this-is-news)
       (or message-this-is-mail
	   (save-excursion
	     (save-restriction
	       (message-narrow-to-headers)
	       (or (message-fetch-field "mixi")
		   (string-match shimbun-mixi-to-regexp
				 (message-fetch-field "to"))))))))

(defun message-send-via-mixi (arg)
  "Send the current message via mixi."
  (let* ((tembuf (message-generate-new-buffer-clone-locals " message temp"))
	 (case-fold-search nil)
	 (mailbuf (current-buffer)))
    ;; Avoid matching with message-mail-p.
    (with-current-buffer mailbuf
      (goto-char (point-min))
      (when (search-forward "\nTo: " nil t)
	(replace-match "\nMixi: ")))
    (unwind-protect
	(save-excursion
	  (set-buffer tembuf)
	  (erase-buffer)
	  ;; Avoid copying text props (except hard newlines).
	  (insert (with-current-buffer mailbuf
		    (mml-buffer-substring-no-properties-except-hard-newlines
		     (point-min) (point-max))))
	  (shimbun-mixi-send-mail (message-fetch-field "mixi")
				  (message-fetch-field "subject")
				  (buffer-substring (message-goto-body)
						    (point-max))))
      (kill-buffer tembuf))
    (set-buffer mailbuf)
    (push 'mixi message-sent-message-via)))

(defun mixi-gnus-setup ()
  (let ((method '(mixi message-mixi-p message-send-via-mixi)))
    (unless (member method message-send-method-alist)
      (setq message-send-method-alist
	    (cons '(mixi message-mixi-p message-send-via-mixi)
		  message-send-method-alist)))))

(provide 'mixi-gnus)

;;; mixi-gnus.el ends here
