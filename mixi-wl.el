;;; mixi-wl.el --- Wanderlust integration for mixi

;; Copyright (C) 2007 OHASHI Akira

;; Author: OHASHI Akira <bg66@koka-in.org>
;; Keywords: news

;; This file is *NOT* a part of Wanderlust.

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
;; (autoload 'mixi-wl-setup "mixi-wl")
;; (add-hook 'wl-init-hook 'mixi-wl-setup)

;; If you have bug reports and/or suggestions for improvement, please
;; send them via <URL:http://mixi.jp/view_community.pl?id=1596390>.

;;; Code:

(require 'sb-mixi)
(require 'wl-draft)

(defvar wl-draft-send-mail-function-original nil)

(defun wl-draft-send-mail-with-mixi ()
  "Send the prepared message buffer with mixi."
  (let* ((case-fold-search t)
	 (default-case-fold-search t)
	 (recipients (or (std11-field-body "mixi-to")
			 (std11-field-body "to"))))
    (if (string-match shimbun-mixi-to-regexp recipients)
	(let ((delimline (save-excursion
			   (goto-char (point-min))
			   (re-search-forward
			    (concat "^" (regexp-quote mail-header-separator)
				    "$\\|^$") nil t)
			   (point-marker)))
	      (id (std11-field-body "message-id")))
	  (shimbun-mixi-send-mail recipients
				  (eword-decode-string
				   (std11-field-body "subject"))
				  (decode-mime-charset-string
				   (buffer-substring (1+ delimline)
						     (point-max))
				   wl-mime-charset))
	  (wl-draft-set-sent-message 'mail 'sent)
	  (wl-draft-write-sendlog 'ok 'mixi nil recipients id))
      (funcall wl-draft-send-mail-function-original))))

(defun mixi-wl-setup ()
  ;; FIXME: Don't set it.
  (setq wl-draft-send-confirm-with-preview nil)
  (unless wl-draft-send-mail-function-original
    (setq wl-draft-send-mail-function-original wl-draft-send-mail-function)
    (setq wl-draft-send-mail-function 'wl-draft-send-mail-with-mixi)))

(provide 'mixi-wl)

;;; mixi-wl.el ends here
