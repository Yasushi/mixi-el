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
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; To use, add the following lines to your ~/.emacs:
;;
;; (autoload 'mixi-wl-setup "mixi-wl")
;; (add-hook 'wl-init-hook 'mixi-wl-setup)

;; If you have bug reports and/or suggestions for improvement, please
;; send them via <URL:http://mixi.jp/view_community.pl?id=1596390>.

;;; Code:

(require 'mixi-utils)
(require 'wl-draft)

(defsubst mixi-wl-get-recipients-from-buffer ()
  (or (std11-field-body "mixi-to")
      (std11-field-body "to")))

(defun wl-draft-send-mail-with-mixi ()
  "Send the prepared message buffer with mixi."
  (let* ((case-fold-search t)
	 (default-case-fold-search t)
	 (recipients (mixi-wl-get-recipients-from-buffer)))
    (let ((delimline (save-excursion
		       (goto-char (point-min))
		       (re-search-forward
			(concat "^" (regexp-quote mail-header-separator)
				"$\\|^$") nil t)
		       (point-marker)))
	  (id (std11-field-body "message-id")))
      (mixi-send-mail recipients
		      (eword-decode-string (or (std11-field-body "subject")
					       ""))
		      (decode-mime-charset-string
		       (buffer-substring (1+ delimline) (point-max))
		       wl-mime-charset))
      (wl-draft-set-sent-message 'mail 'sent)
      (wl-draft-write-sendlog 'ok 'mixi nil (list recipients) id))))

(defun mixi-wl-setup-draft-buffer ()
  (let ((recipients (mixi-wl-get-recipients-from-buffer)))
    (when (and recipients
	       (string-match mixi-to-regexp recipients))
      (make-local-variable 'wl-draft-send-confirm-with-preview)
      (setq wl-draft-send-confirm-with-preview nil)
      (make-local-variable 'wl-draft-send-mail-function)
      (setq wl-draft-send-mail-function 'wl-draft-send-mail-with-mixi))))

(defun mixi-wl-setup ()
  (add-hook 'wl-draft-send-hook 'mixi-wl-setup-draft-buffer))

(provide 'mixi-wl)

;;; mixi-wl.el ends here
