;;; wl-fastfind.el --- Prompt for a query string and display search results.

;; Copyright (C) 2014 Erik Hetzner <egh@e6h.org>

;; Author: Erik Hetzner <egh@e6h.org>
;; Keywords: mail, net news

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;; Code:

(require 'elmo-imap4)
(require 'elmo-search)
(require 'wl-folder)
(require 'wl-summary)
(require 'wl-util)

(defcustom wl-fastfind-method
  nil
  "Method to use for wl-fastfind search."
  :group 'wl-fastfind
  :type '(choice (const :tag "not set" nil)
                 (const :tag "elmo search" 'elmo)
                 (const :tag "gmail search" 'gmail)
                 (const :tag "imap search" 'imap)))

(defcustom wl-fastfind-gmail-username
  nil
  "Username to use with wl-fastfind gmail search."
  :group 'wl-fastfind
  :type '(choice string (const :tag "not set" nil)))

(defcustom wl-fastfind-archive-folder
  nil
  "Folder to search with wl-fastfind imap search."
  :group 'wl-fastfind
  :type '(choice string (const :tag "not set" nil)))

;; Needed for Gmail search.
(add-to-list 'elmo-imap4-search-keys "x-gm-raw")

(defun wl-fastfind-escape-query-string (str)
  "Replace single quotes (') in STR with double quotes (\"), then escape double-quotes."
  (let* ((str1 (replace-regexp-in-string "'" "\"" str nil t))
         (str2 (replace-regexp-in-string "\"" "\\\"" str1 nil t)))
    str2))

(defun wl-fastfind-goto-elmo-search-folder ()
  "Prompt for a query and display results.
Uses elmo-search folder with `elmo-search-default-engine'."
  (let ((q (wl-fastfind-escape-query-string
            (read-string (format "%s query: " elmo-search-default-engine)))))
    (wl-folder-goto-folder-subr (format "[\"%s\"]" q))))

(defun wl-fastfind-goto-gmail-search-folder ()
  "Prompt for a gmail query and display results.
Uses `wl-fastfind-gmail-username' to determine folder."
  (if (null wl-fastfind-gmail-username)
      (error "The variable `wl-fastfind-gmail-username' is not set!")
    (let* ((q (wl-fastfind-escape-query-string
               (read-string "gmail query: "))))
      (wl-folder-goto-folder-subr
       (format "/x-gm-raw:\"%s\"/%%[Gmail]/All Mail:%s@imap.gmail.com:993!"
        q wl-fastfind-gmail-username)))))

(defun wl-fastfind-goto-imap-search-folder ()
  "Prompt for an IMAP query and display results.
Searches `wl-fastfind-archive-folder'."
  (if (null wl-fastfind-archive-folder)
      (error "The variable `wl-fastfind-archive-folder' is not set!")
    (wl-folder-goto-folder-subr
     (concat "/"
             (wl-read-search-condition
              wl-fldmgr-make-filter-default)
             "/" wl-fastfind-archive-folder))))

(defun wl-fastfind-goto-search-folder ()
  "Prompt for a query and display results.
Behavior depends on the value of `wl-fastfind-method'."
  (interactive)
  (cond ((eq wl-fastfind-method 'elmo)
         (wl-fastfind-goto-elmo-search-folder))
        ((eq wl-fastfind-method 'gmail)
         (wl-fastfind-goto-gmail-search-folder))
        ((eq wl-fastfind-method 'imap)
         (wl-fastfind-goto-imap-search-folder))
        (t
         (error "The variable `wl-fastfind-method' is not set! Please customize"))))

(define-key wl-folder-mode-map "'" 'wl-fastfind-goto-search-folder)
(define-key wl-summary-mode-map "'" 'wl-fastfind-goto-search-folder)

(define-key-after
  (lookup-key wl-folder-mode-map [menu-bar Folder])
  [fastfind]
  '("Fast find" . wl-fastfind-goto-search-folder)
  (intern "Go to Draft Folder"))

(define-key-after
  (lookup-key wl-summary-mode-map [menu-bar Summary])
  [fastfind]
  '("Fast find" . wl-fastfind-goto-search-folder)
  (intern "Go to other folder"))

(provide 'wl-fastfind)
;;; wl-fastfind.el ends here
