;;; emacs-git-open.el --- Open a git repo remote website from Emacs -*- lexical-binding: t -*-

;; Copyright (C) 2023 Tom Bonan
;; Author: Tom Bonan <emacs-git-open@tombonan.me>
;; Version: 0.2.0
;; Keywords: git, github
;; URL: https://github.com/tombonan/emacs-git-open
;; Package-Requires: ((emacs "24.3") (magit "3.0.0"))

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Open the current buffer file from the git branch on the given remote on Github
;; in a web browser using the `git-open` function.
;;
;; Inspired by: https://github.com/paulirish/git-open

;;; Change Log:
;;
;; 2024-02-018 - v0.2.0
;; * Open the tree path of the most-recent file commit by default
;;
;; 2023-04-04 - v0.1.0
;; * Initial release for Github remotes via Magit
;; * Added `git-open` and `git-open-copy` to open/copy files on remote
;; * Added `git-open-commit` and `git-open-commit-copy` to open/copy commit from magit-blame buffer
;; * Added `git-open-blame` and copy function to open blame views on remote
;;

;;; Code:
(require 'magit)

(defcustom emacs-git-open-default-remote "origin"
  "Name of the remote to link to."
  :type 'string
  :group 'emacs-git-open)

(defun emacs-git-open--default-branch ()
  (let ((branch-name (magit-get "init.defaultBranch")))
    (if (not branch-name)
        "main"
      branch-name)))

(defun emacs-git-open--relative-file-path ()
  (file-relative-name buffer-file-name (magit-toplevel)))

(defun emacs-git-open--current-branch ()
  (let ((current-branch (magit-get-current-branch)))
    (if (not current-branch)
        (error "Error: not in a Git repository.")
      current-branch)))

(defun emacs-git-open--remote-url ()
  (let ((remote-url (magit-git-string "config" "--get" (concat "remote." emacs-git-open-default-remote ".url"))))
    (if (not remote-url)
        (error "Error: remote not found.")
      remote-url)))

(defun emacs-git-open--parse-remote (remote-url)
  (let* ((parsed-url (replace-regexp-in-string "^git@github.com:" "https://github.com/" remote-url))
         (parsed-url (replace-regexp-in-string "\\.git$" "" parsed-url)))
    parsed-url))

(defun emacs-git-open--remote-file-url (&optional view-mode tree-path)
  "URL of the remote file.

   view-mode (optional): View mode that will allow other views besides the git blob view.
     e.g. 'blob' or 'blame'. Defaults to 'blob' if no argument is passed.
   tree-path (optional): Path of the tree for the file. This can either be a branch name
     or a git commit SHA. Defaults to the most recent SHA of the file if no value is passed.
  "
  (let* ((view-mode (or view-mode "blob"))
         (tree-path (or tree-path (emacs-git-open--get-latest-sha)))
         (remote-url (emacs-git-open--parse-remote (emacs-git-open--remote-url)))
         (remote-url (concat remote-url "/" view-mode "/" tree-path "/" (emacs-git-open--relative-file-path))))
    (if (region-active-p)
        (let ((start-line (line-number-at-pos (region-beginning)))
              (end-line (line-number-at-pos (region-end))))
          (format "%s#L%d-L%d" remote-url start-line end-line))
      remote-url)))

(defun emacs-git-open--get-latest-sha ()
  "Fetch the most-recent SHA for the current file"
  (magit-rev-parse-safe "HEAD" (buffer-file-name)))

(defun emacs-git-open--get-commit-sha ()
  "Fetch the commit SHA of the current line in a magit-blame buffer"
  (interactive)
  (oref (magit-current-blame-chunk) orig-rev))

(defun emacs-git-open--commit-url ()
  "Opens the commit at the current line from a magit-blame buffer"
  (let ((remote-url (emacs-git-open--parse-remote (emacs-git-open--remote-url))))
    (concat remote-url "/commit/" (emacs-git-open--get-commit-sha))))

;;;###autoload
(defun git-open ()
  "Open link to current buffer file on remote in browser"
  (interactive)
  (browse-url (emacs-git-open--remote-file-url "blob")))

;;;###autoload
(defun git-open-copy ()
  "Copy link to current buffer file on remote"
  (interactive)
  (let ((remote-file-url (emacs-git-open--remote-file-url "blob")))
    (kill-new remote-file-url)
    (message "%s copied to clipboard." remote-file-url)))

;;;###autoload
(defun git-open-default-branch ()
  "Open link to current buffer file on remote in browser for the default branch"
  (interactive)
  (browse-url (emacs-git-open--remote-file-url "blob" (emacs-git-open--default-branch))))

;;;###autoload
(defun git-open-default-branch-copy ()
  "Copy link to current buffer file on remote for default branch"
  (interactive)
  (let ((remote-file-url (emacs-git-open--remote-file-url "blob" (emacs-git-open--default-branch))))
    (kill-new remote-file-url)
    (message "%s copied to clipboard." remote-file-url)))

;;;###autoload
(defun git-open-current-branch ()
  "Open link to current buffer file on remote in browser for the current branch"
  (interactive)
  (browse-url (emacs-git-open--remote-file-url "blob" (emacs-git-open--current-branch))))

;;;###autoload
(defun git-open-current-branch-copy ()
  "Copy link to current buffer file on remote for the current branch"
  (interactive)
  (let ((remote-file-url (emacs-git-open--remote-file-url "blob" (emacs-git-open--current-branch))))
    (kill-new remote-file-url)
    (message "%s copied to clipboard." remote-file-url)))

;;;###autoload
(defun git-open-blame ()
  "Open link to blame of current buffer file on remote in browser"
  (interactive)
  (browse-url (emacs-git-open--remote-file-url "blame" (emacs-git-open--default-branch))))

;;;###autoload
(defun git-open-blame-copy ()
  "Copy link to blame of current buffer file"
  (interactive)
  (let ((remote-file-url (emacs-git-open--remote-file-url "blame" (emacs-git-open--default-branch))))
    (kill-new remote-file-url)
    (message "%s copied to clipboard." remote-file-url)))

;;;###autoload
(defun git-open-commit ()
  "Open link to commit from a magit-blame buffer in browser."
  (interactive)
  (browse-url (emacs-git-open--commit-url)))

;;;###autoload
(defun git-open-commit-copy ()
  "Copy link to commit from a magit-blame buffer"
  (interactive)
  (let ((remote-commit-url (emacs-git-open--commit-url)))
    (kill-new remote-commit-url)
    (message "%s copied to clipboard." remote-commit-url)))

(provide 'emacs-git-open)
;;; emacs-git-open.el ends here
