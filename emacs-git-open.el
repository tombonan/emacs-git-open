;;; emacs-git-open.el --- Open a git repo remote website from Emacs -*- lexical-binding: t -*-

;; Copyright (C) 2023 Tom Bonan
;; Author: Tom Bonan <emacs-git-open@tombonan.me>
;; Version: 0.2.0
;; Keywords: git, github
;; URL: https://github.com/tombonan/emacs-git-open
;; Package-Requires: ((emacs "28.1") (magit "3.0.0"))

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
;; 2024-02-18 - v0.2.0
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

(defgroup emacs-git-open nil
  "Open files and commits on GitHub from Emacs."
  :group 'tools
  :group 'vc
  :prefix "emacs-git-open-")

(defcustom emacs-git-open-default-remote "origin"
  "Name of the remote to link to."
  :type 'string
  :group 'emacs-git-open)

(defun emacs-git-open--default-branch ()
  "Return the default branch name from git config, or \"main\" if not set."
  (let ((branch-name (magit-get "init.defaultBranch")))
    (if (not branch-name)
        "main"
      branch-name)))

(defun emacs-git-open--relative-file-path ()
  "Return the current buffer's file path relative to the repository root."
  (file-relative-name buffer-file-name (magit-toplevel)))

(defun emacs-git-open--current-branch ()
  "Return the current git branch name.
Signal an error if not in a git repository."
  (let ((current-branch (magit-get-current-branch)))
    (if (not current-branch)
        (error "Not in a git repository")
      current-branch)))

(defun emacs-git-open--remote-url ()
  "Return the URL of the configured remote.
Signal an error if the remote is not found."
  (let ((remote-url (magit-git-string "config" "--get" (concat "remote." emacs-git-open-default-remote ".url"))))
    (if (not remote-url)
        (error "Remote '%s' not found" emacs-git-open-default-remote)
      remote-url)))

(defun emacs-git-open--parse-remote (remote-url)
  "Convert REMOTE-URL to a browser-friendly HTTPS URL."
  (let* ((parsed-url (replace-regexp-in-string "^git@github.com:" "https://github.com/" remote-url))
         (parsed-url (replace-regexp-in-string "\\.git$" "" parsed-url)))
    parsed-url))

(defun emacs-git-open--remote-file-url (&optional view-mode tree-path)
  "Return the URL of the current file on the remote.
VIEW-MODE specifies the GitHub view type (e.g., \"blob\" or \"blame\").
Defaults to \"blob\" if not specified.
TREE-PATH is a branch name or commit SHA.  Defaults to the most recent
commit SHA of the file if not specified.
If a region is active, line numbers are appended to the URL."
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
  "Return the most recent commit SHA for the current file."
  (magit-git-string "rev-list" "-1" "HEAD" "--" (buffer-file-name)))

(defun emacs-git-open--get-commit-sha ()
  "Return the commit SHA of the current line in a magit-blame buffer."
  (oref (magit-current-blame-chunk) orig-rev))

(defun emacs-git-open--commit-url ()
  "Return the URL of the commit at the current line in a magit-blame buffer."
  (let ((remote-url (emacs-git-open--parse-remote (emacs-git-open--remote-url))))
    (concat remote-url "/commit/" (emacs-git-open--get-commit-sha))))

(defun emacs-git-open--copy-url (url)
  "Copy URL to the kill ring and display a message."
  (kill-new url)
  (message "%s copied to clipboard." url))

;;;###autoload
(defun git-open ()
  "Open the current buffer's file on the remote in a browser."
  (interactive)
  (browse-url (emacs-git-open--remote-file-url "blob")))

;;;###autoload
(defun git-open-copy ()
  "Copy the URL of the current buffer's file on the remote to the kill ring."
  (interactive)
  (emacs-git-open--copy-url (emacs-git-open--remote-file-url "blob")))

;;;###autoload
(defun git-open-default-branch ()
  "Open the current buffer's file on the remote default branch in a browser."
  (interactive)
  (browse-url (emacs-git-open--remote-file-url "blob" (emacs-git-open--default-branch))))

;;;###autoload
(defun git-open-default-branch-copy ()
  "Copy the URL of the current file on the remote default branch to the kill ring."
  (interactive)
  (emacs-git-open--copy-url (emacs-git-open--remote-file-url "blob" (emacs-git-open--default-branch))))

;;;###autoload
(defun git-open-current-branch ()
  "Open the current buffer's file on the remote current branch in a browser."
  (interactive)
  (browse-url (emacs-git-open--remote-file-url "blob" (emacs-git-open--current-branch))))

;;;###autoload
(defun git-open-current-branch-copy ()
  "Copy the URL of the current file on the remote current branch to the kill ring."
  (interactive)
  (emacs-git-open--copy-url (emacs-git-open--remote-file-url "blob" (emacs-git-open--current-branch))))

;;;###autoload
(defun git-open-blame ()
  "Open the blame view of the current buffer's file on the remote in a browser."
  (interactive)
  (browse-url (emacs-git-open--remote-file-url "blame" (emacs-git-open--default-branch))))

;;;###autoload
(defun git-open-blame-copy ()
  "Copy the blame view URL of the current buffer's file to the kill ring."
  (interactive)
  (emacs-git-open--copy-url (emacs-git-open--remote-file-url "blame" (emacs-git-open--default-branch))))

;;;###autoload
(defun git-open-commit ()
  "Open link to commit from a magit-blame buffer in browser."
  (interactive)
  (browse-url (emacs-git-open--commit-url)))

;;;###autoload
(defun git-open-commit-copy ()
  "Copy the commit URL from a magit-blame buffer to the kill ring."
  (interactive)
  (emacs-git-open--copy-url (emacs-git-open--commit-url)))

(provide 'emacs-git-open)
;;; emacs-git-open.el ends here
