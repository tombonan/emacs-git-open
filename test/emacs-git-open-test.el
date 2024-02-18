;;; emacs-git-open-test.el --- Tests for emacs-git-open

(require 'emacs-git-open)

(ert-deftest emacs-git-open--parse-remote-test ()
  (should (equal "https://github.com/tombonan/emacs-git-open"
                 (emacs-git-open--parse-remote "git@github.com:tombonan/emacs-git-open.git")))
  (should (equal "https://github.com/tombonan/emacs-git-open"
                 (emacs-git-open--parse-remote "https://github.com/tombonan/emacs-git-open.git"))))

(ert-deftest emacs-git-open--current-branch-test ()
  (with-mock
    (stub magit-get-current-branch => "main")
    (should (equal "main" (emacs-git-open--current-branch)))))

(ert-deftest emacs-git-open--remote-file-url-test ()
  (with-mock
    (stub emacs-git-open--parse-remote => "https://github.com/tombonan/emacs-git-open")
    (stub emacs-git-open--relative-file-path => "README.md")
    (stub emacs-git-open--get-latest-sha => "1c0ad582e81748d38dfe1f1e1c9f43a96d39613a")
    (should
     (equal "https://github.com/tombonan/emacs-git-open/blob/1c0ad582e81748d38dfe1f1e1c9f43a96d39613a/README.md"
            (emacs-git-open--remote-file-url)))))

(ert-deftest emacs-git-open--remote-file-url-test-view-mode ()
  (with-mock
    (stub emacs-git-open--parse-remote => "https://github.com/tombonan/emacs-git-open")
    (stub emacs-git-open--relative-file-path => "README.md")
    (should (equal "https://github.com/tombonan/emacs-git-open/blame/main/README.md" (emacs-git-open--remote-file-url "blame")))))

(ert-deftest emacs-git-open--remote-file-url-test-tree-path ()
  (with-mock
    (stub emacs-git-open--parse-remote => "https://github.com/tombonan/emacs-git-open")
    (stub emacs-git-open--relative-file-path => "README.md")
    (should (equal "https://github.com/tombonan/emacs-git-open/blob/master/README.md"
                   (emacs-git-open--remote-file-url nil "master")))))

(ert-deftest emacs-git-open--remote-file-url-test-with-region ()
  (with-mock
    (stub emacs-git-open--parse-remote => "https://github.com/tombonan/emacs-git-open")
    (stub emacs-git-open--relative-file-path => "README.md")
    (stub region-active-p => t)
    (stub line-number-at-pos => 16)
    (stub region-beginning => 16)
    (stub region-end => 16)
    (should (equal "https://github.com/tombonan/emacs-git-open/blob/main/README.md#L16-L16"
                   (emacs-git-open--remote-file-url)))))

;; Error handling
(expectations
  (desc "emacs-git-open--current-branch-test-failure")
  (expect (error-message "Error: not in a Git repository.")
    (with-mock
      (stub magit-get-current-branch => nil)
      (emacs-git-open--current-branch)))
  (desc "emacs-git-open--remote-url-test-failure")
  (expect (error-message "Error: remote not found.")
    (with-mock
      (stub magit-git-string => nil)
      (emacs-git-open--remote-url))))

;;; emacs-git-open-test.el ends here
