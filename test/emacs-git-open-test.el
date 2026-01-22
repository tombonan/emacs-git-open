;;; emacs-git-open-test.el --- Tests for emacs-git-open  -*- lexical-binding: t -*-

(require 'emacs-git-open)
(require 'eieio)

;; Mock class for testing magit-blame-chunk functionality
(defclass emacs-git-open-test--mock-blame-chunk ()
  ((orig-rev :initarg :orig-rev
             :documentation "The original revision SHA."))
  "Mock blame chunk for testing.")

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
            (emacs-git-open--remote-file-url "blob")))))

(ert-deftest emacs-git-open--remote-file-url-test-view-mode ()
  (with-mock
   (stub emacs-git-open--parse-remote => "https://github.com/tombonan/emacs-git-open")
   (stub emacs-git-open--relative-file-path => "README.md")
   (stub emacs-git-open--get-latest-sha => "1c0ad582e81748d38dfe1f1e1c9f43a96d39613a")
    (should (equal
             "https://github.com/tombonan/emacs-git-open/blame/1c0ad582e81748d38dfe1f1e1c9f43a96d39613a/README.md"
             (emacs-git-open--remote-file-url "blame")))))

(ert-deftest emacs-git-open--remote-file-url-test-tree-path ()
  (with-mock
    (stub emacs-git-open--parse-remote => "https://github.com/tombonan/emacs-git-open")
    (stub emacs-git-open--relative-file-path => "README.md")
    (should (equal "https://github.com/tombonan/emacs-git-open/blob/master/README.md"
                   (emacs-git-open--remote-file-url "blob" "master")))))

(ert-deftest emacs-git-open--remote-file-url-test-with-region ()
  (with-mock
    (stub emacs-git-open--parse-remote => "https://github.com/tombonan/emacs-git-open")
    (stub emacs-git-open--relative-file-path => "README.md")
    (stub emacs-git-open--get-latest-sha => "1c0ad582e81748d38dfe1f1e1c9f43a96d39613a")
    (stub region-active-p => t)
    (stub line-number-at-pos => 16)
    (stub region-beginning => 16)
    (stub region-end => 16)
    (should (equal "https://github.com/tombonan/emacs-git-open/blob/1c0ad582e81748d38dfe1f1e1c9f43a96d39613a/README.md#L16-L16"
                   (emacs-git-open--remote-file-url "blob")))))

(ert-deftest git-open-default-branch-test ()
  (with-mock
    (stub emacs-git-open--parse-remote => "https://github.com/tombonan/emacs-git-open")
    (stub emacs-git-open--relative-file-path => "README.md")
    (stub emacs-git-open--default-branch => "main")
    (mock (browse-url "https://github.com/tombonan/emacs-git-open/blob/main/README.md"))
    (git-open-default-branch)))

(ert-deftest git-open-current-branch-test ()
  (with-mock
    (stub emacs-git-open--parse-remote => "https://github.com/tombonan/emacs-git-open")
    (stub emacs-git-open--relative-file-path => "README.md")
    (stub emacs-git-open--current-branch => "feature-branch")
    (mock (browse-url "https://github.com/tombonan/emacs-git-open/blob/feature-branch/README.md"))
    (git-open-current-branch)))

(ert-deftest emacs-git-open--get-commit-sha-test ()
  (with-mock
    (stub magit-current-blame-chunk =>
          (emacs-git-open-test--mock-blame-chunk :orig-rev "2bc16018bdc5236792805abd9f545b61d67d8460"))
    (should (equal "2bc16018bdc5236792805abd9f545b61d67d8460" (emacs-git-open--get-commit-sha)))))

(ert-deftest emacs-git-open--commit-url-test ()
  (with-mock
    (stub emacs-git-open--parse-remote => "https://github.com/tombonan/emacs-git-open")
    (stub emacs-git-open--get-commit-sha => "1726757d97bc5b5db958b3437287eb765c6f57ef")
    (should (equal "https://github.com/tombonan/emacs-git-open/commit/1726757d97bc5b5db958b3437287eb765c6f57ef"
                   (emacs-git-open--commit-url)))))

;; Error handling
(ert-deftest emacs-git-open--current-branch-error-test ()
  (with-mock
    (stub magit-get-current-branch => nil)
    (should-error (emacs-git-open--current-branch)
                  :type 'error)))

(ert-deftest emacs-git-open--remote-url-error-test ()
  (with-mock
    (stub magit-git-string => nil)
    (should-error (emacs-git-open--remote-url)
                  :type 'error)))

;;; emacs-git-open-test.el ends here
