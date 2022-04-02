;;; svgo.el-test.el --- Tests for svgo.el

;;; Code:

(autoload 'shell-which "./svgo.el")

(ert-deftest shell-which-existing ()
  (should (string-equal (shell-which "tar") "/usr/bin/tar")))

(ert-deftest shell-which-non-existing ()
  (should (eq (shell-which "non-existing-command") nil)))

;;; svgo.el-test.el ends here