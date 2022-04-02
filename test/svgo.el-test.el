;;; svgo.el-test.el --- Tests for svgo.el

;;; Code:

(autoload 'svgo--shell-which "./svgo.el")

(ert-deftest shell-which-existing ()
  (should (string-equal (svgo--shell-which "tar") "/usr/bin/tar")))

(ert-deftest shell-which-non-existing ()
  (should (eq (svgo--shell-which "non-existing-command") nil)))

;;; svgo.el-test.el ends here
