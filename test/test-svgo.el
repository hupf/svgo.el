;;; test-svgo.el --- Tests for svgo.el

;;; Commentary:

;;; The following tests rely a bit heavily on mocks, but since we want
;;; to avoid the side-effects of the actual installing of the NPM
;;; package, this serves the purpose of at least testing the
;;; functions' internal logic.

;;; Code:

(require 'svgo)

(describe "svgo--shell-which"
          (it "returns the path to the command if present"
              (expect (svgo--shell-which "yes")
                      :to-equal
                      "/usr/bin/yes"))

          (it "returns nil if the path is not present"
              (expect (svgo--shell-which "non-existing-command")
                      :to-be
                      nil)))

(describe "svgo"
          (before-each
           (spy-on 'point-min :and-return-value 0)
           (spy-on 'point-max :and-return-value 100)
           (spy-on 'svgo--shell-which :and-return-value "/path/to/svgo")
           (spy-on 'undo))

          (it "replaces buffer content with optimized result from `svgo'"
              (spy-on 'shell-command-on-region :and-return-value 0)
              (svgo)
              (expect 'shell-command-on-region :to-have-been-called-with 0 100 "svgo -i -" "*svgo*" t "*svgo-errors*" t)
              (expect 'undo :not :to-have-been-called))

          (it "undos empty buffer caused by error result"
              (spy-on 'shell-command-on-region :and-return-value 1)
              (svgo)
              (expect 'shell-command-on-region :to-have-been-called-with 0 100 "svgo -i -" "*svgo*" t "*svgo-errors*" t)
              (expect 'undo :to-have-been-called))

          (it "prints message if both svgo and npm are missing"
              (spy-on 'svgo--shell-which :and-return-value nil)
              (spy-on 'message)
              (svgo)
              (expect 'message :to-have-been-called-with "No `svgo' command found and `npm' is not present"))

          (it "installs svgo if svgo is missing, npm is present and user accepts"
              (spy-on 'svgo--shell-which :and-call-fake
                      (lambda (command) (if (string-equal command "npm") "/path/to/npm" nil)))
              (spy-on 'read-answer :and-return-value "yes")
              (spy-on 'shell-command :and-return-value 0)
              (spy-on 'shell-command-on-region :and-return-value 0)
              (svgo)
              (expect 'read-answer :to-have-been-called)
              (expect 'shell-command :to-have-been-called-with "npm install -g svgo" "*svgo*" "*svgo-errors*"))

          (it "does not install svgo if svgo is missing, npm is present but user rejects"
              (spy-on 'svgo--shell-which :and-call-fake
                      (lambda (command) (if (string-equal command "npm") "/path/to/npm" nil)))
              (spy-on 'read-answer :and-return-value "no")
              (spy-on 'shell-command :and-return-value 0)
              (spy-on 'shell-command-on-region :and-return-value 0)
              (svgo)
              (expect 'read-answer :to-have-been-called)
              (expect 'shell-command :not :to-have-been-called)))

;;; test-svgo.el ends here
