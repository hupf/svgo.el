;;; test-svgo.el --- Tests for svgo.el

;;; Code:

(require 'svgo)

(describe "svgo--shell-which"
          (it "returns the path to the command if present"
              (expect (svgo--shell-which "tar")
                      :to-equal
                      "/usr/bin/tar"))

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
              (expect 'undo :to-have-been-called)))

;;; test-svgo.el ends here
