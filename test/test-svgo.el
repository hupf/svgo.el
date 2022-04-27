;;; test-svgo.el --- Tests for svgo.el -*- lexical-binding: t; -*-

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
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

;;; The following tests rely a bit heavily on mocks, but since we want
;;; to avoid the side-effects of the actual installing of the NPM
;;; package, this serves the purpose of at least testing the
;;; function's internal logic.

;;; Code:

(require 'svgo)

(describe "svgo"
          (before-each
           (spy-on 'point-min :and-return-value 0)
           (spy-on 'point-max :and-return-value 100)
           (spy-on 'svgo--shell-which :and-return-value "/path/to/svgo")
           (spy-on 'message)
           (spy-on 'undo))

          (it "replaces buffer content with optimized result from `svgo'"
              (spy-on 'shell-command-on-region :and-return-value 0)
              (svgo)
              (expect 'shell-command-on-region :to-have-been-called-with 0 100 "svgo -i -" "*svgo*" t "*svgo*" t)
              (expect 'undo :not :to-have-been-called))

          (it "undos empty buffer caused by error result"
              (spy-on 'shell-command-on-region :and-return-value 1)
              (svgo)
              (expect 'shell-command-on-region :to-have-been-called-with 0 100 "svgo -i -" "*svgo*" t "*svgo*" t)
              (expect 'undo :to-have-been-called))

          (it "prints message if both svgo and npm are missing"
              (spy-on 'svgo--shell-which :and-return-value nil)
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
              (expect 'shell-command :to-have-been-called-with "npm install -g svgo" "*svgo*" "*svgo*"))

          (it "does not install svgo if svgo is missing, npm is present but user rejects"
              (spy-on 'svgo--shell-which :and-call-fake
                      (lambda (command) (if (string-equal command "npm") "/path/to/npm" nil)))
              (spy-on 'read-answer :and-return-value "no")
              (spy-on 'shell-command :and-return-value 0)
              (spy-on 'shell-command-on-region :and-return-value 0)
              (svgo)
              (expect 'read-answer :to-have-been-called)
              (expect 'shell-command :not :to-have-been-called)))

(describe "svgo--shell-which"
          (it "returns the path to the command if present"
              (expect (svgo--shell-which "yes") :to-equal "/usr/bin/yes"))

          (it "returns nil if the path is not present"
              (expect (svgo--shell-which "non-existing-command") :to-be nil)))

(describe "svgo--human-bytes"
          (it "returns \"123 B\" for 123"
              (expect (svgo--human-bytes 123) :to-equal "123 B"))

          (it "returns \"1.23 kB\" for 1234"
              (expect (svgo--human-bytes 1234) :to-equal "1.23 kB"))

          (it "returns \"123.46 kB\" for 123456"
              (expect (svgo--human-bytes 123456) :to-equal "123.46 kB"))

          (it "returns \"1.23 MB\" for 1234567"
              (expect (svgo--human-bytes 1234567) :to-equal "1.23 MB")))

(describe "svgo--with-buffer-size-change"
          (it "returns result of wrapped function, before/after & percentage values"
              (expect (svgo--with-buffer-size-change (lambda () "foo")) :to-equal (list "foo" "0 B" "0 B" "0.00 %"))))

;;; test-svgo.el ends here
