;;; svgo.el --- SVG optimization with SVGO

;;; Commentary:
;;; This package uses the Node utility SVGO to optimize SVG files.  It
;;; provides a command and a minor mode (that activates the key binding
;;; C-c f) to optimize the SVG contens of the current buffer.

;;; Author: Mathis Hofer <mathis@fsfe.org>
;;; Version: 1.0.0
;;; Keywords: svg, svgo, node
;;; URL: https://github.com/hupf/svgo.el/

;;; License:

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

;;; Code:

(require 'subr-x)

;;;###autoload
(defun svgo ()
  "Optimize current buffer with SVGO."
  (interactive)
  ;; Use current buffer's Node version if nvm.el is present
  (when (fboundp 'nvm-use-for-buffer)
      (nvm-use-for-buffer))

  (when (svgo--ensure)
      (when (> (shell-command-on-region
              (point-min) (point-max)
              "svgo -i -" "*svgo*" t "*svgo-errors*" t)
             0)
          ;; If command failed, make sure the region's content is not lost
          (undo))))

(define-minor-mode svgo-mode
  "Toggle SVGO mode.

When SVGO mode is enabled, the binding C-c f optimizes the SVG
vector graphic contents in current buffer or selected region with
SVGO.
See the command \\[svgo] and https://github.com/svg/svgo."
 :init-value nil
 :lighter " SVGO" ;; The indicator for the mode line.
 :keymap `((,(kbd "C-c f") . svgo)))

(defun svgo--ensure ()
  "Ensure SVGO is present or install it if user agrees."
  (let ((svgo-bin (svgo--shell-which "svgo")))
    (if svgo-bin
        svgo-bin
      (if (svgo--shell-which "npm")
          (if (svgo--prompt-install)
              (if (> (shell-command "npm i -g svgo" "*svgo*" "*svgo-errors*") 0)
                  (progn
                    (switch-to-buffer "*svgo-errors*")
                    (message "An error occurred installing `svgo' using NPM")
                    nil)
                (svgo--shell-which "svgo")))
        (message "No `svgo' command found and `npm' is not present")))))

(defun svgo--shell-which (command)
  "Return the path to the given COMMAND if present or nil."
  (let ((path (shell-command-to-string (concat "/usr/bin/which " command))))
    (if (string-equal path "")
        nil
      (string-trim path))))

(defun svgo--prompt-install ()
  "Prompt the user whether SVGO should be installed using NPM."
  (string-equal
   (read-answer "No `svgo' command found, would you like to install it? "
                '(("yes" ?y "install `svgo' using NPM")
                  ("no" ?n "don't install `svgo'")))
   "yes"))

(provide 'svgo)
;;; svgo.el ends here
