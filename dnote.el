;;; dnote.el --- Wrapper for the dnote CLI -*- lexical-binding: t -*-
;; Copyright 2020 by Dave Pearson <davep@davep.org>

;; Author: Dave Pearson <davep@davep.org>
;; Version: 0.01
;; Keywords: {{keywords}}
;; URL: https://github.com/davep/dnote.el
;; Package-Requires: ((emacs "24"))

;; This program is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation, either version 3 of the License, or (at your
;; option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General
;; Public License for more details.
;;
;; You should have received a copy of the GNU General Public License along
;; with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; dnote.el is a wrapper for the dnote CLI. See
;; https://github.com/dnote/dnote for more details about dnote itself.

;;; Code:

(defvar dnote--path nil
  "The location of the dnote CLI.")

(defun dnote--installed-p ()
  "Is dnote installed?"
  (unless (get 'dnote--path 'checked)
    (setq dnote--path (executable-find "dnote"))
    (put 'dnote--path 'checked t))
  (not (null dnote--path)))

(defun dnote--cmd (cmd)
  "Run the dnote command CMD."
  (if (dnote--installed-p)
      (shell-command-to-string (format "%s %s" dnote--path cmd))
    (error "The dnote CLI does not appear to be installed")))

(defun dnote--books ()
  "Get the list of dnote books."
  (split-string (dnote--cmd "view --name-only")))

(defun dnote--add (book content)
  "Add CONTENT to BOOK in dnote."
  (dnote--cmd (format "add %s --content %s" book (shell-quote-argument content))))

;;;###autoload
(defun dnote-quick-add (book content)
  "Quickly add CONTENT to BOOK.

This command is designed to very quickly add a one-liner to dnote."
  (interactive
   (let* ((book (completing-read "Book: " (dnote--books)))
          (text (read-string
                 (format "Text (for %s): " book)
                 (when (use-region-p) (buffer-substring-no-properties (region-beginning) (region-end))))))
     (list book text)))
  (dnote--add book content)
  (message "Note added to book \"%s\"." book))

(provide 'dnote)

;;; dnote.el ends here
