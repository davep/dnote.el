;;; dnote.el --- Wrapper for the dnote CLI -*- lexical-binding: t -*-
;; Copyright 2020 by Dave Pearson <davep@davep.org>

;; Author: Dave Pearson <davep@davep.org>
;; Version: 0.01
;; Keywords: docs
;; URL: https://github.com/davep/dnote.el
;; Package-Requires: ((emacs "24.3"))

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

(defvar-local dnote--book nil
  "The book the current buffer relates to.")

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

(defun dnote--read-book ()
  "Prompt the user for a dnote book name.

If the buffer-local `dnote--book' is non-nil no read is done and
its value is returned."
  (or dnote--book (completing-read "Book: " (dnote--books))))

;;;###autoload
(defun dnote-quick-add (book content)
  "Quickly add CONTENT to BOOK.

This command is designed to very quickly add a one-liner to dnote."
  (interactive
   (let* ((book (dnote--read-book))
          (text (read-string
                 (format "Text (for %s): " book)
                 (when (use-region-p) (buffer-substring-no-properties (region-beginning) (region-end))))))
     (list book text)))
  (dnote--add book content)
  (message "Note added to book \"%s\"." book))

;;;###autoload
(defun dnote-add-current-buffer (book)
  "Add the content of the current buffer to BOOK."
  (interactive (list (dnote--read-book)))
  (dnote--add book (buffer-substring-no-properties (point-min) (point-max)))
  ;; If this is a dnote-mode buffer make the (currently, given my design
  ;; requirement) safe assumption that we can kill the buffer now that we've
  ;; saved it.
  (when (derived-mode-p 'dnote-mode)
    (kill-buffer))
  (message "Saved as a note in \"%s\"." book))

(defvar dnote-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'dnote-add-current-buffer)
    map)
  "Mode map for `dnote-mode'.")

;;;###autoload
(define-derived-mode dnote-mode text-mode "dnote")

;;;###autoload
(defun dnote-add (book)
  "Write a note and add it to BOOK."
  (interactive (list (dnote--read-book)))
  (with-current-buffer (pop-to-buffer (format "*dnote add: %s*" book))
    (dnote-mode)
    (setq dnote--book book)))

(provide 'dnote)

;;; dnote.el ends here
