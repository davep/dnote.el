# dnote.el

## Commentary

`dnote.el` is a wrapper for the [dnote](https://www.getdnote.com/) CLI. The
main aim of the package is to provide quick and easy methods of recording
information in dnote without needing to leave Emacs.

Available commands are:

### dnote-quick-add

Quickly add a note to dnote, prompting for the book and also the note
content. This is intended for quick one-liners. If there is a selection in
place the content will default to the selection.

### dnote-add-current-buffer

Quickly create a note in dnote, using the content of the current buffer as
the content of the note.

### dnote-add

Add a note to dnote, with the note being written in a new window. Use
<kbd>C-c</kbd> <kbd>C-c</kbd> to save the note.

### dnote-app

Opens the dnote web application in your web browser.

### dnote-sync

Syncs dnote notes with the dnote backend, if you have one configured and
have previously logged in with the dnote CLI.

[//]: # (README.md ends here)
