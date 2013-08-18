# fetchmacs

A major mode for interfacing with [Fetchnotes](http://www.fetchnotes.com) inside of Emacs.

## Installation

    (load-file 'fetchmacs.el)

## Authentication

    (defvar fetchmacs-user-email "user@n.ame")
    (defvar fetchmacs-user-pass "password")

## Usage

`fetchmacs-view-notes` - Open your notes in `\*fetchmacs-view-notes-buffer\*`

From here, I tried to imitate [magit](http://philjackson.github.com/magit/)'s controls:

* `RET` : `fetchmacs-view-edit-note-at-point`
* `/` : `fetchmacs-search` - like in Gmail's shortcuts
* `c` : `fetchmacs-create-new-note`
* `d` : `fetchmacs-delete-note-at-point`
* `e` : `fetchmacs-view-edit-note-at-point`
* `g` : `fetchmacs-refresh`
* `n` : `fetchmacs-goto-next-note`
* `o` : `fetchmacs-view-edit-note-at-point`
* `p` : `fetchmacs-goto-previous-note`
* `t` : `fetchmacs-filter-by-tag`
