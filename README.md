# Fetchmacs

A major mode for interfacing with [Fetchnotes](http://www.fetchnotes.com) inside of Emacs.

## Installation

    (require 'package)
    (add-to-list 'package-archives
        '("marmalade" .
          "http://marmalade-repo.org/packages/"))
    (package-initialize)

Then, use package-install to install fetchmacs:

    M-x package-install RET fetchmacs RET

## Authentication

You can invoke `fetchnotes-get-creds` to be prompted for your fetchnotes login information manually, or just invoke `fetchnotes-view-notes` and it'll prompt you if necessary.

If your auth is incorrect, either `M-x fetchnotes-get-creds RET` or eval `(fetchnotes-get-creds t)` to force a reset of your credentials.

## Usage

`fetchmacs-view-notes` - Open your notes in `\*fetchmacs-view-notes-buffer\*`

From here, I tried to imitate [magit](http://philjackson.github.com/magit/) and `dired` controls where it made sense:

* `RET` : `fetchmacs-view-edit-note-at-point`
* `/` : `fetchmacs-search`
* `c` : `fetchmacs-create-new-note`
* `d` : `fetchmacs-delete-note-at-point`
* `e` : `fetchmacs-view-edit-note-at-point`
* `g` : `fetchmacs-refresh`
* `n` : `fetchmacs-goto-next-note`
* `o` : `fetchmacs-view-edit-note-at-point`
* `p` : `fetchmacs-goto-previous-note`
* `t` : `fetchmacs-filter-by-tag`
