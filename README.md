# eFar

This package provides FAR-like file manager for Emacs.

## Features

### Navigation
* moving cursor - \<up\>, \<down\>, \<left\>, \<right\>
* go to first/last file in current directory - <home>/<end> or <C-left>/<C-right>
* enter directory under cursor - RET
* switch to other panel - TAB
* fast navigating to the file with specific name in current directory - just typing any part of file name (C-s/C-r to go to next/previous occurrence)
* go to the given directory - C-c c d

### Appearance
* changing mode from double-panel to single-panel - C-c v M
* changing number of columns in current panel - C-c v +   or   C-c v -

### Selecting disks (Windows) or mount points (Unix)
* display list of available disks (Windows) or mount points (Unix) - C-c f d

### Edit and auto preview
* open file under cursor in other buffer and switch to buffer - <f4>
* open file under cursor in other buffer and keep eFar buffer actual - <f3>
* open file under cursor in external application - <M-f4>
* directories and files of predfined types are automatically opened in other buffer when naigating through the file list - this function can configured via customization
 
### File operations
* mark file under cursor - <insert>
* unmark all marked files - <C-insert>
* copy marked files (or file under cursor if no files marked) - <f5>
* move marked files (or file under cursor if no files marked) - <f6>
* delete marked files (or file under cursor if no files marked) - <f8>
* create new directory - <f7>
* create new file - C-x C-f
* show statistic (size, number of file and directories) for the directory under cursor - C-c c s
* run edif for selected files - C-c c e
* copy to the clipboard path to the file under cursor - C-c c p

### Changing sort order

### Filtering by file mask

### Files search

### Last visited directories

### Bookmarks

### Save/restore state

### Auto refresh when files change

### Help
* display list of all available key bindigs - C-?

### Customization

Numerous customizable parameters including key bindings via M-x customize


## Usage
To start eFar just load file efar.el and type M-x efar.
Package is not available in MELPA, hence there is a plan to publish it.
