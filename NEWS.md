# eFar NEWS

## Change log

### In development
* Working with archives

### Version 1.26
* **New**: The way of key binding castomization changed. See [Readme](README.md) for details.  
Default eFar key bindings used in previous versions have been changed according to the [Emacs Key Binding Conventions](https://www.gnu.org/software/emacs/manual/html_node/elisp/Key-Binding-Conventions.html).  
  Actual key bindings you can check by **C-e ? (M-x efar-do-show-help)**  
  In case if you'd like to get back to previous configuration you can load [get-back-keys.el](adds/get-back-keys.el) in your Emacs init file.  

### Version 1.25
* **New**: Color themes  
Press **\<C-t\>** to switch color theme. For now four themes available: blue, black, white and sand.
* **Bugfix**: disable batch replace and rename functions in modes other than files and file search modes

### Version 1.24
* **New**: Search history  
From now on all search requests and results are stored in search history.  
Search history by default opened with **\<C-f7\>**.  
When eFar restarts search history is cleaned up.
* **New**: Blink red in status bar on errors/warning.
* **Bugfix**: files skipped during file search/directory comparison are not shown in the additional information results.
* **Bugfix**: read files in normal mode (not literally) when searching text.

### Version 1.23
* **Bugfix**: [issue #17](https://github.com/suntsov/efar/issues/17).
  
### Version 1.22
* **New**: Directory comparator implemented  
To compare content of two directories open these directories in left and right panels and press **\<M-f6\>**. See Readme for details.
* Fixes for some minor bugs.
* Sub-processing code refactoring and optimization.
  
### Changes in versions prior version 1.22 have not been documented...
