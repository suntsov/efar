# eFar NEWS

## Change log

### Changes in versions prior version 1.22 have not been documented...

### Version 1.22
* New: Directory comparator implemented  
To compare content of two directories open these directories in left and right panels and press <M-f6>. See Readme for details.
* Fixes for some minor bugs.
* Sub-processing code refactoring and optimization.
  
### Version 1.23
* bug fix for the issue https://github.com/suntsov/efar/issues/17.
  
### Version 1.24
* New: Search history  
From now all search requests and results are stored in search history.  
Search history by default opened with **\<C-f7\>**.  
When eFar restarts search history is cleaned up.
* New: Blink red in status bar on errors/warning.
* Bugfix: files skipped during file search/directory comparison are not shown in the additional information results.
* Bugfix: read files in normal mode (not literally) when searching text.
