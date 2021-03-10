;;; efar.el --- FAR-like file manager -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Free Software Foundation, Inc.

;; Author: "Vladimir Suntsov" <vladimir@suntsov.online>
;; Maintainer: vladimir@suntsov.online
;; Version: 1.0
;; Package-Requires: ((emacs "26.1"))
;; Keywords: files
;; URL: https://github.com/suntsov/efar

;; This package is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This package is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides FAR-like file manager.

;; To start eFar just type M-x efar.
;; When Efar is called with universal argument, default-directory of actual buffer is automatically opened in left panel (C-u M-x efar).

;; Press C-? to show buffer with all available key bindings.

;; Use M-x customize to configure numerous eFar parameters including key bindings and faces.


;;; Code:

(require 'ido)
(require 'subr-x)
(require 'filenotify)
(require 'dired)

(defvar efar-state nil)
;;variables for file search
(defvar efar-search-processes '())
(defvar efar-search-process-manager nil)
(defvar efar-search-results '())
(defvar efar-last-search-params nil)
(defvar efar-update-search-results-timer nil)
(defvar efar-search-server nil)
(defvar efar-search-server-port nil)
(defvar efar-search-clients '())
(defvar efar-search-running? nil)
(defvar efar-search-process-pending-messages '())

(provide 'efar)

;;--------------------------------------------------------------------------------
;; eFar customization variables
;;--------------------------------------------------------------------------------
;; GROUPS
(defgroup efar nil
  "FAR-like file manager"
  :group 'applications)

(defgroup efar-parameters nil
  "eFar main customization parameters"
  :group 'efar)

(defgroup efar-search-parameters nil
  "eFar file search parameters"
  :group 'efar-parameters)

(defgroup efar-faces nil
  "eFar faces"
  :group 'efar)

(defgroup efar-search-faces nil
  "eFar faces"
  :group 'efar-search-parameters)

(defgroup efar-keys nil
  "eFar key bindings"
  :group 'efar)

;; MAIN PARAMETERS
(eval-and-compile
  (defcustom efar-buffer-name "*eFAR*"
    "Name for the Efar buffer."
    :group 'efar-parameters
    :type 'string))

(defcustom efar-state-file-name (concat user-emacs-directory ".efar-state")
  "Path to the eFar state save file."
  :group 'efar-parameters
  :type 'string)

(defcustom efar-default-startup-dirs (cons user-emacs-directory user-emacs-directory)
  "Default direcotries shown at startup."
  :group 'efar-parameters
  :type 'cons)

(defcustom efar-save-state? t
  "Save eFar state when eFar buffer is killed to restore it at next open."
  :group 'efar-parameters
  :type 'boolean)

(defcustom efar-filter-directories? nil
  "Apply filter to the directories and files (t) or to files only (nil)."
  :group 'efar-parameters
  :type 'boolean)

(defcustom efar-add-slash-to-directories t
  "Add ending / to directories."
  :group 'efar-parameters
  :type 'boolean)

(defcustom efar-auto-read-directories nil
  "Automatically show content of the directory under cursor in other buffer."
  :group 'efar-parameters
  :type 'boolean)

(defcustom efar-auto-read-files t
  "Automatically show contentof the file under cursor in other buffer."
  :group 'efar-parameters
  :type 'boolean)

(defcustom efar-auto-read-file-extensions (list "el" "py" "magik" "txt")
  "List of file types (extensions) to be automatically read."
  :group 'efar-parameters
  :type 'list)

(defcustom efar-auto-read-max-file-size 1048576
  "Maximum size in bytes for files which will be automatically read."
  :group 'efar-parameters
  :type 'integer)

(defcustom efar-max-items-in-directory-history 100
  "Maximum number of directories to be stored in directory history."
  :group 'efar-parameters
  :type 'integer)

(defcustom efar-max-search-processes 8
  "Number of subprocesses to use for file search."
  :group 'efar-search-parameters
  :type 'integer)

(defcustom efar-search-results-buffer-name "*eFar search results*"
  "Name of the buffer to display search results in."
  :group 'efar-search-parameters
  :type 'string)

(defcustom efar-search-default-file-mask "*.el"
  "Default file name mask to use for file search."
  :group 'efar-search-parameters
  :type 'string)

(defcustom efar-search-coding 'iso-latin-1
  "Coding system to use for file search."
  :group 'efar-search-parameters
  :type 'symbol)

(defcustom efar-search-follow-symlinks? t
  "Follow directory symlinks when searching?"
  :group 'efar-search-parameters
  :type 'boolean)

;; FACES
(defface efar-border-line-face
  '((t :foreground "white"
       :background "navy"
       :underline nil))
  "Border line face"
  :group 'efar-faces)


(defface efar-file-face
  '((t :foreground "deep sky blue"
       :background "navy"
       :underline nil))
  "File item style (default)"
  :group 'efar-faces)

(defface efar-file-executable-face
  '((t :foreground "green"
       :background "navy"
       :underline nil))
  "File item style (executable file)"
  :group 'efar-faces)

(defface efar-dir-face
  '((t :foreground "white"
       :background "navy"
       :underline nil))
  "Directory item style"
  :group 'efar-faces)

(defface efar-file-current-face
  '((t :foreground "black"
       :background "cadet blue"
       :underline nil))
  "Current file item style"
  :group 'efar-faces)

(defface efar-dir-current-face
  '((t :foreground "white"
       :background "cadet blue"
0       :underline nil))
  "Current directory item style"
  :group 'efar-faces)

(defface efar-marked-face
  '((t :foreground "gold"
       :background "navy"
       :underline nil))
  "Marked item style"
  :group 'efar-faces)


(defface efar-dir-name-face
  '((t :foreground "white"
       :background "navy"
       :underline nil))
  "Directory name header style"
  :group 'efar-faces)

(defface efar-header-face
  '((t :foreground "orange"
       :background "navy"
       :underline nil))
  "Header style"
  :group 'efar-faces)

(defface efar-dir-name-current-face
  '((t :foreground "navy"
       :background "bisque"
       :underline nil))
  "Current directory name header style"
  :group 'efar-faces)

(defface efar-non-existing-file-face
  '((t :foreground "red"
       :background "navy"
       :underline nil))
  "Style for non-existing files (in bookmarks and directory history"
  :group 'efar-faces)

(defface efar-non-existing-current-file-face
  '((t :foreground "red"
       :background "bisque"
       :underline nil))
  "Style for non-existing current files (in bookmarks and directory history"
  :group 'efar-faces)

(defface efar-search-file-link-face
  '((t :foreground "black"
       :background "snow2"
       :bold t
       :underline t))
  "The face used for representing the link to the file"
  :group 'efar-search-faces)

(defface efar-search-line-link-face
  '((t :foreground "black"
       :background "ivory"))
  "The face used for representing the link to the source code line"
  :group 'efar-search-faces)


;; macros
(defmacro efar-with-notification-disabled(&rest body)
  "Execute BODY with efar-notifications disabled."
  `(let ((file-notifier-left (efar-get :panels :left :file-notifier))
	 (file-notifier-right (efar-get :panels :right :file-notifier)))
     
     (when (file-notify-valid-p (cdr file-notifier-left))
       (efar-remove-notifier :left))
     
     (when (file-notify-valid-p (cdr file-notifier-right))
       (efar-remove-notifier :right))
     
     ,@body
     
     (efar-setup-notifier (efar-get :panels :left :dir) :left)
     (efar-setup-notifier (efar-get :panels :right :dir) :right)))

(defmacro efar-write-enable (&rest body)
  "Make eFar buffer writable, execute BODY and then make it back read-only."
  `(with-current-buffer ,efar-buffer-name
     (read-only-mode -1)
     ,@body
     (read-only-mode 1)))

(defmacro efar-retry-when-error (&rest body)
  "Retry BODY until success or user answer 'No'."
  `(while
       (string=
	"error"
	(condition-case err
	    
	    ,@body
	  
	  (error (when
		     (string= "Yes"
			      (ido-completing-read
			       (concat "Error: \"" (error-message-string err) "\". Try again? ")
			       (list "Yes" "No")))
		   "error"))))))


;; KEY bindings

(defvar efar-keys '()
  "The list of eFar keybindings.")

(setq efar-keys '())

(defun efar-register-key(key func arg custom-key-name description &optional show-in-help? ignore-in-modes)
  "Registers a new KEY binding.
This list is used to set up local key bindings,
generate content of help buffer and prepare key binding customizaton entries.

FUNC is a function to call with argument ARG.
CUSTOM-KEY-NAME is a name of custom variable by which a key binding is available
in customization menu.
DESCRIPTION is a string describing the key in customization menu.
SHOW-IN-HELP? is optional boolean indicating whether to show key in help buffer.
IGNORE-IN-MODES is a list of modes which should ignore this key binding."
  (push (list key func arg custom-key-name description show-in-help? ignore-in-modes) efar-keys))

;;   key-sequence function to call  arg variable name to save custom Key description
(efar-register-key (kbd "<down>")  'efar-move-cursor  :down 'efar-move-down-key
		   "move cursor one line down" t)
(efar-register-key (kbd "<up>")   'efar-move-cursor  :up 'efar-move-up-key
		   "move cursor one line up" t)
(efar-register-key (kbd "<right>")  'efar-move-cursor  :right  'efar-move-right
		   "move cursor to the column on the right" t)
(efar-register-key (kbd "<left>")  'efar-move-cursor  :left  'efar-move-left-key
		   "move cursor to the column on the left" t)
(efar-register-key (kbd "<prior>")  'efar-move-cursor  :left  nil "" nil)
(efar-register-key (kbd "<next>")  'efar-move-cursor  :right  nil "" nil)
(efar-register-key (kbd "<home>")  'efar-move-cursor  :home 'efar-move-home-key
		   "move cursor to the beginning of the list" t)
(efar-register-key (kbd "C-<left>")  'efar-move-cursor  :home  'efar-move-home-alt-key
		   "move cursor to the beginning of the list (alternative)" t)
(efar-register-key (kbd "<end>")  'efar-move-cursor  :end 'efar-move-end-key
		   "move cursot to the end of the list" t)
(efar-register-key (kbd "C-<right>")  'efar-move-cursor  :end 'efar-move-end-alt-key
		   "move cursot to the end of the list (alternative)" t)
(efar-register-key (kbd "RET") '((:files . efar-enter-directory) (:dir-hist . efar-navigate-to-file) (:bookmark . efar-navigate-to-file) (:disks . efar-navigate-to-file) (:search . efar-navigate-to-file))  nil  'efar-enter-directory-key
		   "go into or to the item under cursor" :space-after)

(efar-register-key (kbd "C-<down>") 'efar-scroll-other-window :down 'efar-scroll-other-down-key
		   "scroll other window down" t)
(efar-register-key (kbd "C-<up>") 'efar-scroll-other-window :up 'efar-scroll-other-up-key
		   "scroll other window up" t)

(efar-register-key (kbd "<insert>") 'efar-mark-file   nil 'efar-mark-file-key
		    "mark item under cursor" t (list :dir-hist :bookmark :disks :search))
(efar-register-key (kbd "<C-insert>") 'efar-deselect-all  nil 'efar-deselect-all-kay
		   "unmark all items in the list" :space-after (list :dir-hist :bookmark :disks :search))

(efar-register-key (kbd "TAB")   'efar-switch-to-other-panel nil 'efar-switch-to-other-panel-key
		   "switch to other panel" t)
(efar-register-key (kbd "C-c TAB")  'efar-open-dir-other-panel nil 'efar-open-dir-othet-panel-key
		   "open current directory in other panel" :space-after)

(efar-register-key (kbd "<f4>")   'efar-edit-file   nil 'efar-open-file-key
		   "edit file under cursor" t)
(efar-register-key (kbd "<M-f4>")  'efar-open-file-in-ext-app nil 'efar-open-file-in-ext-app-key
		   "open file under cursor in externall application" t)
(efar-register-key (kbd "<f3>")  'efar-edit-file   t 'efar-read-file-key
		   "show content of the file in other window" :space-after)

(efar-register-key (kbd "<f5>")   'efar-copy-or-move-files :copy 'efar-copy-file-key
		   "copy marked file(s)" t (list :dir-hist :bookmark :disks :search))
(efar-register-key (kbd "<f6>")  'efar-copy-or-move-files :move 'efar-move-file-key
		   "move/rename marked file(s)" t (list :dir-hist :bookmark :disks :search))
(efar-register-key (kbd "<f7>")  'efar-create-new-directory nil 'efar-create-direcotry-key
		   "create new directory" t (list :dir-hist :bookmark :disks :search))
(efar-register-key (kbd "<f8>")  '((:files . efar-delete-selected) (:bookmark . efar-delete-bookmark))   nil 'efar-delete-file-key
		   "delete selected file(s) or bookmark" :space-after '(:dir-hist :disks :search))


(efar-register-key (kbd "C-c f d") 'efar-show-disk-selector  nil 'efar-show-disk-selector-key
		   "show list of available disks (Windows) or mount points (Unix)" t)
(efar-register-key (kbd "C-c f s") 'efar-change-sort-function  nil 'efar-change-sort-key
		   "change sort function and/or order for current panel" t (list :dir-hist :bookmark :disks :search))
(efar-register-key (kbd "C-c f f") 'efar-filter-files  nil 'efar-filter-files-key
		   "set/remove filtering for current panel" :space-after (list :dir-hist :bookmark :disks :search))

(efar-register-key (kbd "C-c v M") 'efar-change-mode  nil 'efar-change-mode-key
		   "toggle mode: double panel <-> single panel" t)
(efar-register-key (kbd "C-c v +") 'efar-change-column-number t 'efar-inc-column-number-key
		   "increase number of columns in current panel" t)
(efar-register-key (kbd "C-c v -") 'efar-change-column-number nil 'efar-dec-column-number-key
		   "decrease number of columns in current panel" t)
(efar-register-key (kbd "C-c v m") 'efar-change-file-disp-mode nil 'efar-change-file-disp-mode-key
		   "change file display mode (short, long, detailed) for current panel" :space-after)

(efar-register-key (kbd "C-c c p") 'efar-copy-current-path  nil 'efar-copy-current-path-key
		   "copy to the clipboard the path to the current file" t)
(efar-register-key (kbd "C-c c d") 'efar-cd   nil 'efar-cd-key
		   "go to specific directory" t)
(efar-register-key (kbd "C-c c e") 'efar-ediff-files  nil 'efar-ediff-files-key
		   "run ediff for selected files" t (list :dir-hist :bookmark :disks :search))
(efar-register-key (kbd "C-c c s") 'efar-current-file-stat  nil 'efar-current-file-stat-key
		   "show directory stats (size and files number)" t)
(efar-register-key (kbd "C-c c o") 'efar-display-console  nil 'efar-display-console-key
		    "open console window"  t)
(efar-register-key (kbd "<f12> <f12>")  'efar-init   t 'efar-init-key
		   "reinit and redraw eFar buffer" t)
(efar-register-key (kbd "C-c ?")  'efar-show-help   nil 'efar-show-help-key
		   "show frame with all key bindings" t)
(efar-register-key (kbd "C-c c b") 'efar-show-bookmarks  nil 'efar-show-boormarks-key
		   "show bookmarks" t)
(efar-register-key (kbd "C-c c B") 'efar-add-bookmark  nil 'efar-add-boormark-key
		   "add item under cursor to the bookmarks" t)
(efar-register-key (kbd "C-c c h") 'efar-show-directory-history  nil 'efar-show-directory-history-key
		   "show last visited directories" :space-after)

;; fast-search keys
(efar-register-key (kbd "DEL") 'efar-fast-search  :back nil
		   "Backspace for fast search")
(efar-register-key (kbd "C-s")  'efar-fast-search  :next nil
		   "start fast search/go to next fast search match" t)
(efar-register-key (kbd "C-r")  'efar-fast-search  :prev nil
		   "start fast search/go to previous fast search match" :space-after)

(efar-register-key (kbd "<M-f7>") 'efar-start-search nil 'efar-start-search-key
		   "run file search" t)
(efar-register-key (kbd "<S-f7>") 'efar-show-search-results t 'efar-show-search-results-key
		   "show file search results" t)
(efar-register-key (kbd "<C-M-f7>") 'efar-show-search-results-in-buffer nil 'efar-show-search-results-in-buffer-key
		   "display search results in a separate buffer" :space-after (list :dir-hist :bookmark :disks :files))

(efar-register-key (kbd "C-g") 'efar-abort  nil nil
		   "abort current operation" t)

(efar-register-key (kbd "C-n") 'efar-suggest-hint nil nil nil nil)

(cl-loop for char in (list ?a ?b ?c ?d ?e ?f ?g ?h ?i ?j ?k ?l ?m ?n ?o ?p ?q ?r ?s ?t ?u ?v ?w ?x ?y ?z
			   ?A ?B ?C ?D ?E ?F ?G ?H ?I ?J ?K ?L ?M ?N ?O ?P ?Q ?R ?S ?T ?U ?V ?W ?X ?Y ?Z
			   ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9 ?0
			   ?\( ?\) ?.
			   ?- ?_
			   32) do
			   (efar-register-key	(char-to-string char)	'efar-fast-search	char	nil	""))

;; create customization entries for key bindings
(cl-loop for key in efar-keys do
	 (when (nth 3 key)
	   (custom-declare-variable
	    (intern (symbol-name (nth 3 key)))
	    (nth 0 key)
	    (nth 4 key)
	    :type 'key-sequence
	    :group 'efar-keys)))


;;--------------------------------------------------------------------------------
;; eFar main functions
;;--------------------------------------------------------------------------------
;;;###autoload
(defun efar(arg)
  "Main funtion to run eFar commander.
Argument ARG: when t open default directory of current buffer."
  (interactive "P")
  
  (let(
       ;; if eFar buffer doesn't exist, we need to do initialisation
       (need-init? (not (get-buffer efar-buffer-name)))
       ;; get existing or create new eFar buffer
       (efar-buffer (get-buffer-create efar-buffer-name))
       ;; if eFar is called with prefix argument, then go to default-directory of current buffer
       (go-to-dir (when arg
		    default-directory)))
    
    (with-current-buffer efar-buffer
      ;; make eFar buffer fullscreen
      ;;(delete-other-windows)
      
      ;; do initialisation if necessary and redraw the content of the buffer
      (when need-init?
	(efar-init)
	
	;; make search processes
	;; we do this in advance to speed up search process
	(make-thread 'efar-run-search-processes)

	(efar-calculate-window-size)
	(efar-calculate-widths)
	(efar-write-enable
	 (efar-redraw)))
      
      ;; go to default-directory of current buffer if function is called with prefix argument
      (when go-to-dir
	(efar-go-to-dir go-to-dir :left)
	(efar-write-enable (efar-redraw)))
      
      (efar-suggest-hint))
    
    (switch-to-buffer-other-window efar-buffer)
    (efar-write-enable (efar-redraw))))


(defun efar-init(&optional reinit?)
  "Set up main eFAR configuration.
This function is executed only once when eFAR buffer is created.

REINIT? is a boolean indicating that configuration should be generated enew."
  
  ;; forbid resizing of eFAR window
  ;;  (setq window-size-fixed t)
  ;; disable cursor
  (setq cursor-type nil)
  
  ;; if saving/restoring of state is allowed, read state from file
  (when (and
	 efar-save-state?
	 (file-exists-p efar-state-file-name))
    (setf efar-state (efar-read-state))
    (efar-set-status :ready (concat "eFar state loaded from file " efar-state-file-name) nil t))
  
  ;; if eFAR state cannot be restored from file (missing or broken file) or saving/restoring of state is disabled
  ;; then initialize state storage with default values
  (when (or reinit?
	    (null efar-state))
    (setf efar-state nil)
    (efar-init-state))
  
  (efar-set-key-bindings)
  (efar-set-mouse-bindings)
  
  (efar-init-panel :left)
  (efar-init-panel :right)
  
  (when reinit?
    (efar-write-enable (efar-redraw))))

(defun efar-init-panel(side)
  "Initialize configuration for panel SIDE."
  (pcase (efar-get :panels side :mode)
    (:files (efar-go-to-dir (efar-get :panels side :dir) side))
    (:dir-hist (efar-show-directory-history side))
    (:bookmark (efar-show-bookmarks side))
    (:disks (efar-show-disk-selector))
    (:search (efar-go-to-dir (efar-last-visited-dir side)))))

(defun efar-init-state()
  "Initialize state with default values."
  (setf efar-state (make-hash-table :test `equal))
  
  ;; set directories for both panels
  (efar-set (car efar-default-startup-dirs) :panels :left :dir)
  (efar-set (cdr efar-default-startup-dirs) :panels :right :dir)
  
  ;; make left panel active
  (efar-set :left :current-panel)
  
  ;; set cursor to the first file for both panels
  (efar-set 0 :panels :left :current-pos)
  (efar-set 0 :panels :right :current-pos)
  
  (efar-set '() :panels :left :selected)
  (efar-set '() :panels :right :selected)
  
  (efar-set 0 :panels :left :start-file-number)
  (efar-set 0 :panels :right :start-file-number)
  
  (efar-set "" :fast-search-string)
  (efar-set 0 :fast-search-occur)
  
  (efar-set nil :notification-timer)
  
  (efar-set "" :panels :left :file-filter)
  (efar-set "" :panels :right :file-filter)
  
  (efar-set nil :panels :left :file-notifier)
  (efar-set nil :panels :right :file-notifier)
  
  (efar-set :files :panels :left :mode)
  (efar-set :files :panels :right :mode)
  
  (efar-set '() :pending-notifications)
  
  (efar-set nil :panels :left :sort-order)
  (efar-set nil :panels :right :sort-order)
  
  (efar-set "Name" :panels :left :sort-function-name)
  (efar-set "Name" :panels :right :sort-function-name)
  
  (efar-set :both :mode)
  
  (efar-set nil :column-widths)
  
  (efar-set "Ready" :status-string)
  (efar-set :ready :status)
  (efar-set nil :reset-status?)
  
  (efar-set 1 :panels :left :view :files :column-number)
  (efar-set '(:short :detailed :long) :panels :left :view :files :file-disp-mode)
  (efar-set 1 :panels :left :view :dir-hist :column-number)
  (efar-set '(:long) :panels :left :view :dir-hist :file-disp-mode)
  (efar-set 1 :panels :left :view :bookmark :column-number)
  (efar-set '(:long) :panels :left :view :bookmark :file-disp-mode)
  (efar-set 1 :panels :left :view :disks :column-number)
  (efar-set '(:long) :panels :left :view :disks :file-disp-mode)
  (efar-set 1 :panels :left :view :search :column-number)
  (efar-set '(:long) :panels :left :view :search :file-disp-mode)
  
  (efar-set 1 :panels :right :view :files :column-number)
  (efar-set '(:short :detailed :long) :panels :right :view :files :file-disp-mode)
  (efar-set 1 :panels :right :view :dir-hist :column-number)
  (efar-set '(:long) :panels :right :view :dir-hist :file-disp-mode)
  (efar-set 1 :panels :right :view :bookmark :column-number)
  (efar-set '(:long) :panels :right :view :bookmark :file-disp-mode)
  (efar-set 1 :panels :right :view :disks :column-number)
  (efar-set '(:long) :panels :right :view :disks :file-disp-mode)
  (efar-set 1 :panels :right :view :search :column-number)
  (efar-set '(:long) :panels :right :view :search :file-disp-mode)
  
  (efar-set nil :last-auto-read-buffer)
  
  ;;(efar-set (make-hash-table :test `equal) :last-visited-dirs)
  (efar-set '() :directory-history)
  (efar-set '() :bookmarks)
  
  (efar-set 0 :next-hint-number))

;;--------------------------------------------------------------------------------
;; file sort functions
;;--------------------------------------------------------------------------------
(defconst efar-sort-functions '(("Name" . efar-sort-files-by-name)
				("Modification Date" . efar-sort-files-by-modification-date)
				("Extension" . efar-sort-files-by-extension)
				("Size" . efar-sort-files-by-size)
				("Unsorted" . nil)))

(defun efar-get-sort-function(name)
  "Return sort function by NAME."
  (cdr (assoc name efar-sort-functions)))


(defun efar-sort-function-names(side)
  "Return a list of sort function names.
The name of a function which is currently used for the panel SIDE (or current panel) becomes a first entry in the list."
  (let ((current-function-name (efar-get :panels side :sort-function-name)))
    (append (list current-function-name)
	    (cl-remove current-function-name
		       (mapcar
			(lambda(e)
			  (car e))
			efar-sort-functions)))))

(defun efar-sort-files-by-name(a b)
  "Function to sort files A and B by name.
Directories always go before normal files.
Function string< is used for comparing file names.
Case is ignored."
  (cond
   ;; directories go first
   ((and (cadr a) (not (cadr b)))
    t)
   ((and (not (cadr a)) (cadr b))
    nil)
   ;; entries of same type (directory or usual files) sorted in lexicographic order
   (t (string<
       (downcase (car a))
       (downcase (car b))))))

(defun efar-sort-files-by-modification-date(a b)
  "Function to sort files A and B by modification date.
Directories always go before normal files."
  (cond
   ;; directories go first
   ((and (cadr a) (not (cadr b)))
    t)
   ((and (not (cadr a)) (cadr b))
    nil)
   ;; entries of same type (directory or usual files) sorted by comparing modification date
   (t (<
       (time-to-seconds (nth 6 a))
       (time-to-seconds (nth 6 b))))))

(defun efar-sort-files-by-size(a b)
  "Function to sort files A and B by file size.
Directories always go before normal files."
  (cond
   ;; directories go first
   ((and (cadr a) (not (cadr b)))
    t)
   ((and (not (cadr a)) (cadr b))
    nil)
   ;; entries of same type (directory or usual files) sorted by comparing file size
   (t (<
       (nth 8 a)
       (nth 8 b)))))

(defun efar-sort-files-by-extension(a b)
  "Function to sort files A and B by type (extension).
Directories always go before normal files.
Function string< is used for comparing file extensions.
Case is ignored."
  (cond
   ;; directories go first
   ((and (cadr a) (not (cadr b)))
    t)
   ((and (not (cadr a)) (cadr b))
    nil)
   ;; files sorted by extension (if any) in lexicographic order
   (t (string<
       (downcase (or (file-name-extension (car a)) ""))
       (downcase (or (file-name-extension (car b)) ""))))))

(defun efar-change-sort-function()
  "Ask user for sort function and order and set them for panel SIDE."
  (let ((side (efar-get :current-panel)))
    (efar-set (ido-completing-read "Sort files by: " (efar-sort-function-names side))
	      :panels side :sort-function-name)
    
    (efar-set (string= (char-to-string 9660) (ido-completing-read "Sort order: " (list (char-to-string 9650) (char-to-string 9660) )))
	      :panels side :sort-order)
    
    (efar-refresh-panel side nil (efar-get-short-file-name (efar-current-file)))))

;;--------------------------------------------------------------------------------
;; end of file sort functions
;;--------------------------------------------------------------------------------



;;--------------------------------------------------------------------------------
;; file notification functions
;;--------------------------------------------------------------------------------

(defun efar-setup-notifier(dir side)
  "Set up file-notify-watch for the directory DIR and register it for panel SIDE."
  ;; first remove existing file-notify-watch
  (efar-remove-notifier side)
  
  (let* ((other-side-notifier (efar-get :panels (efar-other-side side) :file-notifier))
	 (descriptor
	  ;; if file-notify-watch for given DIR is already registered for other panel and it is valid, use it
	  (if (and
	       (string= (car other-side-notifier) dir)
	       (file-notify-valid-p (cdr other-side-notifier)))
	      (cdr other-side-notifier)
	    ;; otherwise create new one
	    (file-notify-add-watch dir '(change attribute-change) 'efar-notification))))
    
    (efar-set (cons dir descriptor) :panels side :file-notifier)))


(defun efar-remove-notifier(side &optional dont-check-other)
  "Remove file-notify-watch registered for panel SIDE.
If same watch is registered for other panel, then don't remove the watch, but just unregister it for panel SIDE, unless DONT-CHECK-OTHER is t"
  (let ((descriptor (cdr (efar-get :panels side :file-notifier)))
	(other-descriptor (cdr (efar-get :panels (efar-other-side side) :file-notifier))))
    (when (or dont-check-other (not (equal descriptor other-descriptor)))
      (file-notify-rm-watch descriptor))
    (efar-set nil :panels side :file-notifier)))

(defun efar-notification(event)
  "Callback function triggered when a file EVENT occurs.
It stores notification in the queue and runs timer for 1 second.
Notifications in the queue will be processed only if there are no new notifications within 1 second."
  (let ((descriptor (car event))
	(event-type (nth 1 event)))
    
    ;; for any event except 'stopped
    (unless (equal event-type 'stopped)
      ;; if eFAR buffer exists
      (when (get-buffer efar-buffer-name)
	
	;; if there is yet no notifications in the queue for given descriptor
	;; add descriptor to the queue
	(let ((pending-notifications (efar-get :pending-notifications)))
	  (unless (member descriptor pending-notifications)
	    (push descriptor pending-notifications)
	    (efar-set pending-notifications :pending-notifications)))
	
	;; if there is a running timer, cancel it
	(let ((timer (efar-get :notification-timer)))
	  (when timer (cancel-timer timer))
	  ;; create new timer
	  (efar-set (run-at-time "1 sec" nil
				 (lambda()
				   (efar-process-pending-notifications)))
		    :notification-timer))))))


(defun efar-process-pending-notifications()
  
  "Process all pending file notifications."
  
  ;; while there are notifications in a queue
  (while
      ;; pop next notification
      (let* ((pending-notifications (efar-get :pending-notifications))
	     (descriptor (prog1
			     (pop pending-notifications)
			   (efar-set pending-notifications :pending-notifications))))
	(when descriptor
	  ;; if event comes from the watch is registered for left panel directory, refersh left panel
	  (when (equal descriptor (cdr (efar-get :panels :left :file-notifier)))
	    (efar-refresh-panel :left))
	  ;; if event comes from the watch is registered for right panel directory, refersh right panel
	  (when (equal descriptor (cdr (efar-get :panels :right :file-notifier)))
	    (efar-refresh-panel :right))))))

;;--------------------------------------------------------------------------------
;; end of file notification functions
;;--------------------------------------------------------------------------------

(defun efar-remove-file-state()
  "Remove eFar state file.  Could be helpfull in case of errors during startup."
  (interactive)
  (delete-file efar-state-file-name))

(defun efar-save-state()
  "Save eFar state to the state file.  Data from this file is used during startup to restore last state."
  (with-temp-file efar-state-file-name
    (let ((copy (copy-hash-table efar-state)))
      
      ;; clear up data not not relevant for saving
      (puthash :file-notifier nil (gethash :left (gethash :panels copy)))
      (puthash :file-notifier nil (gethash :right (gethash :panels copy)))
      (puthash :pending-notifications () copy)
      (puthash :files () (gethash :left (gethash :panels copy)))
      (puthash :files () (gethash :right (gethash :panels copy)))
      (puthash :last-auto-read-buffer nil copy)
      
      (print copy (current-buffer)))))


(defun efar-read-state()
  "Read eFar state from the file."
  (interactive)
  (with-temp-buffer
    (insert-file-contents efar-state-file-name)
    (cl-assert (eq (point) (point-min)))
    (read (current-buffer))))

;;------------------------------------------------------------------
;; efar file operations
;;------------------------------------------------------------------
(defun efar-create-new-directory()
  "Create new directory."
  
  (efar-with-notification-disabled
   (let ((new-dir-name (read-string "Input name for new directory: "))
	 (side (efar-get :current-panel)))
     
     ;; try to create a directory
     (efar-retry-when-error
      (make-directory new-dir-name nil))
     
     ;; refresh panel and move selection
     (efar-refresh-panel side nil new-dir-name)
     
     ;; if other panel displays same directory as current one, refresh it as well
     (when (string= (efar-get :panels side :dir) (efar-get :panels (efar-other-side) :dir))
       (efar-refresh-panel (efar-other-side))))))



(defun efar-copy-or-move-files(operation)
  "Copy or move selected files depending on OPERATION."
  (unwind-protect
      (progn
	(efar-set-status :busy (if (equal operation :copy)
				   "Copying files..."
				 "Moving files..."))
	
	(efar-with-notification-disabled
	 (let* ((side (efar-get :current-panel))
		(todir  (efar-get :panels (efar-other-side) :dir) )
		(files (efar-selected-files side nil)))
	   
	   (when files
	     (efar-copy-or-move-files-int operation files (read-directory-name (if (equal operation :copy) "Copy selected files to " "Move selected files to ") todir todir)))
	   
	   (efar-refresh-panel :left)
	   (efar-refresh-panel :right)))
	
	(efar-set-status :ready "Ready"))))

(defun efar-copy-or-move-files-int(operation files todir &optional fromdir overwrite?)
  "Copy or move (depending on OPERATION) FILES into TODIR.
When FROMDIR is not defined then `default-directory' is used.
OVERWRITE? is a boolean indicating that existing files have to be overwritten.
Existing files can be skipped or overwritten depending on user's choice.
User also can select an option to overwrite all remaining files to not be asked multiple times."
  
  (cl-labels ;; make local recursive function (needed to share variable overwrite? in recursion)
      ((do-operation (operation files todir fromdir)
		     ;; if FROMDIR is not set, we use default-directory as source directory
		     (let ((default-directory (if fromdir fromdir default-directory)))
		       
		       (mapc ;; for each file in FILES
			(lambda (f)
			  ;; skip files "." and ".."
			  (unless (or (string= (car f) ".") (string= (car f) ".."))
			    ;; get new file name in destination folder
			    (let ((newfile
				   (if (file-directory-p todir)
				       (expand-file-name (efar-get-short-file-name f) todir)
				     todir)))
			      
			      (cond
			       ;; if file is a real file and doesn't exist in destination folder
			       ((and (not (equal (cadr f) t)) (not (file-exists-p newfile)))
				;; we just copy it using elisp function
				
				(if (equal operation :copy)
				    (efar-retry-when-error (copy-file (car f) newfile))
				  (efar-retry-when-error (rename-file (car f) newfile nil))))
			       
			       
			       ;; if file is a directory and doesn't exist in destination folder
			       ((and (equal (cadr f) t) (not (file-exists-p newfile)))
				;; we just copy directory using elisp function
				(if (equal operation :copy)
				    (efar-retry-when-error (copy-directory (car f) newfile nil nil nil))
				  (efar-retry-when-error (rename-file (car f) newfile))))
			       
			       ;; if file is a real file and does exist in destination folder
			       ((and (not (equal (cadr f) t)) (file-exists-p newfile))
				(progn
				  ;; we ask user what to do (overwrite, not overwrite, overwrite all remaining)
				  ;; if user was already asked before and the answer was "All", we don't ask again
				  (setf overwrite? (cond
						    ((or (null overwrite?) (string= overwrite? "No") (string= overwrite? "Yes"))
						     (ido-completing-read (concat "File " newfile " already exists. Overwrite? ") (list "Yes" "No" "All")))
						    (t overwrite?)))
				  ;; we copy file (overwrite) using elisp function if user approved it
				  (unless (string= overwrite? "No")
				    (if (equal operation :copy)
					(efar-retry-when-error (copy-file (car f) newfile t nil nil nil))
				      (efar-retry-when-error (rename-file (car f) newfile t))))))
			       
			       ;; if file is a directory and does exist in destination folder
			       ((and (equal (cadr f) t) (file-exists-p newfile))
				;; we call local function recursively
				(do-operation operation (directory-files (car f) nil nil t) newfile (expand-file-name (efar-get-short-file-name f) default-directory) )
				(when (equal operation :move)
				  (delete-directory (car f))))))))
			
			files))))
    ;; call local function first time
    (do-operation operation files todir fromdir)) )


(defun efar-delete-selected()
  "Delete selected file(s)."
  (efar-set-status :busy "Deleting files...")
    
    (unwind-protect
	(efar-with-notification-disabled
	 (let* ((side (efar-get :current-panel))
		(selected-files (efar-selected-files side nil)))
	   
	   (when (and selected-files (string= "Yes" (ido-completing-read "Delete selected files? " (list "Yes" "No"))))
	     (mapc(lambda (f)
		    (if (equal (cadr f) t)
			(efar-retry-when-error (delete-directory (car f) t))
		      (efar-retry-when-error (delete-file (car f)))))
		  
		  selected-files)
	     
	     (efar-refresh-panel side)
	     
	     (when (string= (efar-get :panels side :dir) (efar-get :panels (efar-other-side) :dir))
	       (efar-refresh-panel (efar-other-side)))))))
    
    (efar-set-status :ready "Ready"))

(defun efar-get(&rest keys)
  "Get value stored by KEYS."
  (let ((value nil))
    (mapc
     (lambda(key)
       (if value
	   (setf value (gethash key value))
	 (setf value (gethash key efar-state))))
     keys)
    value))

(defun efar-set(value &rest keys)
  "Store VALUE by KEYS."
  (let ((place nil))
    (mapc
     
     (lambda(key)
       (if place
	   (setf place (puthash key (gethash key place (make-hash-table :test `equal)) place))
	 (setf place (puthash key (gethash key efar-state (make-hash-table :test `equal)) efar-state))))
     
     (cl-subseq keys 0 -1))
    
    (puthash (car (cl-subseq keys -1)) value (or place efar-state))))

(defun efar-reset-status()
  "Reset eFar status to default one."
  (when (efar-get :reset-status?)
    (efar-set nil :reset-status?)
    (efar-set-status :ready "Ready")))

(defun efar-set-status(status &optional status-string seconds reset?)
  "Set eFar status to STATUS.
When STATUS-STRING is nil use default 'Ready' string.
When SECONDS is defined then status is displayed given time.
When RESET? is t then status will be automatically changed to default
on any next cursor movement."
  (when reset?
    (efar-set t :reset-status?))
  
  (let ((prev-status (efar-get :status))
	(prev-status-string (efar-get :status-string)))
    
    (efar-set status :status)
    (when status-string (efar-set status-string :status-string))
    (efar-output-status)
    
    (when seconds
      (run-at-time seconds nil
		   `(lambda()
		      (efar-set-status ',prev-status ',prev-status-string))))))

(defun efar-output-status(&optional status)
  "Output STATUS."
  (efar-write-enable
   
   (let* ((w (- (efar-get :window-width) 2))
	  (status-string (efar-prepare-file-name (or status (efar-get :status-string)) w)))
     
     (goto-char 0)
     (forward-line (+ 4 (efar-get :panel-height)))
     (move-to-column 1)
     (let ((p (point)))
       (replace-rectangle p (+ p w) status-string)
       (put-text-property p (+ p w) 'face 'efar-header-face))))
  (sit-for 0.001))


(defun efar-set-key-bindings()
  "Set up local key bindings for eFar buffer."
  (cl-loop for key in efar-keys do
	   (let ((key-seq (if (null (nth 3 key)) (nth 0 key) (symbol-value (nth 3 key)))))
	     (local-set-key
	      key-seq
	      `(lambda()
		 (interactive)
		 (efar-key-press-handle (nth 1 ',key) (nth 2 ',key) (nth 6 ',key)))))))

(defun efar-key-press-handle(func arg ignore-in-modes)
  "Handler for registered key-bindings.
This handler calls function FUNC with argument ARG only in case when current
mode is not in the list IGNORE-IN-MODES."
  (let ((mode (efar-get :panels (efar-get :current-panel) :mode)))
    
    (when (consp func)
      (setq func (symbol-function (cdr (assoc mode func)))))
    
    (if (cl-member mode ignore-in-modes)
	(efar-set-status :ready (concat "Function is not allowed in mode " (symbol-name mode)) nil t)
      
      (if arg
	  (funcall func arg)
	(funcall func)))))

(defun efar-show-help()
  "Display a buffer with list of registered Efar key bindings."
  (let ((buffer (get-buffer-create "*Efar key bindings*")))
    (with-current-buffer buffer
      (read-only-mode -1)
      (erase-buffer)
      
      (cl-loop for key in (reverse efar-keys) do
	       (when (nth 5 key)
		 (let ((key-seq (if (null (nth 3 key)) (nth 0 key) (symbol-value (nth 3 key)))))
		   (insert (key-description key-seq) "\t"  (nth 4 key) "\n")))
	       (when (equal :space-after (nth 5 key))
		 (insert "\n")))
      
      (align-regexp (point-min) (point-max) "\\(\\s-*\\)\t")
      (goto-char 0)
      (read-only-mode 1)
      (setq-local mode-line-format nil)
      (local-set-key (kbd "q") 'delete-frame))
    
    (display-buffer buffer)))


(defun efar-set-mouse-bindings()
  "Set bindings for mouse interaction."
  (local-set-key (kbd "<down-mouse-1>")
		 (lambda (event)
		   (interactive "e")
		   (if (<  (car (nth 6 (nth 1 event))) (+ 2 (efar-panel-width :left) ))
		       (progn
			 (efar-set :left :current-panel))
		     
		     (progn
		       (efar-set :right :current-panel)))
		   (efar-write-enable (efar-redraw)))))

(defun efar-cd()
  "Open directory selector (read-diretory-name) and go to selected directory."
  (efar-go-to-dir (read-directory-name "Go to directory: " default-directory))
  (efar-write-enable (efar-redraw)))

(defun efar-change-column-number(&optional increase)
  "Change the number of columns in current panel.
Increase by 1 when INCREASE is t, decrease by 1 otherwise."
  (let* ((side (efar-get :current-panel))
	 (panel-mode (efar-get :panels side :mode)))
    
    (if increase
	(efar-set (+ (efar-get :panels side :view panel-mode :column-number) 1) :panels side :view panel-mode :column-number)
      (when (> (efar-get :panels side :view panel-mode :column-number) 1)
	(efar-set (- (efar-get :panels side :view panel-mode :column-number) 1) :panels side :view panel-mode :column-number)
	
	(let ((file (car (efar-current-file side))))
	  (unless (string= file "..") (efar-go-to-file file))))))
  
  (efar-calculate-widths)
  (efar-write-enable (efar-redraw)))

(defun efar-change-mode()
  "Switch mode from double to single-panel or vice versa.
If a double mode is active then actual panel becomes fullscreen."
  (let ((current-mode (efar-get :mode))
	(side (efar-get :current-panel)))
    
    (efar-set (cond
	       ((equal current-mode :both) side)
	       (t :both))
	      :mode)
    
    (efar-calculate-widths)
    
    (efar-write-enable (efar-redraw))))

(defun efar-open-dir-other-panel()
  "Opens current pannel's direcotry in other panel."
  (efar-go-to-dir default-directory (efar-other-side))
  (efar-write-enable (efar-redraw)))

(defun efar-copy-current-path()
  "Copies to the clipboard the full path to the current file or directory."
  (kill-new
   (caar
    (efar-selected-files (efar-get :current-panel) t t))))

(defun efar-open-file-in-ext-app()
  "Open file under cursor in external application."
  (let* ((side (efar-get :current-panel))
	 (fnum (+ (efar-get :panels side :start-file-number) (efar-get :panels side :current-pos) ))
	 (file (nth fnum (efar-get :panels side :files)))
	 (@fname (expand-file-name (car file) (efar-get :panels side :dir))))
    
    (let* (
	   ($file-list
	    (if @fname
		(progn (list @fname))
	      (if (string-equal major-mode "dired-mode")
		  (dired-get-marked-files)
		(list (buffer-file-name)))))
	   ($do-it-p (if (<= (length $file-list) 5)
			 t
		       (y-or-n-p "Open more than 5 files? "))))
      (when $do-it-p
	(cond
	 ((string-equal system-type "windows-nt")
	  (mapc
	   (lambda ($fpath)
	     (w32-shell-execute nil $fpath)) $file-list))
	 ((string-equal system-type "darwin")
	  (mapc
	   (lambda ($fpath)
	     (shell-command
	      (concat "open " (shell-quote-argument $fpath))))  $file-list))
	 ((string-equal system-type "gnu/linux")
	  (mapc
	   (lambda ($fpath) (let ((process-connection-type nil))
			      (start-process "" nil "xdg-open" $fpath))) $file-list)))))))




(defun efar-filter-files()
  "Set up file filtering in current panel.
Ask user for file mask and show files in current panel matching this mask only."
  (let ((side (efar-get :current-panel)))
    
    (efar-set
     (read-string "String to filter file names: " (efar-get :panels side :file-filter))
     :panels
     side
     :file-filter)
    
    (efar-get-file-list side)
    (efar-set 0 :panels side :start-file-number)
    (efar-set 0 :panels side :current-pos)
    (efar-write-enable (efar-redraw))))

(defun efar-display-console ()
  "Open shell in new buffer and go to directory opened in current panel."
  (if (not (get-buffer "*efar-shell*"))
      (shell "*efar-shell*")
    (let ((side (efar-get :current-panel)))
      (with-current-buffer (get-buffer "*efar-shell*")
	(insert (concat "cd " (efar-get :panels side :dir)))
	(comint-send-input nil t))
      (display-buffer "*efar-shell*"))))


(defun efar-fast-search(k)
  "Activate and/or perform incremental search.
K is a character typed by the user."
  (let ((str (efar-get :fast-search-string)))
    
    (unless (member k '(:next :prev :back :clear))
      (setf str (concat str (format "%c" k))))
    
    (when (> (length str) 0)
      
      (when (equal k :back)
	(setf str (substring str 0 (- (length str) 1))))
      
      (let* ((side (efar-get :current-panel))
	     (filtered-list (mapcan (lambda (e)
				      (when (string-match str (if (equal (efar-get :panels side :mode) :files)
								  (efar-get-short-file-name e)
								(car e)))
					(list (car e))))
				    (efar-get :panels side :files))))
	
	(cond ((equal k :next)
	       (if (= (+ (efar-get :fast-search-occur) 1) (length filtered-list))
		   (efar-set 0 :fast-search-occur)
		 (efar-set (+ 1 (efar-get :fast-search-occur)) :fast-search-occur)))
	      
	      ((equal k :prev)
	       (if (= (efar-get :fast-search-occur) 0)
		   (efar-set (- (length filtered-list) 1) :fast-search-occur)
		 (efar-set (- (efar-get :fast-search-occur) 1) :fast-search-occur))))
	
	(let ((file-name (nth (efar-get :fast-search-occur) filtered-list)))
	  (when file-name
	    (efar-go-to-file file-name nil 0)
	    (efar-write-enable (efar-redraw))))))
    
    (efar-set str :fast-search-string))
  
  (efar-output-status (concat "Fast search: " (efar-get :fast-search-string))))

(defun efar-quit-fast-search()
  "Quite fast search mode."
  (unless (null efar-state)
    (efar-set "" :fast-search-string)
    (efar-output-status)
    (efar-set 0 :fast-search-occur)))

(defun efar-get-accessible-directory-in-path (path)
  "Return first accessible directory in the PATH going from bottom to up.
If there are no accessible directories, return `user-emacs-directory'."
  
  ;; if directory PATH exists or is accessible
  (if (and (file-exists-p path)
	   (file-accessible-directory-p path))
      ;; return this directory
      (if (equal path "/")
	  path
	(string-trim-right path "[/]"))
    ;; else get parent directory
    (let ((parent-dir
	   (file-name-directory
	    (directory-file-name path))))
      ;; if we are in root directory already
      (if (string= parent-dir path)
	  ;; that means there are no accessible directories in the path and we return user-emacs-directory
	  user-emacs-directory
	;; otherwise check parent directory
	(efar-get-accessible-directory-in-path parent-dir )))))

(defun efar-refresh-panel(&optional side move-to-first? move-to-file-name)
  "Refresh given panel (or a current one when SIDE is not given).
When MOVE-TO-FIRST? is t move cursot to the first file.
When MOVE-TO-FILE-NAME is given then move cursor to the file with that name."
  (let* ((side (or side (efar-get :current-panel)))
	 (dir (efar-get-accessible-directory-in-path (efar-get :panels side :dir))))
    (efar-set dir :panels side :dir)
    (efar-set () :panels side :selected)
    (efar-set "" :panels side :fast-search-string)
    
    (let ((current-file-name (cond
			      (move-to-first? "")
			      (move-to-file-name move-to-file-name)
			      (t (efar-current-file-name side))))
	  (current-file-number (if move-to-first? 0 (efar-current-file-number side))))
      
      (efar-get-file-list side)
      
      (when (> (length (efar-get :panels side :files)) 0)
	(efar-go-to-file current-file-name side current-file-number)))
    
    (efar-write-enable (efar-redraw))))


(defun efar-go-to-dir(dir &optional side)
  "Go to given DIR.
When SIDE is given show directory in this panel, otherwise in current one."
  
  (let* ((dir (when dir (efar-get-accessible-directory-in-path (expand-file-name dir))))
	 (side (or side (efar-get :current-panel)))
	 (current-dir (efar-get :panels side :dir))
	 (parent-dir (when (equal (efar-get :panels side :mode) :files) (efar-get-parent-dir current-dir)))
	 (go-to-parent? (string= dir parent-dir)))
    
    (when (eq side (efar-get :current-panel))
      (setf default-directory dir))
    
    (efar-set () :panels side :selected)
    (efar-set dir :panels side :dir)
    
    (efar-set "" :fast-search-string)
    
    (efar-get-file-list side)
    
    (if go-to-parent?
	(progn
	  (efar-go-to-file current-dir side 0))
      (progn
	(efar-set 0 :panels side :start-file-number)
	(efar-set 0 :panels side :current-pos)))
    
    (let* ((current-hist (efar-get :directory-history))
	   (new-hist (cl-subseq (cl-remove-if (lambda(e) (equal dir (car e))) current-hist)
				0 (when (> (length current-hist) efar-max-items-in-directory-history)
				    (- efar-max-items-in-directory-history 1)))))
      (push (cons (if (equal dir "/") dir (string-trim-right dir "[/]")) side) new-hist)
      
      (efar-set new-hist :directory-history))
    
    ;;(efar-set dir :last-visited-dirs (efar-get-root-directory dir))
    (efar-setup-notifier dir side)
    
    (efar-set :files :panels side :mode)))


(defun efar-get-parent-dir(dir)
  "Return parent directory of given DIR."
  (string-trim-right (file-name-directory (directory-file-name dir)) "[/]"))

(defun efar-go-to-file(file &optional side prev-file-number)
  "Move cursor to given FILE or to PREV-FILE-NUMBER if file cannot be found.
Do this in current panel, unless SIDE is given."
  (let* ((side (or side (efar-get :current-panel)))
	 (file (if (equal (efar-get :panels side :mode) :files)
		   (directory-file-name (expand-file-name file (efar-get :panels side :dir)))
		 file))
	 (number-of-files (length (efar-get :panels side :files)))
	 (panel-mode (efar-get :panels side :mode))
	 (column-number (efar-get :panels side :view panel-mode :column-number))
	 (new-file-number
	  (or
	   (cl-position file
			(mapcar (lambda (e) (car e)) (efar-get :panels side :files))
			:test 'string=)
	   (if (>= prev-file-number number-of-files)
	       (- number-of-files 1)
	     prev-file-number))))
    (cond
     
     ((and
       (>= new-file-number (efar-get :panels side :start-file-number))
       (< new-file-number (+ (efar-get :panels side :start-file-number) (* column-number (efar-get :panel-height)))))
      
      (progn
	(efar-set (- new-file-number (efar-get :panels side :start-file-number)) :panels side :current-pos)))
     
     ((< new-file-number (* column-number (efar-get :panel-height)))
      (progn
	(efar-set 0 :panels side :start-file-number)
	(efar-set new-file-number :panels side :current-pos)))
     
     (t
      (progn
	(efar-set new-file-number :panels side :start-file-number)
	(efar-set 0 :panels side :current-pos))))))


(defun efar-current-file-number(&optional side)
  "Return the number of file under cursor.
Do that for current panel or for panel SIDE if it's given."
  (let* ((side (or side (efar-get :current-panel)))
	 (start-file-number (efar-get :panels side :start-file-number)))
    
    (+ start-file-number (efar-get :panels side :current-pos))))

(defun efar-current-file-name(&optional side)
  "Return the full name of file under cursor.
Do that for current panel or for panel SIDE if it's given."
  (car (efar-current-file side)))

(defun efar-current-file(&optional side)
  "Return the file under cursor with it's attributes.
Do that for current panel or for panel SIDE if it's given."
  (let ((side (or side (efar-get :current-panel))))
    (nth (efar-current-file-number side) (efar-get :panels side :files))))

(defun efar-other-side(&optional side)
  "Return opposite panel to curent panel or to panel SIDE if one is given.
:left <-> :right"
  (let ((side (if side side (efar-get :current-panel))))
    (if (equal side :left) :right :left)))

(defun efar-deselect-all()
  "Unmark all marked files in current panel."
  (efar-write-enable
   (let ((side (efar-get :current-panel)))
     (efar-set '() :panels side :selected)
     (efar-redraw))))

(defun efar-mark-file()
  "Mark file under cursor in current panel."
  (let* ((side (efar-get :current-panel))
	 (start-file-number (efar-get :panels side :start-file-number))
	 (current-position (efar-get :panels side :current-pos))
	 (selected-file-number (+ start-file-number current-position))
	 (selected-items (efar-get :panels side :selected)))
    
    (or (string= (car (nth selected-file-number (efar-get :panels side :files))) "..")
	(if (member selected-file-number selected-items)
	    (efar-set (delete selected-file-number selected-items) :panels side :selected)
	  (efar-set (push selected-file-number selected-items) :panels side :selected)))
    
    (efar-move-cursor  :down)))

(defun efar-edit-file(&optional for-read?)
  "Visit file under cursor.
When FOR-READ? is t switch back to eFar buffer."
  (let* ((side (efar-get :current-panel))
	 (file (caar (efar-selected-files side t)))
	 (mode (efar-get :panels side :mode)))
    
    ;; open file in other window
    (when file
      (let ((buffer (find-file-other-window file)))
	
	;; if file is opened from search result list then enable isearch mode
	(when (and (equal mode :search)
		   (not (string-empty-p (or (cdr(assoc :text efar-last-search-params)) ""))))
	  (setq case-fold-search (cdr (assoc :ignore-case? efar-last-search-params)))
	  
	  (isearch-mode t (cdr (assoc :regexp? efar-last-search-params)))
	  
	  (let ((string (cdr (assoc :text efar-last-search-params))))
	    (if case-fold-search
		(setq string (downcase string)))
	    (isearch-process-search-string string
					   (mapconcat 'isearch-text-char-description string ""))))
	
	;; if file opened for editing unmark its buffer to prevent auto kill of the buffer
	(when (and (equal buffer (efar-get :last-auto-read-buffer))
		   (not for-read?))
	  (efar-set nil :last-auto-read-buffer))
	
	;; if file opened for reading only then goto back to eFar
	(when for-read?
	  (select-window (get-buffer-window (get-buffer efar-buffer-name))))
	
	buffer))))

(defun efar-set-files-order(files side)
  "Change sort direction of FILES in panel SIDE."
  (if (efar-get :panels side :sort-order)
      (reverse files)
    files))

(defun efar-is-root-directory(dir)
  "Return t if DIR is a root directory, nil otherwise."
  (string= (file-name-as-directory dir)
	   (file-name-directory (directory-file-name dir))))

(defun efar-get-file-list(side)
  "Read file list from the directory showed in panel SIDE."
  (let ((filter (efar-get :panels side :file-filter))
	(root? (efar-is-root-directory (efar-get :panels side :dir))))
    
    (efar-set
     ;; if we are not in the root directory, we add entry to go up
     (append (unless root? (list (list ".." t)))
	     ;; change order of files according to selected mode (ASC or DESC)
	     (efar-set-files-order
	      
	      ;; build file list
	      (let ((files
		     ;; remove entries "." and ".." and filter file list according to selected wildcard
		     (cl-remove-if
		      (lambda (f)  (or
				    (string-suffix-p "/." (car f))
				    (string-suffix-p "/.." (car f))
				    (and (or (not (cadr f))
					     efar-filter-directories?)
					 (not (string-suffix-p "/.." (car f)))
					 (> (length filter) 0 )
					 (not (string-match (wildcard-to-regexp filter) (car f))))))
		      
		      ;; files and attributes for current directory
		      (directory-files-and-attributes (efar-get :panels side :dir) t nil t)))
		    
		    ;; get selected sort function
		    (sort-function (efar-get-sort-function (efar-get :panels side :sort-function-name))))
		
		;; sort file list according to selected sort function
		(if sort-function
		    (sort files sort-function)
		  files))
	      
	      side))
     :panels side :files)))

(defun efar-move-cursor (direction)
  "Move cursor in direction DIRECTION."
  (let ((side (efar-get :current-panel)))
    
    (unless (= 0 (length (efar-get :panels side :files)))
      
      (efar-reset-status)
      (efar-quit-fast-search)
      
      (efar-write-enable
       (let* ((curr-pos (efar-get :panels side :current-pos))
	      (max-files-in-column (- (efar-get :panel-height) 1))
	      (max-file-number (length (efar-get :panels side :files)))
	      (start-file-number (efar-get :panels side :start-file-number))
	      (panel-mode (efar-get :panels side :mode))
	      (col-number (efar-get :panels side :view panel-mode :column-number))
	      (affected-item-numbers ()))
	 (cond
	  ;; if UP key pressed
	  ((equal direction :up)
	   (cond
	    ;; if we are on the first file in the list - do nothing
	    ((and (= start-file-number 0) (= curr-pos 0)) nil)
	    ;; if we are on first item
	    ((= curr-pos 0)
	     (progn (let ((rest (- start-file-number (* col-number max-files-in-column))))
		      (efar-set (if (< rest 0) 0 rest)
				:panels side :start-file-number)
		      (efar-set (if (< rest 0) (- start-file-number 1) (- (* col-number max-files-in-column) 1))
				:panels side :current-pos))))
	    ;; else move up by one
	    (t (progn
		 (push  curr-pos affected-item-numbers)
		 (cl-decf curr-pos)
		 (push  curr-pos affected-item-numbers)
		 (efar-set curr-pos :panels side :current-pos)))))
	  
	  ;; if DOWN key is pressed
	  ((equal direction :down)
	   (cond
	    ;; if we are on the last file in the list - do nohing
	    ((= (+ start-file-number curr-pos) (- max-file-number 1)) nil)
	    ;; else if we are on last item
	    ((= curr-pos (- (* max-files-in-column col-number) 1))
	     (progn (efar-set (+ start-file-number curr-pos 1)
			      :panels side :start-file-number)
		    (efar-set 0
			      :panels side :current-pos) ))
	    ;; else move down by one
	    (t (progn
		 (push  curr-pos affected-item-numbers)
		 (cl-incf curr-pos)
		 (push  curr-pos affected-item-numbers)
		 (efar-set curr-pos :panels side :current-pos)))))
	  
	  ;; if LEFT key is pressed
	  ((equal direction :left)
	   (cond
	    ;; if we are on the first file in the list - do nothing
	    ((and (= start-file-number 0) (= curr-pos 0)) nil)
	    
	    ;; we are in right column - move left by max-files-in-column
	    ((>= curr-pos max-files-in-column) (efar-set (- curr-pos max-files-in-column)
							 :panels side :current-pos))
	    
	    ;; we are in left column
	    ((< curr-pos max-files-in-column)
	     (cond
	      ((= start-file-number 0) (efar-set 0
						 :panels side :current-pos))
	      ((> start-file-number (* col-number max-files-in-column)) (efar-set (- start-file-number max-files-in-column)
										  :panels side :start-file-number))
	      ((<= start-file-number (* col-number max-files-in-column)) (and
									  (efar-set 0
										    :panels side :start-file-number)
									  (efar-set 0
										    :panels side :current-pos)))))))
	  
	  ;; if HOME key is pressed
	  ((equal direction :home)
	   (and
	    (efar-set 0
		      :panels side :start-file-number)
	    (efar-set 0
		      :panels side :current-pos)))
	  
	  
	  ;; if END key is pressed
	  ((equal direction :end)
	   (and
	    (efar-set (if (< max-file-number (* col-number max-files-in-column)) 0 (- max-file-number (* col-number max-files-in-column)))
		      :panels side :start-file-number)
	    (efar-set (- (if (< max-file-number (* col-number max-files-in-column))  max-file-number (* col-number max-files-in-column)) 1)
		      :panels side :current-pos)))
	  
	  
	  ;; if RIGHT key is pressed
	  ((equal direction :right)
	   (cond
	    ;; if we are on the last file in the list - do nohing
	    ((= (+ start-file-number curr-pos) (- max-file-number 1)) nil)
	    
	    ;; if there is more than max-files-in-column left then "scroll-down" column
	    ((> (- max-file-number start-file-number curr-pos) max-files-in-column)
	     (if (< curr-pos (* (- col-number 1) max-files-in-column))
		 (efar-set (+ curr-pos max-files-in-column)
			   :panels side :current-pos)
	       (efar-set (+ start-file-number max-files-in-column)
			 :panels side :start-file-number)))
	    ;; else go to the last file in the list
	    (t (efar-set (- max-file-number start-file-number 1) :panels side :current-pos)))))
	 
	 (efar-output-files side affected-item-numbers)
	 
	 (efar-output-file-details side)
	 
	 (condition-case err
	     (efar-auto-read-file)
	   (error (efar-set-status :ready (concat "Error: "(error-message-string err)) nil t))))))))

(defun efar-auto-read-file()
  "Automatically show content of the directory or file under cursor."
  
  (let* ((file (caar (efar-selected-files (efar-get :current-panel) t nil t)))
	 (file-ext (file-name-extension (downcase (or file "")))))
    (when (and file
	       (not (one-window-p)) ;; don't show when eFar occupies whole frame
	       (or  (and efar-auto-read-files ;; if file auto read is enabled
			 (< (file-attribute-size (file-attributes file)) efar-auto-read-max-file-size) ;; if file size is less than maximum allowed size
			 (or (and (equal (length efar-auto-read-file-extensions) 1) (string= "*" (car efar-auto-read-file-extensions))) ;; if any file type is configured to be auto read
			     (and file-ext (cl-member file-ext efar-auto-read-file-extensions :test 'equal)))) ;; or if files type is configured to be ato read
		    
		    (and efar-auto-read-directories ;; if directory auto read is enabled
			 (file-directory-p file)) ;; current item under cursor is a directory
		    
		    (and (not (file-directory-p file)) ;; if current-item under cursor is a file
			 (equal :search (efar-get :panels (efar-get :current-panel) :mode))))) ;; in a file search result
      
      (let ((last-auto-read-buffer (efar-get :last-auto-read-buffer))) ;; get the last auto read buffer
	;; we don't want to keep buffers for auto-opened files/dirs
	;; so we kill last auto read buffer if:
	;; - now we open different file
	;; - last auto read buffer is not modified
	(when (and
	       last-auto-read-buffer
	       (not (string= file (buffer-file-name last-auto-read-buffer)))
	       (not (buffer-modified-p last-auto-read-buffer)))
	  (kill-buffer last-auto-read-buffer))
	
	(let ((last-auto-read-buffer-exists? (get-file-buffer file)) ;; check if buffer for last auto read file exists
	      (buffer (efar-edit-file t))) ;; open current file for read
	  
	  ;; if buffer for last auto read file does not exists
	  ;; remember current buffer as last read one
	  (unless last-auto-read-buffer-exists?
 	    (efar-set buffer :last-auto-read-buffer)))))))

(defun efar-enter-directory()
  "Enter directory under cursor."
  
  (let* ((side (efar-get :current-panel))
	 (file (car (efar-selected-files side t t)))
	 (current-dir-path (efar-get :panels side :dir)))
    (efar-quit-fast-search)
    (when (or
	   ;; file is a normal directory
	   (equal (cadr file) t)
	   ;; file is a symlink pointing to the directory
	   (and (stringp (cadr file))
		(file-directory-p (cadr file))))
      (let ((newdir (expand-file-name (car file) current-dir-path)))
	(cond
	 
	 ((not (file-accessible-directory-p  newdir))
	  (efar-set-status :ready (concat "Directory "  newdir " is not accessible") 3))
	 
	 (t
	  (progn
	    (efar-go-to-dir newdir side)
	    (efar-calculate-widths)
	    (efar-write-enable (efar-redraw)))))))))

(defun efar-scroll-other-window(direction)
  "Scroll content of other window in direction DIRECTION."
  (scroll-other-window (if (eq direction :down) 1 -1)))

(defun efar-switch-to-other-panel()
  "Make other panel active."
  (efar-quit-fast-search)
  (when (equal (efar-get :mode) :both)
    (let ((side (efar-get :current-panel)))
      (if (equal side  :left)
	  (progn
	    (efar-set :right :current-panel)
	    (setf default-directory (efar-get :panels :right :dir)))
	(progn
	  (efar-set :left :current-panel)
	  (setf default-directory (efar-get :panels :left :dir)))))
    (efar-write-enable (efar-redraw))
    (condition-case err
	(efar-auto-read-file)
      (error (efar-set-status :ready (concat "Error: "(error-message-string err)) nil t)))))

(defun efar-calculate-window-size()
  "Calculate and set windows sizes."
  (efar-set (- (window-width) 1) :window-width)
  (efar-set (window-height) :window-height)
  (efar-set (- (window-height) 7) :panel-height))

(defun efar-redraw()
  "The main functio to output content of eFar buffer."
  (efar-calculate-window-size)
  (erase-buffer)
  (efar-draw-border )
  
  (put-text-property (point-min) (point-max) 'face 'efar-border-line-face)
  
  (efar-output-dir-names :left)
  (efar-output-dir-names :right)
  
  (efar-output-file-details :left)
  (efar-output-file-details :right)
  
  (efar-output-files :left)
  (efar-output-files :right)
  
  (when (equal (efar-get :panels :left :mode) :files)
    (efar-output-header :left))
  (when (equal (efar-get :panels :right :mode) :files)
    (efar-output-header :right))
  (efar-output-status))

(defun efar-get-short-file-name(file)
  "Return short name of a given FILE."
  (if (nth 1 file)
      (file-name-nondirectory (directory-file-name (nth 0 file)))
    (file-name-nondirectory (nth 0 file))))

(defun efar-output-file-details(side)
  "Output detailes of the file under cursor in panel SIDE."
  (let ((mode (efar-get :mode))
	(file (car (efar-selected-files side t nil t)))
	(width (efar-panel-width side))
	(status-string "Non-existing or not-accessible file!"))

    (when (and file
	       (not (null (efar-get :panels side :files)))
	       (or (equal mode :both) (equal mode side)))
      
      (let ((file-short-name (efar-get-short-file-name file)))

	(setf status-string (concat  (if (nth 1 file) "Directory: " "File: ")
				  file-short-name
				  "  Modified: "
				  (format-time-string "%D %T" (nth 6 file))
				  (if (and (not (nth 1 file)) (numberp (nth 8 file)))
				      (concat "  Size: " (int-to-string (nth 8 file))))))))

    (let ((col-number (cond
		       ((or (equal side :left) (equal mode :right)) 1)
		       (t (+ (efar-panel-width (efar-other-side side)) 2)))))
      (goto-char 0)

      (forward-line (+ 2 (efar-get :panel-height)))
      
      (move-to-column col-number)
      
      (let ((p (point)))
	(replace-rectangle p (+ p width) (efar-prepare-file-name status-string width))
	
	(put-text-property p (+ p width) 'face 'efar-border-line-face)))))

(defun efar-panel-width(side)
  "Calculate and return the width of panel SIDE."
  (let ((widths (if (equal side :left)
		    (car (efar-get :column-widths))
		  (cdr (efar-get :column-widths)))))
    
    (+ (apply '+ widths)
       (- (length widths) 1))))


(defun efar-output-files(side &optional affected-item-numbers)
  "Output the list of files in panel SIDE.
Redraw files with numbers in AFFECTED-ITEM-NUMBERS if given,
otherwise redraw all."
  (unless (= 0 (length (efar-get :panels side :files)))
    (let ((mode (efar-get :mode))
	  (widths (if (equal side :left)
		      (car (efar-get :column-widths))
		    (cdr (efar-get :column-widths)))))
      
      (when (or (equal mode :both) (equal mode side))
	
	(goto-char 0)
	(forward-line)
	
	(let* ((start-pos (cond ((equal side :left) 1)
				((and (equal side :right) (equal mode :right)) 1)
				(t (+ (efar-panel-width :left) 2))))
	       (max-files-in-column (- (efar-get :panel-height) 1))
	       (cnt 0)
	       (col-number (length widths))
	       
	       
	       (files
		(append
		 (cl-subseq  (efar-get :panels side :files)
			     (efar-get :panels side :start-file-number)
			     (+ (efar-get :panels side :start-file-number)
				(if (> (- (length (efar-get :panels side :files)) (efar-get :panels side :start-file-number)) (* max-files-in-column col-number))
				    (* max-files-in-column col-number)
				  (- (length (efar-get :panels side :files)) (efar-get :panels side :start-file-number)))))
		 
		 ;; append empty items if number of files to display is less then max files in panel
		 ;; in order to overwrite old entries
		 (make-list (let ((rest (- (length (efar-get :panels side :files)) (efar-get :panels side :start-file-number))))
			      (if (> rest (* max-files-in-column col-number))
				  0 (- (* max-files-in-column col-number) rest)))
			    (list "")))))
	  
	  (cl-loop for col from 0 upto (- col-number 1) do
		   
		   (let ((files-in-column (cl-subseq files (* col max-files-in-column) (* (+ col 1) max-files-in-column))))
		     
		     (cl-loop repeat (length files-in-column)  do
			      
			      (forward-line)
			      
			      (when (or (null affected-item-numbers) (member cnt affected-item-numbers))
				(let ((shift (+ start-pos
						(apply '+ (cl-subseq widths 0 col))
						col)))
				  
				  (move-to-column shift)
				  
				  (let* ((f (nth cnt files))
					 (p (point))
					 (w (nth col widths))
					 
					 (marked? (member (+ (efar-get :panels side :start-file-number) cnt) (efar-get :panels side :selected)))
					 
					 (disp-mode (car (efar-get :panels side :view (efar-get :panels side :mode) :file-disp-mode)))
					 
					 (exists? (file-exists-p (car f)))
					 (real-file (if (not (stringp (cadr f)))
							(cdr f)
						      (file-attributes (cadr f))))
					 
					 (dir? (car real-file))
					 (file? (not (car real-file)))
					 
					 (current? (and
						    (= cnt (efar-get :panels side :current-pos))
						    (equal side (efar-get :current-panel))))
					 
					 
					 (str (efar-prepare-file-name (concat (and marked? "*")
									      (pcase disp-mode
										(:short (concat (file-name-nondirectory (car f)) (when (and efar-add-slash-to-directories (car real-file) (not (equal (car f) "/"))) "/")))
										(:long (concat (car f)
											       (when (and efar-add-slash-to-directories (car real-file) (not (equal (car f) "/"))) "/")
											       (when (nth 13 f) (concat " (" (int-to-string (length (nth 13 f))) ")"))))
										(:detailed (efar-prepare-detailed-file-info f w))))
								      w
								      (eq :long disp-mode))))
				    
				    (replace-rectangle p (+ p (length str)) str)
				    
				    
				    (let ((current-face
					   (cond
					    ((and (not exists?) current?) 'efar-non-existing-current-file-face)
					    ((not exists?) 'efar-non-existing-file-face)
					    
					    ((and dir? current?) 'efar-dir-current-face)
					    ((and file? current?) 'efar-file-current-face)
					    (marked? 'efar-marked-face)
					    
					    ((and dir? (not current?)) 'efar-dir-face)
					    ((and file? (not current?)) 'efar-file-face) )))
				      (put-text-property p (+ p w) 'face current-face)))))
			      
			      
			      (cl-incf cnt)))
		   
		   (goto-char 0)
		   (forward-line)))))))


(defun efar-output-header(side)
  "Output eFar header in panel SIDE."
  (let ((mode (efar-get :mode)))
    
    (when (or (equal mode :both) (equal mode side))
      
      (goto-char 0)
      (forward-line)
      
      (let* ((col-number (cond
			  ((or (equal side :left) (not (equal mode :both))) 1)
			  (t (+ (efar-panel-width :left) 2))))
	     
	     (filter (efar-get :panels side :file-filter))
	     (str (concat
		   (substring (efar-get :panels side :sort-function-name) 0 1)
		   (if (efar-get :panels side :sort-order) (char-to-string 9660) (char-to-string 9650) )
		   (unless (string-empty-p filter)
		     (concat " " filter )))))
	
	(move-to-column col-number)
	
	(let ((p (point)))
	  (replace-rectangle p (+ p (length str)) str)
	  (put-text-property p (+ p (length str)) 'face 'efar-header-face))))))


(defun efar-prepare-file-name(fname len &optional cut-from-beginn?)
  "Prepare file name FNAME.
File name is truncated to be not more than LEN characters long.
When CUT-FROM-BEGINN? is t then name is truncated from the beginn,
otherwise from end."
  (let ((cut-from-beginn? (or cut-from-beginn?)))
    
    (cond ((> (length fname) len)
	   (if cut-from-beginn?
	       (concat "<" (cl-subseq fname (+ (- (length fname) len) 1)))
	     (concat (cl-subseq fname 0 (- len 1)) ">")))
	  
	  ((< (length fname) len)
	   (concat fname (make-string (- len (length fname)) ?\s)))
	  
	  ((= (length fname) len)
	   fname))))

(defun efar-output-dir-names(side)
  "Output current directory name for panel SIDE."
  (let ((mode (efar-get :mode)))
    
    (when (or (equal mode :both) (equal mode side))
      
      (goto-char 0)
      
      (let* ((dir
	      (if (> (length (efar-get :panels side :dir)) (efar-panel-width side))
		  (cl-subseq (efar-get :panels side :dir) (- (length (efar-get :panels side :dir)) (efar-panel-width side)))
		(efar-get :panels side :dir)))
	     
	     (col-number (cond
			  ((not (equal mode :both))  (- (floor (window-width) 2) (floor (length dir) 2)))
			  (t (- (* (floor (window-width) 4) (if (equal side :left) 1 3)) (floor (length dir) 2))))))
	
	(move-to-column col-number)
	
	(let ((p (point)))
	  (replace-rectangle p (+ p (length dir)) dir)
	  (if (equal side (efar-get :current-panel))
	      (put-text-property p (+ p (length dir)) 'face 'efar-dir-name-current-face)
	    (put-text-property p (+ p (length dir)) 'face 'efar-dir-name-face)))))))


(defun efar-draw-border()
  "Draw eFar border using pseudo graphic characters."
  (goto-char 0)
  
  (let ((panel-height (efar-get :panel-height)))
    
    ;; insert first line
    
    (efar-draw-border-line
     #x2554 ;; ╔
     #x2566 ;; ╦
     #x2557 ;; ╗
     #x2550 ;; ═
     #x2564
     t) ;; ╤
    
    ;; insert vertical lines
    (cl-loop repeat panel-height do
	     
	     (efar-draw-border-line
	      #x2551 ;; ║
	      #x2551 ;; ║
	      #x2551 ;; ║
	      #x0020 ;; space
	      #x2502 ;; │
	      t))
    
    
    (efar-draw-border-line
     9568 ;; 
     9580
     9571 ;; 
     #x2550 ;; ═
     9575
     t) ;; 
    
    (efar-draw-border-line
     #x2551 ;; ║
     #x2551 ;; ║
     #x2551 ;; ║
     #x0020 ;; space
     nil
     t)
    
    (efar-draw-border-line
     9568 ;;
     #x2569 ;; ╩
     9571 ;;
     #x2550 ;; ═
     nil
     t)
    
    (efar-draw-border-line
     #x2551 ;; ║
     #x0020
     #x2551 ;; ║
     #x0020 ;; space
     nil
     t)
    
    (efar-draw-border-line
     #x255A ;; ╚
     
     #x2550 ;; ═
     #x255D ;; ╝
     #x2550 ;; ═
     nil)))


(defun efar-draw-border-line(left center right filler splitter &optional newline)
  "Draw border line according to given arguments.
LEFT is a character to be used to draw left border.
CENTER is a character to be used to draw vertical splitter between panels.
RIGHT is a character to be used to draw right border.
FILLER is a character to be used as a filler between borders
when SPLITTER is not given, otherwise use SPLITTER.
NEWLINE - if t the insert newline character."
  (let ((mode (efar-get :mode)))
    
    (cl-loop for side
	     from (if (or (equal mode :left) (equal mode :both)) 1 2)
	     upto (if (or (equal mode :right) (equal mode :both)) 2 1)
	     initially do (insert-char left 1)
	     finally do (insert-char right 1)
	     do
	     (let* ((s (if (= side 1) :left :right))
		    (panel-mode (efar-get :panels s :mode))
		    (column-number (efar-get :panels s :view panel-mode :column-number)))
	       
	       (cl-loop for col from 0 upto (- column-number 1)	do
			
			(let ((col-number column-number))
			  
			  (insert-char filler (nth col (if (= side 1)
							   (car (efar-get :column-widths))
							 (cdr (efar-get :column-widths)))))
			  
			  (if (not (= (+ col 1) col-number))
			      (insert-char (or splitter filler) 1))))
	       
	       (when (and (equal mode :both) (equal s :left))
		 (insert-char center 1))))
    
    (when newline
      (newline)
      (forward-line))))

(defun efar-calculate-widths()
  "Calculate widths of all columns in the panels."
  (let* ((widths ())
	 (left-widths ())
	 (right-widths ())
	 
	 (window-width (efar-get :window-width))
	 (mode (efar-get :mode))
	 (cols-left (efar-get :panels :left :view (efar-get :panels :left :mode) :column-number))
	 (cols-right (efar-get :panels :right :view (efar-get :panels :right :mode) :column-number))
	 
	 (left-width (cond
		      ((equal mode :both) (floor (- window-width 3) 2))
		      ((equal mode :left) (- window-width 2))
		      ((equal mode :right) 0)))
	 
	 (right-width (cond
		       ((equal mode :both) (ceiling (- window-width 3) 2))
		       ((equal mode :left) 0)
		       ((equal mode :right) (- window-width 2)))))
    
    (unless (zerop left-width)
      (let* ((left-min-col-width (floor (/ (- left-width (- cols-left 1)) cols-left)))
	     (left-leftover (- left-width (- cols-left 1) (* left-min-col-width cols-left))))
	
	(setf left-widths (make-list cols-left left-min-col-width))
	
	(let ((cnt 0))
	  (while (not (zerop left-leftover))
	    (cl-incf (nth cnt left-widths))
	    (cl-decf left-leftover)
	    (cl-incf cnt)
	    (when (= cnt (length left-widths))
	      (setf cnt 0))))))
    
    (unless (zerop right-width)
      (let* ((right-min-col-width (floor (/ (- right-width (- cols-right 1))  cols-right)))
	     (right-leftover (- right-width (- cols-right 1) (* right-min-col-width cols-right))))
	
	(setf right-widths (make-list cols-right right-min-col-width))
	
	(let ((cnt 0))
	  (while (not (zerop right-leftover))
	    (cl-incf (nth cnt right-widths))
	    (cl-decf right-leftover)
	    (cl-incf cnt)
	    (when (= cnt (length right-widths))
	      (setf cnt 0))))))
    
    (setf widths (cons left-widths right-widths))
    
    (efar-set widths :column-widths)))


(defun efar-frame-size-changed(frame)
  "Function called when FRAME size is changed.
Redraws entire eFar buffer."
  (let ((window (get-buffer-window efar-buffer-name)))
    (when (and window
	       (equal (window-frame window) frame))
      (efar-calculate-window-size)
      (efar-calculate-widths)
      (efar-write-enable
       (efar-redraw)))))

(defun efar-buffer-killed()
  "Function is called when eFar buffer is killed.
Saves eFar state and kills all subprocesses."
  (when (string= (buffer-name) efar-buffer-name)
    (efar-search-kill-all-processes)
    (when (and
	   efar-save-state?
	   
	   (ignore-errors
	     (efar-save-state))
	   (efar-remove-notifier :left)
	   (efar-remove-notifier :right)))
    (setf efar-state nil)))

(defun efar-emacs-killed()
  "Function is called when Emacs is killed.
Saves eFar state and kills all subprocesses."
  (efar-search-kill-all-processes)
  (when (and
	 efar-save-state?
	 (get-buffer efar-buffer-name))
    (ignore-errors
      (efar-save-state))))

;; hooks
(add-hook 'window-size-change-functions 'efar-frame-size-changed)
(add-hook 'kill-buffer-hook 'efar-buffer-killed)
(add-hook 'kill-emacs-hook 'efar-emacs-killed)

(defun efar-get-root-directory(path)
  "Return a root directory for given PATH."
  ;; get parent directory
  (let ((parent-dir
	 (file-name-directory
	  (directory-file-name path))))
    ;; if we are in root directory already
    (if (string= parent-dir path)
	path
      ;; otherwise check parent directory
      (efar-get-root-directory parent-dir ))))

(defun efar-selected-files(side current? &optional up-included? skip-non-existing?)
  "Return a list of selected files in panel SIDE.
When CURRENT? is t return file under cursor only even if some more files marked.
When UP-INCLUDED? is t include '..' directory in the list.
When SKIP-NON-EXISTING? is t then non-existing files removed from the list."
  (let* ((marked-files (efar-get :panels side :selected))
	 (start-file-number (efar-get :panels side :start-file-number))
	 (current-file-number (+ start-file-number (efar-get :panels side :current-pos)))
	 (files (efar-get :panels side :files))
	 
	 (selected-files (when (> (length files) 0)
			   (cl-remove-if (lambda(f) (and skip-non-existing? (not (file-exists-p (car f)))))
					 (remove (unless up-included? (list ".." t))
						 (mapcar
						  (lambda (fn)
						    (nth fn files))
						  (if (or current? (not marked-files))
						      (list current-file-number)
						    marked-files)))))))
    selected-files))


(defun efar-abort()
  "Abort current operation."
  (when (string-empty-p (efar-get :fast-search-string))
    (let ((side (efar-get :current-panel)))
      (when (cl-member (efar-get :panels side :mode) '(:search :bookmark :dir-hist :disks))
	(efar-go-to-dir (efar-last-visited-dir side) side)
	(efar-write-enable (efar-redraw)))))
  (efar-quit-fast-search))

(defun efar-last-visited-dir(&optional side)
  "Return last visited directory for the panel SIDE."
  (let ((side (or side (efar-get :current-panel))))
    (catch 'dir
      (cl-loop for d in (efar-get :directory-history) do
	       (when (equal (cdr d) side)
		 (throw 'dir (car d)))))))

(defun efar-change-file-disp-mode()
  "Change file display mode.
Mode is changed in the loop :short -> :detail -> :long."
  (let* ((side (efar-get :current-panel))
	 (modes (efar-get :panels side :view (efar-get :panels side :mode) :file-disp-mode)))
    (efar-set (reverse (cons (car modes) (reverse (cdr modes))))
	      :panels side :view (efar-get :panels side :mode) :file-disp-mode))
  (efar-write-enable (efar-redraw)))

(defun efar-prepare-detailed-file-info(file width)
  "Prepare string with detailed info for FILE.
Truncate string to WIDTH characters."
  (if (or (equal (car file) "..") (string-empty-p (car file)))
      (car file)
    (let ((size (if (cadr file)
		    "DIR"
		  (efar-file-size-as-string (nth 8 file))))
	  (name (file-name-nondirectory  (car file)))
	  (time (format-time-string "%D %T" (nth 6 file))))
      
      (if (< width 35)
	  (concat (cond ((< (length name) 6)
			 (concat name " "))
			(t
			 (concat (cl-subseq name 0 4) "> ")))
		  (format "%7s" size) "   " time)
	
	(let ((max-name-width (- width 30)))
	  
	  (if (> (length name) max-name-width)
	      (concat (cl-subseq name 0 (- max-name-width 1)) ">   " (format "%7s" size) "   " time)
	    (concat name (make-string (- max-name-width (length name)) ?\s) "   " (format "%7s" size) "   " time)))))))

(defun efar-file-size-as-string(size)
  "Prepare human readable string representing file SIZE."
  (cond ((< size 1024)
	 (concat (int-to-string size) "  B"))
	((< size 1048576)
	 (concat (int-to-string (/ size 1024)) " KB"))
	((< size 1073741824)
	 (concat (int-to-string (/ size 1024 1024)) " MB"))
	(t
	 (concat (int-to-string (/ size 1024 1024 1024)) " GB"))))

(defun efar-ediff-files()
  "Run ediff to compare files under cursor in both panels."
  (let ((file1 (efar-selected-files :left nil))
	(file2 (efar-selected-files :right nil)))
    (if (or
	 (not (= 1 (length file1)))
	 (not (= 1 (length file2)))
	 (car (cadr file1))
	 (car (cadr file2)))
	(efar-set-status :ready "Please mark 2 files to run ediff" 5 t)
      (ediff (caar file1) (caar file2)))))

(defun efar-current-file-stat()
  "Display statisctics for selected file/directory."
  (let ((ok? nil)
	(current-file-entry (caar (efar-selected-files (efar-get :current-panel) t))))
    
    (unwind-protect
	(let ((size 0)
	      (files 0)
	      (dirs 0)
	      (skipped 0)
	      (start-time (time-to-seconds (current-time))))
	  
	  (efar-set-status :busy (format "Calculating size of '%s'..." current-file-entry))
	  
	  (cl-labels
	      ((int-file-size(file)
			     ;; if item is a directory
			     (if (file-directory-p file)
	        		 ;; if directory is not accessible
				 (if (not (file-accessible-directory-p file))
				     ;; increment number of skipped directories
				     (cl-incf skipped)
				   
				   ;; get all items in the directory and call function recursively for each item
				   (mapc (lambda(f)
					   (unless (member f '(".." "."))
					     (int-file-size (expand-file-name f file))))
					 (directory-files file nil nil t))
				   (cl-incf dirs))
			       
			       ;; otherwise if item is a file
			       (cl-incf size (nth 7 (file-attributes file)))
			       (cl-incf files))))
	    
	    ;; run calculation for the current item in current panel
	    (int-file-size current-file-entry))
	  
	  ;; output stat in the status line
	  (efar-set-status :ready
			   (format (concat "%d bytes (%s) in %d directries and %d files."
					   " %d seconds."
					   (unless (zerop skipped) " %d directories skipped (no access)." ))
				   size (efar-file-size-as-string size) dirs files (- (time-to-seconds (current-time)) start-time) skipped)
			   nil t)
	  (setf ok? t))
      
      (unless ok?
	(efar-set :ready "Size calculation failed")))))

(defun efar-show-directory-history(&optional side)
  "Show list of last visited directories in panel SIDE."
  (let ((side (or side (efar-get :current-panel))))
    (efar-set (mapcar (lambda(d) (let ((attrs (file-attributes (car d))))
				   (push (car d) attrs)))
		      (efar-get :directory-history))
	      :panels side :files)
    
    (efar-set "Directory history" :panels side :dir)
    (efar-remove-notifier side)
    (efar-set 0 :panels side :current-pos)
    (efar-set :dir-hist :panels side :mode))
  
  (efar-calculate-widths)
  (efar-write-enable (efar-redraw)))

(defun efar-add-bookmark()
  "Add file under cursor to bookmark list."
  (let ((current-file-entry (caar (efar-selected-files (efar-get :current-panel) t)))
	(bookmarks (efar-get :bookmarks)))
    (push current-file-entry bookmarks)
    (efar-set bookmarks :bookmarks))
  (when (equal (efar-get :panels :left :mode) :bookmark)
    (efar-show-bookmarks :left))
  (when (equal (efar-get :panels :right :mode) :bookmark)
    (efar-show-bookmarks :right)))


(defun efar-show-bookmarks(&optional side)
  "Show list of bookmarks in panel SIDE."
  (let ((side (or side (efar-get :current-panel))))
    (efar-set (mapcar (lambda(d) (let ((attrs (file-attributes d)))
				   (push d attrs)))
		      (efar-get :bookmarks))
	      :panels side :files)
    
    (efar-set "Bookmarks" :panels side :dir)
    (efar-remove-notifier side)
    (efar-set 0 :panels side :current-pos)
    (efar-set :bookmark :panels side :mode))
  
  (efar-calculate-widths)
  (efar-write-enable (efar-redraw)))

(defun efar-delete-bookmark()
  "Delete file from the bookmark list."
  (let* ((side (efar-get :current-panel))
	 (bookmarks (efar-get :bookmarks))
	 (entry (caar (efar-selected-files side t))))
    (when (and bookmarks
	       (string= "Yes" (ido-completing-read "Delete bookmark? " (list "Yes" "No"))))
      (setq bookmarks (cl-remove entry bookmarks :test 'equal))
      (efar-set bookmarks :bookmarks)
      (efar-show-bookmarks side))))

(defun efar-navigate-to-file()
  "Go to the file under cursor."
  (let ((entry (caar (efar-selected-files (efar-get :current-panel) t nil t))))
    (efar-quit-fast-search)
    (when entry
      (efar-go-to-dir (file-name-directory entry))
      (when (and (file-name-nondirectory entry) (not (string-empty-p (file-name-nondirectory entry))))
	(efar-go-to-file (file-name-nondirectory entry)))
      (efar-calculate-widths)
      (efar-write-enable (efar-redraw)))))

(defun efar-show-disk-selector()
  "Show menu with available disks (Windows) or mount points (Unix)."
  (let ((dirs (nconc (if (eq window-system 'w32)
			 (mapcar (lambda(e) (car (split-string e " " t)))
				 (cdr (split-string  (downcase (shell-command-to-string "wmic LogicalDisk get Caption"))
						     "\r\n" t)))
		       (split-string (shell-command-to-string "df -h --output=target | tail -n +2") "\n" t))))
	(side (efar-get :current-panel)))
    
    (efar-set (mapcar (lambda(d) (list d (file-directory-p d)))
		      dirs)
	      :panels side :files)
    (efar-set "Disks" :panels side :dir)
    (efar-remove-notifier side)
    (efar-set 0 :panels side :current-pos)
    (efar-set :disks :panels side :mode))
  
  (efar-calculate-widths)
  (efar-write-enable (efar-redraw)))


(defun efar-suggest-hint()
  "Display in the statusbar the next tip for key bindings."
  (let* ;; get next tip number to show
      ((hint-number (efar-get :next-hint-number))
       ;; all registered key bindings having description
       (keys (reverse (cl-remove-if (lambda(e) (not (nth 5 e))) efar-keys)))
       ;; key binding to show this time
       (key (nth hint-number keys)))
    
    ;; display tip in the statusbar
    (let ((key-seq (if (null (nth 3 key)) (nth 0 key) (symbol-value (nth 3 key)))))
      (efar-set-status :ready (concat "Hint: use key binding '" (key-description key-seq) "' to " (nth 4 key) " (C-n to get next hint)") nil t))
    
    ;; calculate and store next tip number
    (if (= (length keys) (+ hint-number 1))
	(setq hint-number 0)
      (cl-incf hint-number))
    (efar-set hint-number :next-hint-number)))


;;--------------------------------------------------------------------------------
;; File search functionality
;;--------------------------------------------------------------------------------

(define-button-type 'efar-search-find-file-button
  'follow-link t
  'action #'efar-search-find-file-button
  'face 'efar-search-file-link-face)

(define-button-type 'efar-search-find-line-button
  'follow-link t
  'action #'efar-search-find-file-button
  'face 'efar-search-line-link-face)

(defun efar-search-process-command()
  "Prepare the list with command line arguments for the search processes."
  (list (expand-file-name invocation-name invocation-directory)
	"--batch"
	"-l" (symbol-file 'efar)
	"-eval" "(efar-process-search-request)"))

(defun efar-run-search-processes()
  "Run search processes."
  (efar-search-kill-all-processes)
  (sleep-for 1)
  (efar-search-start-server)
  (setq efar-search-clients '())
  (setq efar-search-process-manager (efar-make-search-process))
  (efar-search-send-command efar-search-process-manager
			    :run-search-processes
			    '()))

(defun efar-start-search()
  "Start file search."
  (when efar-search-running?
    (when (string= "Yes" (ido-completing-read "Search is still running. Kill it? " (list "Yes" "No" )))
      (efar-run-search-processes)
      (setq efar-search-running? nil)))
    
  (unless efar-search-running?
    (let* ((side (efar-get :current-panel))
	   (selected-files (or (efar-selected-files side t) (list (list default-directory)))))
      
      (if (not (file-directory-p (caar selected-files)))
	  (efar-set-status :ready "Please select a directory to search files in" nil t)
	
	(let* ((wildcard (read-string "File name mask: " efar-search-default-file-mask))
	       (text (read-string "Text to search: " ""))
	       (ignore-case? (and (not (string-empty-p text)) (string=  "Yes" (ido-completing-read "Ignore case? " (list "Yes" "No")))))
	       (regexp? (and (not (string-empty-p text)) (string=  "Yes" (ido-completing-read "Use regexp? " (list "No" "Yes"))))))
	  
	  (setq efar-last-search-params nil)
	  (setq efar-last-search-params (list (cons :dir (caar selected-files))
					      (cons :wildcard wildcard)
					      (cons :text (if (string-empty-p text) nil text))
					      (cons :ignore-case? ignore-case?)
					      (cons :regexp? regexp?)
					      (cons :start-time (time-to-seconds (current-time)))))
	  
	  (setq efar-update-search-results-timer
		(run-at-time nil 1
			     (lambda()
			       (when (equal :search (efar-get :panels :left :mode))
				 (efar-show-search-results nil :left t))
			       
			       (when (equal :search (efar-get :panels :right :mode))
				 (efar-show-search-results nil :right t)))))
	  (setq efar-search-results '())
	  (setq efar-search-running? t)
	  (efar-show-search-results)
	  (efar-search-send-command efar-search-process-manager
				    :start-search
				    efar-last-search-params))))))


(defun efar-search-kill-all-processes()
  "Kill all search processes."
  (when (and efar-search-process-manager (process-live-p efar-search-process-manager))
    (process-send-string efar-search-process-manager (concat (prin1-to-string (cons :exit '())) "\n")))

  (setq efar-search-process-manager nil)

  (cl-loop for proc in efar-search-processes do
	   (kill-process proc))

  (when (and efar-search-server
	     (process-live-p efar-search-server))
    (delete-process efar-search-server))
  (setq efar-search-server nil))

(defun efar-search-start-server()
  "Start search server which will consume messages from search subprocesses."
  (when (and efar-search-server
	     (process-live-p efar-search-server))
    (delete-process efar-search-server))
  
  (setq efar-search-server
	(make-network-process :server t
			      :service t
			      :name "efar-search-server"
			      :filter #'efar-search-process-filter
			      :sentinel #'efar-search-server-sentinel
			      :noquery t))
  (set-process-query-on-exit-flag efar-search-server nil)
  (setq efar-search-server-port (cadr (process-contact efar-search-server))))

(defun efar-search-server-sentinel(proc message)
  "Set up sentinel for the search server.
When a connection from subprocess PROC is opened (MESSAGE 'open'),
this subprocess is registered in the list."
  (unless (equal proc efar-search-server)
    (when (and (process-live-p proc)
	       (equal message "open from 127.0.0.1\n"))
      (set-process-query-on-exit-flag proc nil)
      (push (process-name proc) efar-search-clients))))

(defun efar-search-finished()
  "Function is called when search is finished."
  (while (accept-process-output))
  
  (when (timerp efar-update-search-results-timer)
    (cancel-timer efar-update-search-results-timer))
  (setq efar-update-search-results-timer nil)
  
  (push (cons :end-time (time-to-seconds (current-time))) efar-last-search-params)

  (setq efar-search-running? nil)

  (efar-show-search-results 'sorted nil nil))

(defun efar-make-search-process()
  "Make search subprocess."
  (let ((proc (make-process
	       :name "efar-search-process"
	       :stderr (get-buffer-create "*eFar search error*")
	       :command (efar-search-process-command)
	       :filter #'efar-search-process-filter
	       :coding efar-search-coding
	       :noquery t)))

    (set-process-query-on-exit-flag proc nil)
    
    (efar-search-send-command proc :setup-server-connection efar-search-server-port)
    
    proc))

(defun efar-search-send-command(proc command args)
  "Send COMMAND and ARGS to the subprocess PROC."
  (let ((cmd (concat (prin1-to-string (cons command args)) "\n")))
    
    (if (process-live-p proc)
	(process-send-string proc cmd)
      (message "Process is not active!"))))

(defun efar-search-process-filter (proc string)
  "Filter function for the search server.
Processes message STRING arriving from search subprocess PROC."
  (let ((pending (assoc proc efar-search-process-pending-messages))
        message
        index)
    ;;create entry if required
    (unless pending
      (setq efar-search-process-pending-messages (cons (cons proc "") efar-search-process-pending-messages))
      (setq pending  (assoc proc efar-search-process-pending-messages)))
    
    (setq message (concat (cdr pending) string))
    
    (while (setq index (string-match "\n" message))
      (setq index (1+ index))
      
      (let* ((res (car (read-from-string (substring message 0 index))))
	     (message-type (car res))
	     (data (cdr res)))
	
	(efar-search-process-message proc message-type data))
      
      (setq message (substring message index)))
    
    (setcdr pending message)))


(defun efar-search-process-message(proc message-type data)
  "Process a message form subprocess PROC.
Message consists of MESSAGE-TYPE and DATA."
  (when (equal message-type :found-file)
    (let* ((file (cdr (assoc :name data)))
	   (attrs (file-attributes file)))
      (push (append (push file attrs) (list (cdr (assoc :lines data)))) efar-search-results)))

  (when (equal message-type :finished)
    (setq efar-search-clients (cl-remove (process-name proc) efar-search-clients :test 'equal))

    (unless efar-search-clients
      (efar-search-finished)))

  (when (equal message-type :file-error)
    (let ((errors (cdr (assoc :errors efar-last-search-params))))
      (push data errors)
      (push (cons :errors errors) efar-last-search-params)))
  
  (when (equal message-type :common-error)
    ;; restart search processes
    (push (cons :errors data) efar-last-search-params)
    (efar-search-kill-all-processes)
    (make-thread 'efar-run-search-processes)
    (efar-search-finished)
    (efar-set-status :ready (concat "Error occured during search: " data))))

(defun efar-search-int-start-search(args)
  "Start file search with parameters defined in ARGS in the subprocess."
  (let ((dir (cdr (assoc :dir args)))
	(wildcard (cdr (assoc :wildcard args)))
	(text (cdr (assoc :text args)))
	(regexp? (or (cdr (assoc :regexp? args)) nil))
	(ignore-case? (or (cdr (assoc :ignore-case? args)) nil)))
    
    (efar-search-files-recursively dir wildcard text regexp? ignore-case?)
    
    (cl-loop for proc in efar-search-processes do
	     (efar-search-send-command proc :finished '()))

    (process-send-string efar-search-server (concat (prin1-to-string (cons :finished '())) "\n"))))

(defun efar-search-files-recursively(dir wildcard &optional text regexp? ignore-case?)
  "Main search function.
Does the search for files in directory DIR.
Files should match file mask WILDCARD.
When TEXT is not nil given string is searched in the files.
When REGEXP? is t text is tritted as regular expression.
Case is ignored when IGNORE-CASE? is t."
  ;; loop over all entries in the DIR
  (cl-loop for entry in (cl-remove-if (lambda(e) (or (string= (car e) ".") (string= (car e) ".."))) (directory-files-and-attributes dir nil nil t)) do

	   (let* ((symlink? (stringp (cadr entry)))
		  (dir? (or (equal (cadr entry) t)
			    (and symlink?
				 (file-directory-p (cadr entry)))))
		  (real-file-name (expand-file-name (if symlink?
							(cadr entry)
						      (car entry))
						    dir)))
	     
	     ;; we process directory only if it is readable
	     ;; otherwise we skip it and report an error
	     (if (and dir?
		      (not (file-readable-p real-file-name)))
		 (process-send-string  efar-search-server (concat (prin1-to-string (cons :file-error real-file-name)) "\n"))
	       
	       ;; when entry is a directory or a symlink pointing to the directory call function recursivelly for it
	       (when (and dir?
			  (or (not symlink?)
			      efar-search-follow-symlinks?))
		 (efar-search-files-recursively real-file-name wildcard text regexp? ignore-case?))
	       
	       ;; when entry is a file or text for search inside files is not given
	       (when (or (not dir?)
			 (not text))
		 
		 ;; if file name matches given WILDCARD
		 (when (string-match-p (wildcard-to-regexp wildcard) real-file-name)
		   
		   ;; if text to search is given
		   (if text
		       ;; if file is readable then send file to subprocess to search the text inside
		       (if (file-readable-p real-file-name)
			   (progn
			     (let* ((proc (efar-next-search-process))
				    (request (cons :process-file (list (cons :file real-file-name) (cons :text text) (cons :regexp? regexp?) (cons :ignore-case? ignore-case?)))))
			       (when real-file-name
				 (process-send-string proc (concat
							    (prin1-to-string
							     request)
							    "\n")))))
			 ;; otherwise skip it and report a file error
			 (process-send-string  efar-search-server (concat (prin1-to-string (cons :file-error real-file-name)) "\n")))
		     
		     ;; otherwise report about found file
		     (process-send-string  efar-search-server (concat (prin1-to-string (cons :found-file (list (cons :name real-file-name) (cons :lines '())))) "\n")))))))))


(defun efar-search-process-file(args)
  "Search text in the file according to the parameters defined in ARGS."
  (let ((file (cdr (assoc :file args)))
	(text (cdr (assoc :text args)))
	(regexp? (or (cdr (assoc :regexp? args)) nil))
	(ignore-case? (or (cdr (assoc :ignore-case? args)) nil)))
    (when (and file text)
      
      (let ((hits (let ((hits '())
			(case-fold-search ignore-case?)
			(search-func (if regexp? 're-search-forward 'search-forward)))
		    
		    (with-temp-buffer
		      (insert-file-contents file)
		      
		      (goto-char 0)
		      
		      (while (funcall search-func text nil t)
			
			(push
			 (cons
			  (line-number-at-pos)
			  (replace-regexp-in-string "\n" "" (thing-at-point 'line t)))
			 hits)
			(forward-line)))
		    
		    (reverse hits))))
	
	(when hits
	  (process-send-string  efar-search-server (concat (prin1-to-string (cons :found-file (list (cons :name file) (cons :lines hits)))) "\n")))))))


(defun efar-process-search-request()
  "Main entry point for search subprocesses.
Waits for commands in standard input."
  (let ((coding-system-for-write efar-search-coding))
    (condition-case err

	(catch :exit
	  
	  (while t
	      
	      (let* ((request-string (read-from-minibuffer ""))
		     (request (car (read-from-string request-string)))
		     (command (car request))
		     (args (cdr request)))
		
		(pcase command
		  
		  (:setup-server-connection
		   (setq efar-search-server (make-network-process :name "efar-server"
								  :host "localhost"
								  :service args
								  :noquery t
								  :nowait t))
		   (set-process-query-on-exit-flag efar-search-server nil)
		   (setq efar-search-server-port args))
		  
		  
		  (:run-search-processes
		   (cl-loop repeat efar-max-search-processes  do
			    (push (efar-make-search-process) efar-search-processes)))
		  
		  (:start-search
		   (progn
		     (efar-search-int-start-search args)
		     (cl-loop for proc in efar-search-processes do
			      (while (accept-process-output proc 1)))
		     (while (accept-process-output nil 1))))
		  
		  (:finished
		   (process-send-string efar-search-server (concat (prin1-to-string (cons :finished '())) "\n")))
		  
		  (:process-file
		   (efar-search-process-file args))
		  
		  (:exit
		   (cl-loop for proc in efar-search-processes do
			    (process-send-string proc (concat (prin1-to-string (cons :exit '())) "\n")))
		   (throw :exit t))))))
      
      (error
       (process-send-string  efar-search-server (concat (prin1-to-string (cons :common-error (error-message-string err))) "\n"))))))

(defun efar-next-search-process()
  "Get next search process from the pool."
  (let* ((processes efar-search-processes)
	 (last (car (last processes))))
    (setq efar-search-processes (cons last (remove last processes)))
    (car efar-search-processes)))

(defun efar-show-search-results(&optional sorted side preserve-position?)
  "Show search results in panel SIDE.
When SORTED is t the file leist is sorted by name.
When PRESERVE-POSITION? is t keep cursor on the file when list is refreshing."
  (let ((side (or side (efar-get :current-panel)))
	(errors (cdr (assoc :errors efar-last-search-params)))
	(result-string nil)
	(status-string nil))
    
    (if (stringp errors)
	(progn
	  (setf result-string "Search results [failed]")
	  (setf status-string (concat "Search failed with erorr: " errors)))
      
      (efar-set
       (if sorted
	   (let ((temp (cl-copy-list efar-search-results)))
	     (sort temp 'efar-sort-files-by-name))
	 efar-search-results)
       :panels side :files)

      (setf result-string (concat "Search results"
				  (when efar-last-search-params
				    (if (cdr (assoc :end-time efar-last-search-params))
					(concat " - " (int-to-string (length (efar-get :panels side :files))) " [finished]"
						(when (cdr (assoc :errors efar-last-search-params))
						  (concat " (" (int-to-string (length (cdr (assoc :errors efar-last-search-params)))) " skipped)")))
				      " [in progress]" ))))
      
      (setf status-string (concat (cond (efar-search-running?
					 "Search running for files ")
					((cdr (assoc :end-time efar-last-search-params))
					 (concat "Search finished in " (int-to-string (round (- (cdr (assoc :end-time efar-last-search-params)) (cdr (assoc :start-time efar-last-search-params))))) " second(s) for files "))
					(t
					 "No search has been executed yet"))

				  (when (cdr (assoc :start-time efar-last-search-params))
				    (setq status-string (concat status-string
								"in " (cdr (assoc :dir efar-last-search-params))
								" matching mask '" (cdr (assoc :wildcard efar-last-search-params)) "'"
								(when (cdr (assoc :text efar-last-search-params))
								  (concat " and containing text '" (cdr (assoc :text efar-last-search-params)) "'"
									  " (" (if (cdr (assoc :ignore-case? efar-last-search-params)) "CI" "CS") ")"))))))))
    
    (efar-set result-string :panels side :dir)
    
    (efar-remove-notifier side)
    (unless preserve-position?
      (efar-set 0 :panels side :current-pos))
    (efar-set :search :panels side :mode)
    
    (efar-calculate-widths)
    (efar-write-enable (efar-redraw))
    
    (efar-set-status :ready status-string nil t)))


(defun efar-show-search-results-in-buffer()
  "Show detailed search results in other buffer."
  (if efar-search-running?
      
      (efar-set-status :ready "Search is still running" nil t)
    
    (efar-set-status :busy "Generating report with search results...")
    
    (and (get-buffer efar-search-results-buffer-name) (kill-buffer efar-search-results-buffer-name))
    
    (let ((buffer (get-buffer-create efar-search-results-buffer-name))
	  (dir (cdr (assoc :dir efar-last-search-params)))
	  (wildcard (cdr (assoc :wildcard efar-last-search-params)))
	  (text (cdr (assoc :text efar-last-search-params)))
	  (ignore-case? (cdr (assoc :ignore-case? efar-last-search-params)))
	  (regexp? (cdr (assoc :regexp? efar-last-search-params)))
	  (errors (cdr (assoc :errors efar-last-search-params))))
      
      (with-current-buffer buffer
	(read-only-mode 0)
	(erase-buffer)
	
	;; insert header with description of search parameters
	(insert (concat "Found " (int-to-string (length (efar-get :panels (efar-get :current-panel) :files)))
			" files in " (int-to-string (round (- (cdr (assoc :end-time efar-last-search-params)) (cdr (assoc :start-time efar-last-search-params))))) " second(s).\n"))
	
	(when errors
	  (let ((p (point)))
	    (insert (concat (int-to-string (length errors)) " file(s) inaccessible. See list at the bottom."))
	    (add-text-properties p (point)
				 '(face efar-non-existing-current-file-face))
	    (insert "\n")))
;;	    (put-text-property p (point) 'font-lock-face '(:background "red"))))

	(insert (concat "Directory: " dir "\n"
			"File name mask: " wildcard "\n"
			(when text
			  (concat "Text '" text "' found in "
				  (let ((hits 0))
				    (cl-loop for file in (efar-get :panels (efar-get :current-panel) :files) do
					     (setq hits (+ hits (length (nth 13 file)))))
				    (int-to-string hits)) " line(s)\n"
				    "Ignore case: " (if ignore-case? "yes" "no") "\n"
				    "Use regexp: " (if regexp? "yes" "no") "\n"))
			"\n"))
	
	(cl-loop for file in (efar-get :panels (efar-get :current-panel) :files) do
		 
		 ;; create a link to the file
		 (insert-button (car file)
				:type 'efar-search-find-file-button
				:file (car file))
		 (insert "\n")
		 
		 ;; create links to source code lines
		 (cl-loop for line in (nth 13 file) do
			  
			  (let ((line-number (car line))
				(line-text (cdr line)))
			    (insert-button (concat (int-to-string line-number) ":\t" line-text)
					   :type 'efar-search-find-line-button
					   :file (car file)
					   :line-number line-number
					   :text text
					   :ignore-case? ignore-case?
					   :regexp? regexp?))
			  (insert "\n"))
		 (insert "\n"))
	
	
	;; output skipped files
	(when errors
	  (insert "\nFollowing files/directries are inaccessible and therefore were skipped:\n")
	  (cl-loop for file in errors do
		   (insert file)
		   (insert "\n")))
	
	;; highlight searched text
	(goto-char (point-min))
	(when text
	  (if (cdr (assoc :regexp? efar-last-search-params))
	      (highlight-regexp text 'hi-yellow)
	    (highlight-phrase text 'hi-yellow)))
	  
	(read-only-mode 1))
      
      (switch-to-buffer-other-window buffer)
      (efar-set-status :ready "Ready"))))

(defun efar-search-find-file-button(button)
  "Open file from search results buffer and navigate to searched text.
BUTTON is a button clicked."
  (let ((file (button-get button :file))
	(line-number (button-get button :line-number))
	(text (button-get button :text))
	(ignore-case? (button-get button :ignore-case?))
	(regexp? (button-get button :regexp?)))
    
    ;; open file in other window
    (find-file-other-window file)
    (goto-char 0)
    
    ;; navigate to the line containing searched text
    ;; and activate isearch for the searched text
    (when line-number
      (forward-line (- line-number 1))
      (isearch-mode t regexp?)
      (if ignore-case?
	  (setq text (downcase text)))
      (isearch-process-search-string text
				     (mapconcat 'isearch-text-char-description text "")))))

;;; efar.el ends here
