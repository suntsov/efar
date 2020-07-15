(setq debug-on-error t)

(when (eq window-system 'w32)
  (set-default 'tramp-auto-save-directory "C:\test")
  (set-default 'tramp-default-method "plink"))

(require 'cl)
(require 'ido)
(require 'tramp)
(require 'subr-x)
(require 'filenotify)

(defvar efar-state nil)


;;--------------------------------------------------------------------------------
;; eFar customization variables
;;--------------------------------------------------------------------------------
;; GROUPS
(defgroup eFar nil
  "FAR-like file manager"
  :group 'applications)

(defgroup eFar-parameters nil
  "eFar main customization parameters"
  :group 'eFar)

(defgroup eFar-faces nil
  "eFar faces"
  :group 'eFar)

(defgroup eFar-keys nil
  "eFar key bindings"
  :group 'eFar)

;; MAIN PARAMETERS
(defcustom efar-buffer-name "*eFAR*"
  "Name for the Efar buffer"
  :group 'eFar-parameters)

(defcustom efar-state-file-name (concat user-emacs-directory ".efar-state")
  "Path to the eFar state save file"
  :group 'eFar-parameters)

(defcustom efar-default-startup-dirs (cons user-emacs-directory user-emacs-directory)
  "Default direcotries shown at startup"
  :group 'eFar-parameters)

(defcustom efar-save-state? t
  "Save eFar state when eFar buffer is killed to restore it at next open"
  :group 'eFar-parameters
  :type 'boolean)

(defcustom efar-filter-directories? nil
  "Apply filter to the directories and files (t) or to files only (nil)"
  :group 'eFar-parameters
  :type 'boolean)

;; FACES
(defface efar-border-line-face
  '((t :foreground "white"
       :background "navy"
       :underline nil
       ))
  "Border line face"
  :group 'eFar-faces)


(defface efar-file-face
  '((t :foreground "deep sky blue"
       :background "navy"
       :underline nil
       ))
  "File item style (default)"
  :group 'eFar-faces)

(defface efar-file-executable-face
  '((t :foreground "green"
       :background "navy"
       :underline nil
       ))
  "File item style (executable file)"
  :group 'eFar-faces)

(defface efar-dir-face
  '((t :foreground "white"
       :background "navy"
       :underline nil
       ))
  "Directory item style"
  :group 'eFar-faces)

(defface efar-file-current-face
  '((t :foreground "black"
       :background "cadet blue"
       :underline nil
       ))
  "Current file item style"
  :group 'eFar-faces)


(defface efar-dir-current-face
  '((t :foreground "white"
       :background "cadet blue"
       :underline nil
       ))
  "Current directory item style"
  :group 'eFar-faces)

(defface efar-marked-face
  '((t :foreground "gold"
       :background "navy"
       :underline nil
       ))
  "Marked item style"
  :group 'eFar-faces)


(defface efar-dir-name-face
  '((t :foreground "white"
       :background "navy"
       :underline nil
       ))
  "Directory name header style"
  :group 'eFar-faces)

(defface efar-header-face
  '((t :foreground "orange"
       :background "navy"
       :underline nil
       ))
  "Header style"
  :group 'eFar-faces)

(defface efar-dir-name-current-face
  '((t :foreground "navy"
       :background "bisque"
       :underline nil
       ))
  "Current directory name header style"
  :group 'eFar-faces)


;; KEYS

(defvar efar-keys '()
  "The list of eFar keybindings")

(setq efar-keys '())

(defun efar-register-key(key func arg custom-key-name description)
  ""
  (push (list key func arg custom-key-name description) efar-keys))

;;			key-sequence	function to call		arg	variable name to save cusom	Key description
(efar-register-key 	"C-o" 		'efar-display-console		nil	'efar-display-console-key 	"Open console window")
(efar-register-key 	"<insert>"	'efar-mark-file			nil	'efar-mark-file-key		"Mark current file/directory")
(efar-register-key	"<C-insert>"	'efar-deselect-all		nil	'efar-deselect-all-kay		"Unmark all files")
(efar-register-key	"<f12> <f12>" 	'efar-init			nil	'efar-init-key			"Reinit and redraw eFar buffer")
(efar-register-key	"<down>" 	'efar-move-cursor		:down 	'efar-move-down-key		"Move cursor down")
(efar-register-key	"<up>" 		'efar-move-cursor		:up	'efar-move-up-key		"Move cursor up")
(efar-register-key	"<right>" 	'efar-move-cursor		:right 	'efar-move-right		"Move cursor to the right")
(efar-register-key	"<left>" 	'efar-move-cursor		:left 	'efar-move-left-key		"Move cursor to the left")
(efar-register-key	"<home>" 	'efar-move-cursor		:home	'efar-move-home-key		"Move cursor to the first file")
(efar-register-key	"C-<left>" 	'efar-move-cursor		:home 	'efar-move-home-alt-key		"Move sursor to the first file (alternative)")
(efar-register-key	"<end>" 	'efar-move-cursor		:end	'efar-move-end-key		"Move cursor to the last file")
(efar-register-key	"C-<right>" 	'efar-move-cursor		:end	'efar-move-end-alt-key		"Move cursor to the last file (alternative)")
(efar-register-key	"RET" 		'efar-enter-directory		nil 	'efar-enter-directory-key	"Enter directory under cursor")
(efar-register-key	"TAB" 		'efar-switch-to-other-panel	nil	'efar-switch-to-other-panel-key	"Go to other panel")
(efar-register-key	"C-c TAB" 	'efar-open-dir-other-panel	nil	'efar-open-dir-othet-panel-key	"Open current directory in other panel")
(efar-register-key	"<f4>" 		'efar-edit-file			nil	'efar-open-file-key		"Edit file under cursor")
(efar-register-key	"<M-f4>" 	'efar-open-file-in-ext-app	nil	'efar-open-file-in-ext-app-key	"Open file in externall application")
(efar-register-key	"<f5>" 		'efar-copy-or-move-files	:copy	'efar-copy-file-key		"Copy file(s)")
(efar-register-key	"<f6>"		'efar-copy-or-move-files	:move	'efar-move-file-key		"Move/rename file(s)")
(efar-register-key	"<M-f1>"	'efar-change-disk		:left	'efar-change-disk-left-key	"Change current disk/mount point (left panel)")
(efar-register-key	"<M-f2>"	'efar-change-disk		:right	'efar-change-disk-right-key	"Change current disk/mount point (right panel)")
(efar-register-key	"<C-f1>"	'efar-change-sort-function 	:left	'efar-change-sort-left-key	"Change sort algorythm and/or order (left panel)")
(efar-register-key	"<C-f2>"	'efar-change-sort-function 	:right	'efar-change-sort-right-key	"Change sort algorythm and/or order (right panel)")
(efar-register-key	"<f7>"		'efar-create-new-directory	nil	'efar-create-direcotry-key	"Create new directory")
(efar-register-key	"<f8>"		'efar-delete-selected		nil	'efar-delete-file-key		"Delete selected file(s)")
(efar-register-key	"<S-f1>"	'efar-filter-files		:left	'efar-filter-files-left-key	"Set/remove filtering for current directory (left panel)")
(efar-register-key	"<S-f2>"	'efar-filter-files		:right	'efar-filter-files-right-key	"Set/remove filtering for current directory (right panel)")
(efar-register-key	"<f11>"		'efar-change-mode		nil	'efar-change-mode-key		"Toggle mode: double panel <-> single panel")
(efar-register-key	"C-c +"		'efar-change-column-number	t	'efar-inc-column-number-key	"Increase number of columns in current panel")
(efar-register-key	"C-c -"		'efar-change-column-number	nil	'efar-dec-column-number-key	"Decrease number of columns in current panel")
(efar-register-key	"<f10>"		'efar-copy-current-path		nil	'efar-copy-current-path-key	"Copy to the clipboard the path to the current file")
(efar-register-key	"C-c C-d"	'efar-cd			nil	'efar-cd-key			"Go to directory")

(efar-register-key	"C-g"	'efar-abort		nil	nil				"Abort operation")

;; fast-search keys
(efar-register-key	"<backspace>"	'efar-fast-search		:back	nil				"Backspace for fast search")
(efar-register-key	"C-s"		'efar-fast-search		:next	nil				"Go to next fast search match")
(efar-register-key	"C-r"		'efar-fast-search		:prev	nil				"Go to previous fast search match")

(loop for char in (list ?a ?b ?c ?d ?e ?f ?g ?h ?i ?j ?k ?l ?m ?n ?o ?p ?q ?r ?s ?t ?u ?v ?w ?x ?y ?z
			?A ?B ?C ?D ?E ?F ?G ?H ?I ?J ?K ?L ?M ?N ?O ?P ?Q ?R ?S ?T ?U ?V ?W ?X ?Y ?Z
			?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9 ?0
			?( ?) ?.	
			?- ?_
			32) do
			
			(efar-register-key	(char-to-string char)	'efar-fast-search	char	nil	""))

;;(efar-register-key	"<M-f7>"	'efar-start-search		nil	'efar-start-search-key		t	"Start file search")

;; create customization entries for key bindings
(loop for key in efar-keys do
      (when (nth 3 key)
	(custom-declare-variable
	 (intern (symbol-name (nth 3 key)))
	 (kbd (nth 0 key))
	 (nth 4 key)
	 :type 'key-sequence
	 :group 'eFar-keys)))


;;--------------------------------------------------------------------------------
;; eFar main functions
;;--------------------------------------------------------------------------------

(defun efar(arg)
  "Main funtion to run eFar commander.
If the function called with prefix argument, then go to default-directory of current buffer."
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
      (delete-other-windows)

      ;; do initialisation if necessary and redraw the content of the buffer
      (when need-init?
	(efar-init)
	(efar-calculate-window-size)
	(efar-calculate-widths)
	(efar-write-enable
	 (efar-redraw)))

      ;; go to default-directory of current buffer if function is called with prefix argument
      (when go-to-dir
	(efar-go-to-dir go-to-dir :left)
	(efar-write-enable (efar-redraw))))
    
    (switch-to-buffer efar-buffer)))


(defun efar-init()
  "Set up main eFAR configuration. Initialize state storage. This function is executed only once when eFAR buffer is created."
  
  ;; forbid resizing of eFAR window
  (setq window-size-fixed t)
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
  (when (null efar-state)
    (efar-init-state))
  
  (efar-set-key-bindings)
  ;;  (efar-set-mouse-bindings)
  
  (efar-go-to-dir (efar-get :panels :left :dir) :left)
  (efar-go-to-dir (efar-get :panels :right :dir) :right))

(defun efar-init-state()
  ""
  (setf efar-state (make-hash-table :test `equal))  
  
  (efar-set (car efar-default-startup-dirs) :panels :left :dir)
  (efar-set (cdr efar-default-startup-dirs) :panels :right :dir)
  
  (setf default-directory (car efar-default-startup-dirs))
  
  (efar-set :left :current-panel)
  
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
  
  (efar-set '() :pending-notifications)
  
  (efar-set nil :panels :left :sort-order)
  (efar-set nil :panels :right :sort-order)
  
  (efar-set "Name" :panels :left :sort-function-name)
  (efar-set "Name" :panels :right :sort-function-name)
  
  (efar-set :both :mode)
  
  (efar-set 5 :panels :left :column-number)
  (efar-set 2 :panels :right :column-number)
  
  (efar-set nil :column-widths)
  
  (efar-set "Ready" :status-string)
  (efar-set :ready :status)
  (efar-set nil :reset-status?)
  
  (efar-set (make-hash-table :test `equal) :directory-history))

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
  "Returns a list of sort function names. 
The name of a function which is currently used for the panel SIDE (or current panel) becomes a first entry in the list."
  (sort
   (mapcar
    (lambda(e)
      (car e))
    efar-sort-functions)
   (lambda(a b) (when (string= a (efar-get :panels side :sort-function-name)) t))))

(defun efar-sort-files-by-name(a b)
  "Function to sort files by name.
Directories always go before normal files.
Function string< is used for comparing file names. 
Case is ignored."
  (cond
   ;; directories go first
   ((and (car (cdr a)) (not (car (cdr b)))) 
    t)
   ((and (not (car (cdr a))) (car (cdr b)))
    nil)
   ;; entries of same type (directory or usual files) sorted in lexicographic order
   (t (string<
       (downcase (car a))
       (downcase (car b))))))

(defun efar-sort-files-by-modification-date(a b)
  "Function to sort files by modification date.
Directories always go before normal files."
  (cond
   ;; directories go first
   ((and (car (cdr a)) (not (car (cdr b)))) 
    t)
   ((and (not (car (cdr a))) (car (cdr b)))
    nil)
   ;; entries of same type (directory or usual files) sorted by comparing modification date
   (t (<
       (time-to-seconds (nth 6 a))
       (time-to-seconds (nth 6 b))))))

(defun efar-sort-files-by-size(a b)
  "Function to sort files by file size.
Directories always go before normal files."
  (cond
   ;; directories go first
   ((and (car (cdr a)) (not (car (cdr b)))) 
    t)
   ((and (not (car (cdr a))) (car (cdr b)))
    nil)
   ;; entries of same type (directory or usual files) sorted by comparing file size 
   (t (<
       (nth 8 a)
       (nth 8 b)))))

(defun efar-sort-files-by-extension(a b)
  "Function to sort files by type (extension).
Directories always go before normal files.
Function string< is used for comparing file extensions. 
Case is ignored."
  (cond
   ;; directories go first
   ((and (car (cdr a)) (not (car (cdr b)))) 
    t)
   ((and (not (car (cdr a))) (car (cdr b)))
    nil)
   ;; files sorted by extension (if any) in lexicographic order
   (t (string<
       (downcase (or (file-name-extension (car a)) ""))
       (downcase (or (file-name-extension (car b)) ""))))))

(defun efar-change-sort-function(side)
  "Ask user for sort function and order and set them for panel SIDE."
  (efar-set (ido-completing-read "Sort files by: " (efar-sort-function-names side))
	    :panels side :sort-function-name)
  
  (efar-set (string= (char-to-string 9660) (ido-completing-read "Sort order: " (list (char-to-string 9650) (char-to-string 9660) )))
	    :panels side :sort-order)

  (efar-refresh-dir side nil (efar-get-short-file-name (efar-current-file))))

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
  "Callback function triggered when a file event occurs.
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
	  (when (not (member descriptor pending-notifications))
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
	    (efar-refresh-dir :left))
	  ;; if event comes from the watch is registered for right panel directory, refersh right panel
	  (when (equal descriptor (cdr (efar-get :panels :right :file-notifier)))
	    (efar-refresh-dir :right))))))

;;--------------------------------------------------------------------------------
;; end of file notification functions
;;--------------------------------------------------------------------------------

(defun efar-remove-file-state()
  "Remove eFar state file. Could be helpfull in case of errors during startup."
  (interactive)
  (delete-file efar-state-file-name))

(defun efar-save-state()
  "Save eFar state to the state file. Data from this file is used during startup to restore last state."
  (with-temp-file efar-state-file-name
    (let ((copy (copy-hash-table efar-state)))

      ;; clear up data not not relevant for saving
      (puthash :file-notifier nil (gethash :left (gethash :panels copy)))
      (puthash :file-notifier nil (gethash :right (gethash :panels copy)))
      (puthash :pending-notifications () copy)
      (puthash :files () (gethash :left (gethash :panels copy)))
      (puthash :files () (gethash :right (gethash :panels copy)))

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
     (efar-handle-error      
      (make-directory new-dir-name nil))
     
     ;; refresh panel and move selection
     (efar-refresh-dir side nil new-dir-name)
     
     ;; if other panel displays same directory as current one, refresh it as well
     (when (string= (efar-get :panels side :dir) (efar-get :panels (efar-other-side) :dir))
       (efar-refresh-dir (efar-other-side))))))



(defun efar-copy-or-move-files(operation)
  ""
  (unwind-protect      
      (progn 
	(efar-set-status :busy (if (equal operation :copy)
				   "Copying files..."
				 "Moving files..."))

	(efar-with-notification-disabled
	 (let* ((side (efar-get :current-panel))
		(todir  (efar-get :panels (efar-other-side) :dir) )	 
		(selected (efar-get :panels side :selected))		    
		(start-file-number (efar-get :panels side :start-file-number))
		(file-number (+ start-file-number (efar-get :panels side :current-pos)))
		(files (efar-selected-files side nil)))

	   (when files
	     (efar-copy-or-move-files-int operation files (read-directory-name (if (equal operation :copy) "Copy selected files to " "Move selected files to ") todir todir)))
	   
	   (efar-refresh-dir :left)
	   (efar-refresh-dir :right)))

	(efar-set-status :ready "Ready"))))

(defun efar-copy-or-move-files-int(operation files todir &optional fromdir overwrite?)
  ""
  
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
			       ((and (not (car (cdr f))) (not (file-exists-p newfile)))
				;; we just copy it using elisp function
				
				(if (equal operation :copy)
				    (efar-handle-error (copy-file (car f) newfile))
				  (efar-handle-error (rename-file (car f) newfile nil))))
			       
			       
			       ;; if file is a directory and doesn't exist in destination folder
			       ((and (car (cdr f)) (not (file-exists-p newfile)))
				;; we just copy directory using elisp function
				(if (equal operation :copy)
				    (efar-handle-error (copy-directory (car f) newfile nil nil nil))
				  (efar-handle-error (rename-file (car f) newfile))))
			       
			       ;; if file is a real file and does exist in destination folder
			       ((and (not (car (cdr f))) (file-exists-p newfile))
				(progn
				  ;; we ask user what to do (overwrite, not overwrite, overwrite all remaining)
				  ;; if user was already asked before and the answer was "All", we don't ask again
				  (setf overwrite? (cond
						    ((or (null overwrite?) (string= overwrite? "No") (string= overwrite? "Yes"))
						     (ido-completing-read (concat "File " newfile " already exists. Overwrite? ") (list "Yes" "No" "All")))
						    (t overwrite?)))
				  ;; we copy file (overwrite) using elisp function if user approved it
				  (when (not (string= overwrite? "No")) 
				    (if (equal operation :copy)
					(efar-handle-error (copy-file (car f) newfile t nil nil nil))
				      (efar-handle-error (rename-file (car f) newfile t))))))
			       
			       ;; if file is a directory and does exist in destination folder
			       ((and (car (cdr f)) (file-exists-p newfile))
				;; we call local function recursively 
				(do-operation operation (directory-files (car f) nil nil t) newfile (expand-file-name (efar-get-short-file-name f) default-directory) )
				(when (equal operation :move)
				  (delete-directory (car f))))))))
			
			files))))
    ;; call local function first time 
    (do-operation operation files todir fromdir)) )


(defun efar-delete-selected()
  ""
  (efar-set-status :busy "Deleting files...")
  
  (let ((side (efar-get :current-panel)))
    
    (unwind-protect
	(efar-with-notification-disabled
	 (let* ((side (efar-get :current-panel))
		(selected-files (efar-selected-files side nil)))
	   
	   (when (and selected-files (string= "Yes" (ido-completing-read "Delete selected files? " (list "Yes" "No"))))
	     (mapc(lambda (f)
		    (if (car (cdr f))
			(efar-handle-error (delete-directory (car f) t))
		      (efar-handle-error (delete-file (car f)))))
		  
		  selected-files)
	     
	     (efar-refresh-dir side)
	     
	     (when (string= (efar-get :panels side :dir) (efar-get :panels (efar-other-side) :dir))
	       (efar-refresh-dir (efar-other-side)))))))
    
    (efar-set-status :ready "Ready")))


(defmacro efar-with-notification-disabled(&rest body)
  ""
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
  `(with-current-buffer ,efar-buffer-name
     (read-only-mode -1)
     ,@body
     (read-only-mode 1)))

(defmacro efar-handle-error (&rest body)
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


(defun efar-get(&rest keys)
  ""
  (let ((value nil))
    (mapc
     (lambda(key)
       (if value
	   (setf value (gethash key value))
	 (setf value (gethash key efar-state))))
     keys)
    value))

(defun efar-set(value &rest keys)
  ""
  (let ((place nil))
    (mapc

     (lambda(key)
       (if place
	   (setf place (puthash key (gethash key place (make-hash-table :test `equal)) place))
	 (setf place (puthash key (gethash key efar-state (make-hash-table :test `equal)) efar-state))))

     (subseq keys 0 -1))
    
    (puthash (car (subseq keys -1)) value (or place efar-state))))

(defun efar-reset-status()
  ""
  (when (efar-get :reset-status?)
    (efar-set nil :reset-status?)
    (efar-set-status :ready "Ready")))

(defun efar-set-status(status &optional status-string seconds reset?)
  ""
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
  ""
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
  "Set up local key bindings"
  (loop for key in efar-keys do
	(local-set-key
	 (kbd (nth 0 key))
	 `(lambda()
	    (interactive)
	    (if (nth 2 ',key)
		(funcall (nth 1 ',key) (nth 2 ',key))
	      (funcall (nth 1 ',key)))))))

(defun efar-set-mouse-bindings()
  ""
  ;; ToDo: implement mouse interaction
  ;;  (local-set-key (kbd "<down-mouse-1>")
  ;;		 (lambda (event)
  ;;		   (interactive "e")
  ;;		   (if (<  (car (nth 6 (nth 1 event))) (+ 2 (efar-get :panel-width) ))
  ;;		       (progn
  ;;			 (efar-set :left :current-panel)
  ;;			 (efar-set default-directory :panels :left :dir))
  ;;		     
  ;;		     (progn
  ;;		       (efar-set :right :current-panel)
  ;;		       (efar-set default-directory :panels :right :dir))
  ;;		     )
  ;;		   (efar-write-enable (efar-redraw))))
  )

(defun efar-cd()
  "Open directory selector (read-diretory-name) and go to selected directory."
  (efar-go-to-dir (read-directory-name "Go to directory: " default-directory))
  (efar-write-enable (efar-redraw)))

(defun efar-change-column-number(&optional increase)
  ""
  (let ((side (efar-get :current-panel)))
    
    (if increase
	(efar-set (+ (efar-get :panels side :column-number) 1) :panels side :column-number)
      (when (> (efar-get :panels side :column-number) 1)
	(efar-set (- (efar-get :panels side :column-number) 1) :panels side :column-number)
	
	(let ((file (car (efar-current-file side))))
	  (when (not (string= file "..")) (efar-go-to-file file))))))
  
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
  (let* ((side (efar-get :current-panel))
	 (fnum (+ (efar-get :panels side :start-file-number) (efar-get :panels side :current-pos) ))
	 (file (nth fnum (efar-get :panels side :files)))
	 (fname (expand-file-name (car file) (efar-get :panels side :dir))))
    (kill-new fname)))

(defun efar-open-file-in-ext-app()
  ""
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








(defun efar-start-search()
  ""
  
  (efar-search-files))




(defun efar-filter-files(side)
  ""
  (let ((side (or side (efar-get :current-panel))))
    
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
  ""
  (if (not (get-buffer "*efar-shell*"))
      (shell "*efar-shell*")
    (let ((side (efar-get :current-panel)))
      (with-current-buffer (get-buffer "*efar-shell*")
	(insert (concat "cd " (efar-get :panels side :dir)))
	(comint-send-input nil t))
      (display-buffer "*efar-shell*"))))


(defun efar-fast-search (k)
  ""    
  (let ((str (efar-get :fast-search-string)))
    
    (when (not (member k '(:next :prev :back :clear))) 
      (setf str (concat str (format "%c" k))))
    
    (when (> (length str) 0)
      
      (when (equal k :back)
	(setf str (substring str 0 (- (length str) 1))))
      
      (let* ((side (efar-get :current-panel))
	     (filtered-list (mapcan (lambda (e)
				      (when (string-match str (efar-get-short-file-name e))
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
  ""			   
  (when (not (null efar-state))
    (efar-set "" :fast-search-string)
    (efar-output-status)
    (efar-set 0 :fast-search-occur)))
  
(defun efar-get-accessible-directory-in-path (path)
  "Return first accessible directory in the PATH going from bottom to up. If there are no accessible directories in the given path, return user-emacs-directory."
  
  ;; if directory PATH exists or is accessible
  (if (and (file-exists-p path)
	   (file-accessible-directory-p path))
      ;; return this directory
      path
    
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

(defun efar-refresh-dir(&optional side move-to-first? move-to-file-name)
  ""
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
  ""
  
  (let* ((dir (when dir (efar-get-accessible-directory-in-path (expand-file-name dir))))
	 (side (or side (efar-get :current-panel)))
	 (current-dir (efar-get :panels side :dir))
	 (parent-dir (efar-get-parent-dir current-dir))
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

    (efar-set dir :directory-history (efar-get-root-directory dir))
    
    (efar-setup-notifier dir side)))


(defun efar-get-parent-dir(dir)
  ""
  (file-name-directory (directory-file-name dir)))

(defun efar-go-to-file(file &optional side prev-file-number)
  ""
  (let* ((side (or side (efar-get :current-panel)))
	 (file (directory-file-name (expand-file-name file (efar-get :panels side :dir))))
	 (number-of-files (length (efar-get :panels side :files)))
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
       (< new-file-number (+ (efar-get :panels side :start-file-number) (* 2 (efar-get :panel-height)))))
      
      (progn
	(efar-set (- new-file-number (efar-get :panels side :start-file-number)) :panels side :current-pos)))
     
     ((< new-file-number (* 2 (efar-get :panel-height)))
      (progn
	(efar-set 0 :panels side :start-file-number)
	(efar-set new-file-number :panels side :current-pos)))
     
     (t 
      (progn
	(efar-set new-file-number :panels side :start-file-number)
	(efar-set 0 :panels side :current-pos))))))


(defun efar-current-file-number(&optional side)
  ""
  (let* ((side (or side (efar-get :current-panel)))
	 (start-file-number (efar-get :panels side :start-file-number)))
    
    (+ start-file-number (efar-get :panels side :current-pos))))

(defun efar-current-file-name(&optional side)
  ""
  (car (efar-current-file side)))

(defun efar-current-file(&optional side)
  ""
  (let ((side (or side (efar-get :current-panel))))
    (nth (efar-current-file-number side) (efar-get :panels side :files))))

;;(loop for i from 1 to 100 do (make-directory (concat "c:/test/" (int-to-string i))))
(defun efar-other-side(&optional side)
  ""
  (let ((side (if side side (efar-get :current-panel))))
    (if (equal side :left) :right :left)))



(defun efar-change-disk(side)
  "Show menu with available disks (Windows) or mount points (Unix) (*ToDo*).
Selected item bacomes actual for panel SIDE."
  (let ((dir (concat
	      (ido-completing-read "Change disk to: "
				   (nconc (when (eq window-system 'w32)
					    (mapcar (lambda(e) (car (split-string e " " t)))
						    (cdr (split-string  (downcase (shell-command-to-string "wmic LogicalDisk get Caption"))
									"\r\n" t))))
					  (list "Manual")))
	      "/")))
    
    (when dir 
      
      (setf dir (if (string= dir "Manual/")
		    (read-string "Input connection string: ")
		  dir))

      (efar-go-to-dir (or (efar-get :directory-history  dir) dir) side)
      
      (efar-write-enable (efar-redraw)))))


(defun efar-files-as-string(file-numbers)
  "Not used"
  (let ((side (efar-get :current-panel)))
    (mapconcat
     (lambda(x)
       (car (nth x (efar-get :panels side :files))))
     file-numbers
     ", ")))



(defun efar-deselect-all()
  ""
  (efar-write-enable
   (let ((side (efar-get :current-panel)))
     (efar-set '() :panels side :selected)
     (efar-redraw))))

(defun efar-mark-file()
  ""
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

(defun efar-edit-file()
  ""
  (let* ((side (efar-get :current-panel))
	 (file (car (car (efar-selected-files side t)))))
    (when file
      (find-file file))))

(defun efar-set-files-order(files side)
  ""
  (if (efar-get :panels side :sort-order)
      (reverse files)
    files))

(defun  efar-is-root-directory(dir)
  "Returns t if DIR is a root directory, nil otherwise."
  (string= (file-name-as-directory dir)
	   (file-name-directory (directory-file-name dir))))

(defun efar-get-file-list(side)
  ""
  (let ((filter (efar-get :panels side :file-filter))
	(root? (efar-is-root-directory (efar-get :panels side :dir))))
    
    (efar-set
     ;; if we are not in the root directory, we add entry to go up
     (append (when (not root?) (list (list ".." t)))
	     ;; change order of files according to selected mode (ASC or DESC)
	     (efar-set-files-order
	      
	      ;; build file list
	      (let ((files
		     ;; remove entries "." and ".." and filter file list according to selected wildcard
		     (remove-if
		      (lambda (f)  (or		       
				    (string-suffix-p "/." (car f))
				    (string-suffix-p "/.." (car f))					    
				    (and (or (not (car (cdr f)))
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
  ""
  (let ((side (efar-get :current-panel)))

    (unless (= 0 (length (efar-get :panels side :files)))

      (efar-reset-status)
      (efar-quit-fast-search)
      
      (efar-write-enable
       (let* ((curr-pos (efar-get :panels side :current-pos))
	      (max-files-in-column (- (efar-get :panel-height) 1))
	      (max-file-number (length (efar-get :panels side :files)))
	      (start-file-number (efar-get :panels side :start-file-number))
	      (col-number (efar-get :panels side :column-number))
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
		 (decf curr-pos)
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
		 (incf curr-pos)
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
	    
	    ;; else if there is more than max-files-in-column left
	    ((> (- max-file-number start-file-number curr-pos) max-files-in-column)
	     (if (< curr-pos (* (- col-number 1) max-files-in-column))
		 (efar-set (+ curr-pos max-files-in-column)
			   :panels side :current-pos)
	       (efar-set (+ start-file-number max-files-in-column)
			 :panels side :start-file-number))))))
	 
	 (efar-output-files side affected-item-numbers)
	 
	 (efar-output-file-details side))))))


(defun efar-enter-directory()
  "Enter directory under cursor"

  (let* ((side (efar-get :current-panel))				       
	 (start-file-number (efar-get :panels side :start-file-number))
	 (file-number (+ start-file-number (efar-get :panels side :current-pos)))
	 (file (nth file-number (efar-get :panels side :files)))
	 (current-dir-path (efar-get :panels side :dir)))
    
    (efar-quit-fast-search)
    (when (car (cdr file))
      (let ((newdir (file-name-as-directory (expand-file-name (car file) current-dir-path))))
	(cond
	 
	 ((not (file-accessible-directory-p  newdir))
	  (efar-set-status :ready (concat "Directory "  newdir " is not accessible") 3))
	 
	 (t				
	  (progn
	    (efar-go-to-dir newdir side)
	    (efar-write-enable (efar-redraw)))))))))

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
    (efar-write-enable (efar-redraw))))

(defun efar-calculate-window-size()
  ""
  (let ((mode (efar-get :mode)))
    
    (efar-set (window-width) :window-width)
    (efar-set (window-height) :window-height)
    (efar-set (- (window-height) 7) :panel-height)))

(defun efar-redraw()
  ""
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
  (efar-output-header :left)
  (efar-output-header :right)
  (efar-output-status))



(defun efar-get-short-file-name(file)
  ""
  (if (nth 1 file)
      (file-name-nondirectory (directory-file-name (nth 0 file)))
    (file-name-nondirectory (nth 0 file))))


(defun efar-output-file-details(side)
  ""
  
  (let ((mode (efar-get :mode)))
    
    (when (and (not (null (efar-get :panels side :files)))
	       (or (equal mode :both) (equal mode side)))
      
      (let ((current-file-number (efar-current-file-number side)))

	(when current-file-number
	  
	  (let* ((file (nth current-file-number (efar-get :panels side :files)))
		 (file-short-name (efar-get-short-file-name file))
		 (col-number (cond
			      ((or (equal side :left) (equal mode :right)) 1)
			      (t (+ (efar-panel-width (efar-other-side side)) 2))))
		 
		 (w (efar-panel-width side))
		 
		 (status-str (efar-prepare-file-name (concat  (if (nth 1 file) "Directory: " "File: ")
							      file-short-name
							      "  Modified: "
							      (format-time-string "%D %T" (nth 6 file))
							      (if (and (not (nth 1 file)) (numberp (nth 8 file)))
								  (concat "  Size: " (int-to-string (nth 8 file))))) w)))
	    
	    (goto-char 0)
	    
	    (forward-line (+ 2 (efar-get :panel-height)))
	    
	    (move-to-column col-number)
	    
	    (let ((p (point)))
	      (replace-rectangle p (+ p w) status-str)
	      
	      (put-text-property p (+ p w) 'face 'efar-border-line-face))))))))

(defun efar-panel-width(side)
  ""
  (let ((widths (if (equal side :left)
		    (car (efar-get :column-widths))
		  (cdr (efar-get :column-widths)))))
    
    (+ (apply '+ widths)
       (- (length widths) 1))))


(defun efar-output-files(side &optional affected-item-numbers)
  ""
  (unless (= 0 (length (efar-get :panels side :files)))
    (let ((mode (efar-get :mode))
	  (widths (if (equal side :left)
		      (car (efar-get :column-widths))
		    (cdr (efar-get :column-widths)))))
      
      (when (or (equal mode :both) (equal mode side))
	
	(goto-char 0)
	(forward-line)
	
	(let* ((start-pos (cond
			   ((equal side :left) 1)
			   ((and (equal side :right) (equal mode :right)) 1)
			   (t (+ (efar-panel-width :left) 2))))
	       (max-files-in-column (- (efar-get :panel-height) 1))
	       (cnt 0)
	       (col-number (length widths))
	       
	       (files	 
		(append
		 (subseq  (efar-get :panels side :files)
			  (efar-get :panels side :start-file-number)  
			  (+ (efar-get :panels side :start-file-number) 
			     (if (> (- (length (efar-get :panels side :files)) (efar-get :panels side :start-file-number)) (* max-files-in-column col-number)) (* max-files-in-column col-number)
			       (- (length (efar-get :panels side :files)) (efar-get :panels side :start-file-number)))))
		 
		 ;; append empty items if number of files to display is less then max files in panel
		 ;; needed to overwrite old entries
		 (make-list (let ((rest (- (length (efar-get :panels side :files)) (efar-get :panels side :start-file-number))))
			      (if (> rest (* max-files-in-column col-number))
				  0 (- (* max-files-in-column col-number) rest))) 
			    (list "")))))
	  
	  (loop for col from 0 upto (- col-number 1) do
		
		(let ((files-in-column (subseq files (* col max-files-in-column) (* (+ col 1) max-files-in-column))))
		  
		  (loop repeat (length files-in-column)  do
			
			(forward-line)
			
			(when (or (null affected-item-numbers) (member cnt affected-item-numbers)) 
			  (let ((shift (+ start-pos
					  (apply '+ (subseq widths 0 col))
					  col)))
			    
			    (move-to-column shift)
			    
			    (let* ((f (nth cnt files))
				   (p (point))
				   (w (nth col widths))
				   
				   (marked? (member (+ (efar-get :panels side :start-file-number) cnt) (efar-get :panels side :selected)))
				   
				   (str (efar-prepare-file-name (concat (and marked? "*") (if (string= (efar-get :panels side :dir) "Search") (car f) (file-name-nondirectory (car f))) ) w)))
			      
			      
			      (replace-rectangle p (+ p (length str)) str)
			      
			      
			      (let ((dir? (car (cdr f)))
				    
				    (current? (and
					       (= cnt (efar-get :panels side :current-pos))
					       (equal side (efar-get :current-panel)))))
				(let ((current-face
				       (cond
					((and dir? current?) 'efar-dir-current-face)
					((and (not dir?) current?) 'efar-file-current-face)
					(marked? 'efar-marked-face)
					((and dir? (not current?)) 'efar-dir-face)
					((and (not dir?) (not current?)) 'efar-file-face) )))
				  (put-text-property p (+ p w) 'face current-face))))))
			
			
			(incf cnt)))
		
		(goto-char 0)
		(forward-line)))))))



(defun efar-output-header(side)
  ""
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
		   (when (not (string-empty-p filter))
		     (concat " " filter )))))
	
	(move-to-column col-number)
	
	(let ((p (point)))
	  (replace-rectangle p (+ p (length str)) str)
	  (put-text-property p (+ p (length str)) 'face 'efar-header-face))))))


(defun efar-prepare-file-name(fname len)
  ""
  (cond ((> (length fname) len) (concat (subseq fname 0 (- len 1)) ">"))
	((< (length fname) len) (concat fname (make-string (- len (length fname)) ?\s)))
	((= (length fname) len) fname)))

(defun efar-output-dir-names(side)
  ""
  (let ((mode (efar-get :mode)))
    
    (when (or (equal mode :both) (equal mode side))
      
      (goto-char 0)
      
      (let* ((dir
	      (if (> (length (efar-get :panels side :dir)) (efar-panel-width side))
		  (subseq (efar-get :panels side :dir) (- (length (efar-get :panels side :dir)) (efar-panel-width side)))	     	     
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
  ""
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
    (loop repeat panel-height do
	  
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
     nil)
    
    ))





(defun efar-draw-border-line(left center right filler splitter &optional newline)
  ""
  (let ((mode (efar-get :mode)))
    
    (loop for side
	  from (if (or (equal mode :left) (equal mode :both)) 1 2)
	  upto (if (or (equal mode :right) (equal mode :both)) 2 1)
	  
	  initially do (insert-char left 1) 
	  
	  finally do (insert-char right 1) 
	  
	  do
	  
	  (let ((s (if (= side 1) :left :right)))
	    
	    (loop for col from 0 upto (- (efar-get :panels s :column-number) 1)
		  do
		  
		  (let ((col-number (efar-get :panels s :column-number)))
		    
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
  ""
  (let* ((widths ())
	 (left-widths ())
	 (right-widths ())
	 
	 (window-width (efar-get :window-width))
	 (mode (efar-get :mode))
	 (cols-left (efar-get :panels :left :column-number))
	 (cols-right (efar-get :panels :right :column-number))
	 
	 (left-width (cond
		      ((equal mode :both) (floor (- window-width 3) 2))
		      ((equal mode :left) (- window-width 2))
		      ((equal mode :right) 0)))
	 
	 (right-width (cond
		       ((equal mode :both) (ceiling (- window-width 3) 2))
		       ((equal mode :left) 0)
		       ((equal mode :right) (- window-width 2)))))

    (when (not (zerop left-width))
      (let* ((left-min-col-width (floor (/ (- left-width (- cols-left 1)) cols-left)))
	     (left-leftover (- left-width (- cols-left 1) (* left-min-col-width cols-left))))
	
	
	(setf left-widths (make-list cols-left left-min-col-width))
	
	(let ((cnt 0))
	  (while (not (zerop left-leftover))
	    (incf (nth cnt left-widths))
	    (decf left-leftover)
	    (incf cnt)
	    (when (= cnt (length left-widths))
	      (setf cnt 0))))))
    
    (when (not (zerop right-width))
      (let* ((right-min-col-width (floor (/ (- right-width (- cols-right 1))  cols-right)))
	     (right-leftover (- right-width (- cols-right 1) (* right-min-col-width cols-right))))
	
	
	(setf right-widths (make-list cols-right right-min-col-width))
	
	(let ((cnt 0))
	  (while (not (zerop right-leftover))
	    (incf (nth cnt right-widths))
	    (decf right-leftover)
	    (incf cnt)
	    (when (= cnt (length right-widths))
	      (setf cnt 0))))))
    
    (setf widths (cons left-widths right-widths))
    
    (efar-set widths :column-widths)))



(defun efar-frame-size-changed(frame)
  "Function called when frame size changes. Redraws entire eFar buffer."
  (when (get-buffer-window efar-buffer-name)
    (efar-calculate-window-size)
    (efar-calculate-widths)
    (efar-write-enable
     (efar-redraw))))

(defun efar-buffer-killed()
  ""
  (when (string= (buffer-name) efar-buffer-name)
    (when (and
	   efar-save-state?
	   
	   (ignore-errors
	     (efar-save-state))
	   (efar-remove-notifier :left)
	   (efar-remove-notifier :right)))
    (setf efar-state nil)))

(defun efar-emacs-killed()
  ""
  (when (and
	 efar-save-state?
	 (get-buffer efar-buffer-name))
    (ignore-errors
      (efar-save-state))))

(add-hook 'window-size-change-functions 'efar-frame-size-changed)

(add-hook 'kill-buffer-hook 'efar-buffer-killed)

(add-hook 'kill-emacs-hook 'efar-emacs-killed)

(defun efar-get-root-directory(path)
  "Returns a root directory for given PATH."
  ;; get parent directory
  (let ((parent-dir
	 (file-name-directory
	  (directory-file-name path))))
    ;; if we are in root directory already
    (if (string= parent-dir path)
	path
      ;; otherwise check parent directory
      (efar-get-root-directory parent-dir ))))


(defun efar-selected-files(side current?)
  ""
  (let* ((marked-files (efar-get :panels side :selected))
	 (start-file-number (efar-get :panels side :start-file-number))
	 (current-file-number (+ start-file-number (efar-get :panels side :current-pos)))
	 (files (efar-get :panels side :files))
	 
	 (selected-files (when (> (length files) 0)
			   (remove (list ".." t)
				   (mapcar
				    (lambda (fn)
				      (nth fn files))
				    (if (or current? (not marked-files))
					(list current-file-number)
				      marked-files))))))
    selected-files))

(defun efar-abort()
  ""
  (efar-quit-fast-search))
