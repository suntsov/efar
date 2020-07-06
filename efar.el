(require 'cl)
(require 'ido)
(require 'tramp)
(require 'subr-x)
(require 'filenotify)

(defconst efar-state-file-name (concat user-emacs-directory ".efar-state"))
(defconst efar-buffer-name "*eFAR*")
(defvar efar-state nil)
(defconst efar-default-startup-dirs (cons user-emacs-directory  user-emacs-directory))
(defconst efar-save-state? nil)

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
    (setf efar-state (efar-read-state)))
  
  ;; if eFAR state cannot be restored from file (missing or broken file) or saving/restoring of state is disabled
  ;; then initialize state storage with default values
  (when (null efar-state)
    
    (setf efar-state (make-hash-table :test `equal))  
    
    (puthash :panel-left (make-hash-table :test `equal) efar-state)
    (puthash :panel-right (make-hash-table :test `equal) efar-state)
    
    
    (puthash :panels (make-hash-table :test `equal) efar-state)
    (puthash :left (make-hash-table :test `equal) (gethash :panels efar-state))
    (puthash :right (make-hash-table :test `equal) (gethash :panels efar-state))
    
					;(puthash 
    (puthash :dir (car efar-default-startup-dirs) (gethash :left (gethash :panels efar-state)))
    (puthash :dir (cdr efar-default-startup-dirs) (gethash :right (gethash :panels efar-state)))
    
    (setf default-directory (car efar-default-startup-dirs))
    
    (puthash :current-panel :left efar-state)
    
    (puthash :current-pos 0 (gethash :left (gethash :panels efar-state)))
    (puthash :current-pos 0 (gethash :right (gethash :panels efar-state)))
    
    (puthash :selected () (gethash :left (gethash :panels efar-state)))
    (puthash :selected () (gethash :right (gethash :panels efar-state)))
    
    (puthash :start-file-number 0 (gethash :left (gethash :panels efar-state)))
    (puthash :start-file-number 0 (gethash :right (gethash :panels efar-state)))
    
    (puthash :fast-search-string "" efar-state)
    (puthash :fast-search-timer nil efar-state)
    
    (puthash :notification-timer nil efar-state)
    
    (puthash :fast-search-occur 0 efar-state)
    
    (puthash :file-filter "" (gethash :left (gethash :panels efar-state)))
    (puthash :file-filter "" (gethash :right (gethash :panels efar-state)))
    
    (puthash :file-notifier nil (gethash :left (gethash :panels efar-state)))
    (puthash :file-notifier nil (gethash :right (gethash :panels efar-state)))
    
    (puthash :pending-notifications () efar-state)
    
    (puthash :sort-order nil (gethash :left (gethash :panels efar-state)))
    (puthash :sort-order nil (gethash :right (gethash :panels efar-state)))
    
    (puthash :sort-function-name "Name" (gethash :left (gethash :panels efar-state)))
    (puthash :sort-function-name "Name" (gethash :right (gethash :panels efar-state)))
    
    (puthash :search-processes '() efar-state)
    
    (puthash :search-files-to-process 0 efar-state)
    
    (puthash :search-results (make-hash-table :test `equal) efar-state)
    (puthash :search-running? nil efar-state)
    
    (puthash :last-dir (make-hash-table :test `equal) efar-state)
    
    (puthash :mode :both efar-state)
    
    (puthash :column-number 3 (gethash :left (gethash :panels efar-state)))
    (puthash :column-number 2 (gethash :right (gethash :panels efar-state)))
    
    (puthash :column-widths nil efar-state)
    
    (puthash :status-string "Ready" efar-state)
    
    (puthash :status :ready efar-state)
    
    (puthash :reset-status? nil efar-state)

    (puthash :directory-history (make-hash-table :test `equal) efar-state)
    
    (efar-set-keys)
    
    (efar-go-to-dir (efar-get-value :dir :left) :left)
    (efar-go-to-dir (efar-get-value :dir :right) :right)))


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


(defun efar-sort-function-names(&optional side)
  "Returns a list of sort function names. 
The name of a function which is currently used for the panel SIDE (or current panel) becomes a first entry in the list."
  (sort
   (mapcar
    (lambda(e)
      (car e))
    efar-sort-functions)
   (lambda(a b) (when (string= a (efar-get-value :sort-function-name side)) t))))

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
  (efar-set-value :sort-function-name
		  (ido-completing-read "Sort files by: " (efar-sort-function-names side))
		  side)
  (efar-set-value :sort-order
		  (string= (char-to-string 9660) (ido-completing-read "Sort order: " (list (char-to-string 9650) (char-to-string 9660) )))
		  side)
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
  
  (let* ((other-side-notifier (efar-get-value :file-notifier (efar-other-side side)))
	 (descriptor
	  ;; if file-notify-watch for given DIR is already registered for other panel and it is valid, use it	  
	  (if (and
	       (string= (car other-side-notifier) dir)
	       (file-notify-valid-p (cdr other-side-notifier)))
	      (cdr other-side-notifier)
	    ;; otherwise create new one
	    (file-notify-add-watch dir '(change attribute-change) 'efar-notification))))
    
    (efar-set-value :file-notifier (cons dir descriptor) side)))


(defun efar-remove-notifier(side &optional dont-check-other)
  "Remove file-notify-watch registered for panel SIDE.
If same watch is registered for other panel, then don't remove the watch, but just unregister it for panel SIDE, unless DONT-CHECK-OTHER is t"
  (let ((descriptor (cdr (efar-get-value :file-notifier side)))
	(other-descriptor (cdr (efar-get-value :file-notifier (efar-other-side side)))))
    (when (or dont-check-other (not (equal descriptor other-descriptor)))
      (file-notify-rm-watch descriptor))
    (efar-set-value :file-notifier nil side)))

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
	(when (not (member descriptor (gethash :pending-notifications efar-state)))
	  (push descriptor (gethash :pending-notifications efar-state)))
	
	;; if there is a running timer, cancel it
	(let ((timer (gethash :notification-timer efar-state)))	  
	  (when timer (cancel-timer timer))
	  ;; create new timer
	  (puthash :notification-timer
		   (run-at-time "1 sec" nil
				(lambda()
				  (efar-process-pending-notifications)))
		   efar-state))))))


(defun efar-process-pending-notifications()
  "Process all pending file notifications."
  
  ;; while there are notifications in a queue
  (while
      ;; pop next notification
      (let ((descriptor (pop (gethash :pending-notifications efar-state))))       
	(when descriptor
	  ;; if event comes from the watch is registered for left panel directory, refersh left panel
	  (when (equal descriptor (cdr (efar-get-value :file-notifier :left)))
	    (efar-refresh-dir :left))
	  ;; if event comes from the watch is registered for right panel directory, refersh right panel
	  (when (equal descriptor (cdr (efar-get-value :file-notifier :right)))
	    (efar-refresh-dir :right))))))

;;--------------------------------------------------------------------------------
;; end of file notification functions
;;--------------------------------------------------------------------------------

(defun efar-remove-file-state()
  ""
  (interactive)
  (delete-file efar-state-file-name))

(defun efar-save-state()
  ""
  (with-temp-file efar-state-file-name
    (let ((copy (copy-hash-table efar-state)))
      
      (puthash :file-notifier nil (gethash :left (gethash :panels copy)))
      (puthash :file-notifier nil (gethash :right (gethash :panels copy)))
      (puthash :pending-notifications () copy))))


(defun efar-read-state() 
  ""
  (interactive)
  (with-temp-buffer
    (insert-file-contents efar-state-file-name)
    (cl-assert (eq (point) (point-min)))
    (read (current-buffer))))

(when (eq window-system 'w32)
  (set-default 'tramp-auto-save-directory "C:\test")
  (set-default 'tramp-default-method "plink"))


;;------------------------------------------------------------------
;; efar file operations
;;------------------------------------------------------------------
(defun efar-create-new-directory()
  "Create new directory."
  
  (efar-with-notification-disabled
   (let ((new-dir-name (read-string "Input name for new directory: "))
	 (side (gethash :current-panel efar-state)))
     
     ;; try to create a directory
     (efar-handle-error      
      (make-directory new-dir-name nil))
     
     ;; refresh panel and move selection
     (efar-refresh-dir side nil new-dir-name)
     
     ;; if other panel displays same directory as current one, refresh it as well
     (when (string= (efar-get-value :dir) (efar-get-value :dir (efar-other-side)))
       (efar-refresh-dir (efar-other-side))))))



(defun efar-copy-or-move-files(operation)
  ""
  
  (unwind-protect
      
      (progn 
	(efar-set-status :busy (if (equal operation :copy)
				   "Copying files..."
				 "Moving files..."))
	
	(let ((start-time (time-to-seconds (current-time))))
	  
	  (efar-with-notification-disabled
	   (let* ((side (gethash :current-panel efar-state))
		  (todir  (efar-get-value :dir (efar-other-side)) )	 
		  (selected (gethash :selected (efar-panel side)))
		  (start-file-number (gethash :start-file-number (efar-panel side)))
		  (file-number (+ start-file-number (gethash :current-pos (efar-panel side))))
		  
		  (files (mapcar
			  (lambda (fn)
			    (efar-get-short-file-name (nth fn (gethash :files (efar-panel side)))))
			  
			  (if selected 
			      selected
			    (list file-number )))))
	     
	     (when  (> file-number 0)	      
	       (efar-copy-or-move-files-int operation files (read-directory-name (if (equal operation :copy) "Copy selected files to " "Move selected files to ") todir todir)))
	     
	     (efar-refresh-dir :left)
	     (efar-refresh-dir :right)))
	  (print (- (time-to-seconds (current-time)) start-time))))
    
    (efar-set-status :ready "Ready")))

(defun efar-copy-or-move-files-int(operation files todir &optional fromdir overwrite?)
  ""
  
  (cl-labels ;; make local recursive function (needed to share variable overwrite? in recursion) 
      ((do-operation (operation files todir fromdir)
		     ;; if FROMDIR is not set, we use default-directory as source directory
		     (let ((default-directory (if fromdir fromdir default-directory)))
		       
		       (mapc ;; for each file in FILES		   
			(lambda (f)
			  ;; skip files "." and ".."
			  (unless (or (string= f ".") (string= f ".."))
			    ;; get new file name in destination folder
			    (let ((newfile
				   (if (file-directory-p todir)
				       (expand-file-name f todir)
				     todir)))
			      
			      (cond 
			       ;; if file is a real file and doesn't exist in destination folder
			       ((and (not (file-directory-p f)) (not (file-exists-p newfile)))
				;; we just copy it using elisp function
				
				(if (equal operation :copy)
				    (efar-handle-error (copy-file f newfile))
				  (efar-handle-error (rename-file f newfile nil))))
			       
			       
			       ;; if file is a directory and doesn't exist in destination folder
			       ((and (file-directory-p f) (not (file-exists-p newfile)))
				;; we just copy directory using elisp function
				(if (equal operation :copy)
				    (efar-handle-error (copy-directory f newfile nil nil nil))
				  (efar-handle-error (rename-file f newfile))))
			       
			       ;; if file is a real file and does exist in destination folder
			       ((and (not (file-directory-p f)) (file-exists-p newfile))
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
					(efar-handle-error (copy-file f newfile t nil nil nil))
				      (efar-handle-error (rename-file f newfile t))))))
			       
			       ;; if file is a directory and does exist in destination folder
			       ((and (file-directory-p f) (file-exists-p newfile))
				;; we call local function recursively 
				(do-operation operation (directory-files f nil nil t) newfile (expand-file-name f default-directory) )
				(when (equal operation :move)
				  (delete-directory f)))))))
			
			files))))
    ;; call local function first time 
    (do-operation operation files todir fromdir)) )


(defun efar-delete-selected()
  ""
  (efar-set-status :busy "Deleting files...")
  
  (unwind-protect
      (progn
	
	(efar-with-notification-disabled
	 (let* ((side (gethash :current-panel efar-state))
		(selected (gethash :selected (efar-panel side)))     
		(start-file-number (gethash :start-file-number (efar-panel side)))
		(file-number (+ start-file-number (gethash :current-pos (efar-panel side))))
		
		(to-delete (if selected 
			       selected
			     (list file-number ))))
	   
	   (when (and (> file-number 0) (string= "Yes" (ido-completing-read "Delete selected files? " (list "Yes" "No"))))
	     (mapc
	      (lambda (e)
		(let* ((file (nth e (gethash :files (efar-panel side))))
		       (file-name (car file))
		       (dir? (car (cdr file)))
		       )
		  
		  (if dir?
		      (efar-handle-error (delete-directory file-name t))
		    (efar-handle-error (delete-file file-name)))))
	      
	      to-delete)
	     
	     (efar-refresh-dir side)
	     
	     (when (string= (efar-get-value :dir) (efar-get-value :dir (efar-other-side)))
	       (efar-refresh-dir (efar-other-side)))))))
    
    (efar-set-status :ready "Ready")))


(defmacro efar-with-notification-disabled(&rest body)
  ""
  `(let ((file-notifier-left (efar-get-value :file-notifier :left))
	 (file-notifier-right (efar-get-value :file-notifier :right)))
     
     (when (file-notify-valid-p (cdr file-notifier-left))
       (efar-remove-notifier :left))
     
     (when (file-notify-valid-p (cdr file-notifier-right))
       (efar-remove-notifier :right))
     
     ,@body
     
     (efar-setup-notifier (efar-get-value :dir :left) :left)
     (efar-setup-notifier (efar-get-value :dir :right) :right)))

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


(defun efar-get-value (p &optional s) 
  ""
  (gethash p (efar-panel (or s (gethash :current-panel efar-state)))))

(defun efar-set-value(p v &optional s)
  ""
  (puthash p v (efar-panel (or s (gethash :current-panel efar-state)))))


(defun efar-reset-status()
  ""
  (when (gethash :reset-status? efar-state)
    (puthash :reset-status? nil efar-state)
    (efar-set-status :ready "Ready")))

(defun efar-set-status(status &optional status-string seconds reset?)
  ""
  (when reset?
    (puthash :reset-status? t efar-state))
  
  (let ((prev-status (gethash :status efar-state))
	(prev-status-string (gethash :status-string efar-state)))
    
    (puthash :status status efar-state)
    (when status-string (puthash :status-string status-string efar-state))
    (efar-output-status)
    
    (when seconds
      (run-at-time seconds nil
		   `(lambda()
		      (efar-set-status ',prev-status ',prev-status-string))))))

(defun efar-output-status(&optional status)
  ""
  (efar-write-enable 
   
   (let* ((w (- (gethash :window-width efar-state) 2))
	  (status-string (efar-prepare-file-name (or status (gethash :status-string efar-state)) w)))
     
     (goto-char 0)
     
     (forward-line (+ 4 (gethash :panel-height efar-state)))
     
     (move-to-column 1)
     
     (let ((p (point)))
       (replace-rectangle p (+ p w) status-string)
       
       (put-text-property p (+ p w) 'face 'efar-header-face))))
  (sit-for 0.001))

(defun efar-set-keys()
  ""
  
  
  (local-set-key (kbd "<down-mouse-1>")
		 (lambda (event)
		   (interactive "e")
		   (if (<  (car (nth 6 (nth 1 event))) (+ 2 (gethash :panel-width efar-state) ))
		       (progn
			 (setf (gethash :current-panel efar-state) :left)
			 (setf default-directory (gethash :dir (efar-panel :left))))
		     
		     (progn
		       (setf (gethash :current-panel efar-state) :right)
		       (setf default-directory (gethash :dir (efar-panel :right))))
		     )
		   (efar-write-enable (efar-redraw))))
  
  
  (let ((characters (list
		     ?a ?b ?c ?d ?e ?f ?g ?h ?i ?j ?k ?l ?m ?n ?o ?p ?q ?r ?s ?t ?u ?v ?w ?x ?y ?z
		     ?A ?B ?C ?D ?E ?F ?G ?H ?I ?J ?K ?L ?M ?N ?O ?P ?Q ?R ?S ?T ?U ?V ?W ?X ?Y ?Z
		     ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9 ?0
		     ?( ?) ?.
		     ?- ?_
		     32)))
    
    
    (loop for k in characters do       
	  (local-set-key (char-to-string k) `(lambda () 
					       (interactive)
					       (efar-fast-search ',k)))))
  
  (local-set-key (kbd "<backspace>") (lambda()
				       (interactive)
				       (efar-fast-search :backspace)))
  
  (local-set-key (kbd "C-n") (lambda ()
			       (interactive)
			       (efar-fast-search :next)))
  (local-set-key (kbd "C-b") (lambda ()
			       (interactive)
			       (efar-fast-search :prev)))
  
  
  (local-set-key (kbd "C-o") (lambda () 
			       (interactive)
			       (efar-display-console)))
  
  
  
  (local-set-key (kbd "<insert>") (lambda () 
				    (interactive)
				    (efar-mark-file)))
  
  (local-set-key (kbd "<f12>") (lambda () 
				 (interactive) 
				 (efar-init)))
  
  (local-set-key (kbd "<down>") (lambda () 
				  (interactive)
				  (efar-move-cursor  :down)))
  
  (local-set-key (kbd "<up>") (lambda () 
				(interactive)
				(efar-move-cursor  :up)))
  
  ;; ToDo improve performance by changing face for affected lines only    
  (local-set-key (kbd "<right>") (lambda () 
				   (interactive)
				   (efar-move-cursor  :right)))
  
  ;; ToDo improve performance by changing face for affected lines only    
  (local-set-key (kbd "<left>") (lambda () 
				  (interactive)
				  (efar-move-cursor  :left)))
  
  (local-set-key (kbd "<home>") (lambda () 
				  (interactive)
				  (efar-move-cursor  :home)))
  
  (local-set-key (kbd "C-<left>") (lambda () 
				    (interactive)
				    (efar-move-cursor  :home)))
  
  (local-set-key (kbd "<end>") (lambda () 
				 (interactive)
				 (efar-move-cursor  :end)))
  
  (local-set-key (kbd "C-<right>") (lambda () 
				     (interactive)
				     (efar-move-cursor  :end)))
  
  (local-set-key (kbd "RET") (lambda () 
			       (interactive)
			       (efar-write-enable			
				(let* ((side (gethash :current-panel efar-state))				       
				       (start-file-number (efar-get-value :start-file-number))
				       (file-number (+ start-file-number (efar-get-value :current-pos)))
				       (file (nth file-number (efar-get-value :files)))
				       (current-dir-path (efar-get-value :dir)))
				  
				  (when (car (cdr file))
				    (let ((newdir (file-name-as-directory (expand-file-name (car file) current-dir-path))))
				      (cond
				       ((not (file-accessible-directory-p  newdir))
					(efar-set-status :ready (concat "Directory "  newdir " is not accessible") 3))
				       
				       (t				
					(progn
					  (efar-go-to-dir newdir side)
					  (efar-redraw))))))))))
  
  (local-set-key (kbd "TAB") (lambda () 
			       (interactive)
			       (when (equal (gethash :mode efar-state) :both)
				 (efar-write-enable
				  (let ((side (gethash :current-panel efar-state)))
				    (if (equal side  :left)
					(progn
					  (setf (gethash :current-panel efar-state) :right)
					  (setf default-directory (gethash :dir (efar-panel :right))))
				      (progn
					(setf (gethash :current-panel efar-state) :left)
					(setf default-directory (gethash :dir (efar-panel :left))))))
				  (efar-redraw)))))
  
  
  (local-set-key (kbd "C-c TAB") (lambda ()
				   (interactive)
				   (efar-open-same-directory-other-panel)))
  
  (local-set-key (kbd "<f4>") (lambda () 
				(interactive)
				(efar-edit-file)))
  
  (local-set-key (kbd "<M-f4>") (lambda () 
				  (interactive)
				  (efar-open-file-in-external-app)))
  
  (local-set-key (kbd "<f5>") (lambda () 
				(interactive)
				(efar-copy-or-move-files :copy)))
  
  (local-set-key (kbd "<f6>") (lambda () 
				(interactive)
				(efar-copy-or-move-files :move)))
  
  
  (local-set-key (kbd "<C-insert>") (lambda () 
				      (interactive)
				      (efar-deselect-all)))
  
  (local-set-key (kbd "<M-f1>") (lambda ()
				  (interactive)
				  (efar-change-directory :left)))
  
  (local-set-key (kbd "<M-f2>") (lambda ()
				  (interactive)
				  (efar-change-directory :right)))
  
  (local-set-key (kbd "<C-f1>") (lambda ()
				  (interactive)
				  (efar-change-sort-function :left)))
  
  (local-set-key (kbd "<C-f2>") (lambda ()
				  (interactive)
				  (efar-change-sort-function :right)))
  
  (local-set-key (kbd "<M-f7>") (lambda ()
				  (interactive)
				  (efar-start-search)))
  
  
  (local-set-key (kbd "<f7>") (lambda ()
				(interactive)
				(efar-create-new-directory)))
  
  (local-set-key (kbd "<f8>") (lambda ()
				(interactive)
				(efar-delete-selected)))
  
  (local-set-key (kbd "<S-f1>") (lambda()
				  (interactive)
				  (efar-filter-files :left)))
  
  (local-set-key (kbd "<S-f2>") (lambda()
				  (interactive)
				  (efar-filter-files :right)))
  
  (local-set-key (kbd "<f11>") (lambda()
				 (interactive)
				 (efar-change-mode)))
  
  
  (local-set-key (kbd "C-c +") (lambda()
				 (interactive)
				 (efar-change-column-number t)))
  
  (local-set-key (kbd "C-c -") (lambda()
				 (interactive)
				 (efar-change-column-number nil)))
  
  (local-set-key (kbd "<f10>") (lambda()
				 (interactive)
				 (efar-copy-current-path)))
  )

(defun efar-change-column-number(increase)
  ""
  (let ((side (gethash :current-panel efar-state)))
    
    (if increase
	(efar-set-value :column-number (+ (efar-get-value :column-number side) 1) side)
      (when (> (efar-get-value :column-number side) 1)
	(efar-set-value :column-number (- (efar-get-value :column-number side) 1) side)
	
	(let ((file (car (efar-current-file side))))
	  (when (not (string= file "..")) (efar-go-to-file file))))))
  
  (efar-calculate-widths)
  (efar-write-enable (efar-redraw)))

(defun efar-change-mode()
  "Switch mode from double to single-panel or vice versa.
If a double mode is active then actual panel becomes fullscreen."
  (let ((current-mode (gethash :mode efar-state))
	(side (gethash :current-panel efar-state)))
    
    (puthash :mode
	     (cond
	      ((equal current-mode :both) side)
	      (t :both))
	     
	     efar-state)
    
    (efar-calculate-widths)
    
    (efar-write-enable (efar-redraw))))

(defun efar-open-same-directory-other-panel()
  "Opens current pannel's direcotry in other panel."
  (efar-go-to-dir default-directory (efar-other-side))
  (efar-write-enable (efar-redraw)))

(defun efar-copy-current-path()
  "Copies to the clipboard the full path to the current file or directory."
  (let* ((side (gethash :current-panel efar-state))
	 (fnum (+ (gethash :start-file-number (efar-panel side)) (gethash :current-pos (efar-panel side)) ))
	 (file (nth fnum (gethash :files (efar-panel side))))
	 (fname (expand-file-name (car file) (gethash :dir (efar-panel side)))))
    (kill-new fname)))

(defun efar-open-file-in-external-app()
  ""
  (let* ((side (gethash :current-panel efar-state))
	 (fnum (+ (gethash :start-file-number (efar-panel side)) (gethash :current-pos (efar-panel side)) ))
	 (file (nth fnum (gethash :files (efar-panel side))))
	 (@fname (expand-file-name (car file) (gethash :dir (efar-panel side)))))
    
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
  (let ((side (or side (gethash :current-panel efar-state))))
    
    (efar-set-value
     :file-filter
     (read-string "String to filter file names: " (efar-get-value :file-filter side))
     side)
    
    (efar-get-file-list side)
    (efar-set-value :start-file-number 0 side)
    (efar-set-value :current-pos 0 side)
    (efar-write-enable (efar-redraw))))

(defun efar-display-console ()
  ""
  (if (not (get-buffer "*efar-shell*"))
      (shell "*efar-shell*")
    (progn
      (with-current-buffer (get-buffer "*efar-shell*")
	(insert (concat "cd " (efar-get-value :dir)))
	(comint-send-input nil t))
      (display-buffer "*efar-shell*")
      )))


(defun efar-fast-search (k)
  ""
  
  (let ((timer (gethash :fast-search-timer efar-state)))
    
    (when timer (cancel-timer timer))
    
    (puthash :fast-search-timer
	     (run-at-time "5 sec" nil
			  (lambda()
			    (when (not (null efar-state))
			      (puthash :fast-search-string "" efar-state)
			      (efar-output-status)
			      (puthash :fast-search-occur 0 efar-state)
			      (puthash :fast-search-timer nil efar-state))))
	     efar-state)
    
    (let ((str (gethash :fast-search-string efar-state)))
      
      (cond
       
       ((equal k :next)
	(when (> (length str) 0)
	  (incf (gethash :fast-search-occur efar-state))))
       
       ((equal k :prev)
	(when (> (length str) 0)
	  (decf (gethash :fast-search-occur efar-state))))
       
       ((equal k :backspace)
	(when (> (length str) 0)
	  (setf str (substring str 0 (- (length str) 1)))))
       
       (t
	(setf str (concat str (format "%c" k)))))
      
      
      (let ((file-name (nth
			(gethash :fast-search-occur efar-state)
			
			(mapcan (lambda (e)
				  (when (string-match str (efar-get-short-file-name e))
				    (list (car e))))
				(efar-get-value :files)))))
	(when file-name
	  (efar-go-to-file file-name nil 0)
	  (efar-write-enable (efar-redraw))))
      
      (puthash :fast-search-string str efar-state))
    
    (efar-output-status (concat "Fast search: " (gethash :fast-search-string efar-state)))))


(defun efar-get-accessible-directory-in-path (path)
  "Return first accessible directory in the PATH going from bottom to up. If there are no accessible directories in the given path, return user-emacs-directory."
  
  ;; if directory PATH doesn't exist or isn't accessible
  (if (not (and (file-exists-p path)
		(file-accessible-directory-p path)))
      
      (efar-output-status (concat "Does not exist " path)) 
    
    ;; get parent directory
    (let ((parent-dir
	   (file-name-directory
	    (directory-file-name path))))
      ;; if we are in root directory already
      (if (string= parent-dir path)
	  ;; that means there are no accessible directories in the path and we return user-emacs-directory
	  user-emacs-directory
	;; otherwise check parent directory
	(efar-get-accessible-directory-in-path parent-dir )))
    
    ;; return first accessible directory
    path))

(defun efar-refresh-dir(&optional side move-to-first? move-to-file-name)
  ""
  (let ((dir (efar-get-accessible-directory-in-path (efar-get-value :dir side))))
    
    (efar-set-value :dir dir side)
    (efar-set-value :selected () side)
    (efar-set-value :fast-search-string "" side)
    
    (let ((current-file-name (cond
			      (move-to-first? "")
			      (move-to-file-name move-to-file-name)
			      (t (efar-current-file-name side))))
	  (current-file-number (if move-to-first? 0 (efar-current-file-number side))))
      
      (efar-get-file-list side)
      
      (efar-go-to-file current-file-name side current-file-number))
    
    (efar-write-enable (efar-redraw))))


(defun efar-go-to-dir(dir &optional side)
  ""
  
  (let* ((dir (when dir (efar-get-accessible-directory-in-path (expand-file-name dir))))
	 (side (or side (gethash :current-panel efar-state)))
	 (current-dir (efar-get-value :dir side))
	 (parent-dir (efar-get-parent-dir current-dir))
	 (go-to-parent? (string= dir parent-dir)))
    
    (when (eq side (gethash :current-panel efar-state))
      (setf default-directory dir))
    
    (efar-set-value :selected ()  side)
    (efar-set-value :dir dir side)
    
    (efar-set-value :file-filter "" side)
    (puthash :fast-search-string "" efar-state)
    
    (efar-get-file-list side)        
    
    (if go-to-parent?
	(progn
	  (efar-go-to-file current-dir side 0))
      (progn
	(efar-set-value :start-file-number 0 side)
	(efar-set-value :current-pos 0 side)))
    
    
    (let ((disk (concat (car (split-string dir "/")) "/")))
      (puthash disk dir (gethash :last-dir efar-state) ))

    (puthash (efar-get-root-directory dir) dir (gethash :directory-history efar-state))
    
    (efar-setup-notifier dir side)))


(defun efar-get-parent-dir(dir)
  ""
  (file-name-directory (directory-file-name dir)))

(defun efar-go-to-file(file &optional side prev-file-number)
  ""
  (let* ((side (or side (gethash :current-panel efar-state)))
	 (file (directory-file-name (expand-file-name file (efar-get-value :dir side))))
	 (number-of-files (length (efar-get-value :files side)))
	 (new-file-number
	  (or
	   (cl-position file
			(mapcar (lambda (e) (car e)) (efar-get-value :files side))
			:test 'string=)
	   (if (>= prev-file-number number-of-files)
	       (- number-of-files 1)
	     prev-file-number))))
    
    (cond
     
     ((and
       (>= new-file-number (efar-get-value :start-file-number side))
       (< new-file-number (+ (efar-get-value :start-file-number side) (* 2 (gethash :panel-height efar-state)))))
      
      (progn
	(efar-set-value :current-pos (- new-file-number (efar-get-value :start-file-number side)) side)))
     
     ((< new-file-number (* 2 (gethash :panel-height efar-state)))
      (progn
	(efar-set-value :start-file-number 0 side)
	(efar-set-value :current-pos new-file-number side)))
     
     (t 
      (progn
	(efar-set-value :start-file-number new-file-number side)
	(efar-set-value :current-pos 0 side))))))


(defun efar-current-file-number(&optional side)
  ""
  (let* ((side (or side (gethash :current-panel efar-state)))
	 (start-file-number (efar-get-value :start-file-number side)))
    
    (+ start-file-number (efar-get-value :current-pos side))))

(defun efar-current-file-name(&optional side)
  ""
  (car (efar-current-file side)))

(defun efar-current-file(&optional side)
  ""
  (nth (efar-current-file-number side) (efar-get-value :files side)))

;;(loop for i from 1 to 100 do (make-directory (concat "c:/test/" (int-to-string i))))
(defun efar-other-side(&optional side)
  ""
  (let ((side (if side side (gethash :current-panel efar-state))))
    (if (equal side :left) :right :left)))



(defun efar-change-directory(side)
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
      (setf dir
	    (or (gethash dir (gethash :last-dir efar-state))
		dir))

      (efar-go-to-dir (or (gethash dir (gethash :directory-history efar-state)) dir) side)
      
      (efar-write-enable (efar-redraw)))))


(defun efar-files-as-string(file-numbers)
  ""
  (let ((side (gethash :current-panel efar-state)))
    (mapconcat
     (lambda(x)
       (car (nth x (gethash :files (efar-panel side)))))
     file-numbers
     ", ")))



(defun efar-deselect-all()
  ""
  (efar-write-enable
   (let ((side (gethash :current-panel efar-state)))
     (puthash :selected () (efar-panel side))
     (efar-redraw))))

(defun efar-mark-file()
  ""
  (let* ((side (gethash :current-panel efar-state))
	 (start-file-number (gethash :start-file-number (efar-panel side)))
	 (current-position (gethash :current-pos (efar-panel side)))
	 (selected-file-number (+ start-file-number current-position)))
    
    (or (string= (car (nth selected-file-number (gethash :files (efar-panel side)))) "..")
	(if (member selected-file-number (gethash :selected (efar-panel side)))
	    (setf (gethash :selected (efar-panel side)) (delete selected-file-number (gethash :selected (efar-panel side))))
	  (push selected-file-number (gethash :selected (efar-panel side)))))
    
    (efar-move-cursor  :down)))

(defun efar-edit-file()
  ""
  (let* ((side (gethash :current-panel efar-state))
	 (fnum (+ (gethash :start-file-number (efar-panel side)) (gethash :current-pos (efar-panel side)) ))
	 (file (nth fnum (gethash :files (efar-panel side))))
	 (fdir (expand-file-name (car file) (gethash :dir (efar-panel side)))))
    (find-file fdir)))


(defun efar-panel(side)
  ""
  (gethash side (gethash :panels efar-state)))

(defun efar-set-files-order(files side)
  ""
  (if (efar-get-value :sort-order side)
      (reverse files)
    files))

(defun  efar-is-root-directory(dir)
  "Returns t if DIR is a root directory, nil otherwise."
  (string= (file-name-as-directory dir)
	   (file-name-directory (directory-file-name dir))))

(defun efar-get-file-list(side)
  ""
  (let ((filter (efar-get-value :file-filter side))
	(root? (efar-is-root-directory (efar-get-value :dir side))))
    
    (puthash :files
	     (mapcar
	      (lambda(e)
		(append e (list (list (cons 23 "sdfj;sdf;sdflkwlkfjksjdlkmsdwmer34") (cons 56 "dsfdjij34j43549t09dv9vlkerlkt5j90dv0-dv"))))
		)
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
					     (and (not (string-suffix-p "/.." (car f)))
						  (> (length filter) 0 )
						  (not (string-match (wildcard-to-regexp filter) (car f))))))
			       
			       ;; files and attributes for current directory
			       (directory-files-and-attributes (gethash :dir (efar-panel side)) t nil t)))
			     
			     ;; get selected sort function
			     (sort-function (efar-get-sort-function (efar-get-value :sort-function-name side))))
			 
			 ;; sort file list according to selected sort function
			 (if sort-function
			     (sort files sort-function)
			   files))
		       
		       side)))
	     
	     (efar-panel side))))


(defun efar-move-cursor (direction)
  ""
  (efar-reset-status)
  
  (efar-write-enable
   (let* ((side (gethash :current-panel efar-state))
	  (curr-pos (gethash :current-pos (efar-panel side)))
	  (max-files-in-column (- (gethash :panel-height efar-state) 1))
	  (max-file-number (length (gethash :files (efar-panel side))))
	  (start-file-number (gethash :start-file-number (efar-panel side)))
	  (col-number (efar-get-value :column-number side))
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
		  (puthash :start-file-number  (if (< rest 0) 0 rest) (efar-panel side))
		  (puthash :current-pos (if (< rest 0) (- start-file-number 1) (- (* col-number max-files-in-column) 1)) (efar-panel side)))))
	;; else move up by one
	(t (progn
	     (push  (gethash :current-pos (efar-panel side)) affected-item-numbers)
	     (decf (gethash :current-pos (efar-panel side)))
	     (push  (gethash :current-pos (efar-panel side)) affected-item-numbers)))))
      
      ;; if DOWN key is pressed
      ((equal direction :down) 
       (cond 
	;; if we are on the last file in the list - do nohing
	((= (+ start-file-number curr-pos) (- max-file-number 1)) nil)
	;; else if we are on last item 
	((= curr-pos (- (* max-files-in-column col-number) 1)) 
	 (progn (puthash :start-file-number (+ start-file-number curr-pos 1) (efar-panel side))
		(puthash :current-pos 0 (efar-panel side)) ))
	;; else move down by one
	(t (progn 
	     (push  (gethash :current-pos (efar-panel side)) affected-item-numbers)
	     (incf (gethash :current-pos (efar-panel side)))
	     (push  (gethash :current-pos (efar-panel side)) affected-item-numbers)	     
	     ))))
      
      ;; if LEFT key is pressed
      ((equal direction :left)
       (cond
	;; if we are on the first file in the list - do nothing
	((and (= start-file-number 0) (= curr-pos 0)) nil)
	
	;; we are in right column - move left by max-files-in-column
	((>= curr-pos max-files-in-column) (puthash :current-pos (- curr-pos max-files-in-column) (efar-panel side)))
	
	;; we are in left column
	((< curr-pos max-files-in-column)
	 (cond
	  ((= start-file-number 0) (puthash :current-pos 0 (efar-panel side)))
	  ((> start-file-number (* col-number max-files-in-column)) (puthash :start-file-number (- start-file-number max-files-in-column) (efar-panel side)))
	  ((<= start-file-number (* col-number max-files-in-column)) (and
								      (puthash :start-file-number 0 (efar-panel side))
								      (puthash :current-pos 0 (efar-panel side)) )))) ))
      
      ;; if HOME key is pressed
      ((equal direction :home)
       (and
	(puthash :start-file-number 0 (efar-panel side))
	(puthash :current-pos 0 (efar-panel side)) ))
      
      
      ;; if END key is pressed
      ((equal direction :end)
       (and
	(puthash :start-file-number (if (< max-file-number (* col-number max-files-in-column)) 0 (- max-file-number (* col-number max-files-in-column))) (efar-panel side))
	(puthash :current-pos (- (if (< max-file-number (* col-number max-files-in-column))  max-file-number (* col-number max-files-in-column)) 1) (efar-panel side)) ))
      
      
      ;; if RIGHT key is pressed
      ((equal direction :right)
       (cond
	;; if we are on the last file in the list - do nohing
	((= (+ start-file-number curr-pos) (- max-file-number 1)) nil)
	
	;; else if there is more than max-files-in-column left
	((> (- max-file-number start-file-number curr-pos) max-files-in-column)
	 (if (< curr-pos (* (- col-number 1) max-files-in-column))
	     (puthash :current-pos (+ curr-pos max-files-in-column) (efar-panel side))
	   (puthash :start-file-number (+ start-file-number max-files-in-column) (efar-panel side)))))))
     
     (efar-output-files side affected-item-numbers)
     
     (efar-output-file-details side))))


(defun efar-calculate-window-size()
  ""
  (let ((mode (gethash :mode efar-state)))
    
    (puthash :window-width (window-width) efar-state)
    (puthash :window-height (window-height) efar-state)
    (puthash :panel-height (- (window-height) 7) efar-state)))

(defun efar-redraw()
  ""
  (efar-calculate-window-size)
  (erase-buffer)
  (efar-draw-border )
  
  (put-text-property (point-min) (point-max) 'face 'efar-border-face)
  
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
  
  (let ((mode (gethash :mode efar-state)))
    
    (when (or (equal mode :both) (equal mode side))
      
      (let ((current-file-number (efar-current-file-number side)))
	
	(when current-file-number
	  
	  (let* ((file (nth current-file-number (efar-get-value :files side)))
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
	    
	    (forward-line (+ 2 (gethash :panel-height efar-state)))
	    
	    (move-to-column col-number)
	    
	    (let ((p (point)))
	      (replace-rectangle p (+ p w) status-str)
	      
	      (put-text-property p (+ p w) 'face 'efar-border-face))))))))

(defun efar-panel-width(side)
  ""
  (let ((widths (if (equal side :left)
		    (car (gethash :column-widths efar-state))
		  (cdr (gethash :column-widths efar-state)))))
    
    (+ (apply '+ widths)
       (- (length widths) 1))))


(defun efar-output-files(side &optional affected-item-numbers)
  ""
  
  (let ((mode (gethash :mode efar-state))
	(widths (if (equal side :left)
		    (car (gethash :column-widths efar-state))
		  (cdr (gethash :column-widths efar-state)))))
    
    (when (or (equal mode :both) (equal mode side))
      
      (goto-char 0)
      (forward-line)
      
      (let* ((start-pos (cond
			 ((equal side :left) 1)
			 ((and (equal side :right) (equal mode :right)) 1)
			 (t (+ (efar-panel-width :left) 2))))
	     (max-files-in-column (- (gethash :panel-height efar-state) 1))
	     (cnt 0)
	     (col-number (length widths))
	     
	     (files
	      (append
	       (subseq  (gethash :files (efar-panel side))
			(gethash :start-file-number (efar-panel side))  
			(+ (gethash :start-file-number (efar-panel side)) 
			   (if (> (- (length (gethash :files (efar-panel side))) (gethash :start-file-number (efar-panel side))) (* max-files-in-column col-number)) (* max-files-in-column col-number)
			     (- (length (gethash :files (efar-panel side))) (gethash :start-file-number (efar-panel side)))))) 
	       
	       ;; append empty items if number of files to display is less then max files in panel
	       ;; needed to overwrite old entries
	       (make-list (let ((rest (- (length (gethash :files (efar-panel side))) (gethash :start-file-number (efar-panel side)))))
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
				 
				 (marked? (member (+ (gethash :start-file-number (efar-panel side)) cnt) (gethash :selected (efar-panel side))))
				 
				 (str (efar-prepare-file-name (concat (and marked? "*") (if (string= (efar-get-value :dir side) "Search") (car f) (file-name-nondirectory (car f))) ) w)))
			    
			    
			    (replace-rectangle p (+ p (length str)) str)
			    
			    
			    (let ((dir? (car (cdr f)))
				  
				  (current? (and
					     (= cnt (gethash :current-pos (efar-panel side)))
					     (equal side (gethash :current-panel efar-state)))))
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
	      (forward-line))))))




(defun efar-output-header(side)
  ""
  (let ((mode (gethash :mode efar-state)))
    
    (when (or (equal mode :both) (equal mode side))
      
      (goto-char 0)
      (forward-line)
      
      (let* ((col-number (cond
			  ((or (equal side :left) (not (equal mode :both))) 1)
			  (t (+ (efar-panel-width :left) 2))))
	     
	     (filter (efar-get-value :file-filter side))
	     (str (concat	       
		   (substring (efar-get-value :sort-function-name side) 0 1)
		   (if (efar-get-value :sort-order side) (char-to-string 9660) (char-to-string 9650) )
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
  (let ((mode (gethash :mode efar-state)))
    
    (when (or (equal mode :both) (equal mode side))
      
      (goto-char 0)
      
      (let* ((dir
	      (if (> (length (gethash :dir (efar-panel side))) (efar-panel-width side))
		  (subseq (gethash :dir (efar-panel side)) (- (length (gethash :dit (efar-panel side))) (efar-panel-width side)))	     	     
		(gethash :dir (efar-panel side))))
	     
	     (col-number (cond
			  ((not (equal mode :both))  (- (floor (window-width) 2) (floor (length dir) 2)))
			  (t (- (* (floor (window-width) 4) (if (equal side :left) 1 3)) (floor (length dir) 2))))))
	
	(move-to-column col-number)
	
	(let ((p (point)))
	  (replace-rectangle p (+ p (length dir)) dir)
	  (if (equal side (gethash :current-panel efar-state))
	      (put-text-property p (+ p (length dir)) 'face 'efar-dir-name-current-face)
	    (put-text-property p (+ p (length dir)) 'face 'efar-dir-name-face)))))))


(defun efar-draw-border()
  ""
  (goto-char 0)
  
  (let ((panel-height (gethash :panel-height efar-state)))
    
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
  (let ((mode (gethash :mode efar-state)))
    
    (loop for side
	  from (if (or (equal mode :left) (equal mode :both)) 1 2)
	  upto (if (or (equal mode :right) (equal mode :both)) 2 1)
	  
	  initially do (insert-char left 1) 
	  
	  finally do (insert-char right 1) 
	  
	  do
	  
	  (let ((s (if (= side 1) :left :right)))
	    
	    (loop for col from 0 upto (- (efar-get-value :column-number s) 1)
		  do
		  
		  (let ((col-number (efar-get-value :column-number s)))
		    
		    (insert-char filler (nth col (if (= side 1)
						     (car (gethash :column-widths efar-state))
						   (cdr (gethash :column-widths efar-state)))))
		    
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
	 
	 (window-width (gethash :window-width efar-state))
	 (mode (gethash :mode efar-state))
	 (cols-left (efar-get-value :column-number :left))
	 (cols-right (efar-get-value :column-number :right))
	 
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
    
    (puthash :column-widths widths efar-state)))



(defface efar-border-face
  '((t :foreground "white"
       :background "navy"
       ;;:weight bold 
       :underline nil
       ))
  "The face used for representing the visited link to the file")


(defface efar-file-face
  '((t :foreground "deep sky blue"
       :background "navy"
       ;;:weight bold 
       :underline nil
       ))
  "The face used for representing the visited link to the file")

(defface efar-file-executable-face
  '((t :foreground "green"
       :background "navy"
       ;;:weight bold 
       :underline nil
       ))
  "The face used for representing the visited link to the file")

(defface efar-file-marked-face
  '((t :foreground "gold"
       :background "navy"
       ;;:weight bold 
       :underline nil
       ))
  "The face used for representing the visited link to the file")

(defface efar-dir-face
  '((t :foreground "white"
       :background "navy"
       ;;:weight bold
       :underline nil
       ))
  "The face used for representing the visited link to the file")



(defface efar-file-current-face
  '((t :foreground "black"
       :background "cadet blue"
       ;;:weight bold 
       :underline nil
       ))
  "The face used for representing the visited link to the file")


(defface efar-dir-current-face
  '((t :foreground "white"
       :background "cadet blue"
       ;;:weight bold
       :underline nil
       ))
  "The face used for representing the visited link to the file")

(defface efar-marked-face
  '((t :foreground "gold"
       :background "navy"
       ;;:weight bold
       :underline nil
       ))
  "The face used for representing the visited link to the file")


(defface efar-dir-name-face
  '((t :foreground "white"
       :background "navy"
       ;;:weight bold 
       :underline nil
       ))
  "The face used for representing the visited link to the file")

(defface efar-header-face
  '((t :foreground "orange"
       :background "navy"
       ;;:weight bold 
       :underline nil
       ))
  
  "The face used for representing the visited link to the file")

(defface efar-dir-name-current-face
  '((t :foreground "navy"
       :background "bisque"
       ;;:weight bold 
       :underline nil
       ))
  "The face used for representing the visited link to the file")



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
  
  
