;;; nsh.el --- create a new shell in a named buffer
;;; Keep separate histories for each bash shell in you nsh-bash-history-dir
;;;
;;; Questions: Paul.Maragakis@gmail.com
;;;
;;; Commentary:
;;; This is a tool that I've used for over a decade.
;;;

;;; Code:

(require 'shell)
(require 'cl-lib)
(require 'project)

;; Customization helps select useful buffers for default split functionality.
(defgroup nsh-customization
  '(
    (nsh-bash-executable custom-variable) ;; location of bash executable
    (nsh-bash-history-dir custom-variable) ;; directory that contains all past shell histories
    (nsh-common-env custom-variable)
    (nsh-mode-unload-hook custom-variable)
    (nsh-mode-hook custom-variable)
    )
  "Customization group for nsh."
  :group 'convenience
  :prefix "split-frame")

(defcustom nsh-mode-unload-hook nil
  "A hook that gets run when `nsh-mode' is unloaded."
  :type 'hook
  :group 'nsh-customization)

(defcustom nsh-mode-hook nil
  "A hook that gets run when `nsh-mode' is entered."
  :type 'hook
  :group 'nsh-customization)

(defcustom nsh-bash-executable
  "/bin/bash"
  "The location of bash for nsh to start."
  :group 'nsh-customization
  :type '(string))


(defun expand-file-name-if-dir-accessible (fname)
  "Expand FNAME if it is the name of an accessible directory; otherwise nil."
  (let* ((a (file-name-as-directory (expand-file-name fname)))
	 (b (file-accessible-directory-p a)))
    (when b a)))

;; We will keep all past histories in directory nsh-bash-history-dir
;; The default location is a personal choice that is customizable
(defcustom nsh-bash-history-dir
  (or
   (getenv "NSHDIR")
   (expand-file-name-if-dir-accessible "~/Dropbox/nsh_history/")
   (expand-file-name-if-dir-accessible "~/Dropbox/.nsh_history/")
   (expand-file-name-if-dir-accessible "~/.emacs.d/nsh_history/")
   (expand-file-name-if-dir-accessible "~/.emacs.d/.nsh_history/")
   (file-name-as-directory (expand-file-name "~/.nsh_history/")))
  "The directory that contains all past shell histories (name should end in /)."
  :group 'nsh-customization
  :type '(string))

;; Custom environment association list.
;; Every new shell sets these common history-related variables
;; and then also sets a different HISTFILE.
;; If you need any additional overrides to the default bash,
;; you can add them to this list.
(defcustom nsh-bash-common-env
  '(("HISTSIZE" . "50000")              ; recall up to 50K commands per shell
    ("HISTFILESIZE" . "1000000")        ; save all history
    ("HISTCONTROL" . "ignoredups")      ; ignore repeated entries
    ("HISTIGNORE" . "ls:ls -lrt:bg:fg") ; ignore ls, bg, fg
    ("HISTTIMEFORMAT" . "%Y-%m-%d %T ") ; show history times
    ("PROMPT_COMMAND" . "history -a")   ; append history after each command.
    )
  "The common environment in the shells."
  :group 'nsh-customization
  :type '(alist :key-type (string :tag "Key")
                :value-type (string :tag "Value")))

;; We use setenv in a 'cl-loop for' to assign all pairs of environment variables.
(defun nsh-env-setup (nsh-name)
  "Setup the common environment of nsh NSH-NAME."
  (let ()
    (cl-loop for (key . value) in nsh-bash-common-env do
	   (setenv key value))
    (setenv "HISTFILE"
            (concat nsh-bash-history-dir nsh-name))))

;; Implementation of nsh, assuming nsh-bash-history-dir exists
;; First select or find a shell name, posssibly reusing an old one,
;; then set a bunch of bash-history variables and create the shell.
;; If an nsh with this name already exists, simply select that buffer.
(defun nsh (nsh-name)
  "Create a new shell in a named buffer NSH-NAME with separate history."
  ;; auto fill the interactive prompt with the names of past shells, but
  ;; exclude files starting with "." --- this excludes ".", ".."
  (interactive (list
                (completing-read "shell name: "
                                 (directory-files nsh-bash-history-dir
                                                  nil "^[^\.]"))))
  ;; Set these environment variables for the bash shell that follows
  ;; Create the bash shell in the named buffer
  (let ((nsh-buffer (get-buffer-create (format "nsh-%s" nsh-name)))
        (explicit-shell-file-name nsh-bash-executable))
    (nsh-env-setup nsh-name)
    (let ((buf (shell nsh-buffer)))
      (unless (derived-mode-p 'nsh-mode) (nsh-mode))
      buf)))

;; Because project.el has become rather useful over the last decade,
;; I provide here an implementation of nsh-in-project, which starts
;; an automatically named shell in the root directory of the project.
;; I typically customize project-switch-commands and add nsh-in-project.
;;###autoload
(defun nsh-in-project ()
  "Create a new shell in project with default name and separate history."
  (interactive)
  (require 'project)
  (let* ((default-directory (project-root (project-current t)))
	 (proj-name (file-name-nondirectory
		     (directory-file-name default-directory)))
	 (nsh-name (format "proj-%s" proj-name)))
    (nsh nsh-name)))

;; create history directory, if it doesn't already exist.
(condition-case err
    ;; first test if directory exists and is readable
    (or (file-accessible-directory-p nsh-bash-history-dir)
	(and (file-exists-p nsh-bash-history-dir)         ; exists, but not readable
	     (set-file-modes nsh-bash-history-dir ?\700)) ; fix permissions (shells are personal)
	(make-directory nsh-bash-history-dir))            ; it wasn't there, make it
  ('error (message "From: %s --> %s"                  ; for the unknown error...
		   load-file-name (error-message-string err))))

;; bookmark support:
;; we defined a derived mode that populates bookmark-make-record-function
;; Other than that, we are fine with the defaults of shell-mode.
;;;###autoload
(define-derived-mode nsh-mode shell-mode "Nsh"
  "Named bash interactive mode."
  (setq-local nsh-mode t)
  (setq-local bookmark-make-record-function #'nsh-bookmark-make-record)
  (setq-local list-buffers-directory (expand-file-name default-directory)))

(put 'nsh-mode 'mode-class 'special)

(declare-function bookmark-prop-get "bookmark" (bookmark prop))

(defun nsh-bookmark-name ()
  "Return a name for the bookmark."
  (buffer-name))

(defun nsh-bookmark-make-record ()
  "Create a bookmark for the current Nsh buffer."
  (let ((nsh-name (substring (buffer-name) 4 nil))) ;; drop nsh- from buffer name
    `(,(nsh-bookmark-name)
      (location . ,default-directory)
      (name . ,nsh-name) ;; drop nsh- from buffer name
      (handler . nsh-bookmark-jump))))

;;;###autoload
(defun nsh-bookmark-jump (bookmark)
  "Default bookmark handler for BOOKMARK of nsh buffers."
  (let ((default-directory (bookmark-prop-get bookmark 'location))
	(nsh-name (bookmark-prop-get bookmark 'name)))
    (nsh nsh-name)))

(put 'nsh-bookmark-jump 'bookmark-handler-type "Nsh")

(provide 'nsh)
;;; nsh.el ends here
