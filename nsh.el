;;; nsh.el --- create a new shell in a named buffer
;;; Keep separate histories for each shell in ~/Dropbox/nsh_history
;;;
;;; Questions: Paul.Maragakis@gmail.com
;;;
;;; Commentary:
;;; This is a tool that I've used for over a decade.
;;; The only part that is no longer meaningful to me is syncing via Dropbox,
;;; but the code works as expected with any bash-history-dir.
;;;

;;; Code:

(require 'shell)


;; Customization helps select useful buffers for default split functionality.
(defgroup nsh-customization
  '(
    (nsh-bash-executable custom-variable) ;; location of bash executable
    (nsh-bash-history-dir custom-variable) ;; directory that contains all past shell histories
    )
  "Customization group for split-frame.el."
  :group 'convenience
  :prefix "split-frame")

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

;; We will keep all past histories in directory bash-history-dir
;; The default location is a personal choice that is customizable
(defcustom nsh-bash-history-dir
  (or
   (getenv "NSHELLDIR")
   (expand-file-name-if-dir-accessible "~/Dropbox/nsh_history/")
   (expand-file-name-if-dir-accessible "~/Dropbox/.nsh_history/")
   (expand-file-name-if-dir-accessible "~/.emacs.d/nsh_history/")
   (expand-file-name-if-dir-accessible "~/.emacs.d/.nsh_history/")
   (file-name-as-directory (expand-file-name "~/.nsh_history/")))
  "The directory that contains all past shell histories."
  :group 'nsh-customization
  :type '(string))

;; ;; In all my non-windows machines, bash is here:
;; (defconst bash-executable "/bin/bash")


;; (defconst bash-history-dir
;;   (file-name-as-directory (expand-file-name "~/Dropbox/nsh_history/")))

;; create history directory, if it doesn't already exist.
(condition-case err
    ;; first test if directory exists and is readable
    (or (file-accessible-directory-p nsh-bash-history-dir)
	(and (file-exists-p nsh-bash-history-dir)         ; exists, but not readable
	     (set-file-modes nsh-bash-history-dir ?\755)) ;   fix permissions
	(make-directory nsh-bash-history-dir))            ; it wasn't there, make it
  ('error (message "From: %s --> %s"                  ; for the unknown error...
		   load-file-name (error-message-string err))))

;; Implementation of nsh, assuming bash-history-dir exists
;; In addition to creating a shell, nsh sets a bunch of bash-history variables
(defun nsh (nsh-name)
  "Create a new shell in a named buffer NSH-NAME with separate history."
  ;; auto fill the interactive prompt with the names of past shells, but
  ;; exclude files starting with "." --- this excludes ".", ".."
  (interactive (list
                (completing-read "shell name: "
                                 (directory-files nsh-bash-history-dir
                                                  nil "^[^\.]"))))
  ;; Set these environment variables for the bash shell that follows
  ;; If all else fails, please set these variables within your shell.
  (setenv "HISTFILE"
          (concat nsh-bash-history-dir nsh-name))
  (setenv "HISTSIZE" "50000")              ; recall up to 50K commands per shell
  ;; Note that setting HISTSIZE above also sets comint-input-ring-size to 50K.
  (setenv "HISTFILESIZE" "1000000")        ; save all history
  (setenv "HISTCONTROL" "ignoredups")      ; ignore repeated entries
  (setenv "HISTIGNORE" "ls:ls -lrt:bg:fg") ; ignore ls, bg, fg
  (setenv "HISTTIMEFORMAT" "%Y-%m-%d %T ") ; show history times
  ;;(setenv "HISTTIMEFORMAT" "%F %T ")       ; show history times
  (setenv "PROMPT_COMMAND" "history -a")   ; append history after each command.
  ;; Create the bash shell in the named buffer
  (let ((nsh-buffer (get-buffer-create (format "nsh-%s" nsh-name)))
        (explicit-shell-file-name nsh-bash-executable))
    (shell nsh-buffer))
)

(provide 'nsh)
;;; nsh.el ends here
