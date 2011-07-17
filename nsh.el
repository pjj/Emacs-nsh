;;;; Create a new shell in a named buffer
;;;; Keep separate histories for each shell in ~/Dropbox/nsh_history
;;;; 
;;;; Questions: Paul.Maragakis@gmail.com

(require 'shell)

;; In all my non-windows machines, bash is here:
(defconst bash-executable "/bin/bash")

;; We will keep all past histories in directory bash-history-dir
(defconst bash-history-dir 
  (file-name-as-directory (expand-file-name "~/Dropbox/nsh_history/")))

;; create history directory, if it doesn't already exist.
(condition-case err 
    ;; first test if directory exists and is readable
    (or (file-accessible-directory-p bash-history-dir) 
	(and (file-exists-p bash-history-dir)         ; exists, but not readable
	     (set-file-modes bash-history-dir ?\755)) ;   fix permissions
	(make-directory bash-history-dir))            ; it wasn't there, make it
  ('error (message "From: %s --> %s"                  ; for the unknown error...
		   load-file-name (error-message-string err))))

;; Implementation of nsh, assuming bash-history-dir exists
;; In addition to creating a shell, nsh sets a bunch of bash-history variables
(defun nsh (nsh-name)
  "Create a new shell in a named buffer with separate history"
  ;; auto fill the interactive prompt with the names of past shells, but
  ;; exclude files starting with "." --- this excludes ".", ".."
  (interactive (list 
                (completing-read "shell name: " 
                                 (directory-files bash-history-dir 
                                                  nil "^[^\.]"))))
  ;; Set these environment variables for the bash shell that follows
  (setenv "HISTFILE" 
          (concat bash-history-dir nsh-name))
  (setenv "HISTFILESIZE" "1000000")        ; save all history
  (setenv "HISTCONTROL" "ignoredups")      ; ignore repeated entries
  (setenv "HISTIGNORE" "ls:ls -lrt:bg:fg") ; ignore ls, bg, fg
  (setenv "HISTTIMEFORMAT" "%F %T ")       ; show history times
  ;; Create the bash shell in the named buffer
  (let ((nsh-buffer (get-buffer-create (format "nsh-%s" nsh-name)))
        (explicit-shell-file-name bash-executable))
    (shell nsh-buffer))
) 

(provide 'nsh)
