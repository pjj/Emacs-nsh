

Learn to M-x shell in emacs buffers and there is no going back to terminals!

Emacs-nsh adds amazing features to shells within buffers.  Name your
shells, and it will automatically keep separate histories for each of
your named shells.  Get Dropbox, and it will allow you to access all
your named-shell histories from all of your synced machines (or use
git to automagically store and sync them).  I've used nsh for years,
and it has saved me tons of time (it only gets better over time).
With a simple rg in your nsh_history directory you can find commands
you issued years ago.

Installation instructions:

1. Download nsh.el to your directory of elisp hacks, e.g. ~/.emacs.d/lisp/var/

2. Add the following line to your emacs init (or .emacs) file:

```
(load "/full_path_to_my_elisp/nsh.el")
```

2b. Alternatively, you can try quelpa with use-package.  Here is the
section in my own init.el

```
;; Emacs, please load my own powerful nsh function.
;; This is probably the command I typed the most times in emacs,
;; and eventually I decided to also assign a single key to it.
;; The function is simple: open a named bash shell, like M-x shell,
;; that uses the comint functionality of Emacs, and that keeps a
;; named history of every single command.  It also enables autocompletion
;; of past named shells, and continues their history from where it left off.
;; Together with a quick rg on the directory with all the shell histories,
;; this package has enabled a memory of past shell commands for about 15 years
;; I only kept the dates at a later time, so the early entries, although valid,
;; probably aren't as easy to sort through chronologically.
;;
(use-package nsh
  :quelpa ((nsh :fetcher github :repo "pjj/Emacs-nsh") :upgrade t)
  :bind (("<f9>" . nsh)))
```

(defvar user-elisp-directory (expand-file-name "~/.emacs.d/lisp"))
(add-to-list 'load-path (concat user-elisp-directory "/var"))

(use-package nsh
  :bind (("<f9>" . nsh)))


3. Optionally, costomize the nsh variables, especially: Nsh Bash History Dir,
if you want to use a non-standard location for the histories.
The default location is ~/.nsh_history/

4. Optionally, customize the variable project-switch-commands
and add an entry for nsh calling the function nsh-in-project.
I typically bind it to the key 'n', so my customization entry is:
(nsh-in-project "Nsh" 110)


Usage:

Start a new named shell with: M-x nsh (Esc-x nsh; or bind to <f9>).  
Enter the shell's name in the minibuffer.  
You can auto-complete the name using your favorite completion solution; 
nsh will look up the names of all your existing histories).
