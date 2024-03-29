# nsh -- named bash shells in Emacs

Learn to M-x nsh in Emacs buffers and there is no going back to terminals!

Nsh adds amazing features to shells within buffers.  Name your shells,
and it will automatically keep separate histories for each of your
named shells.  Get Dropbox, and it will allow you to access all your
named-shell histories from all of your synced machines (or use git 
to automagically store and sync them). Bookmark your nsh and restore it
at the correct directory. Drop an nsh at your project's root. I've
used nsh for years, and it has saved me tons of time (it only gets
better over time).  With a simple rg in your nsh_history directory you
can find commands you issued years ago. Together with burly-open-bookmarks
you can restore your shell window configurations.

## Installation instructions:

1. Download `nsh.el` to your directory of elisp hacks, e.g. `~/.emacs.d/lisp/var/`
and add the following line to your Emacs init (or .emacs) file:

```
(load "/full_path_to_my_elisp/nsh.el")
```

1b. Alternatively, you can use quelpa with use-package.

```
(use-package nsh
  :quelpa ((nsh :fetcher github :repo "pjj/Emacs-nsh") :upgrade t)
  :bind (("<f9>" . nsh)))
```

2. Look at the customization of `nsh-bash-history-dir`
if you want to use a non-standard location for the histories.
The default location is ~/.nsh_history/

3. For optimal interaction with project.el, you can 
customize the variable `project-switch-commands`
and add an entry for nsh calling the function nsh-in-project.
I typically bind it to the key 'n', so my customization entry is:
`(nsh-in-project "Nsh" 110)`

4. Optionally, add the useful command `ngrep` to your `PATH`, to search
your histories of named shells (optionally only those with a pattern):
```
bash$ cat ~/bin/ngrep
#!/bin/bash --posix

# Sort results in order of last modified (newest first)
rg $1 --sort modified ~/.nsh_history/*${2}*
```

4a. If you install the package `rg`, 
you could use the provided function `ngrep` instead.
The `ngrep` function puts the output in a buffer called
`*ngrep*` that has the normal functionality of a `*rg*` buffer.
I often use this function to quickly jump to the relevant secction 
of the history of a particular shell.

5. Bookmarks of shells work as expected.
If you use `burly-bookmark-windows` your shell
will reopen in the current working directory
with its past history.


## Usage:

Start a new named shell with: `M-x nsh` (`Esc-x nsh`; or bind to `<f9>`).
Enter the shell's name in the minibuffer.
You can auto-complete the name using your favorite completion solution; 
nsh will look up the names of all your existing histories).


## Inspiration / credits

The original inspiration came from Joe Bank's package nshell
a package for using named shells with saved histories within Emacs.
I can no longer live without keeping shell histories forever.


## Limitations:

nsh works with tramp as expected: the records are stored on the remote machine.
However, it does not work as expected when the user issues ssh from a local nsh.


