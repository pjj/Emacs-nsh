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

## Table of Contents

- [Installation](#installation)
- [Customization](#customization)
- [Usage](#usage)
- [Session Management](#session-management)
- [Searching Shell Histories](#searching-shell-histories)
- [Bookmarks](#bookmarks)
- [Project Integration](#project-integration)
- [Key Bindings](#key-bindings)
- [Limitations](#limitations)
- [Inspiration / Credits](#inspiration--credits)

## Installation

### Direct Download

Download `nsh.el` to your directory of elisp hacks, e.g. `~/.emacs.d/lisp/var/`,
and add the following line to your Emacs init (or .emacs) file:

```elisp
(load "/full_path_to_my_elisp/nsh.el")
```

### Using quelpa with use-package

```elisp
(use-package nsh
  :quelpa ((nsh :fetcher github :repo "pjj/Emacs-nsh") :upgrade t)
  :bind (("<f9>" . nsh)))
```

## Customization

Look at the customization of `nsh-bash-history-dir`
if you want to use a non-standard location for the histories.
The default location is `~/.nsh_history/`.

The history directory is resolved in the following order:

1. The `NSHDIR` environment variable (if set)
2. `~/Dropbox/nsh_history/` or `~/Dropbox/.nsh_history/` (if accessible)
3. `~/.emacs.d/nsh_history/` or `~/.emacs.d/.nsh_history/` (if accessible)
4. `~/.nsh_history/` (fallback default, created automatically)

Key customization variables (all under `M-x customize-group RET nsh-customization`):

| Variable | Description | Default |
|---|---|---|
| `nsh-bash-executable` | Path to bash executable | `nil` (uses `shell-file-name`) |
| `nsh-bash-history-dir` | Directory for shell histories | See resolution order above |
| `nsh-bash-common-env` | Environment variables set in every shell | History-related defaults (see below) |
| `nsh-session-file` | File for saving/restoring sessions | `~/.emacs.d/nsh-sessions.el` |
| `nsh-session-save-on-exit` | Prompt to save sessions when quitting Emacs | `t` |

The default environment (`nsh-bash-common-env`) configures:

- `HISTSIZE` = 50000 (recall up to 50K commands per shell)
- `HISTFILESIZE` = 1000000 (save all history)
- `HISTCONTROL` = ignoredups
- `HISTIGNORE` = ls:ls -lrt:bg:fg:history
- `HISTTIMEFORMAT` = `%Y-%m-%d %T ` (show timestamps)
- `PROMPT_COMMAND` = `history -a` (append after each command)

## Usage

Start a new named shell with: `M-x nsh` (`Esc-x nsh`; or bind to `<f9>`).
Enter the shell's name in the minibuffer.
You can auto-complete the name using your favorite completion solution;
nsh will look up the names of all your existing histories.

If a shell with that name already exists, nsh switches to its buffer.
Each shell buffer is named `nsh-{name}` and has its own `HISTFILE`
at `nsh-bash-history-dir/{name}`.

## Session Management

Nsh can save and restore all live shell sessions across Emacs restarts.

| Command | Description |
|---|---|
| `M-x nsh-save-sessions` | Save all live nsh buffers (names and directories) to `nsh-session-file` |
| `M-x nsh-restore-sessions` | Recreate shells from a previously saved session file |
| `M-x nsh-save-and-kill` | Save sessions and then kill all nsh buffers |

When `nsh-session-save-on-exit` is non-nil (the default), Emacs will
prompt you to save nsh sessions before exiting if any are active.

## Searching Shell Histories

### From within Emacs (ngrep function)

If you install the [rg](https://github.com/dajva/rg.el) package,
you can use the provided `ngrep` function (`M-x ngrep`).
The `ngrep` function puts the output in a buffer called
`*ngrep*` that has the normal functionality of a `*rg*` buffer.
This is useful for quickly jumping to the relevant section
of the history of a particular shell.

### From the command line (ngrep script)

Optionally, add a `ngrep` command to your `PATH` to search
your histories of named shells from the terminal:

```bash
cat ~/bin/ngrep
#!/bin/bash --posix

# Sort results in order of last modified (newest first)
rg $1 --sort modified ~/.nsh_history/*${2}*
```

## Bookmarks

Bookmarks of nsh shells work as expected.
If you use `burly-bookmark-windows` your shell
will reopen in the current working directory
with its past history.

## Project Integration

For optimal interaction with `project.el`, customize the variable
`project-switch-commands` and add an entry for nsh calling the
function `nsh-in-project`.  This opens a shell automatically named
`proj-{project-name}` at the project root.

I typically bind it to the key `n`, so my customization entry is:

```elisp
(nsh-in-project "Nsh" 110)
```

## Key Bindings

In `nsh-mode` buffers:

| Key | Command | Description |
|---|---|---|
| `C-c C-k` | `nsh-save-and-kill` | Save all sessions and kill all nsh buffers |

## Limitations

Nsh works with TRAMP as expected: the records are stored on the remote machine.
However, it does not work as expected when the user issues ssh from a local nsh.

## Inspiration / Credits

The original inspiration came from Joe Bank's package nshell,
a package for using named shells with saved histories within Emacs.
I can no longer live without keeping shell histories forever.
