[![Build Status](https://github.com/tombonan/emacs-git-open/workflows/CI/badge.svg)](https://github.com/tombonan/emacs-git-open/actions)

# emacs-git-open

Emacs package to open a file on a git remote. Inspired by [git-open](https://github.com/paulirish/git-open)

## Usage

### Interactive File Functions

From the current buffer, call the function to open file on remote to open in browser:

```el
M-x git-open
```

To simply copy the remote file path to clipboard, call:

```el
M-x git-open-copy
```

To view the blame view on the remote, call:
```el
M-x git-open-blame
```

`git-open-blame` also has a copy function instead of opening in browser: `git-open-blame-copy`.

Both `git-open` and `git-open-blame`, and their respective copy functions, will open the file by default,
but if a region is selected, then that range of lines will be highlighted on the remote.


### Interactive Functions from `magit-blame` View


`emacs-git-open` similarly allows you to open the current commit from a `magit-blame` buffer:

```el
M-x git-open-commit
```

<img src="./git-open-commit-usage.gif" alt="Git Open Commit Usage" width="500" height="600">

### Default Branches

My typical workflow involves wanting to open up remote files from the default remote branch, either
to link them to others or to view unmodified files.

By default, this branch is assumed to be `main`, but it can also be configured on a per-project
basis by editing `.git/config`:

```bash
[init]
    defaultBranch = some-default
```

### Todo
- Support remote targets other than 'origin'
- Support other git hosting services (gitlab, bitbucket etc.)
- Tests
- Packaging for MELPA
