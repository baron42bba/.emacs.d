magit-annex adds a few git annex operations to the magit interface.
Most features are present under the annex popup menu, which is bound
to "@". This key was chosen as a leading key mostly to be consistent
with John Wiegley's git-annex.el [1], but also because there aren't
too many single letters available in the magit keymap.

Adding files:
  @a   Add a file in the status buffer to annex.
       Behaves similarly to staging a file with 's'.
  @A   Add all untracked files and unstaged changes to annex.
       Behaves similarly to staging all files with 'S'.

Managing file content:
  @fu   Unlock a file.
  @fl   Lock a file.
  @fg   Get a file.
  @fd   Drop a file.
  @fc   Copy a file.
  @fm   Move a file.

  @eg   Get all files.
  @ed   Drop all files.
  @ec   Copy all files.
  @em   Move all files.

Updating git annex:
  @m   Run `git annex merge'.
  @Pg  Push git annex branch.
  @Pb  Push current and git annex branch.
  @y   Run `git annex sync'.

For working with git annex in dired, see git-annex.el [1].

[1] https://github.com/jwiegley/git-annex-el
