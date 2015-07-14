This plug-in provides support for an additional default remote
which when pushing is used instead of the "merge" default specified
by the git-config(1) option `branch.<name>.remote'.

Together `branch.<name>.remote' and `branch.<name>.merge' set the
default used by git-pull(1) and git-push(1).  Like their git
counterparts `magit-push' and `magit-pull' use these options. So
does `magit-status' which displays commits not pushed to or not
pulled from the default remote/branch.

This works nicely if commits most often flow like this:

  +------------+            +------------+
  |   remote   | -- pull -> |   local    |
  |    repo    | <- push -- |    repo    |
  +------------+            +------------+

But it is inconventient if commits most often flow through your
local repository like this:

  +------------+            +------------+            +------------+
  |  upstream  | -- pull -> |   local    |            |    your    |
  |    repo    |            |    repo    | -- push -> |   public   |
  |            |            +------------+            |    repo    |
  |            | <- merge pull reguest -------------- |            |
  +------------+                                      +------------+

This package modifies Magit to automatically detect whether the
latter workflow is used; and if so provide additional information
related to that "personal" or "push" remote and push to it by
default.

When `magit-push-remote-mode' is turned on and the repository has a
push-remote `magit-push' and `magit-push-tags' now by default push
to the push-remote, and `magit-status' shows information related to
both the push and pull (Git's default) remote.

This is done by REDEFINING `magit-push-tags' and adding specialized
functions to the hooks `magit-status-sections-hook' and
`magit-push-hook'.

To enable this turn on the global `magit-push-remote-mode' and
select the push-remote either per repository or globally using the
git variable `magit.pushremote'.

  (magit-push-remote-mode 1)

  git config --global magit.pushremote <REMOTE_NAME>  # or
  git config magit.pushremote <REMOTE_NAME>

Git already provides a variable for the same purpose, which is
also supported, but note that setting that also Git, but at least
currently does not affect regular Magit push commands

  git config --global remote.pushdefault <REMOTE_NAME>  # or
  git config remote.pushdefault <REMOTE_NAME>
