A package to integrate GNU Global source code tagging system
(http://www.gnu.org/software/global) with Emacs.

Usage:

`ggtags' is similar to the standard `etags' package. These keys
`M-.', `M-,', `M-*' and `C-M-.' should work as expected in
`ggtags-mode'. See the README in https://github.com/leoliu/ggtags
for more details.

All commands are available from the `Ggtags' menu in `ggtags-mode'.

NEWS 0.8.10 (2015-06-12):

- Tags update on save is configurable by `ggtags-update-on-save'.
- New command `ggtags-explain-tags' to explain how each file is
  indexed in current project.
- New user option `ggtags-sort-by-nearness' that sorts matched tags
  by nearness to current directory.

See full NEWS on https://github.com/leoliu/ggtags#news
