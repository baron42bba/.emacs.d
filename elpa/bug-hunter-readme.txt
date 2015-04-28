The Bug Hunter is an Emacs library that finds the source of an error or
unexpected behavior inside an elisp configuration file (typically
`init.el' or `.emacs').

Usage Examples
==============

  If your Emacs init file signals an error during startup, but you don’t
  know why, simply issue
  ,----
  | M-x bug-hunter-init-file RET RET
  `----
  and The Bug Hunter will find it for you. Note that your `init.el' (or
  `.emacs') must be idempotent for this to work.

  If Emacs starts up without errors but something is not working as it
  should, invoke the same command, but give it in an assertion.
  Essentially, if you can write a snippet that detects the issue and
  returns non-nil, just provide this snippet as the assertion and the
  Bug Hunter will do a bisection search for you.

  For example, let’s say there’s something in your init file that’s
  loading the `cl' library, and you don’t want that. You /know/ you’re
  not loading it yourself, but how can you figure out which external
  package is responsible for this outrage?

  ,----
  | M-x bug-hunter-init-file RET (featurep 'cl) RET
  `----

  *That’s it!* You’ll be given a nice buffer reporting the results:

  - Are you getting obscure errors when trying to open /“.tex”/ files?
    - Don’t despair! Just use `(find-file "dummy.tex")' as the
      assertion.
  - Did `ox-html' stop working due to some arcane misconfiguration?
    - Just write an assertion that does an export and checks the result.
  - Does some random command suddenly bind itself to `C-j' and you can’t
    figure out why?
    - `(eq (key-binding "\n") 'unwanted-command)' is the assertion for
      you!

  Finally, you can also use `bug-hunter-file' to hunt in other files.

