CFEngine3 Mode for YASnippet
============================

If you use emacs to write CFEngine 3 promise files, I think you will
like yasnippet-cfengine.  It is a collection of snippets (templates)
for YASnippet.  I wrote it to scratch my own itch.

Installation
------------

First, you need YASnippet in your emacs environment.  You can use

```
M-x package-install<ENTER>yasnippet<ENTER>
```

to install it.  You will need to enable package installation in your
`~/.emacs` file first.  Search Google if you don't know how to do
this.  Searching "emacs package-install" should do it.

I have globally enabled YASnippet in my `~/.emacs`.

```
(require 'yasnippet)
(yas-global-mode 1)
```

Next, cd where your snippets are saved.  Mine are
`~/.emacs.d/elpa/yasnippet-VERSION/snippets/`.  Clone the repository.
Rename the directory to match the yasnippet convention of matching the
mode in which the snippets are used.

```
cd ~/.emacs.d/elpa/yasnippet*/snippets
git clone https://github.com/jeffcarlson72/emacs-yasnippet-cfengine3-mode.git
mv {emacs-yasnippet-,}cfengine3-mode
```

Finally, restart emacs or reload you ~/.emacs file.

Usage
-----

When you are writing CFEngine promises, at the highest level, you will
either need to create bundles or bodies.  There are four types of
bundles, just type one of these and hit the TAB key:

- agent
- edit_line
- monitor
- server

This will expand into a template containing all the promise types
available for that bundle.  (The following underscore shows where the
cursor will appear.)

```
bundle agent _
{
meta:
vars:
...
```

The promise types are in standard order of execution.  Type in the
name for your bundle, then move down to a line indicating the promise
type you want to add.  Hit ENTER to add a new line, then type the
promise type again, followed by the TAB key.

From this:

```
vars:
vars[TAB]
```

To this:

```
vars:
"_"
action
classes
comment
...
```

Name the promiser, add "-> promisee" if necessary.  Then move the
cursor down to the end of one of the promise attributes and hit TAB.
It will expand, adding the rocket (=>) and a template for that
attribute.  Strings will add quotes, lists will add braces, any
attribute which is a "one of X, Y, or Z" type will create a drop-down
with those options.  Just use the arrow keys and ENTER key to pick
one.  Attributes which take bodies as their values will expand with a
menu of options from the common promise library.  If you want to build
your own body, hit BACKSPACE, which will break out of the snippet
entry.  Hit TAB again to jump to the end of the line, and add a comma
or a semicolon as appropriate.

```
        comment => "_"
```

After you have built your promise, you will need to manually delete
any attributes you don't use.  Likewise, delete any promise types you
don't use in your bundle.  I know this means you wind up deleting a
lot of superfluous text, but I felt it was nicer than having to
remember what every single one of the options was.  In other words,
C-k is easier to remember than all that other text.

Since a lot of bodies have the same name as related attributes, to
build a body, preface it with "body-."

From this:

```
body-contain[TAB]
```

To this:

```
body contain _
{
...
```

Again, fill in the template just as described for bundles.  Delete
what you don't need from the template.  C-k is your friend.

Change Log
----------

2021-05-14 - Nick Anderson contributed missing functions, bringing this up to date with 3.18.0a.
