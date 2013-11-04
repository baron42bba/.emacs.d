#!/usr/bin/env python
# -*- Mode: Python; tab-width: 4 -*-
#
# SUMMARY:      Produces Lisp lists of Python paths, pydoc keywords, modules and help topics.
# USAGE:        l = Pydoc_Lisp()
# KEYWORDS:     doc, emacs, lisp, pydoc
#
# AUTHOR:       Bob Weiner
# ORG:          Deepware
#
# ORIG-DATE:    18-Apr-01 at 12:08:46
# LAST-MOD:     22-Apr-01 at 11:03:03 by Bob Weiner
#
# COPYRIGHT:    Copyright (C) 2001  Bob Weiner
#   Available for use and distribution under the terms of the Python license 2.0 or greater.
#
# DESCRIPTION:  
"""
Produces Lisp lists of Python paths, pydoc keywords, modules and help topics.
See the doc for class Pydoc_Lisp for details.  This provides the interface from
pydoc to the Emacs Lisp pydoc commands.
"""
# DESCRIP-END.

## ------------------------------------------------------------------------
## Required modules
## ------------------------------------------------------------------------

from string import find, join
import pydoc
import sys

## ------------------------------------------------------------------------
## Globals
## ------------------------------------------------------------------------

global pydoc_lisp_alist

def pydoc_output_lisp():
    try:
        if isinstance(pydoc_lisp_alist, Pydoc_Lisp):
            # Reinitialize the lists
            pydoc_lisp_alist.__init__()
        else:
            pydoc_lisp_alist = Pydoc_Lisp()
    except NameError:
        pydoc_lisp_alist = Pydoc_Lisp()


## ------------------------------------------------------------------------
## Classes
## ------------------------------------------------------------------------

# This is defined as a class so that it may be easily added to pydoc.py
# rather than keeping it in this separate module.
class Pydoc_Lisp:
    """
    Produces Lisp lists of Python paths, pydoc keywords, modules and help topics.
    Results are stored in the class variables: keywords, modules, topics
    and alist, the latter being an association list containing each of the former
    three values.  An instance prints as the value of `alist'.  Call self.__init__()
    if there is a need to reset the value of the module list.
    """

    ## Class variables
    keywords = modules = paths = topics = alist = None

    def __init__(self):
        Pydoc_Lisp.keywords = self._dict_to_lisp_list(pydoc.Helper.keywords)
        Pydoc_Lisp.modules = self._get_modules_lisp()
        Pydoc_Lisp.paths = self._get_paths_lisp()
        Pydoc_Lisp.topics = self._dict_to_lisp_list(pydoc.Helper.topics)
        Pydoc_Lisp.alist = '(("keywords" . %s)\n ("modules" . %s)\n ("paths" . %s)\n ("topics" . %s))' \
                           % (Pydoc_Lisp.keywords, Pydoc_Lisp.modules, \
                              Pydoc_Lisp.paths, Pydoc_Lisp.topics)
        # Print the alist so the Lisp reader can evaluate it.
        print Pydoc_Lisp.alist

    def __str__(self):
        return Pydoc_Lisp.alist

    def _dict_to_lisp_list(self, dict):
        keys = dict.keys()
        keys.sort()
        return '(("%s"))\n' % join(keys, '") ("')

    def _get_modules_lisp(self):
        "Return a Lisp list of available Python module and package names."
        module_dict = {}

        def callback(path, modname, desc, module_dict=module_dict):
            if modname and modname[-9:] == '.__init__':
                modname = 'Pkg-' + modname[:-9]
            if find(modname, '.') < 0:
                module_dict[modname] = 1

        pydoc.ModuleScanner().run(callback)
        return self._dict_to_lisp_list(module_dict)

    def _get_paths_lisp(self):
        paths = sys.path[:]
        try:
            paths[paths.index('')] = '.'
        except ValueError:
            pass
        return '(("%s"))\n' % join(paths, '") ("')

## ------------------------------------------------------------------------
## Program execution
## ------------------------------------------------------------------------

if __name__ == '__main__':
    l = Pydoc_Lisp()
