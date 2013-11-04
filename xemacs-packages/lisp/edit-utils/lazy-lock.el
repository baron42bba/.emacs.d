;;; lazy-lock.el --- Lazy demand-driven fontification for fast Font Lock mode.

;; Copyright (C) 1994, 1995 Free Software Foundation, Inc.
;; Copyright (C) 2000, 2003 Ben Wing.

;; Author: Ben Wing <ben@xemacs.org>
;; Original Author: Simon Marshall <simon@gnu.org>
;; Maintainer: XEmacs Development Team
;; Keywords: faces files

;;; This file is part of XEmacs.

;; XEmacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; XEmacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with XEmacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Synched up with: FSF 21.2.  Heavily, heavily modified.

;;; Commentary:

;; This version of Lazy Lock has been rewritten for XEmacs by Ben Wing.
;; The FSF version (2.11, as of FSF 21.2) supports GNU Emacs only, and
;; relies on C support that is extremely kludgy (three different hooks:
;; `window-scroll-functions', `window-size-change-functions', and
;; `redisplay-end-trigger-functions', and an additional need to hook onto
;; `before-change-functions') and not supported by XEmacs.  This version
;; uses `pre-idle-hook' instead.

;; Simon Marshall has a long diatribe about `pre-idle-hook', noting the
;; fact that it will be called before nearly every redisplay and therefore
;; assuming (wrongly) that this will make a lazy-lock implemented that way
;; extremely slow, as well as complaining about the need to fontify twice
;; as much as necessary, since we don't know the area that will be
;; displayed.  However, (a) under XEmacs, we have an argument `GUARANTEE'
;; to `window-end' that computes the proper value as of the next redisplay,
;; so we don't run into the need to fontify more than necessary (in fact
;; lazy-lock version 2 still has this problem), and (b) given proper
;; optimization checks in the pre-idle-hook, the speed of running the hook
;; does not turn out to be a major factor at all. (It's easy to profile
;; this given the built-in profiling support in XEmacs.) We DO have a
;; problem under some circumstances with scrolling -- in particular,
;; scrolling using the arrow keys, but not the page-up/page-down keys.
;; This is because point moves outside the displayed area, and redisplay
;; will then change the displayed area.  In such a case we do indeed need
;; to kludgily fontify an area twice the size of the window (and that's
;; only a guess).  However, this is at least mitigated by the fact that we
;; can check to determine whether this will happen and only need to take
;; evasive action in those circumstances.

;; Gerd Marshall in FSF 21 finally went and implemented more-or-less
;; non-kludgy C support for fontification.  This involves a hook
;; `fontification-functions', which is triggered by the absence of the
;; `fontified' property on text that redisplay is displaying.  This way,
;; redisplay tells us exactly what needs to be fontified, and there is no
;; need for guessing by trapping all sorts of hooks.  The one thing that
;; seems somewhat kludgy about this interface is that it passes only a
;; single position, not a range, and asks that the function arbitarily
;; fontify a chunk of 400-600 characters, after which it will check again.
;; However, I trust Gerd's sense of design so I assume there was a reason
;; for this.  Perhaps at some point we will implement this support in
;; XEmacs.

;; Finally, for reasons that are not at all clear to me, someone went ahead
;; and created another lazy fontification package for XEmacs (lazy-shot).
;; That package relies on the extent property `initial-redisplay-function',
;; which would not be so bad except that the implementation of this
;; function is broken in that the function is called through an eval event,
;; which is executed *after* redisplay.  Thus, horrible redisplay flashing.
;; To fix this, let the function be called at pre-idle-hook time, or just
;; scrap this stuff entirely and implement `fontification-functions'.

;; (NB Steve claimed that lazy-lock is too slow or something.  However,
;; I used to use it regularly on a Pentium 90 with no problems.)
;;
;; Note: This version is extensively modified from FSF lazy-lock v2.11.
;; Everything related to window-scroll-functions and redisplay end triggers
;; has been removed, as well as the variable `lazy-lock-defer-on-scrolling'
;; and the following functions:
;; 
;; -- `lazy-lock-fontify-after-scroll'
;; -- `lazy-lock-defer-after-scroll'
;; -- `lazy-lock-fontify-after-resize'
;; -- `lazy-lock-arrange-before-change'
;; -- `lazy-lock-fontify-after-trigger'
;; -- `lazy-lock-fontify-line-after-change'
;; -- `lazy-lock-fontify-rest-after-change'
;; -- `lazy-lock-defer-line-after-change'
;; -- `lazy-lock-defer-rest-after-change'
;; -- `lazy-lock-fontify-after-visage'
;; -- `lazy-lock-fontify-conservatively'
;;
;; The `*-after-change' functions have been combined into a single
;; after-change function, based on jit-lock (our model, based on
;; pre-idle-hook, is much closer to jit-lock's than FSF's lazy-lock, in
;; that both we and jit-lock have redisplay support of some sort that
;; guarantees that visible regions will get fontified, without the need for
;; numerous hooks and the collusion of other packages).  The rest of the
;; functions are simply unnecessary.  Some code from lazy-lock v1 is
;; carried over, in particular the code to walk the frames and windows
;; (since pre-idle-hook is called just once and we're not told which
;; windows need updating).  

;; The prime operation of this package can be see in the functions
;; `lazy-lock-fontify-window' and `lazy-lock-after-change'.
;; `lazy-lock-pre-fontify-windows' is also interesting, but mainly just
;; walks through frames and windows, finds windows to fontify and passes
;; them to `lazy-lock-fontify-window'.

;; "Deferring" in this context is different from the deferring that
;; font-lock itself does.

;; Font-lock defers fontification of changes made to a buffer until right
;; before display of that buffer.  This has lots of advantages -- most
;; noteworthy, it in one fell swoop eliminates almost all the problems with
;; excess fontification.  Temporary buffers will never be displayed, so
;; they never will have any fontification done on them.  Multiple changes
;; to a buffer can be batched up -- this is important because there is a
;; lot of overhead to doing even a one-character fontification.  When a
;; function makes a character-at-a-time change, font-lock used to go crazy,
;; but not any more.

;; Lazy-lock (and jit-lock, etc.) deferral (perhaps we should call it
;; "support-mode deferral" refers to deferring fontification not until the
;; next redisplay, but some time later (1/4 of a second, 3 seconds, 30
;; seconds. etc).  This usually happens in the context of the after-change
;; function, where the actual region changed (perhaps enlarged a bit) is
;; fontified and the following text is marked (in some sense) as
;; unfontified and will get fontified later, for example on an idle-timer
;; set to go off 1/4 of a second after idle.  That way, there will be no
;; interference in typing that would happen when you try to immediately
;; fontify the whole rest of the window every single change.

;; --ben

;; Purpose:
;;
;; To make visiting buffers in `font-lock-mode' faster by making fontification
;; be demand-driven and stealthy.
;; Fontification only occurs when, and where, necessary.
;;
;; See caveats and feedback below.  See also the fast-lock and lazy-shot
;; packages.  (But don't use them at the same time as lazy-lock!)

;; Installation:
;;
;; As of 21.5, put in your ~/.emacs:
;;
;; (setq font-lock-support-mode 'lazy-lock-mode)
;;
;; For 21.4, do this:
;;
;; (add-hook 'font-lock-mode-hook 'turn-on-lazy-lock)
;;
;; Start up a new XEmacs and use font-lock as usual (except that you can
;; use the so-called "gaudier" fontification regexps on big files without
;; frustration).
;;
;; In a buffer (which has `font-lock-mode' enabled) which is at least
;; `lazy-lock-minimum-size' characters long, only the visible portion of the
;; buffer will be fontified.  Motion around the buffer will fontify those
;; visible portions that were not previous fontified.
;;
;; If stealth fontification is enabled, fontification will occur in invisible
;; parts of the buffer after `lazy-lock-stealth-time' seconds of idle time.
;; Caveats:
;;
;; Lazy Lock mode does not work efficiently with Outline mode.  This is because
;; when in Outline mode, although text may be hidden (not visible in the
;; window), the text is visible to Emacs Lisp code (not surprisingly) and Lazy
;; Lock fontifies it mercilessly.  Hopefully this will be fixed one day.

;; Feedback:
;;
;; Feedback is welcome.
;; To submit a bug report (or make comments) please send to ben@xemacs.org.

(require 'font-lock)

(eval-when-compile
  ;; Well, shouldn't Lazy Lock be as lazy as possible?
  ;(setq byte-compile-dynamic t byte-compile-dynamic-docstrings t)
  ;;
  ;; We use this to preserve or protect things when modifying text properties.
  (defmacro save-buffer-state (varlist &rest body)
    "Bind variables according to VARLIST and eval BODY restoring buffer state.
Under FSF, it appears that setting text properties respects the read-only-ness
of a buffer and marks the buffer as modified, as well as maybe even calling
before-change and after-change hooks!  We don't do any of these things under
XEmacs, at least currently, so this whole macro is basically just a `let'."
    (` (let* ((,@ (append varlist
		   '(;(modified (buffer-modified-p)) (buffer-undo-list t)
		     ;(inhibit-read-only t)
		     ;;; FSF (inhibit-point-motion-hooks t)
		     ;before-change-functions after-change-functions
		     ;;; FSF deactivate-mark
		     ;buffer-file-name buffer-file-truename
		     ))))
	 (,@ body)
	 ;(when (and (not modified) (buffer-modified-p))
	 ;  (set-buffer-modified-p nil))
	 )))
  (put 'save-buffer-state 'lisp-indent-function 1)
  ;;
  ;; We use this for clarity and speed.  Naughty but nice.
  (defmacro do-while (test &rest body)
    "(do-while TEST BODY...): eval BODY... and repeat if TEST yields non-nil.
The order of execution is thus BODY, TEST, BODY, TEST and so on
until TEST returns nil."
    (` (while (progn (,@ body) (, test)))))
  (put 'do-while 'lisp-indent-function (get 'while 'lisp-indent-function))
  ;;
  ;; We use this for compatibility with a future Emacs.
  (or (fboundp 'with-temp-message)
      (defmacro with-temp-message (message &rest body)
	(let ((current-message (make-symbol "current-message"))
	      (temp-message (make-symbol "with-temp-message")))
	  `(let ((,temp-message ,message)
		 (,current-message))
	    (unwind-protect
		(progn
		  (when ,temp-message
		    (setq ,current-message (current-message))
		    (message "%s" ,temp-message))
		  ,@body)
	      (and ,temp-message ,current-message
		   (message "%s" ,current-message))))))))

(defvar lazy-lock-window-start-cache (make-hash-table :weakness 'key))
(defvar lazy-lock-window-end-cache (make-hash-table :weakness 'key))
(defvar lazy-lock-window-buffer-cache (make-hash-table :weakness 'key))
(defvar lazy-lock-window-buffer-modiff-cache (make-hash-table :weakness 'key))
(defvar lazy-lock-frame-modiff-cache (make-hash-table :weakness 'key))
(defvar lazy-lock-text-props-changed-cache (make-hash-table :weakness 'key)
  "Table of if non-nil, `lazy-lock' text prop changed and we need to wake up.")

(defvar lazy-lock-mode nil)			; Whether we are turned on.
(defvar lazy-lock-buffers nil)			; For deferral.
(defvar lazy-lock-timers (cons nil nil))	; For deferral and stealth.

(defvar lazy-lock-first-unfontify-pos nil
  "Consider text after this position as contextually unfontified.
If nil, contextual fontification is disabled.")
(make-variable-buffer-local 'lazy-lock-first-unfontify-pos)

(defgroup lazy-lock nil
  "Lazy-lock customizations"
  :group 'font-lock
  :prefix "lazy-lock-")

;;;###autoload
(defcustom lazy-lock-mode nil
  "Non nil means `lazy-lock-mode' is on."
  :group 'lazy-lock
  :require 'lazy-lock ;; which in turn requires font-lock.
  :type 'boolean
  :initialize 'custom-initialize-default
  :set '(lambda (var val)
	  (if val
	      (progn
		(lazy-lock-mode 1)
		(add-hook 'font-lock-mode-hook 'turn-on-lazy-lock))
	    (lazy-lock-mode -1)
	    (remove-hook 'font-lock-mode-hook 'turn-on-lazy-lock)))
  )


;; User Variables:

(defcustom lazy-lock-minimum-size 25600
  "*Minimum size of a buffer for demand-driven fontification.
On-demand fontification occurs if the buffer size is greater than this value.
If nil, means demand-driven fontification is never performed.
If a list, each element should be a cons pair of the form (MAJOR-MODE . SIZE),
where MAJOR-MODE is a symbol or t (meaning the default).  For example:
 ((c-mode . 25600) (c++-mode . 25600) (rmail-mode . 1048576))
means that the minimum size is 25K for buffers in C or C++ modes, one megabyte
for buffers in Rmail mode, and size is irrelevant otherwise.

The value of this variable is used when Lazy Lock mode is turned on."
  :type '(choice (const :tag "none" nil)
		 (integer :tag "size")
		 (repeat :menu-tag "mode specific" :tag "mode specific"
			 :value ((t . nil))
			 (cons :tag "Instance"
			       (radio :tag "Mode"
				      (const :tag "all" t)
				      (symbol :tag "name"))
			       (radio :tag "Size"
				      (const :tag "none" nil)
				      (integer :tag "size")))))
  :group 'lazy-lock)

;; We don't currently support this in our version of lazy-lock.
;; It's tricky to implement given the way our redisplay support works, and
;; it's not clear it's useful. (jit-lock likewise deletes it entirely)
(defcustom lazy-lock-defer-on-the-fly nil
  "*If non-nil, means fontification after a change should be deferred.
If nil, means on-the-fly fontification is performed.  This means when changes
occur in the buffer, those areas are immediately fontified.
If a list, it should be a list of `major-mode' symbol names for which deferred
fontification should occur.  The sense of the list is negated if it begins with
`not'.  For example:
 (c-mode c++-mode)
means that on-the-fly fontification is deferred for buffers in C and C++ modes
only, and deferral does not occur otherwise.

NOTE: Not currently implemented in this version of lazy-lock.

The value of this variable is used when Lazy Lock mode is turned on."
  :type '(choice (const :tag "never" nil)
		 (const :tag "always" t)
		 (set :menu-tag "mode specific" :tag "modes"
		      :value (not)
		      (const :tag "Except" not)
		      (repeat :inline t (symbol :tag "mode"))))
  :group 'lazy-lock)

(defcustom lazy-lock-defer-contextually 'syntax-driven
  "*If non-nil, means deferred fontification should be syntactically true.
If nil, means deferred fontification occurs only on those lines modified.  This
means where modification on a line causes syntactic change on subsequent lines,
those subsequent lines are not refontified to reflect their new context.
If t, means deferred fontification occurs on those lines modified and all
subsequent lines.  This means those subsequent lines are refontified to reflect
their new syntactic context, either immediately or when scrolling into them.
If any other value, e.g., `syntax-driven', means deferred syntactically true
fontification occurs only if syntactic fontification is performed using the
buffer mode's syntax table, i.e., only if `font-lock-keywords-only' is nil.

The value of this variable is used when Lazy Lock mode is turned on."
  :type '(choice (const :tag "never" nil)
		 (const :tag "always" t)
		 (other :tag "syntax-driven" syntax-driven))
  :group 'lazy-lock)

(defcustom lazy-lock-defer-time
  (if (featurep 'lisp-float-type) (/ (float 1) (float 4)) 1)
  "*Time in seconds to delay before beginning deferred fontification.
Deferred fontification occurs if there is no input within this time.
If nil, means fontification is never deferred, regardless of the values of the
variables `lazy-lock-defer-on-the-fly' and `lazy-lock-defer-contextually'.

The value of this variable is used when Lazy Lock mode is turned on."
  :type '(choice (const :tag "never" nil)
		 (number :tag "seconds"))
  :group 'lazy-lock)

;; not by default because it's not stealthy enough -- it can cause
;; annoying and unpredictable delays when it's running and you try to
;; do something.
(defcustom lazy-lock-stealth-time nil ;30
  "*Time in seconds to delay before beginning stealth fontification.
Stealth fontification occurs if there is no input within this time.
If nil, means stealth fontification is never performed.

The value of this variable is used when Lazy Lock mode is turned on."
  :type '(choice (const :tag "never" nil)
		 (number :tag "seconds"))
  :group 'lazy-lock)

(defcustom lazy-lock-stealth-lines (if font-lock-maximum-decoration 100 250)
  "*Maximum size of a chunk of stealth fontification.
Each iteration of stealth fontification can fontify this number of lines.
To speed up input response during stealth fontification, at the cost of stealth
taking longer to fontify, you could reduce the value of this variable."
  :type '(integer :tag "lines")
  :group 'lazy-lock)

(defcustom lazy-lock-stealth-load
  (if (condition-case nil (load-average) (error)) 200)
  "*Load in percentage above which stealth fontification is suspended.
Stealth fontification pauses when the system short-term load average (as
returned by the function `load-average' if supported) goes above this level,
thus reducing the demand that stealth fontification makes on the system.
If nil, means stealth fontification is never suspended.
To reduce machine load during stealth fontification, at the cost of stealth
taking longer to fontify, you could reduce the value of this variable.
See also `lazy-lock-stealth-nice'."
  :type (if (condition-case nil (load-average) (error))
	    '(choice (const :tag "never" nil)
		     (integer :tag "load"))
	  '(const :format "%t: unsupported\n" nil))
  :group 'lazy-lock)

(defcustom lazy-lock-stealth-nice
  (if (featurep 'lisp-float-type) (/ (float 1) (float 8)) 1)
  "*Time in seconds to pause between chunks of stealth fontification.
Each iteration of stealth fontification is separated by this amount of time,
thus reducing the demand that stealth fontification makes on the system.
If nil, means stealth fontification is never paused.
To reduce machine load during stealth fontification, at the cost of stealth
taking longer to fontify, you could increase the value of this variable.
See also `lazy-lock-stealth-load'."
  :type '(choice (const :tag "never" nil)
		 (number :tag "seconds"))	  
  :group 'lazy-lock)

(defcustom lazy-lock-stealth-verbose
  (if (featurep 'lisp-float-type)
      (and (not lazy-lock-defer-contextually) (not (null font-lock-verbose))))
  "*If non-nil, means stealth fontification should show status messages."
  :type 'boolean
  :group 'lazy-lock)

(defcustom lazy-lock-walk-windows 'all-frames
  "*If non-nil, fontify windows other than the selected window.
If `all-frames', fontify windows even on other frames.
A non-nil value slows down redisplay."
  :type 'boolean
  :group 'lazy-lock)

(defcustom lazy-lock-mode-line-string nil ; " Lazy"
  "*String to display in the modeline when `lazy-lock-mode' is active.
Set this to nil if you don't want a modeline indicator."
  :type '(choice string
		 (const :tag "none" nil))
  :group 'lazy-lock)

; (defvar lazy-lock-rounding-size 500
;   "Round end points of fontified chunks to the nearest multiple of this value.
; Fontifying any amount of text involves some overhead; by increasing the
; size, we minimize this, and by rounding to particular points we help to
; minimize constant refontification in some circumstances when the displayed
; area moves little by little.  Setting this too big can cause unnecessary
; delays.")


;; User Functions:

;;;###autoload
(defun lazy-lock-mode (&optional arg)
  "Toggle Lazy Lock mode.
With arg, turn Lazy Lock mode on if and only if arg is positive.  Enable it
automatically in your `~/.emacs' by:

 (setq font-lock-support-mode 'lazy-lock-mode)

When Lazy Lock mode is enabled, fontification can be lazy in a number of ways:

- Demand-driven buffer fontification if `lazy-lock-minimum-size' is non-nil.
  This means initial fontification does not occur if the buffer is greater than
  `lazy-lock-minimum-size' characters in length.  Instead, fontification occurs
  when necessary, such as when scrolling through the buffer would otherwise
  reveal unfontified areas.  This is useful if buffer fontification is too slow
  for large buffers.

- Deferred on-the-fly fontification if `lazy-lock-defer-on-the-fly' is non-nil.
  This means on-the-fly fontification does not occur as you type.  Instead,
  fontification is deferred until after `lazy-lock-defer-time' seconds of Emacs
  idle time, while Emacs remains idle.  This is useful if fontification is too
  slow to keep up with your typing.

- Deferred context fontification if `lazy-lock-defer-contextually' is non-nil.
  This means fontification updates the buffer corresponding to true syntactic
  context, after `lazy-lock-defer-time' seconds of Emacs idle time, while Emacs
  remains idle.  Otherwise, fontification occurs on modified lines only, and
  subsequent lines can remain fontified corresponding to previous syntactic
  contexts.  This is useful where strings or comments span lines.

- Stealthy buffer fontification if `lazy-lock-stealth-time' is non-nil.
  This means remaining unfontified areas of buffers are fontified if Emacs has
  been idle for `lazy-lock-stealth-time' seconds, while Emacs remains idle.
  This is useful if any buffer has any deferred fontification.

Basic Font Lock mode on-the-fly fontification behaviour fontifies modified
lines only.  Thus, if `lazy-lock-defer-contextually' is non-nil, Lazy Lock mode
on-the-fly fontification may fontify differently, albeit correctly.  In any
event, to refontify some lines you can use \\[font-lock-fontify-block].

Stealth fontification only occurs while the system remains unloaded.
If the system load rises above `lazy-lock-stealth-load' percent, stealth
fontification is suspended.  Stealth fontification intensity is controlled via
the variable `lazy-lock-stealth-nice' and `lazy-lock-stealth-lines', and
verbosity is controlled via the variable `lazy-lock-stealth-verbose'."
  (interactive "P")
  (let* ((was-on lazy-lock-mode)
	 (now-on (unless (memq 'lazy-lock-mode font-lock-inhibit-thing-lock)
		   (if arg (> (prefix-numeric-value arg) 0) (not was-on)))))
    (cond ((and now-on (not font-lock-mode))
	   ;; Turned on `lazy-lock-mode' rather than `font-lock-mode'.
	   (let ((font-lock-support-mode 'lazy-lock-mode))
	     (font-lock-mode t)))
	  (now-on
	   ;; Turn ourselves on.
	   (set (make-local-variable 'lazy-lock-mode) t)
	   (lazy-lock-install))
	  (was-on
	   ;; Turn ourselves off.
	   (set (make-local-variable 'lazy-lock-mode) nil)
	   (lazy-lock-unstall)))))

;;;###autoload
(defun turn-on-lazy-lock ()
  "Unconditionally turn on Lazy Lock mode."
  (lazy-lock-mode t))

;; Copied from font-lock-value-in-major-mode (not available for 21.4 users)
(defun lazy-lock-value-in-major-mode (alist)
  "Return value in ALIST for `major-mode', or ALIST if it is not an alist.
Structure is ((MAJOR-MODE . VALUE) ...) where MAJOR-MODE may be t."
  (if (consp alist)
      (cdr (or (assq major-mode alist) (assq t alist)))
    alist))

(defun lazy-lock-install ()
  (let ((min-size (lazy-lock-value-in-major-mode lazy-lock-minimum-size))
	(defer-change (and lazy-lock-defer-time lazy-lock-defer-on-the-fly))
	(defer-context (and lazy-lock-defer-time lazy-lock-defer-contextually
			    (or (eq lazy-lock-defer-contextually t)
				(null font-lock-keywords-only)))))
    ;;
    ;; Tell Font Lock whether Lazy Lock will do fontification.
    (make-local-variable 'font-lock-fontified)
    (setq font-lock-fontified (and min-size (>= (buffer-size) min-size)))
    ;;
    ;; Add the text properties and fontify.
    (if (not font-lock-fontified)
	(lazy-lock-after-fontify-buffer)
      ;; FSF 21.2 here explicitly fontifies the visible window.  Not
      ;; necessary in XEmacs.  You could say this is yet another hokey
      ;; hook-in required to get things working.
      )

    ;; Initialize deferred contextual fontification if requested.
    (when defer-context
      (setq lazy-lock-first-unfontify-pos
	    (or lazy-lock-first-unfontify-pos (point-max))))

    ;;
    ;; Add the fontification hooks.
    (lazy-lock-install-hooks
     font-lock-fontified
     (cond ((eq (car-safe defer-change) 'not)
	    (not (memq major-mode (cdr defer-change))))
	   ((listp defer-change)
	    (memq major-mode defer-change))
	   (t
	    defer-change))
     defer-context)
    ;;
    ;; Add the fontification timers.
    (lazy-lock-install-timers
     (if (or defer-change defer-context) lazy-lock-defer-time)
     lazy-lock-stealth-time)))

;; XEmacs DEFER-SCROLL argument deleted and all related code.
(defun lazy-lock-install-hooks (fontifying defer-change defer-context)
  ;; Make sure our hooks are correct.
  (remove-hook 'pre-idle-hook 'lazy-lock-pre-idle-fontify-windows)
  ;; Make sure our hooks are at the end.  Font-lock in XEmacs installs its
  ;; own pre-idle-hook to implement deferral (#### something that should
  ;; really be merged with this file; or more likely, lazy-lock in its
  ;; entirety should be merged into font-lock).  We *DO NOT* make
  ;; pre-idle-hook be local.  It needs to be able to update the unfontified
  ;; regions of *all* frames.  If you turn off lazy lock in your particular
  ;; buffer and set a local value to nil, none of the other windows will
  ;; get fontified!
  (add-hook 'pre-idle-hook 'lazy-lock-pre-idle-fontify-windows t)
  ;;
  ;; Replace Font Lock mode hook.
  (make-local-hook 'after-change-functions)
  (remove-hook 'after-change-functions 'font-lock-after-change-function t)
  (add-hook 'after-change-functions 'lazy-lock-after-change nil t)

  ;; FSF 21.2: Lots and lots of hooks here.  Hooks for `outline', hooks for
  ;; `hideshow', hooks for redisplay-end-triggers, window-size-changed, and
  ;; window-scroll, before-change-functions needed to set up the end
  ;; triggers, multiple different versions of the after-change hook.  All
  ;; GONE!  VANQUISHED!  DEAD!  DESTROYED!  Thank God.  We have one
  ;; after-change hook and the rest are unnecessary in our model.
  )

(defun lazy-lock-install-timers (dtime stime)
  ;; Schedule or re-schedule the deferral and stealth timers.
  ;; The layout of `lazy-lock-timers' is:
  ;;  ((DEFER-TIME . DEFER-TIMER) (STEALTH-TIME . STEALTH-TIMER)
  ;; If an idle timeout has changed, cancel the existing idle timer (if there
  ;; is one) and schedule a new one (if the new idle timeout is non-nil).
  (unless (eq dtime (car (car lazy-lock-timers)))
    (let ((defer (car lazy-lock-timers)))
      (when (cdr defer)
	(delete-itimer (cdr defer)))
      (setcar lazy-lock-timers (cons dtime (and dtime
	      (run-with-idle-timer dtime t 'lazy-lock-fontify-after-defer))))))
  (unless (eq stime (car (cdr lazy-lock-timers)))
    (let ((stealth (cdr lazy-lock-timers)))
      (when (cdr stealth)
	(delete-itimer (cdr stealth)))
      (setcdr lazy-lock-timers (cons stime (and stime
	      (run-with-idle-timer stime t 'lazy-lock-fontify-after-idle)))))))

(defun lazy-lock-unstall ()
  ;;
  ;; If Font Lock mode is still enabled, make sure that the buffer is
  ;; fontified, and reinstall its hook.  We must do this first.
  (when font-lock-mode
    (when (lazy-lock-unfontified-p)
      (let ((verbose (if (numberp font-lock-verbose)
			 (> (buffer-size) font-lock-verbose)
		       font-lock-verbose)))
	(with-temp-message
	    (when verbose
	      (format "Fontifying %s..." (buffer-name)))
	  ;; Make sure we fontify etc. in the whole buffer.
	  (save-restriction
	    (widen)
	    (lazy-lock-fontify-region (point-min) (point-max))))))
    (add-hook 'after-change-functions 'font-lock-after-change-function nil t))
  ;;
  ;; Remove the text properties.
  (lazy-lock-after-unfontify-buffer)
  ;;
  ;; Remove the fontification hooks.
  (remove-hook 'after-change-functions 'lazy-lock-after-change t)
  )

;; use put-nonduplicable-text-property to avoid unfriendly behavior
;; when doing undo, etc.  We really don't want syntax-highlighting text
;; properties copied into strings or tracked by undo.
;;
;; Old comment:
;; [[ #### If start-open and end-open really behaved like they are supposed to,
;; we wouldn't really need this.  I kind of fixed them up, but there's still
;; a bug -- inserting text into the middle of a region of
;; (start-open t end-open t) text should cause it not to inherit, but it
;; does. ]]
;;
;; They do behave correctly now.  #### What should we be doing? --ben

(defalias 'lazy-lock-put-text-property 'put-nonduplicable-text-property)


;; Hook functions.

;; 1.  Text that needs to be fontified is done through `pre-idle-hook',
;;     which runs directly before redisplay happens.  It is a single hook,
;;     so the hook must look through all the windows/frames/etc to see what
;;     needs to be done.  This is not that elegant of a solution, but it
;;     works and avoids the horrible hacked-up multi-hooks in FSF's
;;     lazy-lock.  Gerd's jit-lock does it right, and #### we should
;;     implement the same support.

;; lazy-lock optimization:
;;
;; pre-idle-hook is called an awful lot -- pretty much every time the
;; mouse moves or a timeout expires, for example.  On Linux (sometimes),
;; IRIX 5.x, and Solaris 2.something, it happens every 1/4 of a second
;; due to the 1/4-second timers installed to compensate for various
;; operating system deficiencies in the handling of SIGIO and SIGCHLD.
;; (Those timers cause a cycle of the event loop.  They don't necessarily
;; have to, but rewriting to avoid this is fairly tricky and requires
;; having significant amounts of code called from signal handlers, which
;; (despite that fact that FSF Emacs reads its X input during a signal
;; handler ?!), is almost always a bad idea -- it's extremely easy to
;; introduce race conditions, which are very hard to track down.
;;
;; So to improve things, I added `frame-modified-tick'.  This is an
;; internal counter that gets ticked any time that any internal
;; redisplay variable gets ticked.  If `frame-modified-tick' is
;; the same as the last time we checked, it means that redisplay will
;; do absolutely nothing when encountering this frame, and thus we
;; can skip out immediately.  This happens when the 1/4-second timer
;; fires while we're idle, or if we just move the mouse. (Moving
;; around in a buffer changes `frame-modified-tick' because the
;; internal redisplay variable "point_changed" gets ticked.  We could
;; easily improve things further by adding more tick counters, mirroring
;; more closely the internal redisplay counters -- e.g. if we had
;; another counter that didn't get ticked when point moved, we could
;; tell if anything was going to happen by seeing if point is within
;; window-start and window-end, since we know that redisplay will
;; only do a window-scroll if it's not. (If window-start or window-end
;; or window-buffer or anything else changed, windows_changed or
;; some other variable will get ticked.))
;;
;; Also, it's wise to try and avoid things that cons.  Avoiding
;; `save-window-excursion', as we do, is definitely a major win because
;; that's a heavy-duty consing function.  In fact, we do no consing at all
;; until the frame-modified tick goes off, and even then the only potential
;; consing we do is save-excursion; but in fact, that is consless too.

(defun lazy-lock-pre-idle-fontify-windows ()
;  (debug-print (gethash (selected-frame) lazy-lock-frame-modiff-cache))
;  (debug-print (frame-modified-tick (selected-frame)))
  (unless nil ;(memq this-command lazy-lock-ignore-commands)
    ;; Do the visible parts of the buffer(s), i.e., the window(s).
    ;(dp "pre-idle-called")
    (if (or (not lazy-lock-walk-windows)
	    (and (eq lazy-lock-walk-windows t) (one-window-p t)))
       (or (window-minibuffer-p (selected-window))
	    (lazy-lock-fontify-window (selected-window)))
      (if (eq lazy-lock-walk-windows t)
	  (lazy-lock-maybe-fontify-frame (selected-frame))
	;; Visit all visible non-minibuffer-only frames on the selected device.
	;; This is harder than it looks, since the `next-frame'
	;; interface is error-prone - finding the starting frame is hard.
	(catch 'lazy-lock-frame-loop-done
	  (let* ((starting-frame (selected-frame))
		 (frame starting-frame))
	    (when (or (not (frame-visible-p frame))
		      (frame-minibuffer-only-p frame))
	      ;; starting-frame not suitable.
	      (setq starting-frame (next-frame starting-frame 'visible-nomini))
	      (when (eq starting-frame frame)
		;; No suitable frames.
		(throw 'lazy-lock-frame-loop-done t))
	      (setq frame starting-frame))
	    (while t
	      (lazy-lock-maybe-fontify-frame frame)
	      (setq frame (next-frame frame 'visible-nomini))
	      (when (eq frame starting-frame)
		(throw 'lazy-lock-frame-loop-done t)))))))))

(defun lazy-lock-maybe-fontify-frame (frame)
  ;; Fontify the given frame if we need to.  We first check the
  ;; appropriate frame-modified-tick to avoid changing global state.
  ;(dp "fontify-frame %s" frame)
  (let ((tick (frame-modified-tick frame)))
    ;(dp tick)
    (unless (eq tick (gethash frame lazy-lock-frame-modiff-cache))
      (puthash frame tick lazy-lock-frame-modiff-cache)
      ;; We have to select the frame due to a bug in walk-windows in XEmacs
      ;; 21.4.
      (with-selected-frame frame
	(walk-windows #'lazy-lock-fontify-window 'no-minibuf frame)))))

;; 2.  Modified text must be marked as unfontified so it can be identified and
;;     fontified later when Emacs is idle.  Deferral occurs by adding one of
;;     `lazy-lock-fontify-*-after-change' (for on-the-fly fontification) or
;;     `lazy-lock-defer-*-after-change' (for deferred fontification) to the
;;     hook `after-change-functions'.

;; Modeled after jit-lock-after-change (21.2).
(defun lazy-lock-after-change (beg end old-len)
  "Mark the rest of the buffer as not fontified after a change.
Installed on `after-change-functions'.
BEG and END are the start and end of the changed text.  OLD-LEN
is the pre-change length.
This function ensures that lines following the change will be refontified
in case the syntax of those lines has changed.  Refontification
will take place when text is fontified stealthily."
  (when lazy-lock-mode
    (save-excursion
      (save-buffer-state nil
	;; It's important that the `fontified' property be set from the
	;; beginning of the line, else font-lock will properly change the
	;; text's face, but the display will have been done already and will
	;; be inconsistent with the buffer's content.
	(goto-char beg)
	(setq beg (point-at-bol))
	
	;; If we're in text that matches a multi-line font-lock pattern,
	;; make sure the whole text will be redisplayed.
	(when (get-text-property beg 'font-lock-multiline)
	  (setq beg (or (previous-single-property-change
			 beg 'font-lock-multiline)
			(point-min))))
	
	;; Make sure we change at least one char (in case of deletions).
	(setq end (min (max end (1+ beg)) (point-max)))
	;; Request refontification of changed region right away.
	;;(when (not lazy-lock-defer-on-the-fly) #### Doesn't work.
	;;We have to arrange a way for the inserted chunk to definitely be
	;;fontified in the defer function.  There may be a whole bunch of
	;;chunks inserted before the defer function is run.  We'd probably
	;;need text properties with a different name from `lazy-lock' to
	;;note all such regions, and maybe keep a minimum and maximum, and
	;;it would make the logic all clogged up. --ben
	(if (= beg end)
	    (font-lock-after-change-function beg end old-len)
	  (lazy-lock-put-text-property beg end 'lazy-lock nil)))
      (unless (memq (current-buffer) lazy-lock-buffers)
	(push (current-buffer) lazy-lock-buffers))
      ;; Mark the change for deferred contextual refontification.
      (when lazy-lock-first-unfontify-pos
	(setq lazy-lock-first-unfontify-pos
	      (min lazy-lock-first-unfontify-pos beg)))
      )))

;; 3.  Deferred fontification and stealth fontification are done from these two
;;     functions.  They are set up as Idle Timers.

(defun lazy-lock-fontify-after-defer ()
  ;; Called from `timer-idle-list'.
  ;; Fontify all windows where deferral has occurred for its buffer.
  ;(beep)
  ;(dp "fontify-after-defer")
  (save-excursion
    (while (and lazy-lock-buffers (not (input-pending-p)))
      (let ((buffer (car lazy-lock-buffers)) windows)
	;; Paranoia: check that the buffer is still live and Lazy Lock mode on.
	(when (buffer-live-p buffer)
	  (set-buffer buffer)
	  (when lazy-lock-mode
	    ;; Perform deferred unfontification, if any.
	    (when lazy-lock-first-unfontify-pos
	      (save-restriction
		(widen)
		(when (and (>= lazy-lock-first-unfontify-pos (point-min))
			   (< lazy-lock-first-unfontify-pos (point-max)))
		  (save-buffer-state nil
		    (lazy-lock-put-text-property lazy-lock-first-unfontify-pos
						 (point-max) 'lazy-lock nil))
		  (setq lazy-lock-first-unfontify-pos (point-max))
		  )))

	    (setq windows (get-buffer-window-list buffer 'nomini t))
	    (while windows
	      (puthash (car windows) t lazy-lock-text-props-changed-cache)
	      ;; #### The following isn't necessary.  Does it speed up the
	      ;; response time?  Or slow down the overall performance?
	      (lazy-lock-fontify-window (car windows))
	      (setq windows (cdr windows)))))
	(setq lazy-lock-buffers (cdr lazy-lock-buffers))))))

(defun lazy-lock-fontify-after-idle ()
  ;; Called from `timer-idle-list'.
  ;; Fontify all buffers that need it, stealthily while idle.
  (unless (or executing-kbd-macro (window-minibuffer-p (selected-window)))
    ;; Loop over all buffers, fontify stealthily for each if necessary.
    (let ((buffers (buffer-list)) (continue t)
	  message ;; FSF 21.2 message-log-max minibuffer-auto-raise
	  )
      (save-excursion
	(do-while (and buffers continue)
	  (set-buffer (car buffers))
	  (if (not (and lazy-lock-mode (lazy-lock-unfontified-p)))
	      (setq continue (not (input-pending-p)))
	    ;; Fontify regions in this buffer while there is no input.
	    (with-temp-message
		(when lazy-lock-stealth-verbose
		  "Fontifying stealthily...")
	      (do-while (and (lazy-lock-unfontified-p) continue)
		(if (and lazy-lock-stealth-load
			 (> (car (load-average)) lazy-lock-stealth-load))
		    ;; Wait a while before continuing with the loop.
		    (progn
		      (when message
			(message "Fontifying stealthily...suspended")
			(setq message nil))
		      (setq continue (sit-for (or lazy-lock-stealth-time 30))))
		  ;; Fontify a chunk.
		  (when lazy-lock-stealth-verbose
		    (if message
			(message "Fontifying stealthily... %2d%% of %s"
				 (lazy-lock-percent-fontified) (buffer-name))
		      (message "Fontifying stealthily...")
		      (setq message t)))
		  ;; Current buffer may have changed during `sit-for'.
		  (set-buffer (car buffers))
		  (lazy-lock-fontify-chunk)
		  (setq continue (sit-for (or lazy-lock-stealth-nice 0)))))))
	  (setq buffers (cdr buffers)))))))

;; 4.  Special circumstances.

(defun lazy-lock-after-fontify-buffer ()
  ;; Called from `font-lock-after-fontify-buffer'.
  ;; Mark the current buffer as fontified.
  ;; FSF: [[This is a conspiracy hack between lazy-lock.el and font-lock.el.]]
  (save-buffer-state nil
    (lazy-lock-put-text-property (point-min) (point-max)
				 'lazy-lock t)))

(defun lazy-lock-after-unfontify-buffer ()
  ;; Called from `font-lock-after-unfontify-buffer'.
  ;; Mark the current buffer as unfontified.
  ;; FSF: [[This is a conspiracy hack between lazy-lock.el and font-lock.el.]]
  (save-buffer-state nil
    (remove-text-properties (point-min) (point-max) '(lazy-lock nil))))



;; Functions for fontification:

;; If packages want to ensure that some region of the buffer is fontified, they
;; should use this function.  For an example, see ps-print.el.

(defun lazy-lock-fontify-region (beg end)
  ;; Fontify between BEG and END, where necessary, in the current buffer.
  (save-restriction
    (widen)
    (when (setq beg (text-property-any beg end 'lazy-lock nil))
      (save-excursion
	(save-match-data
	  (save-buffer-state
	   ;; Ensure syntactic fontification is always correct.
	   ;; Do NOT bind font-lock-beginning-of-syntax-function because
	   ;; that leads to catastrophic behavior when scrolling backwards
	   ;; from the end of a large buffer -- parse-partial-sexp will start
	   ;; at (point-min) each time!
	   (;; FSF 21.2 font-lock-beginning-of-syntax-function
	    next)
	   ;; Find successive unfontified regions between BEG and END.
	   (condition-case data
	       ;; FSF has just BEG as the condition.  We have a bug in
	       ;; text-property-any in 21.4 when BEG > END so we need the
	       ;; extra check.
	       (do-while (and beg (< beg end))
		 (setq next (or (text-property-any beg end 'lazy-lock t) end))
		 ;; Make sure the region end points are at beginning of
		 ;; line.
		 (goto-char beg)
		 (unless (bolp)
		   (beginning-of-line)
		   (setq beg (point)))
		 (goto-char next)
		 (unless (bolp)
		   (forward-line)
		   (setq next (point)))
		 ;; Fontify the region, then flag it as fontified.
		 (font-lock-fontify-region beg next)
		 (lazy-lock-put-text-property beg next 'lazy-lock t)
		 (setq beg (text-property-any next end 'lazy-lock
					      nil)))
	     ((error quit) (message "Fontifying region...%s" data))
	     )))))))

(defun lazy-lock-fontify-chunk ()
  ;; Fontify the nearest chunk, for stealth, in the current buffer.
  (let (;(inhibit-point-motion-hooks t)
	)
    (save-excursion
      (save-restriction
	(widen)
	;; Move to end of line in case the character at point is not fontified.
	(end-of-line)
	;; Find where the previous (next) unfontified regions end (begin).
	(let ((prev (previous-single-property-change (point) 'lazy-lock))
	      (next (text-property-any (point) (point-max) 'lazy-lock nil)))
	  ;; Fontify from the nearest unfontified position.
	  (if (or (null prev) (and next (< (- next (point)) (- (point) prev))))
	      ;; The next, or neither, region is the nearest not fontified.
	      (lazy-lock-fontify-region
	       (progn (goto-char (or next (point-min)))
		      (beginning-of-line)
		      (point))
	       (progn (goto-char (or next (point-min)))
		      (forward-line lazy-lock-stealth-lines)
		      (point)))
	    ;; The previous region is the nearest not fontified.
	    (lazy-lock-fontify-region
	     (progn (goto-char prev)
		    (forward-line (- lazy-lock-stealth-lines))
		    (point))
	     (progn (goto-char prev)
		    (forward-line)
		    (point)))))))))

(defun lazy-lock-vertical-motion-value (window pos lines)
  "Move LINES lines down from POS in the WINDOW's buffer and return pos."
  (let ((buffer (window-buffer window)))
    (if (eq window (selected-window))
	(with-current-buffer buffer
	  (save-excursion
	    (goto-char pos)
	    (vertical-motion lines window)
	    (point))))
      ;; In this case, vertical-motion sets window's point, not window's
      ;; buffer's point.
      (let ((winp (window-point window)))
	(unwind-protect
	    (progn
	      (set-window-point window pos)
	      (vertical-motion lines window)
	      (window-point window))
	  (set-window-point window winp)))))

(defun lazy-lock-fontify-window (window)
  ;; Fontify the given window if we need to.  We first check the
  ;; buffer-local value of lazy-lock-mode to make sure we should do
  ;; the more accurate (but semi-expensive) checks below.
  ;(dp "fontifying %s" window)
  (let ((buffer (window-buffer window))
	we-are-screwed
	(check-text-props (gethash window lazy-lock-text-props-changed-cache)))
    (when (symbol-value-in-buffer 'lazy-lock-mode buffer)
      (with-current-buffer buffer
	(let ((ws (window-start window))
	      ;; use the GUARANTEE option on window-end to be accurate.  this
	      ;; also avoids the need to fontify an over-large area to avoid
	      ;; leaving unfontified areas visible.  Unfortunately, it seems
	      ;; that by just using the return value from window-end, the
	      ;; clipped line at the bottom of the window doesn't get
	      ;; fontified.  So we have to go down from there.  It turns out
	      ;; that an arg of 2 is the minimum that will work -- and 0
	      ;; actually goes up a line!  #### Another bug in vertical-motion?
	      (we ;(window-end window t))
	       (lazy-lock-vertical-motion-value window
						(window-end window t) 2))
	      (point (point buffer))
	      (modiff (buffer-modified-tick buffer)))
	  (cond ((or (< point ws) (> point we))
		 (setq we-are-screwed t check-text-props t))
		((or check-text-props
		     (not
		      (and
		       (eq buffer
			   (gethash window lazy-lock-window-buffer-cache))
		       (eq modiff
			   (gethash window
				    lazy-lock-window-buffer-modiff-cache))
		       (eq ws (gethash window lazy-lock-window-start-cache))
		       (eq we (gethash window lazy-lock-window-end-cache)))))
		 (setq check-text-props t)))
	  (when we-are-screwed
	    (setq ws
		  (lazy-lock-vertical-motion-value window (window-point window)
						   (- (window-height window))))
	    (setq we
		  (lazy-lock-vertical-motion-value window (window-point window)
						   (window-height window)))
	    ;; #### Not currently implemented.  Perhaps not necessary.
; 	    (setq ws (* lazy-lock-rounding-size
; 			(/ ws lazy-lock-rounding-size))
; 		  we (* lazy-lock-rounding-size
; 			(/ (+ we (1- lazy-lock-rounding-size))
; 			   lazy-lock-rounding-size)))
; 	    (setq ws (max (point-min buffer) ws)
; 		  we (min (point-max buffer) we)))
	    )
	  (when check-text-props
	    (puthash window buffer lazy-lock-window-buffer-cache)
	    (puthash window modiff lazy-lock-window-buffer-modiff-cache)
	    (puthash window ws lazy-lock-window-start-cache)
	    (puthash window we lazy-lock-window-end-cache)
	    (lazy-lock-fontify-region ws we)
	    ))))))

(defun lazy-lock-unfontified-p ()
  ;; Return non-nil if there is anywhere still to be fontified.
  (save-restriction
    (widen)
    (text-property-any (point-min) (point-max) 'lazy-lock nil)))

(defun lazy-lock-percent-fontified ()
  ;; Return the percentage (of characters) of the buffer that are fontified.
  (save-restriction
    (widen)
    (let ((beg (point-min)) (size 0) next)
      ;; Find where the next fontified region begins.
      (while (setq beg (text-property-any beg (point-max) 'lazy-lock t))
	(setq next (or (text-property-any beg (point-max) 'lazy-lock nil)
		       (point-max)))
	(incf size (- next beg))
	(setq beg next))
      ;; Float because using integer multiplication will frequently overflow.
      (truncate (* (/ (float size) (point-max)) 100)))))

;; Install ourselves:

(add-hook 'font-lock-after-fontify-buffer-hook
	  'lazy-lock-after-fontify-buffer)

(add-hook 'font-lock-after-unfontify-buffer-hook
	  'lazy-lock-after-unfontify-buffer)

;; XEmacs change: do it the right way.  This works with modeline mousing.
;;;###autoload
(add-minor-mode 'lazy-lock-mode 'lazy-lock-mode-line-string)

;; Provide ourselves:

(provide 'lazy-lock)

;;; lazy-lock.el ends here
