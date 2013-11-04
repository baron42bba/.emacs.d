;;; browse-url-xemacs.el --- browse-url stuff for XEmacs

;; Copyright (C) 1991-1995, 1997-1998 Free Software Foundation, Inc.
;; Copyright (C) 1995 Tinker Systems and INS Engineering Corp.
;; Copyright (C) 1995 Sun Microsystems.
;; Copyright (C) 1995, 1996, 2000 Ben Wing.
;; Copyright (C) 1997 MORIOKA Tomohiko.

;; Maintainer: XEmacs Development Team

;; This file is part of XEmacs.

;; XEmacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; XEmacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with XEmacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This code comes from menubar-items.el in XEmacs core.

;;; Code:

(defvar browse-url-menu)

;;;###autoload
(progn
  (defun browse-url-xemacs-init-menu ()
    (if (featurep 'menubar) 
	(add-submenu '("Options" "Internet") browse-url-menu)
      )))

(cond ((featurep 'xemacs)
       (defvar browse-url-menu
	 '("%_Open URLs With"
	   ["%_Emacs-W3"
	    (customize-set-variable 'browse-url-browser-function 'browse-url-w3)
	    :style radio
	    :selected (eq browse-url-browser-function 'browse-url-w3)
	    :active (and (fboundp 'browse-url-w3)
			 (fboundp 'w3-fetch))]
	   ["Emacs-%_W3 (gnudoit)"
	    (customize-set-variable 'browse-url-browser-function 'browse-url-w3-gnudoit)
	    :style radio
	    :selected (eq browse-url-browser-function 'browse-url-w3-gnudoit)
	    :active (fboundp 'browse-url-w3-gnudoit)]
	   ["%_Netscape"
	    (customize-set-variable 'browse-url-browser-function
				    'browse-url-netscape)
	    :style radio
	    :selected (eq browse-url-browser-function 'browse-url-netscape)
	    :active (fboundp 'browse-url-netscape)]
	   ["%_Mosaic"
	    (customize-set-variable 'browse-url-browser-function
				    'browse-url-mosaic)
	    :style radio
	    :selected (eq browse-url-browser-function 'browse-url-mosaic)
	    :active (fboundp 'browse-url-mosaic)]
	   ["Mosaic (%_CCI)"
	    (customize-set-variable 'browse-url-browser-function 'browse-url-cci)
	    :style radio
	    :selected (eq browse-url-browser-function 'browse-url-cci)
	    :active (fboundp 'browse-url-cci)]
	   ["%_IXI Mosaic"
	    (customize-set-variable 'browse-url-browser-function
				    'browse-url-iximosaic)
	    :style radio
	    :selected (eq browse-url-browser-function 'browse-url-iximosaic)
	    :active (fboundp 'browse-url-iximosaic)]
	   ["%_Lynx (xterm)"
	    (customize-set-variable 'browse-url-browser-function
				    'browse-url-lynx-xterm)
	    :style radio
	    :selected (eq browse-url-browser-function 'browse-url-lynx-xterm)
	    :active (fboundp 'browse-url-lynx-xterm)]
	   ["L%_ynx (xemacs)"
	    (customize-set-variable 'browse-url-browser-function
				    'browse-url-lynx-emacs)
	    :style radio
	    :selected (eq browse-url-browser-function 'browse-url-lynx-emacs)
	    :active (fboundp 'browse-url-lynx-emacs)]
	   ["%_Grail"
	    (customize-set-variable 'browse-url-browser-function
				    'browse-url-grail)
	    :style radio
	    :selected (eq browse-url-browser-function 'browse-url-grail)
	    :active (fboundp 'browse-url-grail)]
	   ["%_KDE"
	    (customize-set-variable 'browse-url-browser-function
				    'browse-url-kde)
	    :style radio
	    :selected (eq browse-url-browser-function 'browse-url-kde)
	    :active (fboundp 'browse-url-kde)]
	   ["Mo%_zilla"
	    (customize-set-variable 'browse-url-browser-function
				    'browse-url-mozilla)
	    :style radio
	    :selected (eq browse-url-browser-function 'browse-url-mozilla)
	    :active (and (fboundp 'browse-url-mozilla)
			 (executable-find browse-url-mozilla-program))]
	   ["%_Firefox"
	    (customize-set-variable 'browse-url-browser-function
				    'browse-url-firefox)
	    :style radio
	    :selected (eq browse-url-browser-function 'browse-url-firefox)
	    :active (and (fboundp 'browse-url-firefox)
			 (executable-find browse-url-firefox-program))]
	   ["G%_aleon"
	    (customize-set-variable 'browse-url-browser-function
				    'browse-url-galeon)
	    :style radio
	    :selected (eq browse-url-browser-function 'browse-url-galeon)
	    :active (fboundp 'browse-url-galeon)]
	   ["%_Opera"
	    (customize-set-variable 'browse-url-browser-function
				    'browse-url-opera)
	    :style radio
	    :selected (eq browse-url-browser-function 'browse-url-opera)
	    :active (fboundp 'browse-url-opera)]
	   ["%_MMM"
	    (customize-set-variable 'browse-url-browser-function
				    'browse-url-mmm)
	    :style radio
	    :selected (eq browse-url-browser-function 'browse-url-mmm)
	    :active (fboundp 'browse-url-mmm)]
	   ["MS-Windows Default %_Browser"
	    (customize-set-variable 'browse-url-browser-function
				    'browse-url-default-windows-browser)
	    :style radio
	    :selected (eq browse-url-browser-function 'browse-url-default-windows-browser)
	    :active (and (fboundp 'mswindows-shell-execute)
			 (fboundp 'browse-url-default-windows-browser))]
	   ["G%_eneric Browser"
	    (customize-set-variable 'browse-url-browser-function
				    'browse-url-generic)
	    :style radio
	    :selected (eq browse-url-browser-function 'browse-url-generic)
	    :active (and (boundp 'browse-url-generic-program)
			 browse-url-generic-program
			 (fboundp 'browse-url-generic))]
	   ["Emacs-W%_3M"
	    (customize-set-variable 'browse-url-browser-function 'browse-url-w3m)
	    :style radio
	    :selected (eq browse-url-browser-function 'browse-url-w3m)
	    :active (and (fboundp 'browse-url-w3m)
			 (fboundp 'w3m-goto-url-new-session))]
	   ["Emacs-W3M (gn%_udoit)"
	    (customize-set-variable 'browse-url-browser-function 'browse-url-w3m-gnudoit)
	    :style radio
	    :selected (eq browse-url-browser-function 'browse-url-w3m-gnudoit)
	    :active (fboundp 'browse-url-w3m-gnudoit)]
	   ))

	 
       (browse-url-xemacs-init-menu)
       ))

(provide 'browse-url-xemacs)

;;; browse-url-xemacs.el ends here
