;;;; A major mode for editing CSS.

;;; Adds font locking, some rather primitive indentation handling and
;;; some typing help.
;;;

(defvar cssm-version "0.11"
  "The current version number of `css-mode'.")

;;; copyright (c) 1998 Lars Marius Garshol, larsga@ifi.uio.no
;;; $Id: css-mode.el,v 1.9 2000/01/05 21:21:56 larsga Exp $

;;; css-mode is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License
;;; as published by the Free Software Foundation; either version 2
;;; of the License, or (at your option) any later version.
;;;
;;; css-mode is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with css-mode; if not, write to the Free Software
;;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

; Send me an email if you want new features (or if you add them yourself).
; I will do my best to preserve the API to functions not explicitly marked
; as internal and variables shown as customizable. I make no promises about
; the rest.

; Bug reports are very welcome. New versions of the package will appear at
; http://www.garshol.priv.no/download/software/css-mode/
; You can register at the same address if you want to be notified when a
; new version appears.

; Thanks to Philippe Le Hegaret, Kjetil Kjernsmo, Alf-Ivar Holm and
; Alfred Correira for much useful feedback. Alf-Ivar Holm also contributed
; patches.

; This version should be up to date with the following:
; CSS1: http://www.w3.org/TR/1999/REC-CSS1-19990111
; CSS2: http://www.w3.org/TR/1998/REC-CSS2-19980512

; To install, put this in your .emacs:
;
; (autoload 'css-mode "css-mode")
; (setq auto-mode-alist       
;      (cons '("\\.css\\'" . css-mode) auto-mode-alist))

;; Todo:

; - must not color URL file name extensions as class selectors (*.css)
; - color [] and url() constructs correctly, even if quoted strings present
; - disregard anything inside strings

;; Possible later additions:
;
; - forward/backward style/@media rule commands
; - more complete syntax table

;; Required modules

(require 'apropos)
(require 'font-lock)
(require 'cl)

;;; The code itself

; Customizable variables:

(defgroup css nil
  "Major mode for editing CSS files."
  :group 'languages
  :group 'hypermedia
  :prefix "cssm-")

(defcustom cssm-indent-level 2
  "*The indentation level inside @media rules."
  :type 'integer
  :group 'css)

(defcustom cssm-mirror-mode t
  "*Whether brackets, quotes etc should be mirrored automatically on insertion."
  :type 'boolean
  :group 'css)

(defcustom cssm-newline-before-closing-bracket nil
  "*In `cssm-mirror-mode', controls whether a newline should be inserted
before the closing bracket or not."
  :type 'boolean
  :group 'css)

(defcustom cssm-indent-function #'cssm-c-style-indenter
  "*Which function to use when deciding which column to indent to.
Built-in choices are `cssm-old-style-indenter' and `cssm-c-style-indenter'."
  :type 'function
  :group 'css)
  
; The rest of the code:

(defvar cssm-properties
  '(
    "ascent"                        ;   2
    "azimuth"                       ;   2
    "background"                    ; 1 2
    "background-attachment"         ; 1 2
    "background-color"              ; 1 2
    "background-image"              ; 1 2
    "background-position"           ; 1 2
    "background-repeat"             ; 1 2
    "baseline"                      ;   2
    "bbox"                          ;   2
    "border"                        ; 1 2
    "border-bottom"                 ; 1 2
    "border-bottom-color"           ;   2
    "border-bottom-style"           ;   2
    "border-bottom-width"           ; 1 2
    "border-color"                  ; 1 2
    "border-collapse"               ;   2
    "border-left"                   ; 1 2
    "border-left-color"             ;   2
    "border-left-style"             ;   2
    "border-left-width"             ; 1 2
    "border-right"                  ; 1 2
    "border-right-color"            ;   2
    "border-right-style"            ;   2
    "border-right-width"            ; 1 2
    "border-spacing"                ;   2
    "border-style"                  ; 1 2
    "border-top"                    ; 1 2
    "border-top-color"              ;   2
    "border-top-style"              ;   2
    "border-top-width"              ; 1 2
    "border-width"                  ; 1 2
    "bottom"                        ;   2
    "cap-height"                    ;   2
    "caption-size"                  ;   2
    ;"cell-spacing"                 ;   2         removed in PR-CSS2-19980324
    "centerline"                    ;   2
    "clear"                         ; 1 2
    "clip"                          ;   2
    "color"                         ; 1 2
    ;"column-span"                  ;   2         removed in REC-CSS2-19980512
    "content"                       ;   2
    "counter-increment"             ;   2
    "counter-reset"                 ;   2
    "cue"                           ;   2
    "cue-after"                     ;   2
    "cue-before"                    ;   2
    "cursor"                        ;   2
    "definition-src"                ;   2
    "descent"                       ;   2
    "direction"                     ;   2
    "display"                       ; 1 2
    "elevation"                     ;   2
    "empty-cells"                   ;   2
    "float"                         ; 1 2
    "font"                          ; 1 2
    "font-family"                   ; 1 2
    "font-size"                     ; 1 2
    "font-size-adjust"              ;   2
    "font-stretch"                  ;   2
    "font-style"                    ; 1 2
    "font-variant"                  ; 1 2
    "font-weight"                   ; 1 2
    "height"                        ; 1 2
    "left"                          ;   2
    "letter-spacing"                ; 1 2
    "line-height"                   ; 1 2
    "list-style"                    ; 1 2
    "list-style-image"              ; 1 2
    "list-style-position"           ; 1 2
    "list-style-type"               ; 1 2
    "margin"                        ; 1 2
    "margin-bottom"                 ; 1 2
    "margin-left"                   ; 1 2
    "margin-right"                  ; 1 2
    "margin-top"                    ; 1 2
    "marker-offset"                 ;   2
    "marks"                         ;   2
    "mathline"                      ;   2
    "max-height"                    ;   2
    "max-width"                     ;   2
    "min-height"                    ;   2
    "min-width"                     ;   2
    "orphans"                       ;   2
    "outline"                       ;   2
    "outline-color"                 ;   2
    "outline-style"                 ;   2
    "outline-width"                 ;   2
    "overflow"                      ;   2
    "padding"                       ; 1 2
    "padding-bottom"                ; 1 2
    "padding-left"                  ; 1 2
    "padding-right"                 ; 1 2
    "padding-top"                   ; 1 2
    "page"                          ;   2
    "page-break-after"              ;   2
    "page-break-before"             ;   2
    "page-break-inside"             ;   2
    "panose-1"                      ;   2
    "pause"                         ;   2
    "pause-after"                   ;   2
    "pause-before"                  ;   2
    "pitch"                         ;   2
    "pitch-range"                   ;   2
    "play-during"                   ;   2
    "position"                      ;   2
    "quotes"                        ;   2
    "richness"                      ;   2
    "right"                         ;   2
    ;"row-span"                     ;   2         removed in REC-CSS2-19980512
    "size"                          ;   2
    "slope"                         ;   2
    "speak"                         ;   2
    ;"speak-date"                   ;   2         removed in PR-CSS2-19980324
    "speak-header"                  ;   2
    "speak-numeral"                 ;   2
    "speak-punctuation"             ;   2
    ;"speak-time"                   ;   2         removed in PR-CSS2-19980324
    "speech-rate"                   ;   2
    "src"                           ;   2
    "stemh"                         ;   2
    "stemv"                         ;   2
    "stress"                        ;   2
    "table-layout"                  ;   2
    "text-align"                    ; 1 2
    "text-decoration"               ; 1 2
    "text-indent"                   ; 1 2
    "text-shadow"                   ;   2
    "text-transform"                ; 1 2
    "top"                           ;   2
    "topline"                       ;   2
    "unicode-bidi"                  ;   2
    "unicode-range"                 ;   2
    "units-per-em"                  ;   2
    "vertical-align"                ; 1 2
    "visibility"                    ;   2
    "voice-family"                  ;   2
    "volume"                        ;   2
    "white-space"                   ; 1 2
    "widows"                        ;   2
    "width"                         ; 1 2
    "widths"                        ;   2
    "word-spacing"                  ; 1 2
    "x-height"                      ;   2
    "z-index"                       ;   2
    )
  "A list of all CSS properties and descriptors.")

(defvar cssm-properties-alist
  (mapcar (lambda(prop)
            (cons (concat prop ":") nil)) cssm-properties)
  "An association list of the CSS properties for completion use.")

(defvar cssm-keywords
  (append '(
            "@charset"              ;   2
            "@font-face"            ;   2
            "@import"               ;   2
            "!\\s-*important"       ; 1 2
            "@media"                ;   2
            "@page"                 ;   2
            )
          (mapcar (lambda(property)
                    (concat property "\\s-*:"))
                  cssm-properties)
          )
  "A list of all CSS keywords.")

(defvar cssm-pseudos '(
    "active"                        ; 1 2
    "after"                         ;   2
    "before"                        ;   2
    "first-child"                   ;   2
    "first-letter"                  ; 1 2
    "first-line"                    ; 1 2
    "focus"                         ;   2
    "hover"                         ;   2
    "lang"                          ;   2
    "link"                          ; 1 2
    "visited"                       ; 1 2
    )
  "A list of all CSS pseudo-classes and elements.")

; internal
(defun cssm-list-2-regexp(altlist)
  "Takes a list and returns the regexp \\(elem1\\|elem2\\|...\\)"
  (let ((regexp "\\("))
    (mapcar (lambda(elem)
              (setq regexp (concat regexp elem "\\|")))
            altlist)
    (concat (substring regexp 0 -2) ; cutting the last "\\|"
            "\\)")
    ))

(defvar cssm-font-lock-keywords
  (list
   (cons (cssm-list-2-regexp cssm-keywords) font-lock-keyword-face)
   (cons "\\.[a-zA-Z][-a-zA-Z0-9.]+" font-lock-variable-name-face)
   (cons (concat ":" (cssm-list-2-regexp cssm-pseudos))
         font-lock-variable-name-face)
   (cons "#[a-fA-F0-9][a-fA-F0-9][a-fA-F0-9]\\([a-fA-F0-9][a-fA-F0-9][a-fA-F0-9]\\)?"
         font-lock-reference-face)
   (cons "\\[.*\\]" font-lock-variable-name-face)
   (cons "#[-a-zA-Z0-9]*" font-lock-function-name-face)
   (cons "rgb(\\s-*[0-9]+\\(\\.[0-9]+\\s-*%\\s-*\\)?\\s-*,\\s-*[0-9]+\\(\\.[0-9]+\\s-*%\\s-*\\)?\\s-*,\\s-*[0-9]+\\(\\.[0-9]+\\s-*%\\s-*\\)?\\s-*)"
         font-lock-reference-face)
   )
  "Rules for highlighting CSS style sheets.")

(defvar cssm-mode-map ()
  "Keymap used in CSS mode.")
(when (not cssm-mode-map)
  (setq cssm-mode-map (make-sparse-keymap))
  (define-key cssm-mode-map (read-kbd-macro "C-c C-c") 'cssm-insert-comment)
  (define-key cssm-mode-map (read-kbd-macro "C-c C-u") 'cssm-insert-url)
  (define-key cssm-mode-map (read-kbd-macro "}") 'cssm-insert-right-brace-and-indent)
  (define-key cssm-mode-map (read-kbd-macro "M-TAB") 'cssm-complete-property))

;;; Cross-version compatibility layer

(when (not (or (apropos-macrop 'kbd)
               (fboundp 'kbd)))
  (defmacro kbd (keys)
    "Convert KEYS to the internal Emacs key representation.
KEYS should be a string constant in the format used for
saving keyboard macros (see `insert-kbd-macro')."
    (read-kbd-macro keys)))

;;; Auto-indentation support

; internal
(defun cssm-insert-right-brace-and-indent()
  (interactive)
  (insert "}")
  (cssm-indent-line))

; internal
(defun cssm-inside-atmedia-rule()
  "Decides if point is currently inside an @media rule."
  (let ((orig-pos (point))
        (atmedia (re-search-backward "@media" 0 t))
        (balance 1)   ; used to keep the {} balance, 1 because we start on a {
        )
    ; Going to the accompanying {
    (re-search-forward "{" (point-max) t)
    (if (null atmedia)
        nil  ; no @media before this point => not inside
      (while (and (< (point) orig-pos)
                  (< 0 balance))
        (if (null (re-search-forward "[{}]" (point-max) 0))
            (goto-char (point-max)) ; break
          (setq balance
                (if (string= (match-string 0) "{")
                    (+ balance 1)
                  (- balance 1)))))
      (= balance 1))
    ))

; internal
(defun cssm-rule-is-atmedia()
  "Decides if point is currently on the { of an @media or ordinary style rule."
  (let ((result (re-search-backward "[@}{]" 0 t)))
    (if (null result)
        nil
      (string= (match-string 0) "@"))))

; internal
(defun cssm-find-column(first-char)
  "Find which column to indent to." 

  ; Find out where to indent to by looking at previous lines
  ; spinning backwards over comments
  (let (pos)
    (while (and (setq pos (re-search-backward (cssm-list-2-regexp
                                               '("/\\*" "\\*/" "{" "}"))
                                              (point-min) t))
                (string= (match-string 0) "*/"))
      (search-backward "/*" (point-min) t))

    ; did the last search find anything?
    (if pos
        (save-excursion
          (let ((construct      (match-string 0))
                (column         (current-column)))
            (apply cssm-indent-function
                   (list (cond
                          ((string= construct "{")
                           (cond
                            ((cssm-rule-is-atmedia)
                             'inside-atmedia)
                            ((cssm-inside-atmedia-rule)
                             'inside-rule-and-atmedia)
                            (t
                             'inside-rule)))
                          ((string= construct "/*")
                           'inside-comment)
                          ((string= construct "}")
                           (if (cssm-inside-atmedia-rule)
                               'inside-atmedia
                             'outside))
                          (t 'outside))
                         column
                         first-char))))
      
      (apply cssm-indent-function
             (list 'outside
                   (current-column)
                   first-char)))))

(defun cssm-indent-line()
  "Indents the current line."
  (interactive)
  (beginning-of-line)
  (let* ((beg-of-line (point))
         (pos (re-search-forward "[]@#a-zA-Z0-9;,.\"{}/*\n:[]" (point-max) t))
         (first (match-string 0))
         (start (match-beginning 0)))

    (goto-char beg-of-line)

    (let ((indent-column (cssm-find-column first)))
      (goto-char beg-of-line)

      ; Remove all leading whitespace on this line (
      (if (not (or (null pos)
                   (= beg-of-line start)))
          (kill-region beg-of-line start))

      (goto-char beg-of-line)
    
      ; Indent
      (indent-to indent-column))))

;;; Indent-style functions

(defun cssm-old-style-indenter(position column first-char-on-line)
  "Old-style indentation for CSS buffers."
  (cond
   ((eq position 'inside-atmedia)
    (if (string= "}" first-char-on-line)
        0
      cssm-indent-level))
   
   ((eq position 'inside-rule)
    (+ column 2))

   ((eq position 'inside-rule-and-atmedia)
    (+ column 2))

   ((eq position 'inside-comment)
    (+ column 3))

   ((eq position 'outside)
    0)))

(defun cssm-c-style-indenter(position column first-char-on-line)
  "C-style indentation for CSS buffers."
  (cond
   ((or (eq position 'inside-atmedia)
        (eq position 'inside-rule))
    (if (string= "}" first-char-on-line)
        0
      cssm-indent-level))

   ((eq position 'inside-rule-and-atmedia)
    (if (string= "}" first-char-on-line)
        cssm-indent-level
      (* 2 cssm-indent-level)))

   ((eq position 'inside-comment)
    (+ column 3))

   ((eq position 'outside)
    0)))

;;; Typing shortcuts

(define-skeleton cssm-insert-curlies
  "Inserts a pair of matching curly parenthesises." nil
  "{ " _ (if cssm-newline-before-closing-bracket "\n" " ")
  "}")

(define-skeleton cssm-insert-quotes
  "Inserts a pair of matching quotes." nil
  "\"" _ "\"")

(define-skeleton cssm-insert-parenthesises
  "Inserts a pair of matching parenthesises." nil
  "(" _ ")")

(define-skeleton cssm-insert-comment
  "Inserts a full comment." nil
  "/* " _ " */")

(define-skeleton cssm-insert-url
  "Inserts a URL." nil
  "url(" _ ")")

(define-skeleton cssm-insert-brackets
  "Inserts a pair of matching brackets." nil
  "[" _ "]")

(defun cssm-enter-mirror-mode()
  "Turns on mirror mode, where quotes, brackets etc are mirrored automatically
   on insertion."
  (interactive)
  (define-key cssm-mode-map (read-kbd-macro "{")  'cssm-insert-curlies)
  (define-key cssm-mode-map (read-kbd-macro "\"") 'cssm-insert-quotes)
  (define-key cssm-mode-map (read-kbd-macro "(")  'cssm-insert-parenthesises)
  (define-key cssm-mode-map (read-kbd-macro "[")  'cssm-insert-brackets))

(defun cssm-leave-mirror-mode()
  "Turns off mirror mode."
  (interactive)
  (define-key cssm-mode-map (read-kbd-macro "{")  'self-insert-command)
  (define-key cssm-mode-map (read-kbd-macro "\"") 'self-insert-command)
  (define-key cssm-mode-map (read-kbd-macro "(")  'self-insert-command)
  (define-key cssm-mode-map (read-kbd-macro "[")  'self-insert-command))

;;; Property completion

(defun cssm-property-at-point()
  "If point is at the end of a property name: returns it."
  (let ((end (point))
        (start (+ (re-search-backward "[^-A-Za-z]") 1)))
    (goto-char end)
    (buffer-substring start end)))

; internal
(defun cssm-maximum-common(alt1 alt2)
  "Returns the maximum common starting substring of alt1 and alt2."
  (let* ((maxlen (min (length alt1) (length alt2)))
         (alt1 (substring alt1 0 maxlen))
         (alt2 (substring alt2 0 maxlen)))
    (while (not (string= (substring alt1 0 maxlen)
                         (substring alt2 0 maxlen)))
      (setq maxlen (- maxlen 1)))
    (substring alt1 0 maxlen)))

; internal
(defun cssm-common-beginning(alts)
  "Returns the maximum common starting substring of all alts elements."
  (let ((common (car alts)))
    (dolist (alt (cdr alts) common)
      (setq common (cssm-maximum-common alt common)))))

(defun cssm-complete-property-frame(completions)
  ; This code stolen from message.el. Kudos to larsi.
  (let ((cur (current-buffer)))
    (pop-to-buffer "*Completions*")
    (buffer-disable-undo (current-buffer))
    (let ((buffer-read-only nil))
      (erase-buffer)
      (let ((standard-output (current-buffer)))
        (display-completion-list (sort completions 'string<)))
      (goto-char (point-min))
      (pop-to-buffer cur))))

(defun cssm-complete-property()
  "Completes the CSS property being typed at point."
  (interactive)
  (let* ((prop   (cssm-property-at-point))
         (alts   (all-completions prop cssm-properties-alist))
         (proplen (length prop)))
    (if (= (length alts) 1)
        (insert (substring (car alts) proplen))
      (let ((beg (cssm-common-beginning alts)))
        (if (not (string= beg prop))
            (insert (substring beg proplen))
          (insert (substring
                   (completing-read "Property: " cssm-properties-alist nil
                                    nil prop)
                   proplen)))))))

;;;###autoload
(defun css-mode()
  "Major mode for editing CSS style sheets.
\\{cssm-mode-map}"
  (interactive)

  ; Initializing
  (kill-all-local-variables)

  ; Setting up indentation handling
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'cssm-indent-line)
  
  ; Setting up font-locking
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(cssm-font-lock-keywords nil t nil nil))

  ; Setting up typing shortcuts
  (make-local-variable 'skeleton-end-hook)
  (setq skeleton-end-hook nil)
  
  (when cssm-mirror-mode
    (cssm-enter-mirror-mode))
  
  (use-local-map cssm-mode-map)
  
  ; Setting up syntax recognition
  (make-local-variable 'comment-start)
  (make-local-variable 'comment-end)
  (make-local-variable 'comment-start-skip)

  (setq comment-start "/* "
        comment-end " */"
        comment-start-skip "/\\*[ \n\t]+")

  ; Setting up syntax table
  (modify-syntax-entry ?* ". 23")
  (modify-syntax-entry ?/ ". 14")
  
  ; Final stuff, then we're done
  (setq mode-name "CSS"
        major-mode 'css-mode)
  (run-hooks 'css-mode-hook))

;;;###autoload(add-to-list 'auto-mode-alist '("\\.css$" . css-mode))

(provide 'css-mode)

;; CSS-mode ends here
