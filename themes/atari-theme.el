;; For best results, run this after installing the Atari Classic Chunky font from 
;; http://members.bitstream.net/marksim/atarimac/fonts.html

(deftheme atari
  "Atari 800 Color Theme")

(defun atari-click ()
  "Play the Atari Basic click"
  (when (symbolp this-command)
    (let ((c (symbol-name this-command)))
      ;;(message "c is %s" c)
      (unless
          (or
           (not this-command)
           (string-match-p "^mouse-" c)
           (string-match-p "^mwheel-" c)
           (string-match-p "-wheel-" c))
        (play-sound '(sound :file "~/.atari-click.mp3"))))))

(let ((coding-system-for-write 'raw-text))
  (with-temp-file "~/.atari-click.mp3"
    (progn
      (set-buffer-multibyte nil)
      (insert (base64-decode-string "
H4sIAAAAAAACA/v/ewILAwHgmZeWD6T4gZiZgYGtjSGUEFhFCPwnBIB2Wfo4+roa61laFDGugjlF
pEGFI9GNgaEB5A4ddedKhuEO/oMjiClQ4WqrwQEG1yNS15Z0MPBODW3duYtBcNOqDdvNHZiMa5sc
JqhwsjQq8Hc8/yDEwigwo7K8vPr/9793bxowOPFVmrx7pKRkDAzWjS0n5wuwMST8Z2Dii7DgsP70
4Ie9APPz/00Mjef/7LDjbzBgcXc35WBlanRYEMDZdtujQUNCxODznPfPfx5uYBD0K57zRJHz/wsH
w/QjjA0S9n9kWOynsx/80SD/X4PD+r8ef/P/PwIssgceFFjI8LEzNx54kKDAsHXRV//nXF8XiTK9
+39uyZfNW+7/UzSYssfp8JKw/W3L2FR/mrlH/gvUmyBkkbr/w6OGIIFH+pry/81M1Rw2OPz1WGn9
/2fjxo0dC5mOnul8VP3/v6uwsMKfe7u8/v//6+7/b3Md2BqIfYwMrGvFV9VbhK8VWCFe/q9Ufenc
Zf8/crYsfSQmq7T1j6mBof7JTVbnP3WWNEw8rPu/XYRpQrtgpcf/nxtXGrE2sb9y9bz//wxvb48r
h5heir/w/f+/NGbMZH/29VL4//+vrOq/zl5lnyYXGh17/tPLLTP2/b+TFunt+O//70ksnA1M+03D
2lIeMIQ/W3XhzBxgvDEukWFgUGBgMGluYGBhYRJ8FMHe/GHBkssVG1QEz8lZHq9J8ZAX60zbePLH
hZyIPCvPlf+lRExmPpXU2+b/hP3Ozq4ImcT7Dw+UcCiy37z+i0ntHMOlujM2q7btnv/0cqZh7f8d
ZzZNa/7DwaJ8ZAM/+0NHz2UfEzyU59gU9/04McFOV+hs8uSPC25suFMyOfifiUam8Smj2Jz5J/h3
FotssDi8/3DDCwEn/s3rv7D4zmFcBstmekaraAJobT7hwgYYQwX9HxgYMoHxwsHAwAuMFkZQDDFA
YghYmLCsGgUDCADAFmVzhgYAAA=="))
      (zlib-decompress-region (point-min) (point-max)))))

(define-minor-mode atari-click-mode
  "A minor mode that plays the old atari click on key press."
  nil nil nil
  (if atari-click-mode
      (add-hook 'pre-command-hook 'atari-click)
    (remove-hook 'pre-command-hook 'atari-click)))

(defcustom atari-click-mode nil
  "Toggle atari-click-mode.
     Setting this variable directly does not take effect;
     use either \\[customize] or the function `atari-click-mode'."
  :set 'custom-set-minor-mode
  :initialize 'custom-initialize-default
  :type    'boolean)

(let
    ((inverse '((nil (:inverse-video t))))
     (empty '((nil nil))))
  (custom-theme-set-faces
   'atari
   '(default ((t (:inherit nil :stipple nil :background "#175A68" :foreground "#67ABB9" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :width normal :height 120 :family "Atari Classic Chunky"))))
   '(cursor ((t ( :background "#67ABB9" ))))
   `(fringe ,empty)
   `(highlight ,inverse)
   `(mode-line ,inverse)
   `(mode-line-inactive ,inverse)
   `(region ,inverse)
   `(button ,inverse)
   `(variable-pitch ,empty)
   `(widget-field ,inverse)
   `(link ,inverse)
   `(link-visited ,inverse)
   `(minibuffer-prompt ,empty)
   `(font-lock-builtin-face ,empty)
   `(font-lock-function-name-face ,empty)
   `(font-lock-preprocessor-face ,empty)
   `(font-lock-string-face ,empty)
   `(font-lock-type-face ,empty)
   `(font-lock-keyword-face ,empty)
   `(font-lock-negation-char-face ,empty)
   `(font-lock-reference-face ,empty)
   `(font-lock-variable-name-face ,empty)
   `(font-lock-warning-face ,empty)
   `(font-lock-comment-delimiter-face ,empty)
   `(font-lock-comment-face ,empty)
   `(font-lock-constant-face ,empty)
   `(font-lock-doc-face ,empty)
   ))

(custom-theme-set-variables
 'atari
 '(blink-cursor-mode nil)
 '(show-paren-mode nil)
 '(atari-click-mode t)
 '(fringe-mode '(60 . 0)))

(provide-theme 'atari)
