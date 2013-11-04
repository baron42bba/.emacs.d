;;;  blowfish.el -- block cipher

;; Copyright (C) 1998 Ray Jones

;; Author: Ray Jones, rjones@pobox.com
;; Keywords: Blowfish, Schneier, oink, cipher, cypher, cryptography
;; Created: 1998-04-01

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, you can either send email to this
;; program's maintainer or write to: The Free Software Foundation,
;; Inc.; 675 Massachusetts Avenue; Cambridge, MA 02139, USA.

;;; Commentary:

;; this code is Bruce Schneier's Blowfish encryption algorithm.  it is
;; fast, license-free, unpatented, and beleived secure.  it takes a
;; key of length 32-448 bits.  it works on 64-bit blocks.  further
;; information is available in Applied Cryptography by Schneier or at
;; http://www.counterpane.com/blowfish.html

;; to use, given a key:
;;
;; (let ((subkeys (blowfish-generate-keys key)) ;; returns a pair, the
;;                                              ;; P-array and the S-boxes
;;       (data [1 2 3 4]))
;;   ;; encipher data in place
;;   (blowfish-cipher-block data (car subkeys) (cdr subkeys))
;;   ;; decipher in place
;;   (blowfish-cipher-block data (car subkeys) (cdr subkeys) 'decrypt)
;;   ;; data should now be back to its starting value
;;   (assert (equal data [1 2 3 4])))

;; note: the time taken to generate subkeys is quite substantial, but
;; encryption and decryption afterwards is quite fast.

;; keys are vectors of 16-bit integers, with the vector being a
;; positive, even number of elements long.


;;; Code:
(require 'cl)

;; blowfish is a 64-bit algorithm, which operates on 32-bit pieces.
;; this code stores 32-bit numbers as vectors w/ 2 16-bit elements.

;; helper functions
(defsubst blowfish-xor (a b result)
  (aset result 0 (logxor (aref a 0) (aref b 0)))
  (aset result 1 (logxor (aref a 1) (aref b 1)))
  (assert (and (< (aref result 0) (ash 1 16))
	       (< (aref result 1) (ash 1 16))) nil
	       ""))

(defsubst blowfish-add (a b result)
  (let* ((low (+ (aref a 1) (aref b 1)))
	 (high (logand (+ (aref a 0) (aref b 0) (ash low -16))
		       #xFFFF)))
    (aset result 0 high)
    (aset result 1 (logand low #xFFFF)))
  (assert (and (< (aref result 0) (ash 1 16))
	       (< (aref result 1) (ash 1 16))) nil
	       ""))

(defun blowfish-cipher-block (data p-array s-boxes &optional direction)
  "encode or decode with the blowfish cipher, in place.
first arg is vector of 4 16-bit words (MSB order).
second arg is P-array to encrypt/decrypt with.
third arg is S-boxes to encrypt/decrypt with.
optional fourth arg is direction \(eq 'decrypt to decrypt\)."

  ;; split the data into two 32 bit pieces (stored as 2 16-bit pieces
  ;; each.)
  (let ((xl (vector (aref data 0) (aref data 1)))
	(xr (vector (aref data 2) (aref data 3)))
        (reverse (eq direction 'decrypt)))
    ;; 16 rounds
    (dotimes (idx 16)
      ;; mix in the p-array.  note the reverse order for decryption.
      (let ((p-val (aref p-array (if reverse (- 17 idx) idx))))
	(blowfish-xor xl p-val xl))
      ;; apply F
      (blowfish-f xl s-boxes xr)
      ;; swap low and high 32 bits
      (rotatef xl xr))

    ;; undo the last swap
    (rotatef xl xr)
    ;; mixin last two elements of P-array
    ;; (when comparing to section in AC 2nd Ed, note that this is 0
    ;; indexed, and the reverse order for decryption.
    (let ((p-val-16 (aref p-array (if reverse 1 16)))
	  (p-val-17 (aref p-array (if reverse 0 17))))
      (blowfish-xor xl p-val-17 xl)
      (blowfish-xor xr p-val-16 xr))

    ;; put the result back into the input vector
    (aset data 0 (aref xl 0))
    (aset data 1 (aref xl 1))
    (aset data 2 (aref xr 0))
    (aset data 3 (aref xr 1))
    
    ;; clean up
    (fillarray xl 0)
    (fillarray xr 0)))

(defvar *blowfish-f-scratch* [0 0]
  "scratch area for the blowfish-f function, to reduce allocation")

(defun blowfish-f (x s-boxes dest)
  "perform the blowfish F-function.  takes vector of two-16-bit
numbers and S-boxes to use as args, and the destination to xor the
result into.  uses *blowfish-f-scratch* to reduce allocation."
  ;; S-boxes are indexed into via 8-bit pieces of X
  (let* ((x0 (aref x 0))
	 (Sa (aref (aref s-boxes 0) (ash x0 -8)))
	 (Sb (aref (aref s-boxes 1) (logand x0 #xFF)))
	 (x1 (aref x 1))
	 (Sc (aref (aref s-boxes 2) (ash x1 -8)))
	 (Sd (aref (aref s-boxes 3) (logand x1 #xFF))))

    (blowfish-add Sa Sb *blowfish-f-scratch*)
    (blowfish-xor Sc *blowfish-f-scratch* *blowfish-f-scratch*)
    (blowfish-add Sd *blowfish-f-scratch* *blowfish-f-scratch*)
    (blowfish-xor *blowfish-f-scratch* dest dest)))

;; initialization constants for the P-array and S-boxes

(defconst blowfish-s-box-inits
  [
   [
    [#xd131 #x0ba6] [#x98df #xb5ac] [#x2ffd #x72db] [#xd01a #xdfb7]
    [#xb8e1 #xafed] [#x6a26 #x7e96] [#xba7c #x9045] [#xf12c #x7f99]
    [#x24a1 #x9947] [#xb391 #x6cf7] [#x0801 #xf2e2] [#x858e #xfc16]
    [#x6369 #x20d8] [#x7157 #x4e69] [#xa458 #xfea3] [#xf493 #x3d7e]
    [#x0d95 #x748f] [#x728e #xb658] [#x718b #xcd58] [#x8215 #x4aee]
    [#x7b54 #xa41d] [#xc25a #x59b5] [#x9c30 #xd539] [#x2af2 #x6013]
    [#xc5d1 #xb023] [#x2860 #x85f0] [#xca41 #x7918] [#xb8db #x38ef]
    [#x8e79 #xdcb0] [#x603a #x180e] [#x6c9e #x0e8b] [#xb01e #x8a3e]
    [#xd715 #x77c1] [#xbd31 #x4b27] [#x78af #x2fda] [#x5560 #x5c60]
    [#xe655 #x25f3] [#xaa55 #xab94] [#x5748 #x9862] [#x63e8 #x1440]
    [#x55ca #x396a] [#x2aab #x10b6] [#xb4cc #x5c34] [#x1141 #xe8ce]
    [#xa154 #x86af] [#x7c72 #xe993] [#xb3ee #x1411] [#x636f #xbc2a]
    [#x2ba9 #xc55d] [#x7418 #x31f6] [#xce5c #x3e16] [#x9b87 #x931e]
    [#xafd6 #xba33] [#x6c24 #xcf5c] [#x7a32 #x5381] [#x2895 #x8677]
    [#x3b8f #x4898] [#x6b4b #xb9af] [#xc4bf #xe81b] [#x6628 #x2193]
    [#x61d8 #x09cc] [#xfb21 #xa991] [#x487c #xac60] [#x5dec #x8032]
    [#xef84 #x5d5d] [#xe985 #x75b1] [#xdc26 #x2302] [#xeb65 #x1b88]
    [#x2389 #x3e81] [#xd396 #xacc5] [#x0f6d #x6ff3] [#x83f4 #x4239]
    [#x2e0b #x4482] [#xa484 #x2004] [#x69c8 #xf04a] [#x9e1f #x9b5e]
    [#x21c6 #x6842] [#xf6e9 #x6c9a] [#x670c #x9c61] [#xabd3 #x88f0]
    [#x6a51 #xa0d2] [#xd854 #x2f68] [#x960f #xa728] [#xab51 #x33a3]
    [#x6eef #x0b6c] [#x137a #x3be4] [#xba3b #xf050] [#x7efb #x2a98]
    [#xa1f1 #x651d] [#x39af #x0176] [#x66ca #x593e] [#x8243 #x0e88]
    [#x8cee #x8619] [#x456f #x9fb4] [#x7d84 #xa5c3] [#x3b8b #x5ebe]
    [#xe06f #x75d8] [#x85c1 #x2073] [#x401a #x449f] [#x56c1 #x6aa6]
    [#x4ed3 #xaa62] [#x363f #x7706] [#x1bfe #xdf72] [#x429b #x023d]
    [#x37d0 #xd724] [#xd00a #x1248] [#xdb0f #xead3] [#x49f1 #xc09b]
    [#x0753 #x72c9] [#x8099 #x1b7b] [#x25d4 #x79d8] [#xf6e8 #xdef7]
    [#xe3fe #x501a] [#xb679 #x4c3b] [#x976c #xe0bd] [#x04c0 #x06ba]
    [#xc1a9 #x4fb6] [#x409f #x60c4] [#x5e5c #x9ec2] [#x196a #x2463]
    [#x68fb #x6faf] [#x3e6c #x53b5] [#x1339 #xb2eb] [#x3b52 #xec6f]
    [#x6dfc #x511f] [#x9b30 #x952c] [#xcc81 #x4544] [#xaf5e #xbd09]
    [#xbee3 #xd004] [#xde33 #x4afd] [#x660f #x2807] [#x192e #x4bb3]
    [#xc0cb #xa857] [#x45c8 #x740f] [#xd20b #x5f39] [#xb9d3 #xfbdb]
    [#x5579 #xc0bd] [#x1a60 #x320a] [#xd6a1 #x00c6] [#x402c #x7279]
    [#x679f #x25fe] [#xfb1f #xa3cc] [#x8ea5 #xe9f8] [#xdb32 #x22f8]
    [#x3c75 #x16df] [#xfd61 #x6b15] [#x2f50 #x1ec8] [#xad05 #x52ab]
    [#x323d #xb5fa] [#xfd23 #x8760] [#x5331 #x7b48] [#x3e00 #xdf82]
    [#x9e5c #x57bb] [#xca6f #x8ca0] [#x1a87 #x562e] [#xdf17 #x69db]
    [#xd542 #xa8f6] [#x287e #xffc3] [#xac67 #x32c6] [#x8c4f #x5573]
    [#x695b #x27b0] [#xbbca #x58c8] [#xe1ff #xa35d] [#xb8f0 #x11a0]
    [#x10fa #x3d98] [#xfd21 #x83b8] [#x4afc #xb56c] [#x2dd1 #xd35b]
    [#x9a53 #xe479] [#xb6f8 #x4565] [#xd28e #x49bc] [#x4bfb #x9790]
    [#xe1dd #xf2da] [#xa4cb #x7e33] [#x62fb #x1341] [#xcee4 #xc6e8]
    [#xef20 #xcada] [#x3677 #x4c01] [#xd07e #x9efe] [#x2bf1 #x1fb4]
    [#x95db #xda4d] [#xae90 #x9198] [#xeaad #x8e71] [#x6b93 #xd5a0]
    [#xd08e #xd1d0] [#xafc7 #x25e0] [#x8e3c #x5b2f] [#x8e75 #x94b7]
    [#x8ff6 #xe2fb] [#xf212 #x2b64] [#x8888 #xb812] [#x900d #xf01c]
    [#x4fad #x5ea0] [#x688f #xc31c] [#xd1cf #xf191] [#xb3a8 #xc1ad]
    [#x2f2f #x2218] [#xbe0e #x1777] [#xea75 #x2dfe] [#x8b02 #x1fa1]
    [#xe5a0 #xcc0f] [#xb56f #x74e8] [#x18ac #xf3d6] [#xce89 #xe299]
    [#xb4a8 #x4fe0] [#xfd13 #xe0b7] [#x7cc4 #x3b81] [#xd2ad #xa8d9]
    [#x165f #xa266] [#x8095 #x7705] [#x93cc #x7314] [#x211a #x1477]
    [#xe6ad #x2065] [#x77b5 #xfa86] [#xc754 #x42f5] [#xfb9d #x35cf]
    [#xebcd #xaf0c] [#x7b3e #x89a0] [#xd641 #x1bd3] [#xae1e #x7e49]
    [#x0025 #x0e2d] [#x2071 #xb35e] [#x2268 #x00bb] [#x57b8 #xe0af]
    [#x2464 #x369b] [#xf009 #xb91e] [#x5563 #x911d] [#x59df #xa6aa]
    [#x78c1 #x4389] [#xd95a #x537f] [#x207d #x5ba2] [#x02e5 #xb9c5]
    [#x8326 #x0376] [#x6295 #xcfa9] [#x11c8 #x1968] [#x4e73 #x4a41]
    [#xb347 #x2dca] [#x7b14 #xa94a] [#x1b51 #x0052] [#x9a53 #x2915]
    [#xd60f #x573f] [#xbc9b #xc6e4] [#x2b60 #xa476] [#x81e6 #x7400]
    [#x08ba #x6fb5] [#x571b #xe91f] [#xf296 #xec6b] [#x2a0d #xd915]
    [#xb663 #x6521] [#xe7b9 #xf9b6] [#xff34 #x052e] [#xc585 #x5664]
    [#x53b0 #x2d5d] [#xa99f #x8fa1] [#x08ba #x4799] [#x6e85 #x076a]
    ]
   [
   [#x4b7a #x70e9] [#xb5b3 #x2944] [#xdb75 #x092e] [#xc419 #x2623]
   [#xad6e #xa6b0] [#x49a7 #xdf7d] [#x9cee #x60b8] [#x8fed #xb266]
   [#xecaa #x8c71] [#x699a #x17ff] [#x5664 #x526c] [#xc2b1 #x9ee1]
   [#x1936 #x02a5] [#x7509 #x4c29] [#xa059 #x1340] [#xe418 #x3a3e]
   [#x3f54 #x989a] [#x5b42 #x9d65] [#x6b8f #xe4d6] [#x99f7 #x3fd6]
   [#xa1d2 #x9c07] [#xefe8 #x30f5] [#x4d2d #x38e6] [#xf025 #x5dc1]
   [#x4cdd #x2086] [#x8470 #xeb26] [#x6382 #xe9c6] [#x021e #xcc5e]
   [#x0968 #x6b3f] [#x3eba #xefc9] [#x3c97 #x1814] [#x6b6a #x70a1]
   [#x687f #x3584] [#x52a0 #xe286] [#xb79c #x5305] [#xaa50 #x0737]
   [#x3e07 #x841c] [#x7fde #xae5c] [#x8e7d #x44ec] [#x5716 #xf2b8]
   [#xb03a #xda37] [#xf050 #x0c0d] [#xf01c #x1f04] [#x0200 #xb3ff]
   [#xae0c #xf51a] [#x3cb5 #x74b2] [#x2583 #x7a58] [#xdc09 #x21bd]
   [#xd191 #x13f9] [#x7ca9 #x2ff6] [#x9432 #x4773] [#x22f5 #x4701]
   [#x3ae5 #xe581] [#x37c2 #xdadc] [#xc8b5 #x7634] [#x9af3 #xdda7]
   [#xa944 #x6146] [#x0fd0 #x030e] [#xecc8 #xc73e] [#xa475 #x1e41]
   [#xe238 #xcd99] [#x3bea #x0e2f] [#x3280 #xbba1] [#x183e #xb331]
   [#x4e54 #x8b38] [#x4f6d #xb908] [#x6f42 #x0d03] [#xf60a #x04bf]
   [#x2cb8 #x1290] [#x2497 #x7c79] [#x5679 #xb072] [#xbcaf #x89af]
   [#xde9a #x771f] [#xd993 #x0810] [#xb38b #xae12] [#xdccf #x3f2e]
   [#x5512 #x721f] [#x2e6b #x7124] [#x501a #xdde6] [#x9f84 #xcd87]
   [#x7a58 #x4718] [#x7408 #xda17] [#xbc9f #x9abc] [#xe94b #x7d8c]
   [#xec7a #xec3a] [#xdb85 #x1dfa] [#x6309 #x4366] [#xc464 #xc3d2]
   [#xef1c #x1847] [#x3215 #xd908] [#xdd43 #x3b37] [#x24c2 #xba16]
   [#x12a1 #x4d43] [#x2a65 #xc451] [#x5094 #x0002] [#x133a #xe4dd]
   [#x71df #xf89e] [#x1031 #x4e55] [#x81ac #x77d6] [#x5f11 #x199b]
   [#x0435 #x56f1] [#xd7a3 #xc76b] [#x3c11 #x183b] [#x5924 #xa509]
   [#xf28f #xe6ed] [#x97f1 #xfbfa] [#x9eba #xbf2c] [#x1e15 #x3c6e]
   [#x86e3 #x4570] [#xeae9 #x6fb1] [#x860e #x5e0a] [#x5a3e #x2ab3]
   [#x771f #xe71c] [#x4e3d #x06fa] [#x2965 #xdcb9] [#x99e7 #x1d0f]
   [#x803e #x89d6] [#x5266 #xc825] [#x2e4c #xc978] [#x9c10 #xb36a]
   [#xc615 #x0eba] [#x94e2 #xea78] [#xa5fc #x3c53] [#x1e0a #x2df4]
   [#xf2f7 #x4ea7] [#x361d #x2b3d] [#x1939 #x260f] [#x19c2 #x7960]
   [#x5223 #xa708] [#xf713 #x12b6] [#xebad #xfe6e] [#xeac3 #x1f66]
   [#xe3bc #x4595] [#xa67b #xc883] [#xb17f #x37d1] [#x018c #xff28]
   [#xc332 #xddef] [#xbe6c #x5aa5] [#x6558 #x2185] [#x68ab #x9802]
   [#xeece #xa50f] [#xdb2f #x953b] [#x2aef #x7dad] [#x5b6e #x2f84]
   [#x1521 #xb628] [#x2907 #x6170] [#xecdd #x4775] [#x619f #x1510]
   [#x13cc #xa830] [#xeb61 #xbd96] [#x0334 #xfe1e] [#xaa03 #x63cf]
   [#xb573 #x5c90] [#x4c70 #xa239] [#xd59e #x9e0b] [#xcbaa #xde14]
   [#xeecc #x86bc] [#x6062 #x2ca7] [#x9cab #x5cab] [#xb2f3 #x846e]
   [#x648b #x1eaf] [#x19bd #xf0ca] [#xa023 #x69b9] [#x655a #xbb50]
   [#x4068 #x5a32] [#x3c2a #xb4b3] [#x319e #xe9d5] [#xc021 #xb8f7]
   [#x9b54 #x0b19] [#x875f #xa099] [#x95f7 #x997e] [#x623d #x7da8]
   [#xf837 #x889a] [#x97e3 #x2d77] [#x11ed #x935f] [#x1668 #x1281]
   [#x0e35 #x8829] [#xc7e6 #x1fd6] [#x96de #xdfa1] [#x7858 #xba99]
   [#x57f5 #x84a5] [#x1b22 #x7263] [#x9b83 #xc3ff] [#x1ac2 #x4696]
   [#xcdb3 #x0aeb] [#x532e #x3054] [#x8fd9 #x48e4] [#x6dbc #x3128]
   [#x58eb #xf2ef] [#x34c6 #xffea] [#xfe28 #xed61] [#xee7c #x3c73]
   [#x5d4a #x14d9] [#xe864 #xb7e3] [#x4210 #x5d14] [#x203e #x13e0]
   [#x45ee #xe2b6] [#xa3aa #xabea] [#xdb6c #x4f15] [#xfacb #x4fd0]
   [#xc742 #xf442] [#xef6a #xbbb5] [#x654f #x3b1d] [#x41cd #x2105]
   [#xd81e #x799e] [#x8685 #x4dc7] [#xe44b #x476a] [#x3d81 #x6250]
   [#xcf62 #xa1f2] [#x5b8d #x2646] [#xfc88 #x83a0] [#xc1c7 #xb6a3]
   [#x7f15 #x24c3] [#x69cb #x7492] [#x4784 #x8a0b] [#x5692 #xb285]
   [#x095b #xbf00] [#xad19 #x489d] [#x1462 #xb174] [#x2382 #x0e00]
   [#x5842 #x8d2a] [#x0c55 #xf5ea] [#x1dad #xf43e] [#x233f #x7061]
   [#x3372 #xf092] [#x8d93 #x7e41] [#xd65f #xecf1] [#x6c22 #x3bdb]
   [#x7cde #x3759] [#xcbee #x7460] [#x4085 #xf2a7] [#xce77 #x326e]
   [#xa607 #x8084] [#x19f8 #x509e] [#xe8ef #xd855] [#x61d9 #x9735]
   [#xa969 #xa7aa] [#xc50c #x06c2] [#x5a04 #xabfc] [#x800b #xcadc]
   [#x9e44 #x7a2e] [#xc345 #x3484] [#xfdd5 #x6705] [#x0e1e #x9ec9]
   [#xdb73 #xdbd3] [#x1055 #x88cd] [#x675f #xda79] [#xe367 #x4340]
   [#xc5c4 #x3465] [#x713e #x38d8] [#x3d28 #xf89e] [#xf16d #xff20]
   [#x153e #x21e7] [#x8fb0 #x3d4a] [#xe6e3 #x9f2b] [#xdb83 #xadf7]
   ]
   [
   [#xe93d #x5a68] [#x9481 #x40f7] [#xf64c #x261c] [#x9469 #x2934]
   [#x4115 #x20f7] [#x7602 #xd4f7] [#xbcf4 #x6b2e] [#xd4a2 #x0068]
   [#xd408 #x2471] [#x3320 #xf46a] [#x43b7 #xd4b7] [#x5000 #x61af]
   [#x1e39 #xf62e] [#x9724 #x4546] [#x1421 #x4f74] [#xbf8b #x8840]
   [#x4d95 #xfc1d] [#x96b5 #x91af] [#x70f4 #xddd3] [#x66a0 #x2f45]
   [#xbfbc #x09ec] [#x03bd #x9785] [#x7fac #x6dd0] [#x31cb #x8504]
   [#x96eb #x27b3] [#x55fd #x3941] [#xda25 #x47e6] [#xabca #x0a9a]
   [#x2850 #x7825] [#x5304 #x29f4] [#x0a2c #x86da] [#xe9b6 #x6dfb]
   [#x68dc #x1462] [#xd748 #x6900] [#x680e #xc0a4] [#x27a1 #x8dee]
   [#x4f3f #xfea2] [#xe887 #xad8c] [#xb58c #xe006] [#x7af4 #xd6b6]
   [#xaace #x1e7c] [#xd337 #x5fec] [#xce78 #xa399] [#x406b #x2a42]
   [#x20fe #x9e35] [#xd9f3 #x85b9] [#xee39 #xd7ab] [#x3b12 #x4e8b]
   [#x1dc9 #xfaf7] [#x4b6d #x1856] [#x26a3 #x6631] [#xeae3 #x97b2]
   [#x3a6e #xfa74] [#xdd5b #x4332] [#x6841 #xe7f7] [#xca78 #x20fb]
   [#xfb0a #xf54e] [#xd8fe #xb397] [#x4540 #x56ac] [#xba48 #x9527]
   [#x5553 #x3a3a] [#x2083 #x8d87] [#xfe6b #xa9b7] [#xd096 #x954b]
   [#x55a8 #x67bc] [#xa115 #x9a58] [#xcca9 #x2963] [#x99e1 #xdb33]
   [#xa62a #x4a56] [#x3f31 #x25f9] [#x5ef4 #x7e1c] [#x9029 #x317c]
   [#xfdf8 #xe802] [#x0427 #x2f70] [#x80bb #x155c] [#x0528 #x2ce3]
   [#x95c1 #x1548] [#xe4c6 #x6d22] [#x48c1 #x133f] [#xc70f #x86dc]
   [#x07f9 #xc9ee] [#x4104 #x1f0f] [#x4047 #x79a4] [#x5d88 #x6e17]
   [#x325f #x51eb] [#xd59b #xc0d1] [#xf2bc #xc18f] [#x4111 #x3564]
   [#x257b #x7834] [#x602a #x9c60] [#xdff8 #xe8a3] [#x1f63 #x6c1b]
   [#x0e12 #xb4c2] [#x02e1 #x329e] [#xaf66 #x4fd1] [#xcad1 #x8115]
   [#x6b23 #x95e0] [#x333e #x92e1] [#x3b24 #x0b62] [#xeebe #xb922]
   [#x85b2 #xa20e] [#xe6ba #x0d99] [#xde72 #x0c8c] [#x2da2 #xf728]
   [#xd012 #x7845] [#x95b7 #x94fd] [#x647d #x0862] [#xe7cc #xf5f0]
   [#x5449 #xa36f] [#x877d #x48fa] [#xc39d #xfd27] [#xf33e #x8d1e]
   [#x0a47 #x6341] [#x992e #xff74] [#x3a6f #x6eab] [#xf4f8 #xfd37]
   [#xa812 #xdc60] [#xa1eb #xddf8] [#x991b #xe14c] [#xdb6e #x6b0d]
   [#xc67b #x5510] [#x6d67 #x2c37] [#x2765 #xd43b] [#xdcd0 #xe804]
   [#xf129 #x0dc7] [#xcc00 #xffa3] [#xb539 #x0f92] [#x690f #xed0b]
   [#x667b #x9ffb] [#xcedb #x7d9c] [#xa091 #xcf0b] [#xd915 #x5ea3]
   [#xbb13 #x2f88] [#x515b #xad24] [#x7b94 #x79bf] [#x763b #xd6eb]
   [#x3739 #x2eb3] [#xcc11 #x5979] [#x8026 #xe297] [#xf42e #x312d]
   [#x6842 #xada7] [#xc66a #x2b3b] [#x1275 #x4ccc] [#x782e #xf11c]
   [#x6a12 #x4237] [#xb792 #x51e7] [#x06a1 #xbbe6] [#x4bfb #x6350]
   [#x1a6b #x1018] [#x11ca #xedfa] [#x3d25 #xbdd8] [#xe2e1 #xc3c9]
   [#x4442 #x1659] [#x0a12 #x1386] [#xd90c #xec6e] [#xd5ab #xea2a]
   [#x64af #x674e] [#xda86 #xa85f] [#xbebf #xe988] [#x64e4 #xc3fe]
   [#x9dbc #x8057] [#xf0f7 #xc086] [#x6078 #x7bf8] [#x6003 #x604d]
   [#xd1fd #x8346] [#xf638 #x1fb0] [#x7745 #xae04] [#xd736 #xfccc]
   [#x8342 #x6b33] [#xf01e #xab71] [#xb080 #x4187] [#x3c00 #x5e5f]
   [#x77a0 #x57be] [#xbde8 #xae24] [#x5546 #x4299] [#xbf58 #x2e61]
   [#x4e58 #xf48f] [#xf2dd #xfda2] [#xf474 #xef38] [#x8789 #xbdc2]
   [#x5366 #xf9c3] [#xc8b3 #x8e74] [#xb475 #xf255] [#x46fc #xd9b9]
   [#x7aeb #x2661] [#x8b1d #xdf84] [#x846a #x0e79] [#x915f #x95e2]
   [#x466e #x598e] [#x20b4 #x5770] [#x8cd5 #x5591] [#xc902 #xde4c]
   [#xb90b #xace1] [#xbb82 #x05d0] [#x11a8 #x6248] [#x7574 #xa99e]
   [#xb77f #x19b6] [#xe0a9 #xdc09] [#x662d #x09a1] [#xc432 #x4633]
   [#xe85a #x1f02] [#x09f0 #xbe8c] [#x4a99 #xa025] [#x1d6e #xfe10]
   [#x1ab9 #x3d1d] [#x0ba5 #xa4df] [#xa186 #xf20f] [#x2868 #xf169]
   [#xdcb7 #xda83] [#x5739 #x06fe] [#xa1e2 #xce9b] [#x4fcd #x7f52]
   [#x5011 #x5e01] [#xa706 #x83fa] [#xa002 #xb5c4] [#x0de6 #xd027]
   [#x9af8 #x8c27] [#x773f #x8641] [#xc360 #x4c06] [#x61a8 #x06b5]
   [#xf017 #x7a28] [#xc0f5 #x86e0] [#x0060 #x58aa] [#x30dc #x7d62]
   [#x11e6 #x9ed7] [#x2338 #xea63] [#x53c2 #xdd94] [#xc2c2 #x1634]
   [#xbbcb #xee56] [#x90bc #xb6de] [#xebfc #x7da1] [#xce59 #x1d76]
   [#x6f05 #xe409] [#x4b7c #x0188] [#x3972 #x0a3d] [#x7c92 #x7c24]
   [#x86e3 #x725f] [#x724d #x9db9] [#x1ac1 #x5bb4] [#xd39e #xb8fc]
   [#xed54 #x5578] [#x08fc #xa5b5] [#xd83d #x7cd3] [#x4dad #x0fc4]
   [#x1e50 #xef5e] [#xb161 #xe6f8] [#xa285 #x14d9] [#x6c51 #x133c]
   [#x6fd5 #xc7e7] [#x56e1 #x4ec4] [#x362a #xbfce] [#xddc6 #xc837]
   [#xd79a #x3234] [#x9263 #x8212] [#x670e #xfa8e] [#x4060 #x00e0]
   ]
   [
   [#x3a39 #xce37] [#xd3fa #xf5cf] [#xabc2 #x7737] [#x5ac5 #x2d1b]
   [#x5cb0 #x679e] [#x4fa3 #x3742] [#xd382 #x2740] [#x99bc #x9bbe]
   [#xd511 #x8e9d] [#xbf0f #x7315] [#xd62d #x1c7e] [#xc700 #xc47b]
   [#xb78c #x1b6b] [#x21a1 #x9045] [#xb26e #xb1be] [#x6a36 #x6eb4]
   [#x5748 #xab2f] [#xbc94 #x6e79] [#xc6a3 #x76d2] [#x6549 #xc2c8]
   [#x530f #xf8ee] [#x468d #xde7d] [#xd573 #x0a1d] [#x4cd0 #x4dc6]
   [#x2939 #xbbdb] [#xa9ba #x4650] [#xac95 #x26e8] [#xbe5e #xe304]
   [#xa1fa #xd5f0] [#x6a2d #x519a] [#x63ef #x8ce2] [#x9a86 #xee22]
   [#xc089 #xc2b8] [#x4324 #x2ef6] [#xa51e #x03aa] [#x9cf2 #xd0a4]
   [#x83c0 #x61ba] [#x9be9 #x6a4d] [#x8fe5 #x1550] [#xba64 #x5bd6]
   [#x2826 #xa2f9] [#xa73a #x3ae1] [#x4ba9 #x9586] [#xef55 #x62e9]
   [#xc72f #xefd3] [#xf752 #xf7da] [#x3f04 #x6f69] [#x77fa #x0a59]
   [#x80e4 #xa915] [#x87b0 #x8601] [#x9b09 #xe6ad] [#x3b3e #xe593]
   [#xe990 #xfd5a] [#x9e34 #xd797] [#x2cf0 #xb7d9] [#x022b #x8b51]
   [#x96d5 #xac3a] [#x017d #xa67d] [#xd1cf #x3ed6] [#x7c7d #x2d28]
   [#x1f9f #x25cf] [#xadf2 #xb89b] [#x5ad6 #xb472] [#x5a88 #xf54c]
   [#xe029 #xac71] [#xe019 #xa5e6] [#x47b0 #xacfd] [#xed93 #xfa9b]
   [#xe8d3 #xc48d] [#x283b #x57cc] [#xf8d5 #x6629] [#x7913 #x2e28]
   [#x785f #x0191] [#xed75 #x6055] [#xf796 #x0e44] [#xe3d3 #x5e8c]
   [#x1505 #x6dd4] [#x88f4 #x6dba] [#x03a1 #x6125] [#x0564 #xf0bd]
   [#xc3eb #x9e15] [#x3c90 #x57a2] [#x9727 #x1aec] [#xa93a #x072a]
   [#x1b3f #x6d9b] [#x1e63 #x21f5] [#xf59c #x66fb] [#x26dc #xf319]
   [#x7533 #xd928] [#xb155 #xfdf5] [#x0356 #x3482] [#x8aba #x3cbb]
   [#x2851 #x7711] [#xc20a #xd9f8] [#xabcc #x5167] [#xccad #x925f]
   [#x4de8 #x1751] [#x3830 #xdc8e] [#x379d #x5862] [#x9320 #xf991]
   [#xea7a #x90c2] [#xfb3e #x7bce] [#x5121 #xce64] [#x774f #xbe32]
   [#xa8b6 #xe37e] [#xc329 #x3d46] [#x48de #x5369] [#x6413 #xe680]
   [#xa2ae #x0810] [#xdd6d #xb224] [#x6985 #x2dfd] [#x0907 #x2166]
   [#xb39a #x460a] [#x6445 #xc0dd] [#x586c #xdecf] [#x1c20 #xc8ae]
   [#x5bbe #xf7dd] [#x1b58 #x8d40] [#xccd2 #x017f] [#x6bb4 #xe3bb]
   [#xdda2 #x6a7e] [#x3a59 #xff45] [#x3e35 #x0a44] [#xbcb4 #xcdd5]
   [#x72ea #xcea8] [#xfa64 #x84bb] [#x8d66 #x12ae] [#xbf3c #x6f47]
   [#xd29b #xe463] [#x542f #x5d9e] [#xaec2 #x771b] [#xf64e #x6370]
   [#x740e #x0d8d] [#xe75b #x1357] [#xf872 #x1671] [#xaf53 #x7d5d]
   [#x4040 #xcb08] [#x4eb4 #xe2cc] [#x34d2 #x466a] [#x0115 #xaf84]
   [#xe1b0 #x0428] [#x9598 #x3a1d] [#x06b8 #x9fb4] [#xce6e #xa048]
   [#x6f3f #x3b82] [#x3520 #xab82] [#x011a #x1d4b] [#x2772 #x27f8]
   [#x6115 #x60b1] [#xe793 #x3fdc] [#xbb3a #x792b] [#x3445 #x25bd]
   [#xa088 #x39e1] [#x51ce #x794b] [#x2f32 #xc9b7] [#xa01f #xbac9]
   [#xe01c #xc87e] [#xbcc7 #xd1f6] [#xcf01 #x11c3] [#xa1e8 #xaac7]
   [#x1a90 #x8749] [#xd44f #xbd9a] [#xd0da #xdecb] [#xd50a #xda38]
   [#x0339 #xc32a] [#xc691 #x3667] [#x8df9 #x317c] [#xe0b1 #x2b4f]
   [#xf79e #x59b7] [#x43f5 #xbb3a] [#xf2d5 #x19ff] [#x27d9 #x459c]
   [#xbf97 #x222c] [#x15e6 #xfc2a] [#x0f91 #xfc71] [#x9b94 #x1525]
   [#xfae5 #x9361] [#xceb6 #x9ceb] [#xc2a8 #x6459] [#x12ba #xa8d1]
   [#xb6c1 #x075e] [#xe305 #x6a0c] [#x10d2 #x5065] [#xcb03 #xa442]
   [#xe0ec #x6e0e] [#x1698 #xdb3b] [#x4c98 #xa0be] [#x3278 #xe964]
   [#x9f1f #x9532] [#xe0d3 #x92df] [#xd3a0 #x342b] [#x8971 #xf21e]
   [#x1b0a #x7441] [#x4ba3 #x348c] [#xc5be #x7120] [#xc376 #x32d8]
   [#xdf35 #x9f8d] [#x9b99 #x2f2e] [#xe60b #x6f47] [#x0fe3 #xf11d]
   [#xe54c #xda54] [#x1eda #xd891] [#xce62 #x79cf] [#xcd3e #x7e6f]
   [#x1618 #xb166] [#xfd2c #x1d05] [#x848f #xd2c5] [#xf6fb #x2299]
   [#xf523 #xf357] [#xa632 #x7623] [#x93a8 #x3531] [#x56cc #xcd02]
   [#xacf0 #x8162] [#x5a75 #xebb5] [#x6e16 #x3697] [#x88d2 #x73cc]
   [#xde96 #x6292] [#x81b9 #x49d0] [#x4c50 #x901b] [#x71c6 #x5614]
   [#xe6c6 #xc7bd] [#x327a #x140a] [#x45e1 #xd006] [#xc3f2 #x7b9a]
   [#xc9aa #x53fd] [#x62a8 #x0f00] [#xbb25 #xbfe2] [#x35bd #xd2f6]
   [#x7112 #x6905] [#xb204 #x0222] [#xb6cb #xcf7c] [#xcd76 #x9c2b]
   [#x5311 #x3ec0] [#x1640 #xe3d3] [#x38ab #xbd60] [#x2547 #xadf0]
   [#xba38 #x209c] [#xf746 #xce76] [#x77af #xa1c5] [#x2075 #x6060]
   [#x85cb #xfe4e] [#x8ae8 #x8dd8] [#x7aaa #xf9b0] [#x4cf9 #xaa7e]
   [#x1948 #xc25c] [#x02fb #x8a8c] [#x01c3 #x6ae4] [#xd6eb #xe1f9]
   [#x90d4 #xf869] [#xa65c #xdea0] [#x3f09 #x252d] [#xc208 #xe69f]
   [#xb74e #x6132] [#xce77 #xe25b] [#x578f #xdfe3] [#x3ac3 #x72e6]
   ]])

(defconst blowfish-p-array-inits 
  [
   [#x243f #x6a88] [#x85a3 #x08d3] [#x1319 #x8a2e] [#x0370 #x7344]
   [#xa409 #x3822] [#x299f #x31d0] [#x082e #xfa98] [#xec4e #x6c89]
   [#x4528 #x21e6] [#x38d0 #x1377] [#xbe54 #x66cf] [#x34e9 #x0c6c]
   [#xc0ac #x29b7] [#xc97c #x50dd] [#x3f84 #xd5b5] [#xb547 #x0917]
   [#x9216 #xd5d9] [#x8979 #xfb1b]
   ])

(defun blowfish-generate-keys (key)
  "given a key vector \(an even number of 16-bit integers\), returns
the p-array and s-boxes for Blowfish encryption using that key, as a
pair: \(P-array . S-boxes\)."
  (let ((s-boxes (vector (make-vector 256 nil)
			 (make-vector 256 nil)
			 (make-vector 256 nil)
			 (make-vector 256 nil)))
	(p-array (make-vector 18 nil)))
    ;; copy the initial values into the s-boxes
    (dotimes (s 4)
      (let ((s-box (aref s-boxes s))
	    (inits (aref blowfish-s-box-inits s)))
	(dotimes (idx 256)
	  (aset s-box idx (copy-seq (aref inits idx))))))
    ;; copy the initial values into the p-array
    (dotimes (idx 18)
      (aset p-array idx (copy-seq (aref blowfish-p-array-inits idx))))

    ;; mix the key into the P-array
    (let ((keylen (length key)))
      (assert (and (>= keylen 2) (evenp keylen)) nil
	      "Blowfish key must be a positive multiple of 2 elements long.")

      (dotimes (idx 18)
	(let* ((keyidx (mod (* 2 idx) keylen))
	       (keyval (vector (aref key keyidx) 
			       (aref key (1+ keyidx))))
	       (p-entry (aref p-array idx)))
	  (blowfish-xor p-entry keyval p-entry)
	  ;; clean up
	  (fillarray keyval 0))))

    ;; begin the iterated filling of the P-array and S-boxes
    (let ((data (vector 0 0 0 0)))
      ;; fill the P-array
      (do ((idx 0 (+ idx 2)))
	  ((= idx 18))
	;; encipher data w/ the current P-array and S-boxes
	(blowfish-cipher-block data p-array s-boxes)
	;; replace the next two P-array elements with the new data
	(let ((Pa (aref p-array idx))
	      (Pb (aref p-array (1+ idx))))
	  (aset Pa 0 (aref data 0))
	  (aset Pa 1 (aref data 1))
	  (aset Pb 0 (aref data 2))
	  (aset Pb 1 (aref data 3))))

      ;; fill the S-boxes
      (dotimes (s 4)
	(do ((idx 0 (+ idx 2))
	     (s-box (aref s-boxes s)))
	    ((= idx 256))
	  ;; encipher data w/ the current P-array and S-boxes
	  (blowfish-cipher-block data p-array s-boxes)
	  ;; replace the next two S-box elements with the new data
	  (let ((Sa (aref s-box idx))
		(Sb (aref s-box (1+ idx))))
	    (aset Sa 0 (aref data 0))
	    (aset Sa 1 (aref data 1))
	    (aset Sb 0 (aref data 2))
	    (aset Sb 1 (aref data 3)))))

      ;; clean up
      (fillarray data 0))
      
    ;; return the result
    (cons p-array s-boxes)))
