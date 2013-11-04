;;;  des.el - Data Encryption Standard block cipher, including 3DES

;; Copyright (C) 1998 Ray Jones

;; Author: Ray Jones, rjones@pobox.com
;; Keywords: DES, 3DES, oink, cipher, cypher, cryptography
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

;; this is not optimized at all, yet.  it uses vectors of boolean
;; values.  changing it to use 16-bit integers instead might be
;; faster, though the permutation functions will be quite a bit more
;; confusing.

;; this code was written using des-how-to.txt, by Matthew Fischer
;; (mfischer@blue.weeg.uiowa.edu)

;;; TODO:

;; add DESX
;; add DES/SK (anyone that knows what DES/SK is, please mail me)
;; add DES with key-dependent S-boxes (a la Biham, see Schneier)

;;; Code:

(require 'cl)

(defun hexstring-to-bitvec (string)
  "convert a hexadecimal string into a MSB-first bit vector"
  (let* ((strlen (length string))
         (bitlen (* 4 strlen))
         (bvec (make-vector bitlen nil)))
    (do ((stridx 0 (1+ stridx))
         (bitidx 0 (+ bitidx 4)))
        ((= stridx strlen) bvec)
      (let* ((char (aref string stridx))
             (val (if (and (<= ?0 char)
                           (<= char ?9))
                      (- char ?0)
                    (+ 10 (- (downcase char) ?a)))))
        (dotimes (offset 4)
          (when (/= 0 (logand val
                              (ash 1 (- 3 offset))))
            (aset bvec (+ bitidx offset) t)))))))

(defun bitvec-to-hexstring (bitvec)
  "convert an MSB-first bit vector into a hexadecimal string"
  (let* ((bitlen (length bitvec))
         (strlen (car (ceiling* bitlen 4)))
         (string (make-string strlen ?0)))
    (do ((stridx 0 (1+ stridx))
         (bitidx 0 (+ bitidx 4)))
        ((= stridx strlen) string)
      (let ((val 0))
        (dotimes (offset 4)
          (let ((bidx (+ bitidx offset)))
            (when (and (< bidx bitlen)
                       (aref bitvec bidx))
              (incf val (ash 1 (- 3 offset))))))
        (aset string stridx (if (< val 10) 
                                (+ ?0 val)
                              (+ ?a (- val 10))))))))

(defun des-permute (vec permute-vals)
  "helper function for permutations in DES code"
  (let* ((len (length permute-vals))
         (outvec (make-vector len nil)))
    (dotimes (offset len outvec)
      (aset outvec offset (aref vec (aref permute-vals offset))))))

;; note that these are 0-indexed.  most references list these as 1-indexed
(defconst des-PC1-vals [56 48 40 32 24 16 8 0 57 49 41 33 25 17 9 1 58 50 42 34 26 18 10 2 59 51 43 35 62 54 46 38 30 22 14 6 61 53 45 37 29 21 13 5 60 52 44 36 28 20 12 4 27 19 11 3])

(defun des-PC1 (key)
  "DES permuted choice #1.
takes a 64-bit key and returns a 56-bit permuted key"
  (assert (= (length key) 64) nil
          "des-PC1: key must be 64 bits long")
  (des-permute key des-PC1-vals))

;; note that these are 0-indexed.  most references list these as 1-indexed
(defconst des-PC2-vals [13 16 10 23 0 4 2 27 14 5 20 9 22 18 11 3 25 7 15 6 26 19 12 1 40 51 30 36 46 54 29 39 50 44 32 47 43 48 38 55 33 52 45 41 49 35 28 31])


(defun des-PC2 (key)
  "DES permuted choice #2.
takes a 56-bit key and returns a 48-bit permuted key."
  (assert (= (length key) 56) nil
          "des-PC2: key must be 56 bits long")
  (des-permute key des-PC2-vals))


(defun des-<<< (vec shift)
  "circular left shift VEC by SHIFT elements"
  (let* ((len (length vec))
         (outvec (make-vector len nil)))
    (when (or (>= shift len)
              (< shift 0))
      (setq shift (mod shift len)))
    (do ((out-idx 0 (1+ out-idx))
         (in-idx shift (1+ in-idx)))
        ((= in-idx len))
      (aset outvec out-idx (aref vec in-idx)))
    (do ((out-idx (- len shift) (1+ out-idx))
         (in-idx 0 (1+ in-idx)))
        ((= in-idx shift) outvec)
      (aset outvec out-idx (aref vec in-idx)))))


(defconst des-key-shifts [1 1 2 2 2 2 2 2 1 2 2 2 2 2 2 1])

(defun des-compute-subkeys (key)
  "computes the 16 48-bit subkeys from a 64-bit key"
  (let* ((subkeys (make-vector 16 []))
         (PC1 (des-PC1 key))
         (C (subseq PC1 0 28))
         (D (subseq PC1 28 56)))
    ;; clean up
    (fillarray PC1 nil)
    ;; compute subkeys
    (dotimes (count 16)
      (let* ((new-C (des-<<< C (aref des-key-shifts count)))
             (new-D (des-<<< D (aref des-key-shifts count)))
             (CD (vconcat new-C new-D)))
        (aset subkeys count (des-PC2 CD))
        ;; clean up
        (fillarray C nil)
        (fillarray D nil)
        ;; replace old with new
        (setq C new-C)
        (setq D new-D)))
    (fillarray C nil)
    (fillarray D nil)
    subkeys))



(defconst des-E-vals [31 0 1 2 3 4 3 4 5 6 7 8 7 8 9 10 11 12 11 12 13 14 15 16 15 16 17 18 19 20 19 20 21 22 23 24 23 24 25 26 27 28 27 28 29 30 31 0])

(defun des-E (vec)
  "perform the Expansion function on a 32-bit vector"
  (assert (= (length vec) 32) nil
          "des-E: vec must be 32 bits long")
  (des-permute vec des-E-vals))

(defun des-xor-in-place (vec1 vec2)
  "XOR two bit vectors together, storing in the first"
  (let ((len (length vec1)))
    (assert (= len (length vec2)) nil
            "des-xor: vec1 and vec2 must be of same length")
    (dotimes (idx len vec1)
      (aset vec1 idx (not (eq (aref vec1 idx)
                              (aref vec2 idx)))))))

(defun des-integer-to-bitvec (val)
  "converts an integer to a 4-bit bit vector (used to construct S-boxes)"
  (let ((out (make-vector 4 nil)))
    (dotimes (shift 4 out)
      (when (= 1 (logand 1 (ash val (- shift 3))))
        (aset out shift t)))))
        

(defconst des-S-boxes-vals 
  [[[14  4 13  1  2 15 11  8  3 10  6 12  5  9  0  7]
    [ 0 15  7  4 14  2 13  1 10  6 12 11  9  5  3  8]
    [ 4  1 14  8 13  6  2 11 15 12  9  7  3 10  5  0]
    [15 12  8  2  4  9  1  7  5 11  3 14 10  0  6 13]]
 
   [[15  1  8 14  6 11  3  4  9  7  2 13 12  0  5 10]
    [ 3 13  4  7 15  2  8 14 12  0  1 10  6  9 11  5]
    [ 0 14  7 11 10  4 13  1  5  8 12  6  9  3  2 15]
    [13  8 10  1  3 15  4  2 11  6  7 12  0  5 14  9]]

   [[10  0  9 14  6  3 15  5  1 13 12  7 11  4  2  8]
    [13  7  0  9  3  4  6 10  2  8  5 14 12 11 15  1]
    [13  6  4  9  8 15  3  0 11  1  2 12  5 10 14  7]
    [ 1 10 13  0  6  9  8  7  4 15 14  3 11  5  2 12]]

   [[ 7 13 14  3  0  6  9 10  1  2  8  5 11 12  4 15]
    [13  8 11  5  6 15  0  3  4  7  2 12  1 10 14  9]
    [10  6  9  0 12 11  7 13 15  1  3 14  5  2  8  4]
    [ 3 15  0  6 10  1 13  8  9  4  5 11 12  7  2 14]]

   [[ 2 12  4  1  7 10 11  6  8  5  3 15 13  0 14  9]
    [14 11  2 12  4  7 13  1  5  0 15 10  3  9  8  6]
    [ 4  2  1 11 10 13  7  8 15  9 12  5  6  3  0 14]
    [11  8 12  7  1 14  2 13  6 15  0  9 10  4  5  3]]

   [[12  1 10 15  9  2  6  8  0 13  3  4 14  7  5 11]
    [10 15  4  2  7 12  9  5  6  1 13 14  0 11  3  8]
    [ 9 14 15  5  2  8 12  3  7  0  4 10  1 13 11  6]
    [ 4  3  2 12  9  5 15 10 11 14  1  7  6  0  8 13]]

   [[ 4 11  2 14 15  0  8 13  3 12  9  7  5 10  6  1]
    [13  0 11  7  4  9  1 10 14  3  5 12  2 15  8  6]
    [ 1  4 11 13 12  3  7 14 10 15  6  8  0  5  9  2]
    [ 6 11 13  8  1  4 10  7  9  5  0 15 14  2  3 12]]

   [[13  2  8  4  6 15 11  1 10  9  3 14  5  0 12  7]
    [ 1 15 13  8 10  3  7  4 12  5  6 11  0 14  9  2]
    [ 7 11  4  1  9 12 14  2  0  6 10 13 15  3  5  8]
    [ 2  1 14  7  4 10  8 13 15 12  9  0  3  5  6 11]]])

(defconst des-S-boxes (map 'vector 
                           #'(lambda (x) 
                               (map 'vector
                                    #'(lambda (y)
                                        (map 'vector
                                             #'des-integer-to-bitvec
                                             y))
                                    x))
                           des-S-boxes-vals))

(defun des-S (vec)
  "do the S substitution on a 48-bit vector, returning a 32-bit vector"
  (let ((temp-vecs (make-vector 8 nil)))
    (do ((Sidx 0 (1+ Sidx))
         (bitidx 0 (+ bitidx 6)))
        ((= Sidx 8))
      (let ((val1 (+ (if (aref vec (+ bitidx 0)) 2 0)
                     (if (aref vec (+ bitidx 5)) 1 0)))
            (val2 (+ (if (aref vec (+ bitidx 1)) 8 0)
                     (if (aref vec (+ bitidx 2)) 4 0)
                     (if (aref vec (+ bitidx 3)) 2 0)
                     (if (aref vec (+ bitidx 4)) 1 0))))
        (aset temp-vecs Sidx (aref (aref (aref des-S-boxes Sidx)
                                         val1)
                                   val2))))
    (prog1
        (apply #'vconcat (coerce temp-vecs 'list))
      ;; clean up
      (fillarray temp-vecs nil))))

(defconst des-P-vals [15 6 19 20 28 11 27 16 0 14 22 25 4 17 30 9 1 7 23 13 31 26 2 8 18 12 29 5 21 10 3 24])

(defun des-P (vec)
  "perform the P permutation on a 32-bit vector"
  (assert (= (length vec) 32) nil
          "des-P: vec must be 32 bits long")
  (des-permute vec des-P-vals))

(defun des-f (vec key)
  "perform the des f() function on 32-bit VEC and 48-bit KEY"
  (let* ((E-vec (des-E vec))
         (S-vec (des-S (des-xor-in-place E-vec key)))
         (P-vec (des-P S-vec)))
    (prog1
        P-vec
      ;; clean up
      (fillarray E-vec nil)
      (fillarray S-vec nil))))

(defconst des-IP-vals [57 49 41 33 25 17 9 1 59 51 43 35 27 19 11 3 61 53 45 37 29 21 13 5 63 55 47 39 31 23 15 7 56 48 40 32 24 16 8 0 58 50 42 34 26 18 10 2 60 52 44 36 28 20 12 4 62 54 46 38 30 22 14 6])

(defun des-IP (vec)
  "perform the Initial Permutation on a 64-bit data vector"
  (assert (= (length vec) 64) nil
          "des-IP: vec must be 64 bits long")
  (des-permute vec des-IP-vals))


(defconst des-IP-inv-vals [39 7 47 15 55 23 63 31 38 6 46 14 54 22 62 30 37 5 45 13 53 21 61 29 36 4 44 12 52 20 60 28 35 3 43 11 51 19 59 27 34 2 42 10 50 18 58 26 33 1 41 9 49 17 57 25 32 0 40 8 48 16 56 24])

(defun des-IP-inv (vec)
  "perform the inverse of the Initial Permutation on a 64-bit data vector"
  (assert (= (length vec) 64) nil
          "des-IP-inv: vec must be 64 bits long")
  (des-permute vec des-IP-inv-vals))

(defun des-cipher-block (vec subkeys &optional reverse)
  "perform the DES cipher on a 64-bit VEC using SUBKEYS.
if optional third arg REVERSE is true, decrypts."
  (let* ((IP (des-IP vec))
         (L (subseq IP 0 32))
         (R (subseq IP 32 64)))
    ;; clean up
    (fillarray IP nil)

    (dotimes (count 16)
      (let ((f-R (des-f R (aref subkeys (if reverse (- 15 count) count))))
            (old-R R)
            (old-L L))
        (setq L old-R)
        (setq R (des-xor-in-place old-L f-R))
        ;; clean up
        (fillarray f-R nil)))

    (let ((RL (vconcat R L)))
      (prog1
          (des-IP-inv RL)
        ;; clean up
        (fillarray RL nil)
        (fillarray R nil)
        (fillarray L nil)))))

(defun des-triple-des (data subkeys1 subkeys2 subkeys3 &optional reverse)
  "perform the triple-DES encryption on DATA with three sets of subkeys.
if optional fifth arg REVERSE is true, decrypts."
  (if (not reverse)
      (let* ((E1 (des-cipher-block data subkeys1))
             (E2 (des-cipher-block E1 subkeys2 t))
             (E3 (des-cipher-block E2 subkeys3)))
        ;; clean up
        (fillarray E1 nil)
        (fillarray E2 nil)
        E3)

    (let* ((D1 (des-cipher-block data subkeys3 t))
           (D2 (des-cipher-block D1 subkeys2))
           (D3 (des-cipher-block D2 subkeys1 t)))
      ;; clean up
      (fillarray D1 nil)
      (fillarray D2 nil)
      D3)))
