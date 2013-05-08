;;;; passphrase-generator.lisp
;;;;
;;;; A random passphrase generator inspired by https://xkcd.com/936/


(defpackage :password-generator
     (:use :cl :cl-who :hunchentoot :parenscript))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; App initializiation
(defun all-satisfy (predicate seq)
  "Check if all elements of a sequence satisfy the predicate."
  (reduce #'(lambda (a b)
              (and a b))
          (map 'list predicate seq)))

(defun lowercase-letter-p (char)
  "Is char a lowercase letter?"
  (find char "abcdefghijklmnopqrstuvwxyz"))

(defun random-elt (seq)
  "Choose a random element from a sequence."
  (elt seq (random (length seq))))

(defun read-file-lines (filename)
  "Read the lines of a file into a list."
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
       while line
       collect line)))

(defun get-words-list (filename)
  "Read all of the lowercase words in a words file into a vector of strings."
  (let* ((raw-words (read-file-lines filename)))
    (map 'vector
         (lambda (x) x)
         (remove-if (lambda (word)
                      (not (all-satisfy #'lowercase-letter-p word)))
                    raw-words))))

(defun initialize (&key (words-file "/usr/share/dict/words"))
  "Initialize the application."
  (setf *random-state* (make-random-state t))
  (defparameter *words* (get-words-list words-file)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Runtime functions
(defun random-phrase (&optional (acc)
                      &key (num 4) (words *words*) (max-word-length))
  "Generate a random phrase
     num: number of words in the phrase
     words: list of words to use
     max-word-length: don't use any words longer than this"
  (if (equalp num 0)
      acc
      (let ((new-word (random-elt words)))
        (if (and max-word-length (> (length new-word) max-word-length))
            ;; Word is too long, try again
            (random-phrase acc
                           :num num :words words
                           :max-word-length max-word-length)
            ;; Word is fine
            (random-phrase (cons new-word acc)
                           :num (1- num) :words words
                           :max-word-length max-word-length)))))







