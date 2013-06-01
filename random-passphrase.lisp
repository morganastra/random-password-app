;;;; passphrase-generator.lisp
;;;;
;;;; A random passphrase generator inspired by https://xkcd.com/936/
(ql:quickload :restas)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; App initializiation
(restas:define-module #:random-passphrase
  (:use :cl :restas))

(in-package #:random-passphrase)

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
(defun random-phrase (&key (acc) (num 4) (words *words*) (max-word-length 10)
                        (min-word-length 0))
  "Generate a random phrase
     num: number of words in the phrase
     words: list of words to use
     max-word-length: don't use any words longer than this. Default 10
     min-word-length: don't use any words shorter than this. Default 0"
  (if (equalp num 0)
      acc
      (let ((new-word (random-elt words)))
        (if (or (< (length new-word) min-word-length)
                (> (length new-word) max-word-length))
            ;; Word is too long or too short, try again
            (random-phrase :acc acc
                           :num num :words words
                           :max-word-length max-word-length
                           :min-word-length min-word-length)
            ;; Word is fine
            (random-phrase :acc (cons new-word acc)
                           :num (1- num) :words words
                           :max-word-length max-word-length
                           :min-word-length min-word-length)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Restas routes
(define-route random-passphrase ("")
  (prin1-to-string (random-phrase)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Go!
(initialize)
(start '#:random-passphrase :port 8000)





