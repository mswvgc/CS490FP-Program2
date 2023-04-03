#lang racket

;;;;; Michael Wright
;;;;; mswvgc@umsystems.edu
;;;;; 4/6/2023
;;;;; CS 490 FP Program 2 - functional version

;;;;; This program uses a purely functional style, all hash-tables are immutable

;;; Reads a text file and generates a hash table of word frequencies. 
(define (process-file file-path)
  
  ;; Reads the contents of a file and stores it into a string
  (define input-string
    (call-with-input-file file-path (lambda (input-port) (port->string input-port))))

  ;; Makes a list of characters  from a string
  (define char-list (string->list input-string))

  ;; Normalizes a list of characters 
  (define normalized-char-list
    (map (lambda (char) ; Maps over list of characters and returns a list consisting of only lowercases and spaces
           (cond
             [(member char '(#\newline #\" #\, #\. #\? #\! #\- #\— #\: #\;)) #\space]  ; If #t, return a space (Replacing the punctuation with a space)
             [else (char-downcase char)])) ; If #f, return a lowercase (Replacing the char with its lowercase equivalent)
         char-list))

  ;; Makes a list of characters into a string 
  (define normalized-string (list->string normalized-char-list)) 

  ;; Splits a string into a list of words 
  (define words (string-split normalized-string)) ; Whitespace delimited

  ;; Returns a hash table consisting of word frequencies from a list of words
  (define (count-words lst)
    
    ;; Recursive function to process the list of words and build the hash table
    (define (process-words ht words)
      (if (null? words)
          ht
          (let* ([word (car words)]
                 [remaining-words (cdr words)]
                 [current-count (hash-ref ht word 0)]
                 [new-ht (hash-set ht word (+ current-count 1))])
            (process-words new-ht remaining-words))))
    
    ;; Creates an empty immutable hash table
    (define ht (hash))

    ;; Processes the list of words and builds the hash table
    (process-words ht lst))
  
  ;; Creates a hash table with frequency counts for each word in a list of words
  (define word-frequencies (count-words words))
  
  ;; Returns the sum of all the values in a hash table 
  (define (sum-hash hash)
    (apply + (hash-values hash))) 

  ;; Computes the total count of words in the input list by summing up the values in the `word-frequencies` hash table
  (define total (sum-hash word-frequencies))

  ;; Creates a new hash table with the normalized frequency count for each word
  (define updated-word-frequencies-alist
    (hash-map
     word-frequencies
     (lambda (word value)
       (cons word (* -1 (log (/ value total) 10))))))

  ;; Convert the alist to a hash table
  (define output-hash
    (for/hash ([pair updated-word-frequencies-alist])
      (values (car pair) (cdr pair))))

  output-hash)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Replace with proper file paths;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define doyle-profile (process-file "/home/mw/Downloads/Input_Files/Doyle.txt"))
(define lovecraft-profile (process-file "/home/mw/Downloads/Input_Files/Lovecraft.txt"))
(define mystery1-profile (process-file "/home/mw/Downloads/Input_Files/mystery1.txt"))
(define mystery2-profile (process-file "/home/mw/Downloads/Input_Files/mystery2.txt"))

;;; Compare two author profiles and return a measure of their differences
(define (compare-profiles profile1 profile2)
  
  (define diff-profile
    (for/hash ([(word occurances) (in-hash profile1)]
               #:when (hash-has-key? profile2 word))
      (let ([diff (abs (- occurances (hash-ref profile2 word)))])
        (values word diff))))
  
  (let ([num-diffs (hash-count diff-profile)]) ; Compute the measure of difference between the two profiles
    (if (= num-diffs 0)
        1 ; If there are no differences, the profiles are identical
        (/ (apply + (hash-values diff-profile)) num-diffs)))) ; Otherwise, compute the average absolute difference between the profiles



;;; Given two author profiles and a mystery profile, predict which author wrote the mystery text
;;; Use macro to get variable names at runtime
(define-syntax-rule (predict-author author-profile1 author-profile2 mystery-profile)
  
  (begin
    ;; Get the name of the mystery profile
    (define mystery-profile-name (symbol->string (syntax->datum #'mystery-profile)))
    
    (display (string-append "Analyzing " mystery-profile-name "...\n"))
    ; Compare the mystery profile to the two author profiles and predict which author its more likely to have
    (if (< (compare-profiles author-profile1 mystery-profile) (compare-profiles author-profile2 mystery-profile))
        (display (string-append "         " mystery-profile-name " is probably " (symbol->string (syntax->datum #'author-profile1)) "\n"))
        (display (string-append "         " mystery-profile-name " is probably " (symbol->string (syntax->datum #'author-profile2)) "\n")))))




;;;; Calling the main functions to display prediction outputs
(predict-author doyle-profile lovecraft-profile mystery1-profile)
(predict-author doyle-profile lovecraft-profile mystery2-profile)
