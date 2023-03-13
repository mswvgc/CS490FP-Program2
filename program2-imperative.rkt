#lang racket

;;;;; Michael Wright
;;;;; mswvgc@umsystems.edu
;;;;; 03/12/2023
;;;;; CS 490 FP Program 2 - imperative version

;;;;; This program currently uses mutable hash tables to store and update word frequencies and differences between author profiles
;;;;; I am in the process of refactoring the code into a more functional style that avoids the use of mutable data structures and loop constructs
;;;;; I encountered some issues with correctly implementing the immutable hash tables, so I am including the imperitve version as a fallback solution


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
    
    ;; Creates a empty hash table
    (define ht (make-hash))

    ;; Updates a word count in the hash table
    (define (update-count ht word)
      (let ((current-count (hash-ref ht word 0))) ; Retrieves the current count of the word
        (hash-set! ht word (+ current-count 1)))) ; Increments the current count of the word by 1

   
    (for-each (lambda (word) ; Iterates over the list of words and updates the count of each word
                (if (hash-has-key? ht word) ; Checks if the word is already in the hash table
                    (update-count ht word) ; If #t, update its count by 1
                    (hash-set! ht word 1))) ; If #f, add it to the hash table with a count of 1
              lst)

    ht) ; Returns the hash table

  ;; Creates a hash table with frequency counts for each word in a list of words
  (define word-frequencies (count-words words))
  
  ;; Returns the sum of all the values in a hash table 
  (define (sum-hash hash)
    (apply + (hash-values hash))) 

  ;; Computes the total count of words in the input list by summing up the values in the `word-frequencies` hash table
  (define total (sum-hash word-frequencies))

  (for ([(k v) (in-hash word-frequencies)])
    ;; Update the hash table with the normalized frequency count for each word
    (hash-update! word-frequencies k (lambda (old-val) (* -1 (log (/ old-val total) 10))) 0))
  word-frequencies)

(define doyle-profile (process-file "/home/mw/Downloads/Input_Files/Doyle.txt"))
(define lovecraft-profile (process-file "/home/mw/Downloads/Input_Files/Lovecraft.txt"))
(define mystery1-profile (process-file "/home/mw/Downloads/Input_Files/mystery1.txt"))
(define mystery2-profile (process-file "/home/mw/Downloads/Input_Files/mystery2.txt"))

;;;; Compare two author profiles and return a measure of their differences
(define (compare-profiles profile1 profile2)
  
  ;; Create hash table to store the differences between the profiles
  (define diff-profile (make-hash))
  
  (for ([(word occurances) (in-hash profile1)])  ; Iterate over the keys and values in the first profile
    (when (hash-has-key? profile2 word) ; Check if the second profile contains the same key/word
      (let ([diff (abs (- occurances (hash-ref profile2 word)))]) ; Compute the absolute difference between the frequency count of each word in profile1 and profile2
        ; If the two author profiles have the same frequency count for each word, the absolute difference between frequency counts is 0 for every word
        (hash-set! diff-profile word diff))))  ; Add the difference to the diff-profile hash tablel
  
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
