;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname assignment10) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;Constants
(define GRID-HEIGHT 40)
(define GRID-WIDTH 40)
(define CELL-HEIGHT 15)
(define CELL-WIDTH 15)
(define ACTIVE-COLOR "green")
(define TYPING-COLOR "purple")
(define STUCK-COLOR "red")
(define TYPING-BOX-HEIGHT 20)
(define SCENE-HEIGHT (+ TYPING-BOX-HEIGHT (* GRID-HEIGHT CELL-HEIGHT)))
(define SCENE-WIDTH (* GRID-WIDTH CELL-WIDTH))
(define SCENE (empty-scene SCENE-WIDTH SCENE-HEIGHT))
(define FONT-SIZE CELL-HEIGHT)
(define TYPING-X (/ SCENE-WIDTH 2))
(define TYPING-Y (- SCENE-HEIGHT (/ TYPING-BOX-HEIGHT 2)))

;A Word is a (make-word String posn)
(define-struct word [str position])
; where:
; - str is the String value of the word
; - position is the x and y position of the word on the grid
(define w1 (make-word "hello" (make-posn 0 0)))
(define w2 (make-word "friend" (make-posn 15 10)))
(define w3 (make-word "chip" (make-posn 25 20)))
(define w4 (make-word "bottom" (make-posn 25 39)))

#;
(define (word-tmpl w)
  ...(word-str w)...(posn-x (word-position w))...(posn-y (word-position w)))

;A List of Words (LoW) is one of:
; - empty '()
; - cons(Word LoW)
(define list0 '())
(define list1 (list w1 w2 w3))
(define list2 (list w4))

#;
(define (low-tmpl low)
  (cond
    [(empty? low) ...]
    [(cons? low) ... (first low) ... (low-tmpl (rest low))]))

; A world (w) is one of:
; - (make-world LoW LoW String Boolean Number)
(define-struct world [falling-words inactive-words current-word gen-word? score])
; A world represents
; - falling words represents the list of falling words
; - inactive-words represents the list of words that have fallen and are no longer falling.
; - current-word is what the user has typed
; - gen-word? is the boolean that tells whether to generate a new word
; - score is the number of ticks that have passed
(define world1 (make-world list1 list2 "test" false 1))

#;
(define (world-tmpl w)
  ...(world-falling-words w)...
  ...(world-inactive-words w)...
  ...(world-current-word w)...
  ...(world-gen-word? w)...
  ...(world-score w))




;Wishlist

;----
;on tick:
; - lower-words : World -> World
;        -Lower all of the falling worlds by one row
; - make-inactive? : Word LoIW -> Boolean
;        -Checks if word hits bottom or any inactive words
; - move-to-inactive : World Word -> World
;        -If inactive, remove from list of falling words
;        -add to list of inactive words
; - generate-word : ? -> Word
;        -Create new word every other tick (update boolean of World)
;        -Should generate a random x less than the (edge-length)
; - update-time: Number -> Number
;        -Keeping time of game

;to draw:

; - render-world World -> Image
;Render the 3 parts of the world
(define (render-world w)
    (place-falling-words (world-falling-words w)
                         (place-inactive-words (world-inactive-words w)
                                               (place-current-word (world-current-word w) SCENE))))

; TODO check-expects 

; List of Strings (LOS) is one of:
; - empty
; - cons(String LOS)
(define los1 (explode "TEST"))
#;
(define (los-templ los)
  (cond [(empty? los) ...]
        [(cons? los) ... (first los) ... (los-templ (rest los))]))

; grid-to-pix-x: Number -> Number 
; Gives the pixel value of the desired column
(define (grid-to-pix-x x)
  (* (+ x 0.5) CELL-WIDTH))
(check-expect (grid-to-pix-x 10) 157.5)
(check-expect (grid-to-pix-x 0) 7.5)

; grid-to-pix-y: Number -> Number 
; Gives the pixel value of the desired row
(define (grid-to-pix-y y)
  (* (+ y 0.5) CELL-HEIGHT))
(check-expect (grid-to-pix-y 10) 157.5)
(check-expect (grid-to-pix-y 0) 7.5)

; place-letters: LoS Posn Color Image -> Image
; Places an exploded word on the grid
(define (place-letters los x y c scene)
  (cond [(empty? los) scene]
        [(cons? los) (place-image (text (first los) FONT-SIZE c)
                                  (grid-to-pix-x x) (grid-to-pix-y y)
                                  (place-letters (rest los) (+ x 1) y c scene))]))
(check-expect (place-letters los1 0 0 "red" SCENE)
              (place-image (text "T" 15 "red") 7.5 7.5
                           (place-image (text "E" 15 "red") 22.5 7.5
                                        (place-image (text "S" 15 "red") 37.5 7.5
                                                     (place-image (text "T" 15 "red") 52.5 7.5 SCENE)))))

; place-word: Word Color Image -> Image
; Places a single word on the scene with the appropriate color
(define (place-word w c scene)
  (place-letters (explode (word-str w))
               (posn-x (word-position w))
               (posn-y (word-position w))
               c
               scene))
(check-expect (place-word w1 ACTIVE-COLOR SCENE)
              (place-letters (list "h" "e" "l" "l" "o") 0 0 "green" SCENE))
(check-expect (place-word w2 TYPING-COLOR SCENE)
              (place-letters (list "f" "r" "i" "e" "n" "d") 15 10 "purple" SCENE))
              
; -place-falling-words LoW -> Image
;Place the falling words with the appropriate color
(define (place-falling-words lofw scene)
    (cond
    [(empty? lofw) scene]
    [(cons? lofw) (place-word (first lofw) ACTIVE-COLOR
                              (place-falling-words (rest lofw) scene))]))
(check-expect (place-falling-words list0 SCENE) SCENE)
(check-expect (place-falling-words list1 SCENE)
              (place-word w1 "green"
                          (place-word w2 "green"
                                      (place-word w3 "green"
                                                  SCENE))))


; - place-inactive-words LoW -> Image
;Place the inactive words with the appropriate color
(define (place-inactive-words loiw scene)
    (cond
    [(empty? loiw) scene]
    [(cons? loiw) (place-word (first loiw) STUCK-COLOR
                              (place-inactive-words (rest loiw) scene))]))
(check-expect (place-inactive-words list0 SCENE) SCENE)
(check-expect (place-inactive-words list1 SCENE)
              (place-word w1 "red"
                          (place-word w2 "red"
                                      (place-word w3 "red"
                                                  SCENE))))
(check-expect (place-inactive-words list2 SCENE)
              (place-word w4 "red" SCENE))

; - place-current-word String -> Image
;Place the current word with the appropriate color
(define (place-current-word str scene)
  (place-image (text str FONT-SIZE TYPING-COLOR) TYPING-X TYPING-Y scene))

; TODO check-expects


;on key:
; - update-word World KeyEvent -> World
;       -if alphabetic, add-letter,
;       -if backspace, remove-letter,
;       -if enter, delete-word,
;       -if else, do nothing
; - add-letter String -> String
; - remove-letter String -> String
; - delete-word World String -> World
;     -check falling words for current word and remove

;stop when:
; - stop World -> Boolean
;    - check if any inactive word has posn-y of y=0 (top of grid)
; - generate-score World Number -> Number
;    -outputs score in last-picture


;(define (main tick-rate)
 ; (big-bang type-world
  ;          [on-tick ]
   ;         [to-draw ]
    ;        [on-key ]
     ;       [stop-when ]))