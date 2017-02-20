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

;----
;on tick:
; - process : World -> World
;        - Reconstructs the world assuring all updates are handled
(define (process w)
  (make-world
     (generate-word-maybe
      (lower-words (world-falling-words w) (world-inactive-words w))
      (world-gen-word? w))
     (get-inactives (world-falling-words w) (world-inactive-words w))
     (world-current-word w)
     (not (world-gen-word? w)) ;flip the bool so we do/dont generate next tick
     (update-time (world-score w))))
; TODO Tests... ugh this one is gonna fucking suck

; x-range-overlap? Number Number Number Number -> Boolean
; Checks if x1 and len1 range has an overlap with x2 and len2 range
(define (x-range-overlap? x1 len1 x2 len2)
  (cond [(= x1 x2) true]
        [(< x1 x2) (> (+ x1 len1) x2)]
        [(> x1 x2) (< x1 (+ x2 len2))]))
(check-expect (x-range-overlap? 0 10 0 1) true)
(check-expect (x-range-overlap? 0 1 1 1) false)
(check-expect (x-range-overlap? 0 2 1 1) true)
(check-expect (x-range-overlap? 5 5 10 10) false)
(check-expect (x-range-overlap? 5 5 9 10) true)
(check-expect (x-range-overlap? 10 5 5 5) false)
(check-expect (x-range-overlap? 10 5 5 10) true)
               
; - would-intersect? : Word Word -> Boolean
; Checks if Word 1 would intersect with inactive Word 2 if moved down

(check-expect (would-intersect? w1 w2) #f)
(check-expect (would-intersect? w1 w2) #f)

(define (would-intersect? w1 w2)
  ;They will intersect if w2's y is the same as w1's y + 1,
  ;and the x to x+len(string) range of w1 doesn't overlap with w2's
  (and (= (posn-y (word-position w2)) (+ 1 (posn-y (word-position w1))))
       (x-range-overlap? (posn-x (word-position w1)) (string-length (word-str w1))
                         (posn-x (word-position w2)) (string-length (word-str w2)))))
; TODO Tests


; - make-inactive? : Word LoW (loiw) -> Boolean
; Checks if word hits bottom or any inactive words
(check-expect (make-inactive? w1 list0) #f)
(check-expect (make-inactive? w4 list0) #t)
(check-expect (make-inactive? (make-word "Test" (make-posn 11 19)) list1) #f)

(define (make-inactive? w low)
  (cond [(empty? low)
         ;if no intersect with words, return whether it's on the bottom or not
         (= (- GRID-HEIGHT 1) (posn-y (word-position w)))]
        [(cons? low)
         ; else check if it intersects with the first word
         (or (would-intersect? w (first low)) (make-inactive? w (rest low)))]))      

  
; - get-inactives : LoW -> LoW
; If inactive, add to list of inactive words
;(check-expect (get-inactives list0 list2) list2)
;(check-expect (get-inactives list1 list2) )

(define (get-inactives lofw loiw)
  (cond
    [(empty? lofw) loiw]
    [(cons? lofw) (if (make-inactive? (first lofw) loiw)
                     (cons (first lofw) (get-inactives (rest lofw) loiw))
                     (get-inactives (rest lofw) loiw))]))                     

;lower-word : Word -> Word
; Increases the y value of the word by 1
(check-expect (lower-word (make-word "Test1" (make-posn 10 10)))
              (make-word "Test1" (make-posn 10 11)))
(check-expect (lower-word (make-word "Test2" (make-posn 0 0)))
              (make-word "Test2" (make-posn 0 1)))

(define (lower-word w)
  (make-word (word-str w)
             (make-posn
              (posn-x (word-position w))
              (+ 1(posn-y (word-position w))))))


; - lower-words : LoW LoW -> LoW
; Lower all of the falling worlds by one row, takes lofw and loiw
(check-expect (lower-words list0 list2) '())
(check-expect (lower-words list1 list2) (list (make-word "hello" (make-posn 0 1))
                                              (make-word "friend" (make-posn 15 11))
                                              (make-word "chip" (make-posn 25 21))))


(define (lower-words lofw loiw)
  (cond
    [(empty? lofw) '()]
    [(cons? lofw) (if (make-inactive? (first lofw) loiw)
                     (lower-words (rest lofw) loiw)
                     (cons (lower-word (first lofw))
                           (lower-words (rest lofw) loiw)))]))
; TODO tests


; - get-new-word : - -> Word
;        - Get a random word string
;        -Should generate a random x less than the (edge-length)
; - generate-word-maybe : LoW Boolean -> LoW
; Create new word every other tick (update boolean of World)
(define (generate-word-maybe low gen?)
  (if gen? low low)) ; TODO cons with (gen-new-word)

; - update-time: Number -> Number
;        -Keeping time of game
(define (update-time tick)
  (+ 1 tick))
(check-expect (update-time 0) 1)
(check-expect (update-time 100) 101)


;----------------
;to draw:

; - render-world World -> Image
;Render the 3 parts of the world
(check-expect (render-world world1)
              (place-falling-words (world-falling-words world1)
                                   (place-inactive-words (world-inactive-words world1)
                                                         (place-current-word (world-current-word world1) SCENE))))
(define (render-world w)
    (place-falling-words (world-falling-words w)
                         (place-inactive-words (world-inactive-words w)
                                               (place-current-word (world-current-word w) SCENE))))

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
(check-expect (place-current-word "test" SCENE)
              (place-image (text "test" FONT-SIZE TYPING-COLOR) TYPING-X TYPING-Y SCENE))

; -------------------
;on key:

; - update-word World KeyEvent -> World
;       -if alphabetic, add letter,
;       -if backspace, remove-letter,
;       -if enter, delete-word, ( use remove function )
;       -if else, do nothing
(define (update-word w key)
  (cond [(string-alphabetic? key)
         (make-world
          (world-falling-words w)
          (world-inactive-words w)
          (string-append (world-current-word w) key)
          (world-gen-word? w)
          (world-score w))]
        [(key=? key "\b")
         (make-world
          (world-falling-words w)
          (world-inactive-words w)
          (remove-letter (world-current-word w))
          (world-gen-word? w)
          (world-score w))]
        [(or (key=? key "\r") (key=? key 'numpad-enter))
         (make-world
          (remove-word (world-current-word w) (world-falling-words w))
          (world-inactive-words w)
          ""
          (world-gen-word? w)
          (world-score w))]
        ; TODO - extra credit - "up" and "down" keys will speed up/down tick rate, which will need to be put in World
        ; Then big bang on-tick will need to be [on-tick process (world-tick world)]
        [else w]))
; TODO many testssss

;remove-word : String LoW -> LoW
; Removes any word matching the string from the LoW
(check-expect (remove-word (word-str w1) list1) (list w2 w3))
(check-expect (remove-word (word-str w2) list1) (list w1 w3))
(check-expect (remove-word (word-str w1) list0) '())

(define (remove-word str low)
  (cond
    [(empty? low) '()]
    [(cons? low) (if (string=? str (word-str (first low)))
                     (remove-word str (rest low)) 
                     (cons (first low) (remove-word str (rest low))))]))
; remove all occurrences of the word? otherwise change this to just (cons (first low) (rest low))

; - remove-letter String -> String
; Removes the last letter from the string if there is one else returns empty string

(check-expect (remove-letter "Test") "Tes")
(check-expect (remove-letter "") "")

(define (remove-letter s)
  (if (= 0 (string-length s))
      "" (substring s 0 (- (string-length s) 1))))


; --------------
;stop when:
; - stop World -> Boolean
;    - determines when to stop game and words have reached limit of grid 
(check-expect (game-over (make-world (list (make-word "Word1" (make-posn 30 20)))
                                     (list (make-word "EndWord" (make-posn 11 0))
                                           (make-word "EndWord1" (make-posn 9 12)))
                                     "test"
                                     false
                                     1)) #t)
(check-expect (game-over (make-world (list (make-word "Word1" (make-posn 30 20)))
                                     (list (make-word "EndWord" (make-posn 10 12)))
                                     "test"
                                     false
                                     1)) #f)

(define (game-over w)
  (check-limit (world-inactive-words w)))

;check-limit:
; - LoW -> Boolean
;     - checks if any inactive word has posn-y of y=0 (top of grid)
(check-expect (check-limit (list (make-word "EndWord" (make-posn 10 0))
                                 (make-word "EndWord1" (make-posn 9 12)))) #t)
(check-expect (check-limit (list (make-word "EndWord" (make-posn 14 12))
                                 (make-word "EndWord1" (make-posn 9 12)))) #f)
(check-expect (check-limit (list (make-word "EndWord" (make-posn 5 12))
                                 (make-word "EndWord1" (make-posn 12 0)))) #t)

(define (check-limit low)
 (cond
    [(empty? low) #f]
    [(cons? low) (or (= 0  (posn-y (word-position (first low))))
                 (check-limit (rest low)))]))
                                                         

; - generate-score World Number -> Number
;    -outputs score in last-picture


(define (main world tick-rate)
  (big-bang world
            [on-tick process tick-rate]
            [to-draw render-world]
            [on-key update-word]
            [stop-when game-over]))
