#lang racket/base

(require 2htdp/image
         racket/list
         racket/format
         racket/match
         racket/string)

(provide kona-palette
         kona-names
         kona->name
         kona->string
         name->kona
         name->kona-fraction
         kona-categories
         show-kona-palette
         color->rgb-string
         kona-colors
         monochrome-palette)

(define use-new-konas #f)

(define kona-colors
  (list
   (list 'meringue (color 254 242 192))
   (list 'butter (color 249 222 175))
   (list 'maize (color 255 239 170))
   (list 'buttercup (color 253 220 141))
   (list 'citrus (color 255 216 35))
   (list 'canary (color 255 199 18))
   (list 'lemon (color 255 221 131))
   (list 'corn-yellow (color 254 190 41))
   (list 'curry (color 236 182 84))
   (list 'mustard (color 245 218 163))
   (list 'banana (color 246 208 127))
   (list 'daffodil (color 254 206 132))
   (list 'sunflower (color 250 223 146))
   (list 'cheddar (color 255 204 141))
   (list 'ochre (color 242 174 91))
   (list 'butterscotch (color 210 162 88))
   (list 'yarrow (color 196 136 50))
   (list 'caramel (color 191 142 84))
   (list 'gold (color 180 107 52))
   (list 'amber (color 235 146 44))
   (list 'cedar (color 214 106 57))
   (list 'sunny (color 245 179 30))
   (list 'papaya (color 248 165 33))
   (list 'school-bus (color 241 138 1))
   (list 'orange (color 255 109 34))
   (list 'persimmon (color 255 106 37))
   (list 'mango (color 234 145 105))
   (list 'carrot (color 247 87 55))
   (list 'kumquat (color 253 106 36))
   (list 'torch (color 255 107 34))
   (list 'tangerine (color 244 76 39))
   (list 'flame (color 242 66 42))
   (list 'lipstick (color 202 42 42))
   (list 'tomato (color 185 11 36))
   (list 'poppy (color 151 40 31))
   (list 'cardinal (color 182 0 48))
   (list 'red (color 200 21 42))
   (list 'chinese-red (color 138 25 31))
   (list 'rich-red (color 134 11 39))
   (list 'ruby (color 159 58 50))
   (list 'wine (color 97 3 29))
   (list 'crimson (color 120 50 60))
   (list 'burgundy (color 71 29 39))
   (list 'garnet (color 84 35 54))
   (list 'brick (color 87 25 30))
   (list 'mahogany (color 56 25 20))
   (list 'cocoa (color 80 32 30))
   (list 'cinnamon (color 100 44 29))
   (list 'spice (color 155 85 60))
   (list 'paprika (color 132 43 27))
   (list 'sienna (color 175 88 81))
   (list 'cayenne (color 153 64 58))
   (list 'coral (color 224 45 49))
   (list 'punch (color 255 87 128))
   (list 'melon (color 246 133 139))
   (list 'salmon (color 235 141 115))
   (list 'peach (color 255 175 164))
   (list 'ice-peach (color 253 207 183))
   (list 'light-parfait (color 252 216 182))
   (list 'pale-flesh (color 251 217 205))
   (list 'pearl-pink (color 253 236 242))
   (list 'pink (color 253 205 217))
   (list 'flesh (color 253 197 198))
   (list 'baby-pink (color 250 193 212))
   (list 'peony (color 243 190 200))
   (list 'dusty-peach (color 255 184 188))
   (list 'primrose (color 249 195 193))
   (list 'medium-pink (color 253 168 199))
   (list 'carnation (color 244 153 186))
   (list 'bubble-gum (color 252 149 176))
   (list 'candy-pink (color 250 137 191))
   (list 'camellia (color 238 108 142))
   (list 'azalea (color 247 61 132))
   (list 'bright-pink (color 254 68 151))
   (list 'valentine (color 235 52 142))
   (list 'pomegranate (color 204 26 102))
   (list 'deep-rose (color 192 99 120))
   (list 'blush-pink (color 229 119 158))
   (list 'woodrose (color 249 166 192))
   (list 'rose (color 225 132 159))
   (list 'petal (color 249 189 223))
   (list 'plum (color 170 79 140))
   (list 'cerise (color 138 39 103))
   (list 'berry (color 99 21 81))
   (list 'eggplant (color 65 18 62))
   (list 'raisin (color 48 26 38))
   (list 'hibiscus (color 58 28 66))
   (list 'dark-violet (color 74 17 85))
   (list 'mulberry (color 84 50 111))
   (list 'tulip (color 89 76 132))
   (list 'crocus (color 120 95 160))
   (list 'wisteria (color 174 139 203))
   (list 'pansy (color 190 160 198))
   (list 'lupine (color 182 123 187))
   (list 'magenta (color 135 78 157))
   (list 'violet (color 188 125 195))
   (list 'petunia (color 197 162 194))
   (list 'thistle (color 164 147 192))
   (list 'lilac (color 153 142 182))
   (list 'orchid (color 202 173 217))
   (list 'lavender (color 140 139 207))
   (list 'amethyst (color 116 113 166))
   (list 'brt-peri (color 67 52 117))
   (list 'purple (color 67 44 88))
   (list 'regal (color 31 34 79))
   (list 'midnight (color 37 28 55))
   (list 'deep-blue (color 39 64 146))
   (list 'lapis (color 83 115 192))
   (list 'hyacinth (color 70 106 184))
   (list 'slate (color 98 111 143))
   (list 'dresden-blue (color 136 163 206))
   (list 'denim (color 123 159 221))
   (list 'candy-blue (color 121 162 208))
   (list 'periwinkle (color 130 167 245))
   (list 'blue-jay (color 100 143 211))
   (list 'cornflower (color 149 175 234))
   (list 'grapemist (color 132 154 214))
   (list 'blue-bell (color 157 182 222))
   (list 'cloud (color 195 209 236))
   (list 'blueberry (color 183 216 249))
   (list 'stratosphere (color 95 173 235))
   (list 'evening (color 105 157 230))
   (list 'copen (color 64 132 207))
   (list 'regatta (color 56 85 143))
   (list 'delft (color 76 117 163))
   (list 'cadet (color 53 83 121))
   (list 'windsor (color 38 53 84))
   (list 'prussian (color 0 55 103))
   (list 'navy (color 1 30 60))
   (list 'storm (color 16 42 79))
   (list 'indigo (color 15 31 54))
   (list 'nautical (color 28 28 56))
   (list 'nightfall (color 20 26 86))
   (list 'marine (color 22 54 115))
   (list 'riviera (color 1 62 127))
   (list 'ocean (color 6 39 110))
   (list 'surf (color 29 81 164))
   (list 'royal (color 27 73 231))
   (list 'pacific (color 0 62 129))
   (list 'water (color 7 150 210))
   (list 'alegria (color 61 151 214))
   (list 'lake (color 146 204 244))
   (list 'fog (color 170 201 222))
   (list 'blue (color 200 224 250))
   (list 'sky (color 215 231 244))
   (list 'dusty-blue (color 196 219 237))
   (list 'baby-blue (color 190 217 234))
   (list 'aqua (color 166 226 236))
   (list 'azure (color 163 233 241))
   (list 'robin-egg (color 138 212 241))
   (list 'bahama-blue (color 120 213 231))
   (list 'capri (color 130 211 215))
   (list 'peacock (color 68 164 222))
   (list 'turquoise (color 0 146 183))
   (list 'lagoon (color 27 185 233))
   (list 'cyan (color 9 175 213))
   (list 'caribbean (color 0 167 205))
   (list 'oasis (color 7 117 168))
   (list 'teal-blue (color 22 89 132))
   (list 'celestial (color 8 81 134))
   (list 'glacier (color 0 132 150))
   (list 'everglade (color 41 108 125))
   (list 'emerald (color 7 105 118))
   (list 'jade-green (color 16 138 113))
   (list 'breakers (color 2 173 199))
   (list 'sage (color 118 182 181))
   (list 'pool (color 26 219 218))
   (list 'candy-green (color 107 199 184))
   (list 'aloe (color 152 203 188))
   (list 'pond (color 151 213 190))
   (list 'ice-frappe (color 179 231 218))
   (list 'mint (color 187 229 181))
   (list 'asparagus (color 165 210 167))
   (list 'pistachio (color 117 203 142))
   (list 'leaf (color 98 141 95))
   (list 'laurel (color 106 137 93))
   (list 'spring (color 129 170 130))
   (list 'celadon (color 126 171 148))
   (list 'old-green (color 119 169 144))
   (list 'seafoam (color 163 199 189))
   (list 'tarragon (color 185 200 133))
   (list 'green-tea (color 198 219 154))
   (list 'honey-dew (color 182 218 146))
   (list 'pear (color 136 218 120))
   (list 'sour-apple (color 101 213 87))
   (list 'kiwi (color 99 211 99))
   (list 'leprechaun (color 45 116 56))
   (list 'fern (color 32 120 70))
   (list 'clover (color 5 120 63))
   (list 'jungle (color 10 96 49))
   (list 'holly (color 7 122 93))
   (list 'cypress (color 70 184 161))
   (list 'kale (color 10 180 154))
   (list 'blue-grass (color 0 164 146))
   (list 'willow (color 2 83 68))
   (list 'kelly (color 13 65 52))
   (list 'juniper (color 20 51 43))
   (list 'forest (color 29 56 37))
   (list 'spruce (color 0 59 59))
   (list 'hunter-green (color 19 37 21))
   (list 'evergreen (color 40 57 38))
   (list 'pesto (color 0 77 52))
   (list 'basil (color 45 81 35))
   (list 'palm (color 57 67 33))
   (list 'peridot (color 111 138 71))
   (list 'bonsai (color 145 155 69))
   (list 'olive (color 163 166 89))
   (list 'artichoke (color 186 189 118))
   (list 'celery (color 225 222 151))
   (list 'zucchini (color 212 210 133))
   (list 'wasabi (color 222 206 49))
   (list 'cactus (color 197 216 100))
   (list 'peapod (color 164 185 20))
   (list 'sprout (color 153 177 83))
   (list 'chartreuse (color 169 195 60))
   (list 'lime (color 145 170 8))
   (list 'grass-green (color 78 122 45))
   (list 'avocado (color 73 90 36))
   (list 'moss (color 76 73 42))
   (list 'ivy (color 104 117 64))
   (list 'herb (color 169 163 129))
   (list 'sweet-pea (color 131 132 100))
   (list 'parsley (color 185 181 134))
   (list 'straw (color 206 176 148))
   (list 'stone (color 163 151 129))
   (list 'mushroom (color 122 104 90))
   (list 'bison (color 128 119 90))
   (list 'taupe (color 159 127 112))
   (list 'sable (color 88 57 39))
   (list 'cappuccino (color 67 51 35))
   (list 'chestnut (color 73 54 24))
   (list 'espresso (color 46 37 30))
   (list 'coffee (color 64 39 34))
   (list 'chocolate (color 68 48 39))
   (list 'brown (color 61 31 23))
   (list 'mocha (color 87 54 49))
   (list 'earth (color 108 71 52))
   (list 'biscuit (color 162 134 94))
   (list 'honey (color 196 157 102))
   (list 'wheat (color 215 171 106))
   (list 'raffia (color 218 191 164))
   (list 'parchment (color 203 199 170))
   (list 'khaki (color 215 206 177))
   (list 'tan (color 214 188 165))
   (list 'sand (color 241 217 193))
   (list 'putty (color 218 203 180))
   (list 'champagne (color 233 217 184))
   (list 'eggshell (color 250 246 211))
   (list 'cream (color 255 232 208))
   (list 'natural (color 242 231 213))
   (list 'ivory (color 254 247 231))
   (list 'bone (color 247 239 226))
   (list 'oyster (color 238 228 218))
   (list 'snow (color 246 246 244))
   (list 'white (color 246 246 246))
   (list 'silver (color 220 220 222))
   (list 'shadow (color 212 212 212))
   (list 'ash (color 188 184 183))
   (list 'medium-grey (color 145 140 140))
   (list 'iron (color 141 160 167))
   (list 'shale (color 144 159 162))
   (list 'smoke (color 166 150 151))
   (list 'pewter (color 152 150 155))
   (list 'coal (color 96 94 108))
   (list 'steel (color 114 122 133))
   (list 'graphite (color 98 120 133))
   (list 'charcoal (color 44 45 49))
   (list 'pepper (color 29 33 34))
   (list 'black (color 20 20 20))))


(define kona-categories
  '((quick-yellows
     (maize citrus corn-yellow butterscotch gold))
    (quick-oranges (school-bus orange tangerine))
    (quick-reds (coral ruby red burgundy))
    (quick-pinks
     (pearl-pink
      baby-pink
      peach
      bubble-gum
      candy-pink
      deep-rose
      azalea
      pomegranate))
    (quick-purples (orchid violet lavender purple))
    (quick-blues
     (sky
      blueberry
      aqua
      cloud
      periwinkle
      grapemist
      evening
      lagoon
      royal
      indigo))
    (quick-greens
     (mint
      seafoam
      chartreuse
      pistachio
      sour-apple
      sage
      kale
      fern
      avocado
      hunter-green))
    (quick-browns
     (raffia
      biscuit
      mushroom
      spice
      earth
      chocolate
      cocoa
      espresso))
    (quick-neutrals
     (snow bone natural silver ash steel charcoal black))
    (yellows
     (meringue
      maize
      butter
      sunflower
      lemon
      buttercup
      mustard
      cheddar
      daffodil
      banana
      citrus
      canary
      corn-yellow
      curry
      ochre
      sunny
      wheat
      papaya
      butterscotch
      caramel
      yarrow
      gold))
    (oranges
     (papaya
      mango
      amber
      school-bus
      orange
      torch
      persimmon
      kumquat
      cedar
      carrot
      tangerine))
    (reds
     (flame
      coral
      cayenne
      lipstick
      ruby
      red
      poppy
      crimson
      paprika
      tomato
      cardinal
      chinese-red
      rich-red
      burgundy
      wine))
    (pinks
     (pearl-pink
      pale-flesh
      light-parfait
      pink
      ice-peach
      flesh
      baby-pink
      primrose
      petal
      peony
      dusty-peach
      peach
      medium-pink
      woodrose
      carnation
      bubble-gum
      candy-pink
      melon
      salmon
      rose
      blush-pink
      camellia
      punch
      bright-pink
      deep-rose
      azalea
      valentine
      sienna
      pomegranate))
    (purples
     (orchid
      petunia
      pansy
      thistle
      wisteria
      violet
      lilac
      lupine
      lavender
      amethyst
      plum
      crocus
      magenta
      tulip
      cerise
      crimson
      mulberry
      brt-peri
      purple
      garnet
      berry
      dark-violet
      hibiscus
      eggplant
      raisin))
    (blues
     (sky
      blue
      dusty-blue
      azure
      baby-blue
      blueberry
      aqua
      cloud
      fog
      robin-egg
      lake
      bahama-blue
      capri
      blue-bell
      cornflower
      periwinkle
      pool
      dresden-blue
      stratosphere
      denim
      candy-blue
      grapemist
      evening
      lagoon
      peacock
      blue-jay
      alegria
      cyan
      breakers
      caribbean
      copen
      lapis
      water
      slate
      delft
      turquoise
      hyacinth
      glacier
      oasis
      regatta
      cadet
      royal
      surf
      teal-blue
      deep-blue
      celestial
      brt-peri
      windsor
      marine
      riviera
      pacific
      prussian
      storm
      regal
      ocean
      midnight
      nautical
      nightfall
      indigo
      navy))
    (greens
     (celery
      ice-frappe
      mint
      green-tea
      zucchini
      honey-dew
      cactus
      wasabi
      pond
      asparagus
      tarragon
      seafoam
      aloe
      pear
      artichoke
      parsley
      chartreuse
      pistachio
      candy-green
      sour-apple
      kiwi
      sage
      herb
      peapod
      sprout
      olive
      celadon
      spring
      old-green
      cypress
      lime
      bonsai
      sweet-pea
      kale
      leaf
      laurel
      peridot
      blue-grass
      ivy
      grass-green
      jade-green
      everglade
      fern
      leprechaun
      holly
      clover
      avocado
      emerald
      moss
      basil
      jungle
      palm
      willow
      pesto
      evergreen
      kelly
      forest
      spruce
      juniper
      hunter-green))
    (browns
     (sand
      champagne
      khaki
      putty
      parchment
      raffia
      tan
      straw
      honey
      stone
      biscuit
      taupe
      gold
      bison
      mushroom
      spice
      earth
      sable
      mocha
      cinnamon
      chestnut
      cappuccino
      chocolate
      cocoa
      coffee
      brick
      brown
      espresso
      mahogany))
    (neutrals
     (ivory
      white
      snow
      eggshell
      bone
      cream
      natural
      oyster
      silver
      shadow
      ash
      iron
      ;smoke
      shale
      pewter
      medium-grey
      steel
      graphite
      coal
      charcoal
      pepper
      black))))

(define kona-palette (map second kona-colors))
(define kona-names (map first kona-colors))
(define colors-kona (map reverse kona-colors))

(define (name->kona s)
  (let* ([key (if (string? s) 
                  (string->symbol (string-downcase s))
                  s)]
         [pair (assoc key kona-colors)])
    (if pair 
        (second pair)
        #f)))

; name->kona-fraction : symbol or string -> ([0.0, 1.0] [0.0, 1.0] [0.0, 1.0])
;
(define (name->kona-fraction s)
  (let ([c (name->kona s)])
    (if c
        (list
         (/ (color-red c) 255.0)
         (/ (color-green c) 255.0)
         (/ (color-blue c) 255.0))
        (list 1.0 0.0 0.0))))

; kona->name : color -> symbol or #f
;
(define (kona->name c)
  (let ([pair (assoc c colors-kona)])
    (if pair 
        (second pair)
        #f)))

; kona->string : color -> string or #f
(define (kona->string rgb)
  (~a "Kona " (string-replace 
               (string-titlecase (symbol->string (kona->name rgb)))
               "-" " ")))

;; Display a kona-based palette: swatch, name, and rgb. 
;;
(define (show-kona-palette pal)
  (map (λ (c) (list (circle 20 'solid c) (kona->name c) c)) pal))


(define (color->list c)
  (match-let ([(color r g b _) c])
    (list r g b)))




; Given a category name (e.g. 'neutrals), what are the corresponding ; colors
;
(define (monochrome-palette category)
  (map name->kona (second (assoc category kona-categories))))  


;; Convert a color into an integer code
;;
(define (color->int c)
  (let ([r (color-red c)]
        [g (color-green c)]
        [b (color-blue c)])
    (+ b (* 256 (+ g (* 256 r))))))


(define (color->rgb-string c)
  (format "#~a"
          (string-upcase 
           (regexp-replace* " " 
                            (~r (color->int c) #:base 16 #:min-width 6) 
                            "0"))))


;; Mapping suitable for conversion to json 
;;
(define konas-as-jsexpr
  (for/list ([entry kona-colors])
    (list (string-titlecase 
           (regexp-replace* "-" (symbol->string (first entry)) " "))
          (color->rgb-string (second entry)))))


(module+ test
  (require rackunit)
  
  (check-equal? (name->kona 'pepper) (color 29 33 34))
  (check-equal? (name->kona "pepper") (color 29 33 34))
  (check-equal? (name->kona "PEPPER") (color 29 33 34))
  (check-equal? (name->kona 'foobar) #f)
  
  (check-equal? (kona->name (color 29 33 34)) 'pepper)
  (check-equal? (kona->name (color 1 2 3)) #f)
  
  #;(map (λ (c) (list (overlay
                       (circle 8 'outline 'black)
                       (circle 8 'solid c))
                      
                      (kona->name c)))
         kona-palette)
  
  #;(for ([cat kona-categories])
      (newline)
      (display (first cat)) (newline)
      (for/list ([c (second cat)])
        (printf "  ~a ~a~n"
                (overlay
                 (circle 8 'outline 'black)
                 (circle 8 'solid (name->kona c)))
                c))))

#;(require json)

#;(display (jsexpr->string
            (for/hash ([cat kona-categories])
              (values (first cat)
                      (for/list ([c (second cat)])
                        (color->rgb-string (name->kona c)))))))

#;(for ([cat kona-categories])
    (match-define (list name cs) cat)
    (newline)
    (displayln (text (~a name) 18 'black))
    (display (apply beside
                    (for/list ([c cs])
                      (above (overlay
                              (square 75 'outline 'black)
                              (square 75 'solid (name->kona c)))
                             (text (~a c) 14 'black))))))


