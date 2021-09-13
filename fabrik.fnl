;; vector math

(fn vec2 [x y]
  (if (not x) [0 0]
      (not y) [x x]
      [x y]))

(fn vec-length [[x y]]
  (math.sqrt (+ (^ x 2) (^ y 2))))

(fn vec-sub [[x0 y0] [x1 y1]]
  [(- x0 x1) (- y0 y1)])

(fn vec-add [[x0 y0] [x1 y1]]
  [(+ x0 x1) (+ y0 y1)])

(fn vec-mul [[x0 y0] [x1 y1]]
  [(* x0 x1) (* y0 y1)])

(fn norm [[x y &as v]]
  (let [len (vec-length v)]
    [(/ x len) (/ y len)]))

(fn move-point [point dir distance]
  (vec-add point (vec-mul (norm dir) (vec2 distance))))

(fn distance [[x1 y1] [x2 y2]]
  (math.abs (math.sqrt (+ (^ (- x2 x1) 2) (^ (- y2 y1) 2)))))

;; Thing

(local max-steps 3)
(var thing nil)

(fn create-thing [n-points max-distance]
  (let [(width height) (love.graphics.getDimensions)
        tmp [[(/ width 2) (/ height 2)]]
        thing []]
    (for [i 2 (+ n-points 1)]
      (let [[x y] (. tmp (- i 1))]
        (table.insert tmp [(+ x (* i max-distance)) y])))
    (for [i (length tmp) 1 -1]
      (table.insert thing (. tmp i)))
    thing))

(fn solve-thing [thing target]
  (let [len (length thing)
        origin (. thing len)
        lens []]
    (for [i 1 (- len 1)]
      (table.insert lens (distance (. thing i) (. thing (+ i 1)))))
    (for [i 0 max-steps]
      (let [start? (= 0 (% i 2))
            from (if start? 2 (- len 1))
            to (if start? len 1)
            step (if start? 1 -1)]
        (tset thing (if start? 1 len) (if start? target origin))
        (for [i from to step]
          (let [p1 (. thing i)
                p2 (. thing (- i step))
                dir (vec-sub p1 p2)
                dist (. lens (if start? (- i 1) i))]
            (tset thing i (move-point p2 dir dist))))))))

;; drawing

(fn draw-thing [thing]
  (let [len (length thing)]
    (for [i 1 (- len 1)]
      (let [[a b] (. thing i)
            [c d] (. thing (+ i 1))
            r 5]
        (love.graphics.setColor [0.752 0.772 0.807])
        (love.graphics.line a b c d)
        (love.graphics.setColor [0.749 0.380 0.415])
        (love.graphics.circle :fill a b r)
        (when (= i (- len 1))
          (love.graphics.circle :fill c d r)))))
  (love.graphics.setColor [0.396 0.450 0.494])
  (each [_ [x y] (ipairs thing)]
    (love.graphics.print (: "%s,%s" :format
                            (/ (math.floor (* 10 x)) 10)
                            (/ (math.floor (* 10 y)) 10))
                         (+ x 5) (+ y 5))))

(fn draw-cursor [[a b]]
  (love.graphics.setColor [0.925 0.745 0.482])
  (love.graphics.circle :fill a b 8))

(fn draw-descr []
  (love.graphics.print "F.A.B.R.I.K.
Forward and Backward Reaching Inverse Kinematics" 10 10))

(fn love.draw []
  (let [mouse-pos (vec2 (love.mouse.getPosition))]
    (draw-descr)
    (draw-cursor mouse-pos)
    (solve-thing thing mouse-pos)
    (draw-thing thing)))

;; Init

(local window-width 512)
(local window-height 448)
(local window-flags {:resizable false :vsync true :minwidth 256 :minheight 224})

(fn love.load []
  (love.window.setTitle "LÖVE FABRIK")
  (love.window.setMode window-width window-height window-flags)
  (love.mouse.setVisible false)
  (love.graphics.setBackgroundColor [0.168 0.188 0.231])
  (set thing (create-thing 5 10)))
