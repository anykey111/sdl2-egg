(use sdl2-basics sdl2-events sdl2-video sdl2-ttf)

(define running  (make-parameter #t))
(define window)
(define render)
(define font)
(define font-surface)
(define font-texture)

(define (main-loop)
  (let ((e (sdl-poll-event)))
    (if e (handle-event e)))
  (sdl-set-render-draw-color render 0 0 0 255)
  (sdl-render-clear render)
  (sdl-set-render-draw-color render 255 0 0 255)
  (sdl-render-draw-line render 10 10 100 100)
  (sdl-render-draw-lines render
    (make-sdl-point 100 100)
    (make-sdl-point 100 200)
    (make-sdl-point 200 100)
    (make-sdl-point 200 200))
  (sdl-render-draw-point render 300 300)
  (sdl-render-draw-points render
    (make-sdl-point 299 300)
    (make-sdl-point 301 300)
    (make-sdl-point 300 299)
    (make-sdl-point 300 301))
  (sdl-render-draw-rect render (make-sdl-rect 295 295 50 50))
  (sdl-render-fill-rect render (make-sdl-rect 305 305 5 8))
  (sdl-render-copy render font-texture #f (make-sdl-rect 300 100 50 50))
  (sdl-render-present render)
  (if (running)
    (main-loop)))

(define (handle-event e)
  (sdl-log "event:" e)
  (select (sdl-event-type e)
    ((SDL-QUIT)
     (sdl-log "quit:")
     (running #f))
    ((SDL-KEYUP SDL-KEYDOWN)
     (sdl-log "key:" (sdl-event-user e)))))

(define (main)
  (sdl-log (sdl-version))
  (sdl-init (+ SDL-INIT-VIDEO))
  (ttf-init)
  (set! font (ttf-open-font "lucon.ttf" 20))
  (set! window (sdl-create-window "sdl2 test" 100 100 640 480 SDL-WINDOW-OPENGL))
  (set! render (sdl-create-renderer window))
  (set! font-surface (ttf-render-text-solid font "FIGA" (make-sdl-color 0 255 0 255)))
  (set! font-texture (sdl-create-texture-from-surface render font-surface))
  (main-loop)
  (ttf-quit)
  (sdl-quit))

(main)