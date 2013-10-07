(module sdl2-timer
 (sdl-add-timer
  sdl-delay
  sdl-get-performance-counter
  sdl-get-performance-frequency
  sdl-get-ticks
  sdl-remove-timer
  sdl-timer-invoke-callback)
 (import scheme chicken foreign)
 (use srfi-1)

(foreign-declare "#include \"sdl2-timer.ffi.c\"")

(define *sdl2-timers* '())

(define (sdl-add-timer interval callback param)
  (let ((id ((foreign-lambda int "sdl2_add_timer" unsigned-integer32 bool)
             interval #f)))
    (and id
      (begin (set! *sdl2-timers* (alist-cons id (list callback param) *sdl2-timers*))
              id))))

(define (sdl-remove-timer id)
  (let ((result ((foreign-lambda bool "SDL_RemoveTimer" int) id)))
    (if result
      (set! *sdl2-timers* (alist-delete id *sdl2-timers*)))
    result))

(define (sdl-delay ms)
  ((foreign-lambda void "SDL_Delay" unsigned-integer32) ms))

(define sdl-get-performance-counter
  ((foreign-lambda unsigned-integer64 "SDL_GetPerformanceCounter")))

(define sdl-get-performance-frequency
  ((foreign-lambda unsigned-integer64 "SDL_GetPerformanceFrequency")))

(define (sdl-get-ticks)
  ((foreign-lambda unsigned-integer32 "SDL_GetTicks")))

(define (sdl-timer-invoke-callback id)
  (let ((kv (assoc id *sdl2-timers*))) ;; (id . (proc user-data)))
    (and kv
      (or (apply (car (cdr kv)) (cdr (cdr kv)))
          (sdl-remove-timer (car kv))))))

)

