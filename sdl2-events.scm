(module sdl2-events
  (sdl-poll-event
   sdl-event-type sdl-event-user sdl-event-key)
  (import scheme chicken foreign)
  (use sdl2-defs c-struct)

(foreign-declare "#include <SDL2/SDL_events.h>")

(define-c-struct (sdl-event "SDL_Event")
  (constructor: make-sdl-event)
  (unsigned-integer type sdl-event-type)
  ((struct "SDL_UserEvent") user sdl-event-user)
  ((struct "SDL_KeyboardEvent") key sdl-event-key))

(define-c-struct (sdl-user-event "SDL_UserEvent")
  (constructor: make-sdl-user-event)
  (unsigned-integer type sdl-user-event-type)
  (unsigned-integer timestamp sdl-user-event-timestamp)
  (unsigned-integer windowID sdl-user-event-window-id)
  (integer code sdl-user-event-code)
  (c-pointer data1 sdl-user-event-data1)
  (c-pointer data2 sdl-user-event-data2))

(define-c-struct (sdl-keyboard-event "SDL_KeyboardEvent")
  (constructor: make-sdl-keyboard-event)
  (unsigned-integer type sdl-keyboard-event-type)
  (unsigned-integer timestamp sdl-keyboard-event-time)
  (unsigned-integer windowID sdl-keyboard-event-window-id)
  (byte state sdl-keyboard-event-state)
  (byte repeat sdl-keyboard-event-repeat)
  ((struct "SDL_Keysym") keysym sdl-keyboard-event-keysym))

(define-c-struct (sdl-keysym "SDL_Keysym")
  (constructor: make-sdl-keysym)
  (integer scancode sdl-keysym-scancode)
  (integer sym sdl-keysym-sym)
  (unsigned-short mod sdl-keysym-mod))

(define-c-struct (sdl-mouse-button-event "SDL_MouseButtonEvent")
  (integer x sdl-mouse-button-event-x)
  (integer y sdl-mouse-button-event-y))

(define (sdl-poll-event)
  (let ((e (make-sdl-event)))
    (and (= 1 ((foreign-lambda int "SDL_PollEvent" (c-pointer "SDL_Event"))
               (c-struct->pointer e)))
         e)))

)
