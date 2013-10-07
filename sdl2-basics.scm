(module sdl2-basics
  (sdl-init sdl-quit sdl-init-subsystem sdl-quit-subsystem
   sdl-clear-hints sdl-get-hint sdl-set-hint sdl-set-hint-with-priority
   sdl-get-error sdl-set-error sdl-clear-error
   sdl-log sdl-log-message sdl-log-resert-priorities sdl-log-set-all-priority
   sdl-log-get-priority sdl-log-set-priority
   sdl-version sdl-version-major sdl-version-minor sdl-version-patch
   SDL-INIT-AUDIO
   SDL-INIT-VIDEO
   SDL-INIT-TIMER
   SDL-INIT-JOYSTICK
   SDL-INIT-HAPTIC
   SDL-INIT-GAMECONTROLLER
   SDL-INIT-EVENTS
   SDL-INIT-EVERYTHING
   SDL-INIT-NOPARACHUTE
   SDL-HINT-DEFAULT
   SDL-HINT-NORMAL
   SDL-HINT-OVERRIDE
   SDL-HINT-FRAMEBUFFER-ACCELERATION
   SDL-HINT-IDLE_TIMER-DISABLED
   SDL-HINT-ORIENTATIONS
   SDL-HINT-RENDER-DRIVER
   SDL-HINT-RENDER-OPENGL-SHADERS
   SDL-HINT-RENDER-SCALE-QUALITY
   SDL-HINT-RENDER-VSYNC
   SDL-LOG-CATEGORY-APPLICATION
   SDL-LOG-CATEGORY-ERROR
   SDL-LOG-CATEGORY-SYSTEM
   SDL-LOG-CATEGORY-AUDIO
   SDL-LOG-CATEGORY-VIDEO
   SDL-LOG-CATEGORY-RENDER
   SDL-LOG-CATEGORY-INPUT
   SDL-LOG-CATEGORY-CUSTOM
   SDL-LOG-PRIORITY-VERBOSE
   SDL-LOG-PRIORITY-DEBUG
   SDL-LOG-PRIORITY-INFO
   SDL-LOG-PRIORITY-WARN
   SDL-LOG-PRIORITY-ERROR
   SDL-LOG-PRIORITY-CRITICAL)
  (import scheme chicken foreign)
  (use c-struct data-structures extras)

(foreign-declare "#include <SDL2/SDL.h>")
(foreign-declare "#include <SDL2/SDL_hints.h>")
(foreign-declare "#include <SDL2/SDL_error.h>")
(foreign-declare "#include <SDL2/SDL_log.h>")
(foreign-declare "#include <SDL2/SDL_version.h>")

(define-c-struct (sdl-version "SDL_version")
  (constructor: make-sdl-version)
  (byte major sdl-version-major)
  (byte minor sdl-version-minor)
  (byte patch sdl-version-patch))

(define-record-printer (sdl-version v out)
  (fprintf out "#<sdl-version ~A.~A.~A>"
    (sdl-version-major v)
    (sdl-version-minor v)
    (sdl-version-patch v)))

(define (sdl-init flags)
  (let ((e ((foreign-lambda int "SDL_Init" unsigned-integer32) flags)))
    (unless (zero? e)
      (error "SDL_Init" e (sdl-get-error)))))

(define (sdl-init-subsystem flags)
  (let ((e ((foreign-lambda int "SDL_InitSubSystem" unsigned-integer32) flags)))
    (unless (zero? e)
      (error "SDL_InitSubSystem" e (sdl-get-error)))))

(define sdl-quit
  (foreign-lambda void "SDL_Quit"))

(define sdl-quit-subsystem
  (foreign-lambda void "SDL_QuitSubSystem" unsigned-integer32))

(define sdl-clear-hints
  (foreign-lambda void "SDL_ClearHints"))

(define sdl-get-hint
  (foreign-lambda c-string "SDL_GetHint" c-string))

(define sdl-set-hint
  (foreign-lambda bool "SDL_SetHint" c-string c-string))

(define sdl-set-hint-with-priority
  (foreign-lambda bool "SDL_SetHintWithPriority" c-string c-string int))

(define sdl-get-error
  (foreign-lambda c-string "SDL_GetError"))

(define sdl-set-error
  (foreign-lambda int "SDL_SetError" c-string))

(define sdl-clear-error
  (foreign-lambda void "SDL_ClearError"))

(define (sdl-log-priority priority)
  (case priority
    ((verbose) SDL-LOG-PRIORITY-VERBOSE)
    ((debug) SDL-LOG-PRIORITY-DEBUG)
    ((info) SDL-LOG-PRIORITY-INFO)
    ((warn) SDL-LOG-PRIORITY-WARN)
    ((error) SDL-LOG-PRIORITY-ERROR)
    ((critical) SDL-LOG-PRIORITY-CRITICAL)
    (else priority)))

(define (sdl-log-category category)
  (case category
    ((application) SDL-LOG-CATEGORY-APPLICATION)
    ((error) SDL-LOG-CATEGORY-ERROR)
    ((system) SDL-LOG-CATEGORY-SYSTEM)
    ((audio) SDL-LOG-CATEGORY-AUDIO)
    ((video) SDL-LOG-CATEGORY-VIDEO)
    ((render) SDL-LOG-CATEGORY-RENDER)
    ((input) SDL-LOG-CATEGORY-INPUT)
    (else category)))

(define (sdl-log . args)
  ((foreign-lambda* void ((c-string text))
     "SDL_Log(\"%s\", text);") (apply conc args)))

(define (sdl-log-message category priority . args)
  ((foreign-lambda* void ((int category) (int priority) (c-string text))
     "SDL_LogMessage(category, priority, \"%s\", text);")
   (sdl-log-category category) (sdl-log-priority priority) (apply conc args)))

(define sdl-log-resert-priorities
  (foreign-lambda void "SDL_LogResetPriorities"))

(define (sdl-log-set-all-priority priority)
  ((foreign-lambda void "SDL_LogSetAllPriority" int)
   (sdl-log-priority priority)))

(define (sdl-log-get-priority category)
  ((foreign-lambda int "SDL_LogGetPriority" int)
   (sdl-log-category category)))

(define (sdl-log-set-priority category priority)
  ((foreign-lambda void "SDL_LogSetPriority" int int)
   (sdl-log-priority priority)
   (sdl-log-category category)))

(define (sdl-version)
  (let ((version (make-sdl-version)))
    ((foreign-lambda void "SDL_GetVersion" c-pointer)
     (c-struct->pointer version sdl-version))
    version))

(define SDL-INIT-AUDIO (foreign-value "SDL_INIT_AUDIO" int))
(define SDL-INIT-VIDEO (foreign-value "SDL_INIT_VIDEO" int))
(define SDL-INIT-TIMER (foreign-value "SDL_INIT_TIMER" int))
(define SDL-INIT-JOYSTICK (foreign-value "SDL_INIT_JOYSTICK" int))
(define SDL-INIT-HAPTIC (foreign-value "SDL_INIT_HAPTIC" int))
(define SDL-INIT-GAMECONTROLLER (foreign-value "SDL_INIT_GAMECONTROLLER" int))
(define SDL-INIT-EVENTS (foreign-value "SDL_INIT_EVENTS" int))
(define SDL-INIT-EVERYTHING (foreign-value "SDL_INIT_EVERYTHING" int))
(define SDL-INIT-NOPARACHUTE (foreign-value "SDL_INIT_NOPARACHUTE" int))
(define SDL-HINT-DEFAULT (foreign-value "SDL_HINT_DEFAULT" int))
(define SDL-HINT-NORMAL (foreign-value "SDL_HINT_NORMAL" int))
(define SDL-HINT-OVERRIDE (foreign-value "SDL_HINT_OVERRIDE" int))
(define SDL-HINT-FRAMEBUFFER-ACCELERATION (foreign-value "SDL_HINT_FRAMEBUFFER_ACCELERATION" int))
(define SDL-HINT-IDLE_TIMER-DISABLED (foreign-value "SDL_HINT_IDLE_TIMER_DISABLED" int))
(define SDL-HINT-ORIENTATIONS (foreign-value "SDL_HINT_ORIENTATIONS" int))
(define SDL-HINT-RENDER-DRIVER (foreign-value "SDL_HINT_RENDER_DRIVER" int))
(define SDL-HINT-RENDER-OPENGL-SHADERS (foreign-value "SDL_HINT_RENDER_OPENGL_SHADERS" int))
(define SDL-HINT-RENDER-SCALE-QUALITY (foreign-value "SDL_HINT_RENDER_SCALE_QUALITY" int))
(define SDL-HINT-RENDER-VSYNC (foreign-value "SDL_HINT_RENDER_VSYNC" int))
(define SDL-LOG-CATEGORY-APPLICATION (foreign-value "SDL_LOG_CATEGORY_APPLICATION" int))
(define SDL-LOG-CATEGORY-ERROR (foreign-value "SDL_LOG_CATEGORY_ERROR" int))
(define SDL-LOG-CATEGORY-SYSTEM (foreign-value "SDL_LOG_CATEGORY_SYSTEM" int))
(define SDL-LOG-CATEGORY-AUDIO (foreign-value "SDL_LOG_CATEGORY_AUDIO" int))
(define SDL-LOG-CATEGORY-VIDEO (foreign-value "SDL_LOG_CATEGORY_VIDEO" int))
(define SDL-LOG-CATEGORY-RENDER (foreign-value "SDL_LOG_CATEGORY_RENDER" int))
(define SDL-LOG-CATEGORY-INPUT (foreign-value "SDL_LOG_CATEGORY_INPUT" int))
(define SDL-LOG-CATEGORY-CUSTOM (foreign-value "SDL_LOG_CATEGORY_CUSTOM" int))
(define SDL-LOG-PRIORITY-VERBOSE (foreign-value "SDL_LOG_PRIORITY_VERBOSE" int))
(define SDL-LOG-PRIORITY-DEBUG (foreign-value "SDL_LOG_PRIORITY_DEBUG" int))
(define SDL-LOG-PRIORITY-INFO (foreign-value "SDL_LOG_PRIORITY_INFO" int))
(define SDL-LOG-PRIORITY-WARN (foreign-value "SDL_LOG_PRIORITY_WARN" int))
(define SDL-LOG-PRIORITY-ERROR (foreign-value "SDL_LOG_PRIORITY_ERROR" int))
(define SDL-LOG-PRIORITY-CRITICAL (foreign-value "SDL_LOG_PRIORITY_CRITICAL" int))
)
