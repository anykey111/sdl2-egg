(module sdl2-basics
  (sdl-init sdl-quit sdl-init-subsystem sdl-quit-subsystem
   sdl-clear-hints sdl-get-hint sdl-set-hint sdl-set-hint-with-priority
   sdl-get-error sdl-set-error sdl-clear-error
   sdl-log sdl-log-message sdl-log-resert-priorities sdl-log-set-all-priority
   sdl-log-get-priority sdl-log-set-priority
   sdl-version sdl-version-major sdl-version-minor sdl-version-patch)
  (import scheme chicken foreign)
  (use c-struct data-structures extras sdl2-defs)

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

)
