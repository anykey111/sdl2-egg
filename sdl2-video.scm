(module sdl2-video
  (sdl-create-window
   sdl-create-window-and-renderer
   sdl-create-window-from
   sdl-destroy-window
   sdl-create-renderer
   sdl-set-render-draw-color
   sdl-render-clear
   sdl-render-present
   sdl-render-draw-line
   sdl-render-draw-lines
   sdl-render-draw-point
   sdl-render-draw-points
   make-sdl-point sdl-point-x sdl-point-y
   sdl-render-draw-rect
   sdl-render-fill-rect
   make-sdl-color
   make-sdl-rect sdl-rect-x sdl-rect-y sdl-rect-w sdl-rect-h
                 sdl-rect-x! sdl-rect-y! sdl-rect-w! sdl-rect-h!
   sdl-create-texture-from-surface
   sdl-render-copy
   sdl-disable-screensaver
   sdl-enable-screensaver
   sdl-video-init
   sdl-video-quit
   sdl-gl-set-attribute sdl-gl-get-attribute
   sdl-gl-create-context sdl-gl-delete-context sdl-gl-make-current
   sdl-gl-set-swap-interval sdl-gl-get-swap-interval sdl-gl-swap-window
   sdl-gl-get-current-context sdl-gl-get-current-window
   sdl-gl-bind-texture sdl-gl-unbind-texture
   sdl-load-bmp sdl-free-surface sdl-destroy-texture)
  (import scheme chicken foreign)
  (use sdl2-basics sdl2-defs c-struct srfi-1 lolevel)

(foreign-declare "#include <SDL2/SDL_video.h>")
(foreign-declare "#include <SDL2/SDL_render.h>")

(define-foreign-type SDL-Window (c-pointer "SDL_Window"))
(define-foreign-type SDL-Renderer (c-pointer "SDL_Renderer"))
(define-foreign-type SDL-Surface (c-pointer "SDL_Surface"))
(define-foreign-type SDL-Texture (c-pointer "SDL_Texture"))
(define-foreign-type SDL-GLContext (c-pointer "SDL_GLContext"))
(define-foreign-type SDL-Point (c-pointer "SDL_Point"))
(define-foreign-type SDL-Rect (c-pointer "SDL_Rect"))

(define-c-struct (sdl-window "struct SDL_Window*"))
(define-c-struct (sdl-renderer "struct SDL_Renderer*"))
(define-c-struct (sdl-texture "struct SDL_Texture*"))
(define-c-struct (sdl-glcontext "struct SDL_GLContext*"))
(define-c-struct (sdl-surface "SDL_Surface"))

(define-c-struct (sdl-pixel-format "SDL_PixelFormat")
  (unsigned-int format sdl-pixel-format-format)
  (c-pointer palette sdl-pixel-format-palette)
  (unsigned-char BitsPerPixel sdl-pixel-format-bits-per-pixel))

(define-c-struct (sdl-point "SDL_Point")
  (int x sdl-point-x sdl-point-x!)
  (int y sdl-point-y sdl-point-y!))

(define (make-sdl-point #!optional (x 0) (y 0))
  (let ((p (sdl-point)))
    (sdl-point-x! p x)
    (sdl-point-y! p y)
    p))

(define-c-struct (sdl-rect "SDL_Rect")
  (int x sdl-rect-x sdl-rect-x!)
  (int y sdl-rect-y sdl-rect-y!)
  (int w sdl-rect-w sdl-rect-w!)
  (int h sdl-rect-h sdl-rect-h!))

(define (make-sdl-rect #!optional (x 0) (y 0) (w 0) (h 0))
  (let ((p (sdl-rect)))
    (sdl-rect-x! p x)
    (sdl-rect-y! p y)
    (sdl-rect-w! p w)
    (sdl-rect-h! p h)
    p))

(define-c-struct (sdl-color "SDL_Color")
  (byte r sdl-color-r sdl-color-r!)
  (byte g sdl-color-g sdl-color-g!)
  (byte b sdl-color-b sdl-color-b!)
  (byte a sdl-color-a sdl-color-a!))

(define (make-sdl-color r g b #!optional (a 255))
  (let ((p (sdl-color)))
    (sdl-color-r! p r)
    (sdl-color-g! p g)
    (sdl-color-b! p b)
    (sdl-color-a! p a)
    p))

(define-c-struct (sdl-display-mode "SDL_DisplayMode")
  (unsigned-int format sdl-display-mode-format)
  (int w sdl-display-mode-w)
  (int h sdl-display-mode-h)
  (int refresh_rate sdl-display-mode-refresh-rate)
  (c-pointer driverdata sdl-display-mode-driverdata))

(define (sdl-create-window title x y w h #!optional (flags 0))
  (let ((window ((foreign-lambda SDL-Window "SDL_CreateWindow" c-string int int int int int)
                 title x y w h flags)))
    (if window
      (pointer->c-struct window sdl-window)
      (error "SDL_CreateWindow" (sdl-get-error)))))

(define (sdl-create-window-and-renderer width height #!optional (window-flags 0))
  (let-location ((window SDL-Window)
                 (renderer SDL-Renderer))
    (let ((res ((foreign-lambda int "SDL_CreateWindowAndRenderer"
                   int int unsigned-integer32 (c-pointer SDL-Window) (c-pointer SDL-Renderer))
                 width height window-flags (location window) (location renderer))))
      (unless (zero? res)
        (error "SDL_CreateWindowAndRenderer" (sdl-get-error)))
      (values (pointer->c-struct window sdl-window)
              (pointer->c-struct renderer sdl-renderer)))))

(define (sdl-create-window-from ptr)
  (let ((window ((foreign-lambda (c-pointer "SDL_Window") "SDL_CreateWindowFrom" c-pointer) ptr)))
    (if window
      (pointer->c-struct window sdl-window)
      (error "SDL_CreateWindowFrom" (sdl-get-error)))))

(define (sdl-destroy-window window)
  ((foreign-lambda void "SDL_DestroyWindow" SDL-Window) (c-struct->pointer window sdl-window)))

(define (sdl-create-renderer window #!optional (index -1) (flags 0))
  (let ((render ((foreign-lambda SDL-Renderer "SDL_CreateRenderer" SDL-Window int int)
                 (c-struct->pointer window sdl-window) index flags)))
    (if render
      (pointer->c-struct render sdl-renderer)
      (error "SDL_CreateRenderer" (sdl-get-error)))))

(define (sdl-set-render-draw-color render r g b a)
  ((foreign-lambda void "SDL_SetRenderDrawColor" SDL-Renderer int int int int)
   (c-struct->pointer render sdl-renderer) r g b a))

(define (sdl-render-clear render)
  ((foreign-lambda int "SDL_RenderClear" SDL-Renderer)
   (c-struct->pointer render sdl-renderer)))

(define (sdl-render-present render)
  ((foreign-lambda void "SDL_RenderPresent" SDL-Renderer)
   (c-struct->pointer render sdl-renderer)))

(define (sdl-render-draw-line render x1 y1 x2 y2)
  ((foreign-lambda int "SDL_RenderDrawLine" SDL-Renderer int int int int)
   (c-struct->pointer render sdl-renderer) x1 y1 x2 y2))

(define (sdl-point->array args)
  (let* ((count (length args))
         (points (make-blob (* count sizeof-sdl-point))))
    (fold
      (lambda (p offset)
        (move-memory! (c-struct->pointer p sdl-point) points sizeof-sdl-point 0 offset)
        (+ offset sizeof-sdl-point))
      0
      args)
    (values count points)))

(define (sdl-render-draw-lines render . lines)
  (receive (count points)
           (sdl-point->array lines)
    ((foreign-lambda int "SDL_RenderDrawLines" SDL-Renderer SDL-Point int)
     (c-struct->pointer render sdl-renderer) (blob->pointer points) count)))

(define (sdl-render-draw-point render x y)
  ((foreign-lambda int "SDL_RenderDrawPoint" SDL-Renderer int int)
   (c-struct->pointer render sdl-renderer) x y))

(define (sdl-render-draw-points render . points)
  (receive (count points)
         (sdl-point->array points)
    ((foreign-lambda int "SDL_RenderDrawPoints" SDL-Renderer SDL-Point int)
     (c-struct->pointer render sdl-renderer) (blob->pointer points) count)))

(define (sdl-render-draw-rect render rect)
  ((foreign-lambda int "SDL_RenderDrawRect" SDL-Renderer SDL-Rect)
   (c-struct->pointer render sdl-renderer) (c-struct->pointer rect sdl-rect)))

(define (sdl-render-fill-rect render rect)
  ((foreign-lambda int "SDL_RenderFillRect" SDL-Renderer SDL-Rect)
   (c-struct->pointer render sdl-renderer) (c-struct->pointer rect sdl-rect)))

(define (sdl-create-texture-from-surface render surface)
  (let ((texture ((foreign-lambda SDL-Texture "SDL_CreateTextureFromSurface"
                                  SDL-Renderer SDL-Surface)
                  (c-struct->pointer render sdl-renderer)
                  (c-struct->pointer surface sdl-surface))))
    (if texture
      (pointer->c-struct texture sdl-texture)
      (error "SDL_CreateTextureFromSurface" (sdl-get-error)))))

(define (sdl-render-copy render texture #!optional src-rect dst-rect)
  (let ((err ((foreign-lambda int "SDL_RenderCopy" SDL-Renderer SDL-Texture SDL-Rect SDL-Rect)
              (c-struct->pointer render sdl-renderer)
              (c-struct->pointer texture sdl-texture)
              (if src-rect (c-struct->pointer src-rect sdl-rect) #f)
              (if dst-rect (c-struct->pointer dst-rect sdl-rect) #f))))
    (or (zero? err)
        (error "SDL_RenderCopy" (sdl-get-error)))))

(define sdl-disable-screensaver
  (foreign-lambda void "SDL_DisableScreenSaver"))

(define sdl-enable-screensaver
  (foreign-lambda void "SDL_EnableScreenSaver"))

(define (sdl-update-window-surface window)
  (let ((res ((foreign-lambda int "SDL_UpdateWindowSurface" SDL-Window)
              (c-struct->pointer window))))
    (or (zero? res)
        (error "SDL_UpdateWindowSurface" (sdl-get-error) res))))

(define (sdl-video-init driver-name)
  (let ((res ((foreign-lambda int "SDL_VideoInit" c-string) driver-name)))
    (or (zero? res)
        (error "SDL_VideoInit" (sdl-get-error) res))))

(define sdl-video-quit
  (foreign-lambda void "SDL_VideoQuit"))

(define (sdl-gl-get-attribute attr value)
  (let-location ((value int value))
    (let ((err ((foreign-lambda int "SDL_GL_GetAttribute" int (c-pointer int))
                attr (location value))))
      (if (zero? err)
        value
        (error "SDL_GL_GetAttribute" (sdl-get-error))))))

(define (sdl-gl-set-attribute attr value)
  (let ((err ((foreign-lambda int "SDL_GL_SetAttribute" int int) attr value)))
    (or (zero? err)
        (error "SDL_GL_SetAttribute" (sdl-get-error)))))

(define (sdl-gl-create-context window)
  (let ((glcontect ((foreign-lambda SDL-GLContext "SDL_GL_CreateContext" SDL-Window)
                    (c-struct->pointer window sdl-window))))
    (if glcontect
      (pointer->c-struct glcontect sdl-glcontext)
      (error "SDL_GL_CreateContext" (sdl-get-error)))))

(define (sdl-gl-delete-context glcontext)
  ((foreign-lambda void "SDL_GL_DeleteContext" SDL-GLContext)
   (c-struct->pointer glcontext sdl-glcontext)))

(define (sdl-gl-make-current window glcontext)
  (let ((err ((foreign-lambda int "SDL_GL_MakeCurrent" SDL-Window SDL-GLContext)
              (c-struct->pointer window sdl-window)
              (c-struct->pointer glcontext sdl-glcontext))))
    (or (zero? err)
        (error "SDL_GL_MakeCurrent" (sdl-get-error)))))

(define (sdl-gl-set-swap-interval interval)
  (let ((err ((foreign-lambda int "SDL_GL_SetSwapInterval" int) interval)))
    (or (zero? err)
        (error "SDL_GL_SetSwapInterval" (sdl-get-error)))))

(define (sdl-gl-get-swap-interval)
  (let ((interval ((foreign-lambda int "SDL_GL_GetSwapInterval"))))
    (if (negative? interval)
      (error "SDL_GL_GetSwapInterval" (sdl-get-error))
      interval)))

(define (sdl-gl-swap-window window)
  ((foreign-lambda void "SDL_GL_SwapWindow" SDL-Window)
   (c-struct->pointer window sdl-window)))

(define (sdl-gl-get-current-context)
  (let ((glcontext ((foreign-lambda SDL-GLContext "SDL_GL_GetCurrentContext"))))
    (if glcontext
      (pointer->c-struct glcontext sdl-glcontext)
      (error "SDL_GL_GetCurrentContext" (sdl-get-error)))))

(define (sdl-gl-get-current-window)
  (let ((glwindow ((foreign-lambda SDL-Window "SDL_GL_GetCurrentWindow"))))
    (if glwindow
      (pointer->c-struct glwindow sdl-window)
      (error "SDL_GL_GetCurrentWindow" (sdl-get-error)))))

(define (sdl-gl-bind-texture texture texw texh)
  (let-location ((w float (or texw 0.))
                 (h float (or texh 0.)))
    (let ((err ((foreign-lambda int "SDL_GL_BindTexture" SDL-Texture (c-pointer float) (c-pointer float))
                (c-struct->pointer texture sdl-texture)
                (and texw (location w))
                (and texh (location h)))))
      (or (zero? err)
          (error "SDL_GL_BindTexture" (sdl-get-error))))))

(define (sdl-gl-unbind-texture texture)
  (let ((err ((foreign-lambda int "SDL_GL_UnbindTexture" SDL-Texture)
              (c-struct->pointer texture sdl-texture))))
    (or (zero? err)
        (error "SDL_GL_UnbindTexture" (sdl-get-error)))))

(define (sdl-load-bmp file)
  (let ((surface ((foreign-lambda SDL-Surface "SDL_LoadBMP" c-string) file)))
    (if surface
      (pointer->c-struct surface sdl-surface)
      (error "SDL_LoadBMP" (sdl-get-error)))))

(define (sdl-free-surface surface)
  ((foreign-lambda void "SDL_FreeSurface" SDL-Surface)
   (c-struct->pointer surface sdl-surface))
  (c-struct-pointer-set! surface #f sdl-surface))

(define (sdl-destroy-texture texture)
  ((foreign-lambda void "SDL_DestroyTexture" SDL-Texture)
   (c-struct->pointer texture sdl-texture))
  (c-struct-pointer-set! texture #f sdl-texture))

(define (sdl-create-texture renderer format access w h)
  (let ((texture ((foreign-lambda SDL-Texture "SDL_CreateTexture" SDL-Renderer int int int int)
                  (c-struct->pointer renderer sdl-renderer) format access w h)))
    (if texture
      (pointer->c-struct texture sdl-texture)
      (error "SDL_CreateTexture" (sdl-get-error)))))

)
