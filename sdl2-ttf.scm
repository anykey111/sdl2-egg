(module sdl2-ttf
  (ttf-init ttf-quit
   ttf-get-error ttf-set-error
   ttf-open-font ttf-open-font-index ttf-close-font
   ttf-render-text-solid)
  (import scheme chicken foreign)
  (use sdl2-basics sdl2-video c-struct lolevel)

(foreign-declare "#include <SDL2/SDL_ttf.h>")

(define-foreign-type TTF-Font (c-pointer "TTF_Font"))
(define-foreign-type SDL-Surface (c-pointer "SDL_Surface"))

(define-c-struct (ttf-font "TTF_Font*"))

(define (ttf-init)
  (or (zero? ((foreign-lambda int "TTF_Init")))
      (error "TTF_Init")))

(define ttf-quit
  (foreign-lambda void "TTF_Quit"))

(define ttf-get-error sdl-get-error)
(define ttf-set-error sdl-set-error)

(define (ttf-open-font file ptsize)
  (let ((font ((foreign-lambda TTF-Font "TTF_OpenFont" c-string int) file ptsize)))
    (if font
      (pointer->c-struct font ttf-font)
      (error "TTF_OpenFont" (ttf-get-error)))))

(define (ttf-open-font-index file ptsize index)
  (let ((font ((foreign-lambda TTF-Font "TTF_OpenFontIndex" c-string int int) file ptsize index)))
    (if font
      (pointer->c-struct font ttf-font)
      (error "TTF_OpenFontIndex" (ttf-get-error)))))

(define (ttf-close-font font)
  ((foreign-lambda void "TTF_CloseFont" TTF-Font) (c-struct->pointer font sdl-font))
  (c-struct-pointer-set! font (address->pointer 0) ttf-font))

(define (ttf-render-text-solid font text fg)
  (let ((surface ((foreign-lambda* SDL-Surface ((TTF-Font font) (c-string text) (c-pointer color))
                    "C_return(TTF_RenderText_Solid(font,text,*(SDL_Color*)color));")
                  (c-struct->pointer font ttf-font) text (c-struct->pointer fg sdl-color))))
    (if surface
      (pointer->c-struct surface sdl-surface)
      (error "SDL_RenderText_Solid" (ttf-get-error)))))

)
