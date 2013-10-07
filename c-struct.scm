(module c-struct
 (define-c-struct
  c-struct->pointer
  pointer->c-struct
  c-struct?
  blob->pointer
  c-struct-pointer-set!)
 (import scheme chicken foreign)
 (use lolevel)

(define-syntax c-struct?
  (syntax-rules ()
    ((c-struct? c type)
     (##sys#structure? c 'type))))

(define-syntax define-c-struct
  (syntax-rules ()
    ((_ (name type) rest ...)
     (define-c-struct-rest (name type name) rest ...))
    ((_ rest ...)
     (syntax-error "invalid c-struct definition" `(rest ...)))))

(define-syntax define-c-struct-rest
  (syntax-rules (constructor:)
    ((_ (name type init))
     (begin
       (define (init)
         (let ((data (make-blob (foreign-type-size type))))
           (##sys#make-structure 'name data)))
       (define-c-struct-sizeof name type)
       (define-c-struct-type? name)))
    ((_ (name type init) (constructor: constructor) rest ...)
     (define-c-struct-rest (name type constructor) rest ...))
    ((_ (name type init) (field-type field-name getter) rest ...)
     (begin
       (define-c-field getter 0 type name field-type field-name)
       (define-c-struct-rest (name type init) rest ...)))
    ((_ (name type init) (field-type field-name getter setter) rest ...)
     (begin
       (define-c-field getter 0 type name field-type field-name)
       (define-c-field setter 1 type name field-type field-name)
       (define-c-struct-rest (name type init) rest ...)))
    ((_ hdr arg rest ...)
     (syntax-error "invalid c-struct field definition" arg))))

(define blob->pointer
  (foreign-primitive c-pointer ((blob b)) "C_return(b);"))

(define-syntax c-struct-pointer-set!
  (syntax-rules ()
    ((c-struct-pointer-set! c ptr type)
     (let ((s c))
       (##sys#check-structure s 'type)
       (##sys#setslot s 1 ptr)))))

(define-syntax c-struct->pointer
  (syntax-rules ()
    ((c-struct->pointer c)
     (let ((p (##sys#slot c 1)))
       (if (blob? p)
         (blob->pointer p)
         p)))
    ((c-struct->pointer c type)
     (let ((s c))
       (##sys#check-structure s 'type)
       (c-struct->pointer s)))))

(define-syntax pointer->c-struct
  (syntax-rules ()
    ((pointer->c-struct ptr type)
     (begin
       (assert (pointer? ptr))
       (##sys#make-structure 'type ptr)))))

;; 0 - get, 1 - set
(define-syntax define-c-field
  (syntax-rules ()
    ((_ getter 0 type name field-type field-name)
     (define (getter c)
       (##sys#check-structure c 'name)
       ((c-struct-ffi 0 type field-type field-name) (##sys#slot c 1))))
    ((_ setter 1 type name field-type field-name)
     (define (setter c value)
       (##sys#check-structure c 'name)
       ((c-struct-ffi 1 type field-type field-name) (##sys#slot c 1) value)))))

(define-syntax c-struct-ffi
  (er-macro-transformer
    (lambda (e r c)
      ;; e (_ op struct-type field-type field-name)
      (let ((op (list-ref e 1))
            (struct-type (list-ref e 2))
            (field-type (list-ref e 3))
            (field-name (list-ref e 4)))
        (define (struct-type? type)
          (and (list? type) (eq? (car type) 'struct)))
        (define (gen-type type)
          (if (struct-type? type) `(c-pointer ,struct-type) type))
        (define (gen-value-ref name type)
          `(foreign-lambda* ,(gen-type type) ((scheme-pointer ptr))
             ,(if (struct-type? type)
                (sprintf "C_return( &((~A *)ptr) -> ~A);" struct-type name)
                (sprintf "C_return(  ((~A *)ptr) -> ~A);" struct-type name))))
        (define (gen-value-set! name type)
          `(foreign-lambda* void ((scheme-pointer ptr) (,(gen-type type) value))
             ,(if (struct-type? type)
                (sprintf " ((~A *)ptr) -> ~A = *value;" struct-type name)
                (sprintf " ((~A *)ptr) -> ~A =  value;" struct-type name))))
        (if (zero? op)
          (gen-value-ref field-name field-type)
          (gen-value-set! field-name field-type))))))


(define-syntax define-c-struct-sizeof
  (er-macro-transformer
    (lambda (e r c)
      (let ((name (list-ref e 1))
            (type (list-ref e 2)))
        `(define ,(string->symbol (sprintf "sizeof-~A" name))
           (foreign-type-size ,type))))))

(define-syntax define-c-struct-type?
  (er-macro-transformer
    (lambda (e r c)
      (let ((name (list-ref e 1)))
        `(define (,(string->symbol (sprintf "~A?" name)) x)
           (##sys#check-structure x ',name))))))

)
