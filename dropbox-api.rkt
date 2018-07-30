#lang racket
(require (for-syntax racket/base racket/list syntax/parse syntax/flatten-begin))
 (require net/http-client argo json)
(define (access-token) (getenv "DROPBOX_ACCESS_TOKEN"))

;;; JSON Schema
(define string/t "string")
(define number/t "number")
(define boolean/t "boolean")
(define integer/t "integer")
(define-syntax-rule (object/t (property ...) (req ...))
  (hash 'type "object"
        'properties (let ((h (make-hash)))
                      (hash-set! h (car property) (cdr property)) ...
                      h)
        'required (map (compose symbol->string car) (list req ...))))

(define (make-property name body)
  (cons name body))
(define-syntax define-property
  (syntax-rules ()
    [(_ name property #:type t)
     (define name (make-property 'property (hash 'type t)))]
    [(_ name property schema)
     (define name (make-property 'property schema))]))

(define-property url/p url #:type string/t)
(define-property password/p password #:type string/t)
(define-property path/p path #:type string/t)
(define-property recursive/p recursive #:type boolean/t)
(define-property include_media_info/p include_media_info #:type boolean/t)
(define-property include_deleted/p include_deleted #:type boolean/t)
(define-property include_has_explicit_shared_members/p include_has_explicit_shared_members #:type boolean/t)
(define-property include_mounted_folders/p include_has_explicit_shared_members #:type boolean/t)
(define-property limit/p limit #:type integer/t)
(define-property shared_link/p shared_link (object/t (url/p password/p) (url/p password/p)))
;; TODO Define include_property_groups/p

(define list_folder/o
  (object/t (path/p recursive/p include_media_info/p include_deleted/p
                    include_has_explicit_shared_members/p include_mounted_folders/p
                    limit/p shared_link/p)
            (path/p)))

(define (api host uri schema json #:headers [headers '()] #:data [data #f])
  (check-json/schema json schema)
  (http-sendrecv host uri
                 #:ssl? #t
                 #:headers `(,(format "Authorization: Bearer ~a" (access-token))
                             ,(if data
                                  (format "Dropbox-API-Arg: ~a" (jsexpr->string json))
                                  "Content-Type: application/json"))
                 #:method 'POST
                 #:data (or data
                            (jsexpr->string json))))

(struct empty-field ())
(define-syntax (define-json-api stx)
  (syntax-parse stx
    [(_ name:id host:string uri:string (param:keyword ...) (optional-param:keyword ...) (header ...))
     (with-syntax ([(param-id ...) (generate-temporaries #'(param ...))]
                   [(optional-param-id ...)
                    (generate-temporaries #'(optional-param ...))])
       #`(begin
           (define default (empty-field))
           (define #,(flatten-all-begins
                      #'(begin name
                               (begin param param-id) ...
                               (begin optional-param [optional-param-id default]) ...))
             (http-sendrecv
              host uri
              #:ssl? #t
              #:headers `(,(format "Authorization: Bearer ~a" (access-token))
                          "Content-Type: application/json"
                          header ...)
              #:method 'POST
              #:data (jsexpr->string
                      (let ([h (make-hash)])
                        (unless (empty-field? param-id)
                          (hash-set! h (string->symbol (keyword->string 'param)) param-id)) ...
                        (unless (empty-field? optional-param-id)
                          (hash-set! h (string->symbol (keyword->string 'optional-param)) optional-param-id)) ...
                        h))))))]))

(define-syntax (define-content-api stx)
  (syntax-parse stx
    [(k name:id host:string uri:string (param:keyword ...) (optional-param:keyword ...) (header ...))
     (with-syntax ([(param-id ...) (generate-temporaries #'(param ...))]
                   [(optional-param-id ...)
                    (generate-temporaries #'(optional-param ...))])
       #`(begin
           (define default (empty-field))
           (define #,(flatten-all-begins
                      #'(begin name
                               (begin param param-id) ...
                               (begin optional-param [optional-param-id default]) ...))
             (http-sendrecv
              host uri
              #:ssl? #t
              #:headers `(,(format "Authorization: Bearer ~a" (access-token))
                          ,(format
                            "Dropbox-API-Arg: ~a"
                            (jsexpr->string
                             (let ([h (make-hash)])
                               (unless (or (eq? '#:data 'param) (empty-field? param-id))
                                 (hash-set! h (string->symbol
                                               (keyword->string 'param)) param-id)) ...
                               (unless (empty-field? optional-param-id)
                                 (hash-set! h (string->symbol
                                               (keyword->string 'optional-param)) optional-param-id)) ...
                               h)))
                          header ...)
              #:method 'POST
              #,@(cond
                   [(index-of (syntax->datum #'(param ...)) '#:data)
                    => (lambda (idx)
                         (list '#:data
                               #`(list-ref (list param-id ...) #,idx)))]
                   [else '()])))))]))

(define-json-api files/list_folder "api.dropboxapi.com" "/2/files/list_folder"
  (#:path)
  (#:recursive
   #:include_media_info
   #:include_deleted
   #:include_has_explicit_shared_members
   #:include_mounted_folders
   #:limit
   #:shared_link
   #:include_property_groups)
  ())
(provide files/list_folder)

(define-json-api files/move_v2 "api.dropboxapi.com" "/2/files/move_v2"
  (#:from_path #:to_path)
  (#:allow_shared_folder
   #:autorename
   #:allow_ownership_transfer)
  ())

(define-content-api files/upload "content.dropboxapi.com" "/2/files/upload"
  (#:path #:data)
  (#:mode #:autorename #:client_modified)
  ("Content-Type: application/octet-stream"))

(define-content-api files/download "content.dropboxapi.com" "/2/files/download"
  (#:path) () ())
