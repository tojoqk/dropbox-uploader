#lang racket
(require (for-syntax racket/base racket/list syntax/parse syntax/flatten-begin))
(require racket/contract net/http-client argo json)
(define (access-token) (getenv "DROPBOX_ACCESS_TOKEN"))

(define-struct/contract property ([name symbol?] [body jsexpr?]))
(define/contract (list->hash xs)
  (-> (listof pair?) hash?)
  (apply hash (append-map (λ (x) (list (car x) (cdr x))) xs)))

(define/contract (properties->hash xs)
  (-> (listof property?) hash?)
  (list->hash (map (λ (x) (cons (property-name x) (property-body x))) xs)))

;;; JSON Schema
(define string/t "string")
(define number/t "number")
(define boolean/t "boolean")
(define integer/t "integer")
(define/contract (object/t properties reqs)
  (-> (listof property?) (listof property?) hash?)
  (list->hash
   `((type . "object")
     (properties . ,(properties->hash properties))
     (required . ,(map (compose symbol->string property-name) reqs)))))

(define-syntax-rule (define-property-type name p t)
  (define name (property 'p (hash 'type t))))
(define-syntax-rule (define-property-object name p obj)
  (define name (property 'p obj)))

(define-property-type url/p url string/t)
(define-property-type password/p password string/t)
(define-property-type path/p path string/t)
(define-property-type recursive/p recursive boolean/t)
(define-property-type include_media_info/p include_media_info boolean/t)
(define-property-type include_deleted/p include_deleted boolean/t)
(define-property-type include_has_explicit_shared_members/p include_has_explicit_shared_members boolean/t)
(define-property-type include_mounted_folders/p include_has_explicit_shared_members boolean/t)
(define-property-type limit/p limit integer/t)
(define-property-object shared_link/p shared_link (object/t (list url/p password/p) (list url/p password/p)))
;; TODO Define include_property_groups/p

(define list_folder/o
  (object/t (list path/p recursive/p include_media_info/p include_deleted/p
                  include_has_explicit_shared_members/p include_mounted_folders/p
                  limit/p shared_link/p)
            (list path/p)))

(define/contract (api host uri schema json #:headers [headers '()] #:data [data #f])
  (->* (string? string? json-schema? jsexpr?) (#:headers (listof string?) #:data any/c)
       (values any/c any/c any/c))
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

(define-syntax-rule (define-api name host uri schema [header ...])
  (define (name json)
    (api host uri schema json #:headers (list header ...))))

(define-syntax-rule (define-data-api name host uri schema [header ...])
  (define (name json data)
    (api host uri schema json #:headers (list header ...) #:data data)))

(define api.dropboxapi.com "api.dropboxapi.com")
(define-api list_folder api.dropboxapi.com "/2/files/list_folder" list_folder/o ())
