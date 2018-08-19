#lang racket
(require (for-syntax racket/base racket/list syntax/parse syntax/flatten-begin))
(require racket/contract net/http-client json)
(require (only-in srfi/1 append-reverse))
(define (access-token) (getenv "DROPBOX_ACCESS_TOKEN"))

(define/contract (json-api host uri json #:headers [headers '()])
  (->* (string? string? jsexpr?) (#:headers (listof string?))
       (values any/c any/c any/c))
  (http-sendrecv host uri
                 #:ssl? #t
                 #:headers `(,(format "Authorization: Bearer ~a" (access-token))
                             "Content-Type: application/json")
                 #:method 'POST
                 #:data (jsexpr->string json)))

(define/contract (data-api host uri json #:headers [headers '()] #:data [data #f])
  (->* (string? string? jsexpr?) (#:headers (listof string?) #:data any/c)
       (values any/c any/c any/c))
  (http-sendrecv host uri
                 #:ssl? #t
                 #:headers `(,(format "Authorization: Bearer ~a" (access-token))
                             ,(format "Dropbox-API-Arg: ~a" (jsexpr->string json)))
                 #:method 'POST
                 #:data data))

(define (ok? status)
  (regexp-match? #rx" 200 " (bytes->string/utf-8 status)))

(define api.dropboxapi.com "api.dropboxapi.com")
(define (list_folder json)
  (json-api api.dropboxapi.com "/2/files/list_folder" json))

(define (list_folder/continue json)
  (json-api api.dropboxapi.com "/2/files/list_folder/continue" json))

(define (list-folder dirs)
  (define (continue cursor names)
    (sleep 0.1)
    (let-values ([(status headers containts) (list_folder/continue (hash 'cursor cursor))])
      (match (and (ok? status) (read-json containts))
        [(hash-table
          ('has_more has-more?)
          ('cursor cursor)
          ('entries (list (hash-table ('name new-names)) ...)))
         (let ([names (append-reverse new-names names)])
           (if has-more?
               (continue cursor names)
               (reverse names)))]
        [else #f])))
  (define path (if (null? dirs) "" (string-append "/" (string-join dirs "/"))))
  (let-values ([(status headers containts) (list_folder (hash 'path path))])
    (match (and (ok? status) (read-json containts))
      [(hash-table
        ('has_more has-more?)
        ('cursor cursor)
        ('entries (list (hash-table ('name names)) ...)))
       (if has-more?
           (continue cursor (reverse names))
           names)]
      [else #f])))
