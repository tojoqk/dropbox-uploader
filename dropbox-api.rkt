#lang racket
(require (for-syntax racket/base racket/list syntax/parse syntax/flatten-begin))
(require racket/contract net/http-client json sha)
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
(define (/2/files/list_folder json)
  (json-api api.dropboxapi.com "/2/files/list_folder" json))

(define (/2/files/list_folder/continue json)
  (json-api api.dropboxapi.com "/2/files/list_folder/continue" json))

(define (list-folder dirs)
  (define (continue cursor entries)
    (sleep 0.1)
    (let-values ([(status headers containts) (/2/files/list_folder/continue (hash 'cursor cursor))])
      (match (and (ok? status) (read-json containts))
        [(hash-table
          ('has_more has-more?)
          ('cursor cursor)
          ('entries new-entries))
         (let ([entries (append-reverse new-entries entries)])
           (if has-more?
               (continue cursor entries)
               (reverse entries)))]
        [else #f])))
  (define path (if (null? dirs) "" (string-append "/" (string-join dirs "/"))))
  (let-values ([(status headers containts) (/2/files/list_folder (hash 'path path))])
    (match (and (ok? status) (read-json containts))
      [(hash-table
        ('has_more has-more?)
        ('cursor cursor)
        ('entries entries))
       (if has-more?
           (continue cursor (reverse entries))
           entries)]
      [else #f])))

(define content.dropboxapi.com "content.dropboxapi.com")

(define (headers->dropbox-api-result headers)
  (for/first ([header (map bytes->string/utf-8 headers)]
              #:when (regexp-match? #rx"^dropbox-api-result:" header))
    (string->jsexpr (substring header 20))))

(define (/2/files/download json)
  (data-api content.dropboxapi.com "/2/files/download" json))

(define block-size (* 4 1024 1024))
(define (content-hash f)
  (call-with-input-file f
    (λ (p)
      (define (read-block) (read-bytes block-size p))
      (let ([sp (open-output-bytes)])
        (let loop ([block (read-block)])
          (cond
            [(eof-object? block)
             (string-append*
              (map (curryr ~r #:base 16 #:min-width 2 #:pad-string "0")
                   (bytes->list (sha256 (get-output-bytes sp)))))]
            [else
             (write-bytes (sha256 block) sp)
             (loop (read-block))]))))))

