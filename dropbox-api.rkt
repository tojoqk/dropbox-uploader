#lang racket
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
                             ,(format "Dropbox-API-Arg: ~a" (jsexpr->string json))
                             ,@(if data (list "Content-Type: application/octet-stream") '())
                             ,@headers)
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

(define (content-hash f)
  (define block-size (* 4 1024 1024))
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

(define (download-file path filename #:chunk-size [chunk-size (* 4 1024 1024)])
  (call-with-output-file filename
    (λ (p) (download path (λ (chunk) (write-bytes chunk p))))
    #:exists 'replace)
  (if (string=? (content-hash filename) (hash-ref jsexpr 'content_hash))
      filename
      (error 'download-file "mismatch content-hash" filename)))
(define (download path write-chunk #:chunk-size [chunk-size (* 4 1024 1024)])
  (define-values (status headers contents) (/2/files/download (hasheq 'path path)))
  (cond
    [(ok? status)
     (define jsexpr (headers->dropbox-api-result headers))
     (define (read-chunk) (read-bytes chunk-size contents))
     (let loop ([chunk (read-chunk)])
       (cond
         [(eof-object? chunk) 'done]
         [else
          (write-chunk chunk)
          (loop (read-chunk))]))
     jsexpr]
    [(headers->dropbox-api-result headers) => (λ (x) (error 'download x))]
    [else (error 'download (port->string contents))]))

(define (/2/files/upload_session/start json data)
  (data-api content.dropboxapi.com "/2/files/upload_session/start" json #:data data))

(define (/2/files/upload_session/append json data)
  (data-api content.dropboxapi.com "/2/files/upload_session/append_v2" json #:data data))

(define (/2/files/upload_session/finish json)
  (data-api content.dropboxapi.com "/2/files/upload_session/finish" json
            #:headers (list "Content-Type: application/octet-stream")))

(define (upload-file path f)
  (let ([ch (content-hash f)])
    (call-with-input-file f
      (λ (ip)
        (define chunk-size (* 4 1024 1024))
        (define (read-chunk) (read-bytes (current-chunk-size) ip))
        (define result (upload path read-chunk))
        (cond
          [(string=? ch (hash-ref result 'content_hash)) result]
          [else
           (error 'upload-file "mismatch content-hash")])))))

(define (upload path read-chunk)
  (define-values (status headers content)
    (/2/files/upload_session/start (hasheq) (read-chunk)))
  (cond
    [(ok? status)
     (define result (read-json content))
     (define session-id (hash-ref result 'session_id))
     (let loop ([chunk (read-chunk)]
                [offset (bytes-length chunk)])
       (cond
         [(eof-object? chunk)
          (define-values (status headers content)
            (/2/files/upload_session/finish
             (hasheq 'cursor
                     (hasheq 'session_id session-id
                             'offset offset)
                     'commit
                     (hasheq 'path path
                             'mode "overwrite"))))
          (cond
            [(ok? status) (read-json content)]
            [else
             (error 'upload (port->string content))])]
         [else
          (define-values (status headers content)
            (/2/files/upload_session/append
             (hasheq 'cursor
                     (hasheq 'session_id session-id
                             'offset offset))
             chunk))
          (cond
            [(ok? status)
             (loop (read-chunk) (+ offset (bytes-length chunk)))]
            [else
             (error 'upload (port->string content))])]))]
    [else
     (error 'upload (port->string content))]))
