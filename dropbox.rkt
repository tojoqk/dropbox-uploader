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

(define (call-with-response api jsexpr proc)
  (define-values (status headers containts) (api jsexpr))
  (begin0 (proc status headers containts)
    (close-input-port containts)))

(define (call-with-response/data api jsexpr data proc)
  (define-values (status headers containts) (api jsexpr data))
  (begin0 (proc status headers containts)
    (close-input-port containts)))

(define (ok? status)
  (regexp-match? #rx" 200 " (bytes->string/utf-8 status)))

(define api.dropboxapi.com "api.dropboxapi.com")

(define (/2/files/get_metadata json)
  (json-api api.dropboxapi.com "/2/files/get_metadata" json))

(define (get-metadata path)
  (let-values ([(status headers containts) (/2/files/get_metadata (hash 'path path))])
    (and (ok? status) (read-json containts))))
(provide/contract [get-metadata (-> string? (or/c jsexpr? false/c))])

(define (/2/files/list_folder json)
  (json-api api.dropboxapi.com "/2/files/list_folder" json))

(define (/2/files/list_folder/continue json)
  (json-api api.dropboxapi.com "/2/files/list_folder/continue" json))

(define (dropbox-list-folder path)
  (let ([path (if (string=? path "/") "" path)])
    (call-with-response  /2/files/list_folder (hash 'path path)
      (λ (status headers containts)
        (match (and (ok? status) (read-json containts))
          [(hash-table
            ('has_more has-more?)
            ('cursor cursor)
            ('entries entries))
           (if has-more?
               (let continue ([cursor cursor]
                              [entries (reverse entries)])
                 (sleep 0.1)
                 (let-values ([(status headers containts) (/2/files/list_folder/continue
                                                           (hash 'cursor cursor))])
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
               entries)]
          [else #f])))))
(provide/contract [dropbox-list-folder (-> string? (or/c (listof jsexpr?) false/c))])

(define (/2/files/delete_v2 jsexpr)
  (json-api api.dropboxapi.com "/2/files/delete_v2" jsexpr))

(define (dropbox-delete path)
  (call-with-response /2/files/delete_v2 (hash 'path path)
    (λ (status headers containts)
      (and (ok? status) (read-json containts)))))
(provide/contract [dropbox-delete (-> string? (or/c jsexpr? false/c))])

(define (/2/files/create_folder_v2 jsexpr)
  (json-api api.dropboxapi.com "/2/files/create_folder_v2" jsexpr))

(define (dropbox-create-folder path)
  (call-with-response  /2/files/create_folder_v2 (hash 'path path)
    (λ (status headers containts)
      (and (ok? status) (read-json containts)))))
(provide/contract [dropbox-create-folder (-> string? (or/c jsexpr? false/c))])

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

(define (dropbox-download-file path filename #:chunk-size [chunk-size (* 4 1024 1024)])
  (define jsexpr
    (call-with-output-file filename
      (λ (out)
        (call-with-input-from-dropbox
         path
         (λ (in)
           (let loop ()
             (define bs (read-bytes chunk-size in))
             (cond
               [(eof-object? bs) (void)]
               [else
                (write-bytes bs out)
                (loop)])))))
      #:exists 'replace))
  (if (string=? (content-hash filename) (hash-ref jsexpr 'content_hash))
      jsexpr
      (error 'dropbox-download-file "mismatch content-hash" filename)))
(provide/contract [dropbox-download-file (->* (string? string?)
                                              (#:chunk-size exact-positive-integer?)
                                              jsexpr?)])

(define (call-with-input-from-dropbox path proc)
  (call-with-response /2/files/download (hasheq 'path path)
    (λ (status headers contents)
      (cond
        [(ok? status)
         (define jsexpr (headers->dropbox-api-result headers))
         (proc contents)
         jsexpr]
        [(headers->dropbox-api-result headers) => (λ (x) (error 'download x))]
        [else (error 'dropbox-download (port->string contents))]))))
(provide/contract
 [call-with-input-from-dropbox
     (-> string? (-> input-port? any/c) jsexpr?)])

(define (/2/files/upload_session/start json data)
  (data-api content.dropboxapi.com "/2/files/upload_session/start" json #:data data))

(define (/2/files/upload_session/append json data)
  (data-api content.dropboxapi.com "/2/files/upload_session/append_v2" json #:data data))

(define (/2/files/upload_session/finish json)
  (data-api content.dropboxapi.com "/2/files/upload_session/finish" json
            #:headers (list "Content-Type: application/octet-stream")))

(define (dropbox-upload-file path filename #:chunk-size [chunk-size (* 4 1024 1024)])
  (define jsexpr
    (call-with-input-file filename
      (λ (in)
        (call-with-output-to-dropbox
         path
         (λ (out)
           (let loop ()
             (define bs (read-bytes chunk-size in))
             (cond
               [(eof-object? bs) (void)]
               [else
                (write-bytes bs out)
                (loop)])))))))
  (if (string=? (content-hash filename) (hash-ref jsexpr 'content_hash))
      jsexpr
      (error 'dropbox-upload-file "mismatch content-hash" filename)))
(provide/contract [dropbox-upload-file (->* (string? string?)
                                            (#:chunk-size exact-positive-integer?)
                                            jsexpr?)])

(define (call-with-output-to-dropbox path proc)
  (call-with-response/data /2/files/upload_session/start (hasheq) #""
    (λ (status headers content)
      (define result #f)
      (cond
        [(ok? status)
         (define jsexpr (read-json content))
         (define session-id (hash-ref jsexpr 'session_id))
         (define offset 0)
         (define out
           (make-output-port
            'dropbox-uploader
            always-evt
            (lambda (chunk start end non-block? brackable?)
              (call-with-response/data
                  /2/files/upload_session/append
                  (hasheq 'cursor
                          (hasheq 'session_id session-id
                                  'offset offset))
                  (subbytes chunk 0 (- end start))
                (λ (status headers content)
                  (set! offset (+ offset (- end start)))
                  (cond
                    [(ok? status)
                     (- end start)]
                    [else
                     (error 'dropbox-upload (port->string content))]))))
            (thunk
             (call-with-response
                 /2/files/upload_session/finish
                 (hasheq 'cursor
                         (hasheq 'session_id session-id
                                 'offset offset)
                         'commit
                         (hasheq 'path path
                                 'mode "overwrite"))
               (λ (status headers content)
                 (cond
                   [(ok? status)
                    (set! result (read-json content))]
                   [else
                    (error 'upload (port->string content))]))))))
         (proc out)
         (close-output-port out)
         result]
        [else
         (error 'dropbox-upload (port->string content))]))))
(provide/contract
 [call-with-output-to-dropbox
  (-> string? (-> output-port? any/c) jsexpr?)])
