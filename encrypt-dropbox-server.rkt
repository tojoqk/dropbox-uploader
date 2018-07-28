#lang web-server/insta
(require simple-http crypto dotenv)

(define (start req)
  (response/xexpr
   `(html (head (title "Encrypt Dropbox"))
          (body (h1 "Encrypt Dropbox")))))
