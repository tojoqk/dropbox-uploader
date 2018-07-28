#lang web-server/insta

(define (start req)
  (response/xexpr
   `(html (head (title "Encrypt Dropbox"))
          (body (h1 "Encrypt Dropbox")))))
