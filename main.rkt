#lang racket
(require "dropbox.rkt")
(provide (rename-out (prefix-out dropbox- (all-from-out "dropbox.rkt"))
                     [dropbox-call-with-input-from-dropbox call-with-input-from-dropbox]
                     [dropbox-call-with-output-to-dropbox call-with-output-to-dropbox]))
