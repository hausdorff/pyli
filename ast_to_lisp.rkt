#lang racket

;(require racket/pretty)

(define (emit)
  (let ([in (read)])
    (display (pretty-format in 20))))
(emit)
