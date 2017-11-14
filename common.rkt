;; Copyright © 2017 Eric Griffis <dedbox@gmail.com>
;;
;; This software may be modified and distributed under the terms of the MIT
;; license. See the LICENSE file for details.

#lang racket/base

(provide (all-defined-out))

(require racket/format
         racket/logging
         racket/string)

(define beacon-port 3699)

(define-syntax-rule (define-log-level NAME log-level)
  (define (NAME addr . msgs)
    (define msecs (~r (/ (current-inexact-milliseconds) 1000) #:precision '(= 6)))
    (define addr-str (if (eq? addr -) "-" (apply ~a addr #:separator ":")))
    (define msg-str (string-join (map ~s msgs)))
    (log-level "~a ~a ~a" msecs addr-str msg-str)))

(define-log-level DEBUG log-debug)
(define-log-level INFO log-info)
(define-log-level WARN log-warning)
(define-log-level ERROR log-error)
(define-log-level FATAL log-fatal)

(define (log-to-stdout proc [level 'info])
  (with-logging-to-port (current-output-port) proc level))
