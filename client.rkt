;; Copyright © 2017 Eric Griffis <dedbox@gmail.com>
;;
;; This software may be modified and distributed under the terms of the MIT
;; license. See the LICENSE file for details.

#lang racket/base

(provide (all-defined-out))

(require axon
         "common.rkt"
         racket/dict)

(define beacon-time-secs 1)

;;; Discovery

(define (discover-servers)
  (define servers null)
  (define beacon-addr `("255.255.255.255" ,beacon-port))
  (define σ (udp-peer sexp-printer sexp-parser beacon-addr))
  (give σ (list beacon-addr 'PING))
  (INFO - 'PING)
  (sync/timeout beacon-time-secs
                (loop-evt (λ () (recv-evt σ))
                          (λ (msg)
                            (apply INFO msg)
                            (define addr (car msg))
                            (define info (cadr msg))
                            (define host (car addr))
                            (define name (car info))
                            (define port (cadr info))
                            (set! servers
                              (cons (list name host port) servers)))))
  (stop σ)
  servers)

;;; Control

(define (control-client server-addr)
  (tcp-client sexp-codec-factory server-addr))

(define (world-client server-addr)
  (udp-peer sexp-printer sexp-parser server-addr))

(define world-client-address udp-peer-local-address)

;;; Main

(module+ main
  (require racket/match)

  (log-to-stdout
   (λ ()
     (define servers (discover-servers))

     (displayln "Choose a server:")
     (for ([i (length servers)]
           [server servers])
       (writeln (cons i server)))
     (display "> ")

     (define server-info (list-ref servers (read)))
     (define server-name (car server-info))
     (define server-addr (cdr server-info))

     (define ctrl (control-client server-addr))
     (define world (world-client server-addr))
     (define world-addr (udp-peer-local-address world))
     (define world-port (cadr world-addr))
     (INFO world-addr 'connected 'to server-addr)

     (define my-id (ctrl `(START ,world-port)))
     (INFO world-addr 'agent 'id my-id)

     (define agents null)

     (define (do-idle)
       (displayln (ctrl `(IDLE ,world-port))))

     (define (do-move target speed)
       (displayln (ctrl `(MOVE ,world-port ,target ,speed))))

     (define (do-set key val)
       (displayln (ctrl `(SET ,world-port ,key ,val))))

     (define (do-get id key)
       (define A (dict-ref agents id #f))
       (define A-meta (and A (car (cddddr A))))
       (writeln (dict-ref (or A-meta null) key #f)))

     (let ([π-read (source (λ () (read)))])
       (with-handlers ([exn:break? void])
         (forever
           (match
               (sync
                (recv-evt π-read)
                (loop-evt (λ () (recv-evt world))
                          (λ (msg)
                            (define agents-addr (car msg))
                            (set! agents (cadr msg))
                            (define my-mode (car (dict-ref agents my-id)))
                            (apply (if (eq? my-mode 'IDLE) DEBUG INFO) msg))))
             ['idle (do-idle)]
             [`(move ,target ,speed) (do-move target speed)]
             [`(set ,key ,val) (do-set key val)]
             [`(get ,id ,key) (do-get id key)]
             [`(get ,key) (do-get my-id key)]
             ['quit (break-thread (current-thread))]
             [_ 'BAD-INPUT])))
       (stop π-read))

     (ctrl `(STOP ,world-port))

     (stop ctrl)
     (stop world)
     (INFO - 'done))))
