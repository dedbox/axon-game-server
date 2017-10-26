;; Copyright © 2017 Eric Griffis <dedbox@gmail.com>
;;
;; This software may be modified and distributed under the terms of the MIT
;; license. See the LICENSE file for details.

#lang racket/base

(provide (all-defined-out))

(require axon/codec
         axon/control
         axon/filter
         axon/internal
         axon/process
         axon/udp
         axon/tcp
         db
         racket/format
         racket/match)

(define server-name "MrG's Forest")
(define server-host "0.0.0.0")
(define server-port 3700)

;; Discovery Beacon

(define beacon
  (let ([σ (udp-server sexp-printer sexp-parser server-port server-host)])
    (filter
     (λ ()
       (forever
         (match-define (list host port _) (recv σ))
         (give σ (list host port server-name))))
     (λ () (close σ)))))

(define dbc (sqlite3-connect #:database "state.db" #:use-place #f))

;; Client Data

(define (add-client name addr)
  (query-exec dbc "INSERT INTO clients (name, address) VALUES (?, ?)"
              name (~a addr)))

(define (client id)
  (query-maybe-row dbc "SELECT id, name FROM clients where id = ?" id))

(define (name->client-id name)
  (query-maybe-row dbc "SELECT id FROM clients WHERE name = ?" name))

(define (address->client-id addr)
  (query-maybe-value dbc "SELECT id FROM clients WHERE address = ?" addr))

(define (set-client-address id addr)
  (query-exec dbc "UPDATE clients SET address = ? WHERE id = ?" (~a addr) id))

(define (unset-client-address id)
  (query-exec dbc "UPDATE clients SET address = NULL WHERE id = ?" id))

;; Client Assets

(define (clients)
  (map vector->list
       (query-rows dbc
                   "SELECT id, name FROM clients WHERE address IS NOT NULL")))

(define (connect name addr)
  (define id (or (name->client-id addr)
                 (and (add-client name addr) (name->client-id addr))))
  (set-client-address id addr)
  id)

(define (disconnect addr)
  (define id (address->client-id addr))
  (and id (unset-client-address id)))

;; Character Data

(define (id->character-id id)
  (query-maybe-value
   dbc "SELECT character_id FROM client_character WHERE client_id = ?" id))

(define (make-client-character id char-id)
  (query-exec dbc "INSERT INTO client_character VALUES (?, ?)" id char-id))

(define (set-client-character-id id char-id)
  (query-exec
   dbc "UPDATE client_character SET character_id = ? WHERE client_id = ?"
   id char-id))

;; Character Assets

(define (character addr char-id)
  (define id (address->client-id addr))
  (and id
       (if (id->character-id id)
           (set-client-character-id id char-id)
           (make-client-character id char-id))
       #t))

;; World Assets

;; Asset Service

(define (peer-factory addr)
  (serve (bind ([CLIENTS (clients)]
                [(CLIENT ,id) (client id)]
                [(CONNECT ,name) (connect name addr)]
                [DISCONNECT (disconnect addr)]
                [(CHARACTER ,char-id) (character addr char-id)]
                ))
         void
         (λ () (disconnect addr))))

(define asset-service
  (tcp-service sexp-codec-factory peer-factory server-port server-host))

;; Agent Simulation

;; World Simulation

;; Simulation Server

;; Cleanup

(stop beacon)
(stop asset-service)
