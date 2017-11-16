;; Copyright © 2017 Eric Griffis <dedbox@gmail.com>
;;
;; This software may be modified and distributed under the terms of the MIT
;; license. See the LICENSE file for details.

#lang racket/base

(provide (all-defined-out))

(require axon
         "common.rkt"
         racket/dict
         racket/struct)

;;; Discovery

(define (beacon-server server-name server-addr)
  (define server-host (car server-addr))
  (define beacon-addr (list server-host beacon-port))
  (define σ (udp-server sexp-printer sexp-parser beacon-addr))
  (filter (λ ()
            (forever
              (define msg (recv σ))
              (apply INFO msg)
              (define msg-addr (car msg))
              (define msg-data (cadr msg))
              (give σ (list msg-addr (list server-name (cadr server-addr) msg-addr)))))
          (λ () (close σ))))

;;; Simulation

(struct vec (x y) #:prefab)

(define (vlen a)
  (sqrt (+ (expt (vec-x a) 2) (expt (vec-y a) 2))))

(define (vecop op)
  (λ (a b)
    (cond [(and (vec? a) (vec? b)) (vec (op (vec-x a) (vec-x b))
                                        (op (vec-y a) (vec-y b)))]
          [(vec? a) (vec (op (vec-x a) b)
                         (op (vec-y a) b))]
          [(vec? b) (vec (op a (vec-x b))
                         (op a (vec-y b)))]
          [else (op a b)])))

(define vec+ (vecop +))
(define vec- (vecop -))
(define vec* (vecop *))
(define vec/ (vecop /))

(struct agent (id mode target position velocity meta) #:prefab
        #:name -agent
        #:constructor-name -agent)

(define (agent id
               [mode 'IDLE]
               [target #f]
               [position (vec 0 0)]
               [velocity (vec 0 0)]
               [meta null])
  (-agent id mode target position velocity meta))

(define (agent->list A)
  (define A-list (struct->list A))
  (define A-id (car A-list))
  (define A-mode (cadr A-list))
  (define A-target (caddr A-list))
  (define A-position (cadddr A-list))
  (define A-velocity (car (cddddr A-list)))
  (define A-meta (cadr (cddddr A-list)))
  (list A-id
        A-mode
        (and A-target (struct->list A-target))
        (struct->list A-position)
        (struct->list A-velocity)
        A-meta))

(define (world-server world-fps server-addr)
  (define agents null)
  (define next-id -1)

  (define (do-start addr)
    (and (not (dict-has-key? agents addr))
         (set! next-id (+ next-id 1))
         (set! agents (dict-set agents addr (agent next-id)))
         next-id))

  (define (do-stop addr)
    (and (dict-has-key? agents addr)
         (set! agents (dict-remove agents addr))
         #t))

  (define (do-idle addr)
    (and (update-agent addr 'IDLE #f - (vec 0 0) -)
         #t))

  (define (do-move addr x-t speed)
    (and (update-agent addr 'MOVE x-t -
                       (λ (A)
                         (define p (vec- x-t (agent-position A)))
                         (vec* p (/ speed (vlen p))))
                       -)
         #t))

  (define (do-set addr key val)
    (and (update-agent addr - - - - (λ (A) (dict-set (agent-meta A) key val)))
         #t))

  (define (update-agent addr mode x-t x v meta)
    (and (dict-has-key? agents addr)
         (set! agents
           (dict-update agents addr
                        (λ (A)
                          (define (f arg accessor)
                            (cond [(eq? arg -) (accessor A)]
                                  [(procedure? arg) (arg A)]
                                  [else arg]))
                          (agent (agent-id A)
                                 (f mode agent-mode)
                                 (f x-t agent-target)
                                 (f x agent-position)
                                 (f v agent-velocity)
                                 (f meta agent-meta)))))))

  (define (update-world Δt)
    (DEBUG - 'tick)
    (define (update-agents addr A)
      (define id (agent-id A))
      (define mode (agent-mode A))
      (define x-t (agent-target A))
      (define x (agent-position A))
      (define v (agent-velocity A))
      (define meta (agent-meta A))
      (define Δx (vec* v Δt))
      (cons addr (if (eq? mode 'MOVE)
                     (if (< (vlen (vec- x-t x)) (vlen Δx))
                         (agent id 'IDLE #f x-t (vec 0 0) meta)
                         (agent id 'MOVE x-t (vec+ x Δx) v meta))
                     (agent id mode x-t (vec+ x Δx) v meta))))
    (set! agents (dict-map agents update-agents)))

  (define (publish Δt)
    (define addrs (dict-keys agents))
    (define payload (map agent->list (dict-values agents)))
    (DEBUG - 'publish addrs payload)
    (for ([addr addrs])
      (give world-srv (list addr payload))))

  (define world-sim (simulate world-fps update-world))
  (define world-srv (udp-server sexp-printer sexp-parser server-addr))
  (define world-pub (simulate world-fps publish))

  (commanded
    (filter (λ () (sync world-sim world-srv world-pub))
            void
            (λ () (stop world-pub) (stop world-sim)))
    (bind ([(START ,addr) (do-start addr)]
           [(STOP ,addr) (do-stop addr)]
           [(IDLE ,addr) (do-idle addr)]
           [(MOVE ,addr ,target ,speed) (do-move addr (apply vec target) speed)]
           [(SET ,addr ,key ,val) (do-set addr key val)]))))

(define (start-agent world addr)
  (command world `(START ,addr)))

(define (stop-agent world addr)
  (command world `(STOP ,addr)))

(define (idle-agent world addr)
  (command world `(IDLE ,addr)))

(define (move-agent world addr target speed)
  (command world `(MOVE ,addr ,target ,speed)))

(define (set-agent-meta world addr key val)
  (command world `(SET ,addr ,key ,val)))

;;; Control

(define (control-service server-name server-addr world-fps)
  (define beacon (beacon-server server-name server-addr))
  (define world (world-server world-fps server-addr))
  (define agents null)

  (define (peer-factory peer-addr)
    (set! peer-addr (cddr peer-addr))
    (INFO peer-addr 'connected)

    (define (address port)
      (list (car peer-addr) port))

    (define (update-agents proc)
      (set! agents (dict-update agents peer-addr proc null)))

    (define (do-start port)
      (define id (start-agent world (address port)))
      (and id
           (update-agents (λ (ports) (cons port ports)))
           id))

    (define (do-stop port)
      (and (stop-agent world (address port))
           (update-agents (λ (ports) (remove port ports)))
           #t))

    (define (do-idle port)
      (and (idle-agent world (address port))
           #t))

    (define (do-move port target speed)
      (and (move-agent world (address port) target speed)
           #t))

    (define (do-set port key val)
      (and (set-agent-meta world (address port) key val)
           #t))

    (serve
     (compose (λ (rep) (INFO peer-addr '<< rep) rep)
              (bind ([ADDR peer-addr]
                     [(START ,port) (do-start port)]
                     [(STOP ,port) (do-stop port)]
                     [(IDLE ,port) (do-idle port)]
                     [(MOVE ,port ,target ,speed) (do-move port target speed)]
                     [(SET ,port ,key ,val) (do-set port key val)]))
              (λ (req) (INFO peer-addr '>> req) req))
     void
     (λ ()
       (for-each do-stop (dict-ref agents peer-addr null))
       (set! agents (dict-remove agents peer-addr))
       (INFO peer-addr 'disconnected))))

  (begin0
      (tcp-service sexp-codec-factory peer-factory server-addr
                   void
                   (λ () (stop beacon) (stop world) (INFO - 'STOPPED)))
    (INFO - 'STARTED)))

;;; Main

(module+ main
  (define server-name "MrG's Forest")
  (define server-addr '("0.0.0.0" 3700))
  (define world-fps 5)

  (log-to-stdout
   (λ ()
     (parameterize-break #f
       (define control-svc (control-service server-name server-addr world-fps))
       (with-handlers ([exn:break? void])
         (sync/enable-break control-svc))
       (stop control-svc)))))
