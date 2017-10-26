;; Copyright © 2017 Eric Griffis <dedbox@gmail.com>
;;
;; This software may be modified and distributed under the terms of the MIT
;; license. See the LICENSE file for details.

#lang racket/base

(provide (all-defined-out))

(require axon/codec
         axon/filter
         axon/internal
         axon/process
         axon/udp)

(define beacon (udp-stream sexp-printer sexp-parser "255.255.255.255" 3700 #f 0))
(give beacon '("255.255.255.255" 3700 SERVERS))
(recv beacon)
