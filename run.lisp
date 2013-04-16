
;;; Author: Pieter Wellens, February 2013

; this version relies on the availability of framenet data version 1.5
; in sharing/framenet/fndata/ (or somewhere else, adapt
; *framenet-data-directory*). 
;;You can request that data from framenet. If you need support for a
;; newer version of framenet then contact pieter@ai.vub.ac.be.

(asdf:operate 'asdf:load-op :framenet)

(in-package :framenet)

;; Start using framenet data (v1.5) by initialization.

(init-fn-data :load-lexical-units t)


