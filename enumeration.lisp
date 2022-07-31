(in-package "ACL2S")
(include-book "interface/top")

(include-book "network")
(include-book "utilities")

(modeling-start)
(set-cgen-guard-checking :all)

;; number of peers in a group
(defconst *n* 10)
(define-attach-constant N pos *n*)

;; avg subscriptions per peer
(defconst *s* 3)
(define-attach-constant S pos *s*)


;; we want theorems of the form
;; for range this to that of weights, something is cool

(definec nth-factors-custom (k :nat) :factors
    (cons (params 1 1 3 5 2 1 5 -3 4 3 5 3 1 60 5 3 120 2)
          `((FM . ,(nth-weights k))
            (PL . ,(nth-weights (1+ k))))))


(defdata-attach factors :test-enumerator nth-factors-custom)

;; distributes peers among mesh topics, returns (alistof topic mesh) 
(definec mk-meshes (peers :lop mtopx :lot ppm :nat) :topic-lop-map
  (match mtopx
    (() '())
    ((tp . rst)
     (match peers
       (() '())
       ;; We need atleast 1 peer per mesh
       (& (let ((x (min (max 1 ppm)
                        (len peers))))
            (acons tp
                   (take x peers)
                   (mk-meshes (nthcdr x peers) rst ppm))))))))


;; create j msgs i,i-1...,i-j
(definec make-mcache (sender :peer i :nat j :nat top :topic) :mcache
  (if (or (zp i) (zp j))
      '()
    (let ((pld (custom-nth-payload i top sender)))
      (cons (list pld sender)
            (make-mcache sender (1- i) (1- j) top)))))

(defdata peer-msgs-map (alistof peer mcache))

(definec dist-msgs (nbrs :lop msgs :mcache acc :peer-msgs-map) :peer-msgs-map
  (match nbrs
    (() acc)
    ((n . rst) (dist-msgs rst
                          (nthcdr 2 msgs)
                          (acons n
                                 (app (grab 2 msgs)
                                      (cdr (assoc-equal n acc)))
                                 (remove-assoc-equal n acc))))))


;; distribute some msgs for each topic peer subscribes to
(definec mk-peer-msgs (meshes :topic-lop-map i :nat acc :peer-msgs-map) :peer-msgs-map
  (match meshes
    (() acc)
    (((top . nbrs) . rst)
     (b* ((sender (car nbrs))
          (nbrs   (cdr nbrs))
          (j (* (len nbrs) 2)))
       (if (endp nbrs)
           (mk-peer-msgs rst i acc)
         (mk-peer-msgs rst
                       (+ i j 10)
                       (dist-msgs nbrs
                                  (make-mcache sender i j top)
                                  acc)))))))


(create-reduce* 
 (lambda (x res) (add-sub res (cdr x) (car x)))
 peer-lot-mapp
 lotopicpeerp
 (:name flattlm->plt))

(property flat-topic-lop-map (ms :topic-lop-map)
          (lotopicpeerp (flatten-map ms)))

;; get peer subscriptions out of mesh maps
(definec meshes->subs (ms :topic-lop-map) :peer-lot-map
  (reduce* flattlm->plt nil (flatten-map ms)))

(check= (meshes->subs '((DS AN MX) (FM MX)))
        '((MX DS FM) (AN DS)))


(definec random-sublist (ls :tl s :nat) :tl
  (b* (((mv k s) (defdata::genrandom-seed
		   (1- (expt 2 31))
		   (mod s (expt 2 31))))
       (i (mod k (s-attached-base))))
    (match (list ls i)
      ((() &) '())
      (((l . rst) 0) (cons l (random-sublist rst s)))
      (((& . rst) &) (random-sublist rst s)))))
       



(definec mk-subs-map (peers :lop acc :peer-lot-map s :nat) :peer-lot-map
  (b* (((mv k s) (defdata::genrandom-seed
		   (1- (expt 2 31))
		   (mod s (expt 2 31)))))
    (match peers
      (() acc)
      ((p . rst) (add-subs (mk-subs-map rst acc s)
                           p
                           (random-sublist (topics) k))))))

  

;; assign random nbrs from rps to peers in mps
;; assign random topics to peers in rps as subscribed to.
;; assign random peers as FO routers to random subset of nbrs

;; extracts meshes of a peer from topic-lop map
(definec get-peer-meshes (p :peer ms :topic-lop-map) :topic-lop-map
  (match ms
    (() '())
    (((top . ps) . rst)
     (if (in p ps)
         (cons `(,top . ,(remove-equal p ps)) (get-peer-meshes p rst))
       (get-peer-meshes p rst)))))


;;we assume that every peer is a neighbour of every other peer
;; Since all interesting events occur between neighbours, we want to increase
;; the likelyhood of interesting events happening
(definec mk-peer-state (p :peer nt :peer-lot-map ms :topic-lop-map msgs :mcache) :peer-state
  (peer-state (nbr-topic-state 
               nt ;; peer knows nbr subscriptions of all nbrs
               nil ;; no fanout
               nil ;; no last pub data
               (get-peer-meshes p ms)) ;; peer meshes
              (msgs-state (pairlis$ msgs (natlist (+ 10 (len msgs))))
                          msgs
			  `(,(len msgs))
                          nil nil 0 0)
              nil
              nil))


(definec gen-group (ps :lop nt :peer-lot-map ms :topic-lop-map pmm :peer-msgs-map) :group
  (match ps
    (() '())
    ((r . rst) (cons `(,r . ,(mk-peer-state r nt ms (cdr (assoc-equal r pmm))))
                     (gen-group rst nt ms pmm)))))
  

(set-ignore-ok t)
(defun nth-grp-custom (k)
  (b* ((factors (nth-factors-custom k))
       (mtop (car (topics))) ;; have only 1 mesh, size > dhigh
       ;;number of mesh members > dhigh for a single mesh
       (mn (1+ (params-dhigh (car factors))))
       ;;limit n number of peers, should be > mn
       (n (+ (mod k (n-attached-base)) mn 2))
       (peers (reverse
               (map* mk-peer (natlist n))))
       (mps (take mn peers))
       ;(rps (nthcdr mn peers))
       (ms (mk-meshes mps (list mtop) mn))
       (init-plm (meshes->subs ms))
       (nt (mk-subs-map peers init-plm k))
       (pmm (mk-peer-msgs ms 100 nil)))
    ;;messages only in peers that
    ;;are in mesh
    (gen-group peers nt ms pmm)))


;(nth-grp-custom 13)
    

(property nth-grp-custom-generates-groups (n :nat)
          (groupp (nth-grp-custom n)))


(definec group-invariants (gs :group) :boolean
  (match gs
    (() t)
    (((p . s) . rst) (^ (nbr-topic-state-invp (peer-state-nts s))
                        (group-invariants rst)))))

  
(property group-ntstates-satisfy-invariants (n :nat)
          (group-invariants (nth-grp-custom n)))


(defdata-attach group :test-enumerator nth-grp-custom)



;; example factors
(defconst *factors2* (cons
		     (params 1 1 3 5 2 1 5 -3 4 3 5 6 1 60 5 3 120 2)
		     '((FM . (1 ;; time in mesh
                              1 ;; first message deliveries
                              -1 ;; mesh message delivery rate
                              -1 ;; mesh message delivery failures
                              -1 ;; invalid messages
                              1 ;; app specific
                              -1 ;; IP colocation factor
                              -1))))) ;; behaviour penalty


(defconst *dur* 1000) ;;dur epochs or HBM intervals
(define-attach-constant DUR pos *dur*)



;;non-network events
;; HBM ,  JOIN TOPIC (by APP), NEW PEER JOINING, NEW MSG BY APP
(definec gen-hbm-events (peers :lop ntsubs :peer-lot-map allpeers :lop i :nat s :nat) :loev
  (b* (((mv k s) (defdata::genrandom-seed
		   (1- (expt 2 31))
		   (mod s (expt 2 31))))
       (elapsed (1+ (mod k 10)))
       (hbmev `(,(car peers) HBM ,elapsed)))
    (cond ((or (zp i)
               (endp allpeers))
           '())
          ((endp peers) (gen-hbm-events allpeers ntsubs allpeers (1- i) s))
          (t (append ;`((ACL2::P6 SND ACL2::P1 IWANT (ACL2::PID100 ACL2::PID100 ACL2::PID100 ))
                     ;  (ACL2::P1 RCV ACL2::P6 IWANT (ACL2::PID100 ACL2::PID100 ACL2::PID100 ))
                       
                       `(,hbmev)
                     (gen-hbm-events (cdr peers) ntsubs allpeers i s))))))



(defun nth-egl-custom (k)
  (b* ((d (dur-attached-base))
       (grp (nth-grp-custom k))
       (peers (strip-cars grp)))    
    (cons (cons '(ACL2::P1 HBM 1) grp)
          (run-network grp
                       (gen-hbm-events peers
                                       (nbr-topic-state-nbr-topicsubs
                                        (peer-state-nts (cdar grp)))
                                       peers d k)
                       1000
                       *factors2*
                       k
                       nil))))


(property eglp-nth-egl-custom (k :nat)
          (eglp (nth-egl-custom k)))

(defdata-attach egl :test-enumerator nth-egl-custom)


#|
;(strip-cars (nth-egl-custom 90))

(let ((egl (nth-egl-custom 90)))
  (list 
   ;;events
   (strip-cars egl)
   
   ;;last group state
   (cdar (last egl))))
|#


;;use symbol instead of pair for peers

;; inject application level events (not network level)



;; scoring function attack
(property (ptc :pt-counters-map p :peer top :topic)
	  :proofs? nil
	  :hyps (^ (>= (params-meshMessageDeliveriesCap (car *factors2*))
		       (params-meshMessageDeliveriesThreshold (car *factors2*)))
					;(in top (strip-cars (cdr f)))
		   (member-equal `(,p . ,top) (strip-cars ptc)))
	  (=> (> (lookup-score p (calc-nbr-scores-map ptc *factors2* nil)) 0)
	      (> (calcScoreTopic (lookup-counters p top ptc) (lookup-weights (cdr *factors2*) top) (car *factors2*)) 0))
	  :debug? t)









(definecd mesh-peers (p1 :peer p2 :peer gr :group) :boolean
  (and (in p2 (strip-cdrs
               (flatten-map
                (nbr-topic-state-topic-mesh
                 (peer-state-nts
                  (lookup-state p1 gr))))))
       (in p1 (strip-cdrs
               (flatten-map
                (nbr-topic-state-topic-mesh
                 (peer-state-nts
                  (lookup-state p2 gr))))))))


(defdata log (listof group))
(definecd mesh-peers-always (p1 :peer p2 :peer log :log) :boolean
  (if (endp log)
      t
    (and (mesh-peers p1 p2 (car log))
         (mesh-peers-always p1 p2 (cdr log)))))


#|
:q

(defun ctrx-events (ctrx-egl)
  (strip-cars (car (cdadr (caaadr ctrx-egl)))))


(b* ((res (itest?-query '(=> (^ (eglp egl)
                                (mesh-peers 'P1 'P2 (cdar egl)))
                             (mesh-peers-always 'P1 'P2 (strip-cdrs egl))))))
  (print (car res))
  (ctrx-events res))





(property (egl :egl n :nat)
          (=> (mesh-peers '(X . 1) '(X . 2) (cdar egl))
              (mesh-peers-always '(X . 1) '(X . 2) (strip-cdrs egl))))
|#

              


#|
(groupp (list (cons '(X . 1)  (peer-state (nt-state '(((X . 2) FM))
                                                                     nil
                                                                     '((FM (X . 2))))
                                 (new-msgs-state)
                                 nil
                                 (list (cons '(X . 2)  5))))
              (cons '(X . 2)  (peer-state (nt-state '(((X . 1) FM))
                                                    nil
                                                    '((FM (X . 1))))
                                 (new-msgs-state)
                                 nil
                                 (list (cons '(X . 1) 5))))))
  


;;========================================
;; Convert generated events to JSON
;;========================================

:q

(load "~/quicklisp/setup.lisp")
(ql:register-local-projects)

(ql:quickload :acl2s-tcp-worker/json)


(acl2s-json::encode-value '(a b c))

(defun peer-compactor (p)
  (if (peerp p)
      (intern (format nil "~a~a" (car p) (cdr p)))
    p))

(defun format-evnts (es)
  (if (endp es)
      '()
    (cons (format nil "~a" (mapcar #'peer-compactor (car es)))
          (format-evnts (cdr es)))))


(format-evnts (nth-psevnts-custom 5))
    

(acl2s-json::encode-value (format-evnts (nth-psevnts-custom 121)))

|#


;;just a single heartbeat
#|
(check= (run-network nil '((AN HBM 10)) *params*)
	'((AN (:0TAG . PEER-STATE)
                 (:MST (:0TAG . MSGS-STATE))
                 (:NTS (:0TAG . NT-STATE)))
          (AN (:0TAG . PEER-STATE)
                 (:MST (:0TAG . MSGS-STATE))
                 (:NTS (:0TAG . NT-STATE)))))
|#
