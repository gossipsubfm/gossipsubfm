(in-package "ACL2S")
(include-book "peer-state")

;;group of peers
(defdata group (alistof peer peer-state))

(definecd topic-mesh-graph (grp :group tp :topic) :tl
  (match grp
    (() '())
    (((p . st) . rst)
     (let ((ms (cdr (assoc-equal tp (nbr-topic-state-topic-mesh
                                     (peer-state-nts st))))))
       (cons (cons p ms) (topic-mesh-graph rst tp))))))

(definecd lookup-state (p :peer g :group ) :peer-state
  (match g
    (() (new-peer-state))
    (((!p . ps) . &) ps)
    ((& . rst) (lookup-state p rst))))

(defdata egl (alistof evnt group))

(property egl-loevs (egl :egl)
  :TESTING? nil
  (loevp (strip-cars egl)))

(property egl-cadr (egl :egl)
  :TESTING? nil
  (groupp (cdar egl)))

(property egl-lastgrp (egl :egl)
  :TESTING? nil
  (groupp (cdar (last egl))))

(property snd-rcv-evnt (p1 p2 x y :all)
  (=> (evntp `(,p1 SND ,p2 ,x ,y))
      (loevp (app `((,p2 RCV ,p1 ,x ,y)
                    (,p1 SND ,p2 ,x ,y)))))
  :hints (("Goal" :in-theory (enable evntp))))

(definecd network-propagator (evnts acc :loev) :loev
  :skip-tests t
  :body-contracts-hints (("Goal" :in-theory (enable evntp)))
  :timeout 600
  (match evnts
    (() (reverse acc))
    (((p1 'SND p2 x y) . rst)
     ;; we assume instantaneous message transmissions, with SND
     ;; followed immediately by RCV
     (network-propagator
      rst
      (app `((,p2 RCV ,p1 ,x ,y)
             (,p1 SND ,p2 ,x ,y))
           acc)))
    ((evnt . rst)
     (network-propagator rst (cons evnt acc)))))

;; weaving list of events 
(definecd splice (x y :loev) :loev
  (match (list x y)
    ((() ()) '())
    ((() y) y)
    ((x ()) x)
    (((a . xs) (b . ys))
     (append `(,a ,b) (splice xs ys)))))


;; (skip-proofs
;;  (definec gen-non-network-events (p :peer ntsubs :peer-lot-map k :nat) :loev
;;    (if (equal p nil)
;;        nil
;;      (b* ((atops (shuffle (cdr (assoc-equal p ntsubs)) k))
;;           (k (mod k 3))
;;           (aev `((,p LEAVE ,(car atops)))) ;;a leaves a subscribed topic
;;           (notbsubs (set-difference-equal atops (topics)))
;;           (bev `((,p JOIN ,(car notbsubs)))) ;;b joins a new topic
;;           (cev (if (endp atops)
;;                    nil
;;                  `((,p APP ,(car atops)
;;                        ,(custom-nth-payload k (car atops) p)))))
;;           ((when (< (len atops) 3)) nil))
;;        (cond
;;         ((= k 0) (if (loevp aev) aev nil))
;;         ((= k 1) (if (loevp bev) bev nil))
;;         ((= k 2) (if (loevp cev) nil nil)))))))

(property remove-assoc-gr (actor :peer gr :group)
  (groupp (remove-assoc-equal actor gr)))

(defthm car-evntp
  (=> (evntp ev)
      (peerp (car ev)))
  :hints (("Goal" :in-theory (enable evntp))))

(defthm cons-evntp
  (=> (evntp ev)
      (consp ev))
  :hints (("Goal" :in-theory (enable evntp))))

(definecd run-network (gr :group evnts :loev i :nat twpm :twp s :nat) :egl
  :ic (is-valid-twp twpm)
  :function-contract-hints
  (("Goal" :in-theory (enable peer-stateloevp)))
  :body-contracts-hints
  (("Goal" :do-not-induct t :in-theory (enable transition)))
  :timeout 600
  :skip-tests t
  (if (v (zp i)
         (endp evnts))
      '()
    (b* (((mv k s) (defdata::genrandom-seed
                     (1- (expt 2 31))
                     (mod s (expt 2 31))))
         (actor (caar evnts))
         (actor-state (lookup-state actor gr))
         (res3 (transition actor actor-state (car evnts)
                           twpm k))
         (next-actor-state (first res3))
         (next-actor-events (network-propagator (second res3) nil))
         (newgrp `((,actor . ,next-actor-state) . ,(remove-assoc-equal actor gr))))
      (cons `(,(car evnts) . ,newgrp)
            (run-network
             newgrp
             ;;mix generated events with remaining events
             (app next-actor-events (cdr evnts))
             (1- i)
             twpm
             s)))))

(definecd run-network-gr (gr :group evnts :loev i :nat twpm :twp s :nat) :group
  :ic (is-valid-twp twpm)
  :function-contract-hints
  (("Goal" :in-theory (enable peer-stateloevp)))
  :body-contracts-hints
  (("Goal" :do-not-induct t :in-theory (enable transition)))
  :timeout 600
  :skip-tests t
  (cdr (car (reverse (run-network gr evnts i twpm s)))))

