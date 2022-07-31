(in-package "ACL2S")
(in-theory (enable remove-assoc-equal))
(include-book "network")
(include-book "utilities")

(defconst *terrible-weights*
  (list 10 10 -1 -1 -1 10 -1 -1))

(defconst *terrible-params*
  (params 10 ;; activation window
	  1  ;; mesh time quantum
	  2  ;; p2cap
	  100 ;; timeQuantaInMeshCap
	  100 ;; meshMessageDeliveriesCap
	  1  ;; meshMessageDeliveriesThreshold
	  5 ;; TopicCap
	  10 ;; grayListThreshold
	  5 ;; d
	  2 ;; dlo
	  7 ;; dhi
	  6 ;; dlazy
	  1 ;; hbmInterval
	  5 ;; fanoutTTL
	  3 ;; mcacheLen
	  3 ;; mcacheGsp
	  3 ;; seenTTL
	  2 ;; opportunisticGraftThreshold
	  40 ;; topicWeight
	  (/ 1 2) ;; meshMessageDeliveriesDecay
	  (/ 1 2) ;; firstMessageDeliveriesDecay
	  (/ 1 2) ;; behaviorPenaltyDecay
	  (/ 1 2) ;; meshFailurePenaltyDecay
	  (/ 1 2) ;; invalidMessageDeliveriesDecay
	  (/ 1 100) ;; decayToZero
	  6 ;; decayInterval
	  ))

(defconst *terrible-wp* (cons *terrible-weights* *terrible-params*))

(defconst *terrible-twp* (list
			  (cons 'top1 *terrible-wp*)
			  (cons 'top2 *terrible-wp*)
			  (cons 'top3 *terrible-wp*)
			  (cons 'top4 *terrible-wp*)
			  (cons 'critical-topic *terrible-wp*)))

;; Example attack:
;; time in mesh >= cap = 100
;; send zero new messages at each time step so that you
;; achieve the maximal penalty from w3, w3b

;; When we show a violation to p3b are we remembering to allow all the HBM events and corresponding sub-events such as gossiping?

(set-ignore-ok t)


(definec construct-mesh (p2 :peer ps :lop ts :lot) :topic-lop-map
  (match ts
    (() '())
    ((top . rst) (cons `(,top . ,ps)
                       (construct-mesh p2 ps rst)))))


(skip-proofs
(definec initialize-group-of-meshpeers (ps :lop mps :lop ts :lot d :nat) :group
  :skip-tests t
  (match ps
    (() '())
    ((p . rst)
     (let ((pmesh (construct-mesh p (grab d (remove-equal p (app ps mps))) ts)))
       (cons `(,p . ,(peer-state (update-topic-mesh
                                  (update-nbr-topicsubs
                                   (new-nbr-topic-state)
                                   pmesh)
                                  pmesh)
                                 (new-msgs-state)
                                 '() '() '()))
             (initialize-group-of-meshpeers rst mps ts d)))))))


(b* ((ps (gen-peers 1 10))
     (grp (initialize-group-of-meshpeers ps ps '(BLOCK AGG) 5)))
  grp)


(skip-proofs
 (definec emit-meshmsgdeliveries-peer-topic
   (p1 :peer p2 :peer top :topic n :nat) :loev
   (match n
     (0 '())
     (_ (app (list (list p1 'SND p2 'PAYLOAD (app (take 2 (nth-payload-type n))
                                                  (list top p1)))
                   (list p2 'RCV p1 'PAYLOAD (app (take 2 (nth-payload-type n))
                                                  (list top p1))))
             (emit-meshmsgdeliveries-peer-topic p1 p2 top (1- n)))))))


(skip-proofs
(definec emit-meshmsgdeliveries-topic (ps :lop p2 :peer top :topic n :nat) :loev
  (match ps
    (() '())
    ((p . rst) (app (emit-meshmsgdeliveries-peer-topic p p2 top n)
                    (emit-meshmsgdeliveries-topic rst p2 top n))))))

(skip-proofs
(definec emit-evnts (ps :lop p2 :peer ts :lot n :nat) :loev
  (match ts
    (() '())
    ((top . rst)
     (app (emit-meshmsgdeliveries-topic ps p2 top n)
          (emit-evnts ps p2 rst n))))))

(emit-evnts '(P1 P2) 'P100 '(top1) 3)


(definecd hbm-evnts (grp :group) :loev
  :skip-body-contractsp t
  (match grp
    (() '())
    (((p . &) . rst)
     (cons `(,p HBM 1) (hbm-evnts rst)))))

(definecd repeat-evnts (evnts :loev n :nat) :loev
  (match n
    (0 '())
    (& (app evnts (repeat-evnts evnts (1- n))))))
  

;; testing with 2 topics
(time$
(b* ((ps (gen-peers 1 10))
     (grp (initialize-group-of-meshpeers ps ps '(top1 top2) 5))
     (evnts (hbm-evnts grp))
     (ticks 10))
  (run-network-gr grp (repeat-evnts evnts 100)
                  (len evnts) *terrible-twp* 42)))


;; (b* ((ps (gen-peers 1 10))
;;      (grp (initialize-group-of-meshpeers ps ps '(top1 top2) 5))
;;      (evnts (hbm-evnts grp))
;;      (ticks 100000))
;;   (run-network-gr grp (repeat-evnts evnts 100000)
;;                   (* 100000 (len evnts)) *terrible-twp* 42)))
; (ACL2::EV-REC ACL2::*RETURN-LAST-ARG3* ...) took 
; 62.89 seconds realtime, 62.26 seconds runtime

;; Even after 100000 secs of network global time/, mesh remains as it was
;; initialized, with no one pruned, and no change in nbr-scores.  Sample scores
;; in state of P1 (:NBR-SCORES (P6 . 5) (P5 . 5) (P4 . 5) (P3 . 5) (P2 . 5))



;; ACL2S !>(time$
;; (b* ((ps (gen-peers 1 1000))
;;      (grp (initialize-group-of-meshpeers ps ps '(top1 top2) 5))
;;      (evnts (hbm-evnts grp))
;;      (ticks 10))
;;   (run-network-gr grp (repeat-evnts evnts 1000)
;;                   (* 100000 (len evnts)) *terrible-twp* 42)))
;; ; (ACL2::EV-REC ACL2::*RETURN-LAST-ARG3* ...) took 
;; ; 89.23 seconds realtime, 88.82 seconds runtime



;; (b* ((ps (gen-peers 1 100))
;;      (grp (initialize-group-of-meshpeers ps ps '(top1 top2) 5))
;;      (evnts (hbm-evnts grp))
;;      (ticks 10))
;;   (run-network-gr grp (repeat-evnts evnts 100)
;;                   (* 100000 (len evnts)) *terrible-twp*FLo 42)))
;; ; (ACL2::EV-REC ACL2::*RETURN-LAST-ARG3* ...) took 
;; ; 0.64 seconds realtime, 0.64 seconds runtime


;; (b* ((ps (gen-peers 1 1000))
;;      (grp (initialize-group-of-meshpeers ps ps '(top1 top2) 5))
;;      (evnts (hbm-evnts grp))
;;      (ticks 10))
;;   (run-network-gr grp (repeat-evnts evnts 100)
;;                   (* 100000 (len evnts)) *terrible-twp* 42)))
;; ; (ACL2::EV-REC ACL2::*RETURN-LAST-ARG3* ...) took 
;; ; 8.68 seconds realtime, 8.65 seconds runtime


(in-theory (enable tctrsp))
(property score-wont-change (pcm :p-gctrs-map n :pos-rat)
          :proofs? nil
          (equal (calc-nbr-scores-map (list
                                       (cons '(P2 . TOP1) (tctrs 1/2 0 n 1/2 0))
                                       (cons '(P2 . TOP2) (tctrs 1/2 0 n 1/2 0)))
                                      pcm *terrible-twp*)
                 (calc-nbr-scores-map (list
                                       (cons '(P2 . TOP1) (tctrs 1/2 0 (1+ n) 1/2 0))
                                       (cons '(P2 . TOP2) (tctrs 1/2 0 (1+ n) 1/2 0)))
                                      pcm *terrible-twp*)))


(property score-wont-change (pcm :p-gctrs-map n :pos-rat)
          (equal (calc-nbr-scores-map (list
                                       (cons '(P2 . TOP1) (tctrs 1/2 0 n 1/2 0)))
                                      pcm *terrible-twp*)
                 (calc-nbr-scores-map (list
                                       (cons '(P2 . TOP1) (tctrs 1/2 0 (1+ n) 1/2 0)))
                                      pcm *terrible-twp*)))
