(in-package "ACL2S")
(include-book "higher-order")
(include-book "utils")

(def-const *few-seconds* 5)
(def-const *two-minutes* 120)

(include-book "network")
(include-book "utilities")
(include-book "graphs")

;Uncomment line 30 : '(BLOCKS AGG SUB1 SUB2 SUB3)) in scoring.lisp

;; ------------------ Eth2.0 Case Study (github.com/silesiacoin/prysm-spike) ------------------------------

;; Refer to https://github.com/silesiacoin/prysm-spike/blob/d5ac70f0406b445a276ee61ba10fdf0eb6aafa0f/
;;          beacon-chain/p2p/gossip_scoring_params.go
;;
;; d5ac70f0406b445a276ee61ba10fdf0eb6aafa0f
;;
;;
;; GossipSub is my private version forked off of be065ce0510e81d820a2cdb9762e63fd012122ba
;; Libp2p is my private version forked off of 4400328a581babd9a196e1ddffbe996ae7b3b59

;;    IPColocationFactorThreshold = 10
;;    BehaviourPenaltyThreshold   = 6
;;    BehaviourPenaltyDecay       = 10 epochs
;;    DecayInterval               = 1 slot duration
;;    DecayToZero                 = 0.01
;;    RetainScore                 = 100 epoch durations

(defconst *decayToZero* 1/100)

;; https://github.com/silesiacoin/prysm-spike/blob/d5ac70f0406b445a276ee61ba10fdf0eb6aafa0f/
;;     shared/params/config.go#L123

(defconst *seconds-per-slot* 1) ;; ARBITRARY 
(defconst *one-epoch-duration* 1) ;; ARBITRARY 

(defconst *aggregateWeight* (/ 1 2)) ;; weight for aggregate topic, see #L100
(defconst *beaconBlockWeight* (/ 8 10)) ;; weight for beacon topic, see #L77

;; https://github.com/silesiacoin/prysm-spike/blob/d5ac70f0406b445a276ee61ba10fdf0eb6aafa0f/
;;     beacon-chain/p2p/gossip_scoring_params.go#L154
(defconst *slot-duration* (* *seconds-per-slot* 1))

;; https://github.com/silesiacoin/prysm-spike/blob/d5ac70f0406b445a276ee61ba10fdf0eb6aafa0f/
;;    shared/params/config.go#L48
(defconst *slots-per-epoch* 10) ;; ARBITRARY 
(defconst *blocks-per-epoch* *slots-per-epoch*) ;; #L75
(defconst *decay-epoch* 5) ;; #L74

;; In line number comments, if I write BC, I am referring to:
;; https://github.com/silesiacoin/prysm-spike/blob/d5ac70f0406b445a276ee61ba10fdf0eb6aafa0f/
;;     beacon-chain/p2p/pubsub.go

;; If I write SERV, I am referring to:
;; https://github.com/silesiacoin/prysm-spike/blob/d5ac70f0406b445a276ee61ba10fdf0eb6aafa0f/
;;     beacon-chain/p2p/service.go

;; In all other cases, as indicated previously, I am referring to gossip_scoring_params.go.

;; I assume all times are given in units of 1 second ...

(defconst *enable-larger-gossip-history* 'nil)

(defconst *hbmInterval* (/ 700 1000))

(defconst *topicCap* (+ 32 (/ 72 100)))

(defconst *comm-count-per-slot* 10) ;; ARBITRARY - refer to #L173 ... they have not decided yet!
;; https://github.com/silesiacoin/prysm-spike/blob/d5ac70f0406b445a276ee61ba10fdf0eb6aafa0f/
;;     shared/params/config.go#L123
(defconst *target-aggregators-per-committee* 5) ;; ARBITRARY
(defconst *aggregators-per-slot* (* *comm-count-per-slot* *target-aggregators-per-committee*)) ;; #L1871
(defconst *agg-per-epoch* (* *aggregators-per-slot* *slots-per-epoch*))
(defconst *attestationTotalWeight* 1) ;; #L23
;; https://github.com/silesiacoin/prysm-spike/blob/d5ac70f0406b445a276ee61ba10fdf0eb6aafa0f/
;; shared/params/network_config.go#L40
(defconst *AttestationSubnetCount* 3)         ;; ARBITRARY 
(defconst *MinGenesisActiveValidatorCount* 300) ;; ARBITRARY -----------------|
;; -------> but currently needs to be divisible by 50 x attestationSubnetCount.

(defconst *subnet-topicWeight* (/ *attestationTotalWeight* *AttestationSubnetCount*)) ;; #L121
(defconst *activeValidators* *MinGenesisActiveValidatorCount*) ;; #L169
(defconst *subnet-subnetWeight* (/ *activeValidators* *AttestationSubnetCount*)) ;; #L122
(defconst *subnet-minimumWeight* (/ *subnet-subnetWeight* 50)) ;; #L123
(defconst *subnet-numsPerSlot* (/ *subnet-subnetWeight* *slots-per-epoch*)) ;; #L124
(defconst *subnet-comsPerSlot* *comm-count-per-slot*) ;; #L125
(defconst *subnet-exceedsThreshold*
  (>= *subnet-comsPerSlot*
      (* 2 (/ *AttestationSubnetCount* *slots-per-epoch*)))) ;; #L126
(defconst *subnet-firstDecay* (if *subnet-exceedsThreshold* 4 1)) ;; #L127, 130
(defconst *subnet-meshDecay* (if *subnet-exceedsThreshold* 16 4)) ;; #L128, 131

(defconst *eth-default-block-weights*
  (weights (/ 324 10000)  ;; w_1  = time in mesh weight                (#L78)
           1              ;; w_2  = first message deliveries weight    (#L81)
           (/ -717 1000)  ;; w_3  = mesh message deliveries weight     (#L84)
           (/ -717 1000)  ;; w_3b = mesh failure penalty weight        (#L90)
           (- -140 (/ 4475 10000)) ;; w_4  = invalid messages weight   (#L92)
           1       ;; w_5  = application-specific score weight         (#L43, global)
           (- -35 (/ 11 100))   ;; w_6  = IP-co-location factor weight (#L44, global)
           (- -15 (/ 92 100)))) ;; w_7 = behavioral penalty weight     (#L47, global)

(check= t (weightsp *eth-default-block-weights*))

(defconst *eth-default-agg-weights*
  (weights (/ 324 10000) ;; w_1  = time in mesh weight              (#L101)
           (/ 128 1000)  ;; w_2  = first message deliveries weight  (#L104)
           (/ -64 1000)  ;; w_3  = mesh message deliveries weight   (#L107)
           (/ -64 1000)  ;; w_3b = mesh failure penalty weight      (#L113)
           (- -140 (/ 4475 10000)) ;; w_4 = invalid messages weight (#L115)
           1             ;; w_5 = application-specific score weight    (#L43, global)
           (- -35 (/ 11 100))   ;; w_6  = IP-co-location factor weight (#L44, global)
           (- -15 (/ 92 100)))) ;; w_7 = behavioral penalty weight     (#L47, global)

(check= t (weightsp *eth-default-agg-weights*))

(defconst *eth-default-agg-subnet-weights*
  (weights (/ 324 10000) ;; w_1 = time in mesh weight                  (#L134)
           (/ 955 1000)  ;; w_2 = first message deliveries weight      (#L138)
           (- -37 (/ 55 100)) ;; w_3  = mesh message deliveries weight (#L141)
           (- -37 (/ 55 100)) ;; w_3b = mesh failure penalty weight    (#L147)
           -4544              ;; w_4 = invalid messages weight 
           1       ;; w_5  = application-specific score weight         (#L43, global)
           (- -35 (/ 11 100))   ;; w_6  = IP-co-location factor weight (#L44, global)
           (- -15 (/ 92 100)))) ;; w_7 = behavioral penalty weight     (#L47, global)

(check= t (weightsp *eth-default-agg-subnet-weights*))

;; ;; https://github.com/silesiacoin/prysm-spike/blob/d5ac70f0406b445a276ee61ba10fdf0eb6aafa0f/beacon-chain/p2p/gossip_scoring_params.go#L162
;; (definecd scoreDecay(x :non-neg-rational) :non-neg-rational
;;   (b* (((when (= x 0)) 0)
;;        (numOfTimes (/ x *slot-duration*)))
;;     3/4))
;;     ;;(expt *decayToZero* (floor 1 numOfTimes))))

(defconst *eth-default-block-params*
  (params 4               ;; activationWindow    : #L89 aka MeshMessageDeliveriesActivation
	  *slot-duration* ;; meshTimeQuantum     : #L79
	  23              ;; p2cap               : #L83 aka FirstMessageDeliveriesCap
	  300             ;; timeQuantaInMeshCap : #L80
	  (* *blocks-per-epoch*
	     *decay-epoch*) ;; meshMessageDeliveriesCap : #L86
	  ;; meshMessageDeliveriesThreshold             : #L87
	  (/ (* *blocks-per-epoch* *decay-epoch*) 10)
	  *topicCap*        ;; topiccap          : #L39 (global)
	  -16000            ;; greyListThreshold : #L33 (global)
	  8                 ;; d                 : BC #L120
	  6                 ;; dlow              : BC #L119
	  12                ;; dhigh             : default in gossipsub.go
	  6                 ;; dlazy             : default in gossipsub.go
	  *hbmInterval* ;; hbmInterval (aka HeartbeatInterval) : BC #L118
	  60 ;; fanoutTTL (defaults to GossipSubFanoutTTL) : default in gossipsub.go
	  ;; mcacheLen (aka GossipSubHistoryLength) (see below)
	  (if *enable-larger-gossip-history* 12 6) ;; BC #L122, 130, 131
	  ;; mcacheGsp (aka GossipFactor) (defaults to GossipSubGossipFactor)
          ;; (see below)
	  (/ 1 1) ;; default in gossipsub.go
	  (* 500 *hbmInterval*) ;; seenTTL       : BC #L124
	  ;; https://github.com/silesiacoin/prysm-spike/blob/
	  ;; d5ac70f0406b445a276ee61ba10fdf0eb6aafa0f/beacon-chain/p2p/gossip_scoring_params.go#L35
	  5 ;; opportunisticGraftThreshold
	  ;; Let scoreDecay(x) = decayToZero^(1/(x/oneSlotDuration)).  Then, starting at:
	  ;;
	  ;; https://github.com/silesiacoin/prysm-spike/blob/d5ac70f0406b445a276ee61ba10fdf0eb6aafa0f/
	  ;;     beacon-chain/p2p/gossip_scoring_params.go#L73
	  ;;
	  *beaconBlockWeight*
	  ;; MeshMessageDeliveriesDecay = scoreDecay(decayEpoch * oneEpochDuration)
	  398/1000
	  ;; FirstMessageDeliveriesDecay = scoreDecay(20 * oneEpochDuration)
	  794/1000
	  63/100 ;; behaviourPenaltyDecay
	  ;;  MeshFailurePenaltyDecay = scoreDecay(decayEpoch * oneEpochDuration)
	  398/1000
	  ;; InvalidMessageDeliveriesDecay = scoreDecay(50 * oneEpochDuration)
	  912/1000
	  *decayToZero* ;; decayToZero
	  *slot-duration* ;; decayInterval
	  ))

(check= t (paramsp *eth-default-block-params*))

(defconst *eth-default-aggregate-params*
  (params (* 32 *slot-duration*) ;; activationWindow               : #L112
	  *slot-duration*        ;; meshTimeQuantum                : #L102
	  179                    ;; p2cap                          : #L106
	  300                    ;; timeQuantaInMeshCap            : #L103
	  *agg-per-epoch*        ;; meshMessageDeliveriesCap       : #L109
	  (/ *agg-per-epoch* 50) ;; meshMessageDeliveriesThreshold : #L110
	  *topicCap*             ;; topicCap,          see above (global)
	  -16000                 ;; greyListThreshold, see above (global)
	  8                      ;; d                                   : BC #L120
	  6                      ;; dlow                                : BC #L119
	  12                     ;; dhigh                               : default in gossipsub.go
	  6                      ;; dlazy                               : default in gossipsub.go
	  *hbmInterval*          ;; hbmInterval (aka HeartbeatInterval) : BC #L118
	  60                     ;; fanoutTTL (defaults to GossipSubFanoutTTL) : default in gossipsub.go
	  ;; mcacheLen (aka GossipSubHistoryLength) (see below)
	  (if *enable-larger-gossip-history* 12 6) ;; BC #L122, 130, 131
	  ;; mcacheGsp (aka GossipFactor) (defaults to GossipSubGossipFactor)
          ;; (see below)
          ;; TODO : clarify gossipfactor
	  (/ 1 1) ;; default in gossipsub.go
	  (* 500 *hbmInterval*) ;; seenTTL : BC #L124
	  5 ;; opportunisticGraftThreshold
	  *aggregateWeight* ;; topicWeight = aggregateWeight       #L100
	  1/100 ;; MeshMessageDeliveriesDecay  = scoreDecay(1 epoch) #L108
	  1/100 ;; FirstMessageDeliveriesDecay = scoreDecay(1 epoch) #L105
	  63/100 ;; behaviourPenaltyDecay
	  1/100 ;; MeshFailurePenaltyDecay     = scoreDecay(1 epoch) #L114
	  912/1000 ;; InvalidMessageDeliveriesDecay = scoreDecay(50 epochs) #L116
	  *decayToZero*
	  *slot-duration*
	  ))

(check= t (paramsp *eth-default-aggregate-params*))

(defconst *eth-default-aggregate-subnet-params*
  (params (* 17 *slot-duration*) ;; activationWindow               : #L146
	  *subnet-numsPerSlot*   ;; meshTimeQuantum                : #L136
	  24                     ;; p2cap                          : #L140
	  300                    ;; timeQuantaInMeshCap            : #L137
	  *subnet-subnetWeight*  ;; meshMessageDeliveriesCap       : #L143
	  *subnet-minimumWeight* ;; meshMessageDeliveriesThreshold : #L144
	   *topicCap*             ;; topicCap,          see above (global)
	  -16000                 ;; greyListThreshold, see above (global)
	  8                      ;; d                                   : BC #L120
	  6                      ;; dlow                                : BC #L119
	  12                     ;; dhigh                               : default in gossipsub.go
	  6                      ;; dlazy                               : default in gossipsub.go
	  *hbmInterval*          ;; hbmInterval (aka HeartbeatInterval) : BC #L118
	  60                     ;; fanoutTTL (defaults to GossipSubFanoutTTL) : default in gossipsub.go
	  ;; mcacheLen (aka GossipSubHistoryLength) (see below)
	  (if *enable-larger-gossip-history* 12 6) ;; BC #L122, 130, 131
	  ;; mcacheGsp (aka GossipFactor) (defaults to GossipSubGossipFactor)
          ;; (see below)
          ;; TODO
	  (/ 1 1) ;; default in gossipsub.go
	  (* 500 *hbmInterval*) ;; seenTTL : BC #L124
	  5 ;; OpportunisticGraftThreshold
	  ;; TODO: MeshMessageDeliveriesWindow   = 2 seconds                           #L145
	  *subnet-topicWeight*  ;; topicWeight   = *subnet-topicWeight*                      #L134
	  ;; MeshMessageDeliveriesDecay    = scoreDecay(*subnet-meshDecay* x 1 epoch)  #L142
	  749/1000
	  ;; FirstMessageDeliveriesDecay   = scoreDecay(*subnet-firstDecay* x 1 epoch) #L139
	  316/1000
	  631/1000 ;; behaviourPenaltyDecay
	  ;; MeshFailurePenaltyDecay       = scoreDecay(*subnet-meshDecay* x 1 epoch)  #L148
	  749/1000
	  ;; InvalidMessageDeliveriesDecay = scoreDecay(50 epochs)                     #L150
	  912/1000
	  *decayToZero*
	  *slot-duration*
	  ))

(check= t (paramsp *eth-default-aggregate-subnet-params*))

(defconst *eth-twp*
  `((AGG . (,*eth-default-agg-weights* . ,*eth-default-aggregate-params*))
    (BLOCKS . (,*eth-default-block-weights* . ,*eth-default-block-params*))
	;; We can have 0 or more subnet aggregator topics.  For now, let's assume 3.
    (SUB1 . (,*eth-default-agg-subnet-weights* . ,*eth-default-aggregate-subnet-params*))
    (SUB2 . (,*eth-default-agg-subnet-weights* . ,*eth-default-aggregate-subnet-params*))
    (SUB3 . (,*eth-default-agg-subnet-weights* . ,*eth-default-aggregate-subnet-params*))))

(check= t (twpp *eth-twp*))

;; ---------------------- PROPERTIES ------------------------------



;;high values
(defdata lows (range integer (0 <= _ <= 1)))

;;low values
(defdata highs (range integer (300 < _ <= 400)))

(defdata-subtype lows nat)
(defdata-subtype highs nat)

;; setting badbehaviour as 0
(defun nth-bad-counters-custom (n)
  (tctrs 0 ;;setting invalid msg deliveries to 0, as penalty is too high
         (nth-lows (+ n 3)) 
         42
         (nth-lows (+ n 4))
         0))
;; for our scenario, meshfailurepenalty must be 0 since it is only incremented
;; when peer leaves.

;; keeping bad behaviors 0 as it drops score too much
(defun nth-good-counters-custom (n)
  (tctrs 0 (nth-highs (+ n 2)) (nth-highs (+ n 3)) (nth-highs (+ n 4)) 0))

(defun nth-counters-custom (n)
  (if (== 0 (mod n 4))
      (nth-bad-counters-custom n)
    (nth-good-counters-custom n)))

(property nth-pt-tctrs-mapp (n :nat)
          :proofs? nil
          (pt-tctrs-mapp (nth-pt-tctrs-map n)))

(defdata-attach tctrs :enumerator nth-counters-custom)


(set-ignore-ok t)
(defun nth-glb-counters-custom (n)
  (declare (irrelevant n))
  (gctrs 0 0 0))

(property nth-p-gctrs-mapp (n :nat)
          :proofs? nil
          (p-gctrs-mapp (nth-p-gctrs-map n)))

(defdata-attach gctrs :enumerator nth-glb-counters-custom)



;; Event emission


;; I need skip-proofs because I can't prove:
;; (thm (=> (natp n) (symbolp (nth-symbol n))))
(skip-proofs
 (definecd emit-meshmsgdeliveries-peer-topic
   (p1 :peer p2 :peer top :topic n :nat) :loev
   (match n
     (0 '())
     (_ (let ((pld (payload-type (nth-symbol n) (nth-pid-type n) top p1)))
          (app (list (list p1 'SND p2 'PAYLOAD pld)
                     (list p2 'RCV p1 'PAYLOAD pld))
               (emit-meshmsgdeliveries-peer-topic p1 p2 top (1- n))))))))

(definecd emit-meshmsgdeliveries-peer-topics
  (p1 :peer p2 :peer ts :lot n :nat) :loev
  (match ts
    (() '())
    ((top . rst) (append (emit-meshmsgdeliveries-peer-topic p1 p2 top n)
                         (emit-meshmsgdeliveries-peer-topics p1 p2 rst n)))))

(property remove-topic (top :topic ts :lot)
          (lotp (remove-equal top ts)))

(skip-proofs
;; emit attack events from p1 to p2, in the attacked topic top
(definecd emit-evnts (p1 :peer p2 :peer ts :lot top :topic n elapsed :nat)
  :loev
  :function-contract-hints (("Goal" :in-theory (enable hbm-evntp evntp loevp)))
  (app (emit-meshmsgdeliveries-peer-topics p1 p2 (remove-equal top ts) n)
       (emit-meshmsgdeliveries-peer-topic p1 p2 top 1)
       `((,p2 HBM ,elapsed)))))


(skip-proofs
;; emit attack events from p1 to p2, in the attacked topic top
(definecd emit-evnts-ticks (p1 :peer p2 :peer ts :lot top :topic n elapsed
                               ticks :nat)
  :loev
  (app (emit-evnts p1 p2 ts top n elapsed)
       (if (<= (- ticks elapsed) 0)
           nil
         (emit-evnts-ticks p1 p2 ts top n elapsed (- ticks elapsed))))))


(emit-evnts-ticks 'A 'V '(AGG SUB2) 'AGG 3 20 100)

  
(definec construct-mesh (ps :lop ts :lot acc :topic-lop-map) :topic-lop-map
  (match ts
    (() acc)
    ((top . rst)
     (construct-mesh ps rst (mset top ps acc)))))

(definec min-mesh-msgs-for-pos-scores (twpm :twp mm :nat) :nat
  (match twpm
    (() mm)
    (((top . (wts . params)) . rst)
     (let ((m (params-meshMessageDeliveriesThreshold params)))
       (if (> m mm)
           (min-mesh-msgs-for-pos-scores rst m)
         (min-mesh-msgs-for-pos-scores rst mm))))))

(check= 10 (min-mesh-msgs-for-pos-scores *eth-twp* 0))

(definec max-hbm-interval (twpm :twp hbmint :non-neg-rational) :non-neg-rational
  (match twpm
    (() hbmint)
    (((top . (wts . params)) . rst)
     (let ((h (params-hbmInterval params)))
       (if (> h hbmint)
           (max-hbm-interval rst h)
         (max-hbm-interval rst hbmint))))))

(definec max-activationwindow (twpm :twp a :non-neg-rational) :non-neg-rational
  (match twpm
    (() a)
    (((top . (wts . params)) . rst)
     (let ((aw (params-activationWindow params)))
       (if (> aw a)
           (max-hbm-interval rst aw)
         (max-hbm-interval rst a))))))

(skip-proofs
(definec initialize-group-of-meshpeers (ps :lop mps :lop ts :lot d :nat) :group
  :skip-tests t
  (match ps
    (() '())
    ((p . rst)
     (let ((pmesh (construct-mesh (grab d (remove-equal p (app ps mps))) ts nil)))
       (mset p (peer-state (update-topic-mesh
                            (update-nbr-topicsubs
                             (new-nbr-topic-state)
                             pmesh)
                            pmesh)
                           (new-msgs-state)
                           '()
                           '()
                           '())
             (initialize-group-of-meshpeers rst mps ts d)))))))

(check= t (groupp (initialize-group-of-meshpeers '(A V) '(A V)
                                                 '(AGG BLOCKS SUB1 SUB2 SUB3)
                                                 8)))

(defdata lob (listof boolean))
(defdata lops (listof peer-state))

;; attacker peer p
(definec scorePropViolation (ps :peer-state p :peer top :topic) :boolean
  :skip-tests t
  :skip-body-contractsp t
  (^ (> (lookup-score p (calc-nbr-scores-map (peer-state-nbr-tctrs ps)
                                              (peer-state-nbr-gctrs ps)
                                              *eth-twp*))
         0)
      (< (calcScoreTopic (lookup-tctrs p top (peer-state-nbr-tctrs ps))
                          (mget top *eth-twp*))
         0)))

(definecd update-group-peer-edge (p1 :peer p2 :peer grp :group ts :lot) :group
  ;; update sub and mesh for each topic, for peer p1 
  (b* ((ps (lookup-state p1 grp))
       (nts (peer-state-nts ps))
       (psub (nbr-topic-state-nbr-topicsubs nts))
       (psubp (reduce* add-peer-subs psub ts p2))
       (pmesh (nbr-topic-state-topic-mesh nts))
       (pmeshp (reduce* add-peer-subs pmesh ts p2))
       (nts1 (update-nbr-topicsubs nts psubp))
       (nts2 (update-topic-mesh nts1 pmeshp)))
    (mset p1
          (mset :nts
                nts2
                ps)
          grp)))

(create-reduce* (lambda (e grp ts)
                  (update-group-peer-edge (cdr e)
                                          (car e)
                                          (update-group-peer-edge (car e)
                                                                  (cdr e)
                                                                  grp
                                                                  ts)
                                          ts))
                groupp
                graphp
                (:name graph->group)
                (:fixed-vars ((lotp ts))))

(check= t
        (groupp
         (update-group-peer-edge 'p1 'p3
                                 (initialize-group-of-meshpeers '(P1 P2 P100)
                                                                '(P1 P2 P100)
                                                                '(BLOCKS AGG SUB1 SUB2 SUB3)
                                                                2)
                                 '(BLOCKS AGG SUB1 SUB2 SUB3))))

(check= t
        (groupp
         (reduce* graph->group 
                  (initialize-group-of-meshpeers '()
                                                 '()
                                                 (topics)
                                                 ;; degree is whatever decided
                                                 ;; by graph, so let it be 100
                                                 100)
                  '((p1 . p3) (p3 . p5))
                  (topics))))

;; Optimized version of run-network, which only records violations
(skip-proofs
(definecd eth-attack-violations (gr :group evnts :loev p1 :peer p2 :peer top
                                    :topic i x :nat twpm :twp s :nat acc :lob) :lob
    :ic (is-valid-twp twpm)
    :skip-tests t
    :body-contracts-strictp nil
    (if (v (== x 2)
           (zp i)
           (endp evnts))
        (reverse acc)
      (b* (((mv k s) (defdata::genrandom-seed
                       (1- (expt 2 31))
                       (mod s (expt 2 31))))
           (actor (caar evnts))
           (actor-state (lookup-state actor gr))
           ((res4 next-actor-state evs) (transition actor actor-state (car evnts)
                                                    twpm k))
             (next-actor-events (network-propagator evs nil))
             (newgrp (mset actor next-actor-state gr))
             (x (if (== x 1) 2 0))
             (res (scorePropViolation (mget p2 newgrp) p1 top))
             (x (if (== res t) 1 x)))
          (eth-attack-violations
           newgrp
           ;;mix generated events with remaining events
           (app
            (cdr evnts)
            next-actor-events
            )
           p1 ;; attacker
           p2 ;; victim
           top
           (1- i)
           x
           twpm
           s
           (if (v (== x 1)
                  (== x 2))
               (cons newgrp acc)
             acc))))
    )
)


(time$
 (b* ((ticks 1000)
      (attacktopic 'SUB1)
      (d (mget :d (cdr (mget 'SUB1 *eth-twp*))))
      (grp (initialize-group-of-meshpeers '(A B)
                                          '(A B)
                                          (topics)
                                          100))
      (evnts (emit-evnts-ticks 'A 'B (topics) attacktopic 200 18 ticks)))
    (eth-attack-violations grp evnts 'A 'B attacktopic (* 10 (len evnts)) 0
                           *eth-twp* 42 nil)))







;; Optimized version of run-network, which only records violations
(skip-proofs
(definecd eth-attack-violations (gr :group evnts :loev p1 :peer p2 :peer top :topic i :nat twpm :twp s :nat acc :lob) :lob
    :ic (is-valid-twp twpm)
    :skip-tests t
    :body-contracts-strictp nil
    (if (v (zp i)
           (endp evnts))
        (reverse acc)
      (b* (((mv k s) (defdata::genrandom-seed
                       (1- (expt 2 31))
                       (mod s (expt 2 31))))
           (actor (caar evnts))
           (actor-state (lookup-state actor gr))
           (ev (car evnts))
           ((res4 next-actor-state evs) (transition actor actor-state ev
                                                    twpm k))
           (next-actor-events (network-propagator evs nil))
           (newgrp (mset actor next-actor-state gr)))
              (eth-attack-violations
               newgrp
               ;;mix generated events with remaining events
               (app
		(cdr evnts)
		next-actor-events
		)
	       p1 ;; attacker
               p2 ;; victim
	       top
               (1- i)
               twpm
               s
               (if (hbm-evntp ev)
                   (cons (scorePropViolation (mget p2 gr) p1 top) acc)
                 acc)))))
)

(time$
 (b* ((ticks 1000)
      (attacktopic 'SUB1)
      (d (mget :d (cdr (mget 'SUB1 *eth-twp*))))
      (grp (initialize-group-of-meshpeers '(A B)
                                          '(A B)
                                          (topics)
                                          100))
      (evnts (emit-evnts-ticks 'A 'B (topics) attacktopic 200 18 ticks)))
    (eth-attack-violations grp evnts 'A 'B attacktopic (* 10 (len evnts))
                           *eth-twp* 42 nil)))






;; attack on goerli network
;; 18.18 mins
;; 1091.38 seconds realtime, 1007.77 seconds runtime
; (2,818,138,307,424 bytes allocated).
(time$
 (b* ((ticks 100)
      (attacktopic 'SUB1)
      (d (mget :d (cdr (mget 'BLOCKS *eth-twp*))))
      (grp (reduce* graph->group 
                   (initialize-group-of-meshpeers '()
                                                  '()
                                                  (topics)
                                                  100)
                   *goerli*
                   (topics)))
      (evnts (emit-evnts-ticks 'P222 'P834 (topics) attacktopic 200 18 ticks)))
   ;; P222 is attacker, P834 is observer who records violations
   (eth-attack-violations grp evnts 'P222 'P834 attacktopic (* 10 (len evnts))
                          *eth-twp* 42 nil)))

;; attack on rinkeby network
; 1945.18 seconds realtime, 1848.42 seconds runtime
;; 32 mins
; (2,886,666,214,832 bytes allocated).
(time$
 (b* ((ticks 100)
      (d (mget :d (cdr (mget 'BLOCKS *eth-twp*))))
      (grp (reduce* graph->group 
                   (initialize-group-of-meshpeers '()
                                                  '()
                                                  (topics)
                                                  100)
                   *rinkeby*
                   '(BLOCKS AGG SUB1 SUB2 SUB3)))
      (evnts (grab 10000 (evnts-for-property1-attack '(P256) 'P436 '(BLOCKS AGG SUB1 SUB2 SUB3)
                                                     ticks *eth-twp* 'P256 'AGG))))
   ;; P256 is attacker, P436 is observer who records violations
   (eth-attack-violations grp evnts 'P436 'P256 'AGG (* 10 (len evnts))
                          *eth-twp* 42 nil)))


;; attack on ropsten network
; 474.83 seconds realtime, 435.23 seconds runtime
; 8 mins
; (1,141,741,823,040 bytes allocated).
(time$
 (b* ((ticks 100)
      (d (mget :d (cdr (mget 'BLOCKS *eth-twp*))))
      (grp (reduce* graph->group 
                   (initialize-group-of-meshpeers '()
                                                  '()
                                                  (topics)
                                                  100)
                   *ropsten*
                   '(BLOCKS AGG SUB1 SUB2 SUB3)))
      (evnts (grab 10000 (evnts-for-property1-attack '(P256) 'P436 '(BLOCKS AGG SUB1 SUB2 SUB3)
                                                     ticks *eth-twp* 'P256 'AGG))))
   ;; P256 is attacker, P436 is observer who records violations
   (eth-attack-violations grp evnts 'P436 'P256 'AGG (* 10 (len evnts))
                          *eth-twp* 42 nil)))


;; ------------------------------------------------------------------------------------
;; Counterexamples for Property 2
;; ------------------------------------------------------------------------------------


(defdata-attach tctrs :enumerator nth-good-counters-custom)

(property (ptc :pt-tctrs-map
	       pglb :p-gctrs-map
	       p :peer
	       top :topic
	       delta-p3 :non-neg-rational
	       delta-p3b :non-neg-rational
	       delta-p4 :non-neg-rational
	       delta-p6 :non-neg-rational
	       delta-p7 :non-neg-rational)
  :proofs? nil
  :testing-timeout 600
  :CHECK-CONTRACTS? nil
  :hyps (^ (member-equal top (strip-cars *eth-twp*))
	   (member-equal (cons p top) (strip-cars ptc))
	   ;; Require glb indexed by p in pglb to not be empty and thus not return nil
           (member-equal p (strip-cars pglb))
	   ;; Require that at least one delta is non-zero
	   (> (+ delta-p3 delta-p3b delta-p4 delta-p6 delta-p7) 0))
  ;; Define the new ptc using our deltas
  (b* ((tc (lookup-tctrs p top ptc))
       (glb (lookup-gctrs p pglb))
       (new-tc (update-meshMessageDeliveries
		  tc
		  (- (tctrs-meshMessageDeliveries tc) delta-p3)))
       (new-ptc (put-assoc-equal `(,p . ,top) new-tc ptc)))
    (> (lookup-score p (calc-nbr-scores-map ptc pglb *eth-twp*))
       (lookup-score p (calc-nbr-scores-map new-ptc pglb *eth-twp*)))))

;;effect due to purturbations alone : -751/60 = -12.52
(b* ((DELTA-P7 0)
     (DELTA-P6 1)
     (DELTA-P4 0)
     (DELTA-P3B 1)
     (DELTA-P3 1)
     (TOP 'SUB2)
     (P 'P9)
     (PGLB '((P9 0 0 0) (P10 0 0 0) (P11 0 0 0)))
     (PTC '(((P9 . BLOCKS)
             (:0TAG . TCTRS)
             (:FIRSTMESSAGEDELIVERIES . 170)
             (:INVALIDMESSAGEDELIVERIES . 0)
             (:MESHFAILUREPENALTY . 0)
             (:MESHMESSAGEDELIVERIES . 176)
             (:MESHTIME . 123))
            ((P9 . AGG)
             (:0TAG . TCTRS)
             (:FIRSTMESSAGEDELIVERIES . 117)
             (:INVALIDMESSAGEDELIVERIES . 0)
             (:MESHFAILUREPENALTY . 0)
             (:MESHMESSAGEDELIVERIES . 123)
             (:MESHTIME . 170))
            ((P9 . SUB1)
             (:0TAG . TCTRS)
             (:FIRSTMESSAGEDELIVERIES . 164)
             (:INVALIDMESSAGEDELIVERIES . 0)
             (:MESHFAILUREPENALTY . 0)
             (:MESHMESSAGEDELIVERIES . 170)
             (:MESHTIME . 117))
            ((P9 . SUB2)
             (:0TAG . TCTRS)
             (:FIRSTMESSAGEDELIVERIES . 111)
             (:INVALIDMESSAGEDELIVERIES . 0)
             (:MESHFAILUREPENALTY . 0)
             (:MESHMESSAGEDELIVERIES . 117)
             (:MESHTIME . 164))
            ((P9 . SUB3)
             (:0TAG . TCTRS)
             (:FIRSTMESSAGEDELIVERIES . 158)
             (:INVALIDMESSAGEDELIVERIES . 0)
             (:MESHFAILUREPENALTY . 0)
             (:MESHMESSAGEDELIVERIES . 164)
             (:MESHTIME . 111))))
     (tc (lookup-tctrs p top ptc))
     (glb (lookup-gctrs p pglb))
     (new-tc (update-invalidMessageDeliveries
              (update-meshFailurePenalty
               (update-meshMessageDeliveries
                tc
                (+ (tctrs-meshMessageDeliveries tc) delta-p3))
               (+ (tctrs-meshFailurePenalty tc) delta-p3b))
              (+ (tctrs-invalidMessageDeliveries tc) delta-p4)))
     (new-ptc (put-assoc-equal `(,p . ,top) new-tc ptc))
     (new-glb-counters `(,(first glb)
                         ,(+ (second glb) delta-p6)
                         ,(+ (third glb) delta-p7)))
     (new-pglb (put-assoc-equal p new-glb-counters pglb)))
  (list (calcScoreTopic (lookup-tctrs p 'BLOCKS ptc)
                        (cdr (assoc-equal 'BLOCKS *ETH-TWP*)))
        (calcScoreTopic (lookup-tctrs p 'AGG ptc)
                        (cdr (assoc-equal 'AGG *ETH-TWP*)))
        (calcScoreTopic (lookup-tctrs p 'SUB1 ptc)
                        (cdr (assoc-equal 'SUB1 *ETH-TWP*)))
        (calcScoreTopic (lookup-tctrs p 'SUB2 ptc)
                        (cdr (assoc-equal 'SUB2 *ETH-TWP*)))
        (calcScoreTopic (lookup-tctrs p 'SUB3 ptc)
                        (cdr (assoc-equal 'SUB3 *ETH-TWP*)))
        (calcScoreTopic (lookup-tctrs p 'SUB2 new-ptc)
                        (cdr (assoc-equal 'SUB2 *ETH-TWP*)))
        (lookup-score p (calc-nbr-scores-map ptc pglb *eth-twp*))
        (lookup-score p (calc-nbr-scores-map new-ptc new-pglb *eth-twp*))))

;; 21.59 10.24 7.77 7.82 7.76 -4.70

;; (67463/3125 5121/500
;;             194159/25000 48857/6250 193997/25000
;;             -176233/37500 818/25 818/25)



;; ------------------------------------------------------------------------------------
;; Counterexamples for Property 3
;; ------------------------------------------------------------------------------------

(property
   (imd mmd mt fmd mfp p :non-neg-rational wtpm :wp)
   :check-contracts? nil
   :proofs? nil
   (=> (^ (== wtpm (cdr (assoc-equal 'BLOCKS *ETH-TWP*)))
          (>= (params-meshMessageDeliveriesCap (cdr wtpm))
              (params-meshMessageDeliveriesThreshold (cdr wtpm)))
          (> mt (params-activationWindow (cdr wtpm))))
       (>= (calcScoreTopic (tctrs imd mmd  (+ p mt) fmd mfp) wtpm)
           (calcScoreTopic (tctrs imd mmd  mt fmd mfp) wtpm))))

(property
   (imd mmd mt fmd mfp p :non-neg-rational wtpm :wp)
   :check-contracts? nil
   :proofs? nil
   (=> (^ (== wtpm (cdr (assoc-equal 'AGG *ETH-TWP*)))
          (>= (params-meshMessageDeliveriesCap (cdr wtpm))
              (params-meshMessageDeliveriesThreshold (cdr wtpm)))
          (> mt (params-activationWindow (cdr wtpm))))
       (>= (calcScoreTopic (tctrs imd mmd  (+ p mt) fmd mfp) wtpm)
           (calcScoreTopic (tctrs imd mmd  mt fmd mfp) wtpm))))

(property
   (imd mmd mt fmd mfp p :non-neg-rational wtpm :wp)
   :check-contracts? nil
   :proofs? nil
   (=> (^ (== wtpm (cdr (assoc-equal 'SUB2 *ETH-TWP*)))
          (>= (params-meshMessageDeliveriesCap (cdr wtpm))
              (params-meshMessageDeliveriesThreshold (cdr wtpm)))
          (> mt (params-activationWindow (cdr wtpm))))
       (>= (calcScoreTopic (tctrs imd mmd  (+ p mt) fmd mfp) wtpm)
           (calcScoreTopic (tctrs imd mmd  mt fmd mfp) wtpm))))
