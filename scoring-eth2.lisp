(in-package "ACL2S")
(in-theory (enable remove-assoc-equal))
(include-book "higher-order")
(include-book "utils")

(def-const *few-seconds* 5)
(def-const *two-minutes* 120)

(include-book "network")
(include-book "utilities")

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
  (list (/ 324 10000)  ;; w_1  = time in mesh weight                (#L78)
	1              ;; w_2  = first message deliveries weight    (#L81)
	(/ -717 1000)  ;; w_3  = mesh message deliveries weight     (#L84)
	(/ -717 1000)  ;; w_3b = mesh failure penalty weight        (#L90)
        (- -140 (/ 4475 10000)) ;; w_4  = invalid messages weight   (#L92)
	1       ;; w_5  = application-specific score weight         (#L43, global)
        (- -35 (/ 11 100))   ;; w_6  = IP-co-location factor weight (#L44, global)
	(- -15 (/ 92 100)))) ;; w_7 = behavioral penalty weight     (#L47, global)

(check= t (weightsp *eth-default-block-weights*))

(defconst *eth-default-agg-weights*
  (list (/ 324 10000) ;; w_1  = time in mesh weight              (#L101)
	(/ 128 1000)  ;; w_2  = first message deliveries weight  (#L104)
	(/ -64 1000)  ;; w_3  = mesh message deliveries weight   (#L107)
	(/ -64 1000)  ;; w_3b = mesh failure penalty weight      (#L113)
	(- -140 (/ 4475 10000)) ;; w_4 = invalid messages weight (#L115)
	1             ;; w_5 = application-specific score weight    (#L43, global)
	(- -35 (/ 11 100))   ;; w_6  = IP-co-location factor weight (#L44, global)
	(- -15 (/ 92 100)))) ;; w_7 = behavioral penalty weight     (#L47, global)

(check= t (weightsp *eth-default-agg-weights*))

(defconst *eth-default-agg-subnet-weights*
  (list (/ 324 10000) ;; w_1 = time in mesh weight                  (#L134)
	(/ 955 1000)  ;; w_2 = first message deliveries weight      (#L138)
	(- -37 (/ 55 100)) ;; w_3  = mesh message deliveries weight (#L141)
	(- -37 (/ 55 100)) ;; w_3b = mesh failure penalty weight    (#L147)
	-4544              ;; w_4 = invalid messages weight 
        1       ;; w_5  = application-specific score weight         (#L43, global)
        (- -35 (/ 11 100))   ;; w_6  = IP-co-location factor weight (#L44, global)
	(- -15 (/ 92 100)))) ;; w_7 = behavioral penalty weight     (#L47, global)

(check= t (weightsp *eth-default-agg-subnet-weights*))

;; https://github.com/silesiacoin/prysm-spike/blob/d5ac70f0406b445a276ee61ba10fdf0eb6aafa0f/beacon-chain/p2p/gossip_scoring_params.go#L162
(definecd scoreDecay(x :non-neg-rational) :non-neg-rational
  (b* (((when (= x 0)) 0)
       (numOfTimes (/ x *slot-duration*)))
    (expt *decayToZero* (floor 1 numOfTimes))))

;; Currently failing ...
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
	  (scoreDecay (* *decay-epoch* *one-epoch-duration*))
	  ;; FirstMessageDeliveriesDecay = scoreDecay(20 * oneEpochDuration)
	  (scoreDecay (* 20 *one-epoch-duration*)) 
	  (scoreDecay (* 10 *one-epoch-duration*)) ;; behaviourPenaltyDecay
	  ;;  MeshFailurePenaltyDecay = scoreDecay(decayEpoch * oneEpochDuration)
	  (scoreDecay (* *decay-epoch* *one-epoch-duration*))
	  ;; InvalidMessageDeliveriesDecay = scoreDecay(50 * oneEpochDuration)
	  (scoreDecay (* 50 *one-epoch-duration*))
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
	  (scoreDecay *one-epoch-duration*) ;; MeshMessageDeliveriesDecay  = scoreDecay(1 epoch) #L108
	  (scoreDecay *one-epoch-duration*) ;; FirstMessageDeliveriesDecay = scoreDecay(1 epoch) #L105
	  (scoreDecay (* 10 *one-epoch-duration*)) ;; behaviourPenaltyDecay
	  (scoreDecay *one-epoch-duration*) ;; MeshFailurePenaltyDecay     = scoreDecay(1 epoch) #L114
	  (scoreDecay (* 50 *one-epoch-duration*)) ;; InvalidMessageDeliveriesDecay = scoreDecay(50 epochs) #L116
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
	  (scoreDecay (* *subnet-meshDecay* *one-epoch-duration*))
	  ;; FirstMessageDeliveriesDecay   = scoreDecay(*subnet-firstDecay* x 1 epoch) #L139
	  (scoreDecay (* *subnet-firstDecay* *one-epoch-duration*))
	  (scoreDecay (* 10 *one-epoch-duration*)) ;; behaviourPenaltyDecay
	  ;; MeshFailurePenaltyDecay       = scoreDecay(*subnet-meshDecay* x 1 epoch)  #L148
	  (scoreDecay (* *subnet-meshDecay* *one-epoch-duration*))
	  ;; InvalidMessageDeliveriesDecay = scoreDecay(50 epochs)                     #L150
	  (scoreDecay (* 50 *one-epoch-duration*))
	  *decayToZero*
	  *slot-duration*
	  ))

(check= t (paramsp *eth-default-aggregate-subnet-params*))

(defconst *eth-twp*
  (list (cons 'BLOCKS (cons *eth-default-block-weights*
			    *eth-default-block-params*))
	(cons 'AGG (cons *eth-default-agg-weights*
			 *eth-default-aggregate-params*))
	;; We can have 0 or more subnet aggregator topics.  For now, let's assume 3.
	(cons 'SUB1 (cons *eth-default-agg-subnet-weights*
		  	  *eth-default-aggregate-subnet-params*))
	(cons 'SUB2 (cons *eth-default-agg-subnet-weights*
			  *eth-default-aggregate-subnet-params*))
	(cons 'SUB3 (cons *eth-default-agg-subnet-weights*
			 *eth-default-aggregate-subnet-params*))))

(check= t (twpp *eth-twp*))

;; ---------------------- PROPERTIES ------------------------------



;;high values
(defdata lows (range integer (0 <= _ <= 1)))

;;low values
(defdata highs (range integer (100 < _ <= 200)))

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
  (if (== 0 (mod n 2))
      (nth-bad-counters-custom n)
    (nth-good-counters-custom n)))

(defdata-attach tctrs :enumerator nth-counters-custom)

(set-ignore-ok t)
(defun nth-glb-counters-custom (n)
  (declare (irrelevant n))
  (list 0 0 0))

(defdata-attach gctrs :enumerator nth-glb-counters-custom)



;; Properties

;; ------------------------------------------------
;; Some interesting properties about ETH-2.0
;; ------------------------------------------------

;;Property 1

(property (ptc :pt-tctrs-map pcm :p-gctrs-map p :peer top :topic)
	  :proofs? nil
          :TESTING-TIMEOUT 600
	  :hyps  (^ (member-equal top (strip-cars *eth-twp*))
	      (member-equal (cons p top) (strip-cars ptc))
	      (> (lookup-score p (calc-nbr-scores-map ptc pcm *eth-twp*)) 0))
	   (> (calcScoreTopic (lookup-tctrs p top ptc) (cdr (assoc-equal top *eth-twp*)))
             0))

;;counterexample to property 1
;; even with no app-specific score, we get positive overall score due to other
;; topic scores
(b* ((TOP 'AGG)
 (P 'P1)
 (PCM '((P4 0 0 0) (P5 0 0 0) (P6 0 0 0)))
 (PTC '(((P0 . BLOCKS)
         (:0TAG . TCTRS)
         (:FIRSTMESSAGEDELIVERIES . 0)
         (:INVALIDMESSAGEDELIVERIES . 0)
         (:MESHFAILUREPENALTY . 0)
         (:MESHMESSAGEDELIVERIES . 1)
         (:MESHTIME . 42))
        ((P0 . AGG)
         (:0TAG . TCTRS)
         (:FIRSTMESSAGEDELIVERIES . 136)
         (:INVALIDMESSAGEDELIVERIES . 0)
         (:MESHFAILUREPENALTY . 0)
         (:MESHMESSAGEDELIVERIES . 142)
         (:MESHTIME . 189))
        ((P0 . SUB1)
         (:0TAG . TCTRS)
         (:FIRSTMESSAGEDELIVERIES . 0)
         (:INVALIDMESSAGEDELIVERIES . 0)
         (:MESHFAILUREPENALTY . 0)
         (:MESHMESSAGEDELIVERIES . 1)
         (:MESHTIME . 42))
        ((P0 . SUB2)
         (:0TAG . TCTRS)
         (:FIRSTMESSAGEDELIVERIES . 130)
         (:INVALIDMESSAGEDELIVERIES . 0)
         (:MESHFAILUREPENALTY . 0)
         (:MESHMESSAGEDELIVERIES . 136)
         (:MESHTIME . 183))
        ((P0 . SUB3)
         (:0TAG . TCTRS)
         (:FIRSTMESSAGEDELIVERIES . 0)
         (:INVALIDMESSAGEDELIVERIES . 0)
         (:MESHFAILUREPENALTY . 0)
         (:MESHMESSAGEDELIVERIES . 1)
         (:MESHTIME . 42))
        ((P1 . BLOCKS)
         (:0TAG . TCTRS)
         (:FIRSTMESSAGEDELIVERIES . 194)
         (:INVALIDMESSAGEDELIVERIES . 0)
         (:MESHFAILUREPENALTY . 0)
         (:MESHMESSAGEDELIVERIES . 200)
         (:MESHTIME . 147))
        ((P1 . AGG)
         (:0TAG . TCTRS)
         (:FIRSTMESSAGEDELIVERIES . 0)
         (:INVALIDMESSAGEDELIVERIES . 0)
         (:MESHFAILUREPENALTY . 0)
         (:MESHMESSAGEDELIVERIES . 1)
         (:MESHTIME . 42))
        ((P1 . SUB1)
         (:0TAG . TCTRS)
         (:FIRSTMESSAGEDELIVERIES . 188)
         (:INVALIDMESSAGEDELIVERIES . 0)
         (:MESHFAILUREPENALTY . 0)
         (:MESHMESSAGEDELIVERIES . 194)
         (:MESHTIME . 141))
        ((P1 . SUB2)
         (:0TAG . TCTRS)
         (:FIRSTMESSAGEDELIVERIES . 0)
         (:INVALIDMESSAGEDELIVERIES . 0)
         (:MESHFAILUREPENALTY . 0)
         (:MESHMESSAGEDELIVERIES . 1)
         (:MESHTIME . 42))
        ((P1 . SUB3)
         (:0TAG . TCTRS)
         (:FIRSTMESSAGEDELIVERIES . 182)
         (:INVALIDMESSAGEDELIVERIES . 0)
         (:MESHFAILUREPENALTY . 0)
         (:MESHMESSAGEDELIVERIES . 188)
         (:MESHTIME . 135))
        ((P2 . BLOCKS)
         (:0TAG . TCTRS)
         (:FIRSTMESSAGEDELIVERIES . 164)
         (:INVALIDMESSAGEDELIVERIES . 0)
         (:MESHFAILUREPENALTY . 0)
         (:MESHMESSAGEDELIVERIES . 170)
         (:MESHTIME . 117))
        ((P2 . AGG)
         (:0TAG . TCTRS)
         (:FIRSTMESSAGEDELIVERIES . 0)
         (:INVALIDMESSAGEDELIVERIES . 0)
         (:MESHFAILUREPENALTY . 0)
         (:MESHMESSAGEDELIVERIES . 1)
         (:MESHTIME . 42))
        ((P2 . SUB1)
         (:0TAG . TCTRS)
         (:FIRSTMESSAGEDELIVERIES . 158)
         (:INVALIDMESSAGEDELIVERIES . 0)
         (:MESHFAILUREPENALTY . 0)
         (:MESHMESSAGEDELIVERIES . 164)
         (:MESHTIME . 111))
        ((P2 . SUB2)
         (:0TAG . TCTRS)
         (:FIRSTMESSAGEDELIVERIES . 0)
         (:INVALIDMESSAGEDELIVERIES . 0)
         (:MESHFAILUREPENALTY . 0)
         (:MESHMESSAGEDELIVERIES . 1)
         (:MESHTIME . 42))
        ((P2 . SUB3)
         (:0TAG . TCTRS)
         (:FIRSTMESSAGEDELIVERIES . 152)
         (:INVALIDMESSAGEDELIVERIES . 0)
         (:MESHFAILUREPENALTY . 0)
         (:MESHMESSAGEDELIVERIES . 158)
         (:MESHTIME . 105)))))

  (list
   ;(calc-nbr-scores-map ptc pcm *eth-twp*)
        (calcScoreTopic (lookup-tctrs p 'BLOCKS ptc)
                        (cdr (assoc-equal 'BLOCKS *ETH-TWP*)))
        (calcScoreTopic (lookup-tctrs p 'AGG ptc)
                        (cdr (assoc-equal 'AGG *ETH-TWP*)))
        (calcScoreTopic (lookup-tctrs p 'SUB1 ptc)
                        (cdr (assoc-equal 'SUB1 *ETH-TWP*)))
        (calcScoreTopic (lookup-tctrs p 'SUB2 ptc)
                        (cdr (assoc-equal 'SUB2 *ETH-TWP*)))
        (calcScoreTopic (lookup-tctrs p 'SUB3 ptc)
                        (cdr (assoc-equal 'SUB3 *ETH-TWP*)))))
;; P1, AGG
;; 8.29
;; 22.21, -4.5, 7.80, -25, 7.78



;; We will now generate the list of events that led to the above
;; counter-example for P5



;; Property 2

(b* ((TOP 'AGG)
 (P 'P1)
 (PCM '((P4 0 0 0) (P5 0 0 0) (P6 0 0 0)))
 (PTC '(((P1 . BLOCKS)
         (:0TAG . TCTRS)
         (:FIRSTMESSAGEDELIVERIES . 100)
         (:INVALIDMESSAGEDELIVERIES . 0)
         (:MESHFAILUREPENALTY . 0)
         (:MESHMESSAGEDELIVERIES . 100)
         (:MESHTIME . 147))
        ((P1 . AGG)
         (:0TAG . TCTRS)
         (:FIRSTMESSAGEDELIVERIES . 194)
         (:INVALIDMESSAGEDELIVERIES . 0)
         (:MESHFAILUREPENALTY . 0)
         (:MESHMESSAGEDELIVERIES . 200)
         (:MESHTIME . 147))
        ((P1 . SUB1)
         (:0TAG . TCTRS)
         (:FIRSTMESSAGEDELIVERIES . 188)
         (:INVALIDMESSAGEDELIVERIES . 0)
         (:MESHFAILUREPENALTY . 0)
         (:MESHMESSAGEDELIVERIES . 194)
         (:MESHTIME . 141))
        ((P1 . SUB2)
         (:0TAG . TCTRS)
         (:FIRSTMESSAGEDELIVERIES . 194)
         (:INVALIDMESSAGEDELIVERIES . 0)
         (:MESHFAILUREPENALTY . 0)
         (:MESHMESSAGEDELIVERIES . 200)
         (:MESHTIME . 147))
        ((P1 . SUB3)
         (:0TAG . TCTRS)
         (:FIRSTMESSAGEDELIVERIES . 182)
         (:INVALIDMESSAGEDELIVERIES . 0)
         (:MESHFAILUREPENALTY . 0)
         (:MESHMESSAGEDELIVERIES . 188)
         (:MESHTIME . 135)))))
  (list
   (calc-nbr-scores-map ptc pcm *eth-twp*)
        (calcScoreTopic (lookup-tctrs p 'BLOCKS ptc)
                        (cdr (assoc-equal 'BLOCKS *ETH-TWP*)))
        (calcScoreTopic (lookup-tctrs p 'AGG ptc)
                        (cdr (assoc-equal 'AGG *ETH-TWP*)))
        (calcScoreTopic (lookup-tctrs p 'SUB1 ptc)
                        (cdr (assoc-equal 'SUB1 *ETH-TWP*)))
        (calcScoreTopic (lookup-tctrs p 'SUB2 ptc)
                        (cdr (assoc-equal 'SUB2 *ETH-TWP*)))
        (calcScoreTopic (lookup-tctrs p 'SUB3 ptc)
                        (cdr (assoc-equal 'SUB3 *ETH-TWP*)))))






(skip-proofs
 (definecd emit-meshmsgdeliveries-peer-topic
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


;; ps is the list of all peers, except observer. p2 is the observer.
;; ts is list of all topics, n is number of messages sent by peers in ps to p2.
(skip-proofs
(definec emit-evnts (ps :lop p2 :peer ts :lot n :nat) :loev
  (match ts
    (() '())
    ((top . rst)
     (app (emit-meshmsgdeliveries-topic ps p2 top n)
          (emit-evnts ps p2 rst n))))))


(emit-evnts '(P1 P2) 'P100 '(SUB2) 3)



(skip-proofs
(definec emit-evnts-ptc (ps :lop p2 :peer ts :lot maxT :pos ticks :pos n :nat
                            limiter :bool targetpeer :peer targettop :topic
                            targetn :nat) :loev
  :skip-tests t
  (if (<= ticks 0)
      '()
    (app (if limiter
             (app (emit-evnts (remove-equal targetpeer ps) p2 ts n)
                  (emit-evnts ps p2 (remove-equal targettop ts) n)
                  (emit-evnts `(,targetpeer) p2 `(,targettop) targetn)
                  `((,p2 HBM ,maxT)))
           (app (emit-evnts ps p2 ts n)
                `((,p2 HBM ,maxT))))
         (emit-evnts-ptc ps p2 ts maxT (- ticks maxT) n limiter targetpeer
                         targettop targetn)))))


(app (emit-evnts-ptc '(p1 p2) 'p100 '(BLOCKS AGG) 17 34 2 nil nil nil 1)
     (emit-evnts-ptc '(p1 p2) 'p100 '(BLOCKS AGG) 17 34 2 t 'p1 'BLOCKS 1))


(definec construct-mesh (p2 :peer ps :lop ts :lot) :topic-lop-map
  (match ts
    (() '())
    ((top . rst) (cons `(,top . ,ps)
                       (construct-mesh p2 ps rst)))))


(definec min-mesh-msgs-for-pos-scores (twpm :twp mm :nat) :nat
  (match twpm
    (() mm)
    (((top . (wts . params)) . rst)
     (let ((m (params-meshMessageDeliveriesThreshold params)))
       (if (> m mm)
           (min-mesh-msgs-for-pos-scores rst m)
         (min-mesh-msgs-for-pos-scores rst mm))))))

(definec max-hbm-interval (twpm :twp hbmint :non-neg-rational) :non-neg-rational
  (match twpm
    (() hbmint)
    (((top . (wts . params)) . rst)
     (let ((h (params-hbmInterval params)))
       (if (> h hbmint)
           (max-hbm-interval rst h)
         (max-hbm-interval rst hbmint))))))

(skip-proofs
(definec max-activationwindow (twpm :twp a :non-neg-rational) :non-neg-rational
  (match twpm
    (() a)
    (((top . (wts . params)) . rst)
     (let ((aw (params-activationWindow params)))
       (if (> aw a)
           (max-hbm-interval rst aw)
         (max-hbm-interval rst a)))))))

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
                                 '()
                                 '()
                                 '()))
             (initialize-group-of-meshpeers rst mps ts d)))))))

(definec evnts-for-pos-scores (ps :lop p2 :peer ts :lot ticks :pos twpm :twp) :loev
  :skip-body-contractsp t
  (if (null twpm)
      '()
    (app
     (emit-evnts-ptc ps p2 ts (max-hbm-interval twpm 0)
                     (1+ (max-activationwindow twpm 0))
                     (min-mesh-msgs-for-pos-scores twpm 0) nil nil nil 0) ;;first run
     ;;for max activation window to warm up the network
     (emit-evnts-ptc ps p2 ts (max-hbm-interval twpm 0)
                     ticks
                     (min-mesh-msgs-for-pos-scores twpm 0) nil nil nil 0)
     ;;actual run
     )))


;; (len (evnts-for-pos-scores '(P1 P2) 'P100 '(SUB2) 4 *eth-twp*))
;; (b* ((ticks 1000)
;;      (grp (initialize-group-of-meshpeers '(P1 P2 P100)
;; 					 '(P1 P2 P100)
;; 					 '(BLOCKS AGG SUB1 SUB2 SUB3)
;;                                          8))
;;      (evnts (evnts-for-pos-scores '(P1 P2) 'P100 '(BLOCKS AGG SUB1 SUB2 SUB3) ticks *eth-twp*)))
;;   (run-network-gr grp evnts (len evnts) *eth-twp* 42))


(skip-proofs
(definec evnts-for-property1-attack (ps :lop p2 :peer ts :lot ticks :pos twpm :twp
  targetpeer :peer targettop :topic) :loev
  (if (null twpm)
      '()
    (b* ((prms (cddr (assoc-equal targettop twpm)))
         ((when (null prms)) nil)
         (pmmd (max 0 (- (params-meshMessageDeliveriesThreshold prms) 1))))
      (app
       ;; for warmup + ticks/2 time, run to generate positive scores
       ;; (evnts-for-pos-scores ps p2 ts (1+ (floor ticks 2)) twpm)
       ;; then start throttling targeted (peer . top) mesh deliveries
       (emit-evnts-ptc ps p2 ts (max-hbm-interval twpm 0)
                       ticks
                       (* 5 (min-mesh-msgs-for-pos-scores twpm 0))
                       t ;;attack
                       targetpeer
                       targettop
                       pmmd))))))



;; (b* ((ticks 19)
;;      (grp (initialize-group-of-meshpeers '(P1 P2 P100)
;; 					 '(P1 P2 P100)
;; 					 '(BLOCKS AGG SUB1 SUB2 SUB3)
;;                                          2))
;;      (evnts (evnts-for-property1-attack '(P1 P2) 'P100 '(BLOCKS AGG SUB1 SUB2 SUB3)
;;                                         ticks *eth-twp* 'P1 'SUB2)))
;;   (run-network-gr grp evnts (* 2 (len evnts)) *eth-twp* 42))


;; Functions to check whether property 1 is actually being violated

(defdata lob (listof boolean))
(defdata lops (listof peer-state))

(definec ps-reqs (pss :lops p :peer top :topic) :boolean
  (match pss
    (() t)
    ((ps . rst) (^ (member-equal top (strip-cars *eth-twp*))
		   (member-equal (cons p top) (strip-cars (peer-state-nbr-tctrs ps)))
		   (> (lookup-score p (calc-nbr-scores-map (peer-state-nbr-tctrs ps)
							   (peer-state-nbr-gctrs ps)
							   *eth-twp*))
		      0)
		   (ps-reqs rst p top)))))

;; attacker peer p
(definec scorePropViolation (ps :peer-state p :peer top :topic) :boolean
  :skip-tests t
  :skip-body-contractsp t
  (^ (> (lookup-score p (calc-nbr-scores-map (peer-state-nbr-tctrs ps)
                                              (peer-state-nbr-gctrs ps)
                                              *eth-twp*))
         0)
      (< (calcScoreTopic (lookup-tctrs p top (peer-state-nbr-tctrs ps))
                          (cdr (assoc-equal top *eth-twp*)))
         0)))

;; observer peer p
(skip-proofs
(definec egl->pss (egl :egl p :peer) :lops
  (match egl
    (() '())
    (((evnt . grp) . rst)
     (let ((ps (cdr (assoc-equal p grp))))
       (cons (if (null ps)
                 (new-peer-state)
               ps)
             (egl->pss rst p)))))))

(definec scorePropViolations (pss :lops p :peer top :topic) :lob
  (match pss
    (() '())
    ((ps . rst) (cons (scorePropViolation ps p top)
                      (scorePropViolations rst p top)))))

(definec scorePropViolationTopic (ps :peer-state p :peer top :topic) :all
  :skip-tests t
  :skip-body-contractsp t
  (list (calcScoreTopic (lookup-tctrs p 'BLOCKS (peer-state-nbr-tctrs ps))
                        (cdr (assoc-equal top *eth-twp*)))
        (calcScoreTopic (lookup-tctrs p 'AGG (peer-state-nbr-tctrs ps))
                        (cdr (assoc-equal top *eth-twp*)))
        (calcScoreTopic (lookup-tctrs p 'SUB1 (peer-state-nbr-tctrs ps))
                        (cdr (assoc-equal top *eth-twp*)))
        (calcScoreTopic (lookup-tctrs p 'SUB2 (peer-state-nbr-tctrs ps))
                        (cdr (assoc-equal top *eth-twp*)))
        (calcScoreTopic (lookup-tctrs p 'SUB3 (peer-state-nbr-tctrs ps))
                        (cdr (assoc-equal top *eth-twp*)))
        (lookup-score p (calc-nbr-scores-map (peer-state-nbr-tctrs ps)
                                              (peer-state-nbr-gctrs ps)
                                              *eth-twp*))))


(definec scorePropViolationTopics (pss :lops p :peer top :topic) :all
  (match pss
    (() '())
    ((ps . rst) (cons (scorePropViolationTopic ps p top)
                      (scorePropViolationTopics rst p top)))))


;; ------------------------------------------------------------------------------------
;; Attack for Property 1, with a single target peer. T in trace shows a
;; property violation
;; ------------------------------------------------------------------------------------

;; Simple case with just 3 peers, all connected in all meshes
;; increase ticks for longer durations

;; Debug
(b* ((ticks 19)
     (grp (initialize-group-of-meshpeers '(P1 P2 P100)
					 '(P1 P2 P100)
					 '(BLOCKS AGG SUB1 SUB2 SUB3)
                                         8))
     (evnts (evnts-for-property1-attack '(P1 P2) 'P100 '(BLOCKS AGG SUB1 SUB2 SUB3)
                                        ticks *eth-twp* 'P1 'AGG)))
 (run-network-gr grp evnts (* (len evnts) 10)
                                              *eth-twp* 42))



;; (b* ((ticks 100)
;;      (grp (initialize-group-of-meshpeers '(P1 P2 P100)
;; 					 '(P1 P2 P100)
;; 					 '(BLOCKS AGG SUB1 SUB2 SUB3)
;;                                          2))
;;      (evnts (evnts-for-property1-attack '(P1 P2) 'P100 '(BLOCKS AGG SUB1 SUB2 SUB3)
;;                                         ticks *eth-twp* 'P1 'SUB2)))
;;   (scorePropViolationTopics (egl->pss (run-network grp evnts (* 10 (len evnts))
;;                                               *eth-twp* 42)
;;                                  'P100)
;;                        'P1
;;                        'SUB2))



(b* ((ticks 20)
     (grp (initialize-group-of-meshpeers '(P1 P2 P100)
					 '(P1 P2 P100)
					 '(BLOCKS AGG SUB1 SUB2 SUB3)
                                         2))
     (evnts (evnts-for-property1-attack '(P1 P2) 'P100 '(BLOCKS AGG SUB1 SUB2 SUB3)
                                        ticks *eth-twp* 'P1 'AGG)))
  (scorePropViolationTopics (egl->pss (run-network grp evnts (* 10 (len evnts))
                                              *eth-twp* 42)
                                 'P100)
                       'P1
                       'AGG))



;; Shows T, meaning property violated in trace of booleans
(b* ((ticks 100)
     (grp (initialize-group-of-meshpeers '(P1 P2 P100)
					 '(P1 P2 P100)
					 '(BLOCKS AGG SUB1 SUB2 SUB3)
                                         8))
     (evnts (evnts-for-property1-attack '(P1 P2) 'P100 '(BLOCKS AGG SUB1 SUB2 SUB3)
                                        ticks *eth-twp* 'P1 'AGG)))
  (scorePropViolations (egl->pss (run-network grp evnts (* 10 (len evnts))
                                              *eth-twp* 42)
                                 'P100)
                       'P1
                       'AGG))



;; ;; List of events in the trace of our attack
;; (b* ((ticks 100)
;;      (grp (initialize-group-of-meshpeers '(P1 P2 P100)
;; 					 '(P1 P2 P100)
;; 					 '(BLOCKS AGG SUB1 SUB2 SUB3)
;;                                          8))
;;      (evnts (evnts-for-property1-attack '(P1 P2) 'P100 '(BLOCKS AGG SUB1 SUB2 SUB3)
;;                                         ticks *eth-twp* 'P1 'SUB2)))
;;   (strip-cars (run-network grp evnts (* 10 (len evnts))
;;                            *eth-twp* 42)))





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


