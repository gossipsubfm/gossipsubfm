(in-package "ACL2S")
(in-theory (enable remove-assoc-equal))
(include-book "higher-order")
(include-book "utils")

(def-const *few-seconds* 5)
(def-const *two-minutes* 120)


(defdata peer symbol)
(defdata lop (listof peer))

(defdata neg-rat (range rational (_ <= 0)))
(defdata pos-rat (range rational (0 <= _)))
(defdata frac (range rational (0 <= _ <= 1)))

(definec nth-peer-custom (n :nat) :peer
  (intern-in-package-of-symbol
                     (concatenate 'string "P" (str::nat-to-dec-string n))
                     'APP))

(definec gen-peers (b :nat n :nat) :lop
  (match n
    (0 '())
    (& (cons (nth-peer-custom b)
             (gen-peers (1+ b) (1- n))))))

(defdata-attach peer :enumerator nth-peer-custom)

;; Generates n peers
(create-map* (lambda (n) (nth-peer-custom n))
             nat-listp
             lopp
             (:name mk-peers))

(definec topics () :tl
  ;; topics in FILECOIN
; '(BLOCKS MESSAGES))
  ;; sample topics 
; FM SE SEC DS PL PS NT))
  ;; topics in ETH-2.0
  '(BLOCKS AGG SUB1 SUB2 SUB3))  

(definec nth-topic-custom (n :nat) :symbol
  (nth (mod n (len (topics))) (topics)))

(definec topicp (top :all) :bool
  (symbolp top))

(register-type topic
	       :predicate topicp
	       :enumerator nth-topic-custom)

(defdata lot (listof topic))

;; Counters per topic
(defdata tctrs
  (record (invalidMessageDeliveries . pos-rat)
	  (meshMessageDeliveries    . pos-rat)
	  (meshTime                 . pos-rat)
	  (firstMessageDeliveries   . pos-rat)
	  (meshFailurePenalty       . pos-rat)))

(defthm tctrs-check1
  (==
   (tctrsp (mset :0tag 'tctrs
                    (mset :meshtime mt
                          (mset :meshfailurepenalty mfp
                                (mset :firstmessagedeliveries fmd
                                      (mset :invalidmessagedeliveries imd
                                            (mset :meshmessagedeliveries mmd
                                                  nil)))))))
   (^ (pos-ratp mt) (pos-ratp mfp) (pos-ratp mmd) (pos-ratp fmd) (pos-ratp imd)))
  :hints (("goal" :in-theory (enable tctrsp))))

(defthm tctrs-check2
  (==
   (tctrsp (mset :0tag 'tctrs
                    (mset :meshtime mt
                                (mset :meshfailurepenalty mfp
                                      (mset :meshmessagedeliveries fmd
                                            (mset :firstmessagedeliveries imd
                                                  (mset :invalidmessagedeliveries mmd
                                                        nil)))))))
   (^ (pos-ratp mt) (pos-ratp mfp) (pos-ratp mmd) (pos-ratp fmd) (pos-ratp imd)))
  :hints (("goal" :in-theory (enable tctrsp))))

(definecd new-tctrs () :tctrs
  (tctrs 0 0 0 0 0))

(defdata pt (cons peer topic))

;; pt-tctrs-map keeps track of tctrs per peer per topic
(defdata pt-tctrs-map (alistof pt tctrs))

(defun pt-tctrs-topic (peer tops n)
  (if (endp tops)
      '()
    (cons `((,peer . ,(car tops)) . ,(nth-tctrs n))
          (pt-tctrs-topic peer (cdr tops) (1+ n)))))

(defun mk-ptc (tops peers n)
  (if (endp peers)
      '()
    (app (pt-tctrs-topic (car peers) tops n)
         (mk-ptc tops (cdr peers) (+ (* (len peers) (len (topics))) n)))))

(defun nth-pt-tctrs-map-custom (n)
  (mk-ptc (topics) (map* mk-peers (natlist-from n 3)) n))

(defdata-attach pt-tctrs-map :enumerator
  nth-pt-tctrs-map-custom)

;; Global counters : counters across all topics
;; P5, P6 and P7
(defdata gctrs (list pos-rat rational pos-rat))

(defdata-subtype pos-rat rational)
(defdata-subtype neg-rat rational)
(in-theory (disable neg-ratp pos-ratp))

;; Unlike tctrs, P5, P6 and P7 are only per-peer
(defdata p-gctrs-map (alistof peer gctrs))

(defun mk-pc (peers n)
  (if (endp peers)
      '()
    (cons (cons (car peers) (nth-gctrs n))
          (mk-pc (cdr peers) (+ n 1)))))

(defun nth-p-gctrs-map-custom (n)
  (mk-pc (map* mk-peers (natlist-from n 3)) n))

(defdata-attach p-gctrs-map :enumerator nth-p-gctrs-map-custom)


;;maps Peers -> Scores
(defdata peer-rational-map (alistof peer rational))

(property assoc-score (p :peer nbr-scores :peer-rational-map)
          (let ((res (cdr (assoc-equal p nbr-scores))))
            (v (null res) (rationalp res))))


;; functions to affect change in individual tctrs.
;; multiple tctrs can be updated by function composition.
  
(definecd update-invalidMessageDeliveries(tctrs :tctrs invalidMessageDeliveries :pos-rat) :tctrs
  (mset :invalidMessageDeliveries invalidMessageDeliveries tctrs))

(definecd increment-invalidMessageDeliveries (cs :tctrs) :tctrs
  (update-invalidMessageDeliveries cs (+ 1 (tctrs-invalidMessageDeliveries cs))))

(definecd update-meshMessageDeliveries (tctrs :tctrs meshMessageDeliveries :pos-rat) :tctrs
  (mset :meshMessageDeliveries meshMessageDeliveries tctrs))

(definecd increment-meshMessageDeliveries (cs :tctrs) :tctrs
  (update-meshMessageDeliveries cs (+ 1 (tctrs-meshMessageDeliveries cs))))

(definecd update-meshTime (tctrs :tctrs meshTime :pos-rat) :tctrs
  (mset :meshTime meshTime tctrs))

(definecd update-firstMessageDeliveries (tctrs :tctrs firstMessageDeliveries :pos-rat) :tctrs
  (mset :firstMessageDeliveries firstMessageDeliveries tctrs))

(definecd increment-firstMessageDeliveries (cs :tctrs) :tctrs
  (update-firstMessageDeliveries cs (+ 1 (tctrs-firstMessageDeliveries cs))))

(definecd update-meshFailurePenalty (tctrs :tctrs meshFailurePenalty :pos-rat) :tctrs
  (mset :meshFailurePenalty meshFailurePenalty tctrs))

(definecd increment-meshFailurePenalty (cs :tctrs) :tctrs
  (update-meshFailurePenalty cs (+ 1 (tctrs-meshFailurePenalty cs))))

(in-theory (disable tctrsp))

(defdata params
  (record (activationWindow                 . nat)
	  (meshTimeQuantum                  . pos)
	  (p2cap                            . nat)
	  (timeQuantaInMeshCap              . nat)
	  (meshMessageDeliveriesCap         . nat)
	  (meshMessageDeliveriesThreshold   . nat)
	  (topiccap                         . rational)
	  ;;^^  NOTE - this is actually global, needs to be same across topics.
	  (grayListThreshold                . rational)
	  (d                                . nat)
	  (dlow                             . nat)
	  (dhigh                            . nat)
	  (dlazy                            . nat)
	  (hbmInterval                      . pos-rat)
	  (fanoutTTL                        . pos-rat)
	  (mcacheLen                        . pos)
	  (mcacheGsp                        . pos-rat)
	  (seenTTL                          . pos-rat)
	  (opportunisticGraftThreshold      . pos-rat)
	  (topicWeight                      . pos-rat)
          (meshMessageDeliveriesDecay       . frac)
          (firstMessageDeliveriesDecay      . frac)
          (behaviourPenaltyDecay            . frac)
          (meshFailurePenaltyDecay          . frac)
          (invalidMessageDeliveriesDecay    . frac)
          (decayToZero                      . frac)
          (decayInterval                    . pos-rat)))


(defthm params-check
  (== (paramsp
       (MSET :0TAG 'PARAMS
                   (MSET
                    :ACTIVATIONWINDOW
                    VAR-PARAMS-ACTIVATIONWINDOW
                    (MSET
                     :MESHTIMEQUANTUM
                     VAR-PARAMS-MESHTIMEQUANTUM
                     (MSET
                      :P2CAP VAR-PARAMS-P2CAP
                      (MSET
                       :TIMEQUANTAINMESHCAP
                       VAR-PARAMS-TIMEQUANTAINMESHCAP
                       (MSET
                        :MESHMESSAGEDELIVERIESCAP
                        VAR-PARAMS-MESHMESSAGEDELIVERIESCAP
                        (MSET
                         :MESHMESSAGEDELIVERIESTHRESHOLD
                         VAR-PARAMS-MESHMESSAGEDELIVERIESTHRESHOLD
                         (MSET
                          :TOPICCAP VAR-PARAMS-TOPICCAP
                          (MSET
                           :GRAYLISTTHRESHOLD
                           VAR-PARAMS-GRAYLISTTHRESHOLD
                           (MSET
                            :D VAR-PARAMS-D
                            (MSET
                             :DLOW VAR-PARAMS-DLOW
                             (MSET
                              :DHIGH VAR-PARAMS-DHIGH
                              (MSET
                               :DLAZY VAR-PARAMS-DLAZY
                               (MSET
                                :HBMINTERVAL VAR-PARAMS-HBMINTERVAL
                                (MSET
                                 :FANOUTTTL VAR-PARAMS-FANOUTTTL
                                 (MSET
                                  :MCACHELEN VAR-PARAMS-MCACHELEN
                                  (MSET
                                   :MCACHEGSP VAR-PARAMS-MCACHEGSP
                                   (MSET
                                    :SEENTTL VAR-PARAMS-SEENTTL
                                    (MSET
                                     :OPPORTUNISTICGRAFTTHRESHOLD
                                     VAR-PARAMS-OPPORTUNISTICGRAFTTHRESHOLD
                                     (MSET
                                      :TOPICWEIGHT VAR-PARAMS-TOPICWEIGHT
                                      (MSET
                                       :MESHMESSAGEDELIVERIESDECAY
                                       VAR-PARAMS-MESHMESSAGEDELIVERIESDECAY
                                       (MSET
                                        :FIRSTMESSAGEDELIVERIESDECAY
                                        VAR-PARAMS-FIRSTMESSAGEDELIVERIESDECAY
                                        (MSET
                                         :BEHAVIOURPENALTYDECAY
                                         VAR-PARAMS-BEHAVIOURPENALTYDECAY
                                         (MSET
                                          :MESHFAILUREPENALTYDECAY
                                          VAR-PARAMS-MESHFAILUREPENALTYDECAY
                                          (MSET
                                           :INVALIDMESSAGEDELIVERIESDECAY
                                           VAR-PARAMS-INVALIDMESSAGEDELIVERIESDECAY
                                           (MSET
                                            :DECAYTOZERO
                                            VAR-PARAMS-DECAYTOZERO
                                            (MSET
                                              :DECAYINTERVAL
                                              VAR-PARAMS-DECAYINTERVAL
                                              NIL))))))))))))))))))))))))))))
      (^ (natp VAR-PARAMS-MESHMESSAGEDELIVERIESCAP)
         (natp VAR-PARAMS-MESHMESSAGEDELIVERIESTHRESHOLD)
         (pos-ratp VAR-PARAMS-DECAYINTERVAL)
         (natp VAR-PARAMS-ACTIVATIONWINDOW)
         (posp VAR-PARAMS-MESHTIMEQUANTUM)
         (natp VAR-PARAMS-P2CAP)
         (natp VAR-PARAMS-TIMEQUANTAINMESHCAP)
         (rationalp VAR-PARAMS-TOPICCAP)
         (rationalp VAR-PARAMS-GRAYLISTTHRESHOLD)
         (natp VAR-PARAMS-D)
         (natp VAR-PARAMS-DLOW)
         (natp VAR-PARAMS-DHIGH)
         (natp VAR-PARAMS-DLAZY)
         (pos-ratp VAR-PARAMS-HBMINTERVAL)
         (pos-ratp VAR-PARAMS-FANOUTTTL)
         (posp VAR-PARAMS-MCACHELEN)
         (pos-ratp VAR-PARAMS-MCACHEGSP)
         (pos-ratp VAR-PARAMS-SEENTTL)
         (pos-ratp VAR-PARAMS-OPPORTUNISTICGRAFTTHRESHOLD)
         (pos-ratp VAR-PARAMS-TOPICWEIGHT)
         (fracp VAR-PARAMS-MESHMESSAGEDELIVERIESDECAY)
         (fracp VAR-PARAMS-FIRSTMESSAGEDELIVERIESDECAY)
         (fracp VAR-PARAMS-BEHAVIOURPENALTYDECAY)
         (fracp VAR-PARAMS-MESHFAILUREPENALTYDECAY)
         (fracp VAR-PARAMS-INVALIDMESSAGEDELIVERIESDECAY)
         (fracp VAR-PARAMS-DECAYTOZERO)))
  :hints (("goal" :in-theory (enable paramsp))))

(defthm param-get-mmdct
  (=> (paramsp p)
      (^ (natp (MGET :MESHMESSAGEDELIVERIESCAP p))
         (natp (MGET :MESHMESSAGEDELIVERIESTHRESHOLD p)))))

(defdata weights (list pos-rat pos-rat neg-rat neg-rat neg-rat pos-rat
                       neg-rat neg-rat))

(defthm weights-check
  (== (weightsp ws)
      (^ (tlp ws)
         (equal 8 (len ws))
         (pos-ratp (first ws))
         (pos-ratp (second ws))
         (neg-ratp (third ws))
         (neg-ratp (fourth ws))
         (neg-ratp (fifth ws))
         (pos-ratp (sixth ws))
         (neg-ratp (seventh ws))
         (neg-ratp (eighth ws)))))

;; both weights and params are indexed by topic
(defdata wp (cons weights params))
(defdata twp (alistof topic wp))
(defdata maybe-wp (or nil wp))

;; weights, params and wp are huge data structures,
;; so we disable their definitions to speed up theorem proving
(in-theory (disable weightsp paramsp wpp))

(definecd lookup-twpm (top :topic twpm :twp) :maybe-wp
  :skip-tests t
  (match twpm
    (() nil)
    (((!top . wpm) . &) wpm)
    ((& . rst) (lookup-twpm top rst))))

(definecd is-valid-twp (twp :twp) :boolean
  (v (endp twp)
     (let ((params (cddar twp)))
       (^ (weightsp (cadar twp))
          (paramsp params)
          (>= (params-meshMessageDeliveriesCap params)
              (params-meshMessageDeliveriesThreshold params))
          (<= (params-dlow params) (params-d params))
          (>= (params-dhigh params) (params-d params))
          (>= (params-decayInterval params) 1)                
          (is-valid-twp (cdr twp))))))

(defthm lookup-twpm-check1
  (let ((x (lookup-twpm top twpm)))
    (=> (^ (topicp top) (twpp twpm) x)
        (^ (weightsp (car x))
           (paramsp (cdr x))))))

(defthm lookup-twpm-check2
  (b* ((x (lookup-twpm top twpm))
       (ps (cdr x)))
    (=> (^ (topicp top) (twpp twpm) (is-valid-twp twpm) x)
        (<= (MGET :MESHMESSAGEDELIVERIESTHRESHOLD ps)
            (MGET :MESHMESSAGEDELIVERIESCAP ps))))
  :hints (("Goal" :in-theory (enable lookup-twpm is-valid-twp wpp paramsp))))

(defthm lookup-twpm-check3
  (b* ((x (lookup-twpm top twpm))
       (ps (cdr x)))
    (=> (^ (topicp top) (twpp twpm) (is-valid-twp twpm) x)
        (<= (MGET :DLOW ps)
            (MGET :D ps))))
  :hints (("Goal" :in-theory (enable lookup-twpm is-valid-twp wpp paramsp))))


(defthm lookup-twpm-check4
  (=> (^ (twpp twpm) (is-valid-twp twpm))
      (<= (MGET :DLOW (CDDR (CAR TWPM)))
          (MGET :D (CDDR (CAR TWPM)))))
  :hints (("Goal" :in-theory (enable is-valid-twp wpp paramsp))))

(definecd calcP1 (meshTime :pos-rat meshTimeQuantum :pos timeQInMeshCap :nat) :rational
  (min (/ meshTime meshTimeQuantum) timeQInMeshCap))

(definecd calcP2 (firstMessageDeliveries :pos-rat p2cap :nat) :pos-rat
  (min firstMessageDeliveries p2cap))

(definecd calc-deficit (meshMessageDeliveries :pos-rat
		        meshMessageDeliveriesCap :nat
			meshMessageDeliveriesThreshold :nat) :pos-rat
  :ic (>= meshMessageDeliveriesCap meshMessageDeliveriesThreshold)
  (b* ((mmd (min meshMessageDeliveries meshMessageDeliveriesCap))
       (deficit (- meshMessageDeliveriesThreshold mmd)))
    (max deficit 0)))

(definecd calcP3 (meshTime :pos-rat
		  activationWindow :nat
		  meshMessageDeliveries :pos-rat
		  meshMessageDeliveriesCap :nat
		  meshMessageDeliveriesThreshold :nat) :pos-rat
  :ic (>= meshMessageDeliveriesCap meshMessageDeliveriesThreshold)
  (b* ((deficit (calc-deficit meshMessageDeliveries
			      meshMessageDeliveriesCap
			      meshMessageDeliveriesThreshold)))
    (if (and (> meshTime activationWindow)
             (< meshMessageDeliveries meshMessageDeliveriesThreshold))
        (* deficit deficit)
      0)))

;; not checking activation window here because we don't immediately prune a
;; peer just after adding them
(definecd calcP3b (meshTime :pos-rat
                            activationWindow :nat
                            meshFailurePenalty :pos-rat
                            meshMessageDeliveries :pos-rat
                            meshMessageDeliveriesCap :nat
                            meshMessageDeliveriesThreshold :nat) :pos-rat
                            :ic (>= meshMessageDeliveriesCap meshMessageDeliveriesThreshold)
  (if (and (> meshTime activationWindow)
           (< meshMessageDeliveries meshMessageDeliveriesThreshold))
      (let ((deficit (calc-deficit meshMessageDeliveries
                                   meshMessageDeliveriesCap
                                   meshMessageDeliveriesThreshold)))
        (+ meshFailurePenalty (* deficit deficit)))
    meshFailurePenalty))
  
(definecd calcP4 (invalidMessageDeliveries :pos-rat) :pos-rat
    (* invalidMessageDeliveries invalidMessageDeliveries))

(definecd calcP7 (badBehaviour :pos-rat) :pos-rat
  (* badBehaviour badBehaviour))

(definecd calcScoreTopic (tctrs :tctrs wtpm :wp) :rational
  :skip-tests t
  :function-contract-hints (("Goal" :in-theory (enable wpp)))
  :body-contracts-hints (("Goal" :in-theory (enable wpp)))
  :ic (>= (params-meshMessageDeliveriesCap (cdr wtpm))
	  (params-meshMessageDeliveriesThreshold (cdr wtpm)))
  (b* ((weights (car wtpm))
       (params (cdr wtpm)))
    (* (params-topicweight params)
       (+ (* (first weights)
             (calcP1 (tctrs-meshTime tctrs)
                     (params-meshTimeQuantum params)
                     (params-timeQuantaInMeshCap params)))
          (* (second weights)
             (calcP2 (tctrs-firstMessageDeliveries tctrs) (params-p2cap params)))
          (* (third weights)
             (calcP3 (tctrs-meshTime tctrs) (params-activationWindow params) 
                     (tctrs-meshMessageDeliveries tctrs) (params-meshMessageDeliveriesCap params)
                     (params-meshMessageDeliveriesThreshold params)))
          (* (fourth weights)
             (calcP3b (tctrs-meshTime tctrs) (params-activationWindow params)
                      (tctrs-meshFailurePenalty tctrs) (tctrs-meshMessageDeliveries
                                                                          tctrs)
                      (params-meshMessageDeliveriesCap params) (params-meshMessageDeliveriesThreshold params)))
          (* (fifth weights)
             (calcP4 (tctrs-invalidMessageDeliveries tctrs)))
          ))))

(definec lookup-gctrs (p :peer map :p-gctrs-map) :gctrs
  :skip-tests t
  (match map
    (() '(0 0 0))
    (((!p . gct) . &) gct)
    ((& . rst) (lookup-gctrs p rst))))

(defthm lookup-gctrs-check
  (let ((x (lookup-gctrs p gc)))
    (=> (^ (p-gctrs-mapp gc) (peerp p))
        (^ (endp (cdddr x))
           (consp (cddr x))
           (pos-ratp (first x))
           (rationalp (second x))
           (pos-ratp (third x))))))

(definecd lookup-tctrs (p :peer top :topic map :pt-tctrs-map) :tctrs
  :skip-tests t
  (match map
    (() (new-tctrs))
    ((((!p . !top) . tct) . &) tct)
    ((& . rst) (lookup-tctrs p top rst))))

(definecd lookup-score (p :peer map :peer-rational-map) :rational
  :skip-tests t
  (match map
    (() 0)
    (((!p . s) . &) s)
    ((& . rst) (lookup-score p rst))))

(in-theory (disable lookup-twpm lookup-gctrs lookup-tctrs lookup-score))

(definecd calc-glb-score (p :peer gbmap :p-gctrs-map weights :weights) :rational
  (let ((gc (lookup-gctrs p gbmap)))
    (+ (* (sixth weights) (first gc))
       (* (eighth weights) (calcP7 (third gc))))))
 
(in-theory (disable calcScoreTopic-definition-rule calc-glb-score-definition-rule))

(definecd calc-topic-scores (pt-ctrs :pt-tctrs-map twpm :twp acc :peer-rational-map)
  :peer-rational-map
  :ic (is-valid-twp twpm)
  :skip-tests t
  :body-contracts-hints (("Goal" :use (lookup-twpm-check2)))
  :timeout 600
  (match pt-ctrs
    (() acc)
    ((((p . top) . ctrs) . rst)
     (b* ((tmp (lookup-score p acc))
          (wp (lookup-twpm top twpm))
          ((when (null wp)) (calc-topic-scores rst twpm acc))
          (new-score (+ (calcScoreTopic ctrs wp) tmp)))
       (calc-topic-scores rst twpm (put-assoc-equal p new-score acc))))))

(definecd add-glb-scores (peers :lop topic-scores :peer-rational-map gbmap
  :p-gctrs-map weights :weights topiccap :rational) :peer-rational-map
  :skip-tests t
  (match peers
    (() '())
    ((p . rst)
     `((,p . ,(+ (min topiccap (lookup-score p topic-scores))
                 (calc-glb-score p gbmap weights)))
       . ,(add-glb-scores rst topic-scores gbmap weights topiccap)))))

(definecd peers-from-pt-tctrs-map (map :pt-tctrs-map acc :lop) :lop
  (match map
    (() acc)
    ((((p . &) . &) . rst)
     (peers-from-pt-tctrs-map rst (if (in p acc)
                                      acc
                                    (cons p acc))))))

;; weights for global score counters are common for all topics
(defthm consp-twpm-weights
  (=> (^ (twpp twpm) (consp twpm))
      (weightsp (cadar twpm))))

(defthm topiccap-twpm
  (=> (^ (TWPP TWPM)
         (consp TWPM))
      (RATIONALP (MGET :TOPICCAP (CDDR (CAR TWPM)))))
  :hints (("Goal" :in-theory (enable twpp wpp))))
  
(definecd calc-nbr-scores-map (pt-ctrs :pt-tctrs-map gbmap
  :p-gctrs-map twpm :twp) :peer-rational-map
  :ic (^ (consp twpm)
         (is-valid-twp twpm))
  :skip-tests t
  :body-contracts-hints (("Goal" :in-theory (enable paramsp twpp wpp)))
  (b* ((topic-scores-map (calc-topic-scores pt-ctrs twpm nil))
       (peers (peers-from-pt-tctrs-map pt-ctrs nil)))
    (add-glb-scores peers topic-scores-map gbmap (cadar twpm) (params-topiccap (cddar twpm)))))


;; Properties

;; ------------------------------------------------
;; Some interesting properties about topic-counters
;; ------------------------------------------------

;; 1. Increasing meshTime increases score. This gives a counter example.

(acl2::must-fail
 (property mesh-time-inc-score (imd :pos-rat mmd :pos-rat mt :pos-rat fmd
                                    :pos-rat mfp :pos-rat p :pos-rat wtpm :wp)
           (=> (>= (params-meshMessageDeliveriesCap (cdr wtpm))
                   (params-meshMessageDeliveriesThreshold (cdr wtpm)))
               (>= (calcScoreTopic (tctrs imd mmd  (+ p mt) fmd mfp) wtpm)
                   (calcScoreTopic (tctrs imd mmd  mt fmd mfp) wtpm)))))



(property inc-calcP4 (imd :pos-rat p :pos-rat)
          (=> (< 0 p)
              (< (calcP4 imd)
                 (calcP4 (+ p imd))))
          :hints (("Goal" :in-theory (enable calcP4))))

(property neg-fifth-weight (wt :weights)
          (>= 0 (fifth wt))
          :hints (("Goal" :use weights-check)))
          
;; 2. Increasing invalidMessageDeliveries decreases score.
(property imd-dec-score (imd :pos-rat mmd :pos-rat mt :pos-rat fmd
                                   :pos-rat mfp :pos-rat p :pos-rat wtpm :wp)
          (=> (^ (>= (params-meshMessageDeliveriesCap (cdr wtpm))
                     (params-meshMessageDeliveriesThreshold (cdr wtpm)))
                 (< 0 p))
              (>= (calcScoreTopic (tctrs imd mmd  mt fmd mfp) wtpm)
                  (calcScoreTopic (tctrs (+ p imd) mmd  mt fmd mfp) wtpm)))
          :hints (("Goal" :in-theory (enable tctrsp calcScoreTopic calcP4 weightsp))))


;; 3. Decreasing meshMessageDeliveries below the threshold decreases calcP3 score
(property dec-calcP3 (meshTime :pos-rat
                               activationWindow :nat
                               meshMessageDeliveries :pos-rat
                               meshMessageDeliveriesCap :nat
                               meshMessageDeliveriesThreshold :nat
                               p :pos-rat)
          (=> (^ (> meshTime activationWindow)
                 (< meshMessageDeliveries meshMessageDeliveriesThreshold)
                 (<= MESHMESSAGEDELIVERIESTHRESHOLD
                     MESHMESSAGEDELIVERIESCAP)
                 (< 0 p)
                 (< 0 (- meshMessageDeliveries p)))
              (< (calcP3 meshTime activationWindow meshMessageDeliveries
                               meshMessageDeliveriesCap
                               meshMessageDeliveriesThreshold)
                 (calcP3 meshTime activationWindow (- meshMessageDeliveries p)
                               meshMessageDeliveriesCap
                               meshMessageDeliveriesThreshold)))
          :hints (("Goal" :in-theory (enable calcP3 calc-deficit))))

;; 4. Decreasing meshMessageDeliveries below the threshold decreases calcP3b score
(property dec-calcP3b (meshTime :pos-rat
                                activationWindow :nat
                                meshFailurePenalty :pos-rat
                                meshMessageDeliveries :pos-rat
                                meshMessageDeliveriesCap :nat
                                meshMessageDeliveriesThreshold :nat
                                p :pos-rat)
          (=> (^ (> meshTime activationWindow)
                 (< meshMessageDeliveries meshMessageDeliveriesThreshold)
                 (<= MESHMESSAGEDELIVERIESTHRESHOLD
                     MESHMESSAGEDELIVERIESCAP)
                 (< 0 p)
                 (< 0 (- meshMessageDeliveries p)))
              (< (calcP3b meshTime activationWindow
                          meshFailurePenalty
                          meshMessageDeliveries
                          meshMessageDeliveriesCap
                          meshMessageDeliveriesThreshold)
                 (calcP3b meshTime activationWindow
                          meshFailurePenalty
                          (- meshMessageDeliveries p)
                          meshMessageDeliveriesCap
                          meshMessageDeliveriesThreshold)))
          :hints (("Goal" :in-theory (enable calcP3b calc-deficit))))


(property neg-third-weight (wt :weights)
          (>= 0 (third wt))
          :hints (("Goal" :use weights-check)))

(property neg-fourth-weight (wt :weights)
          (>= 0 (fourth wt))
          :hints (("Goal" :use weights-check)))


(property more-less-neg (a :rational b :rational c :neg-rat)
          (=> (< b a)
              (<= (* c a)
                  (* c b))))



(property dec-calcP3-mult (meshTime :pos-rat
                               activationWindow :nat
                               meshMessageDeliveries :pos-rat
                               meshMessageDeliveriesCap :nat
                               meshMessageDeliveriesThreshold :nat
                               weights :weights
                               p :pos-rat)
          (=> (^ (> meshTime activationWindow)
                 (< meshMessageDeliveries meshMessageDeliveriesThreshold)
                 (<= MESHMESSAGEDELIVERIESTHRESHOLD
                     MESHMESSAGEDELIVERIESCAP)
                 (< 0 p)
                 (< 0 (- meshMessageDeliveries p)))
              (>= (* (third weights)
                    (calcP3 meshTime activationWindow meshMessageDeliveries
                               meshMessageDeliveriesCap
                               meshMessageDeliveriesThreshold))
                 (* (third weights)
                    (calcP3 meshTime activationWindow (- meshMessageDeliveries p)
                               meshMessageDeliveriesCap
                               meshMessageDeliveriesThreshold)))))


(property dec-calcP3b-mult (meshTime :pos-rat
                                     activationWindow :nat
                                     meshFailurePenalty :pos-rat
                               meshMessageDeliveries :pos-rat
                               meshMessageDeliveriesCap :nat
                               meshMessageDeliveriesThreshold :nat
                               weights :weights
                               p :pos-rat)
          (=> (^ (> meshTime activationWindow)
                 (< meshMessageDeliveries meshMessageDeliveriesThreshold)
                 (<= MESHMESSAGEDELIVERIESTHRESHOLD
                     MESHMESSAGEDELIVERIESCAP)
                 (< 0 p)
                 (< 0 (- meshMessageDeliveries p)))
              (>= (* (fourth weights)
                     (calcP3b meshTime activationWindow
                              meshFailurePenalty
                              meshMessageDeliveries
                               meshMessageDeliveriesCap
                               meshMessageDeliveriesThreshold))
                 (* (fourth weights)
                    (calcP3b meshTime activationWindow
                             meshFailurePenalty
                             (- meshMessageDeliveries p)
                               meshMessageDeliveriesCap
                               meshMessageDeliveriesThreshold)))))

(property sum-gt (a :rational b :rational c :rational d :rational)
          (=> (^ (>= a c)
                 (>= b d))
              (>= (+ a b)
                  (+ c d))))


;; 5. Decreasing meshMessageDeliveries below the threshold (increasing the
;; deficit) decreases calcP3 + calcP3b score
(property dec-sum-calcP3-calcP3b (meshTime :pos-rat
                                           activationWindow :nat
                                           meshFailurePenalty :pos-rat
                                           meshMessageDeliveries :pos-rat
                                           meshMessageDeliveriesCap :nat
                                           meshMessageDeliveriesThreshold :nat
                                           weights :weights
                                           p :pos-rat)
          (=> (^ (> meshTime activationWindow)
                 (< meshMessageDeliveries meshMessageDeliveriesThreshold)
                 (<= MESHMESSAGEDELIVERIESTHRESHOLD
                     MESHMESSAGEDELIVERIESCAP)
                 (< 0 p)
                 (< 0 (- meshMessageDeliveries p)))
              (>= (+ (* (third weights)
                        (calcP3 meshTime activationWindow meshMessageDeliveries
                                meshMessageDeliveriesCap
                                meshMessageDeliveriesThreshold))
                     (* (fourth weights)
                        (calcP3b meshTime activationWindow
                                 meshFailurePenalty
                                 meshMessageDeliveries
                                 meshMessageDeliveriesCap
                                 meshMessageDeliveriesThreshold)))
                  (+ (* (third weights)
                        (calcP3 meshTime activationWindow (- meshMessageDeliveries p)
                                meshMessageDeliveriesCap
                                meshMessageDeliveriesThreshold))
                     (* (fourth weights)
                        (calcP3b meshTime activationWindow
                                 meshFailurePenalty
                                 (- meshMessageDeliveries p)
                                 meshMessageDeliveriesCap
                                 meshMessageDeliveriesThreshold))))))

(property cancel-add (a :rational b :rational c :rational d :rational)
          (=> (== a b)
              (== (>= (+ a c) (+ b d))
                  (>= c d))))


(property cancel-add2 (a :rational b :rational c :rational d :rational e
          :rational f :rational g :rational h :rational i :rational j :rational
          e2 :rational f2 :rational g2 :rational h2 :rational tw :rational)
          (== (<= (* tw
                         (+ (* a b)
                            (* c d)
                            (* e f)
                            (* g h)
                            (* i j)))
                      (* tw
                         (+ (* a b)
                            (* c d)
                            (* e2 f2)
                            (* g2 h2)
                            (* i j))))
                  (<= (* tw
                         (+ (* e f)
                            (* g h)))
                      (* tw
                         (+ (* e2 f2)
                            (* g2 h2))))))


;; ;; 3. Increasing deficit decreases score.
;; (property mmd-dec-score (imd :pos-rat mmd :pos-rat mt :pos-rat fmd
;;                                    :pos-rat mfp :pos-rat p :pos-rat wtpm :wp)
;;           (=> (^ (>= (params-meshMessageDeliveriesCap (cdr wtpm))
;;                      (params-meshMessageDeliveriesThreshold (cdr wtpm)))
;;                  (< 0 p)
;;                  (< 0 (- mmd p))
;;                  (< mmd (params-meshMessageDeliveriesThreshold (cdr wtpm))))
;;               (>= (calcScoreTopic (tctrs imd mmd  mt fmd mfp) wtpm)
;;                   (calcScoreTopic (tctrs imd (- mmd p)  mt fmd mfp) wtpm)))
;;           :hints (("Goal" :in-theory (enable tctrsp calcScoreTopic))))


