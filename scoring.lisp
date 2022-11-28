(in-package "ACL2S")
(include-book "higher-order")
(include-book "utils")

(def-const *few-seconds* 5)
(def-const *two-minutes* 120)

(defdata peer symbol)
(defdata lop (listof peer))
; Already defined. See 
(defdata frac (range rational (0 <= _ <= 1)))

(defdata-subtype neg-rational rational)
(defdata-subtype non-neg-rational rational)
(defdata-subtype pos-rational rational)
(defdata-subtype frac rational)

(property frac-type (x :frac)
  (^ (rationalp x)
     (<= 0 x)
     (<= x 1))
  :rule-classes :forward-chaining)

(in-theory (disable fracp))

(definec nth-peer-custom (n :nat) :peer
  (intern-in-package-of-symbol
   (concatenate 'string "P" (str::nat-to-dec-string n))
   'APP))

(definec gen-peers (b n :nat) :lop
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
  (record (invalidMessageDeliveries . non-neg-rational)
          (meshMessageDeliveries    . non-neg-rational)
          (meshTime                 . non-neg-rational)
          (firstMessageDeliveries   . non-neg-rational)
          (meshFailurePenalty       . non-neg-rational)))

(property tctrs-check2 (tc :tctrs)
  (^ (non-neg-rationalp (g :meshtime tc))
     (non-neg-rationalp (g :meshfailurepenalty tc))
     (non-neg-rationalp (g :firstmessagedeliveries tc))
     (non-neg-rationalp (g :invalidmessagedeliveries tc))
     (non-neg-rationalp (g :meshmessagedeliveries tc)))
  :rule-classes :forward-chaining)

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
(defdata gctrs (list rational non-neg-rational non-neg-rational))

(property gctrs-check (x :gctrs)
  (^ (lorp x)
     (rationalp (first x))
     (non-neg-rationalp (second x))
     (non-neg-rationalp (third x)))
  :hints (("goal" :in-theory (enable non-neg-rationalp gctrsp)))
  :rule-classes :forward-chaining)

(in-theory (disable gctrsp neg-rationalp non-neg-rationalp))

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

;; functions to affect change in individual tctrs.
;; multiple tctrs can be updated by function composition.
  
(definecd update-invalidMessageDeliveries
  (tctrs :tctrs invalidMessageDeliveries :non-neg-rational) :tctrs
  (s :invalidMessageDeliveries invalidMessageDeliveries tctrs))

(definecd increment-invalidMessageDeliveries (cs :tctrs) :tctrs
  (update-invalidMessageDeliveries cs (+ 1 (tctrs-invalidMessageDeliveries cs))))

(definecd update-meshMessageDeliveries
  (tctrs :tctrs meshMessageDeliveries :non-neg-rational) :tctrs
  (s :meshMessageDeliveries meshMessageDeliveries tctrs))

(definecd increment-meshMessageDeliveries (cs :tctrs) :tctrs
  (update-meshMessageDeliveries cs (+ 1 (tctrs-meshMessageDeliveries cs))))

(definecd update-meshTime (tctrs :tctrs meshTime :non-neg-rational) :tctrs
  (s :meshTime meshTime tctrs))

(definecd update-firstMessageDeliveries
  (tctrs :tctrs firstMessageDeliveries :non-neg-rational) :tctrs
  (s :firstMessageDeliveries firstMessageDeliveries tctrs))

(definecd increment-firstMessageDeliveries (cs :tctrs) :tctrs
  (update-firstMessageDeliveries cs (+ 1 (tctrs-firstMessageDeliveries cs))))

(definecd update-meshFailurePenalty
  (tctrs :tctrs meshFailurePenalty :non-neg-rational) :tctrs
  (s :meshFailurePenalty meshFailurePenalty tctrs))

(definecd increment-meshFailurePenalty (cs :tctrs) :tctrs
  (update-meshFailurePenalty cs (+ 1 (tctrs-meshFailurePenalty cs))))

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
          (hbmInterval                      . pos-rational)
          (fanoutTTL                        . pos-rational)
          (mcacheLen                        . pos)
          (mcacheGsp                        . non-neg-rational)
          (seenTTL                          . non-neg-rational)
          (opportunisticGraftThreshold      . non-neg-rational)
          (topicWeight                      . non-neg-rational)
          (meshMessageDeliveriesDecay       . frac)
          (firstMessageDeliveriesDecay      . frac)
          (behaviourPenaltyDecay            . frac)
          (meshFailurePenaltyDecay          . frac)
          (invalidMessageDeliveriesDecay    . frac)
          (decayToZero                      . frac)
          (decayInterval                    . pos-rational)))


; The records book we use has a loop-stopper that makes it hard to
; determine how msets are rewritten, so I wrote a small book to adjust
; that and now it is part of defdata.

#|


(defthm params-check
  (== (paramsp
       (s :0tag 'params
                   (mset
                    :activationwindow
                    var-params-activationwindow
                    (mset
                     :meshtimequantum
                     var-params-meshtimequantum
                     (mset
                      :p2cap var-params-p2cap
                      (mset
                       :timequantainmeshcap
                       var-params-timequantainmeshcap
                       (mset
                        :meshmessagedeliveriescap
                        var-params-meshmessagedeliveriescap
                        (mset
                         :meshmessagedeliveriesthreshold
                         var-params-meshmessagedeliveriesthreshold
                         (mset
                          :topiccap var-params-topiccap
                          (mset
                           :graylistthreshold
                           var-params-graylistthreshold
                           (mset
                            :d var-params-d
                            (mset
                             :dlow var-params-dlow
                             (mset
                              :dhigh var-params-dhigh
                              (mset
                               :dlazy var-params-dlazy
                               (mset
                                :hbminterval var-params-hbminterval
                                (mset
                                 :fanoutttl var-params-fanoutttl
                                 (mset
                                  :mcachelen var-params-mcachelen
                                  (mset
                                   :mcachegsp var-params-mcachegsp
                                   (mset
                                    :seenttl var-params-seenttl
                                    (mset
                                     :opportunisticgraftthreshold
                                     var-params-opportunisticgraftthreshold
                                     (mset
                                      :topicweight var-params-topicweight
                                      (mset
                                       :meshmessagedeliveriesdecay
                                       var-params-meshmessagedeliveriesdecay
                                       (mset
                                        :firstmessagedeliveriesdecay
                                        var-params-firstmessagedeliveriesdecay
                                        (mset
                                         :behaviourpenaltydecay
                                         var-params-behaviourpenaltydecay
                                         (mset
                                          :meshfailurepenaltydecay
                                          var-params-meshfailurepenaltydecay
                                          (mset
                                           :invalidmessagedeliveriesdecay
                                           var-params-invalidmessagedeliveriesdecay
                                           (mset
                                            :decaytozero
                                            var-params-decaytozero
                                            (mset
                                              :decayinterval
                                              var-params-decayinterval
                                              nil))))))))))))))))))))))))))))
      (^ (natp var-params-meshmessagedeliveriescap)
         (natp var-params-meshmessagedeliveriesthreshold)
         (pos-rationalp var-params-decayinterval)
         (natp var-params-activationwindow)
         (posp var-params-meshtimequantum)
         (natp var-params-p2cap)
         (natp var-params-timequantainmeshcap)
         (rationalp var-params-topiccap)
         (rationalp var-params-graylistthreshold)
         (natp var-params-d)
         (natp var-params-dlow)
         (natp var-params-dhigh)
         (natp var-params-dlazy)
         (pos-rationalp var-params-hbminterval)
         (pos-rationalp var-params-fanoutttl)
         (posp var-params-mcachelen)
         (non-neg-rationalp var-params-mcachegsp)
         (non-neg-rationalp var-params-seenttl)
         (non-neg-rationalp var-params-opportunisticgraftthreshold)
         (non-neg-rationalp var-params-topicweight)
         (fracp var-params-meshmessagedeliveriesdecay)
         (fracp var-params-firstmessagedeliveriesdecay)
         (fracp var-params-behaviourpenaltydecay)
         (fracp var-params-meshfailurepenaltydecay)
         (fracp var-params-invalidmessagedeliveriesdecay)
         (fracp var-params-decaytozero)))
  :hints (("goal" :in-theory (enable paramsp))))

(property params-check2 (x :params)
          (^ (natp (params-meshmessagedeliveriescap x))
             (natp (params-meshmessagedeliveriesthreshold x))
             (pos-rationalp (params-decayinterval x))
             (natp (params-activationwindow x))
             (posp (params-meshtimequantum x))
             (natp (params-p2cap x))
             (natp (params-timequantainmeshcap x))
             (rationalp (params-topiccap x))
             (rationalp (params-graylistthreshold x))
             (natp (params-d x))
             (natp (params-dlow x))
             (natp (params-dhigh x))
             (natp (params-dlazy x))
             (pos-rationalp (params-hbminterval x))
             (pos-rationalp (params-fanoutttl x))
             (posp (params-mcachelen x))
             (non-neg-rationalp (params-mcachegsp x))
             (non-neg-rationalp (params-seenttl x))
             (non-neg-rationalp (params-opportunisticgraftthreshold x))
             (non-neg-rationalp (params-topicweight x))
             (fracp (params-meshmessagedeliveriesdecay x))
             (fracp (params-firstmessagedeliveriesdecay x))
             (fracp (params-behaviourpenaltydecay x))
             (fracp (params-meshfailurepenaltydecay x))
             (fracp (params-invalidmessagedeliveriesdecay x))
             (fracp (params-decaytozero x)))
          :rule-classes :forward-chaining)
|#
          

(defdata weights
  (list non-neg-rational non-neg-rational non-pos-rational
        non-pos-rational neg-rational non-neg-rational
        neg-rational neg-rational))

(property weights-check (ws :weights)
  (^ (tlp ws)
     (equal 8 (len ws))
     (non-neg-rationalp (first ws))
     (non-neg-rationalp (second ws))
     (non-pos-rationalp (third ws))
     (non-pos-rationalp (fourth ws))
     (neg-rationalp (fifth ws))
     (non-neg-rationalp (sixth ws))
     (neg-rationalp (seventh ws))
     (neg-rationalp (eighth ws)))
  :rule-classes ((:forward-chaining :trigger-terms ((weightsp ws)))))

;; both weights and params are indexed by topic
(defdata wp (cons weights params))

(property wp-type (x :wp)
  (^ (weightsp (car x))
     (paramsp (cdr x)))
  :rule-classes :forward-chaining)

(defdata twp (alistof topic wp))
(defdata maybe-wp (or nil wp))
(defdata niltype nil)

(sig assoc-equal (:a (alistof :a :b)) => (v (cons :a :b) niltype))
(sig put-assoc-equal (:a :b (alistof :a :b)) => (alistof :a :b))

;; weights, params and wp are huge data structures,
;; so we disable their definitions to speed up theorem proving
(in-theory (disable weightsp paramsp wpp tctrsp))

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

(property assoc-twp (top :topic twpm :twp)
  (let ((x (cdr (assoc-equal top twpm))))
    (=> x
        (^ (wpp x)
           (weightsp (car x))
           (paramsp (cdr x))))))
          
(property lookup-twpm-check2 (top :topic twpm :twp)
  :check-contracts? nil
  (b* ((x (cdr (assoc-equal top twpm)))
       (ps (cdr x)))
    (=> (^ (is-valid-twp twpm) x)
        (<= (g :meshmessagedeliveriesthreshold ps)
            (g :meshmessagedeliveriescap ps))))
  :hints (("goal" :in-theory (enable is-valid-twp paramsp))))

(property lookup-twpm-check3 (top :topic twpm :twp)
  :check-contracts? nil
  (b* ((x (cdr (assoc-equal top twpm)))
       (ps (cdr x)))
    (=> (^ (is-valid-twp twpm) x)
        (<= (g :dlow ps)
            (g :d ps))))
  :hints (("goal" :in-theory (enable is-valid-twp paramsp))))

(property lookup-twpm-check4 (twpm :twp)
  :check-contracts? nil
  (=> (is-valid-twp twpm)
      (<= (g :dlow (cddr (car twpm)))
          (g :d (cddr (car twpm)))))
  :hints (("goal" :in-theory (enable is-valid-twp paramsp))))

(property car-twpm-check5 (twpm :twp :cons params :all)
  :check-contracts? nil
  (=> (^ (is-valid-twp twpm) (equal params (cddar twpm)))
      (^ (>= (params-meshMessageDeliveriesCap params)
             (params-meshMessageDeliveriesThreshold params))
         (<= (params-dlow params) (params-d params))
         (>= (params-dhigh params) (params-d params))))
  :hints (("Goal" :in-theory (enable is-valid-twp)))
  :rule-classes :forward-chaining)

(definecd calcP1
  (meshTime :non-neg-rational meshTimeQuantum :pos timeQInMeshCap :nat) :rational
  (min (/ meshTime meshTimeQuantum) timeQInMeshCap))

(property maxP1
  (meshTime :non-neg-rational meshTimeQuantum :pos timeQInMeshCap
            :nat)
  (<= (calcP1 meshTime meshTimeQuantum timeQInMeshCap) timeQInMeshCap)
  :hints (("Goal" :in-theory (enable calcP1))))

(definecd calcP2
  (firstMessageDeliveries :non-neg-rational p2cap :nat) :non-neg-rational
  (min firstMessageDeliveries p2cap))

(property maxP2 (firstMessageDeliveries :non-neg-rational p2cap :nat)
  (<= (calcP2 firstMessageDeliveries p2cap) p2cap)
  :hints (("Goal" :in-theory (enable calcP2))))

(definecd calc-deficit
  (meshMessageDeliveries :non-neg-rational
                         meshMessageDeliveriesCap 
                         meshMessageDeliveriesThreshold :nat)  :non-neg-rational
  :ic (>= meshMessageDeliveriesCap meshMessageDeliveriesThreshold)
  (b* ((mmd (min meshMessageDeliveries meshMessageDeliveriesCap))
       (deficit (- meshMessageDeliveriesThreshold mmd)))
    (max deficit 0)))

(definecd calcP3
  (meshTime :non-neg-rational
            activationWindow :nat
            meshMessageDeliveries :non-neg-rational
            meshMessageDeliveriesCap 
            meshMessageDeliveriesThreshold :nat) :non-neg-rational
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
(definecd calcP3b
  (meshTime :non-neg-rational
            activationWindow :nat
            meshFailurePenalty 
            meshMessageDeliveries :non-neg-rational
            meshMessageDeliveriesCap 
            meshMessageDeliveriesThreshold :nat) :non-neg-rational
  :ic (>= meshMessageDeliveriesCap meshMessageDeliveriesThreshold)
  (if (and (> meshTime activationWindow)
           (< meshMessageDeliveries meshMessageDeliveriesThreshold))
      (let ((deficit (calc-deficit meshMessageDeliveries
                                   meshMessageDeliveriesCap
                                   meshMessageDeliveriesThreshold)))
        (+ meshFailurePenalty (* deficit deficit)))
    meshFailurePenalty))
  
(property non-pos-p3-p3b (tctrs :tctrs  wtpm :wp)
  (=> (<= (g :meshmessagedeliveriesthreshold (cdr wtpm))
          (g :meshmessagedeliveriescap (cdr wtpm)))
      (>= 0
          (+ (* (caddr (car wtpm))
                (calcp3 (tctrs-meshtime tctrs)
                        (g :activationwindow (cdr wtpm))
                        (tctrs-meshmessagedeliveries tctrs)
                        (g :meshmessagedeliveriescap (cdr wtpm))
                        (g :meshmessagedeliveriesthreshold (cdr wtpm))))
             (* (cadddr (car wtpm))
                (calcp3b (tctrs-meshtime tctrs)
                         (g :activationwindow (cdr wtpm))
                         (tctrs-meshfailurepenalty tctrs)
                         (tctrs-meshmessagedeliveries tctrs)
                         (g :meshmessagedeliveriescap (cdr wtpm))
                         (g :meshmessagedeliveriesthreshold (cdr wtpm)))))))
  :hints (("Goal" :in-theory (enable twpp wpp weightsp))))

(definecd calcP4
  (invalidMessageDeliveries :non-neg-rational) :non-neg-rational
  (* invalidMessageDeliveries invalidMessageDeliveries))

(definecd calcP7 (badBehaviour :non-neg-rational) :non-neg-rational
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

(property max-helper2 (tctrs :tctrs wtpm :wp)
  :proof-timeout 200
  (=> (>= (params-meshMessageDeliveriesCap (cdr wtpm))
          (params-meshMessageDeliveriesThreshold (cdr wtpm)))
      (b* ((weights (car wtpm))
           (params (cdr wtpm)))
        (<= (* (params-topicweight params)
               (+ (* (first weights)
                     (calcP1 (tctrs-meshTime tctrs)
                             (params-meshTimeQuantum params)
                             (params-timeQuantaInMeshCap params)))
                  (* (second weights)
                     (calcP2 (tctrs-firstMessageDeliveries tctrs)
                             (params-p2cap params)))))
            (* (params-topicweight params)
               (+ (* (first weights)
                     (params-timeQuantaInMeshCap params))
                  (* (second weights)
                     (params-p2cap params)))))))
  :hints (("Goal"
           :in-theory (enable weightsp paramsp calcP2 calcP1 wpp))))

;; This shows that we only need P1 and P2 to get max score. Other
;; components only lead to decline in the score.
(property max-helper3 (tctrs :tctrs wtpm :wp)
  (=> (>= (params-meshMessageDeliveriesCap (cdr wtpm))
          (params-meshMessageDeliveriesThreshold (cdr wtpm)))
      (b* ((weights (car wtpm))
           (params (cdr wtpm)))
        (<= (calcScoreTopic tctrs wtpm)
            (* (params-topicweight params)
               (+ (* (first weights)
                     (calcP1 (tctrs-meshTime tctrs)
                             (params-meshTimeQuantum params)
                             (params-timeQuantaInMeshCap params)))
                  (* (second weights)
                     (calcP2 (tctrs-firstMessageDeliveries tctrs)
                             (params-p2cap params))))))))
  :hints (("Goal"
           :in-theory (enable calcScoreTopic wpp weightsp paramsp))))

;; limit on max. score, independent of counters
(property max-topic-score (tctrs :tctrs wtpm :wp)
  (=> (>= (params-meshMessageDeliveriesCap (cdr wtpm))
          (params-meshMessageDeliveriesThreshold (cdr wtpm)))
      (b* ((weights (car wtpm))
           (params (cdr wtpm)))
        (<= (calcScoreTopic tctrs wtpm)
            (* (params-topicweight params)
               (+ (* (first weights)
                     (params-timeQuantaInMeshCap params))
                  (* (second weights)
                     (params-p2cap params)))))))
  :hints (("Goal" :use (max-helper2 max-helper3)
           :in-theory (enable wpp))))

        
;; Armed with this property, we can define a function to only calculate the
;; maximum possible score.
(definecd calcMaxScoreTopic (wtpm :wp) :rational
  :skip-tests t
  :function-contract-hints (("Goal" :in-theory (enable wpp)))
  :body-contracts-hints (("Goal" :in-theory (enable wpp)))
  :ic (>= (params-meshMessageDeliveriesCap (cdr wtpm))
	  (params-meshMessageDeliveriesThreshold (cdr wtpm)))
  (b* ((weights (car wtpm))
       (params (cdr wtpm)))
    (* (params-topicweight params)
       (+ (* (first weights)
             (params-timeQuantaInMeshCap params))
          (* (second weights)
             (params-p2cap params)))
       )))


(definecd lookup-gctrs (p :peer map :p-gctrs-map) :gctrs
  :skip-tests t
  (let ((x (cdr (assoc-equal p map))))
    (match x
      (() '(0 0 0))
      (& x))))

(definecd lookup-tctrs (p :peer top :topic map :pt-tctrs-map) :tctrs
  :skip-tests t
  (let ((x (cdr (assoc-equal `(,p . ,top) map))))
    (match x
      (() (new-tctrs))
      (& x))))

(definecd lookup-score (p :peer map :peer-rational-map) :rational
  :skip-tests t
  (let ((x (cdr (assoc-equal p map))))
    (match x
      (() 0)
      (& x))))

(definecd calc-glb-score (p :peer gbmap :p-gctrs-map weights :weights)
  :rational
  :skip-tests t
  (let ((gc (lookup-gctrs p gbmap)))
    (+ (* (sixth weights) (first gc))
       (* (seventh weights) (second gc))
       (* (eighth weights) (calcP7 (third gc))))))

;; This shows that we only need P5 to get max score. Other
;; components only lead to decline in the global score.
(encapsulate
 ()
 (local
   (property glb-1
     (p :peer x :non-neg-rational y :non-neg-rational weights :weights)
     :testing? nil
     (<= (+ (* (caddr (cddddr weights))
               x)
            (* (cadddr (cddddr weights))
               y))
         0)
     :hints (("Goal" :in-theory
              (enable weightsp p-gctrs-mapp gctrsp)))
     :rule-classes :linear))
 
 (local
   (property glb-2 (p :peer gbmap :p-gctrs-map weights :weights)
     :testing? nil
     (=> gbmap
         (<= (* (calcp7 (cadddr (car gbmap)))
                (cadddr (cddddr weights)))
             0))
     :hints (("Goal" :in-theory (enable weightsp)))))

 (local
   (property glb-3 (p :peer gbmap :p-gctrs-map weights :weights)
     :testing? nil
     :check-contracts? nil
     (=> (^ gbmap
            (cdr (assoc-equal p gbmap)))
         (<= (* (caddr (cddddr weights))
                (caddr (assoc-equal p gbmap)))
             0))
     :hints (("Goal" :in-theory (enable weightsp gctrsp p-gctrs-mapp)))))

 (local
   (property glb-4 (p :peer gbmap :p-gctrs-map weights :weights)
     :testing? nil
     :check-contracts? nil
     (=> (^ (gctrsp (cdr (car gbmap)))
            (cdr (assoc-equal p gbmap))
            (acl2-numberp (cadr (assoc-equal p gbmap)))
            (acl2-numberp (caddr (assoc-equal p gbmap))))
         (<= (+ (* (caddr (cddddr weights))
                   (caddr (assoc-equal p gbmap)))
                (* (cadddr (cddddr weights))
                   (calcp7 (cadddr (assoc-equal p gbmap)))))
             0))
     :hints (("Goal" :in-theory (enable weightsp gctrsp p-gctrs-mapp)))))

 (property max-glb-score (p :peer gbmap :p-gctrs-map weights :weights)
   :testing? nil
   (<= (calc-glb-score p gbmap weights)
       (* (sixth weights) (first (lookup-gctrs p gbmap))))
   :hints (("Goal" :in-theory
            (enable weightsp gctrsp lookup-gctrs p-gctrs-mapp
                    calc-glb-score)))))

 ;; armed with the above property, we know max. achievable glb score.
(definecd calc-max-glb-score (p :peer gbmap :p-gctrs-map weights :weights)
  :rational
  :skip-tests t
  (* (sixth weights) (first (lookup-gctrs p gbmap))))

(definecd calc-topic-scores
  (pt-ctrs :pt-tctrs-map twpm :twp acc :peer-rational-map) :peer-rational-map
  :ic (is-valid-twp twpm)
  :skip-tests t
  (match pt-ctrs
    (() acc)
    ((((p . top) . ctrs) . rst)
     (b* ((tmp (lookup-score p acc))
          (wp (cdr (assoc-equal top twpm)))
          ((when (null wp)) (calc-topic-scores rst twpm acc))
          (new-score (+ (calcScoreTopic ctrs wp) tmp)))
       (calc-topic-scores rst twpm (put-assoc-equal p new-score acc))))))

(definecd add-glb-scores
  (peers :lop topic-scores :peer-rational-map gbmap
         :p-gctrs-map weights :weights topiccap :rational) :peer-rational-map
  :skip-tests t
  (match peers
    (() '())
    ((p . rst)
     `((,p . ,(+ (min topiccap (lookup-score p topic-scores)) ;;apply topic cap here
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
(property consp-twpm-weights (twpm :twp)
  (=> (consp twpm)
      (weightsp (cadar twpm))))

(property topiccap-twpm (twpm :twp)
  (=> (consp twpm)
      (rationalp (g :topiccap (cddr (car twpm)))))
  :hints (("goal" :in-theory (enable twpp wpp))))
  
(definecd calc-nbr-scores-map
  (pt-ctrs :pt-tctrs-map gbmap :p-gctrs-map twpm :twp) :peer-rational-map
  :ic (^ (consp twpm)
         (is-valid-twp twpm))
  :skip-tests t
  :body-contracts-hints (("Goal" :in-theory (enable paramsp twpp wpp)))
  (b* ((topic-scores-map (calc-topic-scores pt-ctrs twpm nil))
       (peers (peers-from-pt-tctrs-map pt-ctrs nil)))
    (add-glb-scores peers topic-scores-map gbmap (cadar twpm) (params-topiccap (cddar twpm)))))

(definecd calc-max-topic-scores
  (pt-ctrs :pt-tctrs-map twpm :twp acc :peer-rational-map)
  :peer-rational-map
  :ic (is-valid-twp twpm)
  :skip-tests t
  :body-contracts-hints (("Goal" :in-theory (enable wpp)))
  :timeout 100
  (match pt-ctrs
    (() acc)
    ((((p . top) . &) . rst)
     (b* ((tmp (lookup-score p acc))
          (wp (cdr (assoc-equal top twpm)))
          ((when (null wp)) (calc-max-topic-scores rst twpm acc))
          (new-score (+ (calcMaxScoreTopic wp) tmp)))
       (calc-max-topic-scores rst twpm (put-assoc-equal p new-score acc))))))

(definecd add-max-glb-scores
  (peers :lop topic-scores :peer-rational-map gbmap
         :p-gctrs-map weights :weights topiccap :rational)
  :peer-rational-map
  :skip-tests t
  (match peers
    (() '())
    ((p . rst)
     `((,p . ,(+ (min topiccap (lookup-score p topic-scores))
                 (calc-max-glb-score p gbmap weights)))
       . ,(add-max-glb-scores rst topic-scores gbmap weights topiccap)))))

(definecd calc-max-nbr-scores-map (pt-ctrs :pt-tctrs-map gbmap :p-gctrs-map twpm :twp) :peer-rational-map
  :ic (^ (consp twpm)
         (is-valid-twp twpm))
  :skip-tests t
  :body-contracts-hints (("Goal" :in-theory (enable paramsp twpp wpp)))
  (b* ((topic-scores-map (calc-max-topic-scores pt-ctrs twpm nil))
       (peers (peers-from-pt-tctrs-map pt-ctrs nil)))
    (add-max-glb-scores
     peers
     topic-scores-map
     gbmap
     (cadar twpm)
     (params-topiccap (cddar twpm)))))


;; Properties

;; ------------------------------------------------
;; Some interesting properties about topic-counters
;; ------------------------------------------------

(definecd nth-nneg-rat (n :nat) :nat
  (mod (1+ n) 5))

(defdata-attach non-neg-rational :enumerator nth-nneg-rat)

;;1. Increasing meshTime increases topic-score. This gives a counter example.
(acl2::must-fail
 (property mesh-time-inc-score
   (imd mmd mt fmd mfp p :non-neg-rational wtpm :wp)
   :check-contracts? nil
   :proofs? nil
   (=> (>= (params-meshMessageDeliveriesCap (cdr wtpm))
           (params-meshMessageDeliveriesThreshold (cdr wtpm)))
       (>= (calcScoreTopic (tctrs imd mmd  (+ p mt) fmd mfp) wtpm)
           (calcScoreTopic (tctrs imd mmd  mt fmd mfp) wtpm)))))

;; 2. Increasing invalidMessageDeliveries decreases score
(encapsulate
 ()
 (local 
   (property inc-calcP4 (imd p :non-neg-rational)
     (=> (< 0 p)
         (< (calcP4 imd)
            (calcP4 (+ p imd))))
     :hints (("Goal" :in-theory (enable calcP4)))
     :rule-classes :linear))

 (local
   (property neg-fifth-weight (wt :weights)
     (> 0 (fifth wt))
     :hints (("Goal" :in-theory (enable weightsp)))
     :rule-classes :linear))
          
 (property imd-dec-score
   (imd mmd mt fmd mfp p :non-neg-rational wtpm :wp)
   :check-contracts? nil
   (=> (^ (>= (params-meshMessageDeliveriesCap (cdr wtpm))
              (params-meshMessageDeliveriesThreshold (cdr wtpm)))
          (< 0 p))
       (>= (calcScoreTopic (tctrs imd mmd  mt fmd mfp) wtpm)
           (calcScoreTopic (tctrs (+ p imd) mmd  mt fmd mfp) wtpm)))
   :hints (("Goal" :in-theory (enable tctrs tctrsp calcScoreTopic calcP4
                                      wpp weightsp paramsp)))))
  

;; 3. Decreasing meshMessageDeliveries below the threshold decreases calcP3 score
(property dec-calcP3
  (meshTime :non-neg-rational
            activationWindow :nat
            meshMessageDeliveries :non-neg-rational
            meshMessageDeliveriesCap 
            meshMessageDeliveriesThreshold :nat
            p :non-neg-rational)
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
(property dec-calcP3b
  (meshTime :non-neg-rational
            activationWindow :nat
            meshFailurePenalty 
            meshMessageDeliveries :non-neg-rational
            meshMessageDeliveriesCap 
            meshMessageDeliveriesThreshold :nat
            p :non-neg-rational)
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

(property more-less-neg (a b :rational c :neg-rational)
  (=> (< b a)
      (<= (* c a)
          (* c b)))
  :rule-classes :linear)

;; (property dec-calcP3-mult (meshTime :non-neg-rational
;;                                activationWindow :nat
;;                                meshMessageDeliveries :non-neg-rational
;;                                meshMessageDeliveriesCap :nat
;;                                meshMessageDeliveriesThreshold :nat
;;                                weights :weights
;;                                p :non-neg-rational)
;;           (=> (^ (> meshTime activationWindow)
;;                  (< meshMessageDeliveries meshMessageDeliveriesThreshold)
;;                  (<= MESHMESSAGEDELIVERIESTHRESHOLD
;;                      MESHMESSAGEDELIVERIESCAP)
;;                  (< 0 p)
;;                  (< 0 (- meshMessageDeliveries p)))
;;               (>= (* (third weights)
;;                     (calcP3 meshTime activationWindow meshMessageDeliveries
;;                                meshMessageDeliveriesCap
;;                                meshMessageDeliveriesThreshold))
;;                  (* (third weights)
;;                     (calcP3 meshTime activationWindow (- meshMessageDeliveries p)
;;                                meshMessageDeliveriesCap
;;                                meshMessageDeliveriesThreshold)))))

;; (property dec-calcP3b-mult (meshTime :non-neg-rational
;;                                      activationWindow :nat
;;                                      meshFailurePenalty :non-neg-rational
;;                                meshMessageDeliveries :non-neg-rational
;;                                meshMessageDeliveriesCap :nat
;;                                meshMessageDeliveriesThreshold :nat
;;                                weights :weights
;;                                p :non-neg-rational)
;;           (=> (^ (> meshTime activationWindow)
;;                  (< meshMessageDeliveries meshMessageDeliveriesThreshold)
;;                  (<= MESHMESSAGEDELIVERIESTHRESHOLD
;;                      MESHMESSAGEDELIVERIESCAP)
;;                  (< 0 p)
;;                  (< 0 (- meshMessageDeliveries p)))
;;               (>= (* (fourth weights)
;;                      (calcP3b meshTime activationWindow
;;                               meshFailurePenalty
;;                               meshMessageDeliveries
;;                                meshMessageDeliveriesCap
;;                                meshMessageDeliveriesThreshold))
;;                  (* (fourth weights)
;;                     (calcP3b meshTime activationWindow
;;                              meshFailurePenalty
;;                              (- meshMessageDeliveries p)
;;                                meshMessageDeliveriesCap
;;                                meshMessageDeliveriesThreshold)))))

;; (property sum-gt (a :rational b :rational c :rational d :rational)
;;           (=> (^ (>= a c)
;;                  (>= b d))
;;               (>= (+ a b)
;;                   (+ c d))))


;; ;; 5. Decreasing meshMessageDeliveries below the threshold (increasing the
;; ;; deficit) decreases calcP3 + calcP3b score
;; (property dec-sum-calcP3-calcP3b (meshTime :non-neg-rational
;;                                            activationWindow :nat
;;                                            meshFailurePenalty :non-neg-rational
;;                                            meshMessageDeliveries :non-neg-rational
;;                                            meshMessageDeliveriesCap :nat
;;                                            meshMessageDeliveriesThreshold :nat
;;                                            weights :weights
;;                                            p :non-neg-rational)
;;           (=> (^ (> meshTime activationWindow)
;;                  (< meshMessageDeliveries meshMessageDeliveriesThreshold)
;;                  (<= MESHMESSAGEDELIVERIESTHRESHOLD
;;                      MESHMESSAGEDELIVERIESCAP)
;;                  (< 0 p)
;;                  (< 0 (- meshMessageDeliveries p)))
;;               (>= (+ (* (third weights)
;;                         (calcP3 meshTime activationWindow meshMessageDeliveries
;;                                 meshMessageDeliveriesCap
;;                                 meshMessageDeliveriesThreshold))
;;                      (* (fourth weights)
;;                         (calcP3b meshTime activationWindow
;;                                  meshFailurePenalty
;;                                  meshMessageDeliveries
;;                                  meshMessageDeliveriesCap
;;                                  meshMessageDeliveriesThreshold)))
;;                   (+ (* (third weights)
;;                         (calcP3 meshTime activationWindow (- meshMessageDeliveries p)
;;                                 meshMessageDeliveriesCap
;;                                 meshMessageDeliveriesThreshold))
;;                      (* (fourth weights)
;;                         (calcP3b meshTime activationWindow
;;                                  meshFailurePenalty
;;                                  (- meshMessageDeliveries p)
;;                                  meshMessageDeliveriesCap
;;                                  meshMessageDeliveriesThreshold))))))

;; (property cancel-add (a :rational b :rational c :rational d :rational)
;;           (=> (== a b)
;;               (== (>= (+ a c) (+ b d))
;;                   (>= c d))))

;; (property cancel-add2 (a :rational b :rational c :rational d :rational e
;;           :rational f :rational g :rational h :rational i :rational j :rational
;;           e2 :rational f2 :rational g2 :rational h2 :rational tw :rational)
;;           (== (<= (* tw
;;                          (+ (* a b)
;;                             (* c d)
;;                             (* e f)
;;                             (* g h)
;;                             (* i j)))
;;                       (* tw
;;                          (+ (* a b)
;;                             (* c d)
;;                             (* e2 f2)
;;                             (* g2 h2)
;;                             (* i j))))
;;                   (<= (* tw
;;                          (+ (* e f)
;;                             (* g h)))
;;                       (* tw
;;                          (+ (* e2 f2)
;;                             (* g2 h2))))))

;; (in-theory (disable calcP3 calcP3b))


;; 3. Increasing deficit decreases score.
;; (verify
;;  (=> (^ (non-neg-rationalp imd)
;;         (non-neg-rationalp mmd)
;;         (non-neg-rationalp mt)
;;         (non-neg-rationalp fmd)
;;         (non-neg-rationalp mfp)
;;         (non-neg-rationalp p)
;;         (wpp wtpm)
;;         (>= (params-meshMessageDeliveriesCap (cdr wtpm))
;;             (params-meshMessageDeliveriesThreshold (cdr wtpm)))
;;         (< 0 p)
;;         (< 0 (- mmd p))
;;         (< mmd (params-meshMessageDeliveriesThreshold (cdr wtpm))))
;;      (>= (calcScoreTopic (tctrs imd mmd  mt fmd mfp) wtpm)
;;          (calcScoreTopic (tctrs imd (- mmd p)  mt fmd mfp) wtpm))))


;; 3. Increasing deficit decreases score.
;; (property mmd-dec-score (imd :non-neg-rational mmd :non-neg-rational mt :non-neg-rational fmd
;;                                    :non-neg-rational mfp :non-neg-rational p
;;                                    :non-neg-rational wtpm :wp)
;;           :check-contracts? nil
;;           (=> (^ (>= (params-meshMessageDeliveriesCap (cdr wtpm))
;;                      (params-meshMessageDeliveriesThreshold (cdr wtpm)))
;;                  (< 0 p)
;;                  (< 0 (- mmd p))
;;                  (< mmd (params-meshMessageDeliveriesThreshold (cdr wtpm))))
;;               (>= (calcScoreTopic (tctrs imd mmd  mt fmd mfp) wtpm)
;;                   (calcScoreTopic (tctrs imd (- mmd p)  mt fmd mfp) wtpm)))
;;           :hints (("Goal" :in-theory (enable wpp tctrsp calcScoreTopic))))
