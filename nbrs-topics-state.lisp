(in-package "ACL2S")

(include-book "utils")
(include-book "scoring")

(defdata-alias pid-type symbol)
(defdata payload-type (list symbol pid-type topic peer))
(defdata lopid (listof pid-type))
(defdata lopld (listof payload-type))

(defdata verb (enum '(SND RCV)))
(defdata rpc (or (list 'CONNECT1 lot)
                 (list 'CONNECT2 lot)
                 (list 'PRUNE topic)
                 (list 'GRAFT topic)
                 (list 'SUB topic)
                 (list 'UNSUB topic)))
(defdata data (or (list 'IHAVE lopid)
                  (list 'IWANT lopid)
                  (list 'PAYLOAD payload-type)))
(defdata mssg (or rpc data))
(defdata evnt (or (cons peer (cons verb (cons peer mssg)))
                  (list peer 'JOIN topic)
                  (list peer 'LEAVE topic)
                  (list peer verb peer 'CONNECT1 lot)
                  (list peer verb peer 'CONNECT2 lot)
                  (list peer 'HBM pos-rational)
                  (list peer 'APP topic payload-type)))
;; In the case above, the topic is redundant.
(defdata loev (listof evnt))
(defdata (peer-lot-map   (alistof peer lot)))
(defdata (topic-lop-map  (alistof topic lop)))
(defdata (topic-pos-rat-map  (alistof topic non-neg-rational)))

;;The following state deals with neighbours and topics they follow,
;; hence we call it (n)eighbour(t)opic-state or nbr-topic-state
(defdata nbr-topic-state
  (record (nbr-topicsubs . topic-lop-map) ;; peer's neighbours'
          ;; subscriptions, includes peer itself, tokeep track of its own subs
          ;; let's keep this as topic-lop-map, as its convenient, no need to
          ;; invert map
          (topic-fanout  . topic-lop-map) ;; peer doesn't subscribe to these
          ;; topics
          (last-pub      . topic-pos-rat-map) ;; last published time for a topic
          (topic-mesh    . topic-lop-map)))  ;; peer subscribes to these topics
;; and is part of corresponding topic mesh

(property nbr-topic-state-check-prop (x :nbr-topic-state)
  (^ (topic-lop-mapp (g :nbr-topicsubs x))
     (topic-lop-mapp (g :topic-fanout x))
     (topic-pos-rat-mapp (g :last-pub x))
     (topic-lop-mapp (g :topic-mesh x)))
  :hints (("Goal" :in-theory (enable nbr-topic-statep)))
  :rule-classes :forward-chaining)

(definec new-nbr-topic-state () :nbr-topic-state
  (nbr-topic-state nil nil nil nil))

(definec dist-cons (x :cons) :tl
  (cond ((atom (cdr x)) nil)
        (t (cons `(,(car x) . ,(cadr x))
                 (dist-cons `(,(car x) . ,(cddr x)))))))

(check= (dist-cons '(1 2 3)) '((1 . 2) (1 . 3)))

(create-map* dist-cons alistp tlp)

(defdata txp (cons topic peer))
(defdata lotopicpeer (alistof topic peer))
(property lotopicpeer-type (x :lotopicpeer)
  (=> x
      (^ (txpp (car x))
         (topicp (car (car x)))
         (peerp (cdr (car x)))))
  :rule-classes ((:forward-chaining :trigger-terms ((lotopicpeerp x)))))

(property dist-cons-tlm (ls :topic-lop-map)
  (=> ls
      (lotopicpeerp (dist-cons (car ls)))))
(in-theory (disable txpp lotopicpeerp))

(definecd flatten-topic-lop-map (ls :topic-lop-map) :lotopicpeer
  (match ls
    (() '())
    ((l . rst) (app (dist-cons l)
                    (flatten-topic-lop-map rst)))))

(definecd peer-subs (top-peers :topic-lop-map self :peer) :lot
  (match top-peers
    (() '())
    (((top . subscribers) . rst)
     (if (member-equal self subscribers)
         (cons top (peer-subs rst self))
       (peer-subs rst self)))))

;; following invariants should hold for a peer nbr-topic-state
(definec nbr-topic-state-invp (ps :nbr-topic-state) :boolean
;peer doesn't subscribe to topics for which it forwards msgs to fanout
  (^ (! (intersectp-equal (acl2::alist-keys (nbr-topic-state-topic-mesh ps))
                          (acl2::alist-keys (nbr-topic-state-topic-fanout ps))))
;peer knows what topics are subscribed to by nbrs in fanout
     (subsetp-equal (flatten-topic-lop-map (nbr-topic-state-topic-fanout ps))
                    (flatten-topic-lop-map (nbr-topic-state-nbr-topicsubs ps)))
;peer knows what topics are subscribed to by nbrs in each topic mesh
     (subsetp-equal (flatten-topic-lop-map (nbr-topic-state-topic-mesh ps))
                    (flatten-topic-lop-map (nbr-topic-state-nbr-topicsubs ps)))))

(defconst *MX-STATE*
  (nbr-topic-state '((FM P2) (SEC P2) (DS MX) (SE AN))
                   '((SE AN))
                   '((SE . 4))
                   '((FM P2))))

(check= (nbr-topic-state-invp *MX-STATE*) t)

(definecd add-sub (top-peers :topic-lop-map nbr :peer topic :topic)
  :topic-lop-map
  (let ((old-subscribers (cdr (assoc-equal topic top-peers))))
    (put-assoc-equal topic (cons nbr old-subscribers) top-peers)))

(create-reduce* (lambda (tp tmp p) (add-sub tmp p tp))
                topic-lop-mapp
                lotp
                (:name add-peer-subs)
                (:fixed-vars ((peerp p))))

(property assoc-mesh (topic :topic top-peers :topic-lop-map)
  (lopp (cdr (assoc-equal topic top-peers))))
          
(definecd rem-sub (top-peers :topic-lop-map nbr :peer topic :topic)
  :topic-lop-map
  :body-contracts-hints (("Goal" :in-theory (enable topic-lop-mapp)))
  (let ((old-subscribers (cdr (assoc-equal topic top-peers))))
    (put-assoc-equal topic (remove-equal nbr old-subscribers) top-peers)))


(create-reduce* (lambda (nbr tmp topic) (add-sub tmp nbr topic))
                topic-lop-mapp
                lopp
                (:name add-subs)
                (:fixed-vars ((topicp topic))))

(sig remove-assoc-equal (:a (alistof :a :b)) => (alistof :a :b))
(sig put-assoc-equal (:a :b (alistof :a :b)) => (alistof :a :b))
          
(definecd update-topic-mesh
  (nts :nbr-topic-state new-topic-mesh :topic-lop-map) :nbr-topic-state
  (s :topic-mesh new-topic-mesh nts))

(definecd update-last-pub
  (nts :nbr-topic-state new-last-pub :topic-pos-rat-map) :nbr-topic-state
  (s :last-pub new-last-pub nts))

(definecd update-nbr-topicsubs
  (nbr-topic-state :nbr-topic-state new-nbr-topicsubs :topic-lop-map) :nbr-topic-state
  (s :nbr-topicsubs new-nbr-topicsubs nbr-topic-state))

(definecd update-topic-fanout
  (nbr-topic-state :nbr-topic-state new-topic-fanout :topic-lop-map) :nbr-topic-state
  (s :topic-fanout new-topic-fanout nbr-topic-state))

(definecd remove-peer-submap (tmp :topic-lop-map nbr :peer) :topic-lop-map
  (match tmp
    (() '())
    (((tp . ps) . rst)
     `((,tp . ,(remove-equal nbr ps)) . 
       ,(remove-peer-submap rst nbr)))))

(create-reduce*
 (lambda (p tmp) (remove-peer-submap tmp p))
 topic-lop-mapp
 lopp
 (:name remove-subbed-peers))


(create-filter* (lambda (ps) (< (cdr ps) 0))
                peer-rational-mapp
                (:name lt-0-filter))

(create-filter* (lambda (ps s) (> (cdr ps) s))
                peer-rational-mapp
                (:name gt-s-filter)
                (:fixed-vars ((rationalp s))))

(sig shuffle ((listof :a) nat) => (listof :a))
(sig grab (nat (listof :a)) => (listof :a))

(in-theory (disable shuffle grab filter*-*lt-0-filter filter*-*gt-s-filter))

(definecd remove-excess-mesh (mesh :topic-lop-map dhigh d s :nat)
  :topic-lop-map
  (b* (((mv & s) (defdata::genrandom-seed
                   (1- (expt 2 31))
                   (mod s (expt 2 31)))))
    (match mesh
      (() '())
      (((tp . nbrs) . rst)
       (cons (if (< dhigh (len nbrs))
                 `(,tp . ,(grab d (shuffle nbrs s)))
               (car mesh))
             (remove-excess-mesh rst dhigh d s))))))

; PETE: better rule
#|
(property caar-mesh-topicp (mesh :topic-lop-map)
  (=> (consp (car mesh))
      (topicp (car (car mesh)))))
|#

(property caar-mesh-topicp (mesh :topic-lop-map)
  (=> (consp mesh)
      (topicp (caar mesh))))

(property add-mesh-nbrs-help (candidates :topic-lop-map mesh :all)
  :check-contracts? nil
  (=> (topic-lop-mapp (cdr mesh))
      (tlp (cdr (assoc-equal (caar mesh) candidates)))))

(definecd add-mesh-nbrs
  (mesh candidates :topic-lop-map dlow d :nat) :topic-lop-map
  :ic (<= dlow d)
  (match mesh
    (() '())
    (((tp . nbrs) . rst)
     (cons (if (<= (len nbrs) dlow)
               `(,tp . ,(app nbrs
                             (grab (- d (len nbrs))
                                   (cdr (assoc-equal tp candidates)))))
             (car mesh))
           (add-mesh-nbrs rst candidates dlow d)))))

(create-tuple* nbr-topic-state lop)

(property nbr-topicsubs-lookup (subs :peer-lot-map p :peer)
  (lotp (cdr (assoc-equal p subs))))

(property topic-mesh-lookup (topic-mesh :topic-lop-map tp :topic)
  (lopp (cdr (assoc-equal tp topic-mesh))))

(encapsulate
 ()
 (local
   (property meshtime-ctr-help2
     (elapsed :pos-rational mesh-topic-nbrs :lotopicpeer
              nbr-counters :pt-tctrs-map)
     (=> mesh-topic-nbrs
         (non-neg-rationalp
          (g :meshtime (lookup-tctrs (cdr (car mesh-topic-nbrs))
                                        (car (car mesh-topic-nbrs))
                                        nbr-counters))))
     :hints (("goal" :do-not-induct t
              :use ((:instance lotopicpeer-type (x mesh-topic-nbrs))) 
              :in-theory (disable lookup-tctrs txpp)))))

 (local
   (property nn+pos (a :pos-rational b :non-neg-rational)
     (non-neg-rationalp (+ a b))))

 (local
   (property meshtime-ctr-help3 (elapsed :pos-rational mesh-topic-nbrs :lotopicpeer
                                         nbr-counters :pt-tctrs-map)
     (=> mesh-topic-nbrs
         (non-neg-rationalp (+ elapsed
                               (g :meshtime (lookup-tctrs (cdr (car mesh-topic-nbrs))
                                                             (car (car mesh-topic-nbrs))
                                                             nbr-counters)))))
     :hints (("goal" :do-not-induct t
              :use (meshtime-ctr-help2)
              :in-theory (disable lookup-tctrs txpp)))))

 (definecd update-mesh-times-counters
   (nbr-counters :pt-tctrs-map mesh-topic-nbrs :lotopicpeer elapsed :pos-rational) :pt-tctrs-map
   (if (endp mesh-topic-nbrs)
       nbr-counters
     (b* ((p (cdar mesh-topic-nbrs))
          (top (caar mesh-topic-nbrs))
          (tmp (lookup-tctrs p top nbr-counters)))
       (update-mesh-times-counters
        (put-assoc-equal (cons p top)
                         (update-meshTime tmp
                                          (+ elapsed
                                             (tctrs-meshTime tmp)))
                         nbr-counters)
        (cdr mesh-topic-nbrs)
        elapsed)))))

(encapsulate
 ()
 (local
   (property mget-mmd-default (top :topic nbr :peer)
     (== (g :meshmessagedeliveries
               (lookup-tctrs nbr top nil))
         0)
     :hints (("goal" :in-theory (enable lookup-tctrs)))))

 (local
   (property plus-mfp-exptdeficit-posratp
     (meshmessagedeliveriesthreshold meshmessagedeliveriescap :nat
                                     top :topic nbr :peer nbr-counters :pt-tctrs-map)
     (=> (<= meshmessagedeliveriesthreshold
             meshmessagedeliveriescap)
         (non-neg-rationalp
          (+
           (g :meshfailurepenalty (lookup-tctrs nbr top nbr-counters))
           (expt
            (calc-deficit
             (g :meshmessagedeliveries (lookup-tctrs nbr top nbr-counters))
             meshmessagedeliveriescap
             meshmessagedeliveriesthreshold)
            2))))
     :hints (("goal" :in-theory (enable lookup-tctrs)))))

 (definecd retain-mesh-failure-counters
   (nbr-counters :pt-tctrs-map nbr :peer top :topic
                 meshMessageDeliveriesCap meshMessageDeliveriesThreshold :nat)
   :pt-tctrs-map
   :skip-tests t
   :ic (>= meshMessageDeliveriesCap
           meshMessageDeliveriesThreshold)
   (b* ((counters (lookup-tctrs nbr top nbr-counters))
        (dfct     (calc-deficit (tctrs-meshMessageDeliveries counters)
                                meshMessageDeliveriesCap
                                meshMessageDeliveriesThreshold))
        (new-meshFailurePenalty (+ (tctrs-meshFailurePenalty counters)
                                   (* dfct dfct))))
     (put-assoc-equal (cons nbr top)
                      (update-meshFailurePenalty counters new-meshFailurePenalty)
                      nbr-counters))
   :body-contracts-hints (("Goal" :use plus-mfp-exptdeficit-posratp))))

(check=
 (retain-mesh-failure-counters
  '(((MX . FM)
     (:0TAG . TCTRS)
     (:FIRSTMESSAGEDELIVERIES . 1)
     (:INVALIDMESSAGEDELIVERIES . 3)
     (:MESHFAILUREPENALTY . 0)
     (:MESHMESSAGEDELIVERIES . 1)
     (:MESHTIME . 0)))
  'MX
  'FM
  3
  2)
 '(((MX . FM)
    (:0TAG . TCTRS)
    (:FIRSTMESSAGEDELIVERIES . 1)
    (:INVALIDMESSAGEDELIVERIES . 3)
    (:MESHFAILUREPENALTY . 1)
    (:MESHMESSAGEDELIVERIES . 1)
    (:MESHTIME . 0))))

(definecd retain-multiple-mesh-failure-counters
  (nbr-counters-map :pt-tctrs-map tp-nbr-lst :lotopicpeer
                    meshMessageDeliveriesCap meshMessageDeliveriesThreshold :nat)
  :pt-tctrs-map
  :skip-tests t  
  :ic (>= meshMessageDeliveriesCap
          meshMessageDeliveriesThreshold)
  (if (endp tp-nbr-lst)
      nbr-counters-map
    (retain-multiple-mesh-failure-counters
     (retain-mesh-failure-counters
      nbr-counters-map (cdar tp-nbr-lst) (caar tp-nbr-lst)
      meshMessageDeliveriesCap meshMessageDeliveriesThreshold)
     (cdr tp-nbr-lst)
     meshMessageDeliveriesCap
     meshMessageDeliveriesThreshold)))

(in-theory (enable lotopicpeerp))

(create-filter* (lambda (p nbrs) (in (cdr p) nbrs))
                lotopicpeerp
                (:name remvd-topic-nbr)
                (:fixed-vars ((lopp nbrs))))

(check= (filter* remvd-topic-nbr '((FM . P2) (SE . MX)
                                   (DS . P3))
                 '(MX))
        '((SE . MX)))

(create-map* (lambda (tp p) `(,p SND ,(cdr tp) GRAFT ,(car tp)))
             lotopicpeerp
             loevp
             (:name mk-grafts)
             (:fixed-vars ((peerp p))))

(create-map* (lambda (tp p) `(,p SND ,(cdr tp) PRUNE ,(car tp)))
             lotopicpeerp
             loevp
             (:name mk-prunes)
             (:fixed-vars ((peerp p))))

(create-map* (lambda (tp p) `(,p SND ,(cdr tp) SUB ,(car tp)))
             lotopicpeerp
             loevp
             (:name mk-subs)
             (:fixed-vars ((peerp p))))

(create-map* (lambda (tp p) `(,p SND ,(cdr tp) UNSUB ,(car tp)))
             lotopicpeerp
             loevp
             (:name mk-unsubs)
             (:fixed-vars ((peerp p))))

(in-theory (disable filter*-*remvd-topic-nbr map*-*mk-grafts map*-*mk-prunes
                    map*-*mk-subs map*-*mk-unsubs))

(definecd age-lastpub (lastpub :topic-pos-rat-map elapsed :pos-rational) :topic-pos-rat-map
  (match lastpub
    (() '())
    (((tp . age) . rst) (cons `(,tp . ,(+ age elapsed))
                              (age-lastpub rst elapsed)))))

(property topic-subs-assoc (topic-subs :topic-lop-map lastpub :topic-pos-rat-map)
  (=> lastpub
      (lopp (cdr (assoc-equal (car (car lastpub))
                              topic-subs)))))

; Pete: horrible rewrite rule
(property lopp->tlp (x :lop)
  (tlp x)
  :rule-classes ((:rewrite :backchain-limit-lst 2)))

(definecd prune-fanout
  (lastpub :topic-pos-rat-map tfanout topic-subs :topic-lop-map
           fanoutttl :pos-rational d :nat) :topic-lop-map
  :skip-tests t
  (match lastpub
    (() tfanout)
    (((tp . age) . rst) (if (> age fanoutttl)
                            (prune-fanout rst
                                          (remove-assoc-equal tp tfanout)
                                          topic-subs
                                          fanoutttl
                                          d)
                          (b* ((fanout (cdr (assoc-equal tp tfanout)))
                               (l (len fanout))
                               ((unless (< l d))
                                (prune-fanout rst tfanout topic-subs fanoutttl d))
                               (nf (grab (- d l) (set-difference-equal
                                                  (cdr (assoc-equal tp topic-subs))
                                                  fanout))))
                            (prune-fanout rst
                                          (reduce* add-subs tfanout nf
                                                   tp)
                                          topic-subs
                                          fanoutttl
                                          d))))))

(definecd fanout-maintenance
  (nts :nbr-topic-state topic-subs :topic-lop-map
       elapsed fanoutttl :pos-rational d :nat) :nbr-topic-state
  (b* ((new-last-pub (age-lastpub (nbr-topic-state-last-pub nts) elapsed))
       (new-nts (update-last-pub nts new-last-pub)))
    (update-topic-fanout new-nts (prune-fanout new-last-pub
                                               (nbr-topic-state-topic-fanout nts)
                                               topic-subs
                                               fanoutttl
                                               d))))

(property assoc-nbr-topic-state (nts :nbr-topic-state top :topic)
  (lopp (cdr (assoc-equal top (nbr-topic-state-topic-mesh nts)))))

(definecd nbrs-not-in-mesh (nts :nbr-topic-state top :topic) :lop
  (b* ((topic-meshes (nbr-topic-state-topic-mesh nts))
       (topic-mesh-nbrs (cdr (assoc-equal top topic-meshes)))
       ((unless (consp topic-mesh-nbrs)) nil)
       (topic-nbrs (cdr (assoc-equal top (nbr-topic-state-nbr-topicsubs nts)))))
    (set-difference-equal topic-nbrs topic-mesh-nbrs)))

(property extract-keys-nbr-scores (peers :lop nbr-scores :peer-rational-map)
  (peer-rational-mapp (extract-keys peers nbr-scores))
  :hints (("Goal" :in-theory (enable extract-keys))))

(property extract-cars-nbr-scores (nbr-scores :peer-rational-map)
  (lopp (strip-cars nbr-scores)))

(property extract-cdrs-nbr-scores (nbr-scores :peer-rational-map)
  (lorp (strip-cdrs nbr-scores)))

(property dist-cons-top-peers (top :topic peers :lop)
  (lotopicpeerp (dist-cons (cons top peers))))

(definecd opportunistic-grafting-topic
  (p1 :peer top :topic nts
      :nbr-topic-state nbr-scores :peer-rational-map params :params)
  :loev
  (b* ((ogt (params-opportunisticGraftThreshold params))
       (not-mesh-peers (nbrs-not-in-mesh nts top))
       ((unless (consp not-mesh-peers)) nil)
       (mesh-scores (strip-cdrs nbr-scores))
       ((unless (consp mesh-scores)) nil)
       (median-score (median mesh-scores))
       ((unless (< median-score ogt)) nil)
       (eligible-for-grafting
        (strip-cars
         (filter* gt-s-filter
                  (extract-keys not-mesh-peers nbr-scores)
                  median-score))))
    (map* mk-grafts (dist-cons `(,top . ,eligible-for-grafting)) p1)))

(definecd opportunistic-grafting
  (p1 :peer tops :lot nts :nbr-topic-state
      nbr-scores :peer-rational-map params :params)
  :loev
  :skip-tests t
  :ic (^ (>= (params-meshMessageDeliveriesCap params)
             (params-meshMessageDeliveriesThreshold params))
         (<= (params-dlow params) (params-d params))
         (>= (params-dhigh params) (params-d params)))
  (match tops
    (() '())
    ((top . rst) (app (opportunistic-grafting-topic p1 top nts nbr-scores params)
                      (opportunistic-grafting p1 rst nts nbr-scores
                                              params)))))

(property flat-vals-topic-lops (al :topic-lop-map)
  (lopp (acl2s::flatten (acl2::alist-vals al))))

(sig set-difference-equal ((listof :a) (listof :a)) => (listof :a))

(create-tuple* nbr-topic-state loev pt-tctrs-map p-gctrs-map peer-rational-map)

(definecd rem-val-peers (al :topic-lop-map rl :lop) :topic-lop-map
  (match al
    (() '())
    (((k . v) . rst)
     (cons `(,k . ,(set-difference-equal v rl))
           (rem-val-peers rst rl)))))

(create-reduce* (lambda (tp-ps tmp) (app tmp (cdr tp-ps)))
                lopp
                topic-lop-mapp
                (:name subscribers))

(check= (reduce* subscribers
                 nil
                 '((T1 P1 P2 P3)
                   (T2 P4 P5 P1)))
        '(P4 P5 P1 P1 P2 P3))

(create-reduce* (lambda (tp-ps tmp p) (if (in p (cdr tp-ps))
                                          (cons (car tp-ps) tmp)
                                        tmp))
                lotp
                topic-lop-mapp
                (:name subscriptions)
                (:fixed-vars ((peerp p))))

(check= (reduce* subscriptions
                 nil
                 '((FM ANKIT MAX)
                   (SEC MAX))
                 'MAX)
        '(FM SEC))

(in-theory (disable evntp reduce*-*subscribers reduce*-*subscriptions
                    NBR-TOPIC-STATEP paramsp pt-tctrs-mapp p-gctrs-mapp
                    peer-rational-mapp twpp))

(property cars-topic-lop-map (tlm :topic-lop-map)
  (lotp (strip-cars tlm)))

(definecd rem-peers-topic-lop-map (al :topic-lop-map rl :lop) :topic-lop-map
  (match al
    (() '())
    (((k . v) . rst)
     (cons `(,k . ,(set-difference-equal v rl))
           (rem-peers-topic-lop-map rst rl)))
    (& nil)))

; Pete, better rewrite rule
#|
(defthm evnt-trx1
  (=> (evntp (list P1 'RCV P2 'GRAFT TP))
      (evntp (list P1 'SND P2 'PRUNE TP)))
  :hints (("Goal" :in-theory (enable evntp)))
  :rule-)
|#

(defthm evnt-trx1
  (== (evntp (list P1 'SND P2 'PRUNE TP))
      (evntp (list P1 'RCV P2 'GRAFT TP)))
  :hints (("Goal" :in-theory (enable evntp))))

; Pete limit rewriting
(defthm evnt-trx2
  (=> (^ (lotp tps)
         (evntp (list P1 'RCV P2 'CONNECT1 TP)))
      (evntp (list P1 'SND P2 'CONNECT2 tps)))
  :hints (("Goal" :in-theory (enable evntp)))
  :rule-classes ((:rewrite :backchain-limit-lst 1)))


; PETE: better rule
#|
(defthm assoc-twp-h1
  (=> (^ (topicp top)
         (twpp twpm)
         (not (consp (cdr (assoc-equal top twpm)))))
      (not (cdr (assoc-equal top twpm))))
    :hints (("goal" :in-theory (enable twpp))))
|#

(property assoc-twp-h1 (top :all twpm :twp)
  (iff (consp (cdr (assoc-equal top twpm)))
       (cdr (assoc-equal top twpm)))
  :hints (("goal" :in-theory (enable twpp))))

(definecd update-nbr-topic-state1
  (nts :nbr-topic-state
       nbr-scores :peer-rational-map
       tcmap :pt-tctrs-map
       gcmap :p-gctrs-map
       evnt :evnt
       twpm :twp
       s :nat)
  :nbr-topic-stateloevpt-tctrs-mapp-gctrs-mappeer-rational-map
  :skip-tests t
  :timeout 600
  :ic (is-valid-twp twpm)
  :body-contracts-hints (("goal" :do-not-induct t
                          :in-theory (enable evntp twpp wpp)
                          ))
  (b* ((topic-subs (nbr-topic-state-nbr-topicsubs nts))
       (topic-meshes (nbr-topic-state-topic-mesh nts))
       (mesh-nbrs (reduce* subscribers nil topic-meshes))
       (defaultres (list nts nil tcmap gcmap nbr-scores)))
    (match evnt
      ((& 'RCV nbr 'SUB topic)
       (list (update-nbr-topicsubs
              nts
              (add-sub topic-subs nbr topic))
             nil
             tcmap
             gcmap
             nbr-scores))
      ;;UNSUB implies PRUNE, so we also prune the nbr if it is in a mesh
      ;;Whenever we prune, we need to reset counters for nbr, but retain meshFailurePenalty
      ;;If nbr is not in mesh, then default behaviour
      ((& 'RCV nbr 'UNSUB topic)
       (b* ((new-nts
             (update-nbr-topicsubs
              nts
              (rem-sub topic-subs nbr topic)))
            (wp (cdr (assoc-equal topic twpm)))
            (params (cdr wp))
            ((when (null params)) defaultres)
            ((when (in nbr mesh-nbrs)) (list
                                        (update-topic-mesh
                                         new-nts
                                         (rem-sub topic-meshes nbr topic))
                                        nil
                                        (retain-mesh-failure-counters
                                         tcmap nbr topic
                                         (params-meshMessageDeliveriesCap params)
                                         (params-meshMessageDeliveriesThreshold
                                          params))
                                        gcmap
                                        nbr-scores)))
         defaultres))
      ;;TODO : need to check for nbr score before grafting, and whether it is
      ;;in BACKOFF
      ;;GRAFT implies SUB, so we also add nbr subscription
      ((p1 'RCV nbr 'GRAFT topic)
       (cond
        ((^ (in topic (topics))
            (! (in p1 (cdr (assoc-equal topic topic-subs)))))
         (list nts
               ;; p1 not subscribed to topic, send PRUNE
               `((,p1 SND ,nbr PRUNE ,topic))
               tcmap gcmap nbr-scores))
        ((^ (in topic (topics))
            (>= 0 (lookup-score nbr nbr-scores)))
         (list (update-topic-mesh
                (update-nbr-topicsubs
                 nts
                 (add-sub topic-subs nbr topic))
                (add-sub topic-meshes nbr topic))
               nil
               tcmap
               gcmap
               nbr-scores))
        (t defaultres)))
      ((& 'RCV nbr 'PRUNE topic)
       (b* ((wp (cdr (assoc-equal topic twpm)))
            (params (cdr wp))
            ((when (null params)) defaultres))
         (list (update-topic-mesh
                nts
                (rem-sub topic-meshes nbr topic))
               nil
               (retain-mesh-failure-counters tcmap
                                             nbr
                                             topic
                                             (params-meshMessageDeliveriesCap
                                              params)
                                             (params-meshMessageDeliveriesThreshold
                                              params))
               gcmap
               nbr-scores)))
      ((p1 'SND & 'SUB topic)
       (list (update-nbr-topicsubs
              nts
              (add-sub topic-subs p1 topic))
             nil
             tcmap
             gcmap
             nbr-scores))
      ;;UNSUB implies PRUNE, so we also prune the nbr (topic key removed from mesh map)
      ;;If nbr is not in mesh, then default behaviour
      ;; no need to send PRUNE
      ((p1 'SND & 'UNSUB topic)
       (list (update-nbr-topicsubs
              nts
              (rem-sub topic-subs p1 topic))
             nil
             tcmap
             gcmap
             nbr-scores))
      ;;need to check for nbr score before grafting
      ((& 'SND nbr 'GRAFT topic)
       (list (update-topic-mesh
              nts
              (add-sub topic-meshes nbr topic))
             nil
             tcmap
             gcmap
             nbr-scores))
      ((& 'SND nbr 'PRUNE topic)
       (b* ((wp (cdr (assoc-equal topic twpm)))
            (params (cdr wp))
            ((when (null params)) defaultres))
         (list (update-topic-mesh
                nts
                (rem-sub topic-meshes nbr topic))
               nil
               (retain-mesh-failure-counters tcmap
                                             nbr
                                             topic
                                             (params-meshMessageDeliveriesCap
                                              params)
                                             (params-meshMessageDeliveriesThreshold
                                              params))
               gcmap
               nbr-scores)))
      ((self 'RCV p1 'CONNECT1 tps)
       (list (update-nbr-topicsubs nts (reduce* add-peer-subs topic-subs tps p1))
             `((,self SND ,p1 CONNECT2 ,(peer-subs topic-subs self)))
             tcmap
             gcmap
             nbr-scores))
      ((& 'RCV p1 'CONNECT2 tps)
       (list (update-nbr-topicsubs nts (reduce* add-peer-subs topic-subs tps p1))
             nil
             tcmap
             gcmap
             nbr-scores))
      (& defaultres))))

(definecd update-nbr-topic-state2 (nts :nbr-topic-state
                                       nbr-scores :peer-rational-map
                                       tcmap :pt-tctrs-map
                                       gcmap :p-gctrs-map
                                       evnt :evnt
                                       twpm :twp
                                       s :nat)
  :nbr-topic-stateloevpt-tctrs-mapp-gctrs-mappeer-rational-map
  :skip-tests t
  :ic (is-valid-twp twpm)
  :body-contracts-hints (("goal" :do-not-induct t
                          :in-theory (enable evntp twpp wpp)))
  (b* ((topic-subs (nbr-topic-state-nbr-topicsubs nts))
       (topic-meshes (nbr-topic-state-topic-mesh nts))
       (nbrs (reduce* subscribers nil topic-subs))
       (defaultres (list nts nil tcmap gcmap nbr-scores)))
    (match evnt
      ((p1 'JOIN tp) (b* ((wp (cdr (assoc-equal tp twpm)))
                          (params (cdr wp))
                          ((when (null params)) (list nts nil tcmap gcmap nbr-scores))
                          (d (params-d params))
                          (tpnbrs (remove p1 (cdr (assoc-equal tp topic-subs))))
                          (newnbrs (grab d (shuffle tpnbrs s)))
                          (newmesh
                           `((,tp . ,newnbrs) . ,(remove-assoc-equal tp topic-meshes))))
                       (list (update-topic-mesh
                              (update-nbr-topicsubs nts (add-sub topic-subs p1 tp))
                              newmesh)
                             (app
                              (map* mk-grafts
                                    (flatten-topic-lop-map `((,tp . ,newnbrs))) p1)
                              (map* mk-subs
                                    (flatten-topic-lop-map `((,tp . ,nbrs))) p1))
                             tcmap gcmap nbr-scores)))
      ((p1 'LEAVE tp) (list (update-topic-mesh
                             (update-nbr-topicsubs nts (rem-sub topic-subs p1 tp))
                             (remove-assoc-equal tp topic-meshes))
                            ;;unsubs imply PRUNE
                            (map* mk-unsubs
                                  (flatten-topic-lop-map `((,tp . ,nbrs))) p1)
                            tcmap gcmap nbr-scores))
      (& defaultres))))

(defthm twp-dlow-int
  (=> (^ (twpp twpm) twpm)
      (INTEGERP (G :DLOW (CDDR (CAR TWPM)))))
  :hints (("Goal" :in-theory (enable twpp paramsp))))

(defthm twp-MESHMESSAGEDELIVERIESCAP-nat
  (=> (^ (twpp twpm) twpm)
      (ACL2-NUMBERP (G :MESHMESSAGEDELIVERIESCAP (CDDR (CAR TWPM)))))
  :hints (("Goal" :in-theory (enable twpp paramsp))))

(defthm twp-cddar-paramsp
  (=> (^ (twpp twpm) twpm)
      (PARAMSP (CDDR (CAR TWPM))))
  :hints (("Goal" :in-theory (enable twpp))))

(defthm dlow-number
  (=> (^ (twpp twpm) twpm)
      (ACL2-NUMBERP (G :D (CDDR (CAR TWPM)))))
  :hints (("Goal" :in-theory (enable twpp paramsp))))

(defthm dlow-lt-d-twp
  (=> (^ (twpp twpm)
         (is-valid-twp twpm)
         (ACL2-NUMBERP (G :D (CDDR (CAR TWPM)))))
      (<= (G :DLOW (CDDR (CAR TWPM)))
          (G :D (CDDR (CAR TWPM)))))
  :hints (("Goal" :in-theory (enable is-valid-twp wpp paramsp))))

(defthm d-lt-dhigh-twp
  (=> (^ (twpp twpm)
         (is-valid-twp twpm)
         (ACL2-NUMBERP (G :D (CDDR (CAR TWPM)))))
      (<= (g :d (cddr (car twpm)))
          (g :dhigh (cddr (car twpm)))))
  :hints (("Goal" :in-theory (enable is-valid-twp wpp paramsp))))

(defthm mmdt-lt-mmdc-twp
  (=> (^ (twpp twpm)
         (is-valid-twp twpm)
         (ACL2-NUMBERP (G :D (CDDR (CAR TWPM)))))
      (<= (g :meshmessagedeliveriesthreshold (cddr (car twpm)))
          (g :meshmessagedeliveriescap (cddr (car twpm)))))
  :hints (("Goal" :in-theory (enable is-valid-twp wpp paramsp))))

(defthm dlow-gt-0-twp
  (=> (^ (twpp twpm)
         (is-valid-twp twpm)
         (ACL2-NUMBERP (G :D (CDDR (CAR TWPM)))))
      (<= 0 (G :DLOW (CDDR (CAR TWPM)))))
  :hints (("Goal" :in-theory (enable is-valid-twp wpp paramsp))))

(property cons-twpm (twpm :twp)
  (=> (consp twpm)
      (wpp (cdar twpm)))
  :hints (("Goal" :in-theory (enable twpp))))

(definecd update-nbr-topic-state3 (nts :nbr-topic-state
                                       nbr-scores :peer-rational-map
                                       tcmap :pt-tctrs-map
                                       gcmap :p-gctrs-map
                                       evnt :evnt
                                       twpm :twp
                                       s :nat)
  :nbr-topic-stateloevpt-tctrs-mapp-gctrs-mappeer-rational-map
  :skip-tests t
  :timeout 60
  :ic (is-valid-twp twpm)
  :body-contracts-hints (("Goal" :DO-NOT-INDUCT T
                          :in-theory (enable evntp twpp wpp)))
  (b* ((topic-subs (nbr-topic-state-nbr-topicsubs nts))
       (topic-meshes (nbr-topic-state-topic-mesh nts))
       (mesh-nbrs (reduce* subscribers nil topic-meshes))
       (defaultres (list nts nil tcmap gcmap nbr-scores)))
    (match evnt
      ((p1 'HBM elapsed) (b* ((wp (cdar twpm))
                              (params (cdr wp))
                              ((when (null params)) defaultres)
                              (nbr-scores (calc-nbr-scores-map tcmap gcmap twpm))
                              (remvd (strip-cars (filter* lt-0-filter
                                                          nbr-scores)))
                              ;;score based removal
                              (mesh1 (reduce* remove-subbed-peers topic-meshes
                                              remvd))
                              
                              ;;removal due to dhigh
                              (mesh2 (remove-excess-mesh mesh1
                                                         (params-dhigh params)
                                                         (params-d params)
                                                         s))
                              
                              ;; these can not be added to mesh
                              (disqualified-mesh-nbrs (app mesh-nbrs
                                                           (cons p1 remvd)))
                              
                              ;; addition due to dlow
                              ;; only one of these 2 will happen, addition or
                              ;; reduction
                              ;; new peers to be added in the mesh will come
                              ;; from the map of topic-subs\disqualified-nbrs
                              (mesh3 (add-mesh-nbrs mesh2
                                                    (rem-peers-topic-lop-map topic-subs
                                                                             disqualified-mesh-nbrs)
                                                    (params-dlow params)
                                                    (params-d params)))
                              ;;topic-peer alist which is to be pruned
                              (tps-rem (app (filter* remvd-topic-nbr
                                                     (flatten-topic-lop-map topic-meshes)
                                                     remvd) ;; score
                                            (set-difference-equal
                                             (flatten-topic-lop-map mesh1)
                                             (flatten-topic-lop-map mesh2)))) ;;extras
                              ;;topic-peer alist which is to be grafted
                              (tps-gr (set-difference-equal
                                       (flatten-topic-lop-map mesh3)
                                       (flatten-topic-lop-map mesh2)))
                              (newnts (update-topic-mesh nts mesh3))
                              (newnts (fanout-maintenance newnts
                                                          topic-subs
                                                          elapsed
                                                          (params-fanoutTTL
                                                           params)
                                                          (params-d params))))
                           (list newnts
                                 (app (map* mk-prunes tps-rem p1)
                                      (map* mk-grafts tps-gr p1)
                                      (opportunistic-grafting
                                       p1
                                       (strip-cars topic-meshes)
                                       nts 
                                       nbr-scores
                                       params))
                                 (update-mesh-times-counters
                                  tcmap
                                  (flatten-topic-lop-map topic-meshes)
                                  elapsed)
                                 gcmap
                                 nbr-scores)))
      (& defaultres))))

;; Broke this huge function into 3 smaller functions
(definecd update-nbr-topic-state (nts :nbr-topic-state nbr-scores
                                      :peer-rational-map tcmap :pt-tctrs-map
                                      gcmap :p-gctrs-map evnt :evnt twpm :twp s
                                      :nat)
  :nbr-topic-stateloevpt-tctrs-mapp-gctrs-mappeer-rational-map
  :skip-tests t
  :ic (is-valid-twp twpm)
  :body-contracts-hints (("Goal" :DO-NOT-INDUCT T
                          :in-theory (enable evntp twpp wpp)))
  ;;add all components here
  (cond
   ((in (second evnt) '(RCV SND))
    (update-nbr-topic-state1 nts nbr-scores tcmap gcmap evnt twpm s))
   ((in (second evnt) '(JOIN LEAVE))
    (update-nbr-topic-state2 nts nbr-scores tcmap gcmap evnt twpm s))
   (t (update-nbr-topic-state3 nts nbr-scores tcmap gcmap evnt twpm s))))


(property
  nbr-topic-stateloevpt-tctrs-mapp-gctrs-mappeer-rational-map-check
  (x :nbr-topic-stateloevpt-tctrs-mapp-gctrs-mappeer-rational-map)
  (^ (nbr-topic-statep (first x))
     (loevp (second x))
     (pt-tctrs-mapp (third x))
     (p-gctrs-mapp (fourth x))
     (peer-rational-mapp (fifth x))))

(property first-update-nbr-topic-state
  (nts :nbr-topic-state nbr-scores
       :peer-rational-map tcmap :pt-tctrs-map
       gcmap :p-gctrs-map evnt :evnt twpm :twp s
       :nat)
  :check-contracts? nil
  (nbr-topic-statep (first (update-nbr-topic-state nts nbr-scores tcmap
                                                   gcmap evnt twpm s)))
  :hints (("Goal" :in-theory (enable update-nbr-topic-state))))

(property second-update-nbr-topic-state
  (nts :nbr-topic-state nbr-scores
       :peer-rational-map tcmap :pt-tctrs-map
       gcmap :p-gctrs-map evnt :evnt twpm :twp s
       :nat)
  :check-contracts? nil
  (loevp (second (update-nbr-topic-state nts nbr-scores tcmap
                                         gcmap evnt twpm s)))
  :hints (("Goal" :in-theory (enable update-nbr-topic-state))))

(property third-update-nbr-topic-state
  (nts :nbr-topic-state nbr-scores
       :peer-rational-map tcmap :pt-tctrs-map
       gcmap :p-gctrs-map evnt :evnt twpm :twp s
       :nat)
  :check-contracts? nil
  (pt-tctrs-mapp (third (update-nbr-topic-state nts nbr-scores tcmap
                                                gcmap evnt twpm s)))
  :hints (("Goal" :in-theory (enable update-nbr-topic-state))))

(property fourth-update-nbr-topic-state
  (nts :nbr-topic-state nbr-scores
       :peer-rational-map tcmap :pt-tctrs-map
       gcmap :p-gctrs-map evnt :evnt twpm :twp s
       :nat)
  :check-contracts? nil
  (p-gctrs-mapp (fourth (update-nbr-topic-state nts nbr-scores tcmap
                                                gcmap evnt twpm s)))
  :hints (("Goal" :in-theory (enable update-nbr-topic-state))))

(property fifth-update-nbr-topic-state
  (nts :nbr-topic-state nbr-scores
       :peer-rational-map tcmap :pt-tctrs-map
       gcmap :p-gctrs-map evnt :evnt twpm :twp s
       :nat)
  :check-contracts? nil
  (peer-rational-mapp (fifth (update-nbr-topic-state nts nbr-scores tcmap
                                                     gcmap evnt twpm s)))
  :hints (("Goal" :in-theory (enable update-nbr-topic-state))))

(property bind-update-nbr-topic-state
  (nts :nbr-topic-state nbr-scores
       :peer-rational-map tcmap :pt-tctrs-map
       gcmap :p-gctrs-map evnt :evnt twpm :twp s
       :nat)
  :check-contracts? nil
  (b* (((list a b c d e) (update-nbr-topic-state nts nbr-scores tcmap
                                                 gcmap evnt twpm s)))
    (^ (nbr-topic-statep a)
       (loevp b)
       (pt-tctrs-mapp c)
       (p-gctrs-mapp d)
       (peer-rational-mapp e))))
