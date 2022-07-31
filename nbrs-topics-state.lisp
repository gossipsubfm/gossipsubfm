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
                  (list peer 'HBM pos-rat)
                  (list peer 'APP topic payload-type)))
;; In the case above, the topic is redundant.
(defdata loev (listof evnt))
(defdata (peer-lot-map   (alistof peer lot)))
(defdata (topic-lop-map  (alistof topic lop)))
(defdata (topic-nat-map  (alistof topic nat)))

;;The following state deals with neighbours and topics they follow,
;; hence we call it (n)eighbour(t)opic-state or nbr-topic-state
(defdata nbr-topic-state
  (record (nbr-topicsubs . topic-lop-map)     ;; peer's neighbours'
          ;; subscriptions, includes peer itself, tokeep track of its own subs
          ;; let's keep this as topic-lop-map, as its convenient, no need to
          ;; invert map
          (topic-fanout  . topic-lop-map)    ;; peer doesn't subscribe to these
          ;; topics
          (last-pub      . topic-nat-map)    ;; last published time for a topic
          (topic-mesh    . topic-lop-map)))  ;; peer subscribes to these topics
;; and is part of corresponding topic mesh


(defthm nbr-topic-state-check
  (== (NBR-TOPIC-STATEP
       (MSET :0TAG 'NBR-TOPIC-STATE
             (MSET :LAST-PUB VAR-LAST-PUB
                   (MSET :TOPIC-MESH VAR-TOPIC-MESH
                         (MSET :TOPIC-FANOUT VAR-TOPIC-FANOUT
                               (MSET :NBR-TOPICSUBS
                                     VAR-NBR-TOPICSUBS NIL))))))
      (^ (topic-lop-mapp var-nbr-topicsubs)
         (topic-lop-mapp var-topic-fanout)
         (topic-nat-mapp var-last-pub)
         (topic-lop-mapp var-topic-mesh)))
  :hints (("Goal" :in-theory (enable nbr-topic-statep))))



(definec new-nbr-topic-state () :nbr-topic-state
  (nbr-topic-state nil nil nil nil))

(definec dist-cons (x :cons) :tl
  (cond ((atom (cdr x)) nil)
	(t (cons `(,(car x) . ,(cadr x))
		 (dist-cons `(,(car x) . ,(cddr x)))))))

(check= (dist-cons '(1 2 3)) '((1 . 2) (1 . 3)))

(create-map* dist-cons alistp tlp)

(definecd flatten-map (al :alist) :alist
  (flatten (map* dist-cons al)))

(check= (flatten-map '((1) (1 . (1 2)) (1 . (3 4))))
	'((1 . 1) (1 . 2) (1 . 3) (1 . 4)))

(definec flip-cons (x :cons) :cons
  (cons (cdr x) (car x)))

(create-map* flip-cons alistp alistp)


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
     (subsetp-equal (flatten-map (nbr-topic-state-topic-fanout ps))
                    (flatten-map (nbr-topic-state-nbr-topicsubs ps)))
;peer knows what topics are subscribed to by nbrs in each topic mesh
     (subsetp-equal (flatten-map (nbr-topic-state-topic-mesh ps))
                    (flatten-map (nbr-topic-state-nbr-topicsubs ps)))))

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

(definecd rem-sub (top-peers :topic-lop-map nbr :peer topic :topic)
  :topic-lop-map
  :SKIP-BODY-CONTRACTSP T
  (let ((old-subscribers (cdr (assoc-equal topic top-peers))))
    (put-assoc-equal topic (remove-equal nbr old-subscribers) top-peers)))

(create-reduce* (lambda (nbr tmp topic) (add-sub tmp nbr topic))
                topic-lop-mapp
                lopp
                (:name add-subs)
                (:fixed-vars ((topicp topic))))

(in-theory (disable tctrsp))

(property rem-peer-tctrs (p :peer top :topic m :pt-tctrs-map)
          (pt-tctrs-mapp (remove-assoc-equal `(,p . ,top) m)))

(definecd update-topic-mesh
  (nts :nbr-topic-state new-topic-mesh :topic-lop-map) :nbr-topic-state
  (mset :topic-mesh new-topic-mesh nts))

(definecd update-last-pub
  (nts :nbr-topic-state new-last-pub :topic-nat-map) :nbr-topic-state
  (mset :last-pub new-last-pub nts))

(defthm rem-key-topic-lop-map
  (implies (and (topicp topic)
		(topic-lop-mapp tmap))
	   (topic-lop-mapp (remove-assoc-equal topic tmap))))

(definec update-nbr-topicsubs
  (nbr-topic-state :nbr-topic-state new-nbr-topicsubs :topic-lop-map) :nbr-topic-state
  (mset :nbr-topicsubs new-nbr-topicsubs nbr-topic-state))

(definec update-topic-fanout
  (nbr-topic-state :nbr-topic-state new-topic-fanout :topic-lop-map) :nbr-topic-state
  (mset :topic-fanout new-topic-fanout nbr-topic-state))

(definecd remove-peer-submap (tmp :topic-lop-map nbr :peer) :topic-lop-map
  (if (endp tmp)
      nil
    (if (endp (cdar tmp))
        (remove-peer-submap (cdr tmp) nbr)
      (cons `(,(caar tmp) . ,(remove-equal nbr (cdar tmp)))
            (remove-peer-submap (cdr tmp) nbr)))))


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

(defthm shuffle-peers
  (=> (^ (lopp ps) (natp s))
      (lopp (shuffle ps s))))

(defthm grab-peers
  (=> (^ (lopp ps) (natp d))
      (lopp (grab d ps))))

(in-theory (disable shuffle grab))

(definecd remove-excess-mesh (mesh :topic-lop-map dhigh :nat d :nat s :nat)
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


(defdata lotopicpeer (alistof topic peer))

(property caar-mesh-topicp (mesh :topic-lop-map)
         (=> (consp (car mesh))
             (topicp (car (car mesh)))))

(property topic-lop-map-assoc (tp :topic al :topic-lop-map)
          (lopp (cdr (assoc-equal tp al))))

(property app-lopp (xs :lop ys :lop)
          (lopp (app xs ys)))

(defthm add-mesh-nbrs-help
          (IMPLIES (AND 
              (TOPIC-LOP-MAPP CANDIDATES)
              (LOPP (CDR (CAR MESH)))
              (TOPIC-LOP-MAPP (CDR MESH))
              (CONSP MESH)
              (CONSP (CAR MESH)))
         (TLP (CDR (ASSOC-EQUAL (CAR (CAR MESH))
                                CANDIDATES)))))

(definecd add-mesh-nbrs (mesh :topic-lop-map candidates :topic-lop-map dlow
  :nat d :nat)
  :topic-lop-map
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
;; (definecd mesh-mnt-per-topic (nts :nbr-topic-state top :topic prm
;;                                   :peer-rational-map params :params) :nbr-topic-statelop
;;   (b* ((mesh-nbrs (cdr (assoc-equal top (nbr-topic-state-topic-mesh nts))))
;;        ((when (> (len mesh-nbrs) (params-dhigh params))) -)
;;        ((when (< (len mesh-nbrs) (params-dlow params))) -))
;;     (cons nts nil)))


(property nbr-topicsubs-lookup (subs :peer-lot-map p :peer)
          (lotp (cdr (assoc-equal p subs))))

(property topic-mesh-lookup (topic-mesh :topic-lop-map tp :topic)
          (lopp (cdr (assoc-equal tp topic-mesh))))


(defthm meshtime-ctr-help
 (POS-RATP (MGET :MESHTIME (LOOKUP-TCTRS (CDR (CAR MESH-TOPIC-NBRS))
                                            (CAR (CAR MESH-TOPIC-NBRS))
                                            NIL))))
(defthm meshtime-ctr-help2
  (IMPLIES
   (AND (RATIONALP ELAPSED)
        (< 0 ELAPSED)
        (LOTOPICPEERP MESH-TOPIC-NBRS)
        (PT-TCTRS-MAPP NBR-COUNTERS)
        MESH-TOPIC-NBRS
        )
   (POS-RATP (MGET :MESHTIME (LOOKUP-TCTRS (CDR (CAR MESH-TOPIC-NBRS))
                                           (CAR (CAR MESH-TOPIC-NBRS))
                                           NBR-COUNTERS)))))

(property add-posrats (a :rational b :pos-rat)
          (=> (< 0 a)
              (pos-ratp (+ a b))))

(defthm meshtime-ctr-help3
  (IMPLIES
   (AND (RATIONALP ELAPSED)
        (< 0 ELAPSED)
        (LOTOPICPEERP MESH-TOPIC-NBRS)
        (PT-TCTRS-MAPP NBR-COUNTERS)
        MESH-TOPIC-NBRS
        )
   (POS-RATP (+ ELAPSED
                (MGET :MESHTIME (LOOKUP-TCTRS (CDR (CAR MESH-TOPIC-NBRS))
                                           (CAR (CAR MESH-TOPIC-NBRS))
                                           NBR-COUNTERS)))))
  :hints (("Goal" :in-theory (disable LOOKUP-TCTRS))))


(definecd update-mesh-times-counters
  (nbr-counters :pt-tctrs-map mesh-topic-nbrs :lotopicpeer elapsed :pos-rat) :pt-tctrs-map
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
       elapsed)))
  :body-contracts-hints (("Goal" :use meshtime-ctr-help3)))


(defthm mget-def-0
  (=>
   (^ (SYMBOLP TOP)
      (SYMBOLP NBR))
   (== (MGET :MESHMESSAGEDELIVERIES (LOOKUP-TCTRS NBR TOP NIL))
       0))
  :HINTS (("Goal" :in-theory (enable lookup-tctrs))))

(defthm plus-mfp-exptdeficit-posratp
  (IMPLIES
 (AND
  (<= MESHMESSAGEDELIVERIESTHRESHOLD
      MESHMESSAGEDELIVERIESCAP)
  (INTEGERP MESHMESSAGEDELIVERIESTHRESHOLD)
  (<= 0 MESHMESSAGEDELIVERIESTHRESHOLD)
  (INTEGERP MESHMESSAGEDELIVERIESCAP)
  (<= 0 MESHMESSAGEDELIVERIESCAP)
  (SYMBOLP TOP)
  (SYMBOLP NBR)
  (PT-TCTRS-MAPP NBR-COUNTERS))
 (POS-RATP
  (+
   (MGET :MESHFAILUREPENALTY (LOOKUP-TCTRS NBR TOP NBR-COUNTERS))
   (EXPT
       (CALC-DEFICIT
            (MGET :MESHMESSAGEDELIVERIES (LOOKUP-TCTRS NBR TOP NBR-COUNTERS))
            MESHMESSAGEDELIVERIESCAP
            MESHMESSAGEDELIVERIESTHRESHOLD)
       2))))
  :HINTS (("Goal" :in-theory (enable lookup-tctrs))))
  

(definecd retain-mesh-failure-counters
  (nbr-counters :pt-tctrs-map nbr :peer top :topic
                meshMessageDeliveriesCap :nat meshMessageDeliveriesThreshold :nat)
  :pt-tctrs-map
  :skip-tests t
  :ic (>= meshMessageDeliveriesCap
	  meshMessageDeliveriesThreshold)
  (b* ((counters                       (lookup-tctrs nbr top nbr-counters))
       (dfct                           (calc-deficit (tctrs-meshMessageDeliveries counters)
                                                     meshMessageDeliveriesCap
                                                     meshMessageDeliveriesThreshold))
       (new-meshFailurePenalty (+ (tctrs-meshFailurePenalty counters)
                                  (* dfct dfct))))
    (put-assoc-equal (cons nbr top)
		     (update-meshFailurePenalty counters new-meshFailurePenalty)
		     nbr-counters))
    :body-contracts-hints (("Goal" :use plus-mfp-exptdeficit-posratp)))



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
  (nbr-counters-map :pt-tctrs-map tp-nbr-lst :lotopicpeer meshMessageDeliveriesCap :nat meshMessageDeliveriesThreshold :nat)
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

(definecd age-lastpub (lastpub :topic-nat-map elapsed :nat) :topic-nat-map
  (match lastpub
    (() '())
    (((tp . age) . rst) (cons `(,tp . ,(+ age elapsed))
                              (age-lastpub rst elapsed)))))

(defthm topic-subs-assoc
  (IMPLIES
   (AND (TOPIC-LOP-MAPP TOPIC-SUBS)
        (TOPIC-LOP-MAPP TFANOUT)
        (TOPIC-NAT-MAPP LASTPUB)
        (CONSP LASTPUB)
        (CONSP (CAR LASTPUB)))
   (TLP (CDR (ASSOC-EQUAL (CAR (CAR LASTPUB))
                          TOPIC-SUBS)))))


(definecd prune-fanout (lastpub :topic-nat-map tfanout :topic-lop-map topic-subs :topic-lop-map fanoutttl :nat d :nat) :topic-lop-map
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


(definecd fanout-maintenance (nts :nbr-topic-state topic-subs :topic-lop-map
  elapsed :nat fanoutttl :nat d :nat) :nbr-topic-state
  (b* ((new-last-pub (age-lastpub (nbr-topic-state-last-pub nts) elapsed))
       (new-nts (update-last-pub nts new-last-pub)))
    (update-topic-fanout new-nts (prune-fanout new-last-pub
                                               (nbr-topic-state-topic-fanout nts)
                                               topic-subs
                                               fanoutttl
                                               d))))

(defthm lopp->tlp
  (=> (lopp x) (tlp x)))

(property assoc-nbr-topic-state (nts :nbr-topic-state top :topic)
          (lopp (CDR (ASSOC-EQUAL TOP (NBR-TOPIC-STATE-TOPIC-MESH NTS)))))

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

(definecd opportunistic-grafting (p1 :peer
                                    top :topic
                                    nts :nbr-topic-state
                                    nbr-scores :peer-rational-map
                                    params :params)
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



(definecd opportunistic-grafting-topics (p1 :peer
                                            tops :lot
                                            nts :nbr-topic-state
                                            nbr-scores :peer-rational-map
                                            params :params)
  :loev
  :ic (^ (>= (params-meshMessageDeliveriesCap params)
	      (params-meshMessageDeliveriesThreshold params))
	  (<= (params-dlow params) (params-d params))
	  (>= (params-dhigh params) (params-d params)))
  (match tops
    (() '())
    ((top . rst) (app (opportunistic-grafting p1 top nts nbr-scores params)
                      (opportunistic-grafting-topics p1 rst nts nbr-scores
                                                    params)))))

(property flat-vals-topic-lops (al :topic-lop-map)
          (lopp (acl2s::flatten (acl2::alist-vals al))))

(create-tuple* nbr-topic-state loev pt-tctrs-map p-gctrs-map peer-rational-map)

(defthm twp-dlow-int
  (=> (^ (twpp twpm) twpm)
      (INTEGERP (MGET :DLOW (CDDR (CAR TWPM))))))

(defthm twp-cddar-paramsp
  (=> (^ (twpp twpm) twpm)
      (PARAMSP (CDDR (CAR TWPM)))))

(defthm dlow-number
  (=> (^ (twpp twpm) twpm)
      (ACL2-NUMBERP (MGET :D (CDDR (CAR TWPM))))))

(defthm dlow-lt-d-twp
  (=> (^ (twpp twpm)
         (is-valid-twp twpm)
         (ACL2-NUMBERP (MGET :D (CDDR (CAR TWPM)))))
      (<= (MGET :DLOW (CDDR (CAR TWPM)))
          (MGET :D (CDDR (CAR TWPM)))))
  :hints (("Goal" :in-theory (enable lookup-twpm is-valid-twp wpp paramsp))))

(defthm dlow-gt-0-twp
  (=> (^ (twpp twpm)
         (is-valid-twp twpm)
         (ACL2-NUMBERP (MGET :D (CDDR (CAR TWPM)))))
      (<= 0 (MGET :DLOW (CDDR (CAR TWPM)))))
  :hints (("Goal" :in-theory (enable lookup-twpm is-valid-twp wpp paramsp))))

(property set-diff-lop (xs :lop ys :lop)
          (lopp (set-difference-equal xs ys)))
 
(definecd rem-val-peers (al :topic-lop-map rl :lop) :topic-lop-map
  (match al
    (() '())
    (((k . v) . rst)
     (cons `(,k . ,(set-difference-equal v rl))
           (rem-val-peers rst rl)))))

(in-theory (disable paramsp))

(skip-proofs
(definecd update-nbr-topic-state (nts :nbr-topic-state
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
  (b* ((topic-subs (nbr-topic-state-nbr-topicsubs nts))
       (topic-meshes (nbr-topic-state-topic-mesh nts))
       (nbrs (acl2s::flatten (acl2::alist-vals topic-subs)))
       (mesh-nbrs (acl2s::flatten (acl2::alist-vals topic-meshes))))
    (case-match evnt
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
            ((when (null params)) (list nts nil tcmap gcmap nbr-scores)))
         (if (in nbr mesh-nbrs)
             (list
              (update-topic-mesh
               new-nts
               (rem-sub topic-meshes nbr topic))
              nil
              (retain-mesh-failure-counters
               tcmap nbr topic (params-meshMessageDeliveriesCap params) (params-meshMessageDeliveriesThreshold params)))
           (list new-nts nil tcmap gcmap nbr-scores))))
      ;;TODO : need to check for nbr score before grafting, and whether it is
      ;;in BACKOFF
      ;;GRAFT implies SUB, so we also add nbr subscription
      ((p1 'RCV nbr 'GRAFT topic)
       (cond
        ((not (in topic (topics))) (list nts nil tcmap gcmap nbr-scores)) ;;ignore if not a
        ;;valid topic
        ((! (in p1 (cdr (assoc-equal topic topic-subs))))
         (list nts
               `((,p1 SND ,nbr PRUNE ,topic))
               tcmap gcmap nbr-scores))
        ((>= 0 (lookup-score nbr nbr-scores))
         (list (update-topic-mesh
                (update-nbr-topicsubs
                 nts
                 (add-sub topic-subs nbr topic))
                (add-sub topic-meshes nbr topic))
               nil
               tcmap gcmap nbr-scores))
        (t (list nts nil tcmap gcmap nbr-scores))))
      ((& 'RCV nbr 'PRUNE topic)
       (b* ((wp (cdr (assoc-equal topic twpm)))
            (params (cdr wp))
            ((when (null params)) (list nts nil tcmap gcmap nbr-scores)))
         (list
          (update-topic-mesh
           nts
           (rem-sub topic-meshes nbr topic))
          nil
          (retain-mesh-failure-counters tcmap nbr topic (params-meshMessageDeliveriesCap params) (params-meshMessageDeliveriesThreshold params)))))
      ((p1 'SND & 'SUB topic)
       (list (update-nbr-topicsubs
              nts
              (add-sub topic-subs p1 topic))
             nil
             tcmap gcmap nbr-scores))
      ;;UNSUB implies PRUNE, so we also prune the nbr (topic key removed from mesh map)
      ;;If nbr is not in mesh, then default behaviour
      ;; no need to send PRUNE
      ((p1 'SND & 'UNSUB topic)
       (list (update-nbr-topicsubs
              nts
              (rem-sub topic-subs p1 topic))
             nil
             tcmap gcmap nbr-scores))
      ;;need to check for nbr score before grafting
      ((& 'SND nbr 'GRAFT topic)
       (list (update-topic-mesh
              nts
              (add-sub topic-meshes nbr topic))
             nil
             tcmap gcmap nbr-scores))
      ((& 'SND nbr 'PRUNE topic)
       (b* ((wp (cdr (assoc-equal topic twpm)))
            (params (cdr wp))
            ((when (null params)) (list nts nil tcmap gcmap nbr-scores)))
         (list (update-topic-mesh
                nts
                (rem-sub topic-meshes nbr topic))
               nil
               (retain-mesh-failure-counters tcmap nbr topic (params-meshMessageDeliveriesCap params) (params-meshMessageDeliveriesThreshold params)))))
      ((p1 'JOIN tp) (b* ((wp (cdar (assoc-equal tp twpm)))
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
                                    (flatten-map `((,tp . ,newnbrs))) p1)
                              (map* mk-subs
                                    (flatten-map `((,tp . ,nbrs))) p1))
                             tcmap gcmap nbr-scores)))
      ((p1 'LEAVE tp) (list (update-topic-mesh
                             (update-nbr-topicsubs nts (rem-sub topic-subs p1 tp))
                             (remove-assoc-equal tp topic-meshes))
                            ;;unsubs imply PRUNE
                            (map* mk-unsubs
                                  (flatten-map `((,tp . ,nbrs))) p1)
                            tcmap gcmap nbr-scores))
      ((self 'RCV p1 'CONNECT1 tps)
       (list (update-nbr-topicsubs nts (reduce* add-peer-subs topic-subs tps p1))
             `((,self SND ,p1 CONNECT2 ,(peer-subs topic-subs self)))
             tcmap gcmap nbr-scores))
      ((& 'RCV p1 'CONNECT2 tps)
       (list (update-nbr-topicsubs nts (reduce* add-peer-subs topic-subs tps p1))
             nil
             tcmap gcmap nbr-scores))
      ;;for nbrs pruned by low score, need to send PRUNE messages
      ((p1 'HBM elapsed) (b* ((wp (cdar twpm))
                              (params (cdr wp))
                              ((when (null params)) (list nts nil tcmap gcmap
                                                          nbr-scores))
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
                                                    (rem-vals topic-subs
                                                              disqualified-mesh-nbrs)
                                                    (params-dlow params)
                                                    (params-d params)))
                              ;;topic-peer alist which is to be pruned
                              (tps-rem (app (filter* remvd-topic-nbr
                                                     (flatten-map topic-meshes)
                                                     remvd)   ;;score
                                            (flatten-map (map-diff mesh1
                                                                   mesh2)))) ;;extras
                              ;;topic-peer alist which is to be grafted
                              (tps-gr (flatten-map (map-diff mesh3
                                                             mesh2)))
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
                                      (opportunistic-grafting-topics
                                       p1
                                       (strip-cars topic-meshes)
                                       nts 
                                       nbr-scores
                                       params))
                                 (update-mesh-times-counters
                                  tcmap
                                  (flatten-map topic-meshes)
                                  elapsed)
                                 gcmap
                                 nbr-scores)))
      (& (list nts nil tcmap gcmap nbr-scores))))))
