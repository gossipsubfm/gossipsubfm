(in-package "ACL2S")

(include-book "nbrs-topics-state")
(include-book "msgs-state")

(defdata peer-state
  (record (nts . nbr-topic-state)
          (mst . msgs-state)
          (nbr-tctrs . pt-tctrs-map)
          (nbr-gctrs . p-gctrs-map)
          (nbr-scores . peer-rational-map)))

#|
(property mset-diff-mset2 (a b x y r :all)
  :check-contracts? nil
  :hyps (!= a b)
  :body (== (s b y (s a x r))
            (s a x (s b y r))))

(in-theory (disable acl2::mset-diff-mset))

(property (z ms es nsm ntcm ngcm :all)
  :testing? nil
  (equal (peer-statep (S :mst ms
			    (S :nts es
				  (S :0TAG 'peer-state
                                        (S :nbr-scores nsm
					      (S :nbr-tctrs ntcm
                                                    (S :nbr-gctrs ngcm
                                                          NIL)))))))
         z))

         (and (nbr-topic-statep es)
              (msgs-statep ms)
              (peer-rational-mapp nsm)
	      (pt-tctrs-mapp ntcm)
              (p-gctrs-mapp ngcm)))
  :hints (("goal" :in-theory (enable peer-statep))))
|#

(defthm peer-state-check2
  (equal (peer-statep
          (Ss nil
              :mst ms :nts es :0TAG 'peer-state :nbr-scores nsm
              :nbr-tctrs ntcm :nbr-gctrs ngcm))
         (and (nbr-topic-statep es)
              (msgs-statep ms)
              (peer-rational-mapp nsm)
              (pt-tctrs-mapp ntcm)
              (p-gctrs-mapp ngcm)))
  :hints (("goal" :in-theory (enable peer-statep))))

(defthm peer-state-check3
  (=>
   (^ (nbr-topic-statep nts)
      (msgs-statep ms)
      (pt-tctrs-mapp tctrs)
      (p-gctrs-mapp gctrs)
      (peer-rational-mapp scores))
   (peer-statep
    (ss nil
        :mst ms :nts nts :0tag 'peer-state
        :nbr-gctrs gctrs :nbr-tctrs tctrs :nbr-scores scores)))
  :hints (("goal" :in-theory (enable peer-statep))))

(defthm peer-state-check4
  (=> (peer-statep
       (ss nil :mst ms :nts nts :0tag 'peer-state :nbr-gctrs gctrs
           :nbr-tctrs tctrs :nbr-scores scores))
      (^ (nbr-topic-statep nts)
         (msgs-statep ms)
         (pt-tctrs-mapp tctrs)
         (p-gctrs-mapp gctrs)
         (peer-rational-mapp scores)))
  :hints (("goal" :in-theory (enable peer-statep)))
  :rule-classes :forward-chaining)

;;initializes a new peer state if we need one, or don't have one already in the network
(definecd new-peer-state () :peer-state
  (peer-state (new-nbr-topic-state)
              (new-msgs-state)
              '()
              '()
              '()))

(in-theory (disable peer-statep))

(property snd-ihave-evnt (p1 p2 :peer pids :lopid)
  (EVNTP (LIST P1 'SND P2 'IHAVE pids))
  :hints (("Goal" :in-theory (enable evntp))))

(create-map* (lambda (nbr p pids) `(,p SND ,nbr IHAVE ,pids))
             lopp
             loevp
             (:fixed-vars ((peerp p) (lopidp pids)))
             (:name mapgossips))

(sig grab (nat (listof :a)) => (listof :a))
(sig shuffle ((listof :a) nat) => (listof :a))
(sig remove-duplicates-equal ((listof :a)) => (listof :a))

(property flatten-cdrs-tlm (tlm :topic-lop-map)
  (lopp (flatten (strip-cdrs tlm))))

(sig set-difference-equal ((listof :a) (listof :a)) => (listof :a))

;;emit gossip to d random nbrs
(definecd gossip-emission (p :peer ps :peer-state d :nat params :params s :nat) :loev
  :skip-tests t
  :timeout 600
  (b* (((mv k &) (defdata::genrandom-seed
                   (1- (expt 2 31))
                   (mod s (expt 2 31))))
       (ms (peer-state-mst ps))
       (mcache  (msgs-state-pld-cache ms))
       (hwins   (msgs-state-hwindows ms))
       (mcg     (floor (* (reduce* + 0 hwins) (params-mcacheGsp params)) 1))
       (gsp-cache (grab (reduce* + 0
                                 (grab mcg hwins))
                        mcache))
       ((unless (consp gsp-cache)) nil)
       (pids (remove-duplicates-equal (map* get-pids gsp-cache)))
       (ntst (peer-state-nts ps))
       (allnbrs (flatten (strip-cdrs (nbr-topic-state-nbr-topicsubs ntst))))
       (mesh-nbrs (flatten (strip-cdrs (nbr-topic-state-topic-mesh ntst))))
       (fanout-nbrs (flatten (strip-cdrs (nbr-topic-state-topic-fanout ntst))))
       (mf (append mesh-nbrs fanout-nbrs))
       (no-mf (remove-duplicates-equal (remove p (set-difference-equal allnbrs mf))))
       (gossip-nbrs (grab d (shuffle no-mf k))))
    (map* mapgossips gossip-nbrs p pids)))

(definecd rev-lookup-topic (p :peer map :topic-lop-map) :lot
  (match map
    (() '())
    (((tp . nbrs) . rst)
     (if (in p nbrs)
         (cons tp (rev-lookup-topic p rst))
       (rev-lookup-topic p rst)))))

(definecd collect-forward-nbrs (tp :topic map :peer-lot-map) :lop
  (match map
    (() '())
    (((nbr . lot) . rst)
     (if (in tp lot)
         (cons nbr (collect-forward-nbrs tp rst))
       (collect-forward-nbrs tp rst)))))

(create-map* (lambda (nbr p pld) `(,p SND ,nbr PAYLOAD ,pld))
             lopp
             loevp
             (:fixed-vars ((peerp p) (payload-typep pld)))
             (:name mapforwards))

(in-theory (disable map*-*mapforwards))

(sig remove-assoc-equal (:a (alistof :a :b)) => (alistof :a :b))
(create-tuple* nbr-topic-state loev)

(definecd forward-emission-help
  (self source :peer ntst :nbr-topic-state payload :payload-type d k :nat)
  :nbr-topic-stateloev
  :skip-tests t
  (b* ((ms (nbr-topic-state-topic-mesh ntst))
       (fout (nbr-topic-state-topic-fanout ntst))
       (top (third payload))
       (orig (fourth payload))
       (fwd-nbrs (remove orig
                         (remove self
                                 (cdr (assoc-equal top (nbr-topic-state-nbr-topicsubs ntst))))))
       (lp (nbr-topic-state-last-pub ntst))
       (newlp `((,top . 0) . ,(remove-assoc-equal top lp)))
       (fout-nbrs (cdr (assoc-equal top fout)))
       (msubs (strip-cars ms)))
    (cond ((^ (in top msubs)
              (consp (cdr (assoc-equal top ms))))
           (list ntst
                 (map* mapforwards (set-difference-equal
                                    (cdr (assoc-equal top ms))
                                    `(,self ,source ,orig))
                       self
                       payload)))
          ((consp fout-nbrs) (list (update-last-pub ntst newlp)
                                   (map* mapforwards fout-nbrs self
                                         payload)))
          (t (b* ((new-fout-nbrs
                   (grab d (shuffle fwd-nbrs k)))
                  ((when (endp new-fout-nbrs)) (list ntst nil)))
               (list (update-topic-fanout
                      (update-last-pub ntst newlp)
                      (cons `(,top . ,new-fout-nbrs) fout))
                     (map* mapforwards (set-difference-equal
                                        (cdr (assoc-equal top ms))
                                        `(,self ,source ,orig))
                           self
                           payload)))))))

(property fwd-emission-help-thm1
  (self :peer source :peer ntst :nbr-topic-state payload :payload-type d k :nat)
  (nbr-topic-statep (first (forward-emission-help self source ntst payload d k)))
  :hints (("Goal" :do-not-induct t
           :in-theory (enable nbr-topic-stateloevp))))

(property fwd-emission-help-thm2
  (self source :peer ntst :nbr-topic-state payload :payload-type d k :nat)
  (loevp (second (forward-emission-help self source ntst payload d k)))
  :hints (("Goal" :do-not-induct t
           :in-theory (enable nbr-topic-stateloevp forward-emission-help))))

(definecd decay-mult (f :frac e :integer x :non-neg-rational d :frac)
  :non-neg-rational
  :ic (<= 0 e)
  (let ((r (* (expt f (1+ e))
              x)))
    (if (< r d)
        0
      r)))

(property assoc-twpm-ptc (twpm :twp top :topic)
  (=> (^ (cddr (assoc-equal top twpm)))
      (paramsp (cddr (assoc-equal top twpm)))))


(property pos-rational-check (x :all)
  (=> (pos-rationalp x)
      (^ (rationalp x)
         (> x 0))))

(encapsulate
 ()
    
 (local
   (defthm lt-0-not-0
     (=> (< 0 x)
         (!= 0 x))))

 (local
   (defthm hbmint-twp2
     (=> (^ (twpp twpm)
            (cddr (assoc-equal (cdr (car (car ptc))) twpm))
            (pt-tctrs-mapp ptc)
            ptc)
         (!= 0 (g :decayinterval
                     (cddr (assoc-equal (cdr (car (car ptc))) twpm)))))))

 (local
   (defthm floor-pos-rational
     (=> (^ (pos-rationalp a)
            (pos-rationalp b))
         (^ (integerp (floor a b))
            (<= 0 (floor a b))))))
     
 ;; TODO : add all other decays as well, including global decays
 (definecd decay-ctrs (ptc :pt-tctrs-map hbmint :pos-rational twpm :twp) :pt-tctrs-map
   :skip-tests t
   :timeout 600
   :function-contract-hints (("Goal" :in-theory (enable pt-tctrs-mapp)))
   :body-contracts-hints (("Goal" :in-theory (enable pt-tctrs-mapp)))
   (match ptc
     (() '())
     ((((peer . top) . tctrs) . rst)
      (b* ((params (cddr (assoc-equal top twpm)))
           ((when (null params)) (cons (car ptc)
                                       (decay-ctrs rst hbmint twpm)))
           (d20 (params-decayToZero params))
           (e (floor hbmint (params-decayInterval params)))
           (decay-mmd (decay-mult (params-meshMessageDeliveriesDecay params)
                                  e
                                  (tctrs-meshMessageDeliveries tctrs)
                                  d20))
           (decay-fmd (decay-mult (params-firstMessageDeliveriesDecay params)
                                  e
                                  (tctrs-firstMessageDeliveries tctrs)
                                  d20)))
        (cons `(,(cons peer top) . ,(update-firstMessageDeliveries
                                     (update-meshMessageDeliveries
                                      tctrs
                                      decay-mmd)
                                     decay-fmd))
              (decay-ctrs rst hbmint twpm)))))))

(property cdar-twpm (twpm :twp)
  (=> (cdar twpm)
      (wpp (cdar twpm)))
  :rule-classes :forward-chaining)

(property cddar-twpm (twpm :twp)
  (=> (cddar twpm)
      (paramsp (cddar twpm)))
  :rule-classes :forward-chaining)

(create-tuple* peer-state loev)
(sig strip-cars ((alistof :a :b)) => (listof :a))

(property app-loev (xs ys :loev)
  (loevp (app xs ys)))

(property evnt-payload-rcv (e :evnt)
  :check-contracts? nil
  (=> (== (fourth e) 'PAYLOAD)
      (^ (payload-typep (fifth e))
         (peerp (car e))
         (peerp (third e))))
  :hints (("Goal" :in-theory (enable evntp))))

(property evnt-app (e :evnt)
  :check-contracts? nil
  (=> (== (second e) 'APP)
      (^ (peerp (car e))
         (topicp (third e))
         (payload-typep (fourth e))))
  :hints (("Goal" :in-theory (enable evntp))))

(definec forward-emission
  (self :peer evnt :evnt ntstate :nbr-topic-state rs :msgpeer-age s d :nat)
  :nbr-topic-stateloev
  :body-contracts-hints (("Goal" :in-theory (enable evntp)))
  :skip-tests t
  (match evnt
    ((!self 'RCV & 'PAYLOAD m)
     (if (^ (!= self (fourth m)) ;;if not the publisher
            (! (in (payload2pid m)
                   (map* rs->pids (strip-cars rs)))))
         (forward-emission-help self (third evnt) ntstate (fifth evnt) d s)
       (list ntstate '())))
    ((!self 'APP & m)
     (forward-emission-help self self ntstate m d s))
    (& (list ntstate '()))))

(property fwd-emission-thm1
  (self :peer evnt :evnt ntstate :nbr-topic-state rs :msgpeer-age s d :nat)
  :check-contracts? nil
  (nbr-topic-statep (first (forward-emission self evnt ntstate rs s d))))

(property fwd-emission-thm2
  (self :peer evnt :evnt ntstate :nbr-topic-state rs :msgpeer-age s d :nat)
  (loevp (second (forward-emission self evnt ntstate rs s d))))

(definecd is-hbm-evnt (ev :evnt) :boolean
  :body-contracts-hints (("Goal" :do-not-induct t
                          :in-theory (enable evntp)))
  (== (second ev) 'HBM))

(defthm mget-d
  (=> (^ (twpp twpm)
         twpm)
      (natp (g :d (cddr (car twpm)))))
  :rule-classes :forward-chaining)

(in-theory (disable peer-state-nts  peer-state-mst peer-state-nbr-tctrs
                    peer-state-nbr-gctrs peer-state-nbr-scores))

(definecd transition
  (self :peer pstate :peer-state evnt :evnt twpm :twp s :nat) :peer-stateloev
  :skip-tests t
  :timeout 2000
  :ic (is-valid-twp twpm)
  :function-contract-hints (("Goal" :do-not-induct t
                             :in-theory (enable
                                         peer-statep
                                         is-hbm-evnt)))
  :body-contracts-hints (("Goal" :do-not-induct t
                          :in-theory (enable evntp
                                             update-nbr-topic-state
                                             update-nbr-topic-state1
                                             update-nbr-topic-state2
                                             update-nbr-topic-state3
                                             update-msgs-state
                                             update-msgs-state1
                                             update-msgs-state2
                                             is-hbm-evnt
                                             forward-emission
                                             forward-emission-help)))
  (b* ((defaultres (list pstate nil))
       (ntstate (peer-state-nts pstate))
       (params (cddar twpm))
       ((when (null params)) defaultres)
       (nbr-tctrs (peer-state-nbr-tctrs pstate))
       (nbr-gctrs (peer-state-nbr-gctrs pstate))
       (scores (if (is-hbm-evnt evnt)
                   (calc-nbr-scores-map nbr-tctrs nbr-gctrs twpm)
                 (peer-state-nbr-scores pstate)))
       (d (params-d params))
       (msgstate (peer-state-mst pstate))
       (rs (msgs-state-recently-seen msgstate))
       (gossips (if (is-hbm-evnt evnt)
                    (gossip-emission (car evnt) pstate d params s)
                  ()))
       ((list ntstate forwards) (forward-emission self evnt ntstate rs s d))
       ((list ntstate evnts tctrs gctrs scores)
        (update-nbr-topic-state ntstate scores nbr-tctrs nbr-gctrs evnt twpm
                                s))
       ((list msgstate evnts2 tctrs gctrs)
        (update-msgs-state msgstate evnt tctrs gctrs twpm))
       (tctrs (if (is-hbm-evnt evnt)
                  (decay-ctrs tctrs (third evnt) twpm)
                tctrs)))
    (list (peer-state ntstate
                      msgstate
                      tctrs
                      gctrs ;; TODO : will need to decay this as well
                      scores)
          (app evnts
               evnts2
               forwards
               gossips))))


(in-theory (disable update-msgs-state evntp))
