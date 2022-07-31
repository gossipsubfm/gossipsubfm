(in-package "ACL2S")

(include-book "nbrs-topics-state")
(include-book "msgs-state")

(defdata peer-state
  (record (nts . nbr-topic-state)
	  (mst . msgs-state)
	  (nbr-tctrs . pt-tctrs-map)
          (nbr-gctrs . p-gctrs-map)
	  (nbr-scores . peer-rational-map)))

;;initializes a new peer state if we need one, or don't have one already in the network
(definec new-peer-state () :peer-state
  (peer-state (new-nbr-topic-state)
	      (new-msgs-state)
	      '()
              '()
	      '()))

(defthm peer-state-check
  (equal (peer-statep (MSET :mst ms
			    (MSET :nts es
				  (MSET :0TAG 'peer-state
                                        (MSET :nbr-scores nsm
					      (MSET :nbr-tctrs ntcm
                                                    (MSET :nbr-gctrs ngcm
                                                          NIL)))))))
         (and (nbr-topic-statep es)
              (msgs-statep ms)
              (peer-rational-mapp nsm)
	      (pt-tctrs-mapp ntcm)
              (p-gctrs-mapp ngcm)))
  :hints (("goal" :in-theory (enable peer-statep))))

(create-map* (lambda (nbr p pids) `(,p SND ,nbr IHAVE ,pids))
             lopp
             loevp
             (:fixed-vars ((peerp p) (lopidp pids)))
             (:name mapgossips))

;;emit gossip to d random nbrs
(definecd gossip-emission (p :peer ps :peer-state d :nat params :params s :nat) :loev
  :skip-tests t
  :SKIP-BODY-CONTRACTSP T
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

(create-tuple* nbr-topic-state loev pt-tctrs-map p-gctrs-map)

(skip-proofs
 (definecd forward-emission (self :peer source :peer ntst :nbr-topic-state payload
                                  :payload-type d :nat k :nat) :nbr-topic-stateloevpt-tctrs-mapp-gctrs-map
    (b* ((ms (nbr-topic-state-topic-mesh ntst))
         (fout (nbr-topic-state-topic-fanout ntst))
         (nt (remove-assoc-equal self (nbr-topic-state-nbr-topicsubs ntst)))
         (top (third payload))
         (orig (fourth payload))
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
                         payload)
                   nil
                   nil))
            ((consp fout-nbrs) (list (update-last-pub ntst newlp)
                                     (map* mapforwards fout-nbrs self
                                           payload)
                                     nil
                                     nil))
            (t (b* ((new-fout-nbrs
                     (grab d (shuffle (collect-forward-nbrs top nt) k)))
                    ((when (endp new-fout-nbrs)) (list ntst nil nil nil)))
                 (list (update-topic-fanout
                        (update-last-pub ntst newlp)
                        (cons `(,top . ,new-fout-nbrs) fout))
                       (map* mapforwards (set-difference-equal
                                          (cdr (assoc-equal top ms))
                                          `(,self ,source ,orig))
                             self
                             payload)
                       nil
                       nil)))))))

;; TODO : add all other decays as well, including global decays
(definecd decay-ctrs (ptc :pt-tctrs-map hbmint :pos-rat twpm :twp) :pt-tctrs-map
  :skip-tests t
  :timeout 600
  :SKIP-BODY-CONTRACTSP T
  :SKIP-FUNCTION-CONTRACTP T
  (match ptc
    (() '())
    ((((peer . top) . tctrs) . rst)
     (b* ((params (cddr (assoc-equal top twpm)))
          ((when (null params)) (cons (car ptc)
                                      (decay-ctrs rst hbmint twpm)))
	  (e (floor hbmint (params-decayInterval params)))
	  (decay-mmd (* (expt (tctrs-meshMessageDeliveries tctrs) e)
			(params-meshMessageDeliveriesDecay params)))
	  (decay-mmd (if (< decay-mmd (params-decayToZero params))
			 0
		       decay-mmd))
	  (decay-fmd (* (expt (tctrs-firstMessageDeliveries tctrs) e)
			(params-firstMessageDeliveriesDecay params)))
	  (decay-fmd (if (< decay-fmd (params-decayToZero params))
			 0
		       decay-fmd)))
       (cons `(,(cons peer top) . ,(update-firstMessageDeliveries
				    (update-meshMessageDeliveries
				     tctrs
				     decay-mmd)
				    decay-fmd))
	     (decay-ctrs rst hbmint twpm))))))

(create-tuple* peer-state loev)
(skip-proofs
 (definecd transition (self :peer pstate :peer-state evnt :evnt twpm :twp s :nat) :peer-stateloev
  ;:skip-tests t
  :timeout 600
  :ic (is-valid-twp twpm)
  (b* (((when (null twpm)) (list pstate nil))
       (ntstate (peer-state-nts pstate))
       (wp (cdar twpm))
       (params (cdr wp))
       (nbr-tctrs (peer-state-nbr-tctrs pstate))
       (nbr-gctrs (peer-state-nbr-gctrs pstate))
       (scores (if (== (second evnt) 'HBM)
                   (calc-nbr-scores-map nbr-tctrs nbr-gctrs twpm)
                 (peer-state-nbr-scores pstate)))
       ((when (null params)) (list pstate nil))
       (d (params-d params))
       (msgstate (peer-state-mst pstate))
       (rs (msgs-state-recently-seen msgstate))
       (ntstxforwardsxnn (if (^ (== (second evnt) 'RCV)
                                (== (fourth evnt) 'PAYLOAD)
                                (!= self (fourth (fifth evnt))) ;;if not the publisher
                                (! (in (payload2pid (fifth evnt))
                                       (map* rs->pids (strip-cars rs)))))
                             (forward-emission self (third evnt) ntstate (fifth evnt)
                                               d s)
                           nil))
       (ntstxforwardsxnn (if (== (second evnt) 'APP)
                             (forward-emission self self ntstate (fourth evnt)
                                               d s)
                           ntstxforwardsxnn))
       (ntstate (if (consp ntstxforwardsxnn)
                    (first ntstxforwardsxnn)
                  ntstate))
       (forwards (second ntstxforwardsxnn))
       (gossips (if (== (second evnt) 'HBM)
                    (gossip-emission (car evnt) pstate d params s)
                  nil))
       (ntxevntsxtctrsxgctrsxscores (update-nbr-topic-state ntstate
                                                            scores
                                                            nbr-tctrs
                                                            nbr-gctrs
                                                            evnt
                                                            twpm
                                                            s))
       (ntstate (first ntxevntsxtctrsxgctrsxscores))
       (scores (fifth ntxevntsxtctrsxgctrsxscores))
       (res2 (update-msgs-state msgstate
                                evnt
                                (third ntxevntsxtctrsxgctrsxscores) ;;pcmap
                                (fourth ntxevntsxtctrsxgctrsxscores) ;;gcmap
                                twpm)))
    (list (peer-state ntstate
		      (car res2)
                      (if (== (second evnt) 'HBM)
                          (decay-ctrs (third res2) (third evnt) twpm)
                        (third res2))
                      nbr-gctrs ;; TODO : will need to decay this as well
                      scores)
	  (append (second res2)
                  (second ntxevntsxtctrsxgctrsxscores)
                  forwards
                  gossips)))))

(in-theory (disable update-msgs-state evntp))
