(in-package "ACL2S")
(include-book "nbrs-topics-state")

(defdata payload-peer (list payload-type peer))
(defdata pid-peer (list pid-type peer))

(defdata msg-peer (or payload-peer pid-peer))
(defdata lomsg-peer (listof msg-peer))

;; recently seen is an alist of message/message id and age
;; consists of all messages seen in the last 120 seconds
(defdata msgpeer-age (alistof msg-peer rational))

(check= (msgpeer-agep
	 '( (((|100| |10| FM AN) MX) . 2)
            ((|10| P2) . 10)
            (((|3lolol| |10| PL MX) P2) . 90)
            ((|12| P3) . 10)))
        t)


(defdata mcache (listof payload-peer))
(check= (mcachep '(((|1000| |10| PL MX)  P3)
		   ((|2000| |10| NT PT) P4)))
        t)

;;waiting for payload corresponding to pid sent by peer
(defdata msgs-waiting-for (listof pid-peer))

(defdata msgs-state
  (record (recently-seen . msgpeer-age)
          ;; LRU cache of seen message IDs.
	  (pld-cache     . mcache)
          ;; list of messages
          (hwindows      . lon)  ;;history windows : each nat is the number of
          ;;messages received in the last HeartBeat interval
	  (waitingfor    . msgs-waiting-for) ;;received IHAVE, sent IWANT
                                             ;;waiting for full message
          (served        . msgpeer-age) ;; peers whose IWANT was served. Count
                                        ;; of servings to avoid spam.
          (ihaves-received . nat)
          (ihaves-sent     . nat)))


(defthm lonp->tlp
  (=> (lonp x)
      (tlp x)))

(defthm nu
  (=>
   (AND (NOT (CONSP VAR-HWINDOWS))
        VAR-HWINDOWS)
   (not (lonp VAR-HWINDOWS))))


(defthm msgs-state-check
  (== (msgs-statep
       (MSET
        :0TAG 'MSGS-STATE
        (MSET
         :SERVED VAR-SERVED
         (MSET :HWINDOWS
               VAR-HWINDOWS
               (MSET :PLD-CACHE
                     VAR-PLD-CACHE
                     (MSET :WAITINGFOR VAR-WAITINGFOR
                           (MSET :RECENTLY-SEEN
                                 VAR-RECENTLY-SEEN
                                 (MSET :IHAVES-RECEIVED VAR-IHAVES-RECEIVED
                                       (MSET :IHAVES-SENT VAR-IHAVES-SENT
                                             NIL)))))))))
      (^ (lonp var-hwindows)
         (msgpeer-agep var-recently-seen)
         (mcachep var-pld-cache)
         (msgs-waiting-forp var-waitingfor)
         (msgpeer-agep var-served)
         (natp var-ihaves-received)
         (natp var-ihaves-sent)))
  :hints (("Goal" :in-theory (enable msgs-statep))))

(definec increase-ihaves-received (mst :msgs-state n :nat) :msgs-state
  (mset :ihaves-received (+ n (msgs-state-ihaves-received mst)) mst))

(definec reset-ihaves-received (mst :msgs-state) :msgs-state
  (mset :ihaves-received 0 mst))

(defconst *max-want-serves* 3) ;; max number of times IWANTS are served
(defconst *max-ihaves* 20) ;; max number of ihaves set/received per heartbeat


(definecd payload2pid (pld :payload-type) :pid-type
  (second pld))


(definecd mk-pid-peers (pids :lopid sender :peer) :msgs-waiting-for
  (match pids
    (() '())
    ((pid . rst) (cons (list pid sender)
                       (mk-pid-peers rst sender)))))

(defthm union-waitingfor
  (=> (^ (msgs-waiting-forp l1)
         (msgs-waiting-forp l2))
      (msgs-waiting-forp (union-equal l1 l2))))

(definecd update-waitingfor (mst :msgs-state pids :lopid sender :peer) :msgs-state
  (mset :waitingfor (union-equal (mk-pid-peers pids sender)
                                 (msgs-state-waitingfor mst))
        mst))

(definec new-msgs-state () :msgs-state
  (msgs-state nil nil nil nil nil 0 0))

(create-map* (lambda (pldm) (if (payload-peerp pldm)
                                (payload2pid (car pldm))
                              (first pldm)))
             lomsg-peerp
             lopidp
             (:name rs->pids))

(property lomsgs (rs :msgpeer-age)
  (lomsg-peerp (strip-cars rs)))
             

(definecd add-to-recentlyseen (pldm :payload-peer rs :msgpeer-age) :msgpeer-age
  (if (in (second (car pldm))
          (map* rs->pids (strip-cars rs)))
      rs
    `((,pldm . 0) . ,rs)))

(definecd update-m-age (rs :msgpeer-age elapsed :pos-rat seenttl :nat) :msgpeer-age
  (case-match rs
    (() nil)
    (t (if (> (+ elapsed (cdar rs)) seenttl)
           (update-m-age (cdr rs) elapsed seenttl)
         (cons `(,(caar rs) . ,(+ elapsed (cdar rs)))
               (update-m-age (cdr rs) elapsed seenttl))))))
  
(definecd in-mcache (pid :pid-type mcache :mcache) :boolean
  (cond ((endp mcache) nil)
	((== pid (second (caar mcache))) t)
	(t (in-mcache pid (cdr mcache)))))

(defconst *waittime* 1)

(set-ignore-ok t)
;; We abstract out the details of message validation,
;; and assume that all messages delivered are valid
(definecd isValidPayload (pld :payload-type) :boolean
  t)



(definec custom-nth-payload (i :nat top :topic originator :peer) :payload-type
  (list (intern (concatenate 'string
                                          "FULL"
                                          (str::nat-to-dec-string i))
                             "ACL2")
                     (intern (concatenate 'string
                                          "PID"
                                          (str::nat-to-dec-string i))
                             "ACL2")
                     top
                     originator))

(create-map* (lambda (p) (second (car p)))
             mcachep
             lopidp
             (:name get-pids))

(definecd get-plds (pids :lopid mc :mcache) :lopld
  (match mc
    (() '())
    ((p . rst) (if (in (second (car p)) pids)
                   (cons (car p)
                         (get-plds pids rst))
                 (get-plds pids rst)))))

(create-map* (lambda (pld p1 p2) `(,p1 SND ,p2 PAYLOAD ,pld))
             lopldp
             loevp
             (:name send-plds)
             (:fixed-vars ((peerp p1) (peerp p2))))


(definecd get-ihave-senders (waitingfor :msgs-waiting-for) :lop
  (match waitingfor
    (() '())
    ((p . rst) (cons (second p)
                     (get-ihave-senders rst)))))

(create-reduce* + natp lonp)


(definecd increment-car (ls :lon) :lon
  (if (endp ls)
      '()
    (cons (1+ (car ls)) (cdr ls))))

(definecd add-ppm-mcache (ppm :payload-peer mc :mcache) :mcache
  (if (in-mcache (payload2pid (first ppm)) mc)
      mc
    (cons ppm mc)))

(definecd pids-to-serve (nbr :peer pids :lopid servd :msgpeer-age) :lopid
  (match pids
    (() '())
    ((pid . rst) (let ((n (cdr (assoc-equal `(,pid ,nbr) servd))))
                   (if (^ (natp n)
                          (>= n *max-want-serves*))
                       (pids-to-serve nbr rst servd)
                     (cons pid (pids-to-serve nbr rst servd)))))))


(property remove-served-assoc (nbr :peer pid :pid-type servd :msgpeer-age)
          (msgpeer-agep
           (remove-assoc-equal (list pid nbr) servd)))

(definec update-served (nbr :peer pids :lopid servd :msgpeer-age) :msgpeer-age
  (match pids
    (() servd)
    ((pid . rst) (let ((n (cdr (assoc-equal `(,pid ,nbr) servd))))
                   (update-served nbr
                                  rst
                                  (if (natp n)
                                      `(((,pid ,nbr) . ,(1+ n))
  . ,(remove-assoc-equal (list pid nbr) servd))
                                    `(((,pid ,nbr) . 1) . ,servd)))))))
                     
(property remove-assoc-nbr-counters-map (nbr-counters-map :pt-tctrs-map nbr :peer top :topic)
          (pt-tctrs-mapp (remove-assoc-equal `(,nbr . ,top) nbr-counters-map)))


(create-tuple* msgs-state loev pt-tctrs-map p-gctrs-map)

(skip-proofs
 (definecd update-msgs-state (mst :msgs-state evnt :evnt
                                  pcmap :pt-tctrs-map
                                  gcmap :p-gctrs-map
                                  twpm :twp)
   :msgs-stateloevpt-tctrs-mapp-gctrs-map
   :skip-tests t
   :timeout 600
  (b* ((rs      (msgs-state-recently-seen mst))
       (mcache  (msgs-state-pld-cache mst))
       (hwins   (msgs-state-hwindows mst))
       (waiting (msgs-state-waitingfor mst))
       (servd   (msgs-state-served mst))
       (ihr     (msgs-state-ihaves-received mst))
       (ihs     (msgs-state-ihaves-sent mst))
       (default-res `(,mst nil ,pcmap ,gcmap)))
    (match evnt
      ((p1 'APP & pld)
       (b* ((pldm (list pld p1)))
         (list (msgs-state (add-to-recentlyseen pldm rs)
                           (cons pldm mcache)
			   (increment-car hwins)
                           waiting
                           servd
                           ihr
                           ihs)
               nil
               pcmap
               gcmap)))
      ((p1 'RCV nbr 'PAYLOAD pld)
       (b* ((top (third pld))
            (cs (lookup-tctrs nbr top pcmap))
            ;; don't update firstMessageDeliveries everytime, check mcache
	    (pldm (list pld nbr)))
	 (if (isValidPayload pld)
	     `(,(msgs-state (add-to-recentlyseen pldm rs)
                            (add-ppm-mcache pldm mcache)
			    (if (in-mcache (payload2pid pld) mcache)
				hwins
			      (increment-car hwins))
                            waiting
                            servd
                            ihr
                            ihs)
	       nil
	       ,(put-assoc-equal `(,nbr . ,top)
                                (increment-firstMessageDeliveries
                                 (increment-meshMessageDeliveries cs))
                                pcmap)
               gcmap)
	   `(,mst
	     nil
	     ,(put-assoc-equal `(,nbr . ,top)
                               (increment-invalidMessageDeliveries cs)
                               pcmap)
             gcmap))))
      ((p1 'RCV nbr 'IHAVE pids)
       (b* (((unless (< ihr *max-ihaves*)) default-res)
            (dont-haves
             (remove-duplicates-equal
              (set-difference-equal
               (set-difference-equal pids (map* rs->pids (strip-cars rs)))
               (map* get-pids mcache))))
            ((unless (consp dont-haves)) default-res))
         (list (increase-ihaves-received
                (update-waitingfor mst dont-haves nbr)
                (len (remove-duplicates-equal pids)))
               `((,p1 SND ,nbr IWANT ,dont-haves))
               pcmap
               gcmap)))
      ((p1 'RCV nbr 'IWANT pids)
       (b* ((pds (pids-to-serve nbr pids servd))
            ((unless (consp pds)) default-res)
            (tsrv (intersection-equal (map* get-pids mcache) pds))
            ((unless (consp tsrv)) default-res))
         (list (msgs-state rs
                           mcache
                           hwins
                           waiting
                           (update-served nbr tsrv servd)
                           ihr
                           ihs)
                  (map* send-plds (remove-duplicates-equal (get-plds tsrv mcache)) p1 nbr)
                  pcmap
                  gcmap)))
      ((p1 'HBM elapsed)
       (b*  ((wp (cdar twpm))
             (params (cdr wp))
             ((when (null params)) default-res)
             (mcl     (params-mcacheLen params))
	     (new-hwins (grab mcl (cons 0 hwins)))
             (new-mcache (grab (reduce* + 0 new-hwins)
                               mcache))
	     (seenttl (params-seenTTL params))
	     (new-mst (msgs-state (update-m-age rs elapsed seenttl)
				  new-mcache
				  new-hwins
				  waiting
                                  servd
                                  0  ;; reset ihaves received 
                                  0)))  ;;reset ihaves sent.
       (if (and (consp waiting)
                (< *waittime* elapsed))
           (let ((senders (get-ihave-senders waiting)))
             `(,new-mst
               nil
               ;;,(increment-badbehaviours pcmap senders 'FM)))
               ,pcmap
               ,gcmap))
         ;TODO : IHAVE PIDS must also be sent with their topic
         `(,new-mst
           nil
           ,pcmap
           ,gcmap))))
      (& default-res)))))


