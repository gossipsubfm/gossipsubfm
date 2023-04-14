(b* ((TOP 'SUB1)
     (P 'a)
     (PCM `((P4 . ,(gctrs 0 0 0))
            (P5 . ,(gctrs 0 0 0))
            (P6 . ,(gctrs 0 0 0))))
     (PTC '(((a . agg)
     (:0tag . tctrs)
     (:firstmessagedeliveries . 200)
     (:invalidmessagedeliveries . 0)
     (:meshfailurepenalty . 0)
     (:meshmessagedeliveries . 200)
     (:meshtime . 18))
    ((a . blocks)
     (:0tag . tctrs)
     (:firstmessagedeliveries
      .
      1931172904250426286042693055531112535860378709774933/9536743164062500000000000000000000000000000000000)
     (:invalidmessagedeliveries . 0)
     (:meshfailurepenalty . 0)
     (:meshmessagedeliveries . 200)
     (:meshtime . 18))
    ((a . sub1)
     (:0tag . tctrs)
     (:firstmessagedeliveries . 1)
     (:invalidmessagedeliveries . 0)
     (:meshfailurepenalty . 0)
     (:meshmessagedeliveries . 1)
     (:meshtime . 18))
    ((a . sub2)
     (:0tag . tctrs)
     (:firstmessagedeliveries . 200)
     (:invalidmessagedeliveries . 0)
     (:meshfailurepenalty . 0)
     (:meshmessagedeliveries
      .
      1004122441830686632294424130273013541808122410736825701749/5000000000000000000000000000000000000000000000000000000)
     (:meshtime . 18))
    ((a . sub3)
     (:0tag . tctrs)
     (:firstmessagedeliveries . 200)
     (:invalidmessagedeliveries . 0)
     (:meshfailurepenalty . 0)
     (:meshmessagedeliveries
      .
      1004122441830686632294424130273013541808122410736825701749/5000000000000000000000000000000000000000000000000000000)
     (:meshtime . 18)))))
  (list
   (cons 'AGG (calcScoreTopic (lookup-tctrs p 'AGG ptc)
                              (mget 'AGG *ETH-TWP*)))
   (cons 'BLOCKS (calcScoreTopic (lookup-tctrs p 'BLOCKS ptc)
                              (mget 'BLOCKS *ETH-TWP*)))
   (cons 'SUB1 (calcScoreTopic (lookup-tctrs p 'SUB1 ptc)
                              (mget 'SUB1 *ETH-TWP*)))
   (cons 'SUB2 (calcScoreTopic (lookup-tctrs p 'SUB2 ptc)
                              (mget 'SUB2 *ETH-TWP*)))
   (cons 'SUB3 (calcScoreTopic (lookup-tctrs p 'SUB3 ptc)
                              (mget 'SUB3 *ETH-TWP*)))
   (cons 'TOTAL (lookup-score p (calc-nbr-scores-map ptc pcm *eth-twp*))))
  )
;; P1, AGG
;; 8.29
;; 22.21, -4.5, 7.80, -25, 7.78

((agg . 29369/2500)
 (blocks . 58958/3125)
 (sub1 . -617389/25000)
 (sub2 . 95743/12500)
 (sub3 . 95743/12500)
 (total . 530937/25000))


30.57

0.5 AGG + 0.8 BLOCKS

(+ (* 1/2 241/250) (* 4/5  5324/625))  = 91209/12500 = 7.29

SUB -> -859/40*1/3 =  -24.7
1163/300 * 1/3 = 3.2
1163/300 * 1/3 = 3.2

(b* ((r 5)
     (i 3)
     (n (+ r i))) 
  (+ 91209/12500  (* (/ i n) -123467/5000) (* (/ r n) 24037/7500)))



(b* ((TOP 'SUB1)
     (P 'a)
     (PCM `((P4 . ,(gctrs 0 0 0))
            (P5 . ,(gctrs 0 0 0))
            (P6 . ,(gctrs 0 0 0))))
     (PTC '(((a . agg)
             (:0tag . tctrs)
             (:firstmessagedeliveries . 0)
             (:invalidmessagedeliveries . 0)
             (:meshfailurepenalty . 0)
             (:meshmessagedeliveries . 0)
             (:meshtime . 18))
            ((a . blocks)
             (:0tag . tctrs)
             (:firstmessagedeliveries
              .
              46008787464430585368875106329584802797329409111655477273325438580218565338069414404288389803515154489/18189894035458564758300781250000000000000000000000000000000000000000000000000000000000000000000000000)
             (:invalidmessagedeliveries . 0)
             (:meshfailurepenalty . 0)
             (:meshmessagedeliveries . 0)
             (:meshtime . 18))
            ((a . sub1)
             (:0tag . tctrs)
             (:firstmessagedeliveries . 0)
             (:invalidmessagedeliveries . 0)
             (:meshfailurepenalty . 0)
             (:meshmessagedeliveries . 0)
             (:meshtime . 18))
            ((a . sub2)
             (:0tag . tctrs)
             (:firstmessagedeliveries . 0)
             (:invalidmessagedeliveries . 0)
             (:meshfailurepenalty . 0)
             (:meshmessagedeliveries
              .
              4139436357334027246709327557718968643455734211887603076562989171827174620673845343707590655598304937906301659001/5000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000)
             (:meshtime . 18))
            ((a . sub3)
             (:0tag . tctrs)
             (:firstmessagedeliveries . 0)
             (:invalidmessagedeliveries . 0)
             (:meshfailurepenalty . 0)
             (:meshmessagedeliveries
              .
              4139436357334027246709327557718968643455734211887603076562989171827174620673845343707590655598304937906301659001/5000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000)
             (:meshtime . 18)))))
       (list
   (cons 'AGG (calcScoreTopic (lookup-tctrs p 'AGG ptc)
                              (mget 'AGG *ETH-TWP*)))
   (cons 'BLOCKS (calcScoreTopic (lookup-tctrs p 'BLOCKS ptc)
                              (mget 'BLOCKS *ETH-TWP*)))
   (cons 'SUB1 (calcScoreTopic (lookup-tctrs p 'SUB1 ptc)
                              (mget 'SUB1 *ETH-TWP*)))
   (cons 'SUB2 (calcScoreTopic (lookup-tctrs p 'SUB2 ptc)
                              (mget 'SUB2 *ETH-TWP*)))
   (cons 'SUB3 (calcScoreTopic (lookup-tctrs p 'SUB3 ptc)
                              (mget 'SUB3 *ETH-TWP*)))
   (cons 'TOTAL (lookup-score p (calc-nbr-scores-map ptc pcm *eth-twp*))))
  )



