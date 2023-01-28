(define (find-bound vec target first-val)
  (define (ptr-narrow left right)
    (let* ([mid (quotient (+ left right) 2)]
           [mid-val (vector-ref vec mid)])
      (cond [(> left right) -1]
            [(= target mid-val) (if first-val
                                    (if (or (= mid left)
                                            (< (vector-ref vec (sub1 mid)) target))
                                        mid
                                        (ptr-narrow left (sub1 right)))
                                    (if (or (= mid right)
                                            (> (vector-ref vec (add1 mid)) target))
                                        mid
                                        (ptr-narrow (add1 left) right)))]
            [(> mid-val target) (ptr-narrow left (sub1 mid))]
            [else (ptr-narrow (add1 mid) right)])))
  (ptr-narrow 0 (sub1 (vector-length vec))))


(define (search-range nums target)
  (if (empty? nums)
      '(-1 -1)
      (let* ([nums (list->vector nums)]
             [fst-val (find-bound nums target #t)])
        (if (= fst-val -1)
            '(-1 -1)
            (list fst-val
                  (find-bound nums target #f))))))