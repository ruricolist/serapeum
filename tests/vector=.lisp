(in-package :serapeum.tests)

(def-suite vector= :in serapeum)
(in-suite vector=)

(test vector=/empty
  (is (vector= #() #()))
  (is (vector= "" ""))
  (is (vector= "" #()))
  (is (vector= #() ""))
  (is (vector= #* #()))
  (is (vector= #() #*)))

(test vector=/bounds
  (is (vector= #(x y z) #(y z) :start1 1))
  (is (vector= #(x y) #(x y z) :end2 2)))

(test vector=/strings
  (is (vector= "xyz" "xyz"))
  (is (vector= "xyz" #(#\x #\y #\z)))
  (is (not (vector= "xyz" "XYZ")))
  (is (not (vector= "xyz" #(#\X #\Y #\Z))))
  (is (not (vector= #(#\X #\Y #\Z) "xyz")))
  (is (vector= "xyz" "XYZ" :test #'char-equal))
  (is (vector= "xyz" #(#\X #\Y #\Z) :test #'char-equal))
  (is (vector= #(#\X #\Y #\Z) "xyz" :test #'char-equal))
  (is (vector= "xyz" "XYZ" :test #'equalp))
  (is (vector= "xyz" #(#\X #\Y #\Z) :test #'equalp))
  (is (vector= #(#\X #\Y #\Z) "xyz" :test #'equalp))

  (is (vector= "xyz" "yz" :start1 1))
  (is (vector= "yz" "xyz" :start2 1))

  (is (vector= "together" "frog" :start1 1 :end1 3 :start2 2))
  (is (not (vector= "together" "FROG" :start1 1 :end1 3 :start2 2)))
  (is (vector= "together" "FROG" :start1 1 :end1 3 :start2 2
                                 :test #'char-equal))
  (is (vector= "together" "FROG" :start1 1 :end1 3 :start2 2
                                 :test #'equalp)))

(test vector=/bit-vectors
  (is (vector= #*010101 #*010101))
  (is (vector= #*10101 #*010101 :start2 1))
  (is (vector= #*010101 #*10101 :start1 1))
  (is (vector= #*010101 #(0 1 0 1 0 1)))
  (is (vector= #(0 1 0 1 0 1) #*010101)))

(test vector=/numeric
  (is (not (vector= #(1 2 3) #(1.0 2.0 3.0))))
  (is (not
       (vector= (coerce #(1 2 3) 'octet-vector)
                (make-array 3 :element-type 'single-float
                              :initial-contents #(1.0f0 2.0f0 3.0f0)))))

  (is (vector= #(1 2 3) #(1.0 2.0 3.0) :test #'=))
  (is (vector= (coerce #(1 2 3) 'octet-vector)
               (make-array 3 :element-type 'single-float
                             :initial-contents #(1.0f0 2.0f0 3.0f0))
               :test #'=))

  (unless (eql -0.0 0.0)
    (is (not (vector= #(-0.0) #(0.0))))
    (is (not (vector= #(0.0) #(-0.0))))
    (is (vector= #(-0.0) #(0.0) :test #'=))
    (let ((v1 (make-array 1 :element-type 'single-float
                            :initial-contents '(0.0f0)))
          (v2 (make-array 1 :element-type 'single-float
                            :initial-contents '(-0.0f0))))
      (is (not (vector= v1 v2)))
      (is (not (vector= v2 v1)))
      (is (vector= v1 v2 :test #'=))
      (is (vector= v1 v2 :test #'equalp))
      (is (vector= v2 v1 :test #'=))
      (is (vector= v2 v1 :test #'equalp))))

  (unless (eql 0 0.0)
    (is (not (vector= #(0.0) #(0))))
    (is (not (vector= #(0) #(0.0))))
    (is (vector= #(0.0) #(0) :test #'=))
    (is (vector= #(0) #(0.0) :test #'=))

    (let ((v1 (make-array 1 :element-type 'single-float
                            :initial-contents '(0.0f0)))
          (v2 (make-array 1 :element-type '(unsigned-byte 8)
                            :initial-contents '(0))))
      (is (not (vector= v1 v2)))
      (is (not (vector= v2 v1)))
      (is (vector= v1 v2 :test #'=))
      (is (vector= v1 v2 :test #'equalp))
      (is (vector= v2 v1 :test #'=))
      (is (vector= v2 v1 :test #'equalp)))))
