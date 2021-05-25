(defpackage #:serapeum.tests
  (:use #:cl #:alexandria #:serapeum #:FiveAM
    #:serapeum/generalized-arrays)
  (:import-from #:trivia #:match #:ematch)
  (:shadowing-import-from #:serapeum #:let1)
  (:shadow #:test)
  (:export #:run-tests #:serapeum))
