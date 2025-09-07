(defpackage #:serapeum.tests
  (:use #:cl #:alexandria #:serapeum #:FiveAM
        #:serapeum/generalized-arrays)
  (:import-from #:serapeum/macro-tools #:*case-macro-target*)
  (:import-from #:trivia #:match #:ematch)
  (:shadowing-import-from #:serapeum/binding #:let1)
  (:shadow #:test)
  (:export #:run-tests #:serapeum))
