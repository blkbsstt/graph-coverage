(ns graph-coverage.core-test
  (:require [clojure.test :refer :all]
            [graph-coverage.core :refer :all]))

(def graph {:init  #{1} :final #{7}
            :nodes #{1 2 3 4 5 6 7}
            :edges {1 #{7 2}
                    2 #{3 4}
                    3 #{2}
                    4 #{5 6}
                    5 #{6}
                    6 #{1}}})

(def foo 5)
(def bar 5)
(def baz 10)
(def amazing 100)

(deftest a-test
  (testing "FIXME, I fail."
    (is (= 1 1))))

(deftest another-test
  (testing "FIXME, I fail."
    (is (= 0 1))))
