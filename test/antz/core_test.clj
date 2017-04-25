(ns antz.core-test
  (:require [clojure.test :refer :all]
            [antz.core :refer :all]))

(deftest world-test
  (testing "world is a dim-sized 2D vector"
    (with-redefs [world (make-world)]
      (is (vector? world))
      (is (= dim (count world)))
      (is (vector? (first world)))
      (is (= dim (count (first world)))))))

(deftest place-test
 (testing "place returns correct place"
   (with-redefs [world (make-world)]
     (is (identical? ((world 2) 4) (place [2 4])))
     (is (not (identical? ((world 3) 4) (place [3 5])))))))

(deftest create-ant-test
  (with-redefs [world (make-world)]
    (let [ant (create-ant [2 4] 7)]
      (testing "create-ant returns an agent"
        (is (= clojure.lang.Agent (class ant))))
      (testing "agent contains correct location"
        (is (= [2 4] @ant)))
      (testing "is placed at world"
        (is (contains? @((world 2) 4) :ant)))
      (testing "has correct direction"
        (is (= (:dir (:ant @((world 2) 4))) 7))))))

(deftest random-places-test
  (let [rplaces (random-places 10)]
    (testing "returns a set"
      (is (= clojure.lang.PersistentHashSet (class rplaces))))
    (testing "required number of items"
      (is (= 10 (count rplaces))))
    (testing "items are 2-sized vectors within dim"
      (is (every?
            #(and (vector? %)
                  (= 2 (count %))
                  (<= 0 (first %) (dec dim))
                  (<= 0 (second %) (dec dim)))
            rplaces)))))

(deftest setup-test
  (with-redefs [world (make-world)]
    (let [ants (setup)]
      (testing "placed correct amount of food"
        (is (= food-places
               (count
                 (remove zero? (for [rows world
                                     cell rows]
                                 (:food @cell)))))))
      (testing "placed correct amount of ants"
        (is (= (* nants-sqrt nants-sqrt)
               (count
                 (remove nil? (for [rows world
                                    cell rows]
                                (:ant @cell))))))))))
