(ns antz.core-test
  (:require [clojure.test :refer :all]
            [antz.core :refer :all]))

(defn shutdown-agents-fixture [f]
  (f)
  (if (instance? java.io.PrintWriter *out*)
    nil                 ; nrepl (Cursive)
    (shutdown-agents))) ; lein repl

(use-fixtures :once shutdown-agents-fixture)

(deftest world-test
  (testing "world is a dim-sized 2D vector"
    (with-redefs [world (make-world)]
      (is (vector? world))
      (is (= dim (count world)))
      (is (every? vector? world))
      (is (every? #(= (count %) dim) world)))))

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
        (is (contains? @(get-in world [2 4]) :ant)))
      (testing "has correct direction"
        (is (= 7 (:dir (:ant @(get-in world [2 4])))))))))

(deftest random-places-test
  (let [rplaces (random-places 10)]
    (testing "returns a set"
      (is (set? rplaces)))
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
                                (:ant @cell)))))))
      (testing "ants are home"
        (is (every? #(and (:home %) (:ant %))
                    (for [x home-range y home-range]
                      @(place [x y]))))))))

(deftest bound-test
  (testing "returns same within bounds"
    (is (= 3 (bound 8 3))))
  (testing "returns 0 is 0"
    (is (= 0 (bound 8 0))))
  (testing "returns bound is 0"
    (is (= 0 (bound 8 8))))
  (testing "wraps above bound"
    (is (= 3 (bound 8 11))))
  (testing "wraps below bound"
    (is (= 3 (bound 8 -21)))))

(deftest delta-loc-test
  (testing "moves north"
    (is (= [3 2] (delta-loc [3 3] 0))))
  (testing "moves southeast"
    (is (= [4 4] (delta-loc [3 3] 3))))
  (testing "wraps north-south"
    (is (= [3 0] (delta-loc [3 (dec dim)] 4)))))

(deftest turn-test
  (with-redefs [world (make-world)]
    (let [get-dir #(-> % place deref :ant :dir)
          loc @(nth (setup) 0)]
      (testing "turn right"
        (let [dir (get-dir loc)]
          (is (= (bound 8 (inc dir)) (get-dir (turn loc 1))))))
      (testing "360 turn clockwise"
        (let [dir (get-dir loc)]
          (is (= dir (get-dir (turn loc 8))))))
      (testing "360 turn counterclockwise"
        (let [dir (get-dir loc)]
          (is (= dir (get-dir (turn loc -8))))))
      (testing "almost 360"
        (let [dir (get-dir loc)]
          (is (= (bound 8 (dec dir)) (get-dir (turn loc 7)))))))))
