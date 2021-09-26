;; --- Day 7: Handy Haversacks ---
;;
;; You land at the regional airport in time for your next flight. In
;; fact, it looks like you'll even have time to grab some food: all
;; flights are currently delayed due to issues in luggage processing.
;;
;; Due to recent aviation regulations, many rules (your puzzle input)
;; are being enforced about bags and their contents; bags must be
;; color-coded and must contain specific quantities of other
;; color-coded bags. Apparently, nobody responsible for these
;; regulations considered how long they would take to enforce!
;;
;; For example, consider the following rules:
;;
;; light red bags contain 1 bright white bag, 2 muted yellow bags.
;; dark orange bags contain 3 bright white bags, 4 muted yellow bags.
;; bright white bags contain 1 shiny gold bag.
;; muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
;; shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
;; dark olive bags contain 3 faded blue bags, 4 dotted black bags.
;; vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
;; faded blue bags contain no other bags.
;; dotted black bags contain no other bags.
;;
;; These rules specify the required contents for 9 bag types. In this
;; example, every faded blue bag is empty, every vibrant plum bag
;; contains 11 bags (5 faded blue and 6 dotted black), and so on.
;;
;; You have a shiny gold bag. If you wanted to carry it in at least
;; one other bag, how many different bag colors would be valid for the
;; outermost bag? (In other words: how many colors can, eventually,
;; contain at least one shiny gold bag?)
;;
;; In the above rules, the following options would be available to
;; you:
;;
;; A bright white bag, which can hold your shiny gold bag directly.
;; A muted yellow bag, which can hold your shiny gold bag directly,
;; plus some other bags.
;; A dark orange bag, which can hold bright white and muted yellow
;; bags, either of which could then hold your shiny gold bag.
;; A light red bag, which can hold bright white and muted yellow bags,
;; either of which could then hold your shiny gold bag.
;;
;; So, in this example, the number of bag colors that can eventually
;; contain at least one shiny gold bag is 4.
;;
;; How many bag colors can eventually contain at least one shiny gold
;; bag? (The list of rules is quite long; make sure you get all of
;; it.)


(defun parse-full-bag (input-bag)
  (multiple-value-bind (count remainder-start)
      (parse-integer input-bag :junk-allowed t)
    (let*
	((colour-location
	   (search " bag" input-bag))
	 (colour-name
	   (subseq input-bag
		   (1+ remainder-start)
		   colour-location)))
      `(,colour-name . ,count))))


(defun parse-record-list (input item parser)
  (if (search "no other" input)		; special case the empty list
      nil					
      (map 'list parser	  ; map the parser over a cleaned set of items
	   (map 'list #'(lambda (x) (string-left-trim '(#\Space) x))
		(split-sequence item input)))))


(defun parse-colour-and-contents (input-line)
  (let*
      ((split-token " bags contain ")
       (split-location (search split-token input-line))
       (colour (subseq input-line 0 split-location))
       (contents-string (subseq input-line
				(+ split-location
				   (length split-token)))))
    `(,colour . ,(parse-record-list
		  contents-string
		  #\, #'parse-full-bag))))


(defun read-rules (filename)
  (read-parsed-line-records filename #'parse-colour-and-contents))


(defun bag-colour (bag-rule)
  (car bag-rule))


(defun bag-contents (bag-rule)
  (cdr bag-rule))


(defun bag-contains-colour (bag-rule colour)
  (when (assoc colour (bag-contents bag-rule) :test #'equalp)
    (bag-colour bag-rule)))


(defun bags-which-contain (colour rules)
  (remove nil
	  (map 'list
	       #'(lambda (x) (bag-contains-colour x colour))
	       rules)))


(defun bags-which-could-contain (colour rules)
  (let*
      ((possible-bags
	 (bags-which-contain colour rules))
       (possible-containers
	 (mapcan #'(lambda (x) (bags-which-contain x rules))
		 possible-bags)))

    (remove-duplicates
     (append possible-bags
	     possible-containers
	     (mapcan #'(lambda (x) (bags-which-could-contain x rules))
		     possible-containers)))))


(deftest test/7/1a
  (= 9
     (length (read-rules "07.test-input.txt"))))


(defun count-bags (filename)
  (length
   (bags-which-could-contain
    "shiny gold"
    (read-rules filename))))


(deftest test/7/1b
  (= 4
     (count-bags "07.test-input.txt")))


(defsolution solution/7/1 7 1
  (count-bags "07.input.txt"))


;; --- Part Two ---
;;
;; It's getting pretty expensive to fly these days - not because of
;; ticket prices, but because of the ridiculous number of bags you
;; need to buy!
;;
;; Consider again your shiny gold bag and the rules from the above
;; example:
;;
;; faded blue bags contain 0 other bags.
;; dotted black bags contain 0 other bags.
;; vibrant plum bags contain 11 other bags: 5 faded blue bags and 6
;;  dotted black bags.
;; dark olive bags contain 7 other bags: 3 faded blue bags and 4
;;  dotted black bags.
;;
;; So, a single shiny gold bag must contain 1 dark olive bag (and the
;; 7 bags within it) plus 2 vibrant plum bags (and the 11 bags within
;; each of those): 1 + 1*7 + 2 + 2*11 = 32 bags!
;;
;; Of course, the actual rules have a small chance of going several
;; levels deeper than this example; be sure to count all of the bags,
;; even if the nesting becomes topologically impractical!
;;
;; Here's another example:
;;
;; shiny gold bags contain 2 dark red bags.
;; dark red bags contain 2 dark orange bags.
;; dark orange bags contain 2 dark yellow bags.
;; dark yellow bags contain 2 dark green bags.
;; dark green bags contain 2 dark blue bags.
;; dark blue bags contain 2 dark violet bags.
;; dark violet bags contain no other bags.
;;
;; In this example, a single shiny gold bag must contain 126 other
;; bags.
;;
;; How many individual bags are required inside your single shiny gold
;; bag?


(defun container-bags (rule)
  (if (eq (bag-contents rule) nil)
      0
      (reduce #'+
	      (map 'list
		   #'cdr
		   (bag-contents rule)))))


(defun bag-rule (bag-colour rules)
  (assoc bag-colour rules :test #'equalp))


(defun all-contained-bags (bag-colour rules)
  (let*
      ((rule (bag-rule bag-colour rules))
       (contents (bag-contents rule)))

    (labels
	((contained-bags ()
	   (reduce #'+
		   (map 'list
			#'bags-within
			contents)))

	 (bags-within (bag)
	   (* (cdr bag) (all-contained-bags (car bag) rules))))

      (+ (container-bags rule)
	 (contained-bags)))))


(deftest test/7/2a
  (let ((test-rules (read-rules "07.test-input.txt")))
    (labels
	((test (colour count)
	   (= (container-bags (bag-rule colour test-rules)) count)))

      (and
       (test "faded blue" 0)
       (test "dotted black" 0)
       (test "vibrant plum" 11)
       (test "dark olive" 7)
       (= 32
	  (all-contained-bags "shiny gold" test-rules))))))


(deftest test/7/2b
  (= 126
     (all-contained-bags
      "shiny gold"
      (read-rules "07-2.test-input.txt"))))


(defsolution solution/7/2 7 2
  (all-contained-bags
   "shiny gold"
   (read-rules "07.input.txt")))

