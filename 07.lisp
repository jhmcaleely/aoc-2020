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


(defun parse-bag (input-bag)
  (if (search "no other" input-bag)
      '()
      (parse-full-bag input-bag)))


(defun parse-separated-records (input item parser)
  (map 'list parser
       (map 'list #'(lambda (x) (string-left-trim '(#\Space) x))
	    (split-sequence item input))))


(defun parse-colour-and-contents (input-line)
  (let*
      ((split-token " bags contain ")
       (split-location (search split-token input-line))
       (colour (subseq input-line 0 split-location))
       (contents-string (subseq input-line
				(+ split-location
				   (length split-token)))))
    `(,colour . ,(parse-separated-records
		  contents-string
		  #\, #'parse-bag))))


(defun read-rules (filename)
  (read-parsed-line-records filename #'parse-colour-and-contents))


(defun bag-has-colour (bag-rule colour)
  (let
      ((bag-colour (car bag-rule))
       (bag-contents (cdr bag-rule)))

    (when (assoc colour bag-contents :test #'equalp)
      bag-colour)))


(defun bags-which-contain (colour rules)
  (remove nil
	  (map 'list
	       #'(lambda (x) (bag-has-colour x colour))
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


(let
    ((test-rules (read-rules "07.test-input.txt"))
     (sample-rules (read-rules "07.input.txt"))
     (part-label (format nil "Day 7, part 1:")))

  (labels
      ((count-bags (rules)
	 (length (bags-which-could-contain "shiny gold" rules))))

    (when (/= 4
	    (count-bags test-rules))
      (error "~a bag count doesn't match" part-label))

    (format t "~a ~a~%" part-label
	    (count-bags sample-rules))))
