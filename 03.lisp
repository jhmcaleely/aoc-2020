;; With the toboggan login problems resolved, you set off toward the
;; airport. While travel by toboggan might be easy, it's certainly not
;; safe: there's very minimal steering and the area is covered in
;; trees. You'll need to see which angles will take you near the
;; fewest trees.
;;
;; Due to the local geology, trees in this area only grow on exact
;; integer coordinates in a grid. You make a map (your puzzle input)
;; of the open squares (.) and trees (#) you can see. For example:
;;
;; ..##.......
;; #...#...#..
;; .#....#..#.
;; ..#.#...#.#
;; .#...##..#.
;; ..#.##.....
;; .#.#.#....#
;; .#........#
;; #.##...#...
;; #...##....#
;; .#..#...#.#
;;
;; These aren't the only trees, though; due to something you read
;; about once involving arboreal genetics and biome stability, the
;; same pattern repeats to the right many times:
;;
;; ..##.........##.........##.........##.........##.........##.......  --->
;; #...#...#..#...#...#..#...#...#..#...#...#..#...#...#..#...#...#..
;; .#....#..#..#....#..#..#....#..#..#....#..#..#....#..#..#....#..#.
;; ..#.#...#.#..#.#...#.#..#.#...#.#..#.#...#.#..#.#...#.#..#.#...#.#
;; .#...##..#..#...##..#..#...##..#..#...##..#..#...##..#..#...##..#.
;; ..#.##.......#.##.......#.##.......#.##.......#.##.......#.##.....  --->
;; .#.#.#....#.#.#.#....#.#.#.#....#.#.#.#....#.#.#.#....#.#.#.#....#
;; .#........#.#........#.#........#.#........#.#........#.#........#
;; #.##...#...#.##...#...#.##...#...#.##...#...#.##...#...#.##...#...
;; #...##....##...##....##...##....##...##....##...##....##...##....#
;; .#..#...#.#.#..#...#.#.#..#...#.#.#..#...#.#.#..#...#.#.#..#...#.#  --->
;;
;; You start on the open square (.) in the top-left corner and need to
;; reach the bottom (below the bottom-most row on your map).
;;
;; The toboggan can only follow a few specific slopes (you opted for a
;; cheaper model that prefers rational numbers); start by counting all
;; the trees you would encounter for the slope right 3, down 1:
;;
;; From your starting position at the top-left, check the position
;; that is right 3 and down 1. Then, check the position that is right
;; 3 and down 1 from there, and so on until you go past the bottom of
;; the map.
;;
;; The locations you'd check in the above example are marked here with
;; O where there was an open square and X where there was a tree:
;;
;; ..##.........##.........##.........##.........##.........##.......  --->
;; #..O#...#..#...#...#..#...#...#..#...#...#..#...#...#..#...#...#..
;; .#....X..#..#....#..#..#....#..#..#....#..#..#....#..#..#....#..#.
;; ..#.#...#O#..#.#...#.#..#.#...#.#..#.#...#.#..#.#...#.#..#.#...#.#
;; .#...##..#..X...##..#..#...##..#..#...##..#..#...##..#..#...##..#.
;; ..#.##.......#.X#.......#.##.......#.##.......#.##.......#.##.....  --->
;; .#.#.#....#.#.#.#.O..#.#.#.#....#.#.#.#....#.#.#.#....#.#.#.#....#
;; .#........#.#........X.#........#.#........#.#........#.#........#
;; #.##...#...#.##...#...#.X#...#...#.##...#...#.##...#...#.##...#...
;; #...##....##...##....##...#X....##...##....##...##....##...##....#
;; .#..#...#.#.#..#...#.#.#..#...X.#.#..#...#.#.#..#...#.#.#..#...#.#  --->
;;
;; In this example, traversing the map using this slope would cause
;; you to encounter 7 trees.
;;
;; Starting at the top-left corner of your map and following a slope
;; of right 3 and down 1, how many trees would you encounter?


(defun make-tree-map (filename)
  (let* ((map-elements (read-line-records filename))
	 (height (length map-elements))
	 (width (length (first map-elements)))
	 (map (make-array `(,width ,height))))
    (let ((y 0))
      (dolist (line map-elements)
	(dotimes (x width)
	  (setf (aref map x y) (char line x)))
	(incf y)))
    map))


(defun count-trees-on-slope-map (slope tree-map)
  (let ((across (first slope))
	(down (second slope))
	(map-width (array-dimension tree-map 0))
	(map-height (array-dimension tree-map 1)))

    (do ((x across (mod (+ across x) map-width))
	 (y down (+ down y))
	 (n 0))
	((>= y map-height) n)
      (if (eql #\# (aref tree-map x y))
	  (incf n)))))


(defun count-trees-on-slope (slope filename)
  (count-trees-on-slope-map slope (make-tree-map filename)))


(deftest test/3/1
  (= 7
     (count-trees-on-slope '(3 1) "03.test-input.txt")))


(defsolution solution/3/1 3 1
  (count-trees-on-slope '(3 1) "03.input.txt"))


;; Time to check the rest of the slopes - you need to minimize the
;; probability of a sudden arboreal stop, after all.
;;
;; Determine the number of trees you would encounter if, for each of
;; the following slopes, you start at the top-left corner and traverse
;; the map all the way to the bottom:
;;
;; Right 1, down 1.
;; Right 3, down 1. (This is the slope you already checked.)
;; Right 5, down 1.
;; Right 7, down 1.
;; Right 1, down 2.
;;
;; In the above example, these slopes would find 2, 7, 3, 4, and 2
;; tree(s) respectively; multiplied together, these produce the answer
;; 336.
;;
;; What do you get if you multiply together the number of trees
;; encountered on each of the listed slopes?


(defun compute-slope-product (filename)
  (let ((tree-map (make-tree-map filename))
	(test-slopes '((1 1) (3 1) (5 1) (7 1) (1 2))))

    (reduce #'*
	    (map 'list
		 #'(lambda (slope)
		     (count-trees-on-slope-map slope tree-map))
		 test-slopes))))


(deftest test/3/2
  (= 336
     (compute-slope-product "03.test-input.txt")))


(defsolution solution/3/2 3 2
  (compute-slope-product "03.input.txt"))

