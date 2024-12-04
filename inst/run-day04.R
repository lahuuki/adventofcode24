library(adventofcode24)
x <- "./inst/input04.txt"

(p1 <- f04a("./inst/input04.txt"))
# 2442 - too low 12/4/ 1:02pm
# 2454 - str_count
(p2 <- f04b("./inst/input04.txt"))
# 1858

stopifnot(p1 == aoc_solutions$day04a)
stopifnot(p2 == aoc_solutions$day04b)
