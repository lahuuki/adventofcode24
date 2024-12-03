#' Day 03: Mull It Over
#'
#' [Mull It Over](https://adventofcode.com/2024/day/3)
#'
#' @name day03
#' @rdname day03
#' @details
#'
#' **Part One**
#'
#' \"Our computers are having issues, so I have no idea if we have any
#' Chief Historians [in
#' stock]{title="There's a spot reserved for Chief Historians between the green toboggans and the red toboggans. They've never actually had any Chief Historians in stock, but it's best to be prepared."}!
#' You\'re welcome to check the warehouse, though,\" says the mildly
#' flustered shopkeeper at the [North Pole Toboggan Rental
#' Shop](/2020/day/2). The Historians head out to take a look.
#'
#' The shopkeeper turns to you. \"Any chance you can see why our computers
#' are having issues again?\"
#'
#' The computer appears to be trying to run a program, but its memory (your
#' puzzle input) is *corrupted*. All of the instructions have been jumbled
#' up!
#'
#' It seems like the goal of the program is just to *multiply some
#' numbers*. It does that with instructions like `mul(X,Y)`, where `X` and
#' `Y` are each 1-3 digit numbers. For instance, `mul(44,46)` multiplies
#' `44` by `46` to get a result of `2024`. Similarly, `mul(123,4)` would
#' multiply `123` by `4`.
#'
#' However, because the program\'s memory has been corrupted, there are
#' also many invalid characters that should be *ignored*, even if they look
#' like part of a `mul` instruction. Sequences like `mul(4*`, `mul(6,9!`,
#' `?(12,34)`, or `mul ( 2 , 4 )` do *nothing*.
#'
#' For example, consider the following section of corrupted memory:
#'
#'     xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))
#'
#' Only the four highlighted sections are real `mul` instructions. Adding
#' up the result of each instruction produces *`161`*
#' (`2*4 + 5*5 + 11*8 + 8*5`).
#'
#' Scan the corrupted memory for uncorrupted `mul` instructions. *What do
#' you get if you add up all of the results of the multiplications?*
#'
#' **Part Two**
#'
#' *(Use have to manually add this yourself.)*
#'
#' *(Try using `convert_clipboard_html_to_roxygen_md()`)*
#'
#' @param x some data
#' @return For Part One, `f03a(x)` returns .... For Part Two,
#'   `f03b(x)` returns ....
#' @export
#' @examples
#' f03a(example_data_03()) #161
f03a <- function(x) {

  input <- readLines(x)

  mul_matches <- unlist(stringr::str_extract_all(input, "mul\\([0-9]+,[0-9]+\\)"))
  n1 <- as.numeric(gsub("mul\\(([0-9]+),[0-9]+\\)","\\1", mul_matches))
  n2 <- as.numeric(gsub("mul\\([0-9]+,([0-9]+)\\)","\\1", mul_matches))

  return(sum(n1 * n2))

}


#' @rdname day03
#' @export
#' @examples
#' # example code
#' f03b(example_data_03(2)) #48
f03b <- function(x) {

  input <- readLines(x)

  mul_matches <- unlist(stringr::str_extract_all(input, "mul\\([0-9]+,[0-9]+\\)|do\\(\\)|don't\\(\\)"))

  # dont <- which(mul_matches == "don't()")
  # do <- which(mul_matches == "do()")

  do_mul <- TRUE
  mul_todo <- list()

  for(m in mul_matches){

    if(m == "don't()") do_mul <- FALSE
    else if(m == "do()") do_mul  <- TRUE
    else if(do_mul){
      # message("do ", m)
      mul_todo <- c(mul_todo, m)
    } else {
      # message("dont ", m)
    }

  }

  n1 <- as.numeric(gsub("mul\\(([0-9]+),[0-9]+\\)","\\1", mul_todo))
  n2 <- as.numeric(gsub("mul\\([0-9]+,([0-9]+)\\)","\\1", mul_todo))

  return(sum(n1 * n2))

}


#' @param example Which example data to use (by position or name). Defaults to
#'   1.
#' @rdname day03
#' @export
#' @examples
#' file.create(example_data_03())
example_data_03 <- function(example = 1) {
  here::here("inst","testdata",paste0("testdata03.",example,".txt"))
}
