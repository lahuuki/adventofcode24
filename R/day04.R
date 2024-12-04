#' Day 04: Ceres Search
#'
#' [Ceres Search](https://adventofcode.com/2024/day/4)
#'
#' @name day04
#' @rdname day04
#' @details
#'
#' **Part One**
#'
#' \"Looks like the Chief\'s not here. Next!\" One of The Historians pulls
#' out a device and pushes the only button on it. After a brief flash, you
#' recognize the interior of the [Ceres monitoring station](/2019/day/10)!
#'
#' As the search for the Chief continues, a small Elf who lives on the
#' station tugs on your shirt; she\'d like to know if you could help her
#' with her *word search* (your puzzle input). She only has to find one
#' word: `XMAS`.
#'
#' This word search allows words to be horizontal, vertical, diagonal,
#' written backwards, or even overlapping other words. It\'s a little
#' unusual, though, as you don\'t merely need to find one instance of
#' `XMAS` - you need to find *all of them*. Here are a few ways `XMAS`
#' might appear, where irrelevant characters have been replaced with `.`:
#'
#'     ..X...
#'     .SAMX.
#'     .A..A.
#'     XMAS.S
#'     .X....
#'
#' The actual word search will be full of letters instead. For example:
#'
#'     MMMSXXMASM
#'     MSAMXMSMSA
#'     AMXSXMAAMM
#'     MSAMASMSMX
#'     XMASAMXAMM
#'     XXAMMXXAMA
#'     SMSMSASXSS
#'     SAXAMASAAA
#'     MAMMMXMMMM
#'     MXMXAXMASX
#'
#' In this word search, `XMAS` occurs a total of *`18`* times; here\'s the
#' same word search again, but where letters not involved in any `XMAS`
#' have been replaced with `.`:
#'
#'     ....XXMAS.
#'     .SAMXMS...
#'     ...S..A...
#'     ..A.A.MS.X
#'     XMASAMX.MM
#'     X.....XA.A
#'     S.S.S.S.SS
#'     .A.A.A.A.A
#'     ..M.M.M.MM
#'     .X.X.XMASX
#'
#' Take a look at the little Elf\'s word search. *How many times does
#' `XMAS` appear?*
#'
#' **Part Two**
#'
#' *(Use have to manually add this yourself.)*
#'
#' *(Try using `convert_clipboard_html_to_roxygen_md()`)*
#'
#' @param x some data
#' @return For Part One, `f04a(x)` returns .... For Part Two,
#'   `f04b(x)` returns ....
#' @export
#' @examples
#' f04a(example_data_04())
#' f04a(example_data_04(2)) #15, should be 18
#' f04a(example_data_04(3)) #15, should be 18
#' f04b()
f04a <- function(x) {

  input <- readLines(x)

  input_mat <- data.frame(strsplit(input, ""))
  colnames(input_mat) <- NULL

  # dim(input_mat)

  input_vertical <- apply(input_mat, 1, paste0, collapse = "")
  # cat(input_vertical, sep = "\n")

  matches <- c(unlist(match_horizontal), unlist(match_horizontal))

  corner_length <- sum(dim(input_mat)) -1
  input_diagonal_left <- purrr::map_chr(1:corner_length, ~f04_diagonal(.x, slant = "left", input_mat = input_mat))
  input_diagonal_right <- purrr::map_chr(1:corner_length, ~f04_diagonal(.x, slant = "right", input_mat = input_mat))

  matches <- purrr::map(list(horizontal = input,
               vertical = input_vertical,
               diag_left = input_diagonal_left,
               diag_right = input_diagonal_right),
      ~unlist(stringr::str_extract_all(.x, "SAMXMAS|XMASAMX|XMAS|SAMX")))

  # return(matches)

  ## replace double match with 2
  matches <- unlist(matches)
  matches_num <- rep(1, length(matches))
  matches_num[grepl("SAMXMAS|XMASAMX", matches)] <- 2

  return(sum(matches_num))
}


#' @rdname day04
#' @export
f04b <- function(x) {

}


f04_diagonal <- function(i, slant = c("right", "left"), input_mat = input_mat) {

  if(slant == "left"){
    letters <- purrr::map2(1:i, i:1, ~input_mat[.x,.y])

  } else if(slant == "right"){
    input_mat <- input_mat[,ncol(input_mat):1]
    letters <- purrr::map2(1:i, i:1, ~input_mat[.x,.y])
  }
  letters <- unlist(letters)
  letters <- letters[!is.na(letters)]
  return(paste0(letters, collapse = ""))

}


#' @param example Which example data to use (by position or name). Defaults to
#'   1.
#' @rdname day04
#' @export
#' @examples
#' file.create(example_data_04())
#' file.create(example_data_04(3))
example_data_04 <- function(example = 1) {
  here::here("inst","testdata",paste0("testdata04.",example,".txt"))
}
