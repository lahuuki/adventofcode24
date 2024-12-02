#' Day 02: Red-Nosed Reports
#'
#' [Red-Nosed Reports](https://adventofcode.com/2024/day/2)
#'
#' @name day02
#' @rdname day02
#' @details
#'
#' **Part One**
#'
#' Fortunately, the first location The Historians want to search isn\'t a
#' long walk from the Chief Historian\'s office.
#'
#' While the [Red-Nosed Reindeer nuclear fusion/fission
#' plant](/2015/day/19) appears to contain no sign of the Chief Historian,
#' the engineers there run up to you as soon as they see you. Apparently,
#' they *still* talk about the time Rudolph was saved through molecular
#' synthesis from a single electron.
#'
#' They\'re quick to add that - since you\'re already here - they\'d really
#' appreciate your help analyzing some unusual data from the Red-Nosed
#' reactor. You turn to check if The Historians are waiting for you, but
#' they seem to have already divided into groups that are currently
#' searching every corner of the facility. You offer to help with the
#' unusual data.
#'
#' The unusual data (your puzzle input) consists of many *reports*, one
#' report per line. Each report is a list of numbers called *levels* that
#' are separated by spaces. For example:
#'
#'     7 6 4 2 1
#'     1 2 7 8 9
#'     9 7 6 2 1
#'     1 3 2 4 5
#'     8 6 4 4 1
#'     1 3 6 7 9
#'
#' This example data contains six reports each containing five levels.
#'
#' The engineers are trying to figure out which reports are *safe*. The
#' Red-Nosed reactor safety systems can only tolerate levels that are
#' either gradually increasing or gradually decreasing. So, a report only
#' counts as safe if both of the following are true:
#'
#' -   The levels are either *all increasing* or *all decreasing*.
#' -   Any two adjacent levels differ by *at least one* and *at most
#'     three*.
#'
#' In the example above, the reports can be found safe or unsafe by
#' checking those rules:
#'
#' -   `7 6 4 2 1`: *Safe* because the levels are all decreasing by 1 or 2.
#' -   `1 2 7 8 9`: *Unsafe* because `2 7` is an increase of 5.
#' -   `9 7 6 2 1`: *Unsafe* because `6 2` is a decrease of 4.
#' -   `1 3 2 4 5`: *Unsafe* because `1 3` is increasing but `3 2` is
#'     decreasing.
#' -   `8 6 4 4 1`: *Unsafe* because `4 4` is neither an increase or a
#'     decrease.
#' -   `1 3 6 7 9`: *Safe* because the levels are all increasing by 1, 2,
#'     or 3.
#'
#' So, in this example, *`2`* reports are *safe*.
#'
#' Analyze the unusual data from the engineers. *How many reports are
#' safe?*
#'
#' **Part Two**
#'
#' *(Use have to manually add this yourself.)*
#'
#' *(Try using `convert_clipboard_html_to_roxygen_md()`)*
#'
#' @param x some data
#' @return For Part One, `f02a(x)` returns .... For Part Two,
#'   `f02b(x)` returns ....
#' @export
#' @examples
#' f02a(example_data_02()) #2
#' f02b(example_data_02()) #4

#' @rdname day02
#' @export
f02a <- function(x) {

  input <- input_num_list_by_line(x)

  safety_list <- lapply(input, FUN=function(line){

    diff <- line[-1] - line[1:(length(line)-1)]

    one_direction <- all(diff > 0) | all(diff < 0)
    one2three <- all(abs(diff) %in% 1:3)

    # message(sprintf("%s\tone_direction: %s, one2three: %s",paste(diff, collapse =","), one_direction, one2three))

    return(one_direction & one2three)
  })

  # return(safety_list)
  return(sum(unlist(safety_list)))
}

f02b <- function(x){

  input <- input_num_list_by_line(x)

  safety_list <- lapply(input, FUN=function(line){

    any_safe <- lapply(1:length(line), function(i){
      line_i <- line[-i]

      diff <- line_i[-1] - line_i[1:(length(line_i)-1)]

      one_direction <- all(diff > 0) | all(diff < 0)
      one2three <- all(abs(diff) %in% 1:3)

      # message(sprintf("%s\tone_direction: %s, one2three: %s",paste(diff, collapse =","), one_direction, one2three))

      return(one_direction & one2three)
    })
    return(any(unlist(any_safe)))
  })

  return(sum(unlist(safety_list)))
}


#' @param example Which example data to use (by position or name). Defaults to
#'   1.
#' @rdname day02
#' @export
#' file.create(example_data_02())
example_data_02 <- function(example = 1) {
  here::here("inst","testdata",paste0("testdata02.",example,".txt"))
}
