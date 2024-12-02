

#' Get list of numbers by line
#'
#' @param x input file
#'
#' @return list of list of numbers
#' @export
#'
#' @examples
#' input_num_list_by_line(example_data_02())
#' d02_input <- input_num_list_by_line("./inst/input02.txt")
#' length(d02_input) #1000
#' table(unlist(lapply(d02_input, length)))
input_num_list_by_line <- function(x) {
  num_list <- strsplit(readLines(x)," ")
  num_list <- lapply(num_list, as.numeric)
  return(num_list)
}
