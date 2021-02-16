#' \lifecycle{experimental}
#' @title Populate a blank csv file with column names
#'
#' @description Useful for making manual lists (i.e. not from specific meals)
#'
#' @param date date of planned shopping trip
#'
#' @return file silently written to disk in the `inst/shopping_lists` directory
#' @export
#'
manual_csv_list <- function(date = Sys.Date()){
  tibble::tibble(meal_ID= "",
                 tag1= "",
                 tag2= "",
                 tag3="",
                 item="",
                 quantity= "",
                 unit="",
                 department=""
  ) %>%
    readr::write_csv(file = paste0(here::here('inst', 'shopping_lists'), "/", date, "_shopping_list.csv"))

  message(crayon::green('Please verify that file was written to disk.'))
}
