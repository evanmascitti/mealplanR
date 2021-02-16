#' Generate a shopping list
#'
#' Takes a spreadsheet file containing meals and organizes the items by store
#' layout. Optionally sums all uses of a given ingredient from multiple meals
#' into a single quantity to procure.
#'
#' @param path path to a spreadsheet file (supported formats include .csv, .xls,
#'   .xlsx)
#' @param lump whether to combine all usages of a given ingredient into one
#'   quantity. If different units are used to measure the same ingredient, the
#'   instances will be kept separate. For example, "1 lb tomatoes" and "6
#'   tomatoes" will remain separate entries.
#'
#' @return A tibble arranged by department and item
#' @export
#'
create_shopping_list <- function(path, lump = TRUE) {
#
# # ensure file is one of the acceptable types
#   if(stringr::str_detect(path, "csv")){
#     read_fn <- readr::read_csv()
#   } else if(stringr::str_detect(path, "xls")){
#     read_fn <- readxl::read_excel()
#   } else {
#     stop("\nThe path you specified is neither a .csv or .xls/.xlsx file. Did you enter the correct file directory and file name?")
#   }

# combine all instances of one item into a single entry (if lump == TRUE)

  if(lump){
    readr::read_csv(path) %>%
     # tidyr::fill(item) %>%
      dplyr::group_by(item) %>%
      tidyr::nest() %>%
     # dplyr::mutate(value = purrr::map(data, ~stringr::str_split(.$amount, pattern = " ", n=2)) ) %>%
    #  dplyr::mutate(quantity= base::as.double(purrr::map(value, ~.[[1]][1]), .null = NA),
    #                unit= purrr::map_chr(value, ~.[[1]][2])) %>%
      tidyr::unnest(data) %>%
      dplyr::group_by(item, unit, department) %>%
      dplyr::summarise(buy= sum(quantity)) %>%
      dplyr::select(department, item, buy, unit) %>%
      dplyr::arrange(department, item) %>%
      dplyr::mutate(Quantity= stringr::str_c(buy, unit, sep = " "))%>%
      dplyr::ungroup() %>%
      dplyr::select(-c(buy, unit)) %>%
      dplyr::rename(Department= department, Item= item)
} else{

# If lump == FALSE, arrange the list but leave each item instance separate

  readr::read_csv(path) %>%
 #   fill(item) %>%
    dplyr::group_by(item) %>%
    tidyr::nest() %>%
    # dplyr::mutate(value = purrr::map(data, ~str_split(.$amount, pattern = " ", n=2)) ) %>%
    # dplyr::mutate(quantity= base::as.double(purrr::map(value, ~.[[1]][1]), .null = NA) ) %>%
   # dplyr::mutate(unit= purrr::map_chr(value, ~.[[1]][2])) %>%
    tidyr::unnest(data) %>%
    dplyr::select(department, item, buy, unit) %>%
    dplyr::arrange(department, item) %>%
    dplyr::mutate(Quantity= stringr::str_c(buy, unit, sep = " "))%>%
    dplyr::ungroup() %>%
    dplyr::select(-c(buy, unit)) %>%
    dplyr::rename(Department= department, Item= item)
}

}
