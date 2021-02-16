#' @title Populate a directory structure for recipes
#'
#'@description Standardize the hierarchy of folders and files to allow iterative recipe assembly and indexing
#'
#' @details Creates a main folder containing two blank files:
#' 1. A blank csv file in which to add ingredients
#' 2. A blank markdown file titled `\<recipe title\>_directions.md.
#' A png image should be added to the folder to complete the recipe, which will be stored as a list object in the package.
#'
#' @param title recipe title, , snake case, all lowercase letters
#' @param source character string indicating issue or book where recipe was found. For example, "Cook's Country December/January 2021"
#'
#' @return
#' @export
#'

add_recipe <- function(recipe_ID, source){

  ingredients <- tibble::tibble(recipe_ID= rep(title, 10),
                                source= source,
                                tag1= "",
                                tag2= "",
                                tag3= "",
                                recipe_component= "",
                                item= "",
                                quantity= "",
                                unit= "",
                                department= ""
  )

dir.create(paste0('inst/recipes/', title) )

readr::write_csv(ingredients, file = paste0('inst/recipes/', title, '/', title, '_ingredients.csv'))

file.create(paste0('inst/recipes/', title, '/', title, '_directions.md') )

file.create(paste0('inst/recipes/', title, '/', title, '_other_info.md') )
}
