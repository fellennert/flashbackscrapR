#' Get missing urls
#'
#' If the process has stopped, you can read in the files and determine the
#' missing ones
#'
#' @param link_vec A vector containing the links that should have been scraped
#' @param folder_name A character vector with a folder name the scraped files
#' are stored in
#'
#' @return The link tibble containing the links that haven't been scraped so far
#'
#' @examples
#' get_full_section_subs(link_tbl = links_science, folder_name = "science")
#'
#' @export
get_missing_threads <- function(link_vec, folder_name){
  suppressMessages(
  links_scraped <- fs::dir_ls(folder_name, recurse = TRUE) %>%
    purrr::map(purrr::safely(~readr::read_csv(.x, col_types = readr::cols(
      url = readr::col_character(),
      date = readr::col_date(format = ""),
      time = readr::col_time(format = ""),
      author_name = readr::col_character(),
      author_link = readr::col_character(),
      quoted_user = readr::col_character(),
      posting = readr::col_character(),
      posting_wo_quote = readr::col_character()
    ),
    n_max = 5))) %>%
    purrr::map_dfr("result") %>%
    dplyr::distinct(url)
  )
  link_vec[!link_vec %in% links_scraped$url]
}
