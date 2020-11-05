#' Get missing urls
#'
#' If the process has stopped, you can read in the files and determine the
#' missing ones
#'
#' @param link_tbl A tibble containing the thread links that are to be scraped;
#' thread links need to be stored in a column named thread_links
#' @param folder_name A character vector with a folder name the scraped files
#' are stored in
#'
#' @return A tibble with two columns: the sub(sub)section's suffix and a folder
#' name based on the given folder name and the sub(sub)section's name
#'
#' @examples
#' get_full_section_subs(main_section_suffix = "/f102", folder_name = NULL)
#'
#' @export
get_missing_urls <- function(link_tbl, folder_name){
  links_scraped <- fs::ir_tree("science") %>%
  purrr::map(purrr::safely(~vroom::vroom(.x, col_types = readr::cols(
    url = readr::col_character(),
    date = readr::col_date(format = ""),
    time = readr::col_time(format = ""),
    author_name = readr::col_character(),
    author_link = readr::col_character(),
    quoted_user = readr::col_character(),
    posting = readr::col_character(),
    posting_wo_quote = readr::col_character()
  )))) %>%
  purrr::map_dfr("result") %>%
  dplyr::distinct(url)

  link_tbl %>% dplyr::filter(!thread_links %in% links_scraped$url)
}
