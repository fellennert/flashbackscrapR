#' Update section file
#'
#' New thread posts need to be updated once in a while
#'
#' @param last_scrape A character vector indicating the date the urls were
#' scraped for the last time
#' @param section_suffix The suffix for the main section the newer threads are
#' to be taken from
#' @param old_tbl The former scrape
#'
#' @return The old tibble, updated with the new postings
#'
#' @examples
#' update_threads(last_scrape = "2020-11-11", section_suffix = "/f8", old_tibble = old_tbl)
#'
#' @export
update_threads <- function(last_scrape, section_suffix, old_tibble){
  dplyr::mutate(cut_off = last_scrape,
                delay = FALSE) %>%
    purrr::pmap(get_full_thread_links) %>%
    dplyr::bind_rows() %>%
    dplyr::select(-sub_suffix, -file_name) %>%
    dplyr::mutate(folder_name = "temp",
                  delay = FALSE) %>%
    tibble::rowid_to_column(var = "file_name") %>%
    dplyr::select(suffix, folder_name, file_name, delay) %>%
    purrr::pwalk(purrr::safely(scrape_thread_content))

  new_things <- fs::dir_map("temp", readr::read_csv, col_types = readr::cols(
      url = readr::col_character(),
      date = readr::col_date(format = ""),
      time = readr::col_time(format = ""),
      author_name = readr::col_character(),
      author_link = readr::col_character(),
      quoted_user = readr::col_character(),
      posting = readr::col_character(),
      posting_wo_quote = readr::col_character()
    )) %>%
    dplyr::bind_rows()

  old_tibble %>%
    dplyr::filter(!url %in% new_things$url) %>%
    dplyr::bind_rows(new_things) %>%
    dplyr::arrange(url, date, time)
}
