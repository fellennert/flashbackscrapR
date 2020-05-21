#' Scrape thread
#'
#' Scrapes a certain thread
#'
#' @param suffix A string containing a thread's suffix
#' (which can be obtained using \code{\link{get_suffixs}}). Suffixes need
#' to start with '/'.
#' @param folder_name A string. The function can automatically save the results
#' in a csv-file. `folder_name` specifies the name of the folder. If nothing is
#' specified, the function will export nothing.
#' @param file_name A string. The function can automatically save the results
#' in a csv-file. `file_name` specifies the name of the output file. It is not
#' necessary to add `.csv`. If a folder name but no file name is provided,
#' \code{\link{file_name}}) defaults to `scrape_[YYYY-MM-DD].csv`.
#' @param WINDOWS Set to true if you are on a Windows machine and want to export
#' the data in a .csv file.
#' @param delay flashback.org's robots.txt-file asks for putting a five
#' second delay between each iteration. You can ignore that by setting
#' `delay = FALSE`.
#'
#' @return A character vector containing the links.
#'
#' @examples
#' scrape_thread_content(suffix = "/t3145103", folder_name = NULL) #debug!!!!!
#'
#' @export
scrape_thread_content <- function(suffix, folder_name = NULL, file_name = NULL, WINDOWS = FALSE) {
  n_pages <- get_n_pages_thread(suffix = suffix)
  url_vec <- generate_links(suffix = suffix, n_pages = n_pages)
  pages <- purrr::map(url_vec, xml2::read_html)
  post_and_pattern_tbl <- tibble::tibble(
    posting = purrr::map(pages, get_posting) %>% unlist(),
    pattern = purrr::map(pages, get_quote_pattern) %>% purrr::reduce(paste(sep = "|"))
  )
  output_tbl <- tibble::tibble(
    url = suffix,
    date = lubridate::ymd(purrr::map(pages, get_date_thread) %>% unlist()),
    time = purrr::map(pages, get_time) %>% unlist(),
    author_name = purrr::map(pages, get_author_name) %>% unlist(),
    posting = post_and_pattern_tbl$posting,
    posting_wo_quote = remove_quotes(post_and_pattern_tbl$posting, post_and_pattern_tbl$pattern),
    quoted_user = purrr::map(pages, get_quoted_user) %>% unlist()
  )
  def_file_name <- dplyr::if_else(is.null(file_name),
                                  paste0("scrape", "_", as.character(lubridate::today), ".csv"),
                                  file_name)
  if (is.null(folder_name) == FALSE & WINDOWS == FALSE) {
    readr::write_csv(output_tbl, paste(folder_name, def_file_name, sep = "/"))
  }
  if (is.null(folder_name) == FALSE & WINDOWS == TRUE) {
    readr::write_csv(output_tbl, paste(folder_name, def_file_name, sep = "\\"))
  }
  return(output_tbl)
}
