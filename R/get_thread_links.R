#' Get thread links in section
#'
#' Returns the links of threads within a section
#'
#' @param suffix A string containing a sub-section's suffix
#' (which can be obtained using \code{\link{get_sub_sections}}). Suffixes need
#' to start with '/'.
#' @param cut_off A character string containing the date at which the latest post in the
#' thread should have been posted on. Has to be in the format "YYYY-MM-DD".
#' Defaults to "2000-01-01".
#' @param delay flashback.org's robots.txt-file asks for putting a five
#' second delay between each iteration. You can ignore that by setting
#' `delay = FALSE`.
#'
#' @return A character vector containing the links.
#'
#' @examples
#' get_thread_links(suffix = "/f245", cut_off = "2020-05-01", delay = TRUE)
#'
#' @export
get_thread_links <- function(suffix, cut_off = "2000-01-01", delay = TRUE) {
  n_pages <- get_n_pages_links(suffix)
  if (is.na(n_pages) == TRUE) return(print("Error. No valid suffix supplied."))

  links <- generate_links(suffix, n_pages)
  date_ind <- lubridate::today()
  i <- 0
  thread_links <- vector("list", length = n_pages)
  thread_dates <- vector("list", length = n_pages)

  while((date_ind >= lubridate::ymd(cut_off)) && (i < length(links))) {
    i <- i + 1
    page <- xml2::read_html(links[[i]])
    thread_dates[[i]] <- get_date_links(page)
    date_ind <- tail(thread_dates[[i]], 1)
    thread_links[[i]] <- get_links(page)
    thread_links[[i]] <- thread_links[[i]][check_flyttad(page)]
    if (delay == TRUE) {
      Sys.sleep(5)
    }
  }
  return(tibble::tibble(
    links = thread_links %>% purrr::compact() %>% purrr::reduce(c),
    date = thread_dates %>% purrr::compact() %>% purrr::reduce(c)
    ) %>%
    dplyr::filter(date >= lubridate::ymd(cut_off)) %>%
    purrr::pluck(1))
}




