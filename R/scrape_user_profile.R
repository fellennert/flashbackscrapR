#' Scrape profiles
#'
#' Scrapes a certain user profile
#'
#' @param suffix A character string containing a user profile's suffix.
#'
#' @return A tibble containing the user name, the date a user joined, their status, and the number of postings.
#'
#' @examples
#' scrape_user_profile(suffix = "/u1000001")
#'
#' @export
scrape_user_profile <- function(suffix) {
  content <- xml2::read_html(paste0("https://flashback.org", suffix))

  tibble::tibble(
    author_name = rvest::html_nodes(content, "#site-left .panel-title") %>% rvest::html_text(),
    status = rvest::html_nodes(content, "#site-left .col-sm-6 span") %>% rvest::html_text(),
    joining_date = rvest::html_nodes(content, "#site-left strong") %>%
      rvest::html_text() %>%
      purrr::pluck(1) %>%
      lubridate::ymd(),
    no_postings = rvest::html_nodes(content, "#site-left strong") %>%
      rvest::html_text() %>%
      purrr::pluck(2) %>%
      gsub(" ", "", .) %>%
      stringr::str_remove_all("[^0-9]") %>%
      readr::parse_number()
  )
}
