#' Get sub-sections
#'
#' Returns the sub-sections' names and suffixes
#'
#' @param main_section_suffix a string containing a main section's suffix
#' (which can be obtained using \code{\link{get_main_sections}})
#'
#' @return a character vector containing the sub-sections suffixes.
#' Please note that sub-sections can also contain sub-sub-sections.
#' These can be obtained using \code{\link{get_subsub}}
#'
#' @examples
#' get_sub("/f4")
#'
#' @export
get_sub <- function(main_section_suffix) {
  check <- get_main_sections() %>% purrr::pluck(2)
  if (!main_section_suffix %in% check) {
    stop("Invalid value: suffix does not refer to an existing main section.")
  }
  link <- paste0("https://www.flashback.org", main_section_suffix)
  page <- xml2::read_html(link)
  sub <- rvest::html_nodes(page, xpath = "//td[contains(@class, 'td_forum')]") %>%
    rvest::html_nodes("a") %>%
    rvest::html_attr("href")
  return(sub)
}
