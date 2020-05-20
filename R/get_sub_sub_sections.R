#' Get sub-sub-sections
#'
#' Returns the sub-sub-sections' names suffixes
#'
#' @param sub_section_suffix a string containing a sub-section's suffix
#' (which can be obtained using \code{\link{get_sub_sections}}). Suffixes need
#' to start with '/'.
#'
#' @return A tibble with the sub-sub-sections' names and suffixes. If there are
#' no sub-sub-sections, it is `NULL` and a warning is printed.
#'
#' @examples
#' get_subsub("/f243")
#'
#' @export
get_subsub <- function(sub_section_suffix) {
  link <- paste0("https://www.flashback.org", sub_section_suffix)
  page <- xml2::read_html(link)
  suffix <- rvest::html_nodes(page, xpath = "//td[contains(@class, 'td_forum')]") %>%
    rvest::html_nodes("a") %>%
    rvest::html_attr("href")

  name <- rvest::html_nodes(page, xpath = "//td[contains(@class, 'td_forum')]") %>%
    rvest::html_nodes("a") %>%
    rvest::html_text()

  tbl <- if(length(suffix) != 0) {
    tibble::tibble(
      name = name,
      suffix = suffix
    )
  } else {
    tibble::tibble(
      name = NULL,
      suffix = NULL
    )
  }
  if (is.null(tbl %>% purrr::pluck(1)) == TRUE) {
    warning("There were no sub-sub-sections found.")
  }
  return(tbl)
}
