#' Get sub-sub-sections
#'
#' Returns the sub-sub-sections' names suffixes
#'
#' @param sub_section_suffix a string containing a sub-section's suffix
#' (which can be obtained using \code{\link{get_sub_sections}}). Suffixes need
#' to start with '/'.
#'
#' @return A character vector with the sub-sub-sections. If there are no sub-sub-sections, it is `NULL`.
#'
#' @examples
#' get_subsub("/f243")
#'
#' @export
get_subsub <- function(sub_section_suffix) {
  link <- paste0("https://www.flashback.org", sub_section_suffix)
  page <- xml2::read_html(link)
  temp <- rvest::html_nodes(page, xpath = "//td[contains(@class, 'td_forum')]") %>%
    rvest::html_nodes("a") %>%
    rvest::html_attr("href")

  sub_sub <- if(length(temp) != 0) {
    temp
  } else {
    NULL
  }
  if (is.null(sub_sub) == TRUE) {
    warning("There were no sub-sub-sections found.")
  }
  return(sub_sub)
}
