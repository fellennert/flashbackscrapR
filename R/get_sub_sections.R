#' Get sub-sections
#'
#' Returns the sub-sections' names and link suffixes
#'
#' @param main_section_suffix A character string containing a main section's
#' suffix (which can be obtained using \code{\link{get_main_sections}}). Needs
#' to start with \code{/}.
#'
#' @return A tibble containing the sub-sections' titles and suffixes. Please
#' note that sub-sections can also contain sub-sub-sections. These can be
#' obtained using \code{\link{get_subsub}}.
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

  name <- rvest::html_nodes(page, xpath = "//td[contains(@class, 'td_forum')]") %>%
    rvest::html_nodes("a") %>%
    rvest::html_text()

  suffix <- rvest::html_nodes(page, xpath = "//td[contains(@class, 'td_forum')]") %>%
    rvest::html_nodes("a") %>%
    rvest::html_attr("href")

  return(tibble::tibble(
    name = name,
    suffix = suffix
  ))
}

