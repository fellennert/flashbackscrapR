#' Get entire main section's links
#'
#' Returns a file that allows for scraping the entire section, folder names
#' mimic the section structure
#'
#' @param suffix The section's suffix
#' @param folder_name A character vector with a folder name the scraped files
#' are supposed to be stored in
#' @param cut_off A character string containing the date at which the latest
#' post in the thread should had been posted on. Has to be in the format
#' \code{YYYY-MM-DD}. Defaults to \code{"2000-01-01"}.
#' @param delay flashback.org's robots.txt-file asks for putting a five
#' second delay between each iteration. You can deliberately ignore this by
#' setting \code{delay = FALSE}. Note that THIS IS NOT RECOMMENDED!
#'
#' @return A tibble with the name of the sub(sub) section's suffix
#' \code{sub_suffix}, the name of the folder the scraped thread should be stored
#' at \code{folder_name}, the thread links \code{thread_links}, and the
#' prospective file name \code{file_name}
#'
#' @examples
#' get_full_section_links(suffix = "/f102", folder_name = NULL,
#'   cut_off = "2020-10-25", delay = TRUE)
#'
#' @export
get_full_thread_links <- function(suffix, path, cut_off = "2000-01-01", delay = TRUE){
  initial_tibble <- tibble::tibble(
    suffix = suffix,
    folder_name = path,
    cut_off = cut_off,
    delay = delay
  )

  purrr::pmap(initial_tibble, ~{
    tibble::tibble(
      sub_suffix = suffix,
      folder_name = folder_name,
      thread_links = tryCatch(get_thread_links(suffix = suffix, cut_off = cut_off, pure_suffix = TRUE),
                              error = function(e) NA_character_)
    )
  }) %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(file_name = 1) %>%
    dplyr::group_by(folder_name) %>%
    dplyr::mutate(file_name = cumsum(file_name) %>%
                    as.character())


}
