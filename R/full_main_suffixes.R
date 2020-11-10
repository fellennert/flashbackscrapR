#' Get entire main section
#'
#' Returns a file that allows for scraping the entire section, folder names
#' mimic the section structure
#'
#' @param main_section_suffix The section's suffix
#' @param folder_name A character vector containing the folder name the scraped
#' files are supposed to be stored in
#'
#' @return A tibble with two columns: the sub(sub)section's suffix and a folder
#' name based on the given folder name and the sub(sub)section's name
#'
#' @examples
#' get_full_section_subs(main_section_suffix = "/f102", folder_name = NULL)
#'
#' @export
get_full_section_subs <- function(main_section_suffix, folder_name = NULL) {
  subs <- get_sub(main_section_suffix) %>%
    dplyr::mutate(sub = name %>%
                    stringr::str_to_lower() %>%
                    stringr::str_replace_all("[^[:alnum:]]", " ") %>%
                    stringr::str_squish() %>%
                    stringr::str_replace_all("[^[:alnum:]]", "_") %>%
                    stringr::str_replace_all(pattern = c("å" = "a",
                                                         "ä" = "a",
                                                         "ö" = "o",
                                                         "ü" = "u"))) %>%
    tibble::rowid_to_column("indicator")

  oldw <- getOption("warn")
  options(warn = -1)

  suppressWarnings(
  subsubs <- purrr::map2(subs$indicator, subs$suffix, ~tibble::tibble(
    indicator = .x,
    subsub = tryCatch(get_subsub(.y) %>% dplyr::pull("name"),
                             error = function(e) NA_character_),
    subsub_suffix = tryCatch(get_subsub(.y) %>% dplyr::pull("suffix"),
                             error = function(e) NA_character_)
    )) %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(subsub = subsub %>%
                    stringr::str_to_lower() %>%
                    stringr::str_replace_all("[^[:alnum:]]", " ") %>%
                    stringr::str_squish() %>%
                    stringr::str_replace_all("[^[:alnum:]]", "_") %>%
                    stringr::str_replace_all(pattern = c("å" = "a",
                                                         "ä" = "a",
                                                         "ö" = "o",
                                                         "ü" = "u")))
  )

  output <- dplyr::bind_rows(
    subs %>% dplyr::left_join(subsubs, by = "indicator") %>%
      dplyr::mutate(path = dplyr::case_when(is.na(subsub) ~ fs::path(sub),
                                            TRUE ~ fs::path(sub, subsub)),
                    suffix = dplyr::case_when(is.na(subsub_suffix) ~ suffix,
                                            TRUE ~ subsub_suffix)) %>%
      dplyr::select(suffix, path),
    subs %>% dplyr::left_join(subsubs, by = "indicator") %>%
      tidyr::drop_na() %>%
      dplyr::distinct(sub, suffix) %>%
      dplyr::mutate(path = fs::path(sub, "data")) %>%
      dplyr::select(suffix, path)
  )

  if (!is.null(folder_name)) return(output %>% dplyr::mutate(path = fs::path(folder_name, path)))
  output
}
