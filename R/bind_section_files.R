#' Bind each section's files together
#'
#' Binds the files for each section into one tibble
#'
#' @param folder_name A character vector containing the name of the folders the
#' results are stored in. CSV files stored in the same folder are bound together
#' and exported into a CSV file that has the same name as the path of the folder
#' they were stored in.
#' @param export_csv A logical vector, defaults to \code{TRUE}. If \code{TRUE},
#' the concatenated tibbles are saved in a "file" folder in the folder indicated
#' by \code{folder_name}
#'
#' @return A list containing the section files.
#'
#' @examples
#' bind_section_files("test_scrape")
#'
#' @export
bind_section_files <- function(folder_name, export_csv = FALSE){

  tree <- fs::dir_ls(folder_name, recurse = TRUE)

  folder_list <- tree[!stringr::str_detect(tree, pattern = ".csv$")] %>%
    purrr::map(fs::dir_ls) %>%
    purrr::map(~.x[stringr::str_detect(.x, pattern = ".csv$")])

  prepared_names <- names(folder_list) %>%
    purrr::map_chr(~stringr::str_remove(.x, pattern = paste0(folder_name, "/"))) %>%
    purrr::map_chr(~stringr::str_replace_all(.x, pattern = c("/" = "-"))) %>%
    purrr::map_chr(~paste0(.x, ".csv"))


  tibble_list <- folder_list %>%
    purrr::map(~{purrr::map_dfr(.x,
                                ~readr::read_csv(.x, col_types = readr::cols(
                                  url = readr::col_character(),
                                  date = readr::col_date(format = ""),
                                  time = readr::col_time(format = ""),
                                  author_name = readr::col_character(),
                                  author_link = readr::col_character(),
                                  quoted_user = readr::col_character(),
                                  posting = readr::col_character(),
                                  posting_wo_quote = readr::col_character()
                                )) %>%
                                  45

                                dplyr::arrange(url, date, time))
    })

  if (export_csv == TRUE) {
    fs::dir_create(fs::path(folder_name, "files"))
    purrr::walk2(tibble_list, prepared_names, ~readr::write_csv(.x, fs::path(folder_name, "files", .y)))
  }

  purrr::compact(tibble_list)
}
