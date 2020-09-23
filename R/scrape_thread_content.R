#' Scrape thread
#'
#' Scrapes a certain thread
#'
#' @param suffix A character string containing a thread's suffix (which can be
#' obtained using \code{\link{get_thread_links()}}). Suffixes need to start with
#' \code{/}.
#' @param export_csv A logical vector. Defaults to \code{FALSE}. The function
#' can automatically save the output in a csv file. If \code{export_csv = TRUE}
#' , a csv file is exported. The output folder can be specified using the
#' \code{folder} argument.
#' @param folder_name A character string which specifies the name of the folder
#' the output should be saved in. The folder's name is added to the path of the
#' current working directory which can be obtained using \code{getwd()} and
#' modified with \code{setwd()}. If nothing is specified and
#' \code{export_csv = TRUE}, the function will export the csv file straight into
#' the working directory.
#' @param file_name A character string which specifies the name of the output
#' file. It is not necessary to add `.csv`. If no file name is provided,
#' \code{file_name} defaults to \code{scrape_[YYYY-MM-DD].csv}.
#' @param delay A logical vector, defaults to \code{TRUE}. flashback.org's
#' robots.txt-file asks for putting a five second delay between each iteration.
#' You can deliberately ignore this by setting \code{delay = FALSE}. Note that
#' THIS IS NOT RECOMMENDED!
#'
#' @return A character vector containing the links.
#'
#' @examples
#' scrape_thread_content(suffix = "/t3145103", export_csv = TRUE, folder_name = "results", file_name = "test", delay = TRUE)
#'
#' @export
scrape_thread_content <- function(suffix, export_csv = FALSE, folder_name = NULL, file_name = NULL, delay = TRUE) {
  n_pages <- get_n_pages_thread(suffix = suffix)
  url_vec <- generate_links(suffix = suffix, n_pages = n_pages)

  if (delay == TRUE) {
    pages <- purrr::map(url_vec, ~{
      Sys.sleep(5)
      xml2::read_html(.x)
    })
  }

  if (delay == FALSE) {
    pages <- purrr::map(url_vec, ~{
      xml2::read_html(.x)
    })
  }

  if (stringr::str_detect(rvest::html_nodes(pages[[1]], ".text-warning") %>%
                            rvest::html_text(),
                          "Du är inte inloggad eller också har du inte behörighet att se den här sidan.")
      &&
      length(rvest::html_nodes(pages[[1]], ".text-warning") %>%
                            rvest::html_text()) != 0) {
    stop("Login required.")
  }
  if (stringr::str_detect(rvest::html_node(pages[[1]], "h2") %>%
                            rvest::html_text(),
                          "Ojdå! Sidan kunde inte hittas")
      &&
      length(rvest::html_node(pages[[1]], "h2") %>%
                            rvest::html_text()) != 0) {
    stop("Page not found.")
  }


  output_tbl <- tibble::tibble(
    url = suffix,
    date = lubridate::ymd(purrr::map(pages, get_date_thread) %>% unlist() %>% .[!is.na(.)]),
    time = purrr::map(pages, get_time) %>% unlist(),
    author_name = purrr::map(pages, get_author_name) %>% unlist(),
    author_link = purrr::map(pages, get_author_link) %>% unlist(),
    quoted_user = purrr::map(pages, get_quoted_user) %>% unlist() %>% .[. != "message from moderator"]
  ) %>%
    dplyr::bind_cols(purrr::map_dfr(pages, get_content_remove_quotes))

  if (export_csv == TRUE) save_it(folder_name, file_name, output_tbl)
  if (export_csv == FALSE & is.null(folder_name) == FALSE | is.null(file_name) == FALSE) {
    save_it(folder_name, file_name, output_tbl)
  }
  return(output_tbl)
}
