#' Get thread titles in section
#'
#' Returns the links of threads within a section
#'
#' @param suffix A character string containing a sub-section's suffix
#' (which can be obtained using \code{\link{get_sub_sections}}). Suffixes need
#' to start with \code{/}.
#' @param delay flashback.org's robots.txt-file asks for putting a five
#' second delay between each iteration. You can deliberately ignore this by
#' setting \code{delay = FALSE}. Note that THIS IS NOT RECOMMENDED!
#'
#' @return A tibble containing the links, titles, and dates.
#'
#' @examples
#' get_thread_links(suffix = "/f245", cut_off = "2020-05-01", title = TRUE, delay = TRUE)
#'
#' @export
get_thread_titles <- function(suffix, delay = TRUE) {
  n_pages <- get_n_pages_links(suffix)
  if (is.na(n_pages) == TRUE) return(print("Error. No valid suffix supplied."))

  links <- generate_links(suffix, n_pages)
  date_ind <- lubridate::today()
  i <- 0
  thread_links <- vector("list", length = n_pages)
  thread_titles <- vector("list", length = n_pages)
  thread_dates <- vector("list", length = n_pages)

  for (i in seq_along(links)) {
    page <- xml2::read_html(links[[i]])
    thread_dates[[i]] <- get_date_links(page)
    date_ind <- tail(thread_dates[[i]], 1)
    thread_links[[i]] <- get_links(page)
    thread_links[[i]] <- thread_links[[i]][check_flyttad(page)]
    thread_titles[[i]] <- get_thread_title(page)
    thread_titles[[i]] <- thread_titles[[i]][check_flyttad(page)]
    if (delay == TRUE) Sys.sleep(5)
  }

  return_list <- list(thread_links %>% purrr::compact() %>% purrr::reduce(c),
                      thread_titles %>% purrr::compact() %>% purrr::reduce(c),
                      thread_dates %>% purrr::compact() %>% purrr::reduce(c)
  )

  length_vec <- double(length = 3)

  for (i in seq_along(length_vec)) {
    length_vec[[i]] <- length(return_list[[i]])
  }
  for (i in seq_along(length_vec)) {
    length(return_list[[i]]) <- min(length_vec)
  }

  return(tibble::tibble(
      links = return_list[[1]],
      title = return_list[[2]],
      date = return_list[[3]]
      )
    )
  }
}




