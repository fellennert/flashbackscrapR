# AUXILIARY FUNCTIONS FOR get_links AND get_title ---------------------------------------------

get_n_pages_links <- function(section) {
  link <- paste0("https://www.flashback.org", section)
  page <- xml2::read_html(link)
  n_pages <- rvest::html_nodes(page, xpath = "//div[contains(@class, 'row row-forum-toolbar')]") %>%
    rvest::html_text()
  if (length(n_pages) > 1) n_pages <- n_pages[2]
  n_pages %>%
    stringr::str_split("av") %>%
    purrr::pluck(1) %>%
    purrr::pluck(2) %>%
    stringr::str_sub(1, 10) %>%
    as.numeric()
}


generate_links <- function(suffix, n_pages) {
  n_pages <- 1:n_pages
  links <- character(length = length(n_pages))
  for (i in seq_along(n_pages)) {
    links[i] <- paste0("https://www.flashback.org", suffix, "p", i)
  }
  return(links)
}


get_date_links <- function(page) {
  rvest::html_nodes(page, ".td_last_post div") %>%
    rvest::html_text() %>%
    stringr::str_extract("[:digit:]{4}\\-[:digit:]{2}\\-[:digit:]{2}|Igår|Idag") %>%
    .[!is.na(.)] %>%
    stringr::str_replace_all(pattern = c("Igår" = as.character(lubridate::today() - 1),
                                         "Idag" = as.character(lubridate::today())))
}


get_links <- function(page) {
  rvest::html_nodes(page, ".pull-right+ a") %>%
    rvest::html_attr("href")
}


check_flyttad <- function(page) {
  check <- rvest::html_nodes(page, "#threadslist .text-center .fa") %>%
    rvest::html_attrs() %>%
    purrr::reduce(c)
  return(check != "fa fa-arrow-right")
}


get_thread_title <- function(page) {
  rvest::html_nodes(page, ".pull-right+ a") %>%
    rvest::html_text()
}


