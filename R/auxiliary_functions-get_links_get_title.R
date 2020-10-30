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
  date <- rvest::html_nodes(page, ".td_last_post div") %>%
    rvest::html_text()
  date <- stringr::str_extract_all(date, "[:digit:]{4}\\-[:digit:]{2}\\-[:digit:]{2}|Igår|Idag") %>%
    unlist()
  for (i in seq_along(date)) {
    if (date[i] == "Idag") {
      date[i] <- as.character(lubridate::today())
    } else {
      if (date[i] == "Igår") {
        date[i] <- as.character(lubridate::today() - 1)
      } else {
        date[i] <- date[i]
      }
    }
  }
  return(lubridate::ymd(date))
}


get_links <- function(page) {
  threads <- rvest::html_nodes(page,
                        xpath="//td[contains(@class, 'td_title')]") %>%
    rvest::html_nodes("a") %>%
    rvest::html_attr("href")
  threads <- threads[stringr::str_detect(threads, "/t")]
  threads <- stringr::str_split(threads, "l")
  threads <- purrr::map_chr(threads, 1)
  threads <- stringr::str_split(threads, "p")
  threads <- purrr::map_chr(threads, 1)
  threads <- stringr::str_split(threads, "s")
  threads <- purrr::map_chr(threads, 1)
  threads <- unique(threads)

  return(threads)
}


check_flyttad <- function(page) {
  check <- rvest::html_nodes(page, "#threadslist .text-center .fa") %>%
    rvest::html_attrs() %>%
    purrr::reduce(c)
  return(check != "fa fa-arrow-right")
}


get_thread_title <- function(page) {
  threads <- rvest::html_nodes(page,
                        xpath="//td[contains(@class, 'td_title')]") %>%
    rvest::html_text()
  threads <- threads[stringr::str_detect(threads, "\\d+.visningar")]
  thread_name <- stringr::str_extract(threads, "\\t[[:print:]]+\\n")
  thread_name <- stringr::str_replace_all(thread_name, "\\t|\\n", "")
  return(thread_name)
}


