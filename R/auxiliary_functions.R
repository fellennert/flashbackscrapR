#######################################
######### auxiliary functions #########
#######################################

# for get_links

get_n_pages_links <- function(section) {
  link <- paste0("https://www.flashback.org", section)
  page <- xml2::read_html(link)
  n_pages <- rvest::html_nodes(page, xpath = "//div[contains(@class, 'row row-forum-toolbar')]") %>%
    rvest::html_text()
  n_pages <- n_pages[2]
  n_pages <- stringr::str_split(n_pages, "av")
  n_pages <- n_pages[[1]]
  n_pages <- n_pages[2]
  n_pages <- as.numeric(stringr::str_sub(n_pages, 1, 8))
  return(n_pages)
}

generate_links <- function(suffix, n_pages) {
  n_pages <- 1:n_pages
  links <- character(length = length(n_pages))
  for (i in seq_along(n_pages)) {
    links[i] <- paste0("https://www.flashback.org", suffix, "p", i)
  }
  return(links)
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

get_thread_title <- function(page) {
  threads <- rvest::html_nodes(page,
                        xpath="//td[contains(@class, 'td_title')]") %>%
    rvest::html_text()
  threads <- threads[stringr::str_detect(threads, "\\d+.visningar")]
  thread_name <- stringr::str_extract(threads, "\\t[[:print:]]+\\n")
  thread_name <- stringr::str_replace_all(thread_name, "\\t|\\n", "")
  return(thread_name)
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

check_flyttad <- function(page) {
  check <- rvest::html_nodes(page, "#threadslist .text-center .fa") %>%
    rvest::html_attrs() %>%
    purrr::reduce(c)
  return(check != "fa fa-arrow-right")
}

# for scrape_thread

#######################################
############ scrape thread ############
#######################################

# (1.) get number of pages

get_n_pages_thread <- function(suffix) {
  page <- xml2::read_html(paste0("https://www.flashback.org", suffix))
  n_pages <- rvest::html_nodes(page, xpath = "//div[contains(@class, 'row row-forum-toolbar')]") %>%
    rvest::html_text()
  n_pages <- n_pages[2]
  n_pages <- stringr::str_split(n_pages, "av")
  n_pages <- n_pages[[1]]
  n_pages <- n_pages[2]
  n_pages <- as.numeric(stringr::str_sub(n_pages, 1, 8))
  if (is.na(n_pages) == TRUE) {
    n_pages <- 1
  }
  return(n_pages)
}

# (2.) generate links

generate_links <- function(suffix, n_pages) {
  mainpage <- "https://www.flashback.org"
  n_pages <- 1:n_pages
  links <- character(length = length(n_pages))
  for (i in seq_along(n_pages)) {
    links[i] <- paste0(mainpage, suffix, "p", i)
  }
  return(links)
}

# (3.) scrape singular pages

# (3.1) date

get_date_thread <- function(page) {
  today <- as.character(lubridate::today())
  yesterday <- as.character(lubridate::today()-1)
  date <- rvest::html_nodes(page, ".post-heading") %>%
    rvest::html_text() %>%
    stringr::str_remove_all("\n") %>%
    stringr::str_remove_all("\r") %>%
    stringr::str_remove_all("\t") %>%
    stringr::str_squish()
  date <- purrr::map_chr(stringr::str_split(date, ","), 1)
  for (k in 1:length(date)) {
    date[k] <- dplyr::if_else(stringr::str_detect(date[k], "([2][0][0-2][0-9])[-]([0-1][0-9])[-]([0-3][0-9])"),
                       date[k],
                       dplyr::if_else(stringr::str_detect(date[k], "Idag"),
                               today,
                               yesterday))
  }
  return(date)
}

# (3.2) time

get_time <- function(page) {
  return(rvest::html_nodes(page, ".post-heading") %>%
           rvest::html_text() %>%
           stringr::str_extract_all("([0-2][0-9])[:]([0-5][0-9])") %>%
           purrr::flatten_chr())
}

# (3.3) authors' names

get_author_name <- function(page) {
  return(rvest::html_nodes(page, ".post-user-username") %>%
           rvest::html_text() %>%
           stringr::str_remove_all("\n") %>%
           stringr::str_remove_all("\t"))
}

# (3.4) authors' urls

get_author_url <- function(link) {
  return(rvest::html_nodes(page, ".post-user-username") %>%
           rvest::html_attr("href"))
}

# (3.5) postings' text -- with citations

get_posting <- function(page) {
  return(rvest::html_nodes(page, ".post_message") %>%
           rvest::html_text() %>%
           stringr::str_remove_all("\n") %>%
           stringr::str_remove_all("\r") %>%
           stringr::str_remove_all("\t") %>%
           stringr::str_replace_all("[^[:alnum:]]", " ") %>%
           stringr::str_squish())
}

# (3.6) quotes

get_quote_pattern <- function(page) {
  quotes <- rvest::html_nodes(page, xpath = "//div[contains(@class, 'post-bbcode-quote')]") %>%
    rvest::html_text() %>%
    stringr::str_remove_all("\n") %>%
    stringr::str_remove_all("\r") %>%
    stringr::str_remove_all("\t") %>%
    stringr::str_replace_all("[^[:alnum:]]", " ") %>%
    stringr::str_squish()
  quotes <- quotes[stringr::str_detect(quotes, "^Citat")]
  return(paste(unique(stringr::str_sub(quotes, start = -(2/3*stringr::str_length(quotes)))), collapse = "|"))
}

# (3.7) cited users

get_quoted_user <- function(page) {
  text <- rvest::html_nodes(page, ".post_message") %>%
    rvest::html_text() %>%
    stringr::str_remove_all("\n") %>%
    stringr::str_remove_all("\r") %>%
    stringr::str_remove_all("\t") %>%
    stringr::str_replace_all("[^[:alnum:]]", " ") %>%
    stringr::str_squish()

  cit_user <- character(length = length(text))
  for (j in 1:length(text)){
    temp <- stringr::str_split(text[j], "postat av")[[1]][2]
    cit_user[j] <- stringr::word(stringr::str_trim(temp), 1)
  }
  return(cit_user)
}

# (4.) clean postings

remove_quotes <- function(posting, pattern) {
  posting_tbl <- tibble::enframe(posting)
  postings_wo_quotes <- posting_tbl %>%
    dplyr::filter(!stringr::str_detect(value, "^Citat Ursprungligen postat av"))
  postings_w_quotes <- posting_tbl %>%
    dplyr::filter(stringr::str_detect(value, "^Citat Ursprungligen postat av"))
  postings_w_quotes$text_wo_cit <- character(length = nrow(postings_w_quotes))

  for (i in 1:nrow(postings_w_quotes)) {
    if (length(stringr::str_split(postings_w_quotes$value[i], pattern = pattern, n = 2)[[1]]) == 2) {
      postings_w_quotes$text_wo_cit[i] <- stringr::str_split(postings_w_quotes$value[i], pattern = pattern, n = 2)[[1]][[2]]
    } else {
      postings_w_quotes$text_wo_cit[i] <- paste0("flawed citation", postings_w_quotes$value[i])
    }
  }

  output_tbl <- postings_w_quotes %>%
    dplyr::select(name, value = text_wo_cit) %>%
    dplyr::bind_rows(postings_wo_quotes) %>%
    dplyr::arrange(name)
  return(output_tbl$value)
}

# (5.) save it
save_it <- function(folder_name, file_name, output_tbl) {
  date_chr <- as.character(lubridate::today())

  if (is.null(file_name) == TRUE) {
    file_name <- paste0("scrape-", as.character(lubridate::today()))
  }
  readr::write_csv(output_tbl, file.path(folder_name, paste0(file_name, ".csv")))
}
