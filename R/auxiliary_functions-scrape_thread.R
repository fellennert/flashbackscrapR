# FOR scrape_thread -------------------------------------------------------


### 1st part: create variables to map over

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


generate_links <- function(suffix, n_pages) {
  mainpage <- "https://www.flashback.org"
  n_pages <- 1:n_pages
  links <- character(length = length(n_pages))
  for (i in seq_along(n_pages)) {
    links[i] <- paste0(mainpage, suffix, "p", i)
  }
  return(links)
}


### 2nd part: acquire meta data

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


get_time <- function(page) {
  return(rvest::html_nodes(page, ".post-heading") %>%
           rvest::html_text() %>%
           stringr::str_extract_all("([0-2][0-9])[:]([0-5][0-9])") %>%
           purrr::flatten_chr())
}


get_author_name <- function(page) {
  rvest::html_nodes(page, ".post-row:nth-child(1) .post-left") %>%
    rvest::html_text() %>%
    stringr::str_remove_all("\n") %>%
    stringr::str_remove_all("\t") %>%
    tibble::enframe() %>%
    tidyr::separate(value, sep = "fler inlägg av ", into = c("drop_it", "author")) %>%
    tidyr::separate(author, sep = "Hitta alla inlägg", into = c("author", "drop_it_2")) %>%
    dplyr::pull(author)
}


get_quoted_user <- function(page) {
  text <- rvest::html_nodes(page, ".post_message") %>%
    rvest::html_text() %>%
    stringr::str_remove_all("\n") %>%
    stringr::str_remove_all("\r") %>%
    stringr::str_remove_all("\t") %>%
    stringr::str_replace_all("[^[:alnum:]]", " ") %>%
    stringr::str_squish()

  if(length(text) == 0) return("message from moderator")

  cit_user <- character(length = length(text))
  for (j in 1:length(text)){
    temp <- stringr::str_split(text[j], "postat av")[[1]][2]
    cit_user[j] <- stringr::word(stringr::str_trim(temp), 1)
  }
  return(cit_user)
}


### 3rd part: acquire postings (with and without quotes)

## functions for acquiring postings and quotes

# acquire posting

get_posting <- function(page) {
  rvest::html_nodes(page, ".post_message") %>%
    rvest::html_text() %>%
    stringr::str_remove_all("\n") %>%
    stringr::str_remove_all("\r") %>%
    stringr::str_remove_all("\t") %>%
    stringr::str_replace_all("[^[:alnum:]]", " ") %>%
    stringr::str_replace_all("Citat", " Citat") %>%
    stringr::str_squish()
}

# acquire quotes

get_quotes <- function(page) {
  rvest::html_nodes(page, xpath = "//div[contains(@class, 'post-bbcode-quote')]") %>%
    rvest::html_text() %>%
    stringr::str_remove_all("\n") %>%
    stringr::str_remove_all("\r") %>%
    stringr::str_remove_all("\t") %>%
    stringr::str_replace_all("[^[:alnum:]]", " ") %>%
    stringr::str_replace_all("Citat", " Citat") %>%
    stringr::str_squish()
}

# remove quotes

remove_quotes <- function(posting, pattern) {
  for_extract <- stringr::str_locate_all(posting, pattern)
  for_removal <- map2(posting, for_extract, ~{
    stringr::str_sub(.x, start = .y[,1], end = .y[, 2])
    })
  remove_it <- map2(posting, for_removal, ~{
    if (length(.y) == 0) {
      return(.x)
    } else {
        stringr::str_remove_all(.x, pattern = stringr::str_c(.y, collapse = "|"))
    }
    }
    ) %>%
    purrr::reduce(c) %>%
    stringr::str_squish()

  tibble::tibble(
    posting = posting,
    posting_wo_quote = remove_it
  )
}

# final function

get_content_remove_quotes <- function(page) {
  posting <- get_posting(page)
  if (length(posting) == 0) return(tibble::tibble(posting = "0", posting_wo_quote = "0") %>% dplyr::slice(0))

  pattern <- get_quotes(page) %>%
  .[stringr::str_detect(., "^Citat")] %>%
  stringr::str_c(., collapse = "|")

  remove_quotes(posting, pattern)
}

### 4th part: save results

save_it <- function(folder_name, file_name, output_tbl) {
  date_chr <- as.character(lubridate::today())

  if (is.null(file_name) == TRUE) {
    file_name <- paste0("scrape-", as.character(lubridate::today()))
  }
  readr::write_csv(output_tbl, file.path(folder_name, paste0(file_name, ".csv")))
}
