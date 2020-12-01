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

insist_scrape_page <- function(x) {
  purrr::insistently(xml2::read_html, purrr::rate_backoff(pause_base = 5, pause_cap = 20, max_times = 5, jitter = TRUE)) (x)
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

get_author_link <- function(page) {
  rvest::html_nodes(page, ".post-user-username") %>%
    rvest::html_attr("href")
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

add_author_name <- function(output_tbl, pages){

  output_tbl %>%
    dplyr::left_join(tibble::tibble(
                       author_name = output_tbl %>%
                         dplyr::filter(!is.na(.data[["author_name"]])) %>%
                         dplyr::pull(.data[["author_name"]]),
                       date = output_tbl %>%
                         dplyr::filter(!is.na(.data[["author_name"]])) %>%
                         dplyr::pull(.data[["date"]]),
                       time = output_tbl %>%
                         dplyr::filter(!is.na(.data[["author_name"]])) %>%
                         dplyr::pull(.data[["time"]]),
                       author_link = purrr::map(pages, get_author_link) %>%
                                                  unlist()
                       ),
                       by = c("author_name", "date", "time")
    ) %>%
    dplyr::select(.data[["url"]]:.data[["author_name"]],
                  .data[["author_link"]],
                  .data[["quoted_user"]],
                  .data[["posting"]]:.data[["posting_wo_quote"]]
                  ) %>%
    dplyr::distinct(.data[["author_name"]],
                    .data[["author_link"]],
                    .data[["time"]],
                    .data[["posting"]],
                    .data[["posting_wo_quote"]],
                    .keep_all = TRUE)
}

clean_quoted_user <- function(posting, author_name){
  author_tbl <- tibble::tibble(
    true_name = author_name,
    cleaned_name = author_name %>%
    stringr::str_replace_all("[^[:alnum:]]", " ") %>%
    stringr::str_squish() %>%
    stringr::str_to_lower()
  ) %>%
    dplyr::distinct(cleaned_name, .keep_all = TRUE) %>%
    dplyr::filter(stringr::str_detect(cleaned_name, "[:alnum:]"))
  result <- stringr::str_locate(posting, pattern = paste(author_tbl$cleaned_name, collapse = "|"))
  temp <- stringr::str_sub(posting, start = result[, 1], end = result[, 2]) %>%
    tibble::enframe(name = NULL, value = "cleaned_name")
  dplyr::left_join(temp, author_tbl, by = "cleaned_name") %>%
    dplyr::pull("true_name")
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
  for_removal <- purrr::map2(posting, for_extract, ~{
    stringr::str_sub(.x, start = .y[,1], end = .y[, 2])
    })
  remove_it <- purrr::map2(posting, for_removal, ~{
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

  if (is.null(file_name) == TRUE) {
    file_name <- paste0("scrape-",
                        as.character(lubridate::today()),
                        as.character(sample(x = 1000000, size = 1)))
  }
  fs::dir_create(folder_name)
  readr::write_csv(x = output_tbl,
                   file = paste0(folder_name, "/", file_name, ".csv"))
}

### scraping large threads

scrape_large_thread <- function(suffix, urls = url_vec, export_csv, folder_name, file_name, delay){

  chunks <- split(urls %>% rev(), ceiling(seq_along(urls)/500))
  chunk_names <- paste0(stringr::str_remove(suffix, "/"), "_", seq_along(chunks), ".csv")

  purrr::walk2(chunks, chunk_names, ~{
    pages <- vector(mode = "list", length = length(.x))
    if (delay == TRUE) {
      for (i in seq_along(.x)){
        Sys.sleep(5)
        pages[[i]] <- insist_scrape_page(.x[[i]])
      }
    }

    if (delay == FALSE) {
      for (i in seq_along(.x)){
        pages[[i]] <- insist_scrape_page(.x[[i]])
      }
    }

    tibble::tibble(
      url = suffix,
      date = lubridate::ymd(purrr::map(pages, get_date_thread) %>% unlist() %>% .[!is.na(.)]),
      time = purrr::map(pages, get_time) %>% unlist(),
      author_name = purrr::map(pages, get_author_name) %>% unlist(),
      quoted_user = purrr::map(pages, get_quoted_user) %>% unlist() %>% .[. != "message from moderator"]
    ) %>%
      dplyr::bind_cols(purrr::map_dfr(pages, get_content_remove_quotes)) %>%
      add_author_name(., pages) %>%
      dplyr::mutate(posting_wo_quote = dplyr::case_when(posting_wo_quote == "" ~ posting,
                                                        TRUE ~ posting_wo_quote)) %>%
      dplyr::mutate(quoted_user = clean_quoted_user(posting, author_name),
                    quoted_user = dplyr::case_when(posting == posting_wo_quote ~ NA_character_,
                                                   TRUE ~ quoted_user),
                    author_name = dplyr::case_when(!stringr::str_detect(author_name, "[:alnum:]") ~ NA_character_,
                                                   TRUE ~ author_name,
                                                   TRUE ~ author_link)) %>%
    readr::write_csv(.y)
  })

  gc()

  output_tbl <- purrr::map_dfr(chunk_names, readr::read_csv,
                               col_types = readr::cols(
                                 url = readr::col_character(),
                                 date = readr::col_date(format = ""),
                                 time = readr::col_time(format = ""),
                                 author_name = readr::col_character(),
                                 author_link = readr::col_character(),
                                 quoted_user = readr::col_character(),
                                 posting = readr::col_character(),
                                 posting_wo_quote = readr::col_character()
                               )
  ) %>%
    dplyr::arrange(date, time)

  fs::file_delete(chunk_names)

  if (export_csv == TRUE) save_it(folder_name, file_name, output_tbl)
  if (export_csv == FALSE & is.null(folder_name) == FALSE | is.null(file_name) == FALSE) {
    save_it(folder_name, file_name, output_tbl)
  }
  output_tbl
}
