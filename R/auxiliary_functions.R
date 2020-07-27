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
  rvest::html_nodes(page, ".post-row:nth-child(1) .post-left") %>%
    rvest::html_text() %>%
    stringr::str_remove_all("\n") %>%
    stringr::str_remove_all("\t") %>%
    enframe() %>%
    tidyr::separate(value, sep = "fler inlägg av ", into = c("drop_it", "author")) %>%
    tidyr::separate(author, sep = "Hitta alla inlägg", into = c("author", "drop_it_2")) %>%
    dplyr::pull(author)
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

get_quotes <- function(page) {
  quotes <- rvest::html_nodes(page, xpath = "//div[contains(@class, 'post-bbcode-quote')]") %>%
    rvest::html_text() %>%
    stringr::str_remove_all("\n") %>%
    stringr::str_remove_all("\r") %>%
    stringr::str_remove_all("\t") %>%
    stringr::str_replace_all("[^[:alnum:]]", " ") %>%
    stringr::str_squish()
  quotes[stringr::str_detect(quotes, "^Citat")]
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

remove_quote_not_beginning <- function(value, quote) {
  temp <- stringr::str_split(value, quote) %>% map(stringr::str_squish)
  map_chr(temp, paste, collapse = " ")
}

remove_quotes_beginning_between <- function(multiple_quotes_beginning_and_between_list) {
  output_tbl <- vector(mode = "list", length = length(multiple_quotes_beginning_and_between_list))
    for (i in seq_along(multiple_quotes_beginning_and_between_list)) {
      temp_tbl <- multiple_quotes_beginning_and_between_list[[i]]
      output_tbl[[i]] <- remove_quotes_beginning_between_auxiliary(name = temp_tbl[[1]],
                                                                   value = temp_tbl[[2]],
                                                                   quote = temp_tbl[[5]],
                                                                   second_quote = temp_tbl[[6]])
    }
    dplyr::bind_rows(output_tbl)
}

remove_quotes_beginning_between_auxiliary <- function(name, value, quote, second_quote) {
  temp <- stringr::str_split(string = value, pattern = quote) %>%
    purrr::map(2) %>%
    unlist() %>%
    stringr::str_split_fixed(pattern = second_quote, n = 2)
  temp <- c(temp[1:(nrow(temp)-1), 1], temp[nrow(temp), 2]) %>%
    paste(collapse = '') %>%
    stringr::str_squish()

  tibble::tibble(
    name = name,
    posting = value[[1]],
    posting_wo_quote = temp
  )
}

remove_quotes_between <- function(multiple_quotes_between) {
  output_tbl <- vector(mode = "list", length = length(multiple_quotes_between))
    for (i in seq_along(multiple_quotes_between)) {
      temp_tbl <- multiple_quotes_between[[i]]
      output_tbl[[i]] <- remove_quotes_between_auxiliary(name = temp_tbl[[1]],
                                                         value = temp_tbl[[2]],
                                                         quote = temp_tbl[[5]]
                                                                   )
    }
    dplyr::bind_rows(output_tbl)
}

remove_quotes_between_auxiliary <- function(name, value, quote) {
  temp <- value[[1]]
  for (i in seq_along(quote)) {
    temp <- stringr::str_split(temp, quote[[i]]) %>% purrr::map_chr(paste, collapse = " ")
  }
  tibble::tibble(
    name = name,
    posting = value[[1]],
    posting_wo_quote = temp %>% stringr::str_squish()
  )
}

remove_quotes <- function(posting, pattern) {
  if (sum(stringr::str_length(pattern)) == 0) return(posting)
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

get_content_remove_quotes <- function(page) {

  quotes <- get_quotes(page)

  pattern <- paste(quotes, collapse = "|")

  posting <- get_posting(page) %>%
    tibble::enframe() %>%
    dplyr::mutate(reps = stringr::str_count(value, pattern))

  if (length(quotes) == 0) return(posting %>%
                                       dplyr::select(name, posting = value, posting_wo_quote = value))

  no_quote <- posting %>%
    dplyr::filter(reps == 0) %>%
    select(name, posting = value, posting_wo_quote = value)

  post_with_quote <- posting %>%
    dplyr::filter(reps != 0) %>%
    dplyr::group_by(name, value) %>%
    tidyr::expand(rep_number = seq(1:reps)) %>%
    mutate(reps = max(rep_number)) %>%
    dplyr::bind_cols(enframe(quotes, name = NULL, value = "quote"))

  quote_in_beginning <- post_with_quote %>%
    dplyr::filter(reps == 1 &
                    stringr::str_detect(value, "^Citat")) %>%
    dplyr::mutate(no_quote = str_split_fixed(string = value, pattern = quote, n = 2) %>%
                    purrr::pluck(2) %>%
                    stringr::str_squish()) %>%
    select(name, posting = value, posting_wo_quote = no_quote)

  quote_not_in_beginning <- post_with_quote %>%
    dplyr::filter(reps == 1 &
                    stringr::str_detect(value, "^Citat") == FALSE) %>%
    dplyr::mutate(no_quote = remove_quote_not_beginning(value, quote)) %>%
    select(name, posting = value, posting_wo_quote = no_quote)

  multiple_quotes_beginning_and_between <- post_with_quote %>%
    dplyr::filter(stringr::str_detect(value, "^Citat") &
                    reps > 1) %>%
    dplyr::mutate(second_quote = dplyr::lead(quote) %>% tidyr::replace_na(" ")) %>%
    group_by(name, .add = TRUE) %>%
    dplyr::group_split() %>%
    remove_quotes_beginning_between()

  if (ncol(multiple_quotes_beginning_and_between) != 0) {
    multiple_quotes_beginning_and_between <- distinct(multiple_quotes_beginning_and_between,
                                                      name,
                                                      .keep_all = TRUE)
  }

  multiple_quotes_between <- post_with_quote %>%
    dplyr::filter(!stringr::str_detect(value, "^Citat") &
                    reps > 1) %>%
    dplyr::mutate(second_quote = dplyr::lead(quote) %>% tidyr::replace_na(" ")) %>%
    group_by(name, .add = TRUE) %>%
    dplyr::group_split() %>%
    remove_quotes_between()

  if (ncol(multiple_quotes_between) != 0) {
    multiple_quotes_between <- distinct(multiple_quotes_between,
                                        name,
                                        .keep_all = TRUE)
  }

  dplyr::bind_rows(no_quote,
            quote_in_beginning,
            quote_not_in_beginning,
            multiple_quotes_beginning_and_between,
            multiple_quotes_between) %>%
    dplyr::arrange(name)
}

# (5.) save it
save_it <- function(folder_name, file_name, output_tbl) {
  date_chr <- as.character(lubridate::today())

  if (is.null(file_name) == TRUE) {
    file_name <- paste0("scrape-", as.character(lubridate::today()))
  }
  readr::write_csv(output_tbl, file.path(folder_name, paste0(file_name, ".csv")))
}
