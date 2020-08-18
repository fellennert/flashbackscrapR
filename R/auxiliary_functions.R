# AUXILIARY FUNCTIONS -----------------------------------------------------


# FOR get_links AND get_title ---------------------------------------------

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

get_posting <- function(page) {
  rvest::html_nodes(page, ".post_message") %>%
    rvest::html_text() %>%
    stringr::str_remove_all("\n") %>%
    stringr::str_remove_all("\r") %>%
    stringr::str_remove_all("\t") %>%
    stringr::str_replace_all("[^[:alnum:]]", " ") %>%
    stringr::str_squish()
}

remove_second_order <- function(quotes, second_order_quote, third_order_quote) {
  if (length(third_order_quote) > 0) {
    pattern_third_order <- paste0("^", third_order_quote) %>%
      paste(., collapse = "|")
    second_order_quote <- second_order_quote[!stringr::str_detect(second_order_quote, pattern_third_order)]
  }
  if (length(third_order_quote) > 0) quotes <- remove_third_order(quotes, third_order_quote)
  temp_pattern <- paste(second_order_quote, collapse = "|")
  temp <- stringr::str_detect(quotes, temp_pattern)
  ind <- vector(mode = "logical", length = length(temp))
  ind[[1]] <- FALSE
  for (i in 2:length(temp)) {
    if (temp[[i-1]] == TRUE & temp[[i]] == TRUE) {
      ind[[i]] <- TRUE
      temp[[i]] <- FALSE
    }
  }
  quotes <- quotes[!ind]
  return(quotes)
}

remove_third_order <- function(quotes, third_order_quote) {
  temp_pattern <- paste(third_order_quote, collapse = "|")
  temp <- stringr::str_detect(quotes, temp_pattern)
  ind <- vector(mode = "logical", length = length(temp))
  ind[[1]] <- FALSE
  ind[[2]] <- FALSE
  for (i in 3:length(temp)) {
    if (temp[[i-2]] == TRUE & temp[[i-1]] == TRUE & temp[[i]] == TRUE) ind[[i]] <- TRUE
  }
  return(quotes[!ind])
}

get_quotes <- function(page) {
  quotes <- rvest::html_nodes(page, xpath = "//div[contains(@class, 'post-bbcode-quote')]") %>%
    rvest::html_text() %>%
    stringr::str_remove_all("\n") %>%
    stringr::str_remove_all("\r") %>%
    stringr::str_remove_all("\t") %>%
    stringr::str_replace_all("[^[:alnum:]]", " ") %>%
    stringr::str_squish()
  quotes <- quotes[stringr::str_detect(quotes, "^Citat")]
  second_order_quote <- rvest::html_nodes(page, ".post-clamped-text div") %>%
    rvest::html_text()%>%
    stringr::str_remove_all("\n") %>%
    stringr::str_remove_all("\r") %>%
    stringr::str_remove_all("\t") %>%
    stringr::str_replace_all("[^[:alnum:]]", " ") %>%
    stringr::str_squish() %>%
    .[stringr::str_detect(., "^Citat")]

  third_order_quote <- rvest::html_nodes(page, ".post-clamped-text .post-bbcode-quote .post-bbcode-quote") %>%
    rvest::html_text()%>%
    stringr::str_remove_all("\n") %>%
    stringr::str_remove_all("\r") %>%
    stringr::str_remove_all("\t") %>%
    stringr::str_replace_all("[^[:alnum:]]", " ") %>%
    stringr::str_squish() %>%
    paste("Citat", ., sep = " ") %>%
    .[. != "Citat "]

  if (length(second_order_quote) != 0) quotes <- remove_second_order(quotes, second_order_quote, third_order_quote)

  return(quotes)
}

## alternative for quote removal

alternative_get_quotes <- function(page) {
  quotes <- rvest::html_nodes(page, xpath = "//div[contains(@class, 'post-bbcode-quote')]") %>%
    rvest::html_text() %>%
    stringr::str_remove_all("\n") %>%
    stringr::str_remove_all("\r") %>%
    stringr::str_remove_all("\t") %>%
    stringr::str_replace_all("[^[:alnum:]]", " ") %>%
    stringr::str_squish()
  quotes <- quotes[stringr::str_detect(quotes, "^Citat")]

  for_removal <- rvest::html_nodes(page, ".post-bbcode-quote .post-bbcode-quote") %>%
    rvest::html_text() %>%
    stringr::str_remove_all("\n") %>%
    stringr::str_remove_all("\r") %>%
    stringr::str_remove_all("\t") %>%
    stringr::str_replace_all("[^[:alnum:]]", " ") %>%
    stringr::str_squish() %>%
    paste("Citat", ., sep = " ")

  quotes[!quotes %in% for_removal]
}

## auxiliary functions for removing quotes

# one quote, not in the beginning of posting
remove_quote_not_beginning <- function(value, quote) {
  if (length(value) == 0) return(value)
  temp <- stringr::str_split(value, quote) %>% purrr::map(stringr::str_squish)
  purrr::map_chr(temp, paste, collapse = " ")
}


# multiple quotes, one in beginning of posting
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
    stringr::str_squish() %>%
    paste(collapse = "")
  tibble::tibble(
    name = name,
    posting = value[[1]],
    posting_wo_quote = temp
  )
}


# multiple quotes, none in beginning of posting
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
    temp <- stringr::str_split(temp, quote[[i]]) %>%
      purrr::map_chr(paste, collapse = " ")
  }
  tibble::tibble(
    name = name,
    posting = value[[1]],
    posting_wo_quote = temp %>%
      stringr::str_squish()
  )
}


# function that obtains postings and removes quotes

get_content_remove_quotes <- function(page, n_pages) {
  quotes <- get_quotes(page)
  pattern <- paste(quotes, collapse = "|")

  posting <- get_posting(page) %>%
    tibble::enframe() %>%
    dplyr::mutate(reps = stringr::str_count(value, pattern))

  if (length(quotes) == 0) return(posting %>%
                                       dplyr::select(name,
                                                     posting = value,
                                                     posting_wo_quote = value)
                                  )

  if (n_pages == 1 && posting$reps[[1]] != 0) posting$reps[[1]] <- 0

  no_quote <- posting %>%
    dplyr::filter(reps == 0) %>%
    dplyr::select(name,
                  posting = value,
                  posting_wo_quote = value
                  )

  post_for_quote <- posting %>%
    dplyr::filter(reps != 0) %>%
    dplyr::group_by(name, value) %>%
    tidyr::expand(rep_number = seq(1:reps)) %>%
    dplyr::mutate(reps = max(rep_number))

  post_with_quote <- tryCatch(
    expr = {
      dplyr::bind_cols(post_for_quote, tibble::enframe(quotes, name = NULL, value = "quote"))
    },
    error = function(e) {
      quotes_new <- alternative_get_quotes(page)
      dplyr::bind_cols(post_for_quote, tibble::enframe(quotes_new, name = NULL, value = "quote"))
    }
  )


  quote_in_beginning <- post_with_quote %>%
    dplyr::filter(reps == 1 &
                    stringr::str_detect(value, "^Citat")) %>%
    dplyr::mutate(no_quote = stringr::str_split_fixed(string = value, pattern = quote, n = 2) %>%
                    purrr::pluck(2) %>%
                    stringr::str_squish()) %>%
    dplyr::select(name, posting = value, posting_wo_quote = no_quote)

  quote_not_in_beginning <- post_with_quote %>%
    dplyr::filter(reps == 1 &
                    !stringr::str_detect(value, "^Citat")) %>%
    dplyr::mutate(no_quote = remove_quote_not_beginning(value, quote)) %>%
    dplyr::select(name, posting = value, posting_wo_quote = no_quote)

  multiple_quotes_beginning_and_between <- post_with_quote %>%
    dplyr::filter(stringr::str_detect(value, "^Citat") &
                    reps > 1) %>%
    dplyr::mutate(second_quote = dplyr::lead(quote) %>% tidyr::replace_na(" ")) %>%
    dplyr::group_by(name, .add = TRUE) %>%
    dplyr::group_split() %>%
    remove_quotes_beginning_between()

  if (ncol(multiple_quotes_beginning_and_between) != 0) {
    multiple_quotes_beginning_and_between <- dplyr::distinct(multiple_quotes_beginning_and_between,
                                                      name,
                                                      .keep_all = TRUE)
  }

  multiple_quotes_between <- post_with_quote %>%
    dplyr::filter(!stringr::str_detect(value, "^Citat") &
                    reps > 1) %>%
    dplyr::mutate(second_quote = dplyr::lead(quote) %>% tidyr::replace_na(" ")) %>%
    dplyr::group_by(name, .add = TRUE) %>%
    dplyr::group_split() %>%
    remove_quotes_between()

  if (ncol(multiple_quotes_between) != 0) {
    multiple_quotes_between <- dplyr::distinct(multiple_quotes_between,
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

### 4th part: save results

save_it <- function(folder_name, file_name, output_tbl) {
  date_chr <- as.character(lubridate::today())

  if (is.null(file_name) == TRUE) {
    file_name <- paste0("scrape-", as.character(lubridate::today()))
  }
  readr::write_csv(output_tbl, file.path(folder_name, paste0(file_name, ".csv")))
}
