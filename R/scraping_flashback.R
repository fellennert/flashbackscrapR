############################################
############ scraping flashback ############
############################################

library(rvest)
library(tibble)
library(purrr)
library(magrittr)
library(lubridate)



##########################################################
############ combine sub and sub-sub sections ############
##########################################################

create_sub_and_subsub_vec <- function(subsub_sections) {
  subs <- subsub_sections %>%
    distinct(sub_section) %>%
    rename(url = sub_section)
  subsubs <- sub_sub_sections %>%
    filter(sub_sub_section != "0") %>%
    select(url = sub_sub_section)
  return(bind_rows(subs, subsubs))
}

###########################################
############ pages of sections ############ --> rather: get links from sections!
###########################################

get_n_pages_sub_and_subsub <- function(section) {
  mainpage <- "https://www.flashback.org"
  link <- paste0(mainpage, section)
  page <- read_html(link)
  n_pages <- html_nodes(page, xpath = "//div[contains(@class, 'row row-forum-toolbar')]") %>%
    html_text()
  n_pages <- n_pages[2]
  n_pages <- str_split(n_pages, "av")
  n_pages <- n_pages[[1]]
  n_pages <- n_pages[2]
  n_pages <- as.numeric(str_sub(n_pages, 1, 8))
  return(tibble(
    suffix = section,
    n_pages = n_pages
  ))
}

##########################################
############ get thread links ############
##########################################

# (1.) generate links of sections

generate_links <- function(suffix, n_pages) {
  mainpage <- "https://www.flashback.org"
  n_pages <- 1:n_pages
  links <- character(length = length(n_pages))
  for (i in seq_along(n_pages)) {
    links[i] <- paste0(mainpage, suffix, "p", i)
  }
  return(links)
}

# (2.) get threads in section

get_links <- function(link) {
  require(rvest)
  page <- read_html(link)
  threads <- html_nodes(page,
                        xpath="//td[contains(@class, 'td_title')]") %>%
    html_nodes("a") %>%
    html_attr("href")
  threads <- threads[str_detect(threads, "/t")]
  threads <- str_split(threads, "l")
  threads <- map_chr(threads, 1)
  threads <- str_split(threads, "p")
  threads <- map_chr(threads, 1)
  threads <- str_split(threads, "s")
  threads <- map_chr(threads, 1)
  threads <- unique(threads)

  return(threads)
}

# (3.) combined function

get_thread_links <- function(suffix, n_pages) {
  pb$tick()$print()
  links <- generate_links(suffix = suffix, n_pages = n_pages)
  threads <- map(links, get_links) %>% unlist()
  return(threads)
}

#######################################
############ scrape thread ############
#######################################

# (1.) get number of pages

get_n_pages_thread <- function(thread_link) {
  mainpage <- "https://www.flashback.org"
  page <- read_html(paste0(mainpage, thread_link))
  n_pages <- html_nodes(page, xpath = "//div[contains(@class, 'row row-forum-toolbar')]") %>%
    html_text()
  n_pages <- n_pages[2]
  n_pages <- str_split(n_pages, "av")
  n_pages <- n_pages[[1]]
  n_pages <- n_pages[2]
  n_pages <- as.numeric(str_sub(n_pages, 1, 8))
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

get_date <- function(page) {
  today <- as.character(lubridate::today())
  yesterday <- as.character(lubridate::today()-1)
  date <- html_nodes(page, ".post-heading") %>%
    html_text() %>%
    str_remove_all("\n") %>%
    str_remove_all("\r") %>%
    str_remove_all("\t") %>%
    str_squish()
  date <- map_chr(str_split(date, ","), 1)
  for (k in 1:length(date)) {
    date[k] <- if_else(str_detect(date[k], "([2][0][0-2][0-9])[-]([0-1][0-9])[-]([0-3][0-9])"),
                       date[k],
                       if_else(str_detect(date[k], "Idag"),
                               today,
                               yesterday))
  }
  return(date)
}

# (3.2) time

get_time <- function(page) {
  return(html_nodes(page, ".post-heading") %>%
           html_text() %>%
           str_extract_all("([0-2][0-9])[:]([0-5][0-9])") %>%
           flatten_chr())
}

# (3.3) authors' names

get_author_name <- function(page) {
  return(html_nodes(page, ".post-user-username") %>%
           html_text() %>%
           str_remove_all("\n") %>%
           str_remove_all("\t"))
}

# (3.4) authors' urls

get_author_url <- function(link) {
  return(html_nodes(page, ".post-user-username") %>%
           html_attr("href"))
}

# (3.5) postings' text -- with citations

get_posting <- function(page) {
  return(html_nodes(page, ".post_message") %>%
           html_text() %>%
           str_remove_all("\n") %>%
           str_remove_all("\r") %>%
           str_remove_all("\t") %>%
           str_replace_all("[^[:alnum:]]", " ") %>%
           str_squish())
}

# (3.6) quotes

get_quote_pattern <- function(page) {
  quotes <- html_nodes(page, xpath = "//div[contains(@class, 'post-bbcode-quote')]") %>%
    html_text() %>%
    str_remove_all("\n") %>%
    str_remove_all("\r") %>%
    str_remove_all("\t") %>%
    str_replace_all("[^[:alnum:]]", " ") %>%
    str_squish()
  quotes <- quotes[str_detect(quotes, "^Citat")]
  return(paste(unique(str_sub(quotes, start = -(2/3*str_length(quotes)))), collapse = "|"))
}

# (3.7) cited users

get_quoted_user <- function(page) {
  text <- html_nodes(page, ".post_message") %>%
    html_text() %>%
    str_remove_all("\n") %>%
    str_remove_all("\r") %>%
    str_remove_all("\t") %>%
    str_replace_all("[^[:alnum:]]", " ") %>%
    str_squish()

  cit_user <- character(length = length(text))
  for (j in 1:length(text)){
    temp <- str_split(text[j], "postat av")[[1]][2]
    cit_user[j] <- word(str_trim(temp), 1)
  }
  return(cit_user)
}

# (4.) clean postings

remove_quotes <- function(posting, pattern) {
  posting_tbl <- enframe(posting)
  postings_wo_quotes <- posting_tbl %>%
    filter(!str_detect(value, "^Citat Ursprungligen postat av"))
  postings_w_quotes <- posting_tbl %>%
    filter(str_detect(value, "^Citat Ursprungligen postat av"))
  postings_w_quotes$text_wo_cit <- character(length = nrow(postings_w_quotes))

  for (i in 1:nrow(postings_w_quotes)) {
    if (length(str_split(postings_w_quotes$value[i], pattern = pattern, n = 2)[[1]]) == 2) {
      postings_w_quotes$text_wo_cit[i] <- str_split(postings_w_quotes$value[i], pattern = pattern, n = 2)[[1]][[2]]
    } else {
      postings_w_quotes$text_wo_cit[i] <- paste0("flawed citation", postings_w_quotes$value[i])
    }
  }

  output_tbl <- postings_w_quotes %>%
    select(name, value = text_wo_cit) %>%
    bind_rows(postings_wo_quotes) %>%
    arrange(name)
  return(output_tbl$value)
}

# (5.) write results

write_results <- function(output_tbl, folder_name, id) {
  write_csv(output_tbl, paste0(folder_name, "/output_tbl_", id, ".csv"))
}

# (6.) scraping function

scrape_thread_content <- function(id, thread_link, folder_name) {
  pb$tick()$print()
  n_pages <- get_n_pages_thread(thread_link = thread_link)
  url_vec <- generate_links(suffix = thread_link, n_pages = n_pages)
  pages <- map(url_vec, read_html)
  post_and_pattern_tbl <- tibble(
    posting = map(pages, get_posting) %>% unlist(),
    pattern = map(pages, get_quote_pattern) %>% reduce(paste(sep = "|"))
  )
  output_tbl <- tibble(
    url = thread_link,
    date = ymd(map(pages, get_date) %>% unlist()),
    time = map(pages, get_time) %>% unlist(),
    author_name = map(pages, get_author_name) %>% unlist(),
    posting = post_and_pattern_tbl$posting,
    posting_wo_quote = remove_quotes(post_and_pattern_tbl$posting, post_and_pattern_tbl$pattern),
    quoted_user = map(pages, get_quoted_user) %>% unlist()
  )
  write_results(output_tbl, folder_name, id)
  return(output_tbl)
  gc()
}

# (7.) try again
get_missed_ones <- function(scrape_output_list, url_vector) {
  output_transp <- scrape_output_list %>% transpose()
  success_tbl <- bind_rows(output_transp$result)
  failed <- output_transp$error %>% map_lgl(is_null)
  failed_links <- url_vector[!failed]

  pb <- progress_estimated(length(failed_links))
  new_scrape <- map(failed_links, safely(scrape_thread_content))

  return(list(success_tbl, new_scrape))
}
