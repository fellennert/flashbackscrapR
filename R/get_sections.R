#' Get main sections
#'
#' Returns the main sections' names and suffixes
#'
#' @param mainpage The flashback.org homepage; default value suffices.
#'
#' @return A tibble with two columns: 'name' and 'suffix'. The former indicates
#' the section's name, the latter its link's suffix.
#'
#' @examples
#' get_main_sections()
#'
#' @export
get_main_sections <-
  function (mainpage = "https://www.flashback.org") {

    page <- xml2::read_html(mainpage)

    main_section <-
      rvest::html_nodes(page, xpath="//div[starts-with(@class,'list-forum-title')]") %>%
      rvest::html_nodes("a") %>%
      rvest::html_text()
    main_section <- main_section[!stringr::str_detect(main_section, "^\\n")]

    main_urls <-
      rvest::html_nodes(page, xpath="//div[starts-with(@class,'list-forum-title')]") %>%
      rvest::html_nodes("a") %>%
      rvest::html_attr("href")
    main_urls <- main_urls[stringr::str_detect(main_urls, "\\/f")]

    return(tibble::tibble(
      name = main_section,
      suffix = main_urls
      )
    )
}

