#' Gets the artciles from the top news search results for the company name
#' @param company_name name of the company
#' @param n number of articles
#' @export
#' @return character
#' @importFrom stringr str_replace_all
#' @importFrom xml2 read_html
#' @import dplyr
#' @importFrom rvest html_nodes
#' @importFrom rvest html_text
#' @examples
#' get_articles("Debenhams")
#' get_articles("Vodafone")


get_articles <- function(company_name, n = 10){

  articles = {}
  links = get_links(company_name, n = n)
  for (i in (1:n)) {
    webpage <- read_html(links[i])
    text_html <- html_nodes(webpage, 'p')
    article <- toString(html_text(text_html))
    article <- str_replace_all(article, "\n", " ")
    article <- str_replace_all(article, "\t", "")
    article <- str_replace_all(article, "\r", "")
    article <- str_replace_all(article, "  ", " ")
    article <- str_replace_all(article, ".,", ".")
    article <- str_replace_all(article, "\"" , ".")
    articles[i] <- tibble(article)}
  return(articles)
}
