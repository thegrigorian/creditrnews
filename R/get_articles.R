#' Gets the artciles from the top news search results for the company name
#' @param company_name name of the company
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


get_articles <- function(company_name){

  articles={}
  links=get_links(company_name)
  for (i in (1:10)) {
    webpage <- read_html(links[i])
    text_html <- html_nodes(webpage,'p')
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
