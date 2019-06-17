#' Brings out the most common words from the top news search results
#' @param company_name name of the company
#' @export
#' @return list
#' @import tidytext
#' @import dplyr
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_col
#' @importFrom ggplot2 coord_flip
#' @examples
#' get_words("Debenhams")
#' get_words("Vodafone")



get_words <- function(company_name) {

  articles <- get_articles(company_name)
  text <- tibble(text=articles, article = 1:10)

  tidy_articles <- text %>%
    unnest_tokens(word, text)
  library(tidytext)
  data(stop_words)
  tidy_articles <- tidy_articles %>%
    anti_join(stop_words)

  p <- tidy_articles %>%
    count(word, sort = TRUE) %>%
    filter(n > 7) %>%
    mutate(word = reorder(word, n)) %>%
    ggplot(aes(word, n)) +
    geom_col(color="white", fill="royalblue") +
    ylab("number") +
    ggtitle("Number of times the word has appeared in the news") +
    coord_flip()
  return(p)
}
