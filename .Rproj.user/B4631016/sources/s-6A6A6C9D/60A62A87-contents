#' Brings out the wordclouds for the most common words from the top news search results
#' @param company_name name of the company
#' @export
#' @return list
#' @import tidytext
#' @importFrom utils data
#' @import dplyr
#' @importFrom wordcloud comparison.cloud
#' @importFrom reshape2 acast
#' @import lexicon
#' @examples
#' get_wordcloud("Debenhams")
#' get_wordcloud("Vodafone")


get_wordcloud <- function(company_name){

  articles <- get_articles(company_name)
  text <- tibble(text=articles, article = 1:10)

  tidy_articles <- text %>%
    unnest_tokens(word, text)
  library(tidytext)

  data(stop_words)
  tidy_articles <- tidy_articles %>%
    anti_join(stop_words)
  library(lexicon)
  data(hash_sentiment_loughran_mcdonald)
  names(hash_sentiment_loughran_mcdonald)[1] <- "word"
  names(hash_sentiment_loughran_mcdonald)[2] <- "sentiment"
  hash_sentiment_loughran_mcdonald$sentiment=ifelse(hash_sentiment_loughran_mcdonald$sentiment==1,"positive", "negative")
  sentiments <- hash_sentiment_loughran_mcdonald


  p<- tidy_articles%>%
    inner_join(sentiments) %>%
    count(word, sentiment, sort = TRUE) %>%
    acast(word ~ sentiment, value.var = "n", fill = 0) %>%
    comparison.cloud(colors = c("darkred", "darkgreen"),
                     max.words = 200)
  return(p)

}

