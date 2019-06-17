#' Brings out the sentiment report for the most common words from the top news search results
#' @param company_name name of the company
#' @export
#' @return list
#' @import tidytext
#' @importFrom utils data
#' @import dplyr
#' @import tidyr
#' @import ggplot2
#' @import lexicon
#' @import data.table
#' @examples
#' get_sentiment_plot("Debenhams")
#' get_sentiment_plot("Vodafone")



get_sentiment_plot <- function(company_name) {
  articles <- get_articles(company_name)
  text <- tibble(text=articles, article = 1:10)

  tidy_articles <- text %>%
    unnest_tokens(word, text)

  library(tidytext)
  data(stop_words)

  tidy_articles <- tidy_articles %>%
    anti_join(stop_words)

  arti <- tidy_articles %>%
    group_by(article) %>%
    summarise(count=n())

  arti <- arti %>%
    mutate(cumsum = cumsum(arti$count))

  tidy_articles$wordnumber <- as.integer(row.names(tidy_articles))

  new <- left_join(tidy_articles, arti)

  new$dif <- new$cumsum-new$count

  new$wordnumber=new$wordnumber-new$dif
  library(lexicon)
  data(hash_sentiment_loughran_mcdonald)
  names(hash_sentiment_loughran_mcdonald)[1] <- "word"
  names(hash_sentiment_loughran_mcdonald)[2] <- "sentiment"
  hash_sentiment_loughran_mcdonald$sentiment=ifelse(hash_sentiment_loughran_mcdonald$sentiment==1,"positive", "negative")
  sentiments <- hash_sentiment_loughran_mcdonald

  article_sentiment <- new %>%
    inner_join(sentiments) %>%
    count(article, index = wordnumber %/% 5, sentiment) %>%
    spread(sentiment, n, fill = 0) %>%
    mutate(sentiment = positive-negative)


  article_sentiment$color <- ifelse(article_sentiment$sentiment>0, "forestgreen", "firebrick1")

  p=ggplot(article_sentiment, aes(index, sentiment,fill=article_sentiment$color) )+
    geom_col(show.legend = FALSE)
  return(p)
}
