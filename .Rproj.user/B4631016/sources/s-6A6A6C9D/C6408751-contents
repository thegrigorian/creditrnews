#' Brings out the sentiment report for the most common words from the top news search results
#' @param company_name name of the company
#' @export
#' @return list
#' @import tidytext
#' @import dplyr
#' @importFrom wordcloud comparison.cloud
#' @importFrom reshape2 acast
#' @import lexicon
#' @examples
#' get_sentiment_report("Debenhams")
#' get_sentiment_report("Vodafone")


get_sentiment_report <- function(company_name){
  articles <- get_articles(company_name)
  text <- tibble(text=articles, article = 1:10)

  tidy_articles <- text %>%
    unnest_tokens(word, text)
  library(tidytext)
  data(stop_words)
  tidy_articles <- tidy_articles %>%
    anti_join(stop_words)

  word_count <- tidy_articles %>%
    group_by(article) %>%
    summarise(wordcount=n())
  library(lexicon)
  data(hash_sentiment_loughran_mcdonald)
  names(hash_sentiment_loughran_mcdonald)[1] <- "word"
  names(hash_sentiment_loughran_mcdonald)[2] <- "sentiment"
  sentiments <- hash_sentiment_loughran_mcdonald


  with_sentiment <- tidy_articles %>%
    inner_join(sentiments)

  with_sentiment$positive=with_sentiment$sentiment==1



  with_sentiment <- with_sentiment %>%
    group_by(article) %>%
    summarise(positive = sum(positive==TRUE), negative=-n()+positive, sentiment=positive+negative)

  with_sentiment <- left_join(with_sentiment, word_count)

  with_sentiment$sensitivity <- paste0(round(with_sentiment$sentiment/with_sentiment$wordcount*100,2), "%")


  return(with_sentiment)
}
