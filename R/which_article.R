#' Brings out the must read articles from the top news search results
#' @param company_name name of the company
#' @export
#' @return character
#' @import tidytext
#' @import dplyr
#' @import lexicon
#' @examples
#' which_article("Debenhams")
#' which_article("Vodafone")



which_article <- function(company_name){
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



  with_sentiment <- tidy_articles %>%
    inner_join(sentiments)

  with_sentiment$positive=with_sentiment$sentiment==1



  with_sentiment <- with_sentiment %>%
    group_by(article) %>%
    summarise(positive = sum(positive==TRUE), negative=-n()+positive, sentiment=positive-negative)



  with_sentiment$warning <- with_sentiment$negative<mean(with_sentiment$negative)
  links<-get_links(company_name)
  return(links[with_sentiment$warning==TRUE])

}
