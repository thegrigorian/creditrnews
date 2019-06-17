#' Brings out the most common words from the top news search results with tf-idf
#' @param company_name name of the company
#' @export
#' @return list
#' @import tidytext
#' @import dplyr
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_col
#' @importFrom ggplot2 coord_flip
#' @examples
#' get_words_tfidf("Debenhams")
#' get_words_tfidf("Vodafone")



get_words_tfidf <- function(company_name){
  articles <- get_articles(company_name)
  text <- tibble(text=articles, article = 1:10)

  article_words <- text %>%
    unnest_tokens(word, text) %>%
    count(article, word, sort = TRUE)

  total_words <- article_words %>%
    group_by(article) %>%
    summarize(total = sum(n))

  words <- left_join(article_words, total_words)

  freq_by_rank <- words %>%
    group_by(article) %>%
    mutate(rank = row_number(),
           `term frequency` = n/total)

  words <- words %>%
    bind_tf_idf(word, article, n)

  p<- words %>%
    arrange(desc(tf_idf)) %>%
    mutate(word = factor(word, levels = rev(unique(word)))) %>%
    group_by(article) %>%
    top_n(5) %>%
    ungroup() %>%
    ggplot(aes(word, tf_idf, fill = article)) +
    geom_col(show.legend = FALSE) +
    labs(x = NULL, y = "tf-idf") +
    coord_flip()
  return(p)

}
