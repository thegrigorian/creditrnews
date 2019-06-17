#' Gets the links of the top news search results for the company name
#' @param company_name name of the company
#' @export
#' @return character
#' @importFrom stringr str_replace_all
#' @importFrom xml2 read_html
#' @import dplyr
#' @importFrom rvest html_nodes
#' @importFrom rvest html_attr
#' @examples
#' get_links("Debenhams")
#' get_links("Vodafone")


get_links <- function(company_name) {
  company_name <- str_replace_all(company_name, " ", "+") #transform into search format
  company_name <- paste0(company_name, "&tbm=nws&source=lnt&tbs=sbd")  #add news component
  u <- paste0("http://www.google.co.uk/search?q=", company_name) #generate search link
  ht <- read_html(u) #get html
  links <- ht %>% html_nodes(xpath='//h3/a') %>% html_attr('href') #find the path
  links <- gsub('/url\\?q=','',sapply(strsplit(links[as.vector(grep('url',links))],split='&'),'[',1)) #get links
  return(links)
}
