library(stringr)
library(rvest)
library(xml2)
library(stringr)
library(pdftools)


webUrl = "https://www.jstage.jst.go.jp/browse/cbij/%s/%s/_contents/-char/en/"
htmlTags = list(
  "article-list" = "div#search-resultslist-wrap",
  "article-title" = "div.searchlist-title",
  "article-author" = "div.searchlist-authortags.customTooltip",
  "article-additional-info" = "div.searchlist-additional-info",
  "article-doi" = "div.searchlist-doi",
  "article-pdf" = "div.lft",
  "article-abstract" = "div.inner-content abstract",
  "article-keywords" = "div.global-tags"
)

getPdfName <- function(url){
  x <- strsplit(url,"/")
  name.txt <- x[[1]][8] 
  name.txt
}


getPdf <- function(url){
  text <- pdf_text(url)
  name = getPdfName(url)
  write(text, name)
  text
}

getAbstract<- function(url){
  
  abs<-text[[1]][1]
  article_abstract=regexpr("Abstract\n(.*?)[K|k]ey",abs)
  article_abstract = regmatches(abs, article_abstract)
  article_abstract = substr(article_abstract, 13, str_length(article_abstract)-5)
  article_abstract = str_replace_all(article_abstract, "\n", "")
  
}

countSubLinks <- function(year){
  # <TODO @Jasneek: parse the extra urls for the years 2001-2008 and return the count>
  #Status: DONE
  if(year <= 2020 & year > 2008){
    n=1
  }
  else if (year %in% c(2008,2007,2006,2005)){
    n=3
  }
  else if (year %in% c(2004,2003,2002,2001)){
    n=4
  }
  return(n)
}


getEmailAddress <- function(text){
  # <TODO @Divya: parse the pdf text and return email address of authors>
  email = NA
  email
}



scrapePage <- function(url){
  # <TODO @Kajal, status - DONE: parse the url page for information>
  # Title, Authors, Author Affiliations, 
  # Correspondence Author, Correspondence Author's Email
  # Publish Date, Abstract, Keywords, Full Paper (Text format)
  page = read_html(url)
  article_lists <- html_nodes(page, htmlTags[["article-list"]])
  article_lists <- html_nodes(article_lists, "ul")
  for(i in seq_along(article_lists)){
    li = html_nodes(article_lists[[i]], "li")
    for(j in seq_along(li)){
      article_page = li[j]
      article_title = html_text(html_node(article_page, htmlTags[['article-title']]))
      article_author = html_text(html_node(article_page, htmlTags[['article-author']]))
      article_doi = gsub("DOI","",html_text(html_node(article_page, htmlTags[['article-doi']]))) #removed "DOI" from article_doi
      
      article_pdf = html_nodes(article_page, htmlTags[['article-pdf']])
      article_pdf = html_nodes(article_pdf, "span")
      article_pdf = html_nodes(article_pdf, ".bluelink-style")
      article_pdf_link = html_attr(article_pdf, "href")
      
      article_additional_info = html_text(html_node(article_page, htmlTags[['article-additional-info']]))
      published = regexpr("Published: .* \n", article_additional_info)
      published = regmatches(article_additional_info, published)
      published = substr(published, 0, str_length(published) - 3)
      
      released = regexpr("Released: .*", article_additional_info)
      released = regmatches(article_additional_info, released)
      released = substr(released, 0, str_length(released) - 1)
      
      article_keywords = html_node(article_page, htmlTags[['article-keywords']])
      article_keywords = html_nodes(article_keywords, "span")
      article_keywords = html_text(article_keywords)
      
      pdf_full_text = NA #getPdf(article_pdf_link) takes time so commeneted
      
      #article_abstract = NA # html_text(html_node(page,".datapanel_[opend")) # <Todo Jasneek:> This is not working - html_text(html_node(article_page, htmlTags[['article-abstract']]))
      # Either fix above or parse abstract from pdf text returned by divya. Your call and task!
      
      author_email = getEmailAddress(pdf_full_text) # <Todo Divya:> Get email adress of authors from the pdf text. Use regexpr to get it.
      
      cat(j, "Article is ", article_title, "\n")
      cat("Author: ", article_author, "\n")
      cat("Author email: ", author_email, "\n")
      cat("Article DOI: ", article_doi, "\n")
      cat("Article pdf link: ", article_pdf_link, "\n")
      cat("Additional Info: ", published, " ", released, "\n")
      cat("Keywords: ", article_keywords, "\n")
      cat("Abstract: ", article_abstract, "\n\n")
    }
  }
}

getJournal <- function(year){
  if(year > 2020 | year < 2001){
    stop("The articles for the given year do not exist for our Journal. Please provide year between 2001-2020")
  }
  
  yearUrls = c()
  
  if(year > 2008){
    yearUrls[length(yearUrls) + 1] = sprintf(webUrl, year%%100, 0)
  }
  else{
    extraUrlLength = countSubLinks(year)
    for( i in seq(extraUrlLength)){
      yearUrls[length(yearUrls) + 1] = sprintf(webUrl, year%%100, i)
    }
  }
  
  for( i in seq_along(yearUrls)){
    scrapePage(yearUrls[i])
  }
}

