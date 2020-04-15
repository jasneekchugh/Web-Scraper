"getJournal(year) is the entry function which takes year and gives journal of that year ONLY.
We have parsed all the years and store in Summary.csv
getJournalThenOn(year) is the entry function which takes year and gives journal from that year 
onwards till 2020 and saves in Summary.csv
"

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

parsedData <<- data.frame()

getPdfName <- function(url){
  "
  Gets the unique name of the pdf so as to save it in the laptop.
  Parameter: Pdf url
  Returns: Unique name of the pdf
  "
  
  x <- strsplit(url,"/")
  name.txt <- x[[1]][8] 
  name.txt
}


getPdf <- function(url){
  "
  Scrapes the pdf from the given url.
  Parameter: Url of the pdf
  Returns: Full text of the pdf
  "
  if(is.na(url)){
    return(NA)
  }
  
  text <- pdf_text(url)
  name = getPdfName(url)
  text = str_replace_all(text, "\n", "")
  text = str_replace_all(text, "\t", " ")
  text = str_replace_all(text, "\r", "")
  # write(text, name) # uncomment this to save the pdf in laptop
  text
}


getAbstract<- function(text){
  "
  Extracts the abstract from the pdf text.
  Parameter: pdf full text
  Returns: Abstract of the paper
  "
  
  abs<-text[[1]][1]
  article_abstract=regexpr("Abstract(.*?)(Key)",abs)
  article_abstract = regmatches(abs, article_abstract)
  article_abstract = substr(article_abstract, 9, str_length(article_abstract)-4)
  article_abstract = str_replace_all(article_abstract, "\n", "")
  article_abstract= as.character(article_abstract)
  article_abstract = str_squish(str_trim(article_abstract))
}


countSubLinks <- function(year){
  "
  Count the published volumes in an year.
  Parameter: year
  Returns: Number of published volume in the year
  "
  
  n = 1
  if(year <= 2020 & year > 2008){
    n=1
  }
  else if (year %in% c(2008,2007,2006,2005)){
    n=3
  }
  else if (year %in% c(2004,2003,2002,2001)){
    n=4
  }
  n
}


getEmailAddress <- function(text){
  "
  Extracts the email address of the authors from the pdf.
  Parameter: pdf full text
  Returns: Email addresses of the Authors
  "
  
  emails = unlist(regmatches(text, gregexpr("([_a-z0-9-]+(\\.[_a-z0-9-]+)*@[a-z0-9-]+(\\.[a-z0-9-]+)*(\\.[a-z]{2,4}))", text)))
  emails = paste(emails, collapse = ",")
}


scrapePage <- function(url){
  "
  Scrapes the volume for given year. A year contain different volumes, this function works on
  individual volume to scrape. See the countSubLinks() which determines the published volume count
  in an year.
  Parameter: url of the volume to scrape
  Returns: None. It stores the parsed info in a global dataframe parsedData
  "
  
  page = read_html(url)
  article_lists <- html_nodes(page, htmlTags[["article-list"]])
  article_lists <- html_nodes(article_lists, "ul")
  for(i in seq_along(article_lists)){
    li = html_nodes(article_lists[[i]], "li")
    for(j in seq_along(li)){
      article_page = li[j]
      
      article_title = html_text(html_node(article_page, htmlTags[['article-title']]))
      article_title = paste(article_title, collapse = ",")
      
      article_author = html_text(html_node(article_page, htmlTags[['article-author']]))
      article_author = paste(article_author, collapse = ",")
      
      article_doi = html_text(html_node(article_page, htmlTags[['article-doi']]))
      article_doi = substr(article_doi, 4, str_length(article_doi))
      
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
      article_keywords = paste(article_keywords, collapse = ',')
      
      pdf_full_text = getPdf(ifelse(length(article_pdf_link) > 0, article_pdf_link,  NA)) # takes time so commeneted
      pdf_full_text = paste(pdf_full_text, collapse = ',')
      
      article_abstract = getAbstract(pdf_full_text)
      
      author_email = getEmailAddress(pdf_full_text)
      
      cat("\nArticle Title: ", article_title, "\n")
      cat("Author: ", article_author, "\n")
      cat("Correspondence's Email: ", author_email, "\n")
      cat("Article DOI: ", article_doi, "\n")
      cat("Article Pdf Link: ", article_pdf_link, "\n")
      cat("Additional Info: ", published, " ", released, "\n")
      cat("Keywords: ", article_keywords, "\n")
      cat("Abstract: ", article_abstract, "\n\n")
      
      parsedData <<- rbind(parsedData, data.frame("Article Title"=toString(article_title), 
                                                  "Authors"=toString(article_author), 
                                                  "Correspondence's Email"=toString(author_email),
                                                  "Article DOI"=toString(article_doi),
                                                  "Article Pdf Link"=toString(article_pdf_link),
                                                  "Published"=toString(sub("Published: |", "", published)), 
                                                  "Released"=toString(sub("Released: |", "", released)),
                                                  "Keywords"= toString(article_keywords),
                                                  "Abstract"=toString(article_abstract),
                                                  "Full-Text"=toString(pdf_full_text)
      ))
    }
  }
}


getJournal <- function(year, write=TRUE){
  "
  This is the main function which needs to be called to extract the issues of particular year
  Parameter: year in 2001-2020
  Returns: None. Save the entire scrape results in Summary.csv file. Summary.csv needs to be imported in 
          in Excel using import csv data method in Excel. Otherwise data will appeared scattered and
          misalligned if not imported as .csv file.
          Blank Excel Sheet-> Data-> From Text/Csv-> choose Summary.csv-> Load
  "
  
  if(year > 2020 | year < 2001){
    stop("The articles for the given year does not exist for our Journal. Please provide year between 2001-2020")
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
  
  if(write){
    cat("Writing all the information in Summary.csv \n")
    parsedData[parsedData == ""] <-NA
    write.csv(parsedData, "Summary.csv")
    cat("Completed writing all the information in Summary.csv \n")
  }
}

getJournalThenOn <- function(year){
  "
  This is the main function which needs to be called to extract the issues of particular year and the following
  years
  Parameter: year in 2001-2020
  Returns: None. Save the entire scrape results in Summary.csv file. Summary.csv needs to be imported in 
          in Excel using import csv data method in Excel. Otherwise data will appeared scattered and
          misalligned if not imported as .csv file.
          Blank Excel Sheet-> Data-> From Text/Csv-> choose Summary.csv-> Load
  "
  for(i in seq(year%%100, 20)){
    getJournal(2000+i, FALSE)
  }
  
  cat("Writing all the information in Summary.csv \n")
  parsedData[parsedData == ""] <-NA
  write.csv(parsedData, "Summary.csv")
  cat("Completed writing all the information in Summary.csv \n")
}

