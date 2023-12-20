library(rvest)
library(dplyr)
library(purrr)
#rm(list=ls())

all_journals = read.csv("Desktop/CORSAIRE DATA FILES/all_journals.csv")
all_omics = read.csv("Desktop/CORSAIRE DATA FILES/omics_archives.csv")

all_omics$Link <- gsub("\\.php/", "-open-access\\.php/", all_omics$Link)

#url= sub("\\.php", "-open-access.php", url)
#message("Trying new URL: ", url)




#extract_archives <- function(urls, journal_names) {
  #map2_df(journal_names, urls, function(journal_name, url) {
      # Attempt to read the HTML from the URL
    #page <- tryCatch({
     #   read_html(url)
        # If read_html is successful, set success to TRUE
      #  return(page)
     # }, error = function(e) {
        #warning(paste("Failed to read URL:", url, "Error:", e$message))
       # # Modify the URL if there's an error
       # modified_url <- sub("\\.php", "-open-access.php",url)
       # print("new url:")
       # print(modified_url)
       # new_page = read_html(modified_url)
       # return(new_page)
     # })
    
   # if (!is.null(page) && inherits(page, "xml_document")) {
      # Extract the archive name and link
    #  archive_name <- page %>% html_nodes(".col-12.col-sm-4") %>% html_text(trim = TRUE)
    #  archive_link <- page %>% html_nodes(".col-12.col-sm-4") %>% html_attr("href")
      
      # Return the data frame with the extracted information
      #return(data.frame(Journal = journal_name, Archive_Name = archive_name, 
                        Archive_Link = archive_link, stringsAsFactors = FALSE))
    #} else {
      # If the page is still NULL after modifying the URL, return NA values
     # return(data.frame(Journal = journal_name, Archive_Name = NA_character_, 
                        Archive_Link = NA_character_, stringsAsFactors = FALSE))
   # }
  #}
#}



extract_archives <- function(urls, journal_names) {
  # Use map2_df to iterate over pairs of journal names and URLs
  map2_df(journal_names, urls, function(journal_name, url) {
    # Initialize page to NULL
    page <- NULL
    attempt <- 1
    
    # Keep trying until successful or a certain number of attempts
    while(is.null(page) && attempt <= 2) {
      page <- tryCatch({
        # Try to read the HTML
        read_html(url)
      }, error = function(e) {
        warning(paste("Failed to read URL on attempt", attempt, ":", url, "Error:", e$message))
        
        if(attempt == 1) {
          # Modify the URL if there's an error on the first attempt
          url <- sub("\\.php", "-open-access.php", url)
          print(paste("Trying new URL:", url))
        } else {
          # Return NA if the second attempt also fails
          return(NA)
        }
      })
      
      # Increment the attempt counter
      attempt <- attempt + 1
    }
    
    # If page is NA, return NA values
    if (is.na(page)) {
      return(data.frame(Journal = journal_name, Archive_Name = NA_character_, 
                        Archive_Link = NA_character_, stringsAsFactors = FALSE))
    }
    
    if (!is.null(page) && inherits(page, "xml_document")) {
      # Extract the archive name and link
      archive_name <- page %>% html_nodes(".col-12.col-sm-4") %>% html_text(trim = TRUE)
      archive_link <- page %>% html_nodes(".col-12.col-sm-4") %>% html_attr("href")
      
      # Return the data frame with the extracted information
      data.frame(Journal = journal_name, Archive_Name = archive_name, 
                 Archive_Link = archive_link, stringsAsFactors = FALSE)
    } else {
      # If the page is still NULL after the loop, return NA values
      data.frame(Journal = journal_name, Archive_Name = NA_character_, 
                 Archive_Link = NA_character_, stringsAsFactors = FALSE)
    }
  })
}


all_omics_archives = extract_archives(all_omics$Link,all_omics$Journal)
all_omics = all_omics %>%  filter(!row_number() %in% c(191))

all_omics$Link[1] = "https://www.omicsonline.org/archive-pain-relief-open-access.php"
#all_omics$Journal[157] = "International Journal of Clinical Rheumatology"

#RHEUMATOLOGY = "https://www.openaccessjournals.com/journals/clinical-rheumatology-archive.html"
#^^^^^^          = "https://www.omicsonline.org/archive-osteoarthritis.php"



#archives_with_na <- apply(all_omics_archives, 1, function(x) any(is.na(x)))
#na_df <- all_omics_archives[archives_with_na, ]


extract_authorprofiles <- function(urls,journal_names) {
  map2_df(urls,journal_names, function(url,journal_names) {
    page <- tryCatch(
      read_html(url),
      error = function(e) {
        warning(paste("Failed to read URL:", url, "Error:", e$message))
        return(NA)
      }
    )
    
    journal_name = journal_names
    
    author_name <- tryCatch( 
      page %>% html_nodes("em a") %>% html_text(trim = TRUE),
      error = function(e) {
        warning(paste("Failed to read URL:", url : "Error:", e$message))
        return("None Found")
      })
    
    author_link =  tryCatch(
      page %>%html_nodes("em a") %>% html_attr("href"),
      error = function(e) {
        print("Error Illide")
        warning(paste("Failed to read URL:", url : "Error:", e$message))
        return("None Found")
      }) 
    
    if(length(author_name) == 0) {
      author_name <- "None Found"
    }
    
    if(length(author_link) == 0) {
      author_link <- "None Found"
    }
    
    data.frame(journal = journal_names, name = author_name,profile_link =author_link, stringsAsFactors = FALSE)
  })
}

clean_omics_archives = na.omit(all_omics_archives)
clean_omics_archives %>%  filter(!row_number() %in% c(13))
all_names = extract_authorprofiles(clean_omics_archives$Archive_Link,clean_omics_archives$Journal)



testurl = "https://en.wikipedia.org/w/index.php?search=Ashoka+giri&title=Special%3ASearch&ns0=1"
testout = read_html(testurl) %>%html_nodes("em a") %>% html_attr("href")