library(rvest)
library(dplyr)
library(purrr)
rm(list=ls())

### Get the Archive Page of Journal of Pain Relief 
archive_link = "https://www.omicsonline.org/archive-pain-relief-open-access.php"
archive_page = read_html(archive_link)

volume_links = archive_page %>% html_nodes(".col-12.col-sm-4") %>% html_attr("href")
volume_names = archive_page %>% html_nodes(".col-12.col-sm-4") %>% html_text()

## Dataframe of Every Volume from 2023-2012 with Name and Link
volume_df = data.frame(volume_names, volume_links,stringsAsFactors = FALSE)


## Get Page of Vol12, Get All Author Names and Profiles
#vol12_link = "https://www.omicsonline.org/archive/jpar-volume-12-issue-10-year-2023.html"
#vol12_page = read_html(vol12_link)

#author_names = vol12_page %>% html_nodes("em a") %>% html_text()
#author_links = vol12_page %>% html_nodes("em a") %>% html_attr("href")
 
## Dataframe of all Authors in Vol12 and links to thier profiles
vol_author_df = data.frame(author_names,author_links, stringsAsFactors = FALSE)

### Go to each Author Page and get Name and Description:
#content_list <- list()

#page_contents <- vector("list", length(vol_author_df$author_links))

#for (i in seq_along(vol_author_df$author_links)) {
  
  #page_contents[[i]] <- read_html(vol_author_df$author_links[i])
  #name = page_contents[[i]] %>% html_nodes("h2") %>% html_text()
  #description = page_contents[[i]] %>% html_nodes(".col-md-6") %>% html_text()
  
  #content_list[[i]] = description
  
#}

#vol_author_df$Description = content_list

##### Try getting All Authors and Descriptions for All Volumes from Journ. of Pain Relief
test = map(volume_df$volume_links, read_html)

#urls = volume_df$volume_links
extract_authorprofiles1 <- function(urls) {
  map_df(urls, function(url) {
    page <- tryCatch(
      read_html(url),
      error = function(e) {
        warning(paste("Failed to read URL:", url, "Error:", e$message))
        return(data.frame(url = url, h2_text = NA_character_))
      }
    )
    
    author_name <- page %>% 
      html_nodes("em a") %>%
      html_text(trim = TRUE)
    
   author_link =  page %>%
      html_nodes("em a") %>% 
      html_attr("href")
    
    data.frame(name = author_name,profile_link =author_link, stringsAsFactors = FALSE)
  })
}

pain_names_df <- extract_authorprofiles(volume_df$volume_links)
write.csv(pain_names_df,'Desktop/PainJournal_Names.csv')
