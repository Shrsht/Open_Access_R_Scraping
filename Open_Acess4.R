library(rvest)
library(purrr)
library(dplyr)
rm(list=ls())

# Get a DataFrame of every single Journal they have:
OA_page_link = "https://www.omicsonline.org/open-access.php"
OA_page_read = read_html(OA_page_link)
journal_subjects = OA_page_read %>% html_nodes(".col-md-6+ .col-md-6 .col-md-6 a")%>% html_text()
journal_subjects_links = OA_page_read %>% html_nodes(".col-md-6+ .col-md-6 .col-md-6 a")%>% html_attr("href")

subject_df = data.frame(journal_subjects,journal_subjects_links)

#########################################################################################

#### From each Subject get a DF of every journal they carry:

extract_all_journals <- function(urls, subject_name) {
  map2_df(urls,subject_name, function(url,subject_name) {
    page <- tryCatch(
      read_html(url),
      error = function(e) {
        warning(paste("Failed to read URL:", url, "Error:", e$message))
        return(data.frame(url = url, h2_text = NA_character_))
      }
    )
    
    subject = subject_name

    journal_name <- page %>% 
      html_nodes("td:nth-child(1) a:nth-child(1)") %>%
      html_text(trim = TRUE)
    
    journal_link =  page %>%
      html_nodes("td:nth-child(1) a:nth-child(1)") %>% 
      html_attr("href")
    
    data.frame(Journal = journal_name, Link = journal_link,Subject = subject_name,
               stringsAsFactors = FALSE)
  })
}

journs_masterdf1 = extract_all_journals(subject_df$journal_subjects_links, subject_df$journal_subjects)

###########################################################################################

## FROM ALL THE LINKS, GET THE ARCHIVES PAGE AND THEN GET THE AUTHOR NAMES FROM EACH ARCHIVE

ortho = subset(journs_masterdf1, (Subject == 'Orthopaedics'))[["Link"]]

domain_to_filter  <- "www.omicsonline.org"
filtered_df <- journs_masterdf1[grepl(paste0("https?://(www\\.)?", domain_to_filter ), journs_masterdf1$Link),]

####. Adding "-archive to each url in filtered.df":

filtered_df$Link <- gsub("https://www\\.omicsonline\\.org/", "https://www.omicsonline.org/archive-", filtered_df$Link)

write.csv(filtered_df,'Desktop/CORSAIRE DATA FILES/omics_archives.csv')
