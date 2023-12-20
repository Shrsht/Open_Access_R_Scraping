library(rvest)
library(dplyr)
library(purrr)

##- Pain Specialist, Rheumatology, Orthopedic Surgeon

##1,151-161,

##Rheumatology:


rheum_archive_page = "https://www.openaccessjournals.com/journals/clinical-rheumatology-archive.html"
page = read_html(rheum_archive_page)

archive_name = page%>% html_nodes(".col-xs-4 a") %>% html_text(trim = TRUE) 
archive_link = page%>% html_nodes(".col-xs-4 a") %>% html_attr("href") 

rheum_archives = data.frame(Archive = archive_name, Link = archive_link)


####Add appropriate http// prefixes:

add_archive <- function(url) {
  sub("archive/", "https://www.openaccessjournals.com/journals/archive/", url)
}

rheum_archives$Link = (sapply(rheum_archives$Link, add_archive))

rheum_names =  function(urls) {
    map_df(urls, function(url) {
      page <- tryCatch(
        read_html(url),
        error = function(e) {
          warning(paste("Failed to read URL:", url, "Error:", e$message))
          return(data.frame(url = url, h2_text = NA_character_))
        }
      )
      author_name = page %>% html_nodes(".textclass+ p") %>% html_text()
      author_paper = page %>% html_nodes(".textclass") %>% html_text()
    
      data.frame(Name = author_name, Paper = author_paper,stringsAsFactors = FALSE)

    })
}

rheum_names_df <- rheum_names(rheum_archives$Link)
write.csv(rheum_names_df,"Desktop/CORSAIRE DATA FILES/Rheumatology_Journal_Names.csv")  

ortho_archives = subset(all_omics_archives, (Journal == 'Journal of Pain & Relief' 
                                             |Journal == "Clinical Research on Foot & Ankle" ))
all_ortho_archives = merge(all_omics_archives,ortho)
all_ortho_archives = na.omit(test)
all_ortho_archives = subset(all_ortho_archives, select = -c(X,Link) )


ortho_names_df <- extract_authorprofiles(all_ortho_archives$Archive_Link,all_ortho_archives$Journal)
all_ortho_archives = all_ortho_archives %>% filter(!row_number() %in% c(40))

write.csv(ortho_names_df,"Desktop/CORSAIRE DATA FILES/Orthopedic Surgeon Names.csv")
write.csv(pain_names_df,"Desktop/CORSAIRE DATA FILES/Pain Specialist Names.csv")
