


# Process - PPA -----------------------------------------------------------

ppa <- ppa %>% 
    mutate(hfs_000X = as.Date(hfs_000X), 
           hfs_014 = as.Date(hfs_014)) %>%
    ## remove three records for pilot hospital submissions 
    ## TODO: check if we piloted the survey in Maprik
    mutate(pilot = grepl("Maprik DH|KUNDIAWA HOSPITAL", hfs_003A) | 
               grepl("Wabag Hospital", hfs_003XA)) %>% 
    filter(!pilot) %>%
    select(-pilot)

ppa_total <- ppa %>% 
    summarise(n = n()) %>% 
    unlist(use.names = FALSE) 
