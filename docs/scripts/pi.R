
# Process - PI ------------------------------------------------------------

pi <- pi %>% 
    mutate(hfs_000X = as.Date(hfs_000X), 
           hfs_014 = as.Date(hfs_014)) %>%
    ## remove three records for pilot hospital submissions 
    mutate(pilot = grepl("Maprik DH|KUNDIAWA HOSPITAL", hfs_003A) | 
               grepl("Wabag Hospital", hfs_003XA)) %>% 
    filter(!pilot) %>%
    select(-pilot) 

hw_total <- pi %>% 
    summarise(n = n()) %>% 
    unlist(use.names = FALSE) 