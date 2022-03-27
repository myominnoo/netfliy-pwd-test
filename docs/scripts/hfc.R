

# Process HFC -------------------------------------------------------------

hfc <- hfc %>% 
    mutate(hfs_000X = as.Date(hfs_000X), 
           hfs_014 = as.Date(hfs_014)) %>%
    ## remove three records for pilot hospital submissions 
    ## TODO: check if we piloted the survey in Maprik
    mutate(pilot = grepl("Maprik DH|KUNDIAWA HOSPITAL", hfs_003A) | 
               grepl("Wabag Hospital", hfs_003XA)) %>% 
    filter(!pilot) %>%
    mutate(hf_type = ifelse(hfs_004X == 1, "Aid Posts", "Health Centres"),
           hf_type = ifelse(hfs_004 %in% c("PH", "DH"), "Hospital", hf_type), 
           ## create region var
           region = NA, 
           region = ifelse(hfs_001A %in% highlands, "Highlands", region), 
           region = ifelse(hfs_001A %in% islands, "Islands", region), 
           region = ifelse(hfs_001A %in% momase, "Momase", region), 
           region = ifelse(hfs_001A %in% southern, "Southern", region), 
           region = ifelse(is.na(region), hfs_001A, region)) %>%
    select(-pilot) 

# table 1 -----------------------------------------------------------------

hf_num <- hfc %>%
    summarise(n = n()) %>% 
    unlist(use.names = FALSE)

t1_final <- hfc %>%
    group_by(region, hf_type) %>% 
    summarise(n = n(), .groups = 'drop') %>% 
    pivot_wider(names_from = region, values_from = n) %>% 
    mutate(across(everything(), ~ ifelse(is.na(.x), 0, .x))) %>% 
    rowwise() %>% 
    mutate(Total = sum(across(Highlands:Southern))) %>%
    ungroup() %>% 
    mutate(across(everything(), ~ as.character(.x))) %>% 
    add_row(hf_type = "Total", 
            Highlands = calc_pct(.$Highlands, hf_num), 
            Islands = calc_pct(.$Islands, hf_num), 
            Momase = calc_pct(.$Momase, hf_num), 
            NCD = calc_pct(.$NCD, hf_num), 
            Southern = calc_pct(.$Southern, hf_num), 
            Total = calc_pct(.$Total, hf_num)) %>% 
    mutate(year = "2021") %>% 
    select(year, hf_type, NCD, everything()) %>% 
    mutate(sn = c(2, 1, 3, 4)) %>% 
    arrange(sn) %>% 
    select(-sn) 




# Table 2 -----------------------------------------------------------------

t2 <- hfc %>% 
    select(region, hf_type, hfs_321B, hfs_322B, hfs_310, 
           ## variables for working microscopy 
           hfs_105A, hfs_323, hfs_324, hfs_325) %>% 
    mutate(across(everything(), ~ ifelse(is.na(.x), 0, .x))) %>% 
    rowwise() %>% 
    mutate(rdt = hfs_310, 
           microscopy = sum(hfs_321B, hfs_322B, hfs_105A, hfs_323, hfs_324, hfs_325), 
           microscopy = (microscopy > 0) * 1, 
           ind1 = sum(hfs_321B, hfs_322B, hfs_310),
           ind1 = (ind1 > 0) * 1) %>%
    mutate(across(c(rdt, microscopy, ind1), ~ ifelse(.x == 0, 2, .x))) 


t2_final <- t2 %>% 
    disaggregate(hf_type, rdt, microscopy, ind1) %>% 
    pivot_longer(cols = -hf_type,
                 names_to = "diag_test", 
                 values_to = "n") %>% 
    pivot_wider(names_from = hf_type, values_from = n) %>% 
    mutate(year = "2021", 
           diag_test = recode(diag_test, 
                              "ind1" = "RDT or microscopy", 
                              "rdt" = "RDT")) %>% 
    select(year, diag_test, `Health Centres`, everything()) 


## Trend figure 

t2_raw <- readxl::read_excel("data/old_data.xlsx", sheet = "hfc_table2")

## Calculate RDT percentage
rdt_pct <- prop.table(table(t2$rdt))[1]

t2_trend <- t2_raw %>% 
    filter(hf_type == "Overall") %>% 
    select(year, diag_test, n) %>%
    pivot_wider(names_from = year, values_from = n) %>% 
    bind_cols(`2021` = c(rdt_pct,
                         prop.table(table(t2$microscopy))[1],
                         prop.table(table(t2$ind1))[1]) * 100) %>% 
    pivot_longer(-diag_test, names_to = "year", values_to = "n")

rdt_p <- prop.test(rdt_pct * hf_num, hf_num, .675) # compare to 2016
rdt_pct <- sprintf("%.1f", rdt_pct * 100)



# Table 3 -----------------------------------------------------------------

t3 <- hfc %>% 
    select(hf_type, hfs_330A, hfs_331A, hfs_332A, hfs_333A) %>% 
    mutate(across(hfs_330A:hfs_333A, ~ (.x > 0) * 1)) %>% 
    rowwise() %>% 
    mutate(flag = sum(hfs_330A, hfs_331A, hfs_332A, hfs_333A), 
           any = ifelse(flag > 0, 1, 0), 
           all = ifelse(flag == 4, 1, 0)) %>% 
    select(hf_type, hfs_330A:hfs_333A, all, any) %>% 
    setNames(c("hf_type", "infant", "child", "young", "adult", "all", "any")) %>% 
    mutate(across(infant:any, ~ ifelse(.x == 0, 2, .x))) 

t3_final <- t3 %>% 
    disaggregate(hf_type, infant:any) %>% 
    pivot_longer(cols = -hf_type, names_to = "aldose", values_to = "n") %>% 
    pivot_wider(names_from = hf_type, values_from = n) %>% 
    mutate(year = "2021", 
           aldose = recode(aldose, 
                           "infant" = "Infant (5-15kg)", 
                           "child" = "Child (15-25kg)", 
                           "young" = "Youth (25-35kg)", 
                           "adult" = "Adult (35+kg)")) %>% 
    select(year, aldose, `Health Centres`, everything())


t3_trend <- c(0, 0, 4.5, 6.8, 51.1, 58.0, 45.0, 87.2, 34.0, 66.2, 
          prop.table(table(t3$all))[1] * 100,
          prop.table(table(t3$any))[1] * 100)

t3_trend <- matrix(t3_trend, ncol = 2, byrow = TRUE) %>% 
    as.data.frame() %>% 
    setNames(c("all dose", "any dose")) %>% 
    mutate(year = c(2010, 2011, 2012, 2014, 2016, 2021)) %>% 
    pivot_longer(-year, names_to = "dose", values_to = "n")



# Table 4 -----------------------------------------------------------------


staff <- c("md", "heo", "nurse", "chw", "cmm", "rla", "nonclinical", "hio", "pharmacist")
status <- c("employ", "present", "temploy", "tpresent")
status <- lapply(staff, paste, status, sep = ".") %>% do.call(c, .)
## non clinical staff has only two statuses: employ and present
status <- status[-c(27, 28)]

t4_final <- hfc %>% 
    select(hfs_100A:hfs_108D) %>% 
    setNames(status) %>%
    replace(is.na(.), 0) %>% 
    summarise(across(everything(), ~ sum(.x))) %>%
    pivot_longer(cols = everything(), names_to = "staff", values_to = "n") %>% 
    separate(staff, c("staff", "status"), extra = "drop", fill = "right") %>% 
    filter(status %in% c("employ", "temploy")) %>% 
    pivot_wider(names_from = status, values_from = n) %>% 
    replace(is.na(.), 0) %>% 
    filter(staff %in% c("md", "heo", "nurse", "chw", "rla")) %>%
    mutate(pct = temploy / employ * 100) %>% 
    add_row(staff = "Total", 
            employ = sum(.$employ), 
            temploy = sum(.$temploy), 
            pct = sum(.$temploy) / sum(.$employ) * 100) %>% 
    mutate(staff = recode(staff, "md" = "MD", "heo" = "HEO", 
                          "nurse" = "Nurse", "chw" = "CHW", 
                          "rla" = "RLA/MLA"), 
           pct = sprintf("%.1f", pct),
           temploy = paste0(temploy, " (", pct, ")"), 
           year = "2021") %>% 
    select(year, everything())

trained_pct <- t4_final %>% 
    filter(staff == "Total") %>% 
    select(pct) %>% 
    unlist(use.names = FALSE)


t4_trend <- readxl::read_excel("data/old_data.xlsx", sheet = "hfc_table4") %>% 
    filter(Position == "Total") %>% 
    select(Year, trained_pct) %>%
    add_row(Year = 2021, trained_pct = as.numeric(trained_pct)) 




# Table 5 -----------------------------------------------------------------

t5_final <- hfc %>% 
    select(hf_type, hfs_330A, hfs_331A, hfs_332A, hfs_333A, 
           ## pq,   dp,      AI - artemether artesunate injections
           hfs_353, hfs_349, hfs_342:hfs_347, 
           ## qi,   qt,      dox
           hfs_354, hfs_355, hfs_350) %>% 
    mutate(across(hfs_330A:hfs_333A, ~ (.x > 0) * 1)) %>% 
    rowwise() %>% 
    mutate(flag = sum(hfs_330A, hfs_331A, hfs_332A, hfs_333A), 
           any = ifelse(flag > 0, 1, 0), 
           al = ifelse(flag == 4, 1, 0), 
           pq = hfs_353,
           dp = hfs_349, 
           ai = sum(c_across(hfs_342:hfs_347))) %>% 
    mutate(across(al:ai, ~ ifelse(.x > 0, 1, 0)), 
           al_pq = al + pq, 
           al_pq = (al_pq == 2) * 1, 
           al_ai = sum(al, ai), 
           al_ai = (al_ai == 2) * 1, 
           al_ai_pq = sum(al_ai, pq), 
           al_ai_pq = (al_ai_pq == 2) * 1, 
           qi_qt_dx = sum(hfs_354, hfs_355, hfs_350), 
           qi_qt_dx = (qi_qt_dx == 3) * 1, 
    ) %>%
    select(hf_type, al, al_pq, dp, al_ai, al_ai_pq, qi_qt_dx) %>% 
    disaggregate(hf_type, al:qi_qt_dx) %>% 
    pivot_longer(cols = -hf_type, names_to = "medication", values_to = "n") %>% 
    pivot_wider(names_from = hf_type, values_from = n) %>% 
    mutate(year = "2021") %>% 
    select(year, medication, `Health Centres`, everything())
