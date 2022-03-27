
# Retrieve personal access token from GitHub to access malcon data automation
pat_key <- Sys.getenv("PAT_KEY")
pat_key <- "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"

# Set form id and url as characters
fid <- c(form1 = "hfc", form2 = "pi",
         form3 = "ppa", form4 = "ei",
         form5 = "fvs")

# Set province names for regions
## https://en.wikipedia.org/wiki/Regions_of_Papua_New_Guinea#:~:text=There%20are%20four%20regions%2C%20each,Ireland%2C%20and%20West%20New%20Britain.
highlands <- c("CHIMBU", "EASTERN HIGHLANDS", "ENGA", "HELA",
               "JIWAKA", "SOUTHERN HIGHLANDS", "WESTERN HIGHLANDS")
islands <- c("BOUGAINVILLE", "EAST NEW BRITAIN", "MANUS", "NEW IRELAND",
             "WEST NEW BRITAIN")
momase <- c("EAST SEPIK", "MADANG", "MOROBE", "SANDAUN")
southern <- c("CENTRAL", "GULF", "MILNE BAY", "NORTHERN", "WESTERN")


# Set current raw file url from repo
url_github <- "raw.githubusercontent.com/malcon-pngimr/malcon/main/data/"



# import data -------------------------------------------------------------

for (id in fid) {
    assign(id, read.csv(paste0("https://", pat_key, "@",
                               url_github, id, ".csv")))
}

# for (id in fid) {
#     write.csv(eval(parse(text = id)), paste0("data/", id, ".csv"))
# }
# 
# for (id in fid) {
#     assign(id, read.csv(paste0("data/2021/", id, ".csv")))
# }


rm(id, fid, url_github)
