## This is a script to extract Japanese contacts from File import
## Methodology
## 1. Creating data base from List of common Japanese surnames and familyeducation
## https://en.wikipedia.org/wiki/List_of_common_Japanese_surnames
## https://www.familyeducation.com/baby-names/browse-origin/surname/japanese
## 2. Matching the imported last name with data.base
## 3. Transfer the matched observation to new jp client only file


my_seg_ja <- apply(my.seg, 1, function(r) do.call(paste, as.list(r)))

test.seg <- do.call(rbind, lapply(1:nrow(jnames), 
                      function(i){
                        matchJP <- grepl(jnames$Surnames[i], my_seg_ja, ignore.case=T)
                        if(any(matchJP))
                          cbind(my.seg[i, ], my.seg[matchJP, ])
                      }))

## And before and after pattern for name list
## "\b" is an anchor to identify word before/after pattern
## "\<" is an escape sequence for the beginning of a word, and ">" is used for end
##e.g
grepl("\\bWord1\\b",c("Word1","Word2","Word12"))
## [1]  TRUE FALSE FALSE

my_begin <- replicate(2, "\\b")
my_end <- replicate(2, "\\b")
teststring <- replicate(2, "hellp")
my.textB <- list(my_begin = my_begin, teststring = teststring, my_end = my_end)

jnames <- read.csv("jnames.csv")
jnames_vec <- jnames[,2]
temp.data.frame <- data.frame(a = replicate(nrow(jnames), "\\b"), b = jnames_vec, c = replicate(nrow(jnames), "\\b"))
## combine columns with paste
temp.name <- apply(temp.data.frame, 1, function(r) do.call(paste, as.list(r)))
## use gsub to remove space " "
fixed.jnames <- gsub(" ", "", temp.name)

my_seg_ja <- apply(my.seg, 1, function(r) do.call(paste, as.list(r)))

test.seg <- do.call(rbind, lapply(1:length(fixed.jnames), 
                                  function(i){
                                    matchJP <- grepl(fixed.jnames[i], my.seg$Last.Name, ignore.case=T)
                                    if(any(matchJP))
                                      cbind(my.seg[i, ], my.seg[matchJP, ])
                                  }))


## export names based on jnames database                                     
test.seg <- do.call(rbind, lapply(1:length(fixed.jnames), 
                                 function(i){
                                   matchJP <- grepl(fixed.jnames[i], my.seg$Last.Name, ignore.case=T)
                                   if(any(matchJP))
                                     cbind(my.seg[matchJP, ])
                                 }))



setwd("C:/Users/tong.jin/Documents/R/japanese_surnames_project")
my.seg <- read.csv("subscribed_segment_export_52b89dcf5f.csv")
jnames_vec <- read.csv("jnames.csv", header = T, fileEncoding = "UTF-8")
temp.data.frame <- data.frame(a = replicate(nrow(jnames_vec), "\\b"), b = jnames_vec, c = replicate(nrow(jnames_vec), "\\b"))
## combine columns with paste
temp.name <- apply(temp.data.frame, 1, function(r) do.call(paste, as.list(r)))
## use gsub to remove space " "
fixed.jnames <- gsub(" ", "", temp.name)

test.seg <- do.call(rbind, lapply(1:length(fixed.jnames), 
                                  function(i){
                                    matchJP <- grepl(fixed.jnames[i], my.seg$Last.Name, ignore.case=T)
                                    if(any(matchJP))
                                      cbind(my.seg[matchJP, ])
                                  }))

write.csv(test.seg, "contacts_with_JA_surnames.csv")


