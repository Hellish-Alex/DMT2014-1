wd <- getwd();
wcdir <- "wcloud"
dir.create(file.path(wd, wcdir))
wcdirName <- paste(wd, "/", wcdir, sep="")

### Counting words frequency

counts <- list()
last_st_vec <- last_statement
for (i in 1:length(last_st_vec)) {
    last_st <- last_st_vec[[i]]
    ## Filtering words
    words <- strsplit(last_st, " ")
    words <- words[[1]]
    words <- tolower(words)
    words <- removeWords(words, stopwords("english"))
    words <- stemDocument(words)
    words <- removePunctuation(words)
    words <- words[words != ""]
    words[words = "famili"] = "family"
    words[words = "sorri"] = "sorry"
    words <- words[-grep('\\n', words)]   
    write(words, paste(wcdirName, "/", i, "keys.txt", sep = ""))
    for (j in 1:length(words)) {
        key <- words[j]
        if (is.null(counts[[ key ]])) {
            counts[[ key ]] <- 0
        }
        counts[[ key ]] <- counts[[ key ]] + 1  
    }
}

word <- Corpus(DirSource("wcloud/"))
word <- tm_map(word, stripWhitespace)
word <- tm_map(word, removeWords,
               c("statement", "offend","offender",
                 "inform", "date", "execution", "last", "yall"))


### Rough side effect fix 
counts$"m" <- 0
counts$"yall" <- 0
counts$"family" <- counts$"family"+ counts$"famili"
counts$"famili" <- 0
counts$"sorry" <- counts$"sorry"+ counts$"sorri"
counts$"sorri" <- 0
counts$s <- 0
counts$y <- 0
counts$people <- counts$people + counts$peopl
counts$peopl <- 0


### Creating keys and values vectors
keys <- names(counts)
values <- vector()
for (n in keys) {
    values <- c(values, counts[[ n ]])
}


