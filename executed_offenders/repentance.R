### Detecting of repentance

# We conside that person had repented before death if at least on of
# repentance_words was in his last statement.
repentance_words <- c("sorry", "forgiv", "lord", "sorry", "god", 
                      "pray", "jesus", "christ", "allah", "apolog")

# This offender declined to make a last statement. 
declined_words <- c("declin")

# People who had repented before death
repentance_people <- numeric(0)
# People who declined to make a last statement 
declined_people <- numeric(0)
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
    words[words == "famili"] = "family"
    words[words == "sorri"] = "sorry"
    words <- words[-grep('\\n', words)]
    if(any(repentance_words %in% words)){
        repentance_people <- c(repentance_people, valid[i])
    }
    if(any(declined_words %in% words)){
        declined_people <- c(declined_people, valid[i])
    }
    declined_people
}

### Adding repentance feature
data_repent <- data
data_repent$repentance <- NA
data_repent[valid, "repentance"] <- 0
data_repent[repentance_people, "repentance"] <- 1
data_repent[declined_people, "repentance"] <- -1
