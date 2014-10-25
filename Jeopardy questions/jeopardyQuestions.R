library(RCurl)
library(RJSONIO)

url <- "http://skeeto.s3.amazonaws.com/share/JEOPARDY_QUESTIONS1.json.gz"
file <- basename(url)
download.file(url, file)

library(R.utils)
gunzip("JEOPARDY_QUESTIONS1.json.gz")

library(RJSONIO)
jsondata1 <- fromJSON("JEOPARDY_QUESTIONS1.json")

json_file <- lapply(jsondata1, function(x) {
  x[sapply(x, is.null)] <- NA
  unlist(x)
})

# Make a data frame out of a JSON list
mat <- do.call("rbind", json_file)
data <- as.data.frame(mat)

# Prepare data
data$value <- as.numeric(sapply(data$value, gsub, pattern=",|\\$", replacement=""))


# Find the most 'expensive' question
data[which.max(data$value),c("question", "answer", "value")]
