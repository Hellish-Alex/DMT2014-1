library(XML)
url <- "http://www.nkcbank.ru/viewCatalog.do?menuKey=254"

# Find & throw away irrelevant tables
#tables <- readHTMLTable(url, stringsAsFactors=FALSE)
#for (i in 1:8) {print(nrow(tables[[i]]))}
#for (i in 1:8) {print(ncol(tables[[i]]))}
#tables <- tables[3:7]

table1 <- readHTMLTable(url, header=FALSE, stringsAsFactors=FALSE, which=3) #special case
colnames(table1) <- table1[1,]
colnames(table1)[4:8] <- table1[2, 1:5]
table1 <- table1[-c(1, 2),]
table1.colnames <- colnames(table1)
table1 <- as.data.frame(sapply(table1, gsub, pattern="\\s|%|-", replacement=""), stringsAsFactors=FALSE)
table1[,c(1, 4:8)] <- data.frame(lapply(table1[,c(1, 4:8)], as.numeric), stringsAsFactors=FALSE)
colnames(table1) <- table1.colnames

tables <- readHTMLTable(url, header=TRUE, stringsAsFactors=FALSE, which=4:7)

# Do the same for tables 2-5
# ...

# Concatenate all tables into one list
tables <- c(list(table1), tables)
