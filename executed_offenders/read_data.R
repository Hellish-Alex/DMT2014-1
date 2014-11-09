### Reading data

data <- readHTMLTable("http://www.tdcj.state.tx.us/death_row/dr_executed_offenders.html", straingsAsFactors=FALSE, header=TRUE)
data <- as.data.frame(data)


### Getting last statements into last_statement

statement <- "http://www.tdcj.state.tx.us/death_row/dr_info/"
# last_statement includs last statements for each person
last_statement<-vector()
num <- 1:(dim(data)[1])
#valid includs indexes of people whose data we obtained  
valid <- numeric(0)
for (i in num) { 
    last_url <- paste(statement, tolower(data$NULL.Last.Name[i]), 
                      tolower(data$NULL.First.Name[i]),"last", ".html", sep = "")
    
    a <- try(s <- htmlTreeParse(last_url, useInternal = TRUE), silent =TRUE)
    if(!inherits(a, "try-error")){
        hmldata <- xpathSApply(s,"//*[@id=\"body\"]", xmlValue)
        last_statement <- c(last_statement, hmldata)
        valid <- c(valid, i)
    }
}