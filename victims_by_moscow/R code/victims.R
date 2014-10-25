# Solution to task is based on assumptions of rigid structure of document

# Get last statements from the link
GetStatement <- function(link){
  stateHTML <- htmlParse(link)
  divs <- getNodeSet(stateHTML, "//div")
  # get last paragraph with statements and data
  rawText <- divs[[8]]
  #split all data with "Last Statement" phrase and take last part
  lastStatement <- strsplit(xmlValue(rawText), "Last Statement") [[1]][3]
  return (lastStatement)
}

# Count entries of word in text

StrCount <- function(x, pattern, split){
  
  unlist(lapply(
    strsplit(as.character(x), split),
    function(z) na.omit(length(grep(pattern, z)))
  ))
  
}
# Count summary weight of last statement based on vocabulary with weights

GetWeight <- function (x, dict) {
  weight = 0
  counter = 0
  for (i in 1:nrow(dict)){
    counter = StrCount(x, dict$V1[i], ' |,|\\.|;|!|\\?')
    if (counter!=0) {
      weight = weight + counter * dict$V2[i]
    }
  }
  return (weight)
}


library("XML")
new.url <- "http://www.tdcj.state.tx.us/death_row/dr_executed_offenders.html"
tree <- htmlTreeParse(new.url)
root <- xmlRoot(tree)
executedData <- as.data.frame(readHTMLTable(new.url, header = TRUE))
links <- getHTMLLinks(new.url) # get all url from page
lastWordLinks <- links[which(grepl("last", links))] # extract links to last statements
executedData$NULL.Link.1 <- paste0("http://www.tdcj.state.tx.us/death_row/", lastWordLinks)
executedData$lastWord <- lapply(executedData$NULL.Link.1, GetStatement) #add column with last statement to list
dictionary <- read.table("tidy_data/Regret.txt") # table with weights
executedData$regrets <- lapply(executedData$lastWord, GetWeight, dictionary) #add column with weight of last statement to list
regretsAsArray <- simplify2array(executedData$regrets) # convert list to vector

executedData[which(regretsAsArray == max(regretsAsArray)), ] # most repentant
executedData[order(regretsAsArray, decreasing = T)[1:5], ] # top 5 most repentant
