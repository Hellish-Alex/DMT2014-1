require('XML')
require('data.table')
require('ggplot2')

parse_reports <- function(filename='raw data/reports.xml'){
  # Parses the reports.xml file 
  #
  # Args:
  #   filename: path to the xml file
  #
  # Returns:
  #   A dataframe containing report id, reporter, and opening time
  
  reportsxml <- xmlParse(filename)
  reportsdf <- xmlToDataFrame(reportsxml, stringsAsFactors = F)
  # get report id's from xml attributes
  ids <- xpathSApply(reportsxml, "//reports/report[@id]", xmlAttrs)
  # cast data to integers
  reportsdf$id <- as.integer(ids)
  reportsdf$reporter <- as.integer(reportsdf$reporter)
  reportsdf$opening_time <- as.integer(reportsdf$opening_time)
  return(reportsdf)
}

parse_other <-function(filename, data_names=c()){
  # Parses the xml file into the data.table
  #
  # Args:
  #   filename: path to the xml file
  #   data_names: names for the collumns in the resulting data.table
  #
  # Returns:
  #   A data.table
  xml <- xmlParse(filename)
  # parse the 'report' xml element, return a dataframe 
  parse_report <- function(xml){
    xdf <- xmlToDataFrame(xml, stringsAsFactors = F)
    xdf$id <- xmlAttrs(xml)
    return(xdf)
  }
  # merge the dataframes from all 'report' elemets into a data.table
  res = rbindlist(xpathApply(xml, '//*/report',parse_report), use.names=T)
  # if data_names was given rename collumns
  if(length(data_names)){
      names(res)[names(res)!='id'] <- data_names
  }
  return(res)
}

merge_files <- function(files, data_names){
  # A wrapper around parse_reports and parse_other for multiple files
  #
  # Args:
  #   files: a list of vector of filenames to parse
  #   data_names: a list of names for corresponding files
  #
  # Returns:
  #   A merged data.table for all files
  reports <- parse_reports()
  # parse files
  frames <- mapply(parse_other, files, data_names, SIMPLIFY = F)
  # merge results
  return(Reduce(
    function(x, y) merge(x,y,by='id', allow.cartesian=TRUE), 
    append(list(reports), frames)))
}

build_data <-function(){
  # An example, parses and merges a number of files into one data.table
  # and tidies the resulting data
  df <- merge_files(
    c('raw data/resolution.xml', 'raw data/assigned_to.xml'), 
    list(c('resolved_when', 'resolved_status'), 
         c('assigned_when', 'contributor')))
  df$id <- as.integer(df$id)
  #df$contributor <- as.factor(df$contributor)
  df$resolved_status <- as.factor(df$resolved_status)
  df$assigned_when <- as.integer(df$assigned_when)
  df$resolved_when <- as.integer(df$resolved_when)
  return(df)
}


contributors <- function(){
  # reads and has a peak at the number of bugs by contributor distribution

  # read the necessary data
  df <- merge_files(
    c('raw data/resolution.xml', 'raw data/assigned_to.xml'), 
    list(c('resolved_when', 'resolved_status'), 
         c('assigned_when', 'contributor')))
  df$id <- as.integer(df$id)
  df$contributor <- as.factor(df$contributor)
  df$resolved_status <- as.factor(df$resolved_status)
  df$assigned_when <- as.integer(df$assigned_when)
  df$resolved_when <- as.integer(df$resolved_when)
  # save data
  write.csv(df, "tidy data/contributors.csv", row.names=FALSE)
  dt <- data.table(df)
  # clean missing and dummy values
  dt <- dt[dt$contributor != 'nobody@mozilla.org' & dt$contributor != '']
  # group by contributors and sort
  contributors <- dt[,list(nbugs=length(unique(.SD$id))), by=contributor]
  setkey(contributors, nbugs)
  setorder(contributors,-nbugs)
  
  # print some insights
  print("Top 10 contributors")
  print(head(contributors, n=10))
  print("Number of resolved bugs summary statistics")
  print(summary(contributors$nbugs))
  # Pareto principle works for mozilla contributors! 
  round(contributors[1:ceiling(nrow(contributors)*0.2),
                     sum(nbugs)]/contributors[,sum(nbugs)]*100)
  png('tidy data/contribs.png')
  hist(contributors$nbugs, 
       xlab='Number of bugs', 
       ylab='Frequency', 
       main="Number of bug per contributor")
  dev.off()
}

op_sys_severity <- function(){
  # study bug distribution by operating system
  # read the necessary data and change variable formats
  df <- merge_files(
    c('raw data/op_sys.xml', 'raw data/severity.xml'),
    list(c('op_when', 'op_sys'),
         c('sev_when', 'severity')))
  df$id <- as.integer(df$id)
  df$severity <- as.factor(df$severity)
  df$op_sys <- as.factor(df$op_sys)
  df$op_when <- as.integer(df$op_when)
  df$sev_when <- as.integer(df$sev_when)
  # save data
  write.csv(df, "tidy data/op_sys_severity.csv", row.names=FALSE)
  # check missing values
  print('Check NA values:')
  summary(is.na(df$op_sys))
  summary(is.na(df$severity))
  # Converting to data.table to get the unique values of severity/operating system
  dt <- data.table(df)
  sever <- dt[,list(nbugs=length(unique(.SD$id))), by = severity]
  setkey(sever, nbugs)
  op_sys <- dt[,list(nbugs=length(unique(.SD$id))), by = op_sys]
  setkey(op_sys, nbugs)
  # basic summary
  print('Summary statistics for bugs severity and users\'s operating systems:')
  summary(sever)
  summary(op_sys)
  # Boxplot for bugs severity
  png('tidy data/severity.png')
  sev_plot <- ggplot(data = sever, 
                     aes(x = reorder(severity, nbugs), y = nbugs)) +
    geom_bar(stat = "identity") +
    theme_bw() +
    labs(list(title = "Distribution of bugs by severity",
              x = "Severity level", y = "Number of Bugs")) +
    theme(plot.title = element_text(size = 16))
  sev_plot
  dev.off()
}

###what was done in class
parse_class <- function(filename='raw data/resolution.xml'){
  resolution_xml <- xmlParse(filename)
  idNodes <- getNodeSet(resolution_xml, "//report[@id]")
  ids <- lapply(idNodes, function(x) xmlAttrs(x)['id'])
  val_when <- lapply(idNodes, xpathApply, path = './update/when', xmlValue)
  val_what <- lapply(idNodes, xpathApply, path = './update/what', xmlValue)
  resolutions <- data.frame(do.call(rbind.data.frame, mapply(cbind, ids, val_when, val_what)), stringsAsFactors = F)
  names(resolutions) <- c("report id", "update_when", "update_what")
  return(resolutions)
}


# -------------------------------------------------------- #
library(sqldf)
#setwd("C:/Programming_DM/DM Track Autumn 2014/DM1_Practice/DMLab1")

resolutions <- function(){
  # 1. Data
  reports <- parse_reports()
  #resolutions <- parse_other(filename = "raw data/resolution.xml")
  bug_statuses <- parse_other(filename = "raw data/bug_status.xml")
  
  names(reports) <- c("opening_time", "reporter", "report_id")
  reports$report_id <- as.integer(reports$report_id)
  reports$opening_time <- as.integer(reports$opening_time)
  reports$reporter <- as.integer(reports$reporter)
  
  names(bug_statuses) <- c("bug_status_update", "bug_status", "report_id")
  bug_statuses$report_id <- as.integer(bug_statuses$report_id)
  bug_statuses$bug_status <- as.factor(bug_statuses$bug_status)
  bug_statuses$bug_status_update <- as.integer(bug_statuses$bug_status_update)
  
  # 2. Select
  resolved_bugs <- sqldf("select max(bug_status_update) as resolved_time, bug_status, report_id from bug_statuses group by report_id having bug_status = 'RESOLVED'")
  
  # 3. Merge
  bug_reports <- merge(reports, resolved_bugs, by="report_id")
  
  # 4. Diff time
  bug_reports$resolving_time <- bug_reports$resolved_time - bug_reports$opening_time
  format_bug_reports$resolving_time <- format(as.POSIXct(bug_reports$resolving_time, "%H:%M:%OS %d.%m.%Y", origin = "0000-01-01"))
  
  bug_reports$resolving_time_weeks <- bug_reports$resolving_time / (60 * 60 * 24 * 7)
  
  # 5. Summary
  summary(bug_reports$resolving_time_weeks)
  
  # 6. Graphics
  library(ggplot2)
  ggplot(bug_reports, aes(x=resolving_time_weeks)) + geom_density()
  
  # 7. Top
  n <- 10
  
  attach(bug_reports)
  top_fast <- (bug_reports[order(+resolving_time), ])[1:n, ]
  top_slow <- (bug_reports[order(-resolving_time), ])[1:n, ]
  detach(bug_reports)
}