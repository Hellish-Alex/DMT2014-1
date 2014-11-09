### Sorting words by frequency
sort_word <- keys[order(values, decreasing = TRUE)]
sort_word_freq <- sort(values, decreasing = TRUE)


### Sorted pairs
head(paste(sort_word, sort_word_freq, sep=" => "), length(last_statement))

# Barplot of the 8 most frequent words 
barplot(sort_word_freq[1:8], names.arg=sort_word[1:8])
dev.copy(png,'pictures/sort_word_freq.png',width = 1200, height = 800)
dev.off()

### Wordcloud of the most frequent words 
wordcloud(word, scale=c(5,0.5), max.words=100,
          random.order=FALSE, rot.per=0,
          use.r.layout=FALSE, colors=brewer.pal(6, "Paired"))
dev.copy(png,'pictures/wordcloud.png',width = 1200, height = 800)
dev.off()


### Pie char of Repentance
slices <- table(data_repent$repentance)
lbls <- c("Declined", "Unrepentanced", "Repentanced")
pie(slices, labels = lbls, main="Pie Chart of Repentance")
dev.copy(png,'pictures/Pie char of Repentance.png',width = 1200, height = 800)
dev.off()

### Persent of Repentance for each race
repentance_race <- tapply(data_repent[valid,"repentance"], data_repent[valid,"NULL.Race"], mean)
barplot(repentance_race, main="Persent of Repentance for each race", col = 1:4)
dev.copy(png,'pictures/Persent of Repentance for each race.png',width = 1200, height = 800)
dev.off()

### Total number of people for each race
barplot(table(data_repent[valid,"NULL.Race"]), main="Race", col = 1:4)
dev.copy(png,'pictures/Race.png',width = 1200, height = 800)
dev.off()

### Histogram of Ages
min_age <- min(as.numeric(levels(data_repent[valid,"NULL.Age"])))
ages <- as.numeric(data_repent[valid,"NULL.Age"]) + min_age - 1
hist(ages, main = "Histogram of Ages")
dev.copy(png,'pictures/Histogram of Ages.png',width = 1200, height = 800)
dev.off()

### Percent of Repentance by age
bData <- data.table(data_repent[, c(7, 11)])
bData$NULL.Age <- as.numeric(bData$NULL.Age) + min_age - 1
t <- aggregate(repentance ~ NULL.Age, bData, mean)
repent_age <- t$repentance
names(repent_age) <- t$NULL.Age
barplot(repent_age, main = "Percent of Repentance by age")
dev.copy(png,'pictures/Percent of Repentance by age.png',width = 1200, height = 800)
dev.off()

