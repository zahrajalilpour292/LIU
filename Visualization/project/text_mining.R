
#https://rpubs.com/pjmurphy/265713
Needed <- c("tm", "SnowballCC", "RColorBrewer",  "wordcloud", "biclust", 
            "cluster", "igraph", "fpc")
install.packages(Needed, dependencies = TRUE)

install.packages("Rcampdf", repos = "http://datacube.wu.ac.at/", type = "source")

#loading text
cname <- file.path("C:", "text")   
cname 
summary(cname)
dir(cname)   
library(tm)
docs <- VCorpus(DirSource(cname))   
summary(docs) 
docs <- tm_map(docs,removePunctuation)   
for (j in seq(docs)) {
  docs[[j]] <- gsub("/", " ", docs[[j]])
  docs[[j]] <- gsub("@", " ", docs[[j]])
  docs[[j]] <- gsub("\\|", " ", docs[[j]])
  docs[[j]] <- gsub("\u2028", " ", docs[[j]])  # This is an ascii character that did not translate, so it had to be removed.
}
docs <- tm_map(docs, removeNumbers)   
docs <- tm_map(docs, tolower)   
docs <- tm_map(docs, PlainTextDocument)
DocsCopy <- docs
docs <- tm_map(docs, removeWords, stopwords("english"))   
docs <- tm_map(docs, PlainTextDocument)
docs <- tm_map(docs, removeWords, c("group","cost","costs","limited","related","may","market","capital","based","audit","december","information","rate","reporting","period","statements","committee","development","directors","companies","training","work","one","interest","date","current","amount","activities","fair","including" ,"company","business","assets","income","services","annual","tax","risk","cash","service","management","year","report","board","value","total","will","also","new","million","shares","share","performance","number","net","years","operating","per","system"))   

docs <- tm_map(docs, stripWhitespace)
docs <- tm_map(docs, PlainTextDocument)
dtm <- DocumentTermMatrix(docs)   

inspect(dtm[1:5, 1:20]) 
tdm <- TermDocumentMatrix(docs)   
tdm 

freq <- colSums(as.matrix(dtm))   
length(freq)
ord <- order(freq)   

dtms <- removeSparseTerms(dtm, 0.2) # This makes a matrix that is 20% empty space, maximum.   
dtms
freq <- colSums(as.matrix(dtm))
head(table(freq), 20)
tail(table(freq), 20)
freq <- colSums(as.matrix(dtms))   

freq
freq <- sort(colSums(as.matrix(dtms)), decreasing=TRUE)   
head(freq, 14) 
findFreqTerms(dtm, lowfreq=200)   # Change "50" to whatever is most appropriate for your text data.
wf <- data.frame(word=names(freq), freq=freq)   
head(wf)  
library(ggplot2)   
p <- ggplot(subset(wf, freq>4000), aes(x = reorder(word, -freq), y = freq))  +
  geom_bar(stat = "identity", fill="steelblue") + labs(title="Most frequent words")+xlab("Words")+ylab("frequency")+
  theme(axis.text.x=element_text(angle=45, hjust=1))

p   
findAssocs(tdm, c("welfare","customer" ), corlimit=0.75) # specifying a correlation limit of 0.85
list1<-findAssocs(dtm, "greenhouse", corlimit=0.84) # specifying a correlation limit of 0.95   
corrdf1 <- t(data.frame(t(sapply(list1,c))))
corrdf1
barplot(t(as.matrix(corrdf1)), beside=TRUE,xlab = "Words",ylab = "Corr",col = "steelblue",main = "Greenhouse",border = "black")

list2 <-findAssocs(dtm, c("safety" ), corlimit=0.7) # specifying a correlation limit of 0.85
corrdf2 <- t(data.frame(t(sapply(list2,c))))
corrdf2
barplot(t(as.matrix(corrdf2)), beside=TRUE,xlab = "Words",ylab = "Corr",col = "steelblue",main = "safety",border = "black")

list3 <-findAssocs(dtm, c("welfare" ), corlimit=0.6) # specifying a correlation limit of 0.85
corrdf3 <- t(data.frame(t(sapply(list3,c))))
corrdf3
barplot(t(as.matrix(corrdf3)), beside=TRUE,xlab = "Words",ylab = "Corr",col = "steelblue",main = "Welfare",border = "black")

findAssocs(dtm, c("quality","air" ), corlimit=0.85) # specifying a correlation limit of 0.85
corrdf4 <- t(data.frame(t(sapply(list4,c))))
corrdf4
barplot(t(as.matrix(corrdf4)), beside=TRUE,xlab = "Words",ylab = "Corr",col = "blue",main = "safety",border = "black")

library(wordcloud)
set.seed(142)   
wordcloud(names(freq), freq, min.freq=400) 
