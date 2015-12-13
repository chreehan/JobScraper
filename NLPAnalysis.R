# most popular words by location, making a word cloud to show each place


library(topicmodels)
library('twitteR')
library('RTextTools')
library(tm)
library(wordcloud)
library(NLP)
library(ggplot2)
NLPset = JobData$text1
for (i in 1:length(JobData$text1)) {
  NLPset[i] = paste(NLPset[i], JobData$text2[i], JobData$keywords[i], sep = ' ')
}

EmptyElems = vector()
y = 0
for (i in 1:length(NLPset)) {
  
  x = NLPset[i]
  print(length(x))
  if (nchar(x) < 9) {
  y = y + 1
  EmptyElems[y] = i
  }
}
NLPset = NLPset[-c(EmptyElems)]
myPorpus <- Corpus(VectorSource(NLPset))
tdm <- TermDocumentMatrix(myPorpus)
findFreqTerms(tdm);

myCorpus <- Corpus(VectorSource(NLPset))
myDtm = DocumentTermMatrix(myCorpus, control = list(removePunctuation = TRUE,
                                                    stopwords = TRUE));

data("crude")
tdm <- TermDocumentMatrix(crude,
                          control = list(removePunctuation = TRUE,
                                         stopwords = TRUE))
dtm <- DocumentTermMatrix(crude,
                          control = list(weighting =
                                           function(x)
                                             weightTfIdf(x, normalize =
                                                           FALSE),
                                         stopwords = TRUE))


freq = colSums(as.matrix(myDtm))
orded = order(freq,decreasing = TRUE)
freq[orded][1:100]
#top interestubg words are python at 75, msc at 22,hadoop at 39
freq[orded][101:200] # hive at 32 matlab at 28, sql at 14+7, sas 12+7, bayesian 15,, c++ 10, 
freq[orded][201:300] # java at 19, mongodb at 8, spark 9
freq[orded][301:400] # tableau at 6, pig at 6, mahout, map/reduce, phd 25,
#find the probability a word is associated
#findAssocs(myDtm, 'principal', 0.5);

#findFreqTerms(myDtm, lowfreq=20)

library(wordcloud)
m <- as.matrix(myDtm)
# calculate the frequency of words
v <- sort(rowSums(t(m)), decreasing=TRUE)
myNames <- names(v)
d <- data.frame(word=myNames, freq=v)
wordcloud(d$word, d$freq, min.freq=3)


ggplot2
vizWords = data.table(Rate = c(75, 22,39,32,28,21,19,15,25,19,8,9,6,6,10),
  name = c('python', 'msc', 'hadoop', 'hive', 
'matlab','sql','sas', 'bayesian',
'phd', 'java', 'mongodb', 'spark',
'pig', 'tableau', 'c++'))

qplot(x=name,y=Rate, data = vizWords, geom = 'bar',stat = 'identity', fill = Rate)



