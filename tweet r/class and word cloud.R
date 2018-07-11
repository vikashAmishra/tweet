str(tweets)
tweets$Negative=as.factor(tweets$Avg<=-1)
table(tweets$Negative)
library("tm")
#tm stands for text mining. We use it to create corpus(set of text documents)
 library("SnowballC")
 #It helps tm function to operate
 corpus= VCorpus(VectorSource(tweets$Tweet))
 corpus[[1]]$content
 #VCorpus is used here for creating a volatile text documents and VectorSource is the source for character >
 
  corpus = tm_map(corpus, content_transformer(tolower))
 
  #here we used tolower function to make all the text in our data lowercase, tm_map is used for making > corpus[[1]]$content 
  corpus = tm_map(corpus, removePunctuation)
  corpus[[1]]$content  
  #Similarly here we used removePunctuation for removing any punctuations from our dataset as we are > corpus = tm_map(corpus, removeWords, c("apple", stopwords("english")))
  corpus = tm_map(corpus, removeWords, c("apple", stopwords("english")))
   corpus[[1]]$content  
   #Here we have removed the stopwords like I, have which have less or no meaning while evaluating the 
   corpus = tm_map(corpus, stemDocument)   
   corpus[[1]]$content
   inspect(frequencies[1000:1005,505:515])
   sparse = removeSparseTerms(frequencies, 0.995)   
   sparse   
   tweetsSparse = as.data.frame(as.matrix(sparse))
   colnames(tweetsSparse) = make.names(colnames(tweetsSparse))
   tweetsSparse$Negative = tweets$Negative  
   tweetsSparse$Negative = tweet$Negative   
   library(caTools)
  
   set.seed(123)
  
   split = sample.split(tweetsSparse$Negative, SplitRatio = 0.7)
  
   trainSparse = subset(tweetsSparse, split==TRUE)
   testSparse = subset(tweetsSparse, split==FALSE)
  
   #Here we have split our data in training and testing dataset where our training dataset contains 70
   #percent of our data   
   
   library(rpart)
   library(rpart.plot)
   tweetCART = rpart(Negative ~ ., data=trainSparse, method="class")   
   prp(tweetCART)   
   #Here the plot shows if the word 'freak' is present in our data then its a negative sentiment, whereas   
