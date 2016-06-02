require(stringr)
require(tm)
require(RWeka)

comm.df$text = gsub("(\\[)(id\\d*)(\\|)([A-Za-zА-Яа-я]*])", " ", comm.df$text) #removing references in comments
comm.df$text = gsub("(https://)(\\S*)", " ", comm.df$text) #remove URLs
comm.df$text = gsub("[^А-Яа-я]", " ", comm.df$text)

corp <- Corpus(VectorSource(comm.df$text), readerControl=list(language="ru", encoding="UTF-8"))
corp <- tm_map(corp, content_transformer(tolower))
corp <- tm_map(corp, removeWords, c(stopwords("ru"), "очень", "просто", "вообще", "хотя", "вроде"))
corp <- tm_map(corp, stripWhitespace)

dtm.control <- list(weighting=weightTf, stemming=FALSE, bounds=list(global=c(5,2000)))
dtm<-DocumentTermMatrix(corp, control=dtm.control)

#attempt at extracting ngrams
BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 4, max = 4))
dtm.bi <- DocumentTermMatrix(corp, control = list(tokenize = BigramTokenizer))
options(mc.cores=1) #you just gotta
