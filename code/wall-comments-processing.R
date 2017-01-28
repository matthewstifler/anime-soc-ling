require(stringr)
require(tm)
require(RWeka)
require(slam)
require(wordcloud)
require(mallet)

comm.df$text = gsub("(\\[)(id\\d*)(\\|)([A-Za-zА-Яа-я]*])", " ", comm.df$text) #removing references in comments
comm.df$text = gsub("(https://)(\\S*)", " ", comm.df$text) #remove URLs
comm.df$text = gsub("[^А-Яа-я]", " ", comm.df$text)

corp <- Corpus(VectorSource(comm.df$text), readerControl=list(language="ru", encoding="UTF-8"))
corp <- tm_map(corp, content_transformer(tolower))
corp <- tm_map(corp, removeWords, c(stopwords("ru"), "очень", "просто", "вообще", "хотя", "вроде", "это"))
corp <- tm_map(corp, stripWhitespace)

dtm.control <- list(weighting=weightTf, stemming=FALSE, bounds=list(global=c(1,Inf)))
dtm <- DocumentTermMatrix(corp, control=dtm.control)

#attempt at extracting ngrams
BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 4, max = 4))
dtm.bi <- DocumentTermMatrix(corp, control = list(tokenize = BigramTokenizer))
options(mc.cores=1) #you just gotta

dtm = removeSparseTerms(dtm, 0.9999)
word.freq <- sort(slam::col_sums(dtm, na.rm = T), decreasing = T)

png("~/anime-soc-ling/plots/wordcloud-dobrochan.png")
wordcloud(words = names(word.freq), freq = word.freq, scale = c(5,0.1),min.freq = 3, max.words = 200, random.order = F, colors = pal)
dev.off()
