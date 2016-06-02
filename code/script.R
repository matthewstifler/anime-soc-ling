library(devtools)
library(stringr)
library(dplyr)
library(tm)
library(wordcloud)
library(mallet)

source_gist("https://gist.github.com/paulokopny/63daf8ca42f9d842b122")
vk <- get.vk.connector(code = "code", app = "karepin")

board.getTopics <- function(vk.get_data, group_id, offset=0, count=20) {
  method="board.getTopics"
  assert_that(!is.null(group_id))
  assert_that(length(group_id)==1)
  assert_that(is.character(group_id)|is.numeric(group_id))
  if (is.character(group_id)){
    assert_that(str_length(group_id)>0)
    group_id <- as.numeric(group_id)
  }
  group_id <- abs(group_id)
  return(vk.get_data(method=method, group_id=group_id, offset=offset, count=count)$topics)
}

board.getComments <- function(vk.get_data, group_id, topic_id, offset=0, count=20) {
  method="board.getComments"
  assert_that(!is.null(group_id))
  assert_that(length(group_id)==1)
  assert_that(is.character(group_id)|is.numeric(group_id))
  if (is.character(group_id)){
    assert_that(str_length(group_id)>0)
    group_id <- as.numeric(group_id)
  }
  group_id <- abs(group_id)
  assert_that(!is.null(topic_id))
  assert_that(length(topic_id)==1)
  assert_that(is.character(topic_id)|is.numeric(topic_id))
  if (is.character(topic_id)){
    assert_that(str_length(topic_id)>0)
  } else {
    assert_that(topic_id>0)
  }
  return(vk.get_data(method=method, group_id=group_id, topic_id=topic_id, offset=offset, count=count)$comments)
}

id = 47

#learn number of discussions
topics = board.getTopics(vk, id, count = 1)
number = topics[[1]]

#get set
i = 0
j = 0
topics.df = data.frame()

#all set, go! this loop extracts all discussions' metadata and puts it in topics.df
for (j in 1:(number %/% 100 + 1)){
  offset = (j-1)*100
  count = ifelse(number - offset > 100, 100, number - offset)
  topics = board.getTopics(vk, 47, offset = offset, count = count)
  for (i in 2:length(topics)) { #what a shame
    topics.df = rbind(topics.df, topics[[i]] %>% as.data.frame()) #goddamn loops again
  }
}

#extracting discussions
i = 0
j = 0
k = 0
comm = c()
comm.df = data.frame()

#nested loops duh
for (j in 1:length(topics.df$tid)){
  tid = topics.df$tid[j]
  number = board.getComments(vk, id, tid, count = 1)[[1]]
  for (i in 1:(number %/% 100 + 1)){
    offset = (i-1)*100
    count = ifelse(number - offset > 100, 100, number - offset)
    comms = board.getComments(vk, id, tid, offset = offset, count = count)
    for (k in 2:length(comms)){
      comm.df = rbind(comm.df, comms[[k]][1:4] %>% as.data.frame())
    }
    str_c(as.character(nrow(comm.df)), " ") %>% str_c(as.POSIXct(Sys.time(), origin = "1970-01-01")) %>% str_c("\n") %>% cat()
  }
}

#working on text starts here

comm.df$text = gsub("[^А-Яа-я]", " ", comm.df$text)
corp <- Corpus(VectorSource(comm.df$text), readerControl=list(language="ru", encoding="UTF-8"))
corp <- tm_map(corp, content_transformer(tolower))
corp <- tm_map(corp, removeWords, c(stopwords("ru"), "очень", "просто", "вообще", "хотя", "вроде"))
corp <- tm_map(corp, stripWhitespace)

#stemming was baaad
dtm.control <- list(weighting=weightTf, stemming=FALSE, bounds=list(global=c(10,1000)))
dtm<-DocumentTermMatrix(corp, control=dtm.control)

findFreqTerms(dtm, 100)
term.freq <- colSums(as.matrix(dtm))
term.freq.sub <- subset(term.freq, term.freq >=200)
plot.df <- data.frame(term = names(term.freq.sub), freq = term.freq.sub)

#plotting top with reorder (!)
g = ggplot(plot.df[order(-plot.df$freq),], aes(x = reorder(term, freq), y = freq)) + geom_bar(stat = "identity") + xlab("Terms") + ylab("Count") +coord_flip() + theme(axis.text=element_text(size=12), axis.title=element_text(size=18))
ggsave(g, filename = "~/anime-soc-ling/plot.png", width = 16, height = 7.61, units = "in", dpi = 75)

m <- as.matrix(dtm)
# calculate the frequency of words and sort it by frequency
word.freq <- sort(colSums(m), decreasing = T)

#now this is how to save wordcloud so that they look neat! incredible!
#png("~/anime-soc-ling/wordcloud.png")
wordcloud(words = names(word.freq), freq = word.freq, scale = c(5,0.1),min.freq = 3, max.words = 200, random.order = F, colors = pal)
#dev.off()
#cool, amirite

#lda, sigh
mallet.instances <- mallet.import(as.character(comm.df$id), as.character(comm.df$text), "~/anime-soc-ling/stoplist", token.regexp = "[\\p{L}\\p{N}-]*\\p{L}+")
topic.model <- MalletLDA(num.topics=10) # количество тем
topic.model$loadDocuments(mallet.instances) 
topic.model$setAlphaOptimization(20, 50)

vocabulary <- topic.model$getVocabulary() # словарь корпуса
word.freqs <- mallet.word.freqs(topic.model) # таблица частотности слов
## вершина частотного списка (по документной частоте)
head(word.freqs[order(word.freqs$doc.freq, decreasing=T),],30)

## параметр — количество итераций
topic.model$train(2000)

## выбор наилучшей темы для каждого токена
topic.model$maximize(10)

## таблица распределения тем по документам
doc.topics <- mallet.doc.topics(topic.model, smoothed=TRUE, normalized=TRUE)
## таблица распределения слов по темам
topic.words <- mallet.topic.words(topic.model, smoothed=TRUE, normalized=TRUE)
## метки для тем (по трем главным словам)
topic.labels <- mallet.topic.labels(topic.model, topic.words, 3)

for (k in 1:nrow(topic.words)) {
  top <- paste(mallet.top.words(topic.model, topic.words[k,], 10)$words,collapse=" ")
  cat(paste(k, top, "\n"))
}

