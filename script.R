library(devtools)
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