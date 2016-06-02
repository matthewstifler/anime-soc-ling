library(devtools)
library(stringr)
library(dplyr)
source_gist("https://gist.github.com/paulokopny/63daf8ca42f9d842b122")
vk <- get.vk.connector(code = "code", app = "karepin")

wall.get <- function(vk.get_data, domain=NULL, owner_id=NULL, offset=0, count=20) {
  print("Waiting...")
  Sys.sleep(1)
  print("Getting posts...")
  method = "wall.get"
  if (is.null(owner_id)) {
    assert_that(is.character(domain))
    assert_that(length(domain)==1)
    assert_that(str_length(domain)>0)
    return(vk.get_data(method=method, domain=domain, offset=offset, count=count))
  } else {
    assert_that(is.null(domain))
    assert_that(is.numeric(owner_id))
    assert_that(length(owner_id)==1)
    assert_that(owner_id!=0)
    return(vk.get_data(method=method, owner_id=owner_id, offset=offset, count=count))
  }
}

#slightly tweaked wall.get for comments retieval
wall.getComments <- function(vk.get_data, owner_id=NULL, post_id = NULL, offset=0, count=100) {
  method = "wall.getComments"
  if (is.null(owner_id)) {
    assert_that(is.character(domain))
    assert_that(length(domain)==1)
    assert_that(str_length(domain)>0)
    return(vk.get_data(method=method, domain=domain, offset=offset, count=count))
  } else {
    assert_that(is.numeric(owner_id))
    assert_that(length(owner_id)==1)
    assert_that(owner_id!=0)
    return(vk.get_data(method=method, owner_id=owner_id, offset=offset, count=count, post_id = post_id))
  }
}

detach("package:tm", unload=TRUE) #bc conflicts
detach("package:NLP", unload = TRUE)

#get number of posts and their ids

domain = "2d_ch"

posts.number = wall.get(vk, domain = domain, count = 1)[[1]]
owner_id = wall.get(vk, domain = domain, count = 1)[[2]][2] %>% unlist %>% as.numeric
posts.df = data.frame()
j = 0
i = 0
#loop for getting all posts' ids
for (j in 1:(posts.number %/% 100 + 1)){ #calculate n of iterations, whole number of all posts / 100 (max number of posts available for one request) + 1 for the remainder
  offset = (j-1)*100 
  count = ifelse(posts.number - offset > 100, 100, posts.number - offset) # if not last iteration, then count = 100, if last - the remainder
  posts = wall.get(vk, domain = domain, count = count, offset = offset)
  for (i in 2:length(posts)) { 
    tmp = posts[[i]] %>% as.data.frame()
    posts.df = rbind(posts.df, tmp[,c(1, ncol(tmp) - 2)]) #1st - id, 2nd - n of comms, so that we know how many we gotta download
  }
  str_c(as.character(nrow(posts.df)), "/") %>% str_c(as.character(posts.number)) %>% str_c(" ") %>% str_c(as.POSIXct(Sys.time(), origin = "1970-01-01")) %>% str_c("\n") %>% cat()
  rm(tmp)
}

#downloading comments

i = 0
j = 0
k = 0
comms = c()
comm.number = c()
comm.df = data.frame()

for (j in 1:length(posts.df$id)){
  post_id = posts.df$id[j]
  comm.number = posts.df$count[j]
  if (comm.number > 0) {
    for (i in 1:(comm.number %/% 100 + 1)){
      offset = (i-1)*100
      count = ifelse(comm.number - offset > 100, 100, comm.number - offset)
      comms = wall.getComments(vk, owner_id = owner_id, post_id = post_id, offset = offset, count = count)
      if (length(comms) > 1){
        for (k in 2:length(comms)){
          comm.df = rbind(comm.df, comms[[k]][1:5] %>% as.data.frame())
        }
      }
      str_c(as.character(nrow(comm.df)), "/") %>% str_c(as.character(sum(posts.df$count))) %>% str_c(" ") %>% str_c(as.POSIXct(Sys.time(), origin = "1970-01-01")) %>% str_c("\n") %>% cat()
    }
  }
}