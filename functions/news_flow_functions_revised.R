library(quanteda)
library(tidytext)
library(dplyr)



#### ---------------- nf_input -----------------

#' @title Input data to quanteda
#' @name nf_input
#' @description Convert  dataframe to quanteda format
#' @param source a dataframe
#' @param textfield a column with text
#' @param timefield a column with time
#' @param lang add a language ("en","fr", ...) 
#' @param country add a country in ISO3 format
#' @param source a media name (6 char maxi without blanks)

nf_input <-   function(    data = "df",
                           textfield = "text",
                           timefield = "time",
                           lang = "en",
                           country = "GBR",
                           source = "Guardi")
{
  # Recode
  text<-data[,textfield]
  time<-as.POSIXct(data[,timefield])
  
  # transfom in quanteda format
  qd<-quanteda::corpus(x = text)
  
  # add metadata
  qd$time<-as.POSIXct(rss[,timefield])
  qd$lang<-lang
  qd$country<-country
  qd$source<-source
  
  return(qd)
}


#### ---------------- unaccent-----------------
#' @title Remove accent end convert to ascii
#' @name unaccent
#' @description Remove accent and convert  texte to ascii - Merci pour la fonction à Alexandre Hobeika, https://data.hypotheses.org/564
#' @param text a text

unaccent <- function(text) {
  text <- gsub("['`^~\"]", " ", text)
  text <- iconv(text, from="UTF-8", to="ASCII//TRANSLIT//IGNORE")
  #  text <- iconv(text, to="ASCII//TRANSLIT//IGNORE")
  text <- gsub("['`^~\"]", "", text)
  return(text)
}


#### ---------------- tok_news-----------------
#' @title Tranfrom a qd object in token
#' @name tok_news
#' @description tokenize news with specific options
#' @param qd a corpus of news (quanteda format)
#' @param to_lower transform in lower case (default = TRUE)
#' @param keep_acro keep acronyms like US, OTAN ... in upper case (default = FALSE)
#' @param rem_punct remove punctuations (default = TRUE)
#' @param rem_stop remove stopwords (default = FALSE)
#' @param lang choose language (default = "en")
#' @param ascii remove accents (default = TRUE)


tok_news<-function(qd,lang="en", to_lower=T, keep_acro=F,rem_punct=T,rem_stop=F,ascii=T)
{
  unaccent <- function(text) {
    text <- gsub("['`^~\"]", " ", text)
    text <- iconv(text, from="UTF-8", to="ASCII//TRANSLIT//IGNORE")
    #  text <- iconv(text, to="ASCII//TRANSLIT//IGNORE")
    text <- gsub("['`^~\"]", "", text)
    return(text)
  }
  
  if (ascii ==T) {qd<-unaccent(qd)}
  if (to_lower){
    if (rem_stop){
      tok<-tokens_remove(tokens(char_tolower(qd,keep_acronyms = keep_acro), remove_punct =rem_punct),stopwords(lang))}
    else {
      tok<-tokens_remove(tokens(char_tolower(qd,keep_acronyms = keep_acro), remove_punct = rem_punct))}
  }
  else {  if (rem_stop){
    tok<-tokens_remove(tokens(qd,remove_punct=rem_punct),stopwords(lang))}
    else {
      tok<-tokens_remove(tokens(qd, remove_punct = rem_punct))}
  }
  return(tok)
}

#### ---------------- nf_dico-----------------
#' @title Create a dictionnary with associated parameters of tokenization 
#' @name nf_dico
#' @description 
#' @param dic a dataframe containing columns tag, word and lang
#' @param to_lower transform in lower case (default = TRUE)
#' @param keep_acro keep acronyms like US, OTAN ... in upper case (default = FALSE)
#' @param rem_punct remove punctuations (default = TRUE)
#' @param rem_stop remove stopwords (default = FALSE)
#' @param lang choose language (default = "en")
#' @param ascii remove accents (default = TRUE)
#' @param style type of keyword (default = "glob", otherwise "regexp")


nf_dico <- function(dic = dic,
                    to_lower=T,
                    keep_acro = F,
                    rem_punct = T,
                    rem_stop = F,
                    lang="en",
                    ascii =T,
                    style ="glob")
{
  
  
  unaccent <- function(text) {
    text <- gsub("['`^~\"]", " ", text)
    text <- iconv(text, from="UTF-8", to="ASCII//TRANSLIT//IGNORE")
    #  text <- iconv(text, to="ASCII//TRANSLIT//IGNORE")
    text <- gsub("['`^~\"]", "", text)
    return(text)
  }
  
  
  
  # Load and select keywords
  dic<-dic[dic$lang==lang,]
  dic<-dic[,c("word","tag")]
  if(to_lower) {dic$word<-tolower(dic$word)}
  if(ascii) {dic$word<-unaccent(dic$word)}
  ##### Code pas joli joli ... ####
  names(dic)<-c("word","sentiment")
  
  tags<-quanteda::as.dictionary(dic)
  ####################################
  dico<-list()
  dico$tags      <- tags
  dico$to_lower  <- to_lower
  dico$keep_acro <- keep_acro
  dico$rem_punct <- rem_punct
  dico$rem_stop  <- rem_stop
  dico$lang      <- lang
  dico$ascii  <- ascii
  dico$style     <- style
  return(dico)
}



#### ---------------- nf_tag-----------------
#' @title create the list of tag found in each text of a corpus
#' @name nf_tag
#' @description apply a dictionnary to the documents and create list of tags found
#' @param news a corpus of news (in quanteda format)
#' @param dico a dictionnary (created by nf_dico)
#' @param tagname the name of the tag)



nf_tag<-function(news = corpus,
                 dico = dico, 
                 tagname="tag")
{
  
  # Tokenize
  tok<-tok_news(news,lang=dico$lang, to_lower=dico$to_lower,           keep_acro=dico$keep_acro,rem_punct=dico$rem_punct,rem_stop=dico$rem_stop, ascii=dico$ascii)
  
  # Search tags
  tok<-tokens_select(tok,dico$tags, valuetype = dico$style, case_insensitive = F)
  tok<-tokens_lookup(tok,dico$tags,valuetype = dico$style, case_insensitive = F)
  collage<-function (x) {paste(x, collapse=" ")}
  x<-lapply(tok, FUN=collage)
  m<-as.character(as.matrix(x,ncol=1))
  
  
  # Add results to quanteda object
  docvars(news)[,paste(tagname,"test",sep="_")]<-m!=""
  docvars(news)[,paste(tagname,"list",sep="_")]<-m
  
  
  return(news)
  
}



#### ---------------- nf_cube ----------------
#' @title cross four dimensions of news tags
#' @name nf_cube
#' @description create a table crossing tags related to four dimensios
#' @param news a corpus of news (in qd format) with various columns of tag
#' @param who the source dimension
#' @param when the time dimension
#' @param timespan aggregation of time
#' @param what the topic dimension defined by a list of tags
#' @param unique counting each tag only once
#' @param normalize normalize the weight of tags to have a sum equal of 1 for ech item (TRUE/FALSE)



nf_cube<-function(      news = corpus,
                        who = "source",
                        when = "time",
                        timespan = "week",
                        what = "tag_list",
                        unique = TRUE,
                        normalize = TRUE)
  
{  
  
  # prepare data
  df<- data.frame(docnames(news), docvars(news)[,c(who,when,what)],stringsAsFactors = F)
  names(df)<-c("id","who","when","what")
  
  # change time span
  df$when<-as.character(cut(as.Date(df$when), timespan, start.on.monday = TRUE))
  
  # unnest what
  df$what<-as.character(df$what)
  df$what[df$what==""]<-"_no_"
  df<-unnest_tokens(df,what,what,to_lower=F)
  
  # correct tags
  if (unique) df<-unique(df)
  
  # normalize
  if (normalize) {
    x<-as.data.frame(table(df$id))
    names(x)<-c("id","weight")
    x$weight<-1/x$weight
    df<-merge(df,x,by="id")
  } else df$weight<-1
  
  # Aggregate
  cube<-df%>%group_by(who, when,what)%>%summarise(weight=sum(weight))
  
  return (cube)
}


#### ---------------- nf_hypercube ----------------
#' @title cross four dimensions of news tags
#' @name nf_hypercube
#' @description create a table crossing tags related to four dimensios
#' @param news a corpus of news (in qd format) with various columns of tag
#' @param who the source dimension
#' @param when the time dimension
#' @param timespan aggreation of time
#' @param what a list of topic 
#' @param where a list of places
#' @param unique counting each tag only once
#' @param normalize normalize the weight of tags to have a sum equal of 1 for ech item (TRUE/FALSE)


nf_hypercube<-function( news = corpus,
                        who = "source",
                        when = "time",
                        timespan = "week",
                        what = "what_list",
                        where = "where_list",
                        unique = TRUE,
                        normalize = TRUE)
  
{
  
  
  
  # prepare data
  df<- data.frame(docnames(news), docvars(news)[,c(who,when,what,where)],stringsAsFactors = F)
  names(df)<-c("id","who","when","what","where")
  
  # change time span
  df$when<-as.character(cut(as.Date(df$when), timespan, start.on.monday = TRUE))
  
  # unnest what
  df$what<-as.character(df$what)
  df$what[df$what==""]<-"_no_"
  df<-unnest_tokens(df,what,what,to_lower=F)
  
  # unnest where
  df$where[df$where==""]<-"_no_"
  df<-unnest_tokens(df,where,where,to_lower=F)
  
  # correct tags
  if (unique) df<-unique(df)
  
  # normalize
  if (normalize) {
    x<-as.data.frame(table(df$id))
    names(x)<-c("id","weight")
    x$weight<-1/x$weight
    df<-merge(df,x,by="id")
  } else df$weight<-1
  
  # Aggregate
  hypercube<-df%>%group_by(who, when,what,where)%>%summarise(weight=sum(weight))
  
  # Return result
  
  return (hypercube)
  
}



#### ---------------- nf_pentacube ----------------
#' @title create a network of interlinked spatial units
#' @name nf_pentacube
#' @description create a network of interlinked states
#' @param news a corpus of news with at less one column of spatial tags
#' @param who the source dimension
#' @param when the time dimension
#' @param timespan aggreation of time
#' @param what a list of topics
#' @param where a list of places
#' @param unique counting each tag only once
#' @param normalize normalize the weight of tags to have a sum equal of 1 for ech item (TRUE/FALSE)


nf_pentacube<-function( news = corpus,
                        who = "source",
                        when = "time",
                        timespan = "week",
                        what = "what_list",
                        where = "where_list",
                        unique = TRUE,
                        normalize = TRUE)
{
  
  
  
  
  # prepare data
  df<- data.frame(docnames(news), docvars(news)[,c(who,when,what,where)],stringsAsFactors = F)
  names(df)<-c("id","who","when","what","where1")
  
  
  # change time span
  df$when<-as.character(cut(as.Date(df$when), timespan, start.on.monday = TRUE))
  
  # unnest what
  df$what[df$what==""]<-"_no_"
  df<-unnest_tokens(df,what,what,to_lower=F)
  
  # unnest where
  df$where1[df$where1==""]<-"_no_"
  df$where2<-df$where1
  df<-unnest_tokens(df,where1,where1,to_lower=F)
  df<-unnest_tokens(df,where2,where2,to_lower=F)
  
  # correct tags
  if (unique) df<-unique(df)
  
  # normalize
  if (normalize) {
    x<-as.data.frame(table(df$id))
    names(x)<-c("id","weight")
    x$weight<-1/x$weight
    df<-merge(df,x,by="id")
  } else df$weight<-1
  
  # Aggregate
  pentacube<-df%>%group_by(who, when,what,where1,where2)%>%summarise(weight=sum(weight))
  
  
  
  # export result
  return(pentacube)
  
}

