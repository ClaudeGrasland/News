library(sf)
library(plotly)
library(RColorBrewer)
library(data.table)
library(reldist)


#### ---------------- what ----------------
#' @title  Compute the average salience of the topic
#' @name what
#' @description create a table of the topic
#' @param hc an hypercube prepared as data.table
#' @param target (default = "Tagged news", alternative = subtag)
#' @param filter TRUE for all news, FALSE for tagged news 
#' @param title Title of the graphic


what <- function (hc = hypercube,
 #                 target = "Tagged news",
#                  filter = FALSE, 
                  title = "What ?")
{

# select data
#if (target=="Tagged news") {hc$tags = hc$tagged} else
#                            {hc$tags = hc$what==target}
#if (filter) {hc<-hc[hc$tagged==T,]
#             ytitle = "% of tagged news"} else
#             {ytitle = "% of total news" }  
tab<-hc[,list(news = sum(weight)),by = tags]
tab$pct<-100*tab$news/sum(tab$news)



p <- plot_ly(tab, 
             labels = ~tags, 
             values = ~pct, 
             type = 'pie') %>%
  layout(title = title,
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

output<-list("table" = tab, "plotly" =p)

return(output)

}



#### ---------------- who.what ----------------
#' @title  visualize variation of the topic between media
#' @name who.what
#' @description create a table of variation of the topic by media
#' @param hc an hypercube prepared as data.table
#' @param target (default = "Tagged news", alternative = subtag)
#' @param filter TRUE for all news, FALSE for tagged news 
#' @param title Title of the graphic


who.what <- function (hc = hypercube,
#                      target = "Tagged news",
#                      filter = FALSE, 
                      title = "Who says What ?")
{


#if (target=="Tagged news") {hc$tags = hc$tagged} else
#                            {hc$tags = hc$what==target}
#if (filter) {hc<-hc[hc$tagged==T,]
#             ytitle = "% of tagged news"} else
#             {ytitle = "% of total news" }   
tab<-hc[,list(news = sum(weight)),by = list(who,tags)]
tab<-tab[,list(tags=tags, news=news, pct = 100*news/sum(news)),by=who]
tab2<-tab[tab$tags==TRUE,]

 p <- plot_ly(tab2,
  x = ~who,
  y = ~pct,
  text = ~paste('<br>Number of news: ', round(news,0), '<br>Percentage of news:', round(pct,2),'%'),
#  name = ~tags,
  type = "bar")  %>%
  layout(title = title,
         yaxis = list(title = "% news"), 
         barmode = 'stack')
output<-list("table" = tab, "plotly" =p)

return(output)

}


#### ---------------- when.what ----------------
#' @title  visualize variation of the topic through time
#' @name when.what
#' @description create a table of variation of the topic by media
#' @param hc an hypercube prepared as data.table
#' @param target (default = "Tagged news", alternative = subtag)
#' @param filter TRUE for all news, FALSE for tagged news 
#' @param title Title of the graphic


when.what <- function (hc = hypercube,
#                      target = "Tagged news",
#                      filter = FALSE, 
                      title = "What and When?")
{
# select data
#if (target=="Tagged news") {hc$tags = hc$tagged} else
#                            {hc$tags = hc$what==target}
#if (filter) {hc<-hc[hc$tagged==T,]
#             ytitle = "% of tagged news"} else
#             {ytitle = "% of total news" }                   
tab<-hc[,list(news = sum(weight)),by = list(when,tags)]
tab<-tab[,list(tags=tags, news=news, pct = 100*news/sum(news)),by=when]
tab2<-tab[tab$tags==TRUE,]
tab2$when<-as.Date(tab2$when)

  p <- plot_ly(tab2,
  x = ~when,
  y = ~pct,
  type = "bar")  %>%
  layout(title = title,
         yaxis = list(title = "% news"), 
         xaxis = list("When"), 
         barmode = 'stack')

output<-list("table" = tab, "plotly" =p)

return(output)

}


#### ---------------- where.what ----------------
#' @title  visualize variation of the topic through space
#' @name where.what
#' @description create a table of variation of the topic by media
#' @param hc an hypercube prepared as data.table
#' @param target (default = "Tagged news", alternative = subtag)
#' @param filter TRUE for all news, FALSE for tagged news 
#' @param title Title of the graphic


where.what <- function (hc = hypercube,
 #                       target = "Tagged news",
#                        filter = FALSE, 
                        title = "What and Where ?")
{
# select data
# select data
#if (target=="Tagged news") {hc$tags = hc$tagged} else
#                            {hc$tags = as.factor(hc$what==target)}
##if (filter) {hc<-hc[hc$tagged==T,]
#             ytitle = "% of tagged news"} else
#             {ytitle = "% of total news" }                   
tab<-hc[,list(news = sum(weight)),by = list(where,tags)]
tab<-tab[,list(tags=tags, news=news, pct = 100*news/sum(news)),by=where]
ref<-tab[,list(news = sum(news)), by = list(tags)]
tab2<-tab[tab$tags==TRUE,]


m0<-min(tab2$pct)
m2<-mean(tab2$pct)
m1<-mean(tab2$pct[tab2$pct<m2])
m3<-mean(tab2$pct[tab2$pct>m2])
m4<-max(tab2$pct)
mybreaks<-c(m0,m1,m2,m3,m4)
mycol<-rev(brewer.pal(4,"RdYlBu"))
tab2$mycolors<-cut(tab2$pct, breaks=mybreaks)
levels(tab2$mycolors)<-c("Very Low","Low","High","Very High")

map<-readRDS("world.rda")
map<-merge(map,tab2,all.x=T,all.y=F,by.x="ISO3",by.y="where")


#map2<-map[is.na(map$pct)==F,]
#map2<-st_centroid(map2)
#map2<-st_drop_geometry(map2)


g <- list(showframe = TRUE,
          framecolor= toRGB("gray50"),
          coastlinecolor = toRGB("gray50"),
          showland = TRUE,
          landcolor = toRGB("gray90"),
          showcountries = TRUE,
          countrycolor = toRGB("white"),
          countrywidth = 0.2,
          projection = list(type = 'azimuthal equal area'))
  #        projection = list(type = 'Mercator'))



p<- plot_geo(map)%>%
  add_markers(x = ~lon,
              y = ~lat,
              sizes = c(0, 250),
              size = ~news,
              color= ~mycolors,
              colors= mycol,
              hoverinfo = "text",
              text = ~paste('State: ', NAMEen,
                            '<br /> Volume (number of topic news) : ', round(news,0),
                            '<br /> Salience (% of topic news) : ', round(pct,2))) %>%
  layout(geo = g,
         title = title)

output<-list("table" = tab, "plotly" =p)

return(output)

}



#### ---------------- when.who.what ----------------
#' @title  visualize variation of the topic by media through time
#' @name when.who.what
#' @description create a table of variation of the topic by media through time
#' @param hc an hypercube prepared as data.table
#' @param target (default = "Tagged news", alternative = subtag)
#' @param filter TRUE for all news, FALSE for tagged news 
#' @param title Title of the graphic


when.who.what <- function (hc = hypercube,
#                          target = "Tagged news",
#                          filter = FALSE, 
                          title = "What by Whom and When ?")
{
# select data
#if (target=="Tagged news") {hc$tags = hc$tagged} else
#                            {hc$tags = hc$what==target}
##if (filter) {hc<-hc[hc$tagged==T,]
#             ytitle = "% of tagged news"} else
#             {ytitle = "% of total news" }                   
tab<-hc[,list(news = sum(weight)),by = list(who,when,tags)]
tab<-tab[,list(tags=tags, news=news, pct = 100*news/sum(news)),by=list(who,when)]
tab2<-tab[tab$tags==TRUE,]
tab2$when<-as.Date(tab2$when)

p <- plot_ly(tab2,
             x = ~when,
             y = ~pct,
             name = ~who,
             color= ~who,
#             colors=palette,
             type="scatter",
             mode = 'lines')%>%
         layout(title = title,
         yaxis = list(title = "% news"), 
         xaxis = list(title = "When"))


output<-list("table" = tab, "plotly" =p)

return(output)

}

#### ---------------- when.where.what ----------------
#' @title  visualize variation of the topic through time and space
#' @name when.where.what
#' @description create a table of variation of the topic through time and space
#' @param hc an hypercube prepared as data.table
#' @param target (default = "Tagged news", alternative = subtag)
#' @param filter TRUE for all news, FALSE for tagged news 
#' @param title Title of the graphic


when.where.what <- function (hc = hypercube,
#                          target = "Tagged news",
#                          filter = FALSE, 
                          top = 20,
                          title = "What by Where and When ?")
{
# select data
#if (target=="Tagged news") {hc$tags = hc$tagged} else
#                            {hc$tags = hc$what==target}
#if (filter) {hc<-hc[hc$tagged==T,]
#             ytitle = "% of tagged news"} else
#             {ytitle = "% of total news" }                   
tab<-hc[,list(news = sum(weight)),by = list(where,when,tags)]
tab<-tab[,list(tags=tags, news=news, pct = 100*news/sum(news)),by=list(where,when)]
tab2<-tab[tab$tags==TRUE,]
tab2$when<-as.Date(tab2$when)



ref<-tab2[tab2$where!="_no_", list(news=sum(news)), by = list(where)]
ref<-ref[order(news, decreasing=T),]
sel<-ref$where[1:top]
tab2<-tab2[tab2$where %in% sel,]

m0<-min(tab2$pct)
m2<-mean(tab2$pct)
m1<-mean(tab2$pct[tab2$pct<m2])
m3<-mean(tab2$pct[tab2$pct>m2])
m4<-max(tab2$pct)
mybreaks<-c(m0,m1,m2,m3,m4)
mycol<-rev(brewer.pal(4,"RdYlBu"))
tab2$mycolors<-cut(tab2$pct, breaks=mybreaks)
levels(tab2$mycolors)<-c("Very Low","Low","High","Very High")


p <- plot_ly(tab2,
             x = ~when,
             y = ~where,
             size =  ~news,
             sizes = c(0,200),
             text = ~paste("News: ", round(news,0), '<br>Pct:', round(pct,3),'%'),
 #            marker = list(sizemin=0, sizeref=1, sizemode = "area"),   
             color= ~mycolors,
             colors= mycol,
             type="scatter",
             mode ="markers")%>%
         layout(title = title,
         yaxis = list(title = "% news"), 
         xaxis = list(title = "Top countries")) 

output<-list("table" = tab, "plotly" =p)

return(output)

}





#### ---------------- who.where.what ----------------
#' @title  visualize variation of the topic through media and space
#' @name who.where.what
#' @description create a table of variation of the topic through media and space
#' @param hc an hypercube prepared as data.table
#' @param target (default = "Tagged news", alternative = subtag)
#' @param filter TRUE for all news, FALSE for tagged news 
#' @param title Title of the graphic


who.where.what <- function (hc = hypercube,
#                          target = "Tagged news",
#                          filter = FALSE, 
                          top = 20,
                          title = "What by Where and Whom ?")
{
# select data
#if (target=="Tagged news") {hc$tags = hc$tagged} else
#                            {hc$tags = hc$what==target}
#if (filter) {hc<-hc[hc$tagged==T,]
#             ytitle = "% of tagged news"} else
#             {ytitle = "% of total news" }                   
tab<-hc[,list(news = sum(weight)),by = list(where,who,tags)]
tab<-tab[,list(tags=tags, news=news, pct = 100*news/sum(news)),by=list(where,who)]
tab2<-tab[tab$tags==TRUE,]




ref<-tab2[tab2$where!="_no_", list(news=sum(news)), by = list(where)]
ref<-ref[order(news, decreasing=T),]
sel<-ref$where[1:top]
tab2<-tab2[tab2$where %in% sel,]

m0<-min(tab2$pct)
m2<-mean(tab2$pct)
m1<-mean(tab2$pct[tab2$pct<m2])
m3<-mean(tab2$pct[tab2$pct>m2])
m4<-max(tab2$pct)
mybreaks<-c(m0,m1,m2,m3,m4)
mycol<-rev(brewer.pal(4,"RdYlBu"))
tab2$mycolors<-cut(tab2$pct, breaks=mybreaks)
levels(tab2$mycolors)<-c("Very Low","Low","High","Very High")


p <- plot_ly(tab2,
             x = ~who,
             y = ~where,
             size =  ~news,
             sizes = c(0,200),
             text = ~paste("News: ", round(news,0), '<br>Pct:', round(pct,3),'%'),
 #            marker = list(sizemin=0, sizeref=1, sizemode = "area"),   
             color= ~mycolors,
             colors= mycol,
             type="scatter",
             mode ="markers")%>%
         layout(title = title,
         yaxis = list(title = "Who"), 
         xaxis = list(title = "Where")) 

output<-list("table" = tab, "plotly" =p)

return(output)

}


