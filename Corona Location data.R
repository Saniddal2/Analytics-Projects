getwd()
library("RJSONIO")  
library("twitteR")  
library("ROAuth")

load(file="cred.Rdata")

setup_twitter_oauth(cred$consumerKey, cred$consumerSecret, 
                    cred$oauthKey, cred$oauthSecret)

Hindustan="Mumbai"
lat= 19.075983
long= 72.877655
dist=100.0
geocode=paste(lat, ',',long,',', dist, 'mi', sep="")
print(geocode)
count=250
text="#Covid"
twList<- searchTwitter(text, n=count, geocode=geocode)
India <- twListToDF(twList)
India$location <- "Mumbai"
India$Region <- "Asia"
View(India)


UnitedStaes="Los Angeles"
lat= 34.052235
long= -118.243683
dist=100.0
geocode=paste(lat, ',',long,',', dist, 'mi', sep="")
print(geocode)
count=250
text="#Covid"
twList<- searchTwitter(text, n=count, geocode=geocode)
USA <- twListToDF(twList)
USA$location <- "Los Angeles"
USA$Region <- "North America"
View(USA)

UnitedKingdom="Barnsley"
lat= 53.552631
long= -1.479726
dist=100.0
geocode=paste(lat, ',',long,',', dist, 'mi', sep="")
print(geocode)
count=250
text="#Covid"
twList<- searchTwitter(text, n=count, geocode=geocode)
London <- twListToDF(twList)
London$location <- "Barnsley"
London$Region <- "Europe"
View(London)

Brazil="Sao Paulo"
lat= -23.550520
long= -46.633308
dist=100.0
geocode=paste(lat, ',',long,',', dist, 'mi', sep="")
print(geocode)
count=250
text="#Covid"
twList<- searchTwitter(text, n=count, geocode=geocode)
Brasil <- twListToDF(twList)
Brasil$location <- "Sao Paulo"
Brasil$Region <- "South America"
View(Brasil)

Countriescovidcases <- rbind(India, USA, London, Brasil)
View(Countriescovidcases)
save(file="./Covidregion", Countriescovidcases)

Covidcopy <- Countriescovidcases
Covidcopy <- Covidcopy[!duplicated(Covidcopy[c("id")]),]
View(Covidcopy)

removeNonASCII<- function(txt){
  return(iconv(txt, "latin1", "ASCII", sub=""))
}

Covidcopy$text <- removeNonASCII(Covidcopy$text)
removeCntrls<- function(x){
  x<- gsub("[[:cntrl:]]",  "",x)
  return(x)
}

Covidcopy$text <- removeCntrls(Covidcopy$text)

removeNewLines<- function(x){
  x<- gsub("\\d+",  " ",x)
  return(x)
}

Covidcopy$text <- removeNewLines(Covidcopy$text)

removeTabs<- function(x){
  x<- gsub("\\t+",  " ",x)
  return(x)
}

Covidcopy$text <- removeTabs(Covidcopy$text)

removeDQuotes<- function(x){
  return(gsub("\"","_", x))
}
Covidcopy$text <- removeDQuotes(Covidcopy$text)

removeSQuotes<- function(x){
  return(gsub("\'","_", x))
}
Covidcopy$text <- removeSQuotes(Covidcopy$text)

removeSemiCommas<- function(x){
  return(gsub(";",",", x))
}
Covidcopy$text <- removeSemiCommas(Covidcopy$text)


strip<- function(x){
  return(gsub(";",",", x))
}
Covidcopy$text <- trimws(Covidcopy$text, which = c("both"), whitespace = "[ \t\r\n]")

Covidcopy$favorited  <- ""
Covidcopy$replyToSN  <- ""
Covidcopy$truncated  <- ""
Covidcopy$replyToUID <- ""
Covidcopy$replyToSID <- ""
Covidcopy$isRetweet  <- ""
Covidcopy$retweeted  <- ""

Covidcopy$statusSource<- removeNonASCII(sub(".*>(.*)</a>", "\\1", Covidcopy$statusSource))
Initial =table(Covidcopy$statusSource)
Initial<- as.data.frame(Initial)
names(Initial)
names(Initial) <- c("sourceApp", "count")
View(Initial)
View(Covidcopy)
load(file = "Covidcopy")
library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(reshape2)
library(dplyr)

s <- get_nrc_sentiment(Covidcopy$text)
barplot(colSums(s),
        las = 2,
        col = rainbow(10),
        ylab = 'Count',
        main = 'Sentiment SCores for Covid
Covidcopy')

Covidcopy$statusSource[c(1,5)]
Covidcopy$statusSource<- removeNonASCII(sub(".*>(.*)</a>", "\\1", Covidcopy$statusSource))
App =table(Covidcopy$statusSource)
App<- as.data.frame(tab)
names(App)
names(App) <- c("source", "count")




library(ggplot2)
ggplot(data=App, aes(x=App$source, y= App$count, col=source, ))+
  geom_bar(stat = "identity", show.legend=FALSE) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

library(tm)
library(RColorBrewer)
library(wordcloud) #https://cran.r-project.org/web/packages/wordcloud/wordcloud.pdf
?wordcloud
windows()
wordcloud(App$source, freq=App$count, scale=c(1,3), 
          random.order=FALSE, random.color=TRUE,
          colors=brewer.pal(8, "Dark2"))
#Check this One, I have not tried it:
##https://cran.r-project.org/web/packages/wordcloud2/vignettes/wordcloud.html

App_1 <- App[App$count <225,]
ggplot(data=App_1, aes(x=App_1$source, y= App_1$count, col=source, ))+
  geom_bar(stat = "identity", show.legend=FALSE) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

wordcloud(App_1$source, freq=App_1$count, scale=c(2,3), 
          random.order=FALSE, random.color=TRUE,
          colors=brewer.pal(8, "Dark2"))

require(devtools)
library("wordcloud2")

wordcloud2(App_1, color = "yellow", backgroundColor = "Black")
wordcloud2(App_1, minRotation = -pi/6, maxRotation = -pi/6, minSize = 10,
           rotateRatio = 7, color = "random-light", backgroundColor = "Black")



load(file="Covidcopy")
Covidcopy$favoriteCount <- as.character(Covidcopy$favoriteCount)
summary(Covidcopy$favoriteCount)
summary(Covidcopy$retweetCount)

Covidcopy$created <- as.character(Covidcopy$created)
Covidcopy$retweetCount <- as.character(Covidcopy$retweetCount)
write.table(Covidcopy, file=".\\Data visualization project\\", 
            quote=TRUE, sep="\t",
            na="NULL",  row.names=FALSE)



View(Covidcopy)

write.csv(Covidcopy, file="TableauCopyExportFinal.xlsx", row.names=FALSE, sep=",")



