library(ggplot2)
library(rstudioapi)
library(plyr)
library(dplyr)
library(stringr)

#set directory and load data
current_path <- getActiveDocumentContext()$path 
setwd(dirname(current_path ))
print( getwd() )
filePath <- getwd()
fileName <- "/motion-JND-pilot1.tsv"
filePath<- paste(filePath,fileName,sep='')
filePath
df <- read.table(file = filePath, sep = '\t',header = TRUE)
head(df)

#length of unique sessionId, 
length(unique(df[["sessionId"]]))
uniIds <- unique(df[["sessionId"]])
uniIds
#isGoodId <- function(X) 
#{ X[ ifelse(length(which(df$sessionId == X))==250, TRUE,FALSE)]
#}
#isGoodId("-LvGl32WAAch5JT0ElYv")
#length(which(df$sessionId == "-LvGl32WAAch5JT0ElYv"))==250
#validIds <- Filter( isGoodId, uniIds)
validIds <- c("-LvGky0NyaZ4yxT1Bea6","-LvGl-7zbUWltBQFjrs8","-LvGlzdbD7lXX4lcsSt7")
df <- subset(df,  (sessionId %in% validIds))


#line chart for each anchor
ggplot(df, aes(x=localIndex, y=compare, group=interaction(sessionId, direction) )) +
  geom_line(aes(color=sessionId))+
  theme(legend.position="top")+
facet_wrap(~anchor)

#Calculate JND, devide into 2 groups of last 10 trials. 
#speed for flow: travel distance: 1000, duration: 100/s, V= 10s px/s
df
dfTail <- subset(df,  localIndex>20)
dfTail <- dfTail %>% mutate(diff= abs(anchor-compare))
conf_level= 0.95

summary <- ddply(dfTail, c("direction","anchor"), summarise,
                 N    = length(diff),
                 mean = mean(diff),
                 sd   = sd(diff),
                 se   = sd / sqrt(N),
                 ci  = mean - qt(1 - ((1 - conf_level) / 2), N - 1) * se
)
summary
#CI plot
ggplot(summary, aes(x=anchor, y=mean )) +
  geom_line(aes(color=direction))+
  geom_point(aes(color=direction, size =2))+
  geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci,color=direction),  width=2) +
  theme(legend.position="top")

#plot after adjustment, and regression
summaryAdjust <- summary %>% mutate(adjustAnchor=ifelse(direction=="up", anchor-mean, anchor+mean))
ggplot(summaryAdjust, aes(x=adjustAnchor, y=mean )) +
  geom_point(aes(color=direction, size =2))+
  geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci,color=direction),  width=2) +
  
  geom_smooth(method = "lm", se = FALSE) +
  theme(legend.position="top")



  