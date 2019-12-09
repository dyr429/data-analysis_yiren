
library(ggplot2)
library(rstudioapi)
library(plyr)

library(dplyr)

## Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   groupvars: a vector containing names of columns that contain grouping variables
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  library(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}


#Set work directory
current_path <- getActiveDocumentContext()$path 
setwd(dirname(current_path ))
print( getwd() )

df <- read.table(file = 'C:/Users/dyr42/OneDrive/WPI Study/motionExp/motion_cm_pilot1.tsv', sep = '\t',header = TRUE)

# check flattened data
head(df$realPercentage,5)
df <- df %>% mutate(diff= abs(realPercentage*100-selectedPercentage))
df <- df %>% mutate(diffraw= selectedPercentage-realPercentage*100)

head(df$diff,5)
df <- df %>% mutate(err= log(diff+0.125,2))
df
# cal summary
conf_level= 0.95
summary <- ddply(df, c("vis","realPercentage"), summarise,
                 N    = length(err),
                 mean = mean(err),
                 sd   = sd(err),
                 se   = sd / sqrt(N),
                 ci  = mean - qt(1 - ((1 - conf_level) / 2), N - 1) * se
)
summary
#cm error
summary <- summary %>% mutate(err= log(mean+0.125,2))

#plot Line chart for both
ggplot(summary, aes(x=realPercentage, y=mean, group=vis)) +
  geom_line(aes(color=vis))+
  geom_point(aes(color=vis, size =3))+
  theme(legend.position="top")

movingSummary <- subset(summary,  vis== "moving")
rotationSummary <- subset(summary,  vis== "rotation")

#moving ci plot
ggplot(movingSummary, aes(x=realPercentage, y=mean )) +
  geom_line(aes(color=vis))+
  geom_point(aes(color=vis, size =3))+
  geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), colour="black", width=.03) +
  theme(legend.position="top")

#rotation ci plot
ggplot(rotationsummary, aes(x=realPercentage, y=mean )) +
  geom_line(aes(color=vis))+
  geom_point(aes(color=vis, size =3))+
  geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), colour="black", width=.03) +
  theme(legend.position="top")

#ci sum
summaryAll <- ddply(df, c("vis"), summarise,
                 N    = length(err),
                 mean = mean(err),
                 sd   = sd(err),
                 se   = sd / sqrt(N),
                 ci  = mean - qt(1 - ((1 - conf_level) / 2), N - 1) * se
)
summaryAll
#Plot overall CI
ggplot(summaryAll, aes(x=vis, y=mean)) +
  geom_point(aes(color=vis, size =3))+
  geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), colour="black", width=.03) +
  theme(legend.position="top")



#Scatter plot of data
ggplot(df, aes(x=vis, y=diffraw,group =vis)) +
  geom_point(aes(color=vis))+
  theme(legend.position="top")


#Interactive Test
# Define the content and format of the tooltip in the "text" aesthetic
df
# Define the content and format of the tooltip in the "text" aesthetic
p <- ggplot(df, aes(x=vis, y=diffraw, group=vis,
                    text=paste("sessionId:",sessionId))) + 
  geom_point(aes(color=vis))

library(plotly)
p <- ggplotly(p, tooltip="text")
print(p)
   
