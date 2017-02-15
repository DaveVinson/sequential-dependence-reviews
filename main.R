############################################################################
########## SEQUENTIAL DEPENDENCY IN YELP REVIEW RATINGS ####################
############################################################################
################## Written by David W. Vinson ##############################
############################################################################
############################################################################

####load libraries ####
x<-c("data.table", "ggplot2","reshape","plyr")
lapply(x, require, character.only = TRUE)
source("functions.R")

############################################################################
####************************* Load the data ***************************#####
############################################################################

#yelp reviews
data1 = read.table("seqdep_full.csv",sep=",", header=FALSE)
colnames(data1)=c("user","business","date","star")
data1 = data.table(data1)
data1$date = ceiling(data1$date/(86400)) #days and then round up (since time moves forward)
data1$datet =((data1$date)/365)+1970 #add years to date 

#Amazon reviews 
data = read.table("ratings_Movies_and_TV.csv",sep=",", header=FALSE)
colnames(data)=c("user","item","star","date")
data = data.table(data)
data$date = ceiling(data$date/(86400)) #days and then round up (since time moves forward)
data$datet =((data$date)/365)+1970 #add years to date 

####*******************************************************************#####
### quick histograms 
par(mfrow=c(2,2), mar=c(4,1,4,1), oma=c(0,3.75,0,0))
hist(data1$star,ylab="",xlab="Star Rating",main="Yelp",cex.main = 1.5, cex.lab=1.5,mgp=c(2.5,1,0))
hist(data$star,ylab="",xlab="Star Rating",main="Amazon",cex.main = 1.5,cex.lab=1.5,mgp=c(2.5,1,0))
hist(data1$datet,ylab="", xlab="Date by Year",main=NA,cex.main = 1.5,cex.lab=1.5, mgp=c(2.5,1,0))
hist(data$datet,ylab="", xlab="Date by Year",main=NA,cex.main = 1.5,cex.lab=1.5, mgp=c(2.5,1,0))
#hist(udat$dif_lag1,ylab="",xlab="Date Difference",main=NA,cex.lab=1.5, mgp=c(2.5,1,0)) #run code line 32-58
mtext('Review Frequency', side = 2, outer = TRUE, line = 2,cex=1.2)

###########################################################################
####*******************************************************************####
####*************** Lagging by review number **************************####
####*******************************************************************####
####*******************************************************************####
###########################################################################

#remove reviews, and reload them one at a time for analyses
rm(data1,data)

data = read.table("seqdep_full.csv",sep=",", header=FALSE)
colnames(data)=c("user","business","date","star") #for yelp
#data = read.table("~/Dropbox/Jones/data/ratings_Movies_and_TV.csv",sep=",", header=FALSE) #for Amazon
#colnames(data)=c("user","item","star","date") #for amazon

### Lag per review by user 
data = data.table(data)
data <- data[order(data$user,data$date),] 
setkey(data,user)

data[,mean_user:=star-(mean(star)),by=user] #x-mean
data$mean_user = scale(as.numeric(data$mean_user), center = TRUE, scale = F)
data$"0" <- sample(data$star) #create a random basline 
data[,"1":=panel_lag(star,1),by=user]
data[,"2":=panel_lag(star,2),by=user]
data[,"3":=panel_lag(star,3),by=user]
data[,"4":=panel_lag(star,4),by=user]
data[,"5":=panel_lag(star,5),by=user]
data[,"6":=panel_lag(star,6),by=user]
data[,"7":=panel_lag(star,7),by=user]

## run a model for each of the lagged levels 
revlag = lm(data$mean_user~data$"1") #run for each of nine variables 
summary(revlag)
confint(revlag,level = .999)


###########################################################################
####*******************************************************************####
####*******************************************************************####
####*******************************************************************####
####*******************************************************************####
###########################################################################

#aggregate for plotting purposes  
melted <- melt(data, id.vars=c("user","business","mean_user","date","star"), #for Yelp
               value.name="value", variable.name="lag") #for Yelp
# melted <- melt(data, id.vars=c("user","item","mean_user","star","date"), #for Amazon
#                value.name="value", variable.name="lag") #for Amazon
#write.csv(melted,"~/Desktop/melteddata.csv",col.names=T) #to save time


x = aggregate(melted$mean_user~melted$variable*melted$value,melted,
              function(x) c(M = mean(x), SE = sd(x)/sqrt(length(x))))
aggs = data.frame(as.matrix(x))
head(aggs)
colnames(aggs)[1]="lag"
colnames(aggs)[2]="star"
colnames(aggs)[3]="mean"
colnames(aggs)[4]="se"
aggs$mean = as.numeric(as.character(aggs$mean))
aggs$se = as.numeric(as.character(aggs$se))
aggs$lag = as.numeric(as.character(aggs$lag))

#################################### plot ############################
limits <- aes(ymax = aggs[aggs$lag!=0,]$mean + aggs[aggs$lag!=0,]$se, ymin= aggs[aggs$lag!=0,]$mean - aggs[aggs$lag!=0,]$se)
cc <- c("#000000","#053061","#2166ac","#4393c3","#92c5de", "#d6604d","#b2182b","#67001f")

p <- ggplot(aggs[aggs$lag!=0,], aes(y=mean, x=star,colour=as.factor(lag),group=as.factor(lag)))

p + geom_point()+geom_line(linetype="solid")+
  geom_errorbar(limits,width=0.25)+
  geom_line(data=aggs[aggs$lag==0,],linetype="dashed") +
  scale_colour_manual(bquote(''*italic(k)*''),breaks=c("1", "2","3","4","5","6","7"),values=cc)+
  geom_vline(xintercept = mean(data$star),linetype="dotted")+ 
  geom_vline(xintercept = median(data$star),linetype="dotted")+
  geom_vline(xintercept = Mode(data$star),linetype="dotted")+ 
  annotate("text", x = (mean(data$star)-.15), y = .2, angle = 90, label = "Mean")+ #yelp
  annotate("text", x = 4.15, y = .2, angle = 90, label = "Median / Mode")+ #yelp
  annotate("text", x = 1.75, y = -.015, label = "Baseline")+ #yelp
  # annotate("text", x = 4.1, y = .2, angle = 90, label = "Mean")+ # amazon
  # annotate("text", x = 4.9, y = .2, angle = 90, label = "Median / Mode")+ #amazon
  # annotate("text", x = 1.4, y = -.015, label = "Baseline")+ #amazon
  ylab(expression(paste('Review Deviation: '*italic(R[x]) - italic(M)(italic(R)[italic(T-x)])*'')))+
  xlab(bquote('Previous ('*italic(n-k)*') Review Rating'))+
  theme(plot.title=element_blank())+
  theme(axis.title.y = element_text(size = rel(1.5),angle=90))+
  theme(axis.title.x = element_text(size = rel(1.5),angle=0))+
  theme(axis.text=element_text(size=rel(1.5),face="plain"))+
  theme(legend.title = element_text(size = rel(1.5),face="plain"))+
  theme(legend.text = element_text(size = rel(1),face="plain"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(legend.key = element_blank(),
        legend.background=element_rect(linetype="solid",size=.1))
#export size:5.64, 4.89 pdf

####****************** magnitude of difference **************************####

melted = melted[melted$variable!=0,] #remove baseline
melted$variable = as.numeric(melted$variable)
melted$squared <- (melted$value-melted$mean_user)^2 #substract from k and square value for magnitude

mod <-lm(squared~as.numeric(variable),data=melted)
summary(mod)
confint(mod,level = .999)

####### aggregate for plot ###### 
test <-aggregate(squared~variable,melted,
                 function(x) c(M = mean(x), SE = sd(x)/sqrt(length(x))))
test = data.frame(as.matrix(test))
colnames(test)[1]="lag"
colnames(test)[2]="squared"
colnames(test)[3]="SE"
test$lag = test$lag-1 #scale 1-7 again 

#################################### plot ############################
limits <- aes(ymax = test$squared + test$SE, ymin= test$squared - test$SE)
p <- ggplot(test, aes(y=test$squared, x=test$lag))
p + geom_point()+geom_line()+geom_errorbar(limits,width=0.25)+
  ylab(bquote('Magnitude of Deviation: '*(italic(R[x]) - italic(M)(italic(R)[italic(T-x)]))^2*''))+ #change reviewer/business
  xlab(bquote('Previous ('*italic(k)*') Review Distance'))+ #change reviewer/business
  theme(plot.title=element_blank())+
  theme(axis.title.y = element_text(size = rel(1.5),angle=90))+
  theme(axis.title.x = element_text(size = rel(1.5),angle=0))+
  theme(axis.text=element_text(size=rel(1.5),face="plain"))+
  theme(legend.title = element_text(size = rel(1.5),face="plain"))+
  theme(legend.text = element_text(size = rel(1.5),face="plain"))+
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major = element_line(),
        panel.background = element_blank())+
  theme_classic()+
  theme(axis.line = element_line(colour = "black"))+
  theme(legend.key = element_blank(),
        legend.background=element_rect(linetype="solid",size=.1)) 
#export size:5.64, 4.89 pdf
