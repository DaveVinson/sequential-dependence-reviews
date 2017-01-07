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

####*******************************************************************#####
####load data ####
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
colnames(data)=c("user","business","date","star")
data = data.table(data)
data$date = ceiling(data$date/(86400)) #days and then round up (since time moves forward)

### Lag per review by user 
data <- data[order(data$user,data$date),] 
setkey(data,user)

data[,mean_user:=star-(mean(star)),by=user] #x-mean
data$mean_user = scale(as.numeric(data$mean_user), center = TRUE, scale = TRUE)
data$"0" <- sample(data$star) #create a random basline 
data[,"1":=panel_lag(star,1),by=user]
data[,"2":=panel_lag(star,2),by=user]
data[,"3":=panel_lag(star,3),by=user]
data[,"4":=panel_lag(star,4),by=user]
data[,"5":=panel_lag(star,5),by=user]
data[,"6":=panel_lag(star,6),by=user]
data[,"7":=panel_lag(star,7),by=user]

#run for each of nine variables 
revlag = lm(data$mean_user~data$"0") 
summary(revlag)
confint(revlag,level = .999)

#aggregate all nine for plotting  
melted <- melt(data, id.vars=c("user","business","mean_user","date","star"),
               value.name="value", variable.name="lag")  

#write.csv(melted,"~/Desktop/melteddata.csv",col.names=T)
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
limits <- aes(ymax = aggs$mean + aggs$se, ymin= aggs$mean - aggs$se)
cc <- scales::seq_gradient_pal("blue", "green", "Lab")(seq(0,1,length.out=8)) #for gradiant colours

p <- ggplot(aggs, aes(y=mean, x=star,colour=as.factor(lag),group=as.factor(lag)))
p + geom_point()+geom_line()+geom_errorbar(limits,width=0.25)+
  scale_colour_manual(bquote(''*italic(k)*''),values=cc)+
  ylab(bquote(''*italic(R[x]) - italic(M)(italic(R)[italic(T-x)])*''))+ #change reviewer/business
  xlab(bquote(''*italic(n-k)*' Review Rating'))+ #change reviewer/business
  theme(plot.title=element_blank())+
  theme(axis.title.y = element_text(size = rel(1.5),angle=90))+
  theme(axis.title.x = element_text(size = rel(1.5),angle=0))+
  theme(axis.text=element_text(size=rel(1.5),face="plain"))+
  theme(legend.title = element_text(size = rel(1.5),face="plain"))+
  theme(legend.text = element_text(size = rel(1.5),face="plain"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(legend.key = element_blank(),
        legend.background=element_rect(linetype="solid",size=.1)) 


####****************** magnitude of difference **************************####
#remove baseline
melted = melted[melted$variable!=0,] 
melted$variable = as.numeric(melted$variable)
#substract from k and square value for magnitude
melted$squared <- (melted$value-melted$mean_user)^2 

mod <-lm(squared~as.numeric(variable),data=melted)
summary(mod)
confint(mod,level = .999)

test <-aggregate(squared~variable,melted,
                 function(x) c(M = mean(x), SE = sd(x)/sqrt(length(x))))
test = data.frame(as.matrix(test))
colnames(test)[1]="lag"
colnames(test)[2]="squared"
colnames(test)[3]="SE"
#scale 1-7 again 
test$lag = test$lag-1 


limits <- aes(ymax = test$squared + test$SE, ymin= test$squared - test$SE)

p <- ggplot(test, aes(y=test$squared, x=test$lag))
p + geom_point()+geom_line()+geom_errorbar(limits,width=0.25)+
  #change reviewer/business
  ylab(bquote(''*(italic(k-R[x]) - italic(M)(italic(R)[italic(T-x)]))^2*''))+ 
  #change reviewer/business
  xlab(bquote(''*italic(k)*' Distance'))+ 
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


