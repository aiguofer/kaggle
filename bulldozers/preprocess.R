#!/usr/bin/Rscript
## Helper function to parse the SaleDate Column ##
getDate <- function(saleDateCol) {
  result <- matrix(0,nrow=length(saleDateCol),ncol=3)
  for(i in 1:length(saleDateCol)) {
    spt <- strsplit(as.character(saleDateCol[i]),'/')[[1]]
    result[i,1] <- as.numeric(as.character(spt[1]))
    result[i,2] <- as.numeric(as.character(spt[2]))
    result[i,3] <- as.numeric(as.character(strsplit(spt[3]," ")[[1]][1]))
  }
  
  colnames(result) <- c("SaleMonth","SaleDay","SaleYear")
  return(result)
}

#multicore on linux
#library("doMC")
#registerDoMC(3)

## Read in the data ##
dat <- read.csv("./data/Train&Valid.csv")
appendix <- read.csv("./data/Machine_Appendix.csv")
test <- read.csv("./data/Test.csv")

train.IDs <- dat$SalesID
test.IDs <- test$SalesID

## I stack the training data on top of the validation data ##
## This is so I have access to every possible level for a factor ##
stacked <- rbind(dat[,-2],test)

## Pull in the data from the machine appendix ##
merged <- merge(stacked,appendix,by="MachineID",all.x=TRUE)
dat <- merge(dat,appendix,by="MachineID",all.x=TRUE)

## This section was used for selecting variables that had a low percentage of missing values
#cols<-NULL
#for(i in 1:ncol(merged)){
#  cols<- c(cols,round(100*length(which(is.na(merged[,i])|merged[,i]==""))/nrow(merged)))
#}
#which(cols<30)
# use above info to select columns
#merged <- merged[,c(2,5,6,17,21,32,53,59,62,63,9)]
#dat <- dat[,c(2,3,6,7,18,22,33,54,60,63,64,10)]

## Put the salesdate at the end and delete repeated columns coming from appendix
merged <- merged[,c(1:ncol(merged),9)]
dat <- dat[,c(1:ncol(dat),10)]
merged <- merged[,-c(1,9,grep("\\.y",colnames(merged)))]
dat <- dat[,-c(1,10,grep("\\.y",colnames(dat)))]

final.data <- data.frame(merged[,1])
feature.map <- NULL
for(i in 2:ncol(merged)) {
  print(i)
  if(names(merged)[i] == "saledate.1") {
    date <- getDate(merged[,i])
    final.data <- cbind(final.data,date)
  } else {
    col <- merged[,i]
    if(names(merged)[i] == "MfgYear" | names(merged)[i] == "YearMade"){
      col[which(col<1940|col>2012|col=="")] <- NA
    }
    else if(names(merged)[i] == "MachineHoursCurrentMeter"){
      col[which(col == "" | is.na(col))] <- 0
    }
    else {
      if(names(merged)[i] == "auctioneerID"){
        col[which(col == "" | col==99 | is.na(col))] <- 0
        col[which(col>0)] <- 1
      }
      else if(names(merged)[i] == "Enclosure"){
        col[which(col == "" | is.na(col))] <- "None or Unspecified"
      }
      
      ## Order index by increasing avg SalePrice
      tmpmap<-NULL
      tmpid<-unique(col)
      for(j in tmpid){
        tmpmap<-rbind(tmpmap,c(j,mean(dat$SalePrice[which(dat[,i+1]==j)],na.rm=TRUE)))
      }
      tmpmap<-tmpmap[order(as.numeric(tmpmap[,2])),]
      
      ## convert to index ##
      int.version <- match(col,tmpmap[,1])
      
      ## save the mapping ##
      map <- cbind(tmpmap[,1],tmpmap[,2],1:length(tmpmap[,1]))
      map <- cbind(rep(names(merged)[i],nrow(map)),map)
      feature.map <- rbind(feature.map,map)
      
      col <- int.version
    }
    
    final.data <- cbind(final.data,col)
    colnames(final.data)[i] <- colnames(merged)[i]
  }
}
colnames(final.data)[1] <- colnames(merged)[1]

## Fix MfgYear
modelid<-unique(final.data$ModelID.x)
#map<-NULL
#for(i in modelid){
#  map<-rbind(map,c(i,round(mean(final.data$MfgYear[which(final.data$ModelID.x==i)],na.rm=TRUE))))
#}
#where<-match(final.data$ModelID.x,map[,1])
#yr<-which(is.na(final.data$MfgYear))
#final.data$MfgYear[yr]<-map[where[yr],2]
#yr<-which(is.na(final.data$MfgYear))
#final.data$MfgYear[yr]<-round(mean(final.data$MfgYear,na.rm=TRUE))

## Fix YearMade when NA by taking the mean YearMade for that specific modelid
map<-NULL
for(i in modelid){
  map<-rbind(map,c(i,round(mean(final.data$YearMade[which(final.data$ModelID.x==i)],na.rm=TRUE))))
}
where<-match(final.data$ModelID.x,map[,1])
yr<-which(is.na(final.data$YearMade))
final.data$YearMade[yr]<-map[where[yr],2]
yr<-which(is.na(final.data$YearMade))
final.data$YearMade[yr]<-round(mean(final.data$YearMade,na.rm=TRUE))

## Create age column
final.data$age <- final.data$SaleYear - final.data$YearMade

## Split joint data back into separate sets
SalePrice <- log(dat$SalePrice[match(train.IDs,dat$SalesID)])
train <- cbind(SalePrice,final.data[match(train.IDs,final.data$SalesID),])
test <-  final.data[match(test.IDs,final.data$SalesID),]

write.csv(train,"./data/processed.train.d.csv",row.names=F)
write.csv(test,"./data/processed.test.d.csv",row.names=F)

