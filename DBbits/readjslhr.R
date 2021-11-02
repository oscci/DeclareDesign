#read Calder individual sheets and reformat

#NB 30 items of each type
#10 in group1 and 11 in group2

#Here reformatted with item type as column


myfilepath <- "/Users/dorothybishop/Rprojects/DeclareDesign/DBbits/calder/"
myfile <- paste0(myfilepath,"jslhr_ed.csv")
ed <- read.csv(myfile)

myfile <- paste0(myfilepath,"jslhr_3s.csv")
s3<- read.csv(myfile)

myfile <- paste0(myfilepath,"jslhr_poss.csv")
poss <- read.csv(myfile)

alldat <- cbind(ed,s3,poss)

alldat2 <- rbind(ed,s3,poss)

colnames(alldat2)[2:8] <- paste0('T',1:7)

w<-which(alldat2[,1] %in% c('GROUP1','GROUP 2','MEAN','SD'))

alldat2<-alldat2[-w,]

alldat2$group <- 1

gp2 <- c("P3", "P4", "P6", "P7", "P11", "P13", "P15", "P17", "P19", "P20", "P21") 
w<-which(alldat2$GROUP.1 %in% gp2)
alldat2$group[w]<-2

colnames(alldat2)[1]<-'ID'

alldat2$morph <- 'ed'
alldat2$morph[22:42]<-'s3'
alldat2$morph[43:63]<-'poss'

#Making a correction, see below
alldat2$T7[20]<-.1

alldat2[,2:8]<-as.numeric(unlist(alldat2[,2:8]))

alldat2[,2:8]<-alldat2[,2:8]*30
#Originally this revealed an error because% was .1111 for P20 on -ed in final session.
#Not possible with 30 items, do either did not complete all items, or there's a typo here. 
#I have corrected it to .1

#NB also reveals that P17 did not do final 2 sessions. 
#NB this participant was doing much better than the othrs at outset


mymeans <- aggregate(alldat2[,2:8],by=list(alldat2$morph,alldat2$group),FUN=mean, na.rm=T)

#I think P17 should be removed

w<-which(alldat2$ID=='P17')
alldat3<-alldat2[-w,]

mymeans <- aggregate(alldat3[,2:8],by=list(alldat3$morph,alldat3$group),FUN=mean, na.rm=T)
mymeans

nufilename <- 'CalderRaw.csv'
write.csv(alldat3,nufilename,row.names=F)
