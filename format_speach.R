# read in patient id
patient<-read.delim("PatientIDs.txt", sep='\t', heade=TRUE)
# fill in data

# read in SW and WS word 
swws<-read.delim("/Users/zacc/USyd/Kirrie-Cheng-Ramon/word_SW.WS.txt",sep='\t', header=FALSE)

# setwd to .textgri file locations
setwd("/Users/zacc/USyd/Kirrie-Cheng-Ramon/Textgrids_PSWLongitudinalStudy/PNFA Textgrids")

# Read in data (CX_report.txt) and create useable data.frames
path = getwd()
file.names <- dir(path, pattern =".TextGrid")
name1<-data.frame(do.call('rbind', strsplit(as.character(file.names),'.Text',fixed=TRUE)))
name1<-as.character(name1$X1)

# make results matrix
results<-matrix(0,length(name1),4)

# Perform reformatting and median calculations
run1<-c(1:length(file.names))[-19]
#for( Fi in 1:length(file.names))
for( Fi in run1){
  dat<-read.delim(file.names[Fi], sep='\t', header=FALSE) 
  
# convert to matrix of PVI and word_duration measures for each word
dat2<-dat[- grep("!", dat[,1]),]
dat2<-dat2[-c(1:2)]

word1<-dat2[-grep("1 ", dat2)]
words<-substr(word1, start = 1, stop = 3)
word.pos<-substr(word1, start = 4, stop = 10)

vals<-dat[which(dat[,1] %in% word1)-1,]
vals<-data.frame(do.call('rbind', strsplit(as.character(vals)," ",fixed=TRUE)))
vals<-vals[,2:3]
colnames(vals)<-c("value1","value2")

dat3<-cbind(words, word.pos,vals)

# summarize vowel duration for each word (PVI)
dat4<-dat3[grep("V", dat3$word.pos),]
dups<-dat4[duplicated(dat4$words),]
dat4<-dat4[dat4$words %in% dups$words,]

diff1<- as.numeric(as.character(dat4$value2)) -as.numeric(as.character(dat4$value1))

PVI<-1
for (i in 1:length(unique(dat4$words))){
one<-diff1[(i*2-1)]
two<-diff1[(i*2)]
PVI<-cbind(PVI,abs(100*((one-two)/mean(c(one,two)))))
}
PVI<-PVI[-1]

# Calculate word duration
dat5<-dat3[dat3$words %in% dups$words,]

word_duration<-1
for(i in 1:length(unique(dat5$words))){
  w1<-unique(dat5$words)[i]
  d1<-dat5[dat5$words == w1,]
  word_duration<-cbind(word_duration, 
                       c(as.numeric(as.character(d1[nrow(d1),"value2"]))-
                           as.numeric(as.character(d1[1,"value1"]))))
}
word_duration<-word_duration[-1]
word_duration<-word_duration *1000

# create matrix
d1<-cbind(PVI,word_duration)
row.names(d1)<-unique(dat5$words)

#calculate medians for each SW or WS word
mSWpvi<-median(d1[row.names(d1) %in% swws$V1[swws$V3 == "SW"],1])
mWSpvi<-median(d1[row.names(d1) %in% swws$V1[swws$V3 == "WS"],1])
mSWwd<-median(d1[row.names(d1) %in% swws$V1[swws$V3 == "SW"],2])
mWSwd<-median(d1[row.names(d1) %in% swws$V1[swws$V3 == "WS"],2])

# place into results table
results[Fi,1]<-mSWpvi
results[Fi,2]<-mWSpvi
results[Fi,3]<-mSWwd
results[Fi,4]<-mWSwd

}

# write out results
row.names(results)<-name1
colnames(results)<-c("SWpvi.median","WSpvi.median","SWword_duration.median","WSword_duration.median")

# write table to results
write.table(results, file="Group_Results_PVI&WordDuration_Medians.txt", sep='\t')

## match output with phenotype ##
# perform fill on patient
# can we match capitals?
  
