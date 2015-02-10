#Demonstrate-Stampede Edition (Feb. 2, 2015)
#Original program by: Dustin Landers
#Stampede Edition by: Stephen Talley and Marco Martinez

require(getopt)
require(sciplot)

readFiles <- function(dir) {
  setwd(dir)
  files <- (Sys.glob("*.txt"))
  listOfFiles <- lapply(files, function(x) read.table(x, header=TRUE))
  return(listOfFiles)
}

CleanData<-function(data, col1, col2,col3=NULL,col4=NULL,col5=NULL){
  names<-c(col1,col2,col3,col4,col5)
  newData<-data[names]
  return(newData)
}

MakeAUCPlot<-function(Her, Pop=NULL, data, AUC.plot.title="My AUC Plot"){
  pdf(file=AUC.plot.title)
  lineplot.CI(Herit, data$AUC, Pop, type="b", 
              main=AUC.plot.title, xlab="Heritability Coefficient", ylab="Mean AUC")
  dev.off()
}
MakeMAEPlot<-function(Her, Pop=NULL, data, MAE.plot.title="My MAE Plot"){
  pdf(file=MAE.plot.title)
  lineplot.CI(Her, data$MAE, Pop, type="b", 
  main=MAE.plot.title, xlab="Heritability Coefficient", ylab="Mean MAE")
  dev.off()
}
#Main function
args<-commandArgs(TRUE)
options <- matrix(c("dir","dir",1,"character"
                    "AUC-plot-title","a",2,"character",
                    "MAE-plot-title","m",2,"character",
                    "herit-string1","H",0,"character",
                    "herit-values1","H",0,"character",
                    "struct-strings1","s",0,"character",
                    "struct-values1","o",0,"character"),
                  ncol=4,byrow=TRUE)
all.opts<-getopt(options,args)

mydata<-readFiles(dir)
#Add in columns for heritability and population structure to dataset
mydata$Herit<-NA
for (i in 1:length(herit.strings)) {
  newData$Herit <- ifelse(sapply(mydata$Name,function(x) grepl(herit.strings[[i]],x)),
                          herit.values[[i]],newData$Herit)
}
mydata$Pop.Struct<-NA
for (i in 1:length(struct.strings)) {
  newData$Structure <- ifelse(sapply(mydata$Name,function(x) grepl(struct.strings[[i]],x)),
                              struct.values[[i]],newData$Structure)
}
Filtr.list<-list()
#For loop to append filtered data to new list
for (i in 1:length(dir())){
  Filtr.list[[length(Filtr.list)+1]]<-CleanData(mydata[[i]],"AUC","MAE","Herit","Pop.Structure")
}
mydata<-NULL #Clean out the original data set to save some memory
MakeAUCPlot(Herit.List, Pop.Struct, Filtr.list, AUC.plot.title)
MakeMAEPlot(Herit.List, Pop.Struct, Filtr.list, MAE.plot.title)