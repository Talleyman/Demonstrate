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

MakeAUCPlot<-function(Her, Pop=NULL, data, AUC.plot.title){
  pdf(file=AUC.plot.title)
  lineplot.CI(Herit, data$AUC, Pop, type="b", 
              main=AUC.plot.title, xlab="Heritability Coefficient", ylab="Mean AUC")
  dev.off()
}
MakeMAEPlot<-function(Her, Pop=NULL, data, MAE.plot.title){
  pdf(file=MAE.plot.title)
  lineplot.CI(Her, data$MAE, Pop, type="b", 
  main=MAE.plot.title, xlab="Heritability Coefficient", ylab="Mean MAE")
  dev.off()
}
#Main function
#First, get command line arguments
args<-commandArgs(TRUE)
options <- matrix(c("dir","d",1,"character"
                    "AUC-plot-title","a",2,"character",
                    "MAE-plot-title","m",2,"character",
                    "herit-string1","H",0,"character",
                    "herit-values1","V",0,"character",
                    "struct-strings1","s",0,"character",
                    "struct-values1","o",0,"character"),
                  ncol=4,byrow=TRUE)
all.opts<-getopt(options,args)
if ( is.null(all.opts$AUC.plot.title ) ){ all.opts$AUC.plot.title = "My AUC Plot" }
if ( is.null(all.opts$MAE.plot.title ) ){ all.opts$MAE.plot.title = "My MAE plot" }

for (i in 1:length(all.opts)){
  assign(possibles[[i]],all.opts[i])
}
mydata<-readFiles(dir)
#Add in columns for heritability and population structure to dataset
for (i in length(mydata)){
  mydata[[i]]$Herit<-NA
  for (j in 1:length(herit.strings)) {
    myData$Herit <- ifelse(sapply(mydata[[i]]$Names,function(x) grepl(herit.strings[[j]],x)),
                           herit.values[[j]],mydata[[i]]$Herit)
  }
}
for (i in length(mydata)){
  mydata[[i]]$Pop.Struct<-NA
  for (j in 1:length(struct.strings)) {
    myData[[i]]$Structure <- ifelse(sapply(mydata[[i]]$Names,function(x) grepl(struct.strings[[j]],x)),
                               struct.values[[j]],mydata[[i]]$Structure)
  }
}
Filtr.list<-list()
#For loop to append filtered data to new list
for (i in 1:length(dir())){
  Filtr.list[[length(Filtr.list)+1]]<-CleanData(mydata[[i]],"AUC","MAE","Herit","Pop.Structure")
}
mydata<-NULL #Remove original data to save space
MakeAUCPlot(Herit.List, Pop.Struct, Filtr.list, AUC.plot.title)
MakeMAEPlot(Herit.List, Pop.Struct, Filtr.list, MAE.plot.title)
