#!/usr/bin/Rscript
#Demonstrate-Stampede Edition (Feb. 2, 2015)
#Original program by: Dustin Landers
#Stampede Edition by: Stephen Talley and Marco Martinez

require(getopt)
require(sciplot)

#Main function
#First, get command line arguments
args<-commandArgs(TRUE)
options <- matrix(c("dir","d",1,"character"
                    "AUC-plot-title","a",2,"character",
                    "MAE-plot-title","m",2,"character",
                    "herit.string1","H",1,"character",
                    "herit.string2","h", 1,"character",
                    "herit.string3","r",1,"character"
                    "herit.value1","V",1,"double",
                    "herit.value2","l",1,"double",
                    "herit.value3","u",1,"double",
                    "struct.string1","S",0,"character",
                    "struct.string2","s",0,"character",
                    "struct.value1","O",0,"logical",
                    "struct.value2","o",0,"logical"),
                  ncol=4,byrow=TRUE)
all.opts<-getopt(options,args)
Demonstrate <- function(dir, AUC.plot.title="Mean AUC By Population Structure and Heritability", MAE.plot.title="Mean MAE By Population Structure and Heritability",
                        herit.strings=list("_03_","_04_","_06_") ,herit.values=list(0.3,0.4,0.6),struct.strings=NULL,struct.values=NULL) {
  
  readFiles <- function(dir) {
    setwd(dir)
    files <- (Sys.glob("*.txt"))
    listOfFiles <- lapply(files, function(x) read.table(x, header=TRUE))
    return(listOfFiles)
  }
  CreateLabels<-function(data){
    mapply(cbind, data, "Herit"=NA, SIMPLIFY=FALSE)
    for (j in 1:length(herit.strings)) {
      data$Herit <- ifelse(sapply(data$Names,function(x) grepl(herit.strings[[j]],x)),
                             herit.values[[j]],data$Herit)
    }
    if (struct.strings !=NULL){
      mapply(cbind, mydata, "Struct"=NA, SIMPLIFY=FALSE)
      for (j in 1:length(struct.strings)) {
        data$Structure <- ifelse(sapply(data$Names,function(x) grepl(struct.strings[[j]],x)),
                                 struct.values[[j]],data$Structure)
      }
    }
  }
  MakeAUCPlot<-function(Herit, Struct=NULL, data, AUC.plot.title){
    pdf(file=AUC.plot.title)
    lineplot.CI(data$Herit, data$AUC, data$Struct, type="b", main=AUC.plot.title, 
                xlab="Heritability Coefficient", ylab="Mean AUC")
    dev.off()
  }
  MakeMAEPlot<-function(Her, Struct=NULL, data, MAE.plot.title){
    pdf(file=MAE.plot.title)
    lineplot.CI(data$Herit, data$MAE, data$Struct, type="b", 
                main=MAE.plot.title, xlab="Heritability Coefficient", ylab="Mean MAE")
    dev.off()
  }
  myData<-readFiles(dir)
  newData<-CreateLabels(myData)
  return(newData)
}
Demonstrate(dir=all.opts$dir, all.opts$AUC-plot-title, all.opts$MAE-plot-title, 
            herit.strings=list(all.opts$herit.string1,all.opts$herit.string2,all.opts$herit.string3), 
            herit.values=list(all.opts$herit.value1,all.opts$herit.value2,all.opts$herit.value3),
            struct.strings=list(all.opts$struct.string1,all.opts$struct.string2),
            struct.values=list(all.opts$struct.value1,all.opts$struct.value2))