#Demonstrate-Stampede Edition (Feb. 2, 2015)
#Original program by: Dustin Landers
#Stampede Edition by: Stephen Talley and Marco Martinez

require(getopt)
require(sciplot)

<<<<<<< HEAD
=======
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
>>>>>>> origin/master
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
for (i in 1:length(all.opts)){
  assign(possibles[[i]],all.opts[i])
}
Demonstrate <- function(dir, AUC.plot.title="Mean AUC By Population Structure and Heritability", MAE.plot.title="Mean MAE By Population Structure and Heritability",
                        herit.strings=list("_03_","_04_","_06_") ,herit.values=list(0.3,0.4,0.6),struct.strings=NULL,struct.values=NULL {
  
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
<<<<<<< HEAD
=======
Filtr.list<-list()
#For loop to append filtered data to new list
for (i in 1:length(dir())){
  Filtr.list[[length(Filtr.list)+1]]<-CleanData(mydata[[i]],"AUC","MAE","Herit","Pop.Structure")
}
mydata<-NULL #Remove original data to save space
MakeAUCPlot(Herit.List, Pop.Struct, Filtr.list, AUC.plot.title)
MakeMAEPlot(Herit.List, Pop.Struct, Filtr.list, MAE.plot.title)
<<<<<<< HEAD
>>>>>>> origin/master
=======
>>>>>>> origin/master
