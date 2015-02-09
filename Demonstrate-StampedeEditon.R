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

CleanData<-function(data, column1, column2){
  names<-c(column1,column2)
  newData<-data[names]
  return(newData)
}

MakeAUCPlot<-function(data, AUC.plot.title="My AUC Plot"){
  pdf(file=AUC.plot.title)
  lineplot.CI(data$Herit, data$AUC, data$Pop.Structure, type="b", 
              main=AUC.plot.title, xlab="Heritability Coefficient", ylab="Mean AUC")
  dev.off()
}
MakeMAEPlot<-function(data, MAE.plot.title="My MAE Plot"){
  pdf(file=MAE.plot.title)
  lineplot.CI(data$Herit, data$MAE, data$Pop.Structure, type="b", 
  main=MAE.plot.title, xlab="Heritability Coefficient", ylab="Mean MAE")
  dev.off()
}

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
