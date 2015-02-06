#Demonstrate-Stampede Edition (Feb. 2, 2015)
#Original program by: Dustin Landers
#Stampede Edition by: Stephen Talley and Marco Martinez (here)

require(getopt)
require(sciplot)

readFiles <- function(dir) {
  setwd(dir)
  files <- (Sys.glob("*.txt"))
  listOfFiles <- lapply(files, function(x) read.table(x, header=TRUE))
  return(listOfFiles)
}

CleanData<-function(data){
  names<-list("AUC","MAE","H","H2")
  for (file in data){
    for (i in names(file)){
      if ([[i]] not in names){
        file[[i]]<-NULL
      }
    }
  }
  return(data)
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
