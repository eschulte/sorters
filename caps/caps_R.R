#Run with Rscript caps_R.R > /dev/null
library(multicore)
cap_line <- function(line){
sline<-strsplit(line," ", fixed=TRUE,useBytes=TRUE)[[1]]
return(paste(toupper(substring(sline,1,1)),substring(sline,2),sep="",collapse=" "))}

fn <- '/tmp/file';
data<-readChar(fn,file.info(fn)$size,useBytes=TRUE);
split_data <- strsplit(data,"\n",fixed=TRUE,useBytes=TRUE)[[1]];
cs <- paste(mclapply(split_data,cap_line,mc.cores=12),sep='',collapse='\n')
cat(cs)
