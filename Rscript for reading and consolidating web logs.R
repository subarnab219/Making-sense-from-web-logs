start.time <- Sys.time()

# for no warinings
options(warn=-1)


# setting inputpath & outputpath
inputpath = "./Data/Log Dumps"
outputpath = "./R/Output"

# setting up libraries
library(stringr)
library(lubridate)

# listing all the raw data file names
list = list.files(inputpath ,pattern=".txt")

# funtion to read the data & structure the data in the desired format
preprocessed = function(x){
  df = read.delim(paste(inputpath,list[x],sep="/"),sep="\t",header=FALSE,fill = TRUE)
  if(length(names(df)) == 1){
    df = as.data.frame(do.call('rbind',strsplit(as.character(df[-1,]),'\t',fixed=TRUE)))
  }
  
  df[,c("V1","V3","V4")] = NULL
  tmp = as.data.frame(do.call('rbind',strsplit(as.character(df$V2),' ',fixed=TRUE)))
  df$date = paste(tmp$V2,tmp$V3,sep=" ")
  df$time = tmp$V4
  df$Year = tmp$V6
  df$V2 = NULL
 
  df$dt = mdy_hms(paste(df$date,df$Year, as.character(df$time)))
  
  tmp = as.data.frame(do.call('rbind',strsplit(as.character(df$V5),'[',fixed=TRUE)))
  df$V5 = NULL
  df$sessionid = gsub("]","",tmp$V5)
  tmp = as.data.frame(do.call('rbind',strsplit(as.character(tmp$V9),' ',fixed=TRUE)))
  df$profileID = gsub("]","",tmp$V1)
  df$code = tmp$V5
  df$serial = tmp$V7
  df$useripaddr = tmp$V9
  df$useragent = tmp$V11
  df$useremail = tmp$V13
  df$status = ifelse(str_sub(tmp$V17,1,13) %in% c("RewardApplied","RewardDenied"),
                     str_sub(tmp$V17,1,13),str_sub(tmp$V18,1,13))
  
  return(df)
}

# appending the data 
tmp0 = preprocessed(1)
for(i in 2:length(list)){
  tx = preprocessed(i)
  tmp0 = rbind(tmp0,tx)
}

# saving the consolidated files
write.csv(tmp0,paste(outputpath,"consolidated.csv",sep = "/"))
save(tmp0,file=paste(outputpath,"consolidated.rda",sep = "/"))

end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

