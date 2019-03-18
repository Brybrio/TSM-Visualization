####
# PLOTTING TSMs WITH GGPLOT
####
## Month to be used
month <- 1:12
names(month) <- c("January","February","March","April","May","June","July","August","September","October","November","December")


### Transforming raster data to ggplot format----
## To individual data frames, maintaining coordinates for plotting
Tsmtodf = function(x){
  # Normal Scenario
  TsmNdf <<- as.data.frame(TsmN[[x]], xy=TRUE,na.rm=TRUE)
  colnames(TsmNdf) <<- c("x","y", "12 AM","01 AM","02 AM","03 AM","04 AM","05 AM","06 AM","07 AM","08 AM","09 AM","10 AM","11 AM","12 PM","01 PM","02 PM","03 PM","04 PM","05 PM","06 PM","07 PM","08 PM","09 PM", "10 PM","11 PM")
  TsmNdf <<- melt(TsmNdf, id=c("x","y"))
  TsmNdf$Scenario <<- "Normal"
  TsmNdf$Month <<- x

  # +1.5° C Scenario
  Tsm1df <<- as.data.frame(Tsm1[[x]], xy=TRUE,na.rm=TRUE)
  colnames(Tsm1df) <<- c("x","y", "12 AM","01 AM","02 AM","03 AM","04 AM","05 AM","06 AM","07 AM","08 AM","09 AM","10 AM","11 AM","12 PM","01 PM","02 PM","03 PM","04 PM","05 PM","06 PM","07 PM","08 PM","09 PM", "10 PM","11 PM")
  Tsm1df <<- melt(Tsm1df, id=c("x","y"))
  Tsm1df$Scenario <<- "+1.5° C"
  Tsm1df$Month <<- x
  
  # +2° C Scenario
  Tsm2df <<- as.data.frame(Tsm2[[x]], xy=TRUE,na.rm=TRUE)
  colnames(Tsm2df) <<- c("x","y", "12 AM","01 AM","02 AM","03 AM","04 AM","05 AM","06 AM","07 AM","08 AM","09 AM","10 AM","11 AM","12 PM","01 PM","02 PM","03 PM","04 PM","05 PM","06 PM","07 PM","08 PM","09 PM", "10 PM","11 PM")
  Tsm2df <<- melt(Tsm2df, id=c("x","y"))
  Tsm2df$Scenario <<- "+2° C"
  Tsm2df$Month <<- x
  
  Tsmdf <<- rbind(TsmNdf,Tsm1df,Tsm2df)
  colnames(Tsmdf) <<- c("x","y","Hour","Tsm","Scenario","Month")
  
  return(Tsmdf)
}
Tsmdf <- lapply(month,Tsmtodf)



#NOW SUBSETTING TO KEEP WORKING


Tsmdfr <- Tsmdf[[8]]



### Plotting----
##First density plot
g <- ggplot(Tsmdfr,aes(x=Tsm,y=Hour,alpha=0.1)) + geom_density_ridges2(aes(fill=Scenario),rel_min_height = 0.01 ,scale=2)+ theme_bw() + ggtitle(paste(org))
g



ggplot(Tsmdfr,aes(x=Tsm,y=Hour,alpha=0.1))+geom_bin2d(bins = 20) + theme_bw() + ggtitle(paste(org))
#ANOLIS OCULATUS



plot(TsmN[[1]],"X16")
plot(TsmN[[6]],"X1")
plot(shape,add=TRUE)




