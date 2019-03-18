####USING GGPLOT2----
pkgs2 <- c('ggridges','reshape2','ggplot2')
install.packages(pkgs2)
lapply(pkgs2, require, character.only = TRUE)

load("Tsm.RData")
load("Te.RData")
load("Ctmax.RData")

plot(Tsm[[4]])
plot(Tsm[[4]],"X12")

plot(Te[[4]])
plot(Te[[4]],"X12")

#Converting to data frame
Tsma <- as.data.frame(Tsm[[5]])
colnames(Tsma) <- c("00","01","02","03","04","05","06","07","08","09","10","11","12",
                    "13","14","15","16","17","18","19","20","21","22","23")

#First stacked density plot with ggplot2
Tsma1 <- melt(Tsma)
colnames(Tsma1) <- c("Hour","Tsm")
p <- ggplot(Tsma1,aes(x=Tsm,y=Hour,height=..density..))+ geom_density_ridges()
p


#Second graph using bins - STILL NEED TO ADJUST BINS - DOESN'T WORK
q <- ggplot(Tsma1,aes(x=Tsm,y=Hour,height=1))+ geom_density_ridges(stat = "binline" ,breaks=c(-20,0,1,3),scale = 0.95) 
#Scale affects whether they touch
q

#Third graph????? NOPE
head(Tsma)
breaks <- c(-50,0,1,3,50) # set up boundaries for intervals/bins
labels <- c("<0", "0:1", "1:3", ">3") # specify interval/bin labels
# bucketing data points into bins
bins <- cut(Tsma[,1], breaks, include.lowest = T, right=FALSE, labels=labels, na.rm=TRUE) # bucketing data points into bins



#Violin Plot
p <- ggplot(Tsma1,aes(x=Hour,y=Tsm))+geom_violin(trim = FALSE)
p

#Boxplot
p <- ggplot(Tsma1,aes(x=Hour,y=Tsm))+ geom_boxplot()
p


###############USING RASTER DATA ----

#Isolating one layer only from "Tsm"
Tsm1 <- Tsm[[5]]

#First method still does not work
cell_size<-area(Tsm1)
#delete NAs from vector of all raster cells
##NAs lie outside of the rastered region, can thus be omitted
cell_size<-cell_size[!is.na(cell_size)]
#compute area [km2] of all cells in geo_raster
raster_area<-length(cell_size)*median(cell_size)
#print area of Georgia according to raster object
print(paste("Area of SHape",round(raster_area, digits=1),'km2'))


library(rgeos)

crs(shape) <- crs(Tsm[[1]])

plot(shape)
plot(Tsm[[1]])

gArea(shape)
crs(shape) <- c("+init=epsg:4326 +proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

require(geosphere)
areaPolygon(shape)


area(Tsm[[1]],weights=TRUE)
area(shape)


q <- round(area(Tsm[[1]]) / 10000000,1)
p <- area(Tsm[[1]], na.rm=TRUE) 
p
area(shape)
Tsm1



###Obtaining the histograms with specified breaks. Can't get the list function to work yet

perc <- list(hist(Tsm1[[1]],breaks=c(-200, 0,1,2, 200),plot=FALSE)$counts,hist(Tsm1[[2]],breaks=c(-200, 0,1,2, 200),plot=FALSE)$counts,hist(Tsm1[[3]],breaks=c(-200, 0,1,2, 200),plot=FALSE)$counts,hist(Tsm1[[4]],breaks=c(-200, 0,1,2, 200),plot=FALSE)$counts,hist(Tsm1[[5]],breaks=c(-200, 0,1,2, 200),plot=FALSE)$counts,hist(Tsm1[[6]],breaks=c(-200, 0,1,2, 200),plot=FALSE)$counts,hist(Tsm1[[7]],breaks=c(-200, 0,1,2, 200),plot=FALSE)$counts,hist(Tsm1[[8]],breaks=c(-200, 0,1,2, 200),plot=FALSE)$counts,hist(Tsm1[[9]],breaks=c(-200, 0,1,2, 200),plot=FALSE)$counts,hist(Tsm1[[10]],breaks=c(-200, 0,1,2, 200),plot=FALSE)$counts,hist(Tsm1[[11]],breaks=c(-200, 0,1,2, 200),plot=FALSE)$counts,hist(Tsm1[[12]],breaks=c(-200, 0,1,2, 200),plot=FALSE)$counts,hist(Tsm1[[13]],breaks=c(-200, 0,1,2, 200),plot=FALSE)$counts,hist(Tsm1[[14]],breaks=c(-200, 0,1,2, 200),plot=FALSE)$counts,hist(Tsm1[[15]],breaks=c(-200, 0,1,2, 200),plot=FALSE)$counts,hist(Tsm1[[16]],breaks=c(-200, 0,1,2, 200),plot=FALSE)$counts,hist(Tsm1[[17]],breaks=c(-200, 0,1,2, 200),plot=FALSE)$counts,hist(Tsm1[[18]],breaks=c(-200, 0,1,2, 200),plot=FALSE)$counts,hist(Tsm1[[19]],breaks=c(-200, 0,1,2, 200),plot=FALSE)$counts,hist(Tsm1[[20]],breaks=c(-200, 0,1,2, 200),plot=FALSE)$counts,hist(Tsm1[[21]],breaks=c(-200, 0,1,2, 200),plot=FALSE)$counts,hist(Tsm1[[22]],breaks=c(-200, 0,1,2, 200),plot=FALSE)$counts,hist(Tsm1[[23]],breaks=c(-200, 0,1,2, 200),plot=FALSE)$counts,hist(Tsm1[[24]],breaks=c(-200, 0,1,2, 200),plot=FALSE)$counts)

#TRYING TO USE A FUNCTION BUT UNABLE YET. CHECK THIS!!!
histo <- function(x){
  temp <- c(hist(x,breaks=c(-200, 0,1,2, 200),plot=FALSE)$counts)
  return(temp)
  }

lapply(Tsm1,histo)

#Probably has to do with rasater file format
##Now using earlier list

#Transforming to list and percentages
perc <- lapply(perc,FUN=function(x){x*100/sum(x)})

#Converting the list of lists to a data frame
df <- data.frame(matrix(unlist(perc), nrow=length(perc), byrow=T),stringsAsFactors=FALSE)

#Now creating the ??? plot
#transforming data frame to matrix
mat <- data.matrix(df, rownames.force = NA)

  
image(1:nrow(mat), 1:ncol(mat), as.matrix(mat))

rm(colnames(mat))


install.packages('plotrix')
library(plotrix)
color2D.matplot(mat)
matrixplot(mat)


##COlor coding for heat map
x = 1:ncol(df)
y = 1:nrow(df)
centers <- expand.grid(y,x)
image(x, y, t(df))

cols4 <- c()


text(centers[,2], centers[,1], c(df), col= "black")
heatmap.2(df = mm, Rowv = FALSE, Colv = FALSE, dendrogram = "none",
          cellnote = mm, notecol = "black", notecex = 2,
          trace = "none", key = FALSE, margins = c(7, 11))
#This is plotrix
color2D.matplot(df, 
                show.values = TRUE,
                axes = FALSE,
                xlab = "",
                ylab = "",
                vcex = 2,
                vcol = "black",
                extremes = c("red", "yellow"))


A <- matrix(c(2,5,2,1,0,0,0,0,1,0,0,0,0,1,3,5,6,0,0,1,0,0,0,2,0,0,1,2,7,2,4,6,2,5,1,0,0,1,0,0,0,1,0,0,3,5,4,0,0,1,0,0,1,0,0,2,0,3,5,7,3,1,4,0,1,0,0,0,0,2,0,0,0,1,3,4,6,0,0,1), byrow=T, nrow=8, ncol=10)
colnames(A) <- letters[1:10]
rownames(A) <- LETTERS[1:8]
print(A)
longData<-melt(A)
print(longData)
longData$Var1 <- factor(longData$Var1, levels=unique(arrange(longData, Var1_clust)[,1]))

test <- as.matrix(Tsm1)




#PICKING COLORS FOR THE "IMAGE" GRAPH. ONE FOR EACH COLUMN
color.picker1 <- function(z){
  if(z <= 1){return("red")}
  else if( z > 1 & z <= 100){return("red")}
  else {return("black")}
}

color.picker2 <- function(z){
  if(z >= 50){return("red")}
  else if( z > 0 & z < 50){return("yellow")}
  else {return("black")}
}

color.picker3 <- function(z){
  if(z >= 50){return("yellow")}
  else if( z > 0 & z < 50){return("green")}
  else {return("black")}
}

color.picker4 <- function(z){
  if(z <= 1){return("green")}
  else if( z > 1 & z <= 100){return("green")}
  else {return("black")}
}

fill.data <- df
fill.data[,1:3] <- matrix(NA, nrow = nrow(df), ncol = ncol(df) - 1)
image(x, y, t(fill.data))
text.cols <- sapply(fill.data[,4], color.picker4)
text(centers[,2], centers[,1], format(c(fill.data),nsmall = 1), col= text.cols)

library(lattice)

###NOW TRYING HEAT MAPS
levelplot(mat)
contourplot(mat)
contourplot(mat, data = NULL, aspect = "iso", xlim, ylim,
            row.values = seq_len(nrow(x)),
            column.values = seq_len(ncol(x)))
heatmap(mat, Rowv=NA, Colv=NA)
heatmap.2(mat, dendrogram = "none")

##NOW AGAIN WITH GGPLOT2
ggplot(mat)
ggplot(Tsma1, aes(Hour, Tsm,fill=Tsm)) + geom_raster()














