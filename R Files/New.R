#New Visualizations using ggplot2

###Trying with ggplot2----
df <- as.data.frame(Tsm[[7]], xy=TRUE)
df <- melt(df,id=c("x","y"))

##Adding other ggplot arguments
#Adding points. NOT USEFUL but added vline
ggplot(df,aes(value,variable)) + geom_point()+ geom_vline(aes(xintercept = 0)) 
#Adding bins?
g+  geom_bin2d(bins = 20) + geom_vline(aes(xintercept = 0)) #THE INTERCEPT TAKES LONGER TIME TO RUN
#Adding density stacked -position = "stack"/"fill"-
ggplot(df, aes(value, fill = variable)) +
  geom_density(alpha = 0.1)
ggplot(df, aes(value, fill = variable)) +
  geom_density(position="stack",alpha = 0.5)


###Other tests----
#real tests

load("Tsm.RData")
load("Te.RData")
load("Ctmax.RData")
Tsm10 <- Tsm[[10]]
Tsm7 <- Tsm[[7]]
Tsm4 <- Tsm[[4]]
#Can also use Raster to Points. or gplot_data
Tsm7df <- as.data.frame(Tsm7, xy=TRUE,na.rm=TRUE)
Tsm4df <- as.data.frame(Tsm4, xy=TRUE,na.rm=TRUE)
Tsm10df <- as.data.frame(Tsm10, xy=TRUE,na.rm=TRUE)
colnames(Tsm7df) <- c("x","y", "12 AM","01 AM","02 AM","03 AM","04 AM","05 AM","06 AM","07 AM","08 AM","09 AM","10 AM","11 AM","12 PM","01 PM","02 PM","03 PM","04 PM","05 PM","06 PM","07 PM","08 PM","09 PM","10 PM","11 PM")
colnames(Tsm4df) <- c("x","y","12 AM","01 AM","02 AM","03 AM","04 AM","05 AM","06 AM","07 AM","08 AM","09 AM","10 AM","11 AM","12 PM","01 PM","02 PM","03 PM","04 PM","05 PM","06 PM","07 PM","08 PM","09 PM","10 PM","11 PM")
colnames(Tsm10df) <- c("x","y","12 AM","01 AM","02 AM","03 AM","04 AM","05 AM","06 AM","07 AM","08 AM","09 AM","10 AM","11 AM","12 PM","01 PM","02 PM","03 PM","04 PM","05 PM","06 PM","07 PM","08 PM","09 PM","10 PM","11 PM")



Tsm10df <- melt(Tsm10df, id=c("x","y"))
Tsm7df <- melt(Tsm7df, id=c("x","y"))
Tsm4df <- melt(Tsm4df, id=c("x","y"))

Tsm7df$Scenario <- "+2.0 C"
Tsm4df$Scenario <- "+1.5 C"
Tsm10df$Scenario <- "Normal"

colnames(Tsm10df) <- c("x","y","Hour","Tsm","Scenario")
colnames(Tsm7df) <- c("x","y","Hour","Tsm","Scenario")
colnames(Tsm4df) <- c("x","y","Hour","Tsm","Scenario")

Tsm7df$Scenario <- as.factor(Tsm7df$Scenario)
Tsm4df$Scenario <- as.factor(Tsm4df$Scenario)
Tsm10df$Scenario <- as.factor(Tsm10df$Scenario)


#This is my combined plot with multiple scenarios to plot with ggplot2
final <- rbind(Tsm10df,Tsm4df,Tsm7df)


#Trying again with ggplot2
#This one produces a single graph with different colors
g <- ggplot(final,aes(x=Tsm,y=Hour)) + geom_density_ridges2(aes(colour=Scenario, fill=Scenario,                                                           alpha=0.9),rel_min_height = 0.01 ,scale=2)+ theme_bw() #Works

g

#Visualization
h <- ggplot(map4, aes(x, y)) + geom_raster(aes(fill = X10))
i <- ggplot(map7, aes(x, y)) + geom_raster(aes(fill = X10))
j <- ggplot(map10, aes(x, y)) + geom_raster(aes(fill = X10))





















grid.arrange(g, h, i,j, nrow = 2, heights = c(3,1), layout_matrix = rbind(c(1), c(2, 3,4)))


install.packages('ggmap')
library(ggmap)



box <- bbox(shape)
terrain <- ggmap(get_map(location = box))

test <- terrain + geom_raster(data = map7, aes(x = x, y = y, fill = X10))
























plot(terrain)
plot(Tsm[[4]],"X10", add=TRUE)


vignette("ggplot2-specs")



dev.off()








#This one makes 2 graphs with different plot spaces
ggplot(final,aes(x=Tsm,y=Hour)) + geom_density_ridges2() + facet_wrap(~Scenario) #Works

#rel_min_height cuts off the tails and cale sizes them to desire
#+ facet_wrap(~Species) helps to have separate plots per category MAYBE TO USE WITH MONTH 


###TRYING NEW THINGS

install.packages('gridExtra')
library(ggspatial)
#Using raster
ggplot() + geom_spatial(data = shape, fill = NA, colour = "black") +
  theme_void() +
  coord_map()

#USEFUL FOR ANIMATION VISUALIZATION
animate(Tsm[[7]])
plot(Tsm[[7]],"X10")

###STARTING OVER AGAIN----
install.packages('rts')
library(rts)

Tsm

  
  
Tsmm <- as.matrix(Tsm)
Tsm7m <- as.matrix(Tsm7)

xres(Tsm7)
yres(Tsm7)
plot(Tsm7)
names(Tsm7)

#To change raster names for attribute plotting
rasterNames  <- gsub("X","", names(Tsm7))
















