####Initial Setup----
#Packages needed for TSM
pkgs <- c('raster','rasterVis','rgdal','ndcf4','plotly','pdftools','tidyverse','elevatr','ggridges','reshape2','ggplot2','gridExtra')
install.packages(pkgs)
lapply(pkgs, require, character.only = TRUE)

#install.packages('devtools')
#library(devtools)
#devtools::install_github(build_vignettes = TRUE,repo = "trenchproject/TrenchR",force=TRUE)

##Setting Working Directory
fdir = "C:\\Users\\Bryan\\Desktop\\TSM\\"
setwd(fdir)

####Data----
#list1 <- function(x)unlist(paste("25_shade/TA1cm_soil_25_",x,".nc",sep=""))
#rlist1 <- list1(1:12)

#Animal to be used
org <- c("Uta stansburiana")

###Global lizard data
##Global dataset for species and CTmax/min
globtherm = read.csv("Data\\Temperatures\\GlobalTherm_upload_10_11_17.csv", 
                     header = TRUE, na.strings = "")
ectotherms = subset(globtherm, Class == "Lepidosauria", 
                    select = c(Genus, Species, Class, Tmax, tmin)) #Subsetting for Lizards () w/ Tmax & tmin only
ectotherms$Binomial = paste(ectotherms$Genus, ectotherms$Species) #Pasting together species and genus names to use as factor
attach(ectotherms) #Easier to grab variables

###Species specific data
##Masses for lizards in g
lizards <-read.csv("Data\\Traits\\jzo_696_sm_appendix-s1.csv",header = TRUE, na.strings = "")
mss = function(Binomial){
  return(mass <<- lizards$Weight..g.[lizards$Species==Binomial])
}
mss(org)

##SVL in mm. Source #1
svlf = function(Binomial){
  return(svl <<- lizards$SVL..mm.[lizards$Species==Binomial])
svlf(org)
}

##SVL in mm. Source #2
SVL <- pdf_text("Data\\Traits\\geb_414_sm_apps2.pdf") %>% readr::read_lines(skip=8) #First part not usable so subsetting from after 8 lines
SVL <- as.data.frame(SVL) %>% separate(SVL,c("Family", "Taxon","Name","SVL"),sep="\\s+") #Separating strings. Only 4 and the rest are discarded
SVL[SVL==""]  <- NA #Empty cells to NA
SVL <- na.omit(SVL) #Deleting all empty lines
SVL$Name <- gsub("_", " ", SVL$Name) # Separating the names so they match with "Binomial"
#Extracting SVL
svlf2 = function(Binomial){
  return(svl2 <<- as.numeric(SVL$SVL[SVL$Name==Binomial]))
}
svlf2(org)

##CTmax
ctm = function(Binomial){
  return(ctmax <<- ectotherms$Tmax[ectotherms$Binomial==Binomial])
}
ctm(org)

###Shapefile
shp = function(Binomial){
  return(shape <<- readOGR(dsn="Data\\Ranges\\REPTILES\\Files", Binomial))
}
shp(org)

###NC files
#Variables to locate climate data
substrate <- c("rock","soil","sand")
depth <- c(0,3,5,10,20,30,50,100)
shade <- c(0,25,50,75,90,100)
month <- c(1:12)
hour <- c(1:24)
doy <- list(15,45,75,105,135,165,195,225,255,285,315,345)
doy <- c(15,45,75,105,135,165,195,225,255,285,315,345)
rho_S <- 0.7


##Subtrate temperature
substrate = function(substrate,shade,depth,month){
  ncfile <- unlist(paste("Data\\Microclim\\",shade,"_","shade","\\","D",depth,"cm","_",substrate,"_",
                  shade,"_",month,".nc",sep="",collapse=NULL)) #NC file location for substrate temp
  
  return(Tg <<- lapply(ncfile,brick))
}
substrate("soil",50,0,1:12)

##Air temperature
air = function(substrate,shade,month){
  ncfile <- unlist(paste("Data\\Microclim\\",shade,"_","shade","\\","TA1cm","_",substrate,"_",
                  shade,"_",month,".nc",sep="",collapse=NULL)) #NC file location for substrate temp
  return(Ta <<- lapply(ncfile,brick))
}
air("soil",50,1:12)

##Wind speed
wind = function(x){
  ncfile <- unlist(paste("Data\\Microclim\\wind_speed_ms_1cm\\V1cm_",x,".nc",sep=""))
  return(u <<- lapply(ncfile,brick))
}
wind(1:12)

##Solar Radiation
solrad = function(x){
  ncfile <- unlist(paste("Data\\Microclim\\solar_radiation_Wm2\\SOLR_",x,".nc",sep=""))
  return(sr <<- lapply(ncfile,brick))
}
solrad(1:12)

##Zenith Angle
zenagl = function(x){
  ncfile <- unlist(paste("Data\\Microclim\\zenith_angle\\ZEN_",x,".nc",sep=""))
  return(psi <<- lapply(ncfile,brick))
}
zenagl(1:12)

####Analysis wih function----

##First cropping to avoid dealing with a lot of data - Extent is changed
crp <- function(x)crop(x,extent(shape))

Ta <- lapply(Ta, crp)
Tg <- lapply(Tg, crp)
u <- lapply(u, crp)
sr <- lapply(sr, crp)
psi <- lapply(psi, crp)


##Then masking unwanted values - also possible with extract but same result
mas <- function(x)mask(x,shape)

Ta <- lapply(Ta, mas)
Tg <- lapply(Tg, mas)
u <- lapply(u, mas)
sr <- lapply(sr,mas)
psi <- lapply(psi,mas)

##Elevation using ELEVATR package
elevation <- get_elev_raster(Ta[[1]],z=5)
elevation <- resample(elevation, Ta[[1]], method='bilinear')
elevation <- replicate(12,elevation)

##Runnign the function
Te <- mapply(Tb_lizard,Ta, Tg, u, svl, mass, psi, rho_S, elevation, doy, sun=TRUE, surface=TRUE, alpha_S=0.9, alpha_L=0.965, epsilon_s=0.965, F_d=0.8, F_r=0.5, F_a=0.5, F_g=0.5) 

#Final Substraction
Tsm <- lapply(Te,FUN=function(x){ctmax-x})

###Plotting----

###Adding another layer
Tx <- Ta+1.5 
Te <- mapply(Tb_lizard,Tx, Tg, u, svl, mass, psi, rho_S, elevation, doy, sun=TRUE, surface=TRUE, alpha_S=0.9, alpha_L=0.965, epsilon_s=0.965, F_d=0.8, F_r=0.5, F_a=0.5, F_g=0.5) 



##Changing projection for plotting
ccrs <- function(x)projectRaster(x,crs=crs(shape))
Tsm <- lapply(Tsm, ccrs)
plot(Tsm[[6]],"X1")
plot(shape, add=TRUE)


###Plotting means for the data for 3D plots
fmean <- function(x)cellStats(x,stat='mean')
tsmean <- lapply(Tsm,fmean)

#Turning the lists into an array
tsmean <- do.call(cbind, tsmean)
#tsmean <- t(tsmean)

###Plotting TSM
###Using the ctmax
zero <- array(0, dim=dim(tsmean))

rownames(tsmean) <- c("12 AM","01 AM","02 AM","03 AM","04 AM","05 AM","06 AM","07 AM","08 AM","09 AM","10 AM","11 AM","12 PM","01 PM","02 PM","03 PM","04 PM","05 PM","06 PM","07 PM","08 PM","09 PM","10 PM","11 PM")

colnames(tsmean) <- c("January","February","March","April","May","June","July","August","September","October","November","December")

p <- plot_ly() %>% add_surface(z=~tsmean) %>% add_surface(z=~zero) %>% layout(title="TSM", scene=list(
  xaxis = list(title = "Month"),
  yaxis = list(title = "Hour"),
  zaxis = list(title = "Tsm")))
p #3D graph




###Plotting Te
Tep <- lapply(Te, ccrs)
Tep <- lapply(Tep,fmean)
Tep <- do.call(cbind, Tep)
CTmax <- array(ctmax, dim=dim(tsmean))
p <- plot_ly() %>% add_surface(z=~Tep) %>% add_surface(z=~CTmax) %>% layout(title="TSM", scene=list(
  xaxis = list(title = "Month"),
  yaxis = list(title = "Hour"),
  zaxis = list(title = "Tsm")))
p #3D graph

tsmeandf <- as.data.frame(tsmean)


###Others----
Tb_lizard=function(Ta, Tg, u, svl, mass, psi, rho_S, elevation, doy, sun=TRUE, surface=TRUE, alpha_S=0.9, alpha_L=0.965, epsilon_s=0.965, F_d=0.8, F_r=0.5, F_a=0.5, F_g=0.5){
  
  psi= psi*pi/180 #convert zenith angle to radians
  
  # constants
  sigma=5.67*10^-8 # stefan-boltzman constant, W m^-2 K^-4
  c_p=29.3 # specific heat of air, J/mol degrees K or C
  
  tau=0.65 # atmospheric transmisivity
  S_p0=1360 # extraterrestrial flux density, W/m^2 (p159)
  
  # Calculate radiation
  # view angles, parameterize for animal suspended above ground (p181), on ground- adjust F_e, F_r, and F_g
  h=svl/1000 # length of cylinder in m
  theta = psi # angle between solar beam and a normal to the plane in radians, = psi for horizontal surfaces
  
  # F_p=(cos (theta)+(4*h*sin (theta))/(pi*d))/(2+4*h/d)  # beam view angle, Fig 11.6
  A=0.121*mass^0.688   # total lizard area, roughgarden 1981 from Norris (1965) and Porter and James (1979)
  A_p= (-1.1756810^-4*psi^2-9.2594*10^-2*psi+26.2409)*A/100      # projected area
  F_p=A_p/A
  
  # radiation
  p_a=101.3* exp (-elevation/8200)  # atmospheric pressure
  m_a=p_a/(101.3*cos (psi))  # (11.12) optical air mass
  m_a[(psi>(80*pi/180))]=5.66
  
  # Flux densities
  epsilon_ac= 9.2*10^-6*(Ta+273)^2 # (10.11) clear sky emissivity
  L_a=epsilon_ac*sigma*(Ta+273)^4  # (10.7) long wave flux densities from atmosphere 
  L_g=epsilon_s*sigma*(Tg+273)^4  # (10.7) long wave flux densities from ground
  
  #S_p=S_p0*tau^m_a # (11.11) direct irradience , W/m^2
  dd2= 1+2*0.1675*cos(2*pi*doy/365)
  S_p=S_p0*tau^m_a*dd2 *cos(psi)  #Sears and Angilletta 2012 #dd is correction factor accounting for orbit
  
  S_d=0.3*(1-tau^m_a)* S_p  # (11.13) diffuse radiation
  #S_t=S_p*cos (psi)+S_d # solar irradience 
  S_r= rho_S*S_p # (11.10) reflected radiation
  
  #__________________________________________________
  # conductance
  
  dim=svl/1000 # characteristic dimension in meters (Table 9.5)
  g_r= 4*sigma*(Ta+273)^3/c_p # (12.7) radiative conductance
  
  g_Ha=1.4*0.135*sqrt(u/dim) # boundary conductance, factor of 1.4 to account for increased convection (Mitchell 1976)
  
  #__________________________________________________
  # operative environmental temperature
  
  #calculate with both surface and air temp (on ground and in tree)
  
  sprop=1 #proportion of radiation that is direct, Sears and Angilletta 2012
  R_abs= sprop*alpha_S*(F_p*S_p+ F_d*S_d + F_r*S_r)+alpha_L*(F_a*L_a+F_g*L_g) # (11.14) Absorbed radiation
  Te=Ta+(R_abs-epsilon_s*sigma*(Ta+273)^4)/(c_p*(g_r+g_Ha))                       
  Te_surf= Tg+(R_abs-epsilon_s*sigma*(Tg+273)^4)/(c_p*(g_r+g_Ha))        
  
  # calculate in shade, no direct radiation
  sprop=0 #proportion of radiation that is direct, Sears and Angilletta 2012
  R_abs= sprop*alpha_S*(F_p*S_p+ F_d*S_d + F_r*S_r)+alpha_L*(F_a*L_a+F_g*L_g) # (11.14) Absorbed radiation
  TeS=Ta+(R_abs-epsilon_s*sigma*(Ta+273)^4)/(c_p*(g_r+g_Ha))                       
  TeS_surf=Tg+(R_abs-epsilon_s*sigma*(Tg+273)^4)/(c_p*(g_r+g_Ha))  
  
  #Select Te to return
  if(sun==TRUE & surface==TRUE) Te= Te_surf
  if(sun==TRUE & surface==FALSE) Te= Te
  if(sun==FALSE & surface==TRUE) Te= TeS_surf
  if(sun==FALSE & surface==FALSE) Te= TeS
  
  return(Te) 
}



