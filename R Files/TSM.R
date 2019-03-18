####
# CALCULATING TSM FOR NORMAL, +1.5°C & +2°C SCENARIOS
####

### INITIAL SETUP----
## Packages needed for TSM
pkgs <- c('raster','rasterVis','rgdal','ndcf4','plotly','pdftools','tidyverse','elevatr','ggridges','reshape2','ggplot2','gridExtra')
install.packages(pkgs)
lapply(pkgs, require, character.only = TRUE)
#TRYING AGAIN

## Setting Working Directory
fdir = "C:\\Users\\Bryan\\Google Drive\\TSMVisualization\\"
setwd(fdir)


## Te Function
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


### LIZARD DATA----
## Global dataset for species and CTmax/min
globtherm = read.csv("Data\\Traits\\GlobalTherm_upload_10_11_17.csv", header = TRUE, na.strings = "")
ectotherms = subset(globtherm, Class == "Lepidosauria", select = c(Genus, Species, Class, Tmax, tmin)) # Subsetting for Lizards (Lepidosuria) w/ Tmax (& tmin)
ectotherms$Binomial = paste(ectotherms$Genus, ectotherms$Species) # Pasting together species and genus names to use for matching
attach(ectotherms) # Easier to grab "variables". Also to incude renamed species posteriorly


##Organism to be used
org <- c("Ctenotus uber") #Still have to create function to run the entire code


## Shapefile
shp = function(Binomial){
  shape <<- readOGR(dsn="Data\\Ranges\\REPTILES\\Files", Binomial)
  return(crs(shape) <<- crs("+init=epsg:4326"))
}
shp(org)


## Masses for lizards in g
lizards <-read.csv("Data\\Traits\\jzo_696_sm_appendix-s1.csv",header = TRUE, na.strings = "")
mss = function(Binomial){
  return(mass <<- lizards$Weight..g.[lizards$Species==Binomial])
}
mss(org)


## SVL in mm. Source #1
svlf = function(Binomial){
  return(svl <<- lizards$SVL..mm.[lizards$Species==Binomial])
  svlf(org)
}
svlf(org)


## SVL in mm. Source #2
SVL <- pdf_text("Data\\Traits\\geb_414_sm_apps2.pdf") %>% readr::read_lines(skip=8) # First part not usable so subsetting from after 8 lines
SVL <- as.data.frame(SVL) %>% separate(SVL,c("Family", "Taxon","Name","SVL"),sep="\\s+") # Separating strings. Only 4 and the rest are discarded
SVL[SVL==""]  <- NA # Empty cells to NA
SVL <- na.omit(SVL) # Deleting all empty lines
SVL$Name <- gsub("_", " ", SVL$Name) # Separating the names so they match with "Binomial"
# Extracting SVL
svlf2 = function(Binomial){
  return(svl2 <<- as.numeric(SVL$SVL[SVL$Name==Binomial]))
}
svlf2(org)


## CTmax
ctm = function(Binomial){
  return(ctmax <<- ectotherms$Tmax[ectotherms$Binomial==Binomial])
}
ctm(org)



### MICROCLIM DATA----
## Variables to locate climate data
substrate <- c("rock","soil","sand")
depth <- c(0,3,5,10,20,30,50,100)
shade <- c(0,25,50,75,90,100)
month <- c(1:12)
hour <- c(1:24)
doy <- list(15,45,75,105,135,165,195,225,255,285,315,345)
doy <- c(15,45,75,105,135,165,195,225,255,285,315,345)
rho_S <- 0.7


## Subtrate temperature
substrate = function(substrate,shade,depth,month){
  ncfile <- unlist(paste("Data\\Microclim\\",shade,"_","shade","\\","D",depth,"cm","_",substrate,"_",
                         shade,"_",month,".nc",sep="",collapse=NULL)) # NC file location for substrate temp
  
  return(Tg <<- lapply(ncfile,brick))
}
substrate("soil",0,0,1:12)


## Air temperature
air = function(substrate,shade,month){
  ncfile <- unlist(paste("Data\\Microclim\\",shade,"_","shade","\\","TA1cm","_",substrate,"_",
                         shade,"_",month,".nc",sep="",collapse=NULL)) # NC file location for substrate temp
  return(Ta <<- lapply(ncfile,brick))
}
air("soil",0,1:12)


## Wind speed
wind = function(month){
  ncfile <- unlist(paste("Data\\Microclim\\wind_speed_ms_1cm\\V1cm_",month,".nc",sep=""))
  return(u <<- lapply(ncfile,brick))
}
wind(1:12)


## Zenith Angle
zenagl = function(month){
  ncfile <- unlist(paste("Data\\Microclim\\zenith_angle\\ZEN_",month,".nc",sep=""))
  return(psi <<- lapply(ncfile,brick))
}
zenagl(1:12)


## Elevation 
elevation <- get_elev_raster(shape,z=5) #Zoom level can be modified
elevation <- resample(elevation, Ta[[1]], method='bilinear') #Making rasteres match more closely
elevation <- replicate(12,elevation) #The function would not work without a list


### PROCESSING----
## First cropping to avoid dealing with a lot of data - Extent is changed only
crp <- function(x)crop(x,extent(shape))
Ta <- lapply(Ta, crp)
Tg <- lapply(Tg, crp)
u <- lapply(u, crp)
psi <- lapply(psi, crp)
elevation <- lapply(elevation, crp)


## Then masking unwanted values - also possible with extract but same result
mas <- function(x)mask(x,shape)
Ta <- lapply(Ta, mas)
Tg <- lapply(Tg, mas)
u <- lapply(u, mas)
psi <- lapply(psi,mas)
elevation <- lapply(elevation, mas)


###TSM NORMAL SCENARIO----
##Runnign the function
TeN <- mapply(Tb_lizard,Ta, Tg, u, svl, mass, psi, rho_S, elevation, doy, sun=TRUE, surface=TRUE, alpha_S=0.9, alpha_L=0.965, epsilon_s=0.965, F_d=0.8, F_r=0.5, F_a=0.5, F_g=0.5)


##Final Substraction
TsmN <- lapply(TeN,FUN=function(x){ctmax-x})



###TSM +1.5° C SCENARIO----
##Adding the +1.5°C to air and substrate data
Ta1 <- lapply(Ta,FUN=function(x){x+1.5})
Tg1 <- lapply(Tg,FUN=function(x){x+1.5})


##Runnign the function
Te1 <- mapply(Tb_lizard,Ta1, Tg1, u, svl, mass, psi, rho_S, elevation, doy, sun=TRUE, surface=TRUE, alpha_S=0.9, alpha_L=0.965, epsilon_s=0.965, F_d=0.8, F_r=0.5, F_a=0.5, F_g=0.5)


#Final Substraction
Tsm1 <- lapply(Te1,FUN=function(x){ctmax-x})



###TSM +2°C SCENARIO----
##Adding the +2°C to air and substrate data
Ta2 <- lapply(Ta,FUN=function(x){x+2})
Tg2 <- lapply(Tg,FUN=function(x){x+2})


##Runnign the function
Te2 <- mapply(Tb_lizard,Ta2, Tg2, u, svl, mass, psi, rho_S, elevation, doy, sun=TRUE, surface=TRUE, alpha_S=0.9, alpha_L=0.965, epsilon_s=0.965, F_d=0.8, F_r=0.5, F_a=0.5, F_g=0.5)


#Final Substraction
Tsm2 <- lapply(Te2,FUN=function(x){ctmax-x})


















