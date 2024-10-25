install.packages("spatialEco")
install.packages("BAMMtools")
install.packages("raster")
library("sf")
library("tmap")
library("raster")
library("sp")
library("spatialEco")
library("BAMMtools")

setwd("C:/Users/ACER/Documents/Term 1-SGDS-UCL/GEOG0114-Principles of Spatial Analysis/Week 4/Week 4 - Dataset/Week 4 - Dataset")

temp <- raster("Kenya Mean Teperature.tif")
nvdi <- raster("Kenya NDVI.tif")
prec <- raster("Kenya Precipitation.tif")
popl <- raster("Kenya Population Density.tif")
elev <- raster("Kenya Elevation.tif")
arid <- raster("Kenya Aridity Index.tif")

kenya_border <- st_read("Kenya_Border_3857.shp")
kenya_states <-st_read("Kenya_States_3857.shp")

tm_shape(temp)+tm_raster(style="cont", title="Mean Temperature", palette="-Spectral")+
  tm_shape(kenya_states)+tm_polygons(alpha=0, border.col="black")+
  tm_layout(frame=FALSE, legend.outside=TRUE)

temp
temp_cl <- c(0, 15, 0, 15, 30, 1)
temp_cl_mat <- matrix(temp_cl, ncol=3, byrow=TRUE)
temp_cl_mat
temp_recl<- reclassify(temp, temp_cl_mat)
temp_recl

tm_shape(temp_recl)+tm_raster(style="cat", title="Temperature", palette=c("grey", "#F1948A"), labels=c("Unsuitable (<15.0)", "Suitable (15 & above)"))+
  tm_shape(kenya_states)+tm_polygons(alpha=0, border.col="black")+
  tm_layout(frame=FALSE, legend.outside=TRUE)

elev
elev_cl<-c(-12,1199.999,1,1199.999,4900,0)
elev_cl_mat<-matrix(elev_cl,ncol=3,byrow=TRUE)
elev_cl_mat
elev_recl<-reclassify(elev, elev_cl_mat)
elev_recl

tm_shape(elev_recl)+tm_raster(style="cat",  title="Elevation", palette=c("grey", "orange"), labels=c("Unsuitable (>1200m)", "Suitable (1200m & below"))+
  tm_shape(kenya_states) + tm_polygons(alpha=0, border.col="black")+
  tm_layout(frame=FALSE, legend.outside=TRUE)

nvdi
nvdi_cl<-c(-1,0.5,0,0.5,0.9,1)
nvdi_cl_mat<-matrix(nvdi_cl, ncol=3, byrow=TRUE)
nvdi_cl_mat
nvdi_recl<-reclassify(nvdi, nvdi_cl_mat)

tm_shape(nvdi_recl) + tm_raster(style="cat", title="NVDI (Vegetation)", palette=c("grey", "green"), labels=c("Unsuitable (0.5 & Below)", "Suitable (>0.5)"))+
  tm_shape(kenya_states)+tm_polygons(alpha=0, border.col="black")+
  tm_layout(frame=FALSE, legend.outside=TRUE)

prec
prec_cl<-c(0,350,0,350,2700,1)
prec_cl_mat<-matrix(prec_cl,ncol=3,byrow=TRUE)
prec_cl_mat
prec_recl<-reclassify(prec, prec_cl_mat)

tm_shape(prec_recl)+tm_raster(style="cat", title="Precipitation (mm)", palette=c("grey", "skyblue"), labels=c("Unsuitable (350mm & Below)", "Suitable (>350mm)"))+
  tm_shape(kenya_states) + tm_polygons(alpha=0, border.col="black")+tm_layout(frame=FALSE, legend.outside=TRUE)

popl
popl_cl<-c(-1,0,0,0,126300,1)
pop_cl_mat<-matrix(popl_cl, ncol=3, byrow=TRUE)
pop_cl_mat
popl_recl<-reclassify(popl, pop_cl_mat)

tm_shape(popl_recl)+tm_raster(style="cat", title="Population Density", palette=c("grey", "orange"), labels=c("Unsuitable (0 people)", "Suitable (at least 1 person)"))+
  tm_shape(kenya_states) + tm_polygons(alpha=0, border.col="black")+
  tm_layout(frame=FALSE, legend.outside = TRUE)

arid
arid_cl<-c(0, 0.20, 0, 0.20, 3, 1)
arid_cl_mat<-matrix(arid_cl, ncol=3, byrow=TRUE)
arid_cl_mat
arid_recl<-reclassify(arid,arid_cl_mat)

tm_shape(arid_recl)+tm_raster(style="cat", title="Aridity (Dryness)",palette= c("grey", "orange"), labels=c("Unsuitable (0.2 & Below)", "Suitable (>0.2)"))+
  tm_shape(kenya_states)+tm_polygons(alpha=0, border.col="black")+
  tm_layout(frame=FALSE, legend.outside = TRUE)

Suitable_LF_Binary<-temp_recl*nvdi_recl*prec_recl*elev_recl*popl_recl*arid_recl
  
tm_shape(Suitable_LF_Binary)+tm_raster(style="cat", title="", palette=c("#f0f0f0", "red"), labels=c("Zone: Not Suitable", "Zone: High Suitable"))+
  tm_shape(kenya_states)+tm_polygons(alpha=0, border.col="black")+tm_text("NAME_1", size="AREA")+
  tm_layout(frame=FALSE, legend.outside=TRUE)

RasterStack<-stack(temp_recl, nvdi_recl, prec_recl, elev_recl, popl_recl, arid_recl)
Suitable_LF_Summed <- calc(RasterStack, sum)
Suitable_LF_Summed@data@min
Suitable_LF_Summed@data@max

tm_shape(Suitable_LF_Summed)+tm_raster(style="cat", title="Suitability Score", palette=c("#FDFEFE", "#FADBD8", "#F5B7B1", "#F1948A", "#E74C3C"), labels=c("Low(2)", "Modest(3)", "Medium (4)", "High (5)", "Highest (6)"))+
  tm_shape(kenya_states)+tm_polygons(alpha=0, border.col="black")+tm_text("NAME_1", size="AREA")+
  tm_layout(frame=FALSE, legend.outside=TRUE)

tempvalues <- values(temp)
tempdf<-as.data.frame(tempvalues)
tempdf<-as.data.frame(tempdf[!is.na(tempdf$tempvalues),])
colnames(tempdf) <- "tempvalues"
tempjenks<-getJenksBreaks(tempdf$tempvalues, 10, subset=nrow(tempdf)*0.10)

tempjenks
tempjenks[1]
tempjenks[2]

temp_jenks_cl <- c(temp@data@min-1, tempjenks[1], 1,
                   tempjenks[1], tempjenks[2], 2,
                   tempjenks[2], tempjenks[3], 3,
                   tempjenks[3], tempjenks[4], 4,
                   tempjenks[4], tempjenks[5], 5,
                   tempjenks[5], tempjenks[6], 6,
                   tempjenks[6], tempjenks[7], 7,
                   tempjenks[7], tempjenks[8], 8,
                   tempjenks[8], tempjenks[9], 9,
                   tempjenks[9], temp@data@max+1, 10)

temp_jenks_cl
temp_jenks_cl_mat<-matrix(temp_jenks_cl, ncol=3, byrow=TRUE)
temp_jenks_cl_mat
temp_jenks_recl<-reclassify(temp, temp_jenks_cl_mat)

tm_shape(temp_jenks_recl)+tm_raster(style="cont", title="Temp (on Jenks scale)", palette="-Spectral")+
  tm_shape(kenya_states)+tm_polygons(alpha=0, border.col="black")+
  tm_layout(frame=FALSE, legend.outside=TRUE)

# 2 prec
# Extract values from Raster
precValues <- values(prec) 
# Change the values from vector object to data.frame object
precDF <- as.data.frame(precValues)
# Remove missing values and reapply column name
precDF <- as.data.frame(precDF[!is.na(precDF$precValues),])
colnames(precDF) <- "precValues"
# Use the getJenksBreaks() function. Sample 0.10 (10%) of the pixels at random and base the categorisation on this. 
# NOTE: Doing this on the full data will take forever - so use the subset argument. 
precJenks <- getJenksBreaks(precDF$precValues, 10, subset = nrow(precDF)*0.10)
# See value in vector
precJenks
# shows first element
precJenks[1] 
# shows second element
precJenks[2] 
# so on and so further...
# Create categorisation by using the Jenks values in the vector
prec_jenks_cl <- c(prec@data@min-1, precJenks[1], 1,
                   precJenks[1], precJenks[2], 2,
                   precJenks[2], precJenks[3], 3,
                   precJenks[3], precJenks[4], 4,
                   precJenks[4], precJenks[5], 5,
                   precJenks[5], precJenks[6], 6,
                   precJenks[6], precJenks[7], 7,
                   precJenks[7], precJenks[8], 8,
                   precJenks[8], precJenks[9], 9,
                   precJenks[9], prec@data@max+1, 10) 
# create matrix
prec_jenks_cl_mat <- matrix(prec_jenks_cl, ncol = 3, byrow = TRUE)
# view categorisation in matrix
prec_jenks_cl_mat
# reclassify original raster using the jenks classifications
prec_jenks_recl <- reclassify(prec, prec_jenks_cl_mat)


# 3. popl
# Extract values from Raster
poplValues <- values(popl)
# Change the values from vector object to data.frame object
poplDF <- as.data.frame(poplValues)
# Remove missing values and reapply column name
poplDF <- as.data.frame(poplDF[!is.na(poplDF$poplValues),])
colnames(poplDF) <- "poplValues"
# Use the getJenksBreaks() function. Sample 0.10 (10%) of the pixels at random and base the categorisation on this. 
# NOTE: Doing this on the full data will take forever - so use the subset argument. 
poplJenks <- getJenksBreaks(poplDF$poplValues, 10, subset = nrow(poplDF)*0.10)
# See value in vector
poplJenks
# shows first element
poplJenks[1] 
# shows second element
poplJenks[2] 
# so on and so further...
# Create categorisation by using the Jenks values in the vector
popl_jenks_cl <- c(popl@data@min-1, poplJenks[1], 1,
                   poplJenks[1], poplJenks[2], 2,
                   poplJenks[2], poplJenks[3], 3,
                   poplJenks[3], poplJenks[4], 4,
                   poplJenks[4], poplJenks[5], 5,
                   poplJenks[5], poplJenks[6], 6,
                   poplJenks[6], poplJenks[7], 7,
                   poplJenks[7], poplJenks[8], 8,
                   poplJenks[8], poplJenks[9], 9,
                   poplJenks[9], popl@data@max+1, 10) 
# create matrix
popl_jenks_cl_mat <- matrix(popl_jenks_cl, ncol = 3, byrow = TRUE)
# view categorisation in matrix
popl_jenks_cl_mat
# reclassify original raster using the jenks classifications
popl_jenks_recl <- reclassify(popl, popl_jenks_cl_mat)


# 4 nvdi
# Extract values from Raster
nvdiValues <- values(nvdi) 
# Change the values from vector object to data.frame object
nvdiDF <- as.data.frame(nvdiValues)
# Remove missing values and reapply column name
nvdiDF <- as.data.frame(nvdiDF[!is.na(nvdiDF$nvdiValues),])
colnames(nvdiDF) <- "nvdiValues"
# Use the getJenksBreaks() function. Sample 0.10 (10%) of the pixels at random and base the categorisation on this. 
# NOTE: Doing this on the full data will take forever - so use the subset argument. 
# EXTRA NOTE: The values for nvdi are very close to each other and so the algorithm splits it to just two cateogries
nvdiJenks <- getJenksBreaks(nvdiDF$tempValues, 2, subset = nrow(nvdiDF)*0.10)
# See value in vector
nvdiJenks
# shows first element
nvdiJenks[1] 
# shows second element
nvdiJenks[2] 
# so on and so further...
# Create categorisation by using the Jenks values in the vector
nvdi_jenks_cl <- c(nvdi@data@min-1, nvdiJenks[1], 1,
                   nvdiJenks[1], nvdi@data@max+1, 2)
# create matrix
nvdi_jenks_cl_mat <- matrix(nvdi_jenks_cl, ncol = 3, byrow = TRUE)
# view categorisation in matrix
nvdi_jenks_cl_mat
# reclassify original raster using the jenks classifications
nvdi_jenks_recl <- reclassify(nvdi, nvdi_jenks_cl_mat)# 2 prec
# Extract values from Raster
precValues <- values(prec) 
# Change the values from vector object to data.frame object
precDF <- as.data.frame(precValues)
# Remove missing values and reapply column name
precDF <- as.data.frame(precDF[!is.na(precDF$precValues),])
colnames(precDF) <- "precValues"
# Use the getJenksBreaks() function. Sample 0.10 (10%) of the pixels at random and base the categorisation on this. 
# NOTE: Doing this on the full data will take forever - so use the subset argument. 
precJenks <- getJenksBreaks(precDF$precValues, 10, subset = nrow(precDF)*0.10)
# See value in vector
precJenks
# shows first element
precJenks[1] 
# shows second element
precJenks[2] 
# so on and so further...
# Create categorisation by using the Jenks values in the vector
prec_jenks_cl <- c(prec@data@min-1, precJenks[1], 1,
                   precJenks[1], precJenks[2], 2,
                   precJenks[2], precJenks[3], 3,
                   precJenks[3], precJenks[4], 4,
                   precJenks[4], precJenks[5], 5,
                   precJenks[5], precJenks[6], 6,
                   precJenks[6], precJenks[7], 7,
                   precJenks[7], precJenks[8], 8,
                   precJenks[8], precJenks[9], 9,
                   precJenks[9], prec@data@max+1, 10) 
# create matrix
prec_jenks_cl_mat <- matrix(prec_jenks_cl, ncol = 3, byrow = TRUE)
# view categorisation in matrix
prec_jenks_cl_mat
# reclassify original raster using the jenks classifications
prec_jenks_recl <- reclassify(prec, prec_jenks_cl_mat)


# 3. popl
# Extract values from Raster
poplValues <- values(popl)
# Change the values from vector object to data.frame object
poplDF <- as.data.frame(poplValues)
# Remove missing values and reapply column name
poplDF <- as.data.frame(poplDF[!is.na(poplDF$poplValues),])
colnames(poplDF) <- "poplValues"
# Use the getJenksBreaks() function. Sample 0.10 (10%) of the pixels at random and base the categorisation on this. 
# NOTE: Doing this on the full data will take forever - so use the subset argument. 
poplJenks <- getJenksBreaks(poplDF$poplValues, 10, subset = nrow(poplDF)*0.10)
# See value in vector
poplJenks
# shows first element
poplJenks[1] 
# shows second element
poplJenks[2] 
# so on and so further...
# Create categorisation by using the Jenks values in the vector
popl_jenks_cl <- c(popl@data@min-1, poplJenks[1], 1,
                   poplJenks[1], poplJenks[2], 2,
                   poplJenks[2], poplJenks[3], 3,
                   poplJenks[3], poplJenks[4], 4,
                   poplJenks[4], poplJenks[5], 5,
                   poplJenks[5], poplJenks[6], 6,
                   poplJenks[6], poplJenks[7], 7,
                   poplJenks[7], poplJenks[8], 8,
                   poplJenks[8], poplJenks[9], 9,
                   poplJenks[9], popl@data@max+1, 10) 
# create matrix
popl_jenks_cl_mat <- matrix(popl_jenks_cl, ncol = 3, byrow = TRUE)
# view categorisation in matrix
popl_jenks_cl_mat
# reclassify original raster using the jenks classifications
popl_jenks_recl <- reclassify(popl, popl_jenks_cl_mat)


# 4 nvdi
# Extract values from Raster
nvdiValues <- values(nvdi) 
# Change the values from vector object to data.frame object
nvdiDF <- as.data.frame(nvdiValues)
# Remove missing values and reapply column name
nvdiDF <- as.data.frame(nvdiDF[!is.na(nvdiDF$nvdiValues),])
colnames(nvdiDF) <- "nvdiValues"
# Use the getJenksBreaks() function. Sample 0.10 (10%) of the pixels at random and base the categorisation on this. 
# NOTE: Doing this on the full data will take forever - so use the subset argument. 
# EXTRA NOTE: The values for nvdi are very close to each other and so the algorithm splits it to just two cateogries
nvdiJenks <- getJenksBreaks(nvdiDF$tempValues, 2, subset = nrow(nvdiDF)*0.10)
# See value in vector
nvdiJenks
# shows first element
nvdiJenks[1] 
# shows second element
nvdiJenks[2] 
# so on and so further...
# Create categorisation by using the Jenks values in the vector
nvdi_jenks_cl <- c(nvdi@data@min-1, nvdiJenks[1], 1,
                   nvdiJenks[1], nvdi@data@max+1, 2)
# create matrix
nvdi_jenks_cl_mat <- matrix(nvdi_jenks_cl, ncol = 3, byrow = TRUE)
# view categorisation in matrix
nvdi_jenks_cl_mat
# reclassify original raster using the jenks classifications
nvdi_jenks_recl <- reclassify(nvdi, nvdi_jenks_cl_mat)

# 6. elev

# Extract values from Raster
elevValues <- values(elev) 
# Change the values from vector object to data.frame object
elevDF <- as.data.frame(elevValues)
# Remove missing values and reapply column name
elevDF <- as.data.frame(elevDF[!is.na(elevDF$elevValues),])
colnames(elevDF) <- "elevValues"
# Use the getJenksBreaks() function. Sample 0.10 (10%) of the pixels at random and base the categorisation on this. 
# NOTE: Doing this on the full data will take forever - so use the subset argument. 
elevJenks <- getJenksBreaks(elevDF$elevValues, 10, subset = nrow(elevDF)*0.10)
# See value in vector
elevJenks
# shows first element
elevJenks[1] 
# shows second element
elevJenks[2] 
# so on and so further...
# Create categorisation by using the Jenks values in the vector
elev_jenks_cl <- c(elev@data@min-1, elevJenks[1], 1,
                   elevJenks[1], elevJenks[2], 2,
                   elevJenks[2], elevJenks[3], 3,
                   elevJenks[3], elevJenks[4], 4,
                   elevJenks[4], elevJenks[5], 5,
                   elevJenks[5], elevJenks[6], 6,
                   elevJenks[6], elevJenks[7], 7,
                   elevJenks[7], elevJenks[8], 8,
                   elevJenks[8], elevJenks[9], 9,
                   elevJenks[9], elev@data@max+1, 10) 
# create matrix
elev_jenks_cl_mat <- matrix(elev_jenks_cl, ncol = 3, byrow = TRUE)
# view categorisation in matrix
elev_jenks_cl_mat
# reclassify original raster using the Jenks classifications
elev_jenks_recl <- reclassify(elev, elev_jenks_cl_mat)

# see plot in its original form
plot(elev_jenks_recl)
# Now flip the values by subtracting the values from the max value (here its 10)
rev_elev_jenks_recl <- elev_jenks_recl
values(rev_elev_jenks_recl) <- elev_jenks_recl@data@max - values(elev_jenks_recl)
# see plot flipped/inverted
plot(rev_elev_jenks_recl)

tm_shape(rev_elev_jenks_recl) + tm_raster(style = "cont", title = "Inverted Elev (on Jenks scale)", palette= "-Spectral") +
  tm_shape(kenya_states) + tm_polygons(alpha = 0, border.col = "black") +
  tm_layout(frame = FALSE, legend.outside = TRUE)

suitablemap_WLC <- (0.372*prec_jenks_cl + 0.356*temp_jenks_recl + 0.159*popl_jenks_recl + 0.077*rev_elev_jenks_recl + 0.037*nvdi_jenks_recl)*arid_recl
plot(suitablemap_WLC)

suitablemap_WLC_cl <- c(-1, 1, 1,
                        1, 2, 2,
                        2, 3, 3,
                        3, 4, 4,
                        4, 5, 5,
                        5, 6, 6,
                        6, 7, 7,
                        7, 8, 8,
                        8, 10, 9)
suitablemap_WLC_cl_mat <-matrix(suitablemap_WLC_cl, ncol=3, byrow=TRUE)
suitablemap_WLC_cl_recl<-reclassify(suitablemap_WLC, suitablemap_WLC_cl_mat)
suitablemap_WLC_cl_recl

tm_shape(suitablemap_WLC_cl_recl)+
  tm_raster(style="pretty",
            title="LF Suitability (AHP WLC)",
            palette="Reds",
            labels=c("Negligable (\u22641)", ">1-2", ">2-3", ">3-4", ">4-5", ">5-6", ">6-7", ">7-8", ">8-10"))+
  tm_shape(kenya_states)+
  tm_polygons(alpha=0, border.col="black")+
  tm_text("NAME_1", size="AREA")+
  tm_layout(frame=FALSE, legend.outside=TRUE, legend.title.size=0.5, legend.text.size=0.5)+
  tm_scale_bar(position=c("left","bottom"))+tm_compass(position=c("right","top"))

