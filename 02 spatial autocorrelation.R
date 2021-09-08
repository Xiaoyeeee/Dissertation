library(sf) #simple features
library(readr)
library(dplyr)
library(janitor)
library(tidyverse)

#-----------------spatial autocorrelation--------------------#
library(spdep)
#calculate the centroids of all wards in London
coordsW <- quantity%>%
  st_centroid()%>%
  st_geometry()

plot(coordsW,axes=TRUE)

#create a neighbours list
#Queen
LWard_nb <- quantity %>%
  poly2nb(., queen=T)
#k-nearest
knn_wards <-coordsW %>%
  knearneigh(., k=4)

LWard_knn <- knn_wards %>%
  knn2nb()

#plot them
plot(LWard_nb, st_geometry(coordsW), col="red")

#add a map underneath
plot(quantity$geometry, add=T)

#create a spatial weights object from these weights
#queens neighbours
Lward.lw <- LWard_nb %>%
  nb2listw(., style="C")

head(Lward.lw$neighbours)

#k-nearest
#queens neighbours
Lward.lw <- LWard_knn %>%
  nb2listw(., style="C")

#------------------------------global index---------------------#
#-------------------count
#calculate global Moran's I
I_LWard_Global_Count <- quantity %>%
  pull(quantity) %>%
  as.vector()%>%
  moran.test(., Lward.lw)

I_LWard_Global_Count

#Geary's C
C_LWard_Global_Count <- 
  quantity %>%
  pull(quantity) %>%
  as.vector()%>%
  geary.test(., Lward.lw)

C_LWard_Global_Count

#Getis Ord General G
#This tells us whether high or low values are clustering. 
G_LWard_Global_Count <- 
  quantity %>%
  pull(quantity) %>%
  as.vector()%>%
  globalG.test(., Lward.lw)

G_LWard_Global_Count

#------------------density
#calculate global Moran's I
I_LWard_Global_Density <- quantity %>%
  pull(poi_density) %>%
  as.vector()%>%
  moran.test(., Lward.lw)

I_LWard_Global_Density

#Geary's C
C_LWard_Global_Density <- 
  quantity %>%
  pull(poi_density) %>%
  as.vector()%>%
  geary.test(., Lward.lw)

C_LWard_Global_Density

#Getis Ord General G
#This tells us whether high or low values are clustering. 
G_LWard_Global_Density <- 
  quantity %>%
  pull(poi_density) %>%
  as.vector()%>%
  globalG.test(., Lward.lw)

G_LWard_Global_Density

#------------------diversity
#calculate global Moran's I
I_LWard_Global_Diversity <- diversity %>%
  pull(diversity) %>%
  as.vector()%>%
  moran.test(., Lward.lw)

I_LWard_Global_Diversity

#Geary's C
C_LWard_Global_Diversity <- 
  diversity %>%
  pull(diversity) %>%
  as.vector()%>%
  geary.test(., Lward.lw)

C_LWard_Global_Diversity

#Getis Ord General G
#This tells us whether high or low values are clustering. 
G_LWard_Global_Diversity <- 
  diversity %>%
  pull(diversity) %>%
  as.vector()%>%
  globalG.test(., Lward.lw)

G_LWard_Global_Diversity
#--------------------------------------------------------------------#

#---------------------------Local Moran's I--------------------------#
#-----------quantity
#use the localmoran function to generate I for each ward in the city
I_LWard_Local_count <- quantity %>%
  pull(quantity) %>%
  as.vector()%>%
  localmoran(., Lward.lw)%>%
  as_tibble()

I_LWard_Local_Density <- quantity %>%
  pull(poi_density) %>%
  as.vector()%>%
  localmoran(., Lward.lw)%>%
  as_tibble()

#what does the output (the localMoran object) look like?
slice_head(I_LWard_Local_Density, n=5)

#copy column 1 and 4
quantity <- quantity %>%
  mutate(poi_count_I = as.numeric(I_LWard_Local_count$Ii))%>%
  mutate(poi_count_Iz =as.numeric(I_LWard_Local_count$Z.Ii))%>%
  mutate(density_I =as.numeric(I_LWard_Local_Density$Ii))%>%
  mutate(density_Iz =as.numeric(I_LWard_Local_Density$Z.Ii))

#mapping outputs
breaks1<-c(-1000,-2.58,-1.96,-1.65,1.65,1.96,2.58,1000)
#create a anew diverging color brewer palette
library(GISTools)
MoranColours<- rev(brewer.pal(8, "RdGy"))

#plot local Moran's I of amenities count
tm_shape(quantity) +
  tm_polygons("poi_count_Iz",
              style="fixed",
              breaks=breaks1,
              palette=MoranColours,
              midpoint=NA,
              title="Local Moran's I, Amenities Count in London")

#plot local Moran's I of amenities density
tm_shape(quantity) +
  tm_polygons("density_Iz",
              style="fixed",
              breaks=breaks1,
              palette=MoranColours,
              midpoint=NA,
              title="Local Moran's I, Amenities Density in London")

#-----------diversity
#use the localmoran function to generate I for each ward in the city
I_LWard_Local_Diversity <- diversity %>%
  pull(diversity) %>%
  as.vector()%>%
  localmoran(., Lward.lw)%>%
  as_tibble()

#what does the output (the localMoran object) look like?
slice_head(I_LWard_Local_Diversity, n=5)

#copy column 1 and 4
diversity <- diversity %>%
  mutate(diversity_I =as.numeric(I_LWard_Local_Diversity$Ii))%>%
  mutate(diversity_Iz =as.numeric(I_LWard_Local_Diversity$Z.Ii))

#mapping outputs
breaks1<-c(-1000,-2.58,-1.96,-1.65,1.65,1.96,2.58,1000)

#plot local Moran's I of amenities diversity
tm_shape(diversity) +
  tm_polygons("diversity_Iz",
              style="fixed",
              breaks=breaks1,
              palette=MoranColours,
              midpoint=NA,
              title="Local Moran's I, Amenities Diversity in London")
#------------------------------------------------------------------#

#-----------------------Getis Ord G Statistic-------3--------------#
#---------Getis Ord G of amenities count
Gi_LWard_Local_Count <- quantity %>%
  pull(quantity) %>%
  as.vector()%>%
  localG(., Lward.lw)

head(Gi_LWard_Local_Count)

quantity <- quantity %>%
  mutate(count_G = as.numeric(Gi_LWard_Local_Count))

#map the outputs
GIColours<- rev(brewer.pal(8, "RdBu"))

#now plot on an interactive map
tm_shape(quantity) +
  tm_polygons("count_G",
              style="fixed",
              breaks=breaks1,
              palette=GIColours,
              midpoint=NA,
              title="Gi*, amenities count in London")


#---------Getis Ord G of amenities density
Gi_LWard_Local_Density <- quantity %>%
  pull(poi_density) %>%
  as.vector()%>%
  localG(., Lward.lw)

head(Gi_LWard_Local_Density)

quantity <- quantity %>%
  mutate(density_G = as.numeric(Gi_LWard_Local_Density))

#now plot on an interactive map
tm_shape(quantity) +
  tm_polygons("density_G",
              style="fixed",
              breaks=breaks1,
              palette=GIColours,
              midpoint=NA,
              title="Gi*, amenities density in London")

#---------Getis Ord G of amenities diversity
Gi_LWard_Local_Diversity <- diversity %>%
  pull(diversity) %>%
  as.vector()%>%
  localG(., Lward.lw)

head(Gi_LWard_Local_Diversity)

diversity <- diversity %>%
  mutate(diversity_G = as.numeric(Gi_LWard_Local_Diversity))

#now plot on an interactive map
tm_shape(diversity) +
  tm_polygons("diversity_G",
              style="fixed",
              breaks=breaks1,
              palette=GIColours,
              midpoint=NA,
              title="Gi*, amenities diversity in London")
#------------------------------------------------------------------#

