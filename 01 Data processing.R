library(sf)
library(readr)
library(dplyr)
library(janitor)
library(tidyverse)
library(tmap)

#------------------------load data----------------------#
#load London boundries
#seems gss_code is not the same as Profile
LondonWards <- st_read("/Users/apple/Desktop/dissertation/data/London-wards-2018/London-wards-2018_ESRI/London_Ward_CityMerged.shp") %>% 
  st_transform(.,27700)
st_crs(LondonWards)
head(LondonWards)

#load poi polygon data
POI_poly <- st_read("/Users/apple/Desktop/dissertation/data/greater-london-latest-free.shp/gis_osm_pois_a_free_1.shp") %>% 
  st_transform(.,27700)
st_crs(POI_poly)

#change polygon to point
POI_topoint <- POI_poly%>%
  st_centroid()#%>%
#st_geometry() #get or set an geometry from an sf object

#load poi points data
POI_point <- st_read("/Users/apple/Desktop/dissertation/data/greater-london-latest-free.shp/gis_osm_pois_free_1.shp") %>% 
  st_transform(.,27700)
st_crs(POI_point)

#combine points and polygon centroids
POI <- rbind(POI_point,POI_topoint)

rm(POI_point)
rm(POI_topoint)
rm(POI_poly)

#-----------------------data clean---------------------------#
#POI <- distinct(POI) #select unique rows from dataframe

#delete points out of London
POI <- POI[LondonWards,]

#tidy categories
POI$fclass[which(POI$fclass == 'chemist')] <- 'pharmacy'
POI$fclass[which(POI$fclass == 'monument')] <- 'memorial'
POI$fclass[which(POI$fclass == 'kiosk')] <- 'newsagent'
POI$fclass[which(POI$fclass == 'recycling clothes')] <- 'recycling'
POI$fclass[which(POI$fclass == 'recycling glass')] <- 'recycling'
POI$fclass[which(POI$fclass == 'recycling metal')] <- 'recycling'
POI$fclass[which(POI$fclass == 'recycling paper')] <- 'recycling'

c=list('battlefield','chalet','embassy','hunting_stand','prison',
       'wastewater_plant','water_mill','water_tower','water_well',
       'water_works','wayside_shrine','vending_any','vending_machine','vending_parking')
for (i in c) {
  POI <-  filter(POI,POI$fclass != i)
}

rm(c)
rm(i)

#add poi points to London Wards
#and calculate quantity and density of poi points
points_sf_joined <- LondonWards %>%
  st_join(POI)%>%
  add_count(NAME)%>%
  janitor::clean_names()%>%
  mutate(area=st_area(.))%>%
  mutate(poi_density=n/area)

#change the name of n
names(points_sf_joined)[names(points_sf_joined) == 'n'] <- 'quantity'

#extract quantity of amenities in every wards
quantity<- points_sf_joined %>%                    
  group_by(gss_code) %>%         
  summarise(poi_density = first(poi_density),
            wardname= first(name),
            quantity= first(quantity))

#change unit to hectare
quantity$poi_ha <- quantity$poi_density*10000

#plot poi density
library(tmap)
tmap_mode("view")
tm_shape(quantity) +
  tm_polygons("poi_ha",
              style="jenks",
              palette="PuOr",
              midpoint=NA,
              popup.vars=c("gss_code", "poi_ha"),
              title="Amenities Density (quantity)
               (1/ha)")


#plot poi count
tm_shape(quantity) +
  tm_polygons("quantity",
              style="jenks",
              palette="PuOr",
              midpoint=NA,
              popup.vars=c("gss_code", "quantity"),
              title="Amenities Count (quantity)")



#calculate poi diversity
#calculate amount of amenities of each type in each wards
diversity<- points_sf_joined %>%                    
  group_by(gss_code,fclass) %>%         
  summarise(diversity = n(),
            wardname= first(name))

#calculate diversity
diversity <- diversity %>% 
  group_by(gss_code) %>% 
  summarise(diversity = n(),
            wardname= first(wardname))

head(diversity)
#plot diversity
tm_shape(diversity) +
  tm_polygons("diversity",
              style="jenks",
              palette="PuOr",
              midpoint=NA,
              popup.vars=c("gss_code", "diversity"),
              title="Amenities Diversity")




#-----------------combine all the variables----------------------#

#load independent variable
Profiles<- read.csv("/Users/apple/Desktop/dissertation/data/index of deprivation/London wards id2019 summary measures.csv", 
                    header = TRUE, sep = ",",  
                    encoding = "latin1")

#check all of the columns have been read in correctly
Datatypelist <- Profiles %>% 
  summarise_all(class) %>%
  pivot_longer(everything(), 
               names_to="All_variables", 
               values_to="Variable_class")

#combine Profiles with LondonWards
Profiles <- LondonWards %>% 
  left_join(Profiles, 
            by = c("GSS_CODE" = "Ward.Code"))%>%
  dplyr::distinct(GSS_CODE, .keep_all = T)%>%
  dplyr::select(NAME,GSS_CODE,DISTRICT, Ward.Name, Borough,Population,Income.score, Employment.score)

quantity <- st_drop_geometry(quantity)

#add quantity to Profiles
Profiles <- Profiles%>%
  left_join(quantity,
            by = c("GSS_CODE" = "gss_code"))%>%
  #dplyr::distinct(GSS_CODE, .keep_all = T)%>%
  dplyr::select(NAME,GSS_CODE,DISTRICT,Population,Income.score, Employment.score,poi_density,quantity)

#add diversity to Profiles
diversity <- st_drop_geometry(diversity)
Profiles <- Profiles%>%
  left_join(diversity,
            by = c("GSS_CODE" = "gss_code"))%>%
  dplyr::select(NAME,GSS_CODE,DISTRICT,Population,Income.score, Employment.score,poi_density,quantity,diversity)



#---------------extract 4 types amenities-----------------#
#------------------------------restaurant-----------------------------#
restaurant <- POI%>%
  filter(.,fclass =='restaurant')

restaurant <- restaurant[LondonWards,]

#add poi points to London Wards
#and calculate quantity and density of poi points
restaurant <- LondonWards %>%
  st_join(restaurant)%>%
  add_count(NAME)%>%
  janitor::clean_names()%>%
  mutate(area=st_area(.))%>%
  mutate(restaurant_density=n/area)

names(restaurant)[names(restaurant) == 'n'] <- 'restaurant_count'

#extract quantity of amenities in every wards
restaurant<- restaurant %>%                    
  group_by(gss_code) %>%         
  summarise(restaurant_density = first(restaurant_density),
            wardname= first(name),
            restaurant_count= first(restaurant_count))

#plot restaurant density in London
tmap_mode("view")
tm_shape(restaurant) +
  tm_polygons("restaurant_density",
              style="jenks",
              palette="PuOr",
              midpoint=NA,
              popup.vars=c("gss_code", "restaurant_density"),
              title="Restaurant Density")

#------------------------------cafe-----------------------------#
cafe<- POI%>%
  filter(.,fclass =='cafe')

cafe <- cafe[LondonWards,]

#add poi points to London Wards
#and calculate quantity and density of poi points
cafe <- LondonWards %>%
  st_join(cafe)%>%
  add_count(NAME)%>%
  janitor::clean_names()%>%
  mutate(area=st_area(.))%>%
  mutate(cafe_density=n/area)

names(cafe)[names(cafe) == 'n'] <- 'cafe_count'

#extract quantity of amenities in every wards
cafe<- cafe %>%                    
  group_by(gss_code) %>%         
  summarise(wardname= first(name),
            cafe_count= first(cafe_count),
            cafe_density = first(cafe_density))

#plot restaurant density in London
tm_shape(cafe) +
  tm_polygons("cafe_density",
              style="jenks",
              palette="PuOr",
              midpoint=NA,
              popup.vars=c("gss_code", "cafe_density"),
              title="Cafe Density")

#------------------------------bench-----------------------------#
bench<- POI%>%
  filter(.,fclass =='bench')

bench <- bench[LondonWards,]

#add poi points to London Wards
#and calculate quantity and density of poi points
bench <- LondonWards %>%
  st_join(bench)%>%
  add_count(NAME)%>%
  janitor::clean_names()%>%
  mutate(area=st_area(.))%>%
  mutate(bench_density=n/area)

names(bench)[names(bench) == 'n'] <- 'bench_count'

#extract quantity of amenities in every wards
bench<- bench %>%                    
  group_by(gss_code) %>%         
  summarise(wardname= first(name),
            bench_count= first(bench_count),
            bench_density = first(bench_density))

#plot bench density in London
tm_shape(bench) +
  tm_polygons("bench_density",
              style="jenks",
              palette="PuOr",
              midpoint=NA,
              popup.vars=c("gss_code", "bench_density"),
              title="Bench Density")

#------------------------------school-----------------------------#
school<- POI%>%
  filter(.,fclass =='school')

school <- school[LondonWards,]

#add poi points to London Wards
#and calculate quantity and density of poi points
school <- LondonWards %>%
  st_join(school)%>%
  add_count(NAME)%>%
  janitor::clean_names()%>%
  mutate(area=st_area(.))%>%
  mutate(school_density=n/area)

names(school)[names(school) == 'n'] <- 'school_count'

#extract quantity of amenities in every wards
school<- school %>%                    
  group_by(gss_code) %>%         
  summarise(wardname= first(name),
            school_count= first(school_count),
            school_density = first(school_density))

#plot school density in London
tm_shape(school) +
  tm_polygons("school_density",
              style="jenks",
              palette="PuOr",
              midpoint=NA,
              popup.vars=c("gss_code", "school_density"),
              title="School Density")


#add restaurant, cafe, bench, school----------------------------------------#
restaurant <- st_drop_geometry(restaurant)
Profiles <- Profiles%>%
  left_join(restaurant,
            by = c("GSS_CODE" = "gss_code"))%>%
  dplyr::select(NAME,GSS_CODE,DISTRICT,Population,Income.score, 
                Employment.score,poi_density,quantity,diversity,restaurant_density)

cafe <- st_drop_geometry(cafe)
Profiles <- Profiles%>%
  left_join(cafe,
            by = c("GSS_CODE" = "gss_code"))%>%
  dplyr::select(NAME,GSS_CODE,DISTRICT,Population,Income.score, 
                Employment.score,poi_density,quantity,diversity,restaurant_density,cafe_density)

bench <- st_drop_geometry(bench)
Profiles <- Profiles%>%
  left_join(bench,
            by = c("GSS_CODE" = "gss_code"))%>%
  dplyr::select(NAME,GSS_CODE,DISTRICT,Population,Income.score, 
                Employment.score,poi_density,quantity,diversity,
                restaurant_density,cafe_density,bench_density)

school <- st_drop_geometry(school)
Profiles <- Profiles%>%
  left_join(school,
            by = c("GSS_CODE" = "gss_code"))%>%
  dplyr::select(NAME,GSS_CODE,DISTRICT,Population,Income.score, 
                Employment.score,poi_density,quantity,diversity,
                restaurant_density,cafe_density,bench_density,school_density)

#--------------------------------------------------------------------------------------------#


#--------------------------create a list including all variables--------------------------------------#
head(Profiles)

lagvariable <- Profiles %>%
  janitor::clean_names()%>%
  mutate(area=st_area(.))%>%
  mutate(pop_density=population/area)%>%
  dplyr::select(name,gss_code,district,population,pop_density,income_score, 
                employment_score,poi_density,quantity,diversity,
                restaurant_density,cafe_density,bench_density,school_density)

head(lagvariable)

#plot 
tm_shape(lagvariable) +
  tm_polygons("pop_density",
              style="jenks",
              palette="PuOr",
              midpoint=NA,
              popup.vars=c("gss_code", "pop_density"),
              title="Population Density")

#plot 
tm_shape(lagvariable) +
  tm_polygons("income_score",
              style="jenks",
              palette="PuOr",
              midpoint=NA,
              popup.vars=c("gss_code", "income_score"),
              title="income score")

#plot 
tm_shape(lagvariable) +
  tm_polygons("employment_score",
              style="jenks",
              palette="PuOr",
              midpoint=NA,
              popup.vars=c("gss_code", "employment_score"),
              title="employment score")


#-------------------plot correlation-------------------------#
library(corrplot)
library(units)

cor_dataframe <- lagvariable %>%
  dplyr::mutate(quantity = drop_units(poi_density), pop_density = drop_units(pop_density)) %>% 
  sf::st_set_geometry(NULL) %>% 
  dplyr::select(quantity,diversity, pop_density,income_score,employment_score)
head(cor_dataframe)

mcor <- cor(cor_dataframe)

#corrplot(mcor)

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(mcor, method = "shade", type = {"lower"}, shade.col = NA, 
         tl.col = "black", tl.srt = 30, addCoef.col = "black", 
         cl.pos = NULL, order = "AOE")



#-------------check the frequency distribution--------------------#
lagvariable$poi_density <- drop_units(lagvariable$poi_density)

ggplot(lagvariable, aes(x=poi_density)) + 
  geom_histogram() 

ggplot(lagvariable, aes(x=log(poi_density))) + 
  geom_histogram() 

ggplot(lagvariable, aes(x=diversity)) + 
  geom_histogram() 


ggplot(lagvariable, aes(x=log(diversity))) + 
  geom_histogram() 

ggplot(cor_dataframe, aes(x=pop_density)) + 
  geom_histogram()

ggplot(cor_dataframe, aes(x=log(pop_density))) + 
  geom_histogram() 

 #plot to see how to transform 
library(car)
symbox(~pop_density, 
       cor_dataframe, 
       na.rm=T,
       powers=seq(0,1,by=.1))

ggplot(cor_dataframe, aes(x=(pop_density)^0.5)) + 
  geom_histogram() 

ggplot(cor_dataframe, aes(x=income_score)) + 
  geom_histogram()

ggplot(cor_dataframe, aes(x=employment_score)) + 
  geom_histogram()

#---------------------------------------------#

#----------------linear regression--------------#
linear_model <- lm(diversity ~ pop_density + income_score, data = lagvariable)
summary(linear_model)

linear_model <- lm(diversity ~ pop_density + employment_score, data = lagvariable)
summary(linear_model) 

linear_model <- lm(quantity ~ pop_density+income_score, data = lagvariable)
summary(linear_model)

linear_model <- lm(diversity ~ pop_density+income_score, data = lagvariable)
summary(linear_model)


