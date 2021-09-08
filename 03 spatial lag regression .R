#----------------spatial lag regression----------------#
library(spdep)
coordsW <- LondonWards%>%
  st_centroid()%>%
  st_geometry()

plot(coordsW)

#Now we need to generate a spatial weights matrix (remember from the lecture a couple of weeks ago). 
#We'll start with a simple binary matrix of queen's case neighbours

LWard_nb <- LondonWards %>%
  poly2nb(., queen=T)

#or nearest neighbours
knn_wards <-coordsW %>%
  knearneigh(., k=4)

LWard_knn <- knn_wards %>%
  knn2nb()

#plot them
plot(LWard_nb, st_geometry(coordsW), col="red")

#log transformation
lagvariable2 <- lagvariable%>%
  mutate(quantity=log(quantity),pop_density=(pop_density)^0.5)%>%
  select(quantity, diversity,pop_density,income_score,employment_score,
         restaurant_density,cafe_density,bench_density,school_density)

library(spatialreg)
library(broom)
#------density
slag_dv_model_queen <- lagsarlm(quantity ~ pop_density+income_score, 
                                data = lagvariable2, 
                                nb2listw(LWard_nb, style="C"), 
                                method = "eigen")
tidy(slag_dv_model_queen)
summary(slag_dv_model_queen)
glance(slag_dv_model_queen)


slag_dv_model_knn <- lagsarlm(quantity ~ pop_density + income_score, 
                              data = lagvariable2, 
                              nb2listw(LWard_knn, style="C"), 
                              method = "eigen")

tidy(slag_dv_model_knn)
summary(slag_dv_model_knn)
glance(slag_dv_model_knn)

#-----diversity
slag_dv_model_queen <- lagsarlm(diversity ~ pop_density + income_score, 
                                 data = lagvariable2, 
                                 nb2listw(LWard_nb, style="C"), 
                                 method = "eigen")

tidy(slag_dv_model_queen)
summary(slag_dv_model_queen)
glance(slag_dv_model_queen)

slag_dv_model_knn <- lagsarlm(diversity ~ pop_density+ income_score, 
                              data = lagvariable2, 
                              nb2listw(LWard_knn, style="C"), 
                              method = "eigen")

tidy(slag_dv_model_knn)
summary(slag_dv_model_knn)
glance(slag_dv_model_knn)


#----------spatial err regression-------------------#
#-------density
sem_model_queen <- errorsarlm(quantity ~ pop_density, 
                                data = lagvariable2, 
                                nb2listw(LWard_nb, style="C"), 
                                method = "eigen")
tidy(sem_model_queen)
summary(sem_model_queen)
glance(sem_model_queen)

sem_model_knn <- errorsarlm(quantity ~ pop_density + income_score, 
                            data = lagvariable2, 
                            nb2listw(LWard_knn, style="C"), 
                            method = "eigen")
tidy(sem_model_knn)
summary(sem_model_knn)
glance(sem_model_knn)


#------diversity
slag_dv_model_queen <- errorsarlm(diversity ~ pop_density + income_score, 
                                data = lagvariable2, 
                                nb2listw(LWard_nb, style="C"), 
                                method = "eigen")


tidy(slag_dv_model_queen)
summary(slag_dv_model_queen)
glance(slag_dv_model_queen)

slag_dv_model_knn <- errorsarlm(quantity ~ pop_density+ income_score, 
                              data = lagvariable2, 
                              nb2listw(LWard_knn, style="C"), 
                              method = "eigen")

tidy(slag_dv_model_knn)
summary(slag_dv_model_knn)
glance(slag_dv_model_knn)

#--------restaurant
#---SLM
slag_dv_model_queen <- lagsarlm(restaurant_denstiy ~ pop_density + income_score, 
                                data = lagvariable2, 
                                nb2listw(LWard_nb, style="C"), 
                                method = "eigen")

tidy(slag_dv_model_queen)
summary(slag_dv_model_queen)
glance(slag_dv_model_queen)

slag_dv_model_knn <- lagsarlm(restaurant_denstiy ~ pop_density+ income_score, 
                              data = lagvariable2, 
                              nb2listw(LWard_knn, style="C"), 
                              method = "eigen")

tidy(slag_dv_model_knn)
summary(slag_dv_model_knn)
glance(slag_dv_model_knn)

#---SEM
sem_model_queen <- errorsarlm(restaurant_denstiy ~ pop_density+income_score, 
                              data = lagvariable2, 
                              nb2listw(LWard_nb, style="C"), 
                              method = "eigen")
tidy(sem_model_queen)
summary(sem_model_queen)
glance(sem_model_queen)

sem_model_knn <- errorsarlm(restaurant_denstiy ~ pop_density + income_score, 
                            data = lagvariable2, 
                            nb2listw(LWard_knn, style="C"), 
                            method = "eigen")
tidy(sem_model_knn)
summary(sem_model_knn)
glance(sem_model_knn)



#--------cafe
#---SLM
slag_dv_model_queen <- lagsarlm(cafe_denstiy ~ pop_density + income_score, 
                                data = lagvariable2, 
                                nb2listw(LWard_nb, style="C"), 
                                method = "eigen")

tidy(slag_dv_model_queen)
summary(slag_dv_model_queen)
glance(slag_dv_model_queen)

slag_dv_model_knn <- lagsarlm(cafe_denstiy ~ pop_density+ income_score, 
                              data = lagvariable2, 
                              nb2listw(LWard_knn, style="C"), 
                              method = "eigen")

tidy(slag_dv_model_knn)
summary(slag_dv_model_knn)
glance(slag_dv_model_knn)


#---SEM
sem_model_queen <- errorsarlm(cafe_denstiy ~ pop_density + income_score, 
                              data = lagvariable2, 
                              nb2listw(LWard_nb, style="C"), 
                              method = "eigen")
tidy(sem_model_queen)
summary(sem_model_queen)
glance(sem_model_queen)

sem_model_knn <- errorsarlm(cafe_denstiy ~ pop_density + income_score, 
                            data = lagvariable2, 
                            nb2listw(LWard_knn, style="C"), 
                            method = "eigen")
tidy(sem_model_knn)
summary(sem_model_knn)
glance(sem_model_knn)
