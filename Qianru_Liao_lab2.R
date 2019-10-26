library(GISTools)
library(RColorBrewer)
library(tbart)
library(leaflet)
library(sp)
library(ggplot2)
# Load the data and change the working directory where the downloaded data ﬁle is located
load('EN.RData')
#getwd()
#do not need to change the working environment 
set.seed(12345)

# Make maps of the study area shaded by the population density and the daily variability of PM2.5 in the year of 2011. The two variables are respectively named as pop.den, pm25.std in the spatial polygon data frame (en.county). 
#shading <- auto.shading(en.county$pop.den,n=5,cols=brewer.pal(5,"BrBG"))
#choropleth(en.county,en.county$pop.den,shading)
#choro.legend(1350000, 1405900, shading, cex = 0.6, title = "Population Density")
en.county1<-spTransform(en.county, "+proj=longlat +datum=WGS84 +no_defs")

m <- leaflet(en.county1) %>%
  setView(-78.80,42.90, 9) %>%
  addProviderTiles("MapBox", options = providerTileOptions(
    id = "mapbox.light",
    accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN')))
bins<-c(0,1,2,3,4,5,Inf)
pal<-colorBin("Set1", domain = en.county$pop.den, bins = bins)

pop.den_map <- m%>%
  addTiles()%>%
  addPolygons(
    fillColor = ~pal(en.county$pop.den),
    weight = 0.5,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7)%>%
    addLegend(pal = pal, values = ~en.county$pop.den, opacity = 0.7, title = 'Population Density in Buffalo',
            position = "bottomright")
pop.den_map
#In most rural areas, the population density in red are lower than 1 and the place are located in the rural areas, which are far away from the Buffalo center.
#And the high population density in light yellow which are higher than 5, are concentrated in the central Buffalo.
#The population density gets higher with the decline of the distance between their areas and Buffalo center.

## Make maps of the study area shaded by the daily variability of PM2.5 in the year of 2011. 
m <- leaflet(en.county1) %>%
  setView(-78.80,42.90, 9) %>%
  addProviderTiles("MapBox", options = providerTileOptions(
    id = "mapbox.light",
    accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN')))
bins<-c(4,4.2,4.4,4.6,4.8,5,5.2,Inf)
pal<-colorBin("Spectral", domain = en.county$pm25.std, bins = bins)

pop.den_map <- m%>%
  addTiles()%>%
  addPolygons(
    fillColor = ~pal(en.county$pm25.std),
    weight = 0.5,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7)%>%
  addLegend(pal = pal, values = ~en.county$pm25.std, opacity = 0.7, title = 'Daily variability of PM 2.5 in 2011',
            position = "bottomright")
pop.den_map
#From the map, we can find out that the areas with lowest PM 2.5 variability (<4.2) in red are located in the southern-east Buffalo areas. 
#And the highest daily variability(>5) of PM 2.5 in green are distributed in the northern-west part of Buffalo, near the Niagara falls and the boundary bewteen the Canada and US.
#From the northern-west corner to the southern-east corner, the color in the maps turns from green to red, which means the daily variability of PM 2.5 decrease gradually in spatial.

#Question 2
# Solve the p-median problem when p value is ranged from 3 to 15 stations (p = 3,4,...,15). 
distance_total = c(rep(0,13))
set.seed(12345)
for (i in 3:15){
  solution.i <- allocations(en.county, p=i)
  unique(solution.i$allocation)%>%print()
  distance_total[i-2]= sum(unique(solution.i$allocdist))
}
distance_total
p_facility = c(3:15)
TotalDistance = distance_total/100000
par(mar = rep(2, 4))
#plot(facility, distance_total, type = "o")
ggplot(,aes(p_facility, distance_total))+geom_line() +geom_point()+xlim(0,15)

#report the solutions for each value of p in terms of weighted distance and the sites selected for facilitie
#When the p is 3, the total weighted distance is 2971383, and the selected sites are 55, 287, 178.
#When the p is 4, the total weighted distance is 2564183, and the selected sites are  18, 17, 284, 178.
#When the p is 5, the total weighted distance is 2246062, and the selected sites are  
#When the p is 6, the total weighted distance is 2035341, and the selected sites are  
#When the p is 7, the total weighted distance is 1849296, and the selected sites are  
#When the p is 8, the total weighted distance is 1772369, and the selected sites are  
#When the p is 9, the total weighted distance is 1670952, and the selected sites are  
#When the p is 10, the total weighted distance is 1556640, and the selected sites are  
#When the p is 11, the total weighted distance is 1480025, and the selected sites are  
#When the p is 12, the total weighted distance is 1408786, and the selected sites are  
#When the p is 13, the total weighted distance is 1338536, and the selected sites are  
#When the p is 14, the total weighted distance is 1274066, and the selected sites are  
#When the p is 15, the total weighted distance is 1254386, and the selected sites are  

# Is there any site which is consistently identiﬁed as facilities?
#Yes, 14, 49, 188...

# spider diagram for p = 6

solution.6 <- allocations(en.county1, p=6)
unique(solution.6$allocation)%>%print()
distance_total6= sum(unique(solution.6$allocdist))
distance_total6

plot_p6 = plot(star.diagram(solution.6),col='darkblue',lwd=2,add=FALSE)
distance_total6_seperate= unique(solution.6$allocdist)
distance_total6_seperate

n <- leaflet(en.county1) %>%
  setView(-78.80,42.90, 9) %>%
  addProviderTiles("MapBox", options = providerTileOptions(
    id = "mapbox.light",
    accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN')))
bins<-c(0,0.02,0.04,0.06,0.08,0.10,0.12,0.14,0.16,0.18,0.2,Inf)
pal<-colorBin("Spectral", domain = solution.6$allocdist, bins = bins)

A=star.diagram(solution.6)


pop.den_map <- n%>%
  addTiles()%>%
  addPolygons(
    fillColor = ~pal(solution.6$allocdist),
    weight = 0.5,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7)%>%
  addLegend(pal = pal, values = ~solution.6$allocdist, opacity = 0.7, title = 'Daily variability of PM 2.5 in 2011',
            position = "bottomright")%>%
  addPolylines(data=A,
              color = '#F00',
              weight = 2)
pop.den_map

#For the p = 5 solution, the best solution uses sites 18, 49, 121, 159, 230
#Solve the p-median problem using allocations in order to ﬁnd the best solution which does not involve using all ﬁve of these sites 
encounty2 = en.county[-c(18,49,121,159,230),]
solution.5_new <- allocations(en.county, encounty2, p =5)
unique(solution.5_new$allocation)

#How does the best 5 facility solution compare to the one that doesnt use all of the best sites, 
#in terms of weighted distance, and in terms of sites used? 
#Map these two solutions using the spider diagram and report the id of ﬁve sites for each solution.
#Answer:
#for distance and sites
solution.5 <- allocations(en.county,p =5)
unique(solution.5$allocation)
distance_total5= sum(unique(solution.5$allocdist))
distance_total5

#for map
solution.5_1 <- allocations(en.county1,p =5)
unique(solution.5_1$allocation)
distance_total5_1= sum(unique(solution.5_1$allocdist))
distance_total5_1

n <- leaflet(en.county1) %>%
  setView(-78.80,42.90, 9) %>%
  addProviderTiles("MapBox", options = providerTileOptions(
    id = "mapbox.light",
    accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN')))
bins<-c(0,0.02,0.04,0.06,0.08,0.10,0.12,0.14,0.16,0.18,0.2,Inf)
pal<-colorBin("Spectral", domain = solution.5_1$allocdist, bins = bins)

B=star.diagram(solution.5_1)

pop.den_map <- n%>%
  addTiles()%>%
  addPolygons(
    fillColor = ~pal(solution.5_1$allocdist),
    weight = 0.5,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7)%>%
  addLegend(pal = pal, values = ~solution.5_1$allocdist, opacity = 0.7, title = 'Daily variability of PM 2.5 in 2011',
            position = "bottomright")%>%
  addPolylines(data=B,
               color = '#F00',
               weight = 2)
pop.den_map


#When we use all the best sites, the sites are 18, 49, 121, 159, 230. And the weighted distance are total 2246062.
encounty2 = en.county[-c(18,49,121,159,230),]
solution.5_new <- allocations(en.county, encounty2, p =5)
unique(solution.5_new$allocation)
distance_total5_new= sum(unique(solution.5_new$allocdist))
distance_total5_new
#When we do not use all the best sites, the sites are 14, 17, 213, 283, 146. And the weighted distance are total 2266259.
#It's obvious that the weighted distance not under the case of best sites are much larger than the weighted distance under the best sites.
#for map 
encounty2 = en.county1[-c(18,49,121,159,230),]
solution.5_new_1 <- allocations(en.county1, encounty2, p =5)
unique(solution.5_new_1$allocation)
distance_total5_new_1= sum(unique(solution.5_new_1$allocdist))
distance_total5_new_1

n <- leaflet(en.county1) %>%
  setView(-78.80,42.90, 9) %>%
  addProviderTiles("MapBox", options = providerTileOptions(
    id = "mapbox.light",
    accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN')))
bins<-c(0,0.02,0.04,0.06,0.08,0.10,0.12,0.14,0.16,0.18,0.2,Inf)
pal<-colorBin("Spectral", domain = solution.5_new_1$allocdist, bins = bins)

C=star.diagram(solution.5_new_1)

pop.den_map <- n%>%
  addTiles()%>%
  addPolygons(
    fillColor = ~pal(solution.5_new_1$allocdist),
    weight = 0.5,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7)%>%
  addLegend(pal = pal, values = ~solution.5_new_1$allocdist, opacity = 0.7, title = 'Daily variability of PM 2.5 in 2011',
            position = "bottomright")%>%
  addPolylines(data=C,
               color = '#F00',
               weight = 2)
pop.den_map

#5. For the p = 5 solution, ﬁnd the best solution which takes into account the population density of each demand zone. 
#Make the spider diagram of the solution with population density as a weight and compare these maps with the solution based on Euclidian distance (above)
weight = en.county$pop.den
eucdists<-euc.dists(en.county, en.county)
weighted_distance = matrix(0,297,297)
for (i in 1:297){
  weighted_distance[i,] = weight[i]*eucdists[i,]
}
#weighted_distance
solution.5_weighteddistance = allocations(en.county, p=5, metric = weighted_distance)
unique(solution.5_weighteddistance$allocation)
distance_total5_weighteddistance= sum(unique(solution.5_weighteddistance$allocdist))
distance_total5_weighteddistance

#Make the spider diagram when consider the effects of weights
weight_new = en.county1$pop.den
eucdists_new<-euc.dists(en.county1, en.county1)
weighted_distance_new = matrix(0,297,297)
for (i in 1:297){
  weighted_distance_new[i,] = weight_new[i]*eucdists_new[i,]
}

solution.5_weighteddistance_new = allocations(en.county1, p=5, metric = weighted_distance_new)
unique(solution.5_weighteddistance_new$allocation)

p <- leaflet(en.county1) %>%
  setView(-78.80,42.90, 9) %>%
  addProviderTiles("MapBox", options = providerTileOptions(
    id = "mapbox.light",
    accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN')))
bins<-c(0,0.02,0.04,0.06,0.08,0.10,0.12,0.14,0.16,0.18,0.2,Inf)
pal<-colorBin("Spectral", domain = solution.5_weighteddistance_new$allocdist, bins = bins, reverse = TRUE)

k=star.diagram(solution.5_weighteddistance_new)

pop.den_map <- p%>%
  addTiles()%>%
  addPolygons(
    fillColor = ~pal(solution.5_weighteddistance_new$allocdist),
    weight = 0.5,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7)%>%
  addLegend(pal = pal, values = ~solution.5_weighteddistance_new$allocdist, opacity = 0.7, title = 'Daily variability of PM 2.5 in 2011',
            position = "bottomright")%>%
  addPolylines(data=k,
               color = '#F00',
               weight = 2)
pop.den_map
#For the p = 5 solution, the best solution under the weighted distance uses sites 9, 232, 218, 74, 156.
#And the total weighted distance is 2089158, which is lower than the distance without considering the weight.

