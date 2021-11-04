library(package="Ecdat")
search()
range(Caschool$mathscr)
quantile(Caschool$mathscr, probs = c(0.25, 0.50, 0.75), names = FALSE) #2a




summary(Caschool$mathscr) #2c

mathscrL <- cut(Caschool$mathscr, breaks = quantile(Caschool$mathscr, names = FALSE), labels = c("M4", "M3", "M2", "M1"), include.lowest = TRUE, right = FALSE, ordered_result = FALSE)

Caschool <- cbind(Caschool,mathscrL)
print(Caschool)

df <- data.frame(Caschool)
str(df)


#Use facet_wrap to visualize and interpret how variables expnstu and elpct 
#change for each category of the variable mathscrL created in question 2c.

library(ggplot2) #2e
dfnew <- data.frame(Caschool)
ggplot(data=dfnew, mapping=aes(x=expnstu, y=elpct)) + geom_point() + facet_wrap(facets=~mathscrL)



#Use ggmap to visualize the density map of the enrollment totals of the counties in QUESTION 2g
#the dataset on a physical map of California. Please use Figure 3.64 as your reference
#guide. Note that you may need to create a new dataset that aggregates the
#total enrollments of each county in California, because the dataset has multiple
#schools per county. There are multiple ways to aggregate data in R, one approach
#is to use the aggregate function along with the sum function as argument.

xpy <- aggregate(Caschool$enrltot, by=list(Category=Caschool$county), FUN=sum)

str(xpy)

#install.packages("mapsapi")
library(mapsapi)

library(ggmap)

library(maptools)

#register_google(key="AIzaSyAZ89FnRrtP7jRLROii7HhP-5zAYSGAQqA")


US.shape <- readShapeSpatial(fn="./tl_2021_us_state/tl_2021_us_state.shp", proj4string = CRS("+proj=longlat +datum=WGS84"))
class(US.shape)

str(US.shape@data)

US.shape.df <- fortify(US.shape)
head(US.shape.df)


str(xpy)
ggplot() + geom_polygon(data=US.shape.df, mapping=aes(x=long, y=lat, group=group),
                        color="black", fill="white", size=0.2) + scale_x_continuous(limits=c(-130,-65)
                        ) + scale_y_continuous(limits=c(24,50))

library(googleway) ## you can use separate API keys for different APIs 
set_key( "GOOGLE_API_KEY", api = "geocode") 
set_key( "GOOGLE_MAP_KEY", api = "map") ## you can view the keys you have with 
google_keys() 
google_map( location = c(52, 0), zoom = 6 )


CAmap <- ggmap(ggmap=get_map("California, USA", source="google", zoom=6, color="bw"))
CAmap

CAmap + stat_density2d(data=LAdataR, mapping=aes(x=lon, y=lat, alpha=..level..,
                                                   fill=..level..), geom="polygon", bins=150) + scale_fill_gradient(low = "yellow"
                                                                                                                    , high = "red") + scale_alpha(guide=FALSE)
