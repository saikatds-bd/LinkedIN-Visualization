install.packages("tidyverse")
install.packages("ggmap")
library(tidyverse)
library(ggmap)
college <- read_csv("E:/LinkedIN learning/Ex_Files_Data_Visualization_R_ggplot2/college.csv")
summary(college)

college <- college %>% 
  mutate(state=as.factor(state), region=as.factor(region),
         highest_degree=as.factor(highest_degree), control=as.factor(control),
         gender=as.factor(gender))
summary(college)

unique(college$loan_default_rate)

college <- college %>%
  mutate(loan_default_rate=as.numeric(loan_default_rate))
summary(college)

#alpha determines the opacity
ggplot(data=college) +
  geom_point(mapping = aes(x=tuition, y=sat_avg, color = control,
                           size=undergrads), alpha=1/2)

#line plots
ggplot(data=college) +
  geom_line(mapping = aes(x=tuition, y=sat_avg, color = control)) +
  geom_point(mapping = aes(x=tuition, y=sat_avg, color = control))
#se determines standard error (background gray shade of the smooth line)
ggplot(data=college,mapping = aes(x=tuition, y=sat_avg, color = control)) + geom_smooth(se=FALSE) + geom_point(alpha=1/25)

#bar and collumn
ggplot(data=college) + geom_bar(mapping=aes(x=region,fill=control))

college %>%
  group_by(region) %>%
  summarise(average_tuition=mean(tuition)) %>%
  ggplot() + geom_col(mapping=aes(x=region, y=average_tuition))

#Histogram
ggplot(data=college) +
  geom_histogram(mapping=aes(x=undergrads),origin=0,binwidth = 10000)

#boxplot
ggplot(data=college) +
  geom_boxplot(mapping = aes(x=control, y=tuition))

#modifying the background
ggplot(data=college) + geom_bar(mapping=aes(x=region,fill=control)) +
  theme(panel.background = element_blank())+
  theme(plot.background = element_blank()) +
  theme(panel.grid.major.y = element_line(color = "grey"))

#Modifying the axes
ggplot(data=college) + geom_bar(mapping=aes(x=region,fill=control)) +
  theme(panel.background = element_blank())+
  theme(plot.background = element_blank()) +
  theme(panel.grid.major.y = element_line(color = "grey")) +
  ylab("Number of Schools") +
  xlab("Region") +
  ylim(0,500)

#Modifying the scales
ggplot(data=college) + geom_bar(mapping=aes(x=region,fill=control)) +
  theme(panel.background = element_blank())+
  theme(plot.background = element_blank()) +
  theme(panel.grid.major.y = element_line(color = "grey")) +
  scale_x_discrete(name="Region") +
  scale_y_continuous(name="Number of Schools", limits = c(0,500)) +
  scale_fill_manual(values = c("orange", "blue"),
                    guide=guide_legend(title="Institution Type", nrow =1,
                                       label.position="bottom",
                                       keywidth = 2.5)) +
  theme(legend.position = "top")

#Annotation
ggplot(data=college) +
  geom_point(mapping = aes(x=tuition, y=sat_avg, color = control,
                           size=undergrads), alpha=1/2) +
  annotate("text", label="Elite Privates", x=45000, y=1450) +
  geom_hline(yintercept = mean(college$sat_avg))+
  annotate("text", label = "Mean SAT", x=47500, y=mean(college$sat_avg)-15) +
  geom_vline(xintercept = mean(college$tuition)) +
  annotate("text", label = "Mean Tuition", y=700, x=mean(college$tuition)+7500) +
  theme(panel.background = element_blank(), legend.key = element_blank()) +
  scale_color_discrete(name="Institution Type") +
  scale_size_continuous(name = "Undergraduates") +
  scale_x_continuous(name="Tuition") +
  scale_y_continuous(name = "SAT score")

#Adding a title  
ggplot(data=college) + geom_bar(mapping=aes(x=region,fill=control)) +
  theme(panel.background = element_blank())+
  theme(plot.background = element_blank()) +
  theme(panel.grid.major.y = element_line(color = "grey")) +
  scale_x_discrete(name="Region") +
  scale_y_continuous(name="Number of Schools", limits = c(0,500)) +
  scale_fill_manual(values = c("orange", "blue"),
                    guide=guide_legend(title="Institution Type", nrow =1,
                                       label.position="bottom",
                                       keywidth = 2.5)) +
  theme(legend.position = "top") + ggtitle("More Colleges are in the southern US than any other region", subtitle = "Source : US Dept. of Education")

#Using themes
library(ggthemes)
ggplot(data=college) + 
  geom_bar(mapping=aes(x=region,fill=control)) +
  theme_solarized()

####Visualizing data with maps

install.packages("devtools")
devtools::install_github("dkahle/ggmap")
library(ggmap)
register_google(key = "*******")

qmap("New York, NY", zoom=10)
nyc_map <- get_map("New York, NY", zoom=10)
ggmap(nyc_map)

#Geocoding
nyc <- geocode("New York, NY")
nyc

lynda <- geocode("Lynda.com")
lynda

white_house <- geocode("White House")
white_house

nyc_map <- get_map(nyc)
ggmap(nyc_map)

ggmap(get_map(lynda))

whitehousemap <- ggmap(get_map(white_house, zoom = 18))
whitehousemap

#Changing Map type
nyc <- geocode("New York, NY")
ggmap(get_map(nyc,maptype = "watercolor"))


#plotting points on a map
nyc <- geocode("New York, NY")
usa <- geocode("United States")

ggmap(get_map(usa, zoom = 4)) +
  geom_point(mapping=aes(x=lon, y=lat), color="red", data=nyc)

placenames <- c("New York, NY", "White House", "Lynda.com", "Mt. Rushmore", "The Alamo")
locations <- geocode(placenames)

places <- tibble(name=placenames, lat = locations$lat, lon = locations$lon)
ggmap(get_map(usa, zoom = 4, maptype = "toner-background")) +
  geom_point(mapping=aes(x=lon, y=lat), color="red", data=places) +
  geom_text(mapping = aes(x=lon, y=lat, label=name), color="red", data = places, nudge_y = 1)


#Building a map manually
states <- map_data("state")
install.packages("mapproj")
ggplot(data=states, mapping = aes(x=long, y=lat, group=group)) +
  geom_polygon() +
  coord_map() +
  theme(axis.ticks = element_blank(), axis.title = element_blank(), axis.text = element_blank(),
        panel.background = element_blank())

#creating a chloreopath map
college_summary <- college %>% 
  group_by(state) %>%
  summarize(schools=n())
college_summary

college_summary <- college_summary %>%
  mutate(region=as.character(setNames(str_to_lower(state.name),
                                      state.abb)[as.character(state)]))
college_summary <- college_summary %>%
  mutate(region=ifelse(as.character(state)=="DC", "district of columbia", region))

college_summary

mapdata <- merge(states, college_summary, by="region")
mapdata

ggplot(data=mapdata) + geom_polygon(mapping = aes(x=long, y=lat, group=group, fill=schools)) +
  coord_map() +
  theme(plot.background = element_blank(),
        panel.background = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank()) +
  scale_fill_gradient(low="beige", high = "red")

#The challenge
ggplot(states) +
  geom_polygon(mapping=aes(x=long,y=lat,group=group), color="grey", fill="beige") +
  coord_map() +
  theme(plot.background=element_blank(), 
        panel.background = element_blank(), 
        axis.title=element_blank(), 
        axis.ticks=element_blank(), 
        axis.text=element_blank()) +
  geom_point(data=college, mapping = aes(x=lon, y=lat))

college <- college %>% 
  filter(state!="AK" & state!="HI")

#adding institution size and control
ggplot(states) +
  geom_polygon(mapping=aes(x=long,y=lat,group=group), color="grey", fill="beige") +
  coord_map() +
  theme(plot.background=element_blank(), 
        panel.background = element_blank(), 
        axis.title=element_blank(), 
        axis.ticks=element_blank(), 
        axis.text=element_blank()) +
  geom_point(data=college, mapping = aes(x=lon, y=lat, color=control, size=undergrads))

#Zooming on California
california <- map_data(map="county", region = "California")
ggplot(california) +
  geom_polygon(mapping=aes(x=long,y=lat,group=group), color="grey", fill="beige") +
  coord_map() +
  theme(plot.background=element_blank(), 
        panel.background = element_blank(), 
        axis.title=element_blank(), 
        axis.ticks=element_blank(), 
        axis.text=element_blank()) +
  geom_point(data=college, mapping = aes(x=lon, y=lat, color=control, size=undergrads, alpha = 0.6))

college <- college %>%
  filter(state=="CA")

#adding city names
city_names<- c("Los Angeles", "San Diego", "San Francisco", "San Jose",
               "Fresno", "Sacramento")
locations <- geocode(city_names)

cities <- tibble(name=city_names, lat=locations$lat, lon=locations$lon)

ggplot(california) +
  geom_polygon(mapping=aes(x=long,y=lat,group=group), color="grey", fill="beige") +
  coord_map() +
  theme(plot.background=element_blank(), 
        panel.background = element_blank(), 
        axis.title=element_blank(), 
        axis.ticks=element_blank(), 
        axis.text=element_blank()) +
  geom_point(data=college, mapping = aes(x=lon, y=lat, color=control, size=undergrads), alpha = 0.6) + 
  geom_text(data=cities, mapping = aes(x=lon, y=lat, label=name)) +
  scale_size_continuous(name = "Undergraduate Population") +
  scale_color_discrete(name="Institutional Control") +
  theme(legend.key = element_blank()) +
  ggtitle("Most Californian Collges are Located in Large Cities",
          subtitle="Source : U.S. Department of Education")


