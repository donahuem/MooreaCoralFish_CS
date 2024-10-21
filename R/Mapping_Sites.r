##### Map of Moorea and survey locations #####
##### Created by: Danielle Barnas #####
##### Edited by: Callie Stephenson #####
##### Created on: 10/22/2021 #####

##### LOAD LIBRARIES #####
library(here)
library(readr)
library(tidyverse)
library(ggmap)
library(sf)
library(ggrepel)
library(viridis)
library(kriging)
library(ggnewscale)
library(wql)
library(glue)
library(gridExtra)
library(PNWColors)
library(patchwork)

##### READ IN DATA #####
alphatag <- read_csv(here("data", "CowTag_to_AlphaTag.csv")) #I changed this to have V13
meta <- read_csv(here("data", "DB_Full_Metadata.csv")) %>%
  left_join(alphatag) %>%
  filter(Location == "Varari")

#THIS IS GOING TO MAKE THE ALLCHEM JUST HAVE THE CVSEASONAL OF ALL THE NUTRIENTS FROM THE WET SEASON
#WE NEED TO DETERMINE IF THIS IS WHAT I WANT TO REPRESENT IN THIS FIGURE
allchem <- read_csv(here("Data", "March_nutrients_processed.csv")) %>%
  select(CowTagID, Parameters, CVSeasonal) %>%
  pivot_wider(names_from = Parameters, values_from = CVSeasonal)
#Coordinates
kml_data <- st_read(here("Data","Varari_Polygon.kml"))
V_kml <- st_coordinates(kml_data)
#V_kml <- getKMLcoordinates(kmlfile=here("Data","Varari_Polygon.kml"), ignoreAltitude=T) DB code using a retired package

chem <- allchem %>%
  filter(CowTagID != "VSEEP")

##### MAP SITE LOCATIONS #####

# isolate seep point for mapping
seeppt <- meta %>%
  filter(CowTagID == "VSEEP")

# mean lat and long for the maps
LocationGPS <- meta %>%
  group_by(Location) %>% # varari vs cabral
  summarise(lon = median(lon + 0.00013, na.rm = TRUE),
            lat = median(lat, na.rm = TRUE))


# Varari
VarariBaseMap<-get_map(LocationGPS %>% filter(Location == "Varari") %>%
                         select(lon,lat) %>% 
                         mutate(lat = lat + 0.0002),
                       maptype = 'satellite',
                       zoom = 19)

# base map
# Varari
VmapSites <- ggmap(VarariBaseMap) +
  labs(x = "Longitude", y = "Latitude") +  #label x and y axes
  geom_point(data = meta,
             aes(x = lon, y = lat),
             size = 8,
             shape = 21,
             fill = "white",
             color = "black") +
  geom_text(data = meta,
             aes(x = lon, y = lat,
                 label = AlphaTag),
             size = 4) +
  # add the seep point separately
  geom_label(data = seeppt,
             aes(x = lon, y = lat + 0.00003),
             label = "Seep\nA",
             fill = "white") +
  theme(axis.text = element_text(size = 13),
        axis.title = element_text(size = 16))

VmapSites

#ggsave(here("Output", "Varari_map_sites.png"), VmapSites, height = 6, width = 6, device = "png")

# Moorea
MooreaMap<-get_map('Moorea', maptype = 'satellite', zoom = 12)
MooreaMapPlot <- ggmap(MooreaMap) + # base map
  geom_rect(data = LocationGPS, aes(xmin = lon[1] - 0.006, xmax = lon[1] + 0.006, ymin = lat[1] - 0.01, ymax = lat[1] + 0.01), color = "white", alpha = 0, size = 1) +
  labs(x = "Longitude", y = "Latitude") +
  geom_text(data = LocationGPS, aes(label = Location), color = "white", hjust = -0.8, size = 6) + # adds Location names to the right of the boxes
  geom_segment(x = LocationGPS$lon[1] + 0.006, y = LocationGPS$lat[1], xend = LocationGPS$lon[1] + 0.023, yend = LocationGPS$lat[1], color = "white", linewidth = 1) +
  theme(axis.title = element_blank(),
        axis.text = element_text(size = 9)) #taking the text off the axis so it can go in the bigger figure

MooreaMapPlot
#ggsave(here("Output", "PaperFigures", "Moorea_Map.png"), MooreaMapPlot, height = 10, width = 10)





##### KRIGING MAP
#### ASK DANIELLE HOW TO MAKE THIS WORK

# make a function to do all the krigings
Krig_function <-function(dat_in = data, Lat = "lat", Lon = "lon", Param = "Values", poly ) {

  dat <- dat_in[,c(Lon, Lat, Param)]
  names(dat) <- c('Lon', 'Lat', 'Param')

  dat<-dat%>%
    drop_na()

  x <- dat$Lon
  y <- dat$Lat
  z <-dat$Param

  krig1 <- kriging(x, y, z, pixels=500, polygons=poly, lags = 3) ###pixels controls how fine or course you want the prediction data frame to be
  krig2 <- krig1$map
  return(krig2)
}

# And do it "safely"
Krig_function_safe<-safely(Krig_function) # skip the NAs without breaking the code

# plot map function
V_krig_map<-function(datakrig=preds){

  ggmap(VarariBaseMap)+
    geom_point(data=datakrig, aes(x=x, y=y, colour=pred), size=4, alpha=0.5) +
    # geom_point(data = VData, aes(x=lon, y=lat))+
    scale_color_viridis_c(" ", option = "plasma")+
    coord_sf() +
    theme(axis.line=element_blank(),
          axis.text.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank()) +
    theme(panel.grid.major = element_line(color = 'white', linetype = "dashed",size = 0.5),
          plot.background=element_rect(fill='white'))+
    ggtitle(glue("Varari: {.y}"))
  #   ggtitle(paste("Varari",DN, TD))
}

alphameta <- alphatag %>%
  left_join(meta) %>%
  select(AlphaTag, lat, lon)

# add point for seep
seepptkrig <- seeppt %>%
  left_join(allchem) %>%
  select(CowTagID, lat, lon, Phosphate_umolL)

# create my palette
mypalette <- (pnw_palette(name = "Bay", n = 19))

# nest by all parameters, tides, day/Night, Date, etc to make it easy to plot all types of maps
# Varari
Varari_kriging <- allchem %>%
  left_join(meta) %>%
  droplevels() %>%
  left_join(alphatag) %>%
  select(lat, lon, AlphaTag, Phosphate_umolL) %>% # select the values that are important for the kriging
  pivot_longer(cols = Phosphate_umolL, names_to = "Parameter", values_to = "Values") %>%
  group_nest(Parameter) %>% # the parameters to group by
  mutate(preds = map(data, ~Krig_function_safe(dat_in = .x, poly = V_kml)), # run the function for every nested group
         longname = paste(Parameter),
         plots = map2(preds, longname, ~ggmap(VarariBaseMap)+
                        geom_point(data=.x$result, aes(x=x, y=y, colour=pred), size=4, alpha=0.5) +
                        geom_label(data = alphameta, aes(x=lon, y=lat, label = AlphaTag)) +
                        geom_label(data = seeppt,
                                   aes(x = lon, y = lat + 0.00001),
                                   label = "Seep\nA",
                                   fill = "white") +
                        scale_color_gradientn(colors = mypalette,trans = "log") + # log transform pred
                        coord_sf() +
                        labs(color = "CV Phosphate",
                             x = "Longitude",y = "Latitude") +
                        theme(axis.line=element_blank(),
                              axis.text = element_text(size = 10),
                              axis.title = element_text(size = 15),
                              legend.position = "top",
                              panel.grid.major = element_line(color = 'white', linetype = "dashed",size = 0.5),
                              plot.background=element_rect(fill='white')) +
                        # add arrow outline
                        geom_segment(aes(xend = -149.9001, yend = -17.53988,
                                         x = -149.8997, y = -17.54014),
                                     arrow = arrow(length = unit(0.5, "cm")),
                                     size = 3,
                                     color = "black") +
                        # add flow direction arrow
                        geom_segment(aes(xend = -149.9001, yend = -17.53988,
                                         x = -149.8997, y = -17.54014),
                                     arrow = arrow(length = unit(0.5, "cm")),
                                     size = 2,
                                     color = "white")
                        ))
#ggsave(here("output","August2021","Biogeochem", glue("Varari: {.y}.png")),plot)}))
krigPlot <- Varari_kriging$plots[[1]]

krigPlot


for(i in 1:length(Varari_kriging$plots)){
  try({
    ggsave(plot = Varari_kriging$plots[[i]], file = here("Output","PaperFigures","Kriging_V_Map.png"), height = 6, width = 6, device = "png")}, silent = TRUE)
}




### Patch maps for paper visual
mymaps <- VmapSites + inset_element(MooreaMapPlot, left = 0.5, bottom = 0.5, right = 1, top = 1.05) +
  plot_annotation(tag_levels = list(c('B','A'))) + # moorea labeled A, overlaid atop varari labeled B
  theme(plot.tag = element_text(size = c(15,15)))

#mymaps <- krigPlot + inset_element(MooreaMapPlot, left = 0.5, bottom = 0.5, right = 1, top = 1.05) +
#  plot_annotation(tag_levels = list(c('B','A'))) + # moorea labeled A, overlaid atop varari labeled B
#  theme(plot.tag = element_text(size = c(15,15)))
#mymaps

ggsave(here("Output","Figure1_Maps.png"),mymaps, height = 6, width = 6, device = "png")


