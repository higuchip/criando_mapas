#Carregando pacotes necessários

library("ggspatial")
library("ggplot2")
library("sf")
library("rnaturalearth")
library("rnaturalearthdata")
library(ggsn)
library(ggmap)

# Obtenção das coordenadas dos pontos
coord_points<- read.table('coords.csv', header = T, dec=",",
                   sep=";", row.names = 1)
coord_inner_background <- coord_points

long_dif <- max(coord_inner_background$long)-min(coord_inner_background$long)
lat_dif <- max(coord_inner_background$lat)-min(coord_inner_background$lat)
dif_degree <- c(long_dif, lat_dif)
coord_inner_background$long[which.min(coord_inner_background$long)]<-
  coord_inner_background$long[which.min(coord_inner_background$long)]-
  max(dif_degree)



#Mapa interno

world <- ne_countries(scale = "medium", returnclass = "sf")

inner_map<-ggplot(data = world) + 
  geom_sf() + coord_sf(xlim = c(-60, -34), ylim = c(-36.7, -20), #coord_sf para definir o recorte de longitude (xlim) e de latitude (ylim) 
                       expand = FALSE)+ xlab("Long") + ylab("Lat")+
  geom_rect(data = data.frame(),aes(xmin = min(coord_inner_background$long), 
                                    xmax = max(coord_inner_background$long)+.5, #geom_rect para definir a área do zoom
                                    ymin = min(coord_inner_background$lat),
                                    ymax = max(coord_inner_background$lat)+0.5),
            colour = "red", fill = "red", alpha=0.5)+
  annotation_scale(location = "bl", width_hint = .5)


#Mapa externo

bc_bbox <- make_bbox(lon = c(coord_inner_background$lon,coord_inner_background$lon+0.5), 
                     lat = c(coord_inner_background$lat, coord_inner_background$lat+0.5), f = 1) #extração do mapa para o zoom
bc_big <- get_map(location = bc_bbox)

outter_map<-ggmap(bc_big) + 
  geom_point(data = coord_points, mapping = aes(x =long, y = lat), 
             color = "red", cex=2, alpha=0.7)+
  scalebar(bc_big,
           dist = 100, dist_unit = "km", st.dist=.05,
           location = "bottomright",
           st.bottom = FALSE, st.color = "black",
           transform = TRUE, model = "WGS84", st.size = 3)
outter_map
# Tema do mapa interno
  
maptheme_inner <- theme(
  axis.text = element_blank(),
  axis.ticks = element_blank(),
  panel.grid = element_blank(),
  panel.border = element_rect(fill = NA, colour = "black"),
  panel.background = element_blank()
)


#Tema do mapa externo

maptheme_outter <- theme(
  axis.text=element_text(size=12),
  axis.title=element_text(size=14,face="bold"),
  axis.ticks = element_blank(),
  panel.grid = element_blank(),
  panel.border = element_rect(fill = NA, colour = "black"),
  panel.background = element_blank()
)

#Geração da Figura

jpeg(filename = "mapa_new.jpg",width = 1800, height = 1800, 
     units = "px",
     quality = 100,
     res = 300, family="serif")
grid.newpage()
vp_outter <- viewport(width = 1, height = 1, x = 0.5, y = 0.5) 
vp_inner <- viewport(width = 0.35, height = 0.35, x = 0.3, y = 0.85) 
print(outter_map + maptheme_outter, vp = vp_outter)
print(inner_map + maptheme_inner, vp = vp_inner)

dev.off()

