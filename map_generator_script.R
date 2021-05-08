#Carregando pacotes necessários

library("ggspatial")
library("ggplot2")
library("sf")
library("rnaturalearth")
library("rnaturalearthdata")
library(ggsn)
library(ggmap)

# Obtenção das coordenadas dos pontos
coord<- read.table('coords.csv', header = T, dec=",",
                   sep=";", row.names = 1)

#Mapa interno

world <- ne_countries(scale = "medium", returnclass = "sf")

inner_map<-ggplot(data = world) + 
  geom_sf() + coord_sf(xlim = c(-60, -34), ylim = c(-36.7, -20), #coord_sf para definir o recorte de longitude (xlim) e de latitude (ylim) 
                       expand = FALSE)+ xlab("Long") + ylab("Lat")+
  geom_rect(data = data.frame(),aes(xmin = min(coord$long), xmax = max(coord$long), #geom_rect para definir a área do zoom
                                    ymin = min(coord$lat),
                                    ymax = max(coord$lat)),
            colour = "red", fill = "red", alpha=0.5)+
  annotation_scale(location = "bl", width_hint = .5)


#Mapa externo

bc_bbox <- make_bbox(lon = coord$lon, lat = coord$lat, f = 1) #extração do mapa para o zoom
bc_big <- get_map(location = bc_bbox)
outter_map<-ggmap(bc_big) + 
  geom_point(data = coord, mapping = aes(x =long, y = lat), 
             color = "red", cex=2, alpha=0.7)+
  scalebar(x.min = bc_bbox["left"]+.1, x.max = bc_bbox["right"]+1,
           y.min = bc_bbox["bottom"]+.1, y.max = bc_bbox["top"],
           dist = 20, dist_unit = "km", st.dist=.05,
           location = "bottomleft",
           st.bottom = FALSE, st.color = "black",
           transform = TRUE, model = "WGS84", st.size = 3)

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

jpeg(filename = "mapa.jpg",width = 1800, height = 1800, 
     units = "px",
     quality = 100,
     res = 300, family="serif")
grid.newpage()
vp_outter <- viewport(width = 1, height = 1, x = 0.5, y = 0.5) 
vp_inner <- viewport(width = 0.3, height = 0.3, x = 0.37, y = 0.86) 
print(outter_map + maptheme_outter, vp = vp_outter)
print(inner_map + maptheme_inner, vp = vp_inner)

dev.off()

