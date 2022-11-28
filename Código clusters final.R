rm(list = ls())

####Paquetes####

library(data.table)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggrepel)
library(stringi)
library(DataViz)
library(factoextra)
library(purrr)
library(ggbiplot)
library(e1071)
library(gridExtra)
library(NbClust)
library(wesanderson)
library(readxl)
library(stringdist)
library(ggcorrplot)
library(qgraph)
library(scales)
library(ggalluvial)
library(forcats)
####Funciones####

simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}

"coldiss" <- function(D, nc = 4, byrank = TRUE, diag = FALSE)
{
  require(gclus)
  
  if (max(D)>1) D <- D/max(D)
  
  if (byrank) {
    spe.color = dmat.color(1-D, cm.colors(nc))
  }
  else {
    spe.color = dmat.color(1-D, byrank=FALSE, cm.colors(nc))
  }
  
  spe.o = order.single(1-D)
  speo.color = spe.color[spe.o,spe.o]
  
  op = par(mfrow=c(1,2), pty="s")
  
  if (diag) {
    plotcolors(spe.color, rlabels=attributes(D)$Labels, 
               main="Dissimilarity Matrix", 
               dlabels=attributes(D)$Labels)
    plotcolors(speo.color, rlabels=attributes(D)$Labels[spe.o], 
               main="Ordered Dissimilarity Matrix", 
               dlabels=attributes(D)$Labels[spe.o])
  }
  else {
    plotcolors(spe.color, rlabels=attributes(D)$Labels, 
               main="Dissimilarity Matrix")
    plotcolors(speo.color, rlabels=attributes(D)$Labels[spe.o], 
               main="Ordered Dissimilarity Matrix")
  }
  
  par(op)
}


stand <- function(x){(x-min(x))/(max(x)-min(x))}



limpiar.nombre <- function(x){
  y <- colnames(x)
  y <- gsub(" ", ".", y)
  y <- gsub("\\.de\\.", ".", y)
  y <- gsub("\\/", ".", y)
  y <- gsub("\\°", "", y)
  y <- trimws(y)
  y <- tolower(y)
  y <- stri_trans_general(y, "Latin-ASCII")
  return(y)
}

#Guardar leyenda del gráfico

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

####Datos####

setwd("C:/Users/lgarcia/Documents/2019/08 Agosto/Clusters")
setwd("C:/Users/felip/Google Drive/Clusters CAVD")

ssra <- fread("SSR.csv") 
colnames(ssra) <- limpiar.nombre(ssra)    

ipa <- fread("Agenda.csv") 
colnames(ipa) <- limpiar.nombre(ipa)

ingesa <- fread("Gestión.csv")
colnames(ingesa) <- limpiar.nombre(ingesa)
ingesa <- ingesa %>% 
  select( -num) %>%
  dplyr::rename(cavd2 = cavd)

coba <- fread("cobertura.csv")

causasa <- fread("Causas.csv", header = T) 
colnames(causasa) <- limpiar.nombre(causasa)

causasa <- causasa %>%
  mutate(poder.simple = as.Date(fecha.poder.simple, format = "%d-%m-%Y"),
         patrocinio.poder = as.Date(fecha.patrocinio.poder, format = "%d-%m-%Y"),
         querella = as.Date(fecha.querella, format = "%d-%m-%Y"),
         cavd2 = tolower(cavd)
  )
causasa$cavd2 <- sapply(causasa$cavd2, simpleCap)

macroa <- fread("Macrosocial.csv")
colnames(macroa) <- limpiar.nombre(macroa) 

macro <- macroa %>%
  filter(ano.actividad == 2018) %>%
  mutate(cavd = trimws(gsub("\\d+", "", cavd)
  ),
  cavd2 = recode(cavd,
                 "Ancud" = "Castro",
                 "Peñalolen" = "Providencia",
                 "Aysén" = "Coyhaique",
                 "Collipulli" = "Angol",
                 "Til Til" = "Colina",
                 "Purranque" = "Osorno",
                 "Collipulli" = "Temuco",
                 "Puerto Natales" = "Punta Arenas",
                 "San Antonio" = "Valparaíso",
                 "Quellón" = "Castro",
                 "Los Angeles" = "Los Ángeles")
  ) %>%
  modify_if(is.character, as.factor)



ssra <- ssra %>% 
  mutate(cavd = trimws(gsub("\\d+", "", cavd)),
         cavd2 = recode(cavd,
                        "Ancud" = "Castro",
                        "Peñalolen" = "Providencia",
                        "Aysén" = "Coyhaique",
                        "Collipulli" = "Angol",
                        "Til Til" = "Colina",
                        "Purranque" = "Osorno",
                        "Collipulli" = "Temuco",
                        "Puerto Natales" = "Punta Arenas",
                        "San Antonio" = "Valparaíso",
                        "Quellón" = "Castro",
                        "Los Angeles" = "Los Ángeles"),
         delito2 = recode(delito
                          ,"Cuasidelitos de homicidio"="Cuasidelito de lesiones u homicidio"
                          ,"Cuasidelitos de lesiones"="Cuasidelito de lesiones u homicidio"
                          ,"Delitos sexuales"="Delitos sexuales"
                          ,"Femicidio"="Homicidio"
                          ,"Femicidio Consumado"="Homicidio"
                          ,"Femicidio frustrado"="Otro tipo de delito"
                          ,"Homicidio"="Homicidio"
                          ,"Lesiones graves"="Lesiones"
                          ,"Lesiones graves gravísimas"="Lesiones"
                          ,"Lesiones leves"="Lesiones"
                          ,"Lesiones menos graves"="Lesiones"
                          ,"No corresponde"="Otro tipo de delito"
                          ,"Otro delito"="Otro tipo de delito"
                          ,"Parricidio"="Homicidio"
                          ,"Parricidio consumado"="Homicidio"
                          ,"Parricidio frustrado"="Otro tipo de delito"
                          ,"Robo con homicidio"="Homicidio"
                          ,"Robo con intimidación"="Robos violentos"
                          ,"Robo con violación"="Delitos sexuales"
                          ,"Robo con violencia"="Robos violentos"
                          ,"Secuestro"="Otro tipo de delito"
                          ,"Sin información"="Otro tipo de delito"
                          ,"Sustracci¢n  menores"="Otro tipo de delito"
                          ,"Sustracción menores"="Otro tipo de delito"
                          ,"Trata de personas fines laborales"="Otro tipo de delito"
                          ,"Trata de personas fines sexuales"="Otro tipo de delito"
                          ,"Trata de Personas Tráfico Ilícito de Inmigrantes"="Otro tipo de delito"),
         etario = ifelse(edad < 18, "NNA (Menores de 18 años)", if_else(
           (edad >= 18 & edad < 30), "Jóvenes (18 a 29 años)", ifelse(
             (edad >= 30 & edad < 60), "Adultos (30 a 59 años)",
             if_else(edad >= 60, "Adultos mayores (60 años o más)",
                     "Edad desconocida")))),
         etario=replace_na(etario,"Edad desconocida"),
         via.ingreso=recode(via.ingreso
                            ,"Sename"="Otros derivantes"
                            ,"Sernam"="Otros derivantes"
                            ,"Servicios de salud"="Otros derivantes"
                            ,"Circuito de femicidio"="Otros derivantes"
                            ,"1ª R SIIn - Interna"="SIIN"
                            ,"1ª R SOI - Interna"="Orientación de SOI"
                            ,"1ª R SOI - Sin teléfono"="SOI (Tercer contacto o Sin teléfono)"
                            ,"1ª R CCP - Interna"="CCP"
                            ,"1ª R SOI - 3er Contacto"="SOI (Tercer contacto o Sin teléfono)"
                            ,"Demanda espontánea WEB"="Demanda espontánea"
                            ,"SRAV - Circuito Femicidio"="Otros derivantes")
  ) %>%
  filter(cavd2 %in% c("Cauquenes", "San Antonio") == F)


cob <- coba %>% 
  mutate(po = as.numeric(po)) %>%
  group_by(cavd) %>% 
  filter(cavd %in% c("Cauquenes",
                     "San Antonio") == F) %>%
  dplyr::summarise(po = sum(po)) %>%
  mutate(po = round(po, 0)) %>%
  filter(cavd !="") %>%
  dplyr::rename("cavd2" = "cavd") 



####car.Limpieza####

colnames(ssra) <- limpiar.nombre(ssra)
colnames(ipa) <- limpiar.nombre(ipa)

ssr <- ssra  %>%
  filter(ano.ingreso == 2018,
         cavd2 %in% c("Cauquenes",
                      "Las Condes") == F &
           delito %in% c("Trata de personas fines laborales",
                         "Trata de personas fines sexuales" ,
                         "Sustracción menores",
                         "Secuestro") == F) 


grupoans <- ssr %>%
  group_by(cavd2) %>%
  dplyr::summarise(n = n()) %>%
  full_join(ssr %>% 
              group_by(cavd2, sexo) %>%
              dplyr::summarise(n = n()) %>%
              spread(sexo, value = n) %>%
              select(-`No definido`)
  ) %>%
  mutate(Hombre = Hombre / n,
         Mujer = Mujer / n) %>%
  full_join(ssr %>% 
              group_by(cavd2, delito2) %>%
              dplyr::summarise(n = n()) %>%
              spread(delito2, value = n)
  ) %>%
  full_join(ssr %>% 
              group_by(cavd2) %>%
              dplyr::summarise(m.edad = mean(edad, na.rm = T))
  ) %>%
  mutate(`Cuasidelito` = `Cuasidelito de lesiones u homicidio`/ n,
         `Delitos sexuales` =  `Delitos sexuales`/ n,
         `Homicidio` =  `Homicidio`/ n,
         `Lesiones` =  `Lesiones`/ n,
         `Robos violentos` =  `Robos violentos`/ n
         #, n = log(n)
  ) %>%
  lapply(replace_na, 0) %>%
  data.frame() %>%
  full_join(cob, by = "cavd2")

grupoa <- grupoans %>% 
  modify_if(is.numeric, scale)


grupo <- grupoa %>%
  column_to_rownames("cavd2") %>%
  scale() %>%
  data.frame() %>%
  select(-Hombre,
         -Otro.tipo.de.delito,
         -Cuasidelito.de.lesiones.u.homicidio)



grupo %>%
  keep(is.numeric) %>%                     # Keep only numeric columns
  gather() %>%                             # Convert to key-value pairs
  ggplot(aes(value)) +                     # Plot the values
  facet_wrap(~ key, scales = "free") +   # In separate panels
  geom_density()                         # as density

####car.PCA####

pca <- prcomp(grupo, 
              center = TRUE,
              scale. = TRUE)

summary(pca)
ggbiplot(pca, labels = row.names(grupo))
ggbiplot(pca, choices = c(3,4), labels = row.names(grupo))




####car.Conglomerados####

#grupo <- grupo[rownames(grupo) != 'Coyhaique', ]

res.dist <- get_dist(grupo, method = "pearson")

fviz_nbclust(keep(grupo, is.numeric), FUNcluster = kmeans, nboot = 500, k.max = 6)
fviz_nbclust(keep(grupo, is.numeric), FUNcluster = kmeans, "wss", k.max = 6)

gruk2 <- eclust(keep(grupo, is.numeric), "kmeans", k = 2, graph = F, nboot = 500)
gruk3 <- eclust(keep(grupo, is.numeric), "kmeans", k = 3, graph = F, nboot = 500)
gruk4 <- eclust(keep(grupo, is.numeric), "kmeans", k = 4, graph = F, nboot = 500)
gruk5 <- eclust(keep(grupo, is.numeric), "kmeans", k = 5, graph = F, nboot = 500)
gruk9 <- eclust(keep(grupo, is.numeric), "kmeans", k = 9, graph = F, nboot = 500)

gruh2 <- eclust(keep(grupo, is.numeric), "hclust", k = 2, graph = F, nboot = 500)
gruh3 <- eclust(keep(grupo, is.numeric), "hclust", k = 3, graph = F, nboot = 500)
gruh4 <- eclust(keep(grupo, is.numeric), "hclust", k = 4, graph = F, nboot = 500)
gruh5 <- eclust(keep(grupo, is.numeric), "hclust", k = 5, graph = F, nboot = 500)
gruh6 <- eclust(keep(grupo, is.numeric), "hclust", k = 6, graph = F, nboot = 500)

grupo$k2 <- factor(gruk2$cluster)
grupo$k3 <- factor(gruk3$cluster)
grupo$k4 <- factor(gruk4$cluster)
grupo$k5 <- factor(gruk5$cluster)
grupo$k9 <- factor(gruk9$cluster)

grupo$h2 <- factor(gruh2$cluster)
grupo$h3 <- factor(gruh3$cluster)
grupo$h4 <- factor(gruh4$cluster)
grupo$h5 <- factor(gruh5$cluster)
grupo$h6 <- factor(gruh6$cluster)

fviz_silhouette(gruk4)

mds <- keep(grupo, is.numeric) %>%
  dist(method = "euclidean") %>%          
  cmdscale() %>%
  as_tibble()
colnames(mds) <- c("Dim.1", "Dim.2")

mds$k2 = grupo$k2
mds$k3 = grupo$k3
mds$k4 = grupo$k4
mds$k5 = grupo$k5
mds$k6 = grupo$k6

mds$h2 = grupo$h2
mds$h3 = grupo$h3
mds$h4 = grupo$h4

ggscatter(mds, x = "Dim.1", y = "Dim.2", 
          label = rownames(grupo),
          size = 1,
          repel = TRUE,
          color = "k4", 
          ellipse = TRUE,
          ellipse.type = "convex"
)


####ges.Limpieza####

###Vigentes por dia####

fechas <- as.Date(as.Date("2018-01-01 UTC") : as.Date("2018-12-31 UTC"),
                  origin = "1970-01-01")
feriados <- as.Date(c(
  "2018-01-01 UTC",
  "2018-03-30 UTC",
  "2018-03-31 UTC",
  "2018-05-01 UTC",
  "2018-05-21 UTC",
  "2018-07-02 UTC",
  "2018-07-16 UTC",
  "2018-08-15 UTC",
  "2018-09-17 UTC",
  "2018-09-18 UTC",
  "2018-09-19 UTC",
  "2018-10-15 UTC",
  "2018-11-01 UTC",
  "2018-11-02 UTC",
  "2018-12-08 UTC",
  "2018-12-25 UTC"
), origin = "1970-01-01")

fechasa <- fechas[!weekdays(fechas) %in% c('sábado','domingo')]
fechasa <- fechas[fechas %in% feriados == F]

fechas <- rep(fechasa,length(levels(factor(ssr$cavd2))))

for(i in 1 : length(levels(factor(ssr$cavd2)))){
  if(i == 1){
    x <- rep(levels(factor(ssr$cavd2))[i], length(fechasa))
  } else{
    y <- rep(levels(factor(ssr$cavd2))[i], length(fechasa))
    x <- c(x, y)
  }
}

df.fechas <- data.frame(fecha = fechas,
                        cavd = x)
cbind(fecha <- fechas)

ing.cavd <- ssra %>%
  mutate(fecha.ingreso = as.Date(fecha.ingreso, format = "%d-%m-%Y")) %>%
  group_by(fecha.ingreso, cavd2) %>%
  dplyr::summarise(ing = n()) %>%
  dplyr::rename(fecha = fecha.ingreso) %>%
  full_join(ssra %>%
              mutate(fecha.finalizacion = as.Date(fecha.finalizacion, format = "%d-%m-%Y")) %>%
              group_by(fecha.finalizacion, cavd2) %>%
              dplyr::summarise(fin = n()) %>%
              dplyr::rename(fecha = fecha.finalizacion), 
            by = c("fecha", "cavd2")
  ) %>%
  mutate(ing = replace_na(ing, 0),
         fin = replace_na(fin, 0),
         suming = NA,
         sumfin = NA) %>%
  arrange(cavd2, fecha)

for(i in 1 : nrow(ing.cavd)){
  if(i == 1){
    ing.cavd$suming[i] = ing.cavd$ing[i]
    ing.cavd$sumfin[i] = ing.cavd$fin[i]
  } else if (ing.cavd$cavd2[i] != ing.cavd$cavd2[i - 1]){
    ing.cavd$suming[i] = ing.cavd$ing[i]
    ing.cavd$sumfin[i] = ing.cavd$fin[i]
  } else{
    ing.cavd$suming[i] = ing.cavd$ing[i] + ing.cavd$suming[i - 1]
    ing.cavd$sumfin[i] = ing.cavd$fin[i] + ing.cavd$sumfin[i - 1]
  }
}

vig.cavd <- ing.cavd %>%
  filter(fecha %in% fechas) %>%
  mutate(vig = suming - sumfin) 

vig.cavd.mean <- vig.cavd %>%
  group_by(cavd2) %>%
  dplyr::summarise(vig = mean(vig))

####Prestaciones####

fechasa <- factor(fechasa)

prest <- ipa %>%
  mutate(cavd = trimws(gsub("\\d+", "", cavd)),
         cavd2 = recode(cavd,
                        "Ancud" = "Castro",
                        "Peñalolen" = "Providencia",
                        "Aysén" = "Coyhaique",
                        "Collipulli" = "Angol",
                        "Til Til" = "Colina",
                        "Purranque" = "Osorno",
                        "Collipulli" = "Temuco",
                        "Puerto Natales" = "Punta Arenas",
                        "San Antonio" = "Valparaíso",
                        "Quellón" = "Castro",
                        "Los Angeles" = "Los Ángeles"),
         esp2 = gsub(", Coordinador", "", especialidad),
         fecha.ingreso = factor(as.Date(fecha.ingreso)),
         esp2 = factor(esp2),
         categoria = factor(categoria)
  ) %>%
  filter(esp2 %in% c("Abogado", "Profesional Psicólogo", "Social") &
           categoria == "Directa c/sesion" &
           fecha.ingreso %in% fechasa) %>%
  group_by(cavd2, fecha.ingreso, esp2) %>%
  tally() %>%
  ungroup() %>%
  complete(cavd2, fecha.ingreso, esp2,
           fill = list(n = 0)) %>%
  filter(esp2 %in% c("Abogado", "Profesional Psicólogo", "Social") &
           fecha.ingreso %in% fechasa) %>%
  group_by(cavd2, esp2) %>%
  dplyr::summarise(m = sum(n)) %>%
  spread(esp2, value = m)

####Ci_Especialidad####

####Asistencia####

asist <- ipa %>%
  filter(ano.ingreso == 2018 
         & `sub-categoria`%in%c( "SC-Directa c/sesion",
                                 "Entrevista de ingreso",
                                 "Valoración")) %>%
  mutate(cavd = recode(cavd, "35 Los Angeles" = "35 Los Ángeles")) %>%
  group_by(cavd) %>%
  dplyr::summarise(n = n()) %>%
  full_join(ipa %>%
              mutate(cavd = recode(cavd, "35 Los Angeles" = "35 Los Ángeles")) %>%
              filter(ano.ingreso == 2018 
                     & `sub-categoria`%in%c( "SC-Directa c/sesion",
                                             "Entrevista de ingreso",
                                             "Valoración")) %>%
              group_by(cavd, asistencia) %>%
              filter(asistencia == "Si") %>%
              dplyr::summarise(n = n()),
            by = "cavd") %>%
  mutate(p = n.y / n.x) %>%
  mutate(cavd2 = trimws(gsub("\\d+ ", "", cavd))) %>%
  dplyr::select(cavd2, asistencia = p) %>%
  filter(cavd2 %in% ssr$cavd2)


#Llenar especialidades vacías
{
  ipa$especialidad <- if_else(ipa$nombre.usuario == "Alejandra Jimena Saez Morales", 
                              "Profesional Psicólogo", 
                              ipa$especialidad)
  ipa$especialidad <- if_else(ipa$nombre.usuario == "Almendra Mikela Fernández Sandoval", 
                              "Social", 
                              ipa$especialidad)
  ipa$especialidad <- if_else(ipa$nombre.usuario == "Alvaro Javier Reyes Muñoz", 
                              "Profesional Psicólogo", 
                              ipa$especialidad)
  ipa$especialidad <- if_else(ipa$nombre.usuario == "Brusiel Arlette Bustos Peña", 
                              "Profesional Psicólogo", 
                              ipa$especialidad)
  ipa$especialidad <- if_else(ipa$nombre.usuario == "Cristian Mauricio Cortés Reyes", 
                              "Profesional Psicólogo, Coordinador", 
                              ipa$especialidad)
  ipa$especialidad <- if_else(ipa$nombre.usuario == "Danila Cecilia Bastias Herrera", 
                              "Abogado", 
                              ipa$especialidad)
  ipa$especialidad <- if_else(ipa$nombre.usuario == "Danna Elizabeth Garbarino Correa", 
                              "Abogado", 
                              ipa$especialidad)
  ipa$especialidad <- if_else(ipa$nombre.usuario == "Ervinzon Misael Gallardo Pinares", 
                              "Social, Coordinador", 
                              ipa$especialidad)
  ipa$especialidad <- if_else(ipa$nombre.usuario == "Felipe Andres Vergara Bravo", 
                              "Abogado, Coordinador", 
                              ipa$especialidad)
  ipa$especialidad <- if_else(ipa$nombre.usuario == "Flor Maria Fernandez Marambio", 
                              "Social, Coordinador", 
                              ipa$especialidad)
  ipa$especialidad <- if_else(ipa$nombre.usuario == "Ignacio Antonio Jara Sánchez", 
                              "Abogado, Coordinador", 
                              ipa$especialidad)
  ipa$especialidad <- if_else(ipa$nombre.usuario == "Javiera Patricia Ormazábal Saavedra", 
                              "Profesional Psicólogo", 
                              ipa$especialidad)
  ipa$especialidad <- if_else(ipa$nombre.usuario == "Jorge Andres Godoy Donoso", 
                              "Profesional Psicólogo", 
                              ipa$especialidad)
  ipa$especialidad <- if_else(ipa$nombre.usuario == "Karen Ines Matus Castillo", 
                              "Social", 
                              ipa$especialidad)
  ipa$especialidad <- if_else(ipa$nombre.usuario == "Lilian Mercedes Zuñiga Lizama", 
                              "Abogado", 
                              ipa$especialidad)
  ipa$especialidad <- if_else(ipa$nombre.usuario == "Marcela Paz del Rosario Albornoz Valdebenito", 
                              "Social", 
                              ipa$especialidad)
  ipa$especialidad <- if_else(ipa$nombre.usuario == "Marcelo Alejandro Orellana Caro", 
                              "Abogado, Coordinador", 
                              ipa$especialidad)
  ipa$especialidad <- if_else(ipa$nombre.usuario == "María Angélica Reyes Parot", 
                              "Profesional Psicólogo, Coordinador", 
                              ipa$especialidad)
  ipa$especialidad <- if_else(ipa$nombre.usuario == "Maria Jose Soulodre Tisi", 
                              "Abogado", 
                              ipa$especialidad)
  ipa$especialidad <- if_else(ipa$nombre.usuario == "Norma Natalia Caamaño Faundez", 
                              "Social", 
                              ipa$especialidad)
  ipa$especialidad <- if_else(ipa$nombre.usuario == "Oliver Javier Fernandez Eguiluz", 
                              "Social, Coordinador", 
                              ipa$especialidad)
  ipa$especialidad <- if_else(ipa$nombre.usuario == "Pablo Felipe Rubiño Lazo", 
                              "Abogado", 
                              ipa$especialidad)
  ipa$especialidad <- if_else(ipa$nombre.usuario == "Pamela del Pilar Maliqueo Aceval", 
                              "Otro", 
                              ipa$especialidad)
  ipa$especialidad <- if_else(ipa$nombre.usuario == "Rosa Adela Renin Miranda", 
                              "Abogado", 
                              ipa$especialidad)
  ipa$especialidad <- if_else(ipa$nombre.usuario == "Roxana Elena Carreño Marchant", 
                              "Social, Coordinador", 
                              ipa$especialidad)
  ipa$especialidad <- if_else(ipa$nombre.usuario == "Uranía Elisabeth Garcés Asenjo", 
                              "Abogado", 
                              ipa$especialidad)
  ipa$especialidad <- if_else(ipa$nombre.usuario == "Vanessa Andrea Alarcón Porflidtt", 
                              "Social, Coordinador", 
                              ipa$especialidad)
  }

setwd("C:/Users/felip/Google Drive/Clusters CAVD/Dias trabajados")
setwd("C:/Users/lgarcia/Documents/2019/08 Agosto/Clusters/Días trabajados")

rrhha <- read_xlsx(list.files()[1], sheet = "Hoja1")

i <- amatch(tolower(rrhha$Persona),
            levels(factor(tolower(ipa$nombre.usuario))),
            maxDist=10)
rrhha$nombre.usuario <- levels(factor(ipa$nombre.usuario))[i]

rrhha <- rrhha %>% 
  mutate(nombre.usuario = tolower(nombre.usuario),
         nombre.usuario = iconv(nombre.usuario)) %>%
  full_join(ipa %>%
              filter(mes.ingreso == 1) %>%
              arrange(fecha.ingreso) %>%
              select(especialidad, nombre.usuario) %>%
              distinct(nombre.usuario, .keep_all = T) %>%
              mutate(nombre.usuario = tolower(nombre.usuario)),
            by = "nombre.usuario"
  ) %>%
  dplyr::mutate(nombre.usuario = gsub("á", "a", nombre.usuario),
                nombre.usuario = gsub("é", "e", nombre.usuario),
                nombre.usuario = gsub("í", "i", nombre.usuario),
                nombre.usuario = gsub("ó", "o", nombre.usuario),
                nombre.usuario = gsub("ú", "u", nombre.usuario)
  )



for(i in 2 : length(list.files())){
  rrhhb <- read_xlsx(list.files()[i], sheet = "Hoja1")
  
  j <- amatch(tolower(rrhhb$Persona),
              levels(factor(tolower(ipa$nombre.usuario))),
              maxDist=10)
  rrhhb$nombre.usuario <- levels(factor(ipa$nombre.usuario))[j]
  
  rrhhb <- rrhhb %>%
    mutate(nombre.usuario = tolower(nombre.usuario)) %>%
    full_join(ipa %>%
                filter(mes.ingreso == i) %>%
                select(especialidad, nombre.usuario) %>%
                distinct(nombre.usuario, .keep_all = T) %>%
                mutate(nombre.usuario = tolower(nombre.usuario)),
              by = "nombre.usuario"
    )  %>%
    dplyr::mutate(nombre.usuario = gsub("á", "a", nombre.usuario),
                  nombre.usuario = gsub("é", "e", nombre.usuario),
                  nombre.usuario = gsub("í", "i", nombre.usuario),
                  nombre.usuario = gsub("ó", "o", nombre.usuario),
                  nombre.usuario = gsub("ú", "u", nombre.usuario)
    )
  
  rrhha <- rbind(rrhha, rrhhb)
  
}

rrhh <- rrhha %>%
  filter(is.na(MES) == F &
           is.na(especialidad) == F) %>%
  mutate(dias.trab = Habil_trab. + `En Cometido`)

diasta <- rrhh %>%
  filter(grepl("Social", especialidad) == T |
           grepl("Abogado", especialidad) == T |
           grepl("Psicólogo", especialidad) == T) %>%
  group_by(CentroCosto, especialidad) %>%
  dplyr::summarise(s = sum(dias.trab)) %>%
  arrange(CentroCosto) %>%
  mutate(s2 = if_else(grepl("Coordinador",
                            especialidad) == T,
                      s * .5,
                      s * 1)) %>%
  select(-s) %>%
  mutate(especialidad = gsub(", Coordinador",
                             "",
                             especialidad)) %>%
  group_by(CentroCosto, especialidad) %>%
  dplyr::summarise(s = sum(s2)) %>%
  spread(especialidad, value = s, fill = 0) 

i <- amatch(tolower(diasta$CentroCosto),
            levels(factor(ssr$cavd2)),
            maxDist=8)

diasta$cavd2 <- levels(factor(ssr$cavd2))[i]

diast <- diasta %>%
  mutate(cavd2 = if_else(CentroCosto == "CAVD ARICA Y PARINACOTA",
                         "Arica",
                         cavd2)) %>%
  filter(is.na(cavd2) == F)

colnames(diast) <- c("cavd", "ci.ab", "ci.ps", "ci.ts", "cavd2")

diast <- diast %>% 
  data.frame() %>%
  dplyr::select(-cavd)

diast %>%
  keep(is.numeric) %>%                     
  gather() %>%                             
  ggplot(aes(value)) +                     
  facet_wrap(~ key, scales = "free_y") +   
  geom_density()

####Duración####

dura <-  ssr %>%
  filter(ano.finalizacion == 2018) %>%
  mutate(dur = (as.Date(fecha.finalizacion, format = "%d-%m-%Y") - as.Date(fecha.ingreso, format = "%d-%m-%Y"))
  ) %>%
  group_by(cavd2)  %>%
  dplyr::summarise(dur = mean(dur)) %>%
  mutate(dur = as.numeric(dur))

####Actividades####

act <-  ipa %>%
  mutate(cavd = trimws(gsub("\\d+", "", cavd)),
         cavd2 = recode(cavd,
                        "Ancud" = "Castro",
                        "Peñalolen" = "Providencia",
                        "Aysén" = "Coyhaique",
                        "Collipulli" = "Angol",
                        "Til Til" = "Colina",
                        "Purranque" = "Osorno",
                        "Collipulli" = "Temuco",
                        "Puerto Natales" = "Punta Arenas",
                        "San Antonio" = "Valparaíso",
                        "Quellón" = "Castro",
                        "Los Angeles" = "Los Ángeles")
  ) %>%
  filter(ano.ingreso == 2018) %>%
  group_by(cavd2, categoria)  %>%
  dplyr::summarise(s = sum(tiempo.minutos)) %>%
  spread(categoria, value = s, fill = 0) %>% 
  mutate(s = `Actividades Administrativas` + 
                 `Actividades CCP` +             
                 `Actividades directas` + 
                 `Actividades indirectas` + 
                 `C-Macrosocial` + 
                 `Cuidado de Equipos` +          
                 `Directa c/sesion`,
         tad = `Actividades Administrativas` / s,
         tccp = `Actividades CCP` / s,
         tdir = `Actividades directas` / s,
         tindir = `Actividades indirectas` / s,
         tmacro = `C-Macrosocial` / s,
         tcuid = `Cuidado de Equipos` / s
  ) %>%
  select(cavd2, starts_with("t"))


####Causas####

causas <- causasa %>%
  mutate(pod = if_else(year(poder.simple) == 2018, 1, 0),
         pat = if_else(year(patrocinio.poder) == 2018, 1, 0),
         q = if_else(year(querella) == 2018, 1, 0),
         cavd2 = recode(cavd2,
                        "Chillan" = "Chillán",
                        "Copiapo" = "Copiapó",
                        "Curico" = "Curicó",
                        "Launion" = "La Unión",
                        "Los Angeles" = "Los Ángeles",
                        "Maipu" = "Maipú",
                        "Peñalolen" = "Providencia",
                        "San Antonio" = "Valparaíso",
                        "Valparaiso" = "Valparaíso",
                        "Ancud" = "Castro",
                        "Peñalolen" = "Providencia",
                        "Aysén" = "Coyhaique",
                        "Collipulli" = "Angol",
                        "Til Til" = "Colina",
                        "Purranque" = "Osorno",
                        "Collipulli" = "Temuco",
                        "Puerto Natales" = "Punta Arenas",
                        "San Antonio" = "Valparaíso",
                        "Quellón" = "Castro",
                        "Los Angeles" = "Los Ángeles")
  ) %>%
  group_by(cavd2) %>%
  dplyr::summarise(pod = sum(pod, na.rm = T),
                   pat = sum(pat, na.rm = T),
                   q = sum(q, na.rm = T)) %>%
  complete(cavd2, fill = list(pod = 0,
                              pat = 0,
                              q = 0)) %>%
  mutate(causas = pod + pat + q)

table(causas$cavd2, causas$cavd2 %in% ssr$cavd2)


####ges.join####

gesans <- ssr %>%
  group_by(cavd2) %>%
  dplyr::summarise(n = n()) %>%
  full_join(ssra %>%
              filter(ano.finalizacion == 2018) %>%
              group_by(cavd2) %>%
              dplyr::summarise(nfin = n())
  ) %>%
  full_join(prest, by = "cavd2") %>% 
  full_join(act, by = "cavd2") %>%
  full_join(dura, by = "cavd2") %>%
  full_join(ingesa, by = "cavd2") %>%
  full_join(asist, by = "cavd2") %>%
  full_join(causas, by = "cavd2") %>% 
  full_join(diast, by = "cavd2") %>%
  full_join(ssra %>%              
              filter(ano.finalizacion == 2018) %>%
              group_by(cavd2, tipo.finalizacion) %>%
              dplyr::summarise(n = n()) %>%
              spread(tipo.finalizacion, value = n)
  ) %>%
  full_join(ssr %>%
              group_by(cavd2, via.ingreso) %>%
              dplyr::summarise(n = n()) %>%
              spread(via.ingreso, value = n)
  ) %>%
  full_join(vig.cavd.mean, by = "cavd2") %>%
  modify_if(is.numeric, replace_na, 0)  %>%
  mutate(interna = (CCP + 
                      SIIN +  
                      `Derivación interna` + 
                      `Orientación de SOI` + 
                      `SOI (Tercer contacto o Sin teléfono)`) / n,
         t1 = `Finaliza T1` /n,
         t2 = `Finaliza T2` /n,
         t3 = `Finaliza T3` /n,
         cierre.administrativo = `Cierre Administrativo` /n,
         no.adscripcion = `No Adscrito` /n,
         ccp = `CCP` /n,
         dem.espontanea = `Demanda espontánea`  /n,
         der.interna = `Derivación interna`  /n,
         mp = `Ministerio Público`   /n,
         orien.soi = `Orientación de SOI`   /n,
         otros.der = `Otros derivantes` /n,
         siin = `SIIN` /n,
         tc.st.soi = `SOI (Tercer contacto o Sin teléfono)` /n,
         fin_ing = nfin / n,
         ab = Abogado / n,
         ps = `Profesional Psicólogo` / n,
         soc = Social / n,
         pod = pod / n,
         pat = pat / n,
         q = q / n
  )  %>%
  filter(cavd2 %in% c("Las Condes",
                      "Cauquenes") == F) %>%
  modify_if(is.numeric, replace_na, 0)

gesa <- gesans %>%
  modify_if(is.numeric, scale) %>%
  data.frame() 


####ges.gráficos####

gesa1 <- gesa %>%
  dplyr::select(cavd2, uso.ci, asistencia,
                ci.ab : ci.ts, 
                tad, tdir, tindir,               
                vig, 
                dur,
                interna, dem.espontanea, otros.der,
                t1, t2, t3, cierre.administrativo
  ) %>%
  lapply(replace_na, 0) %>%
  data.frame() %>%
  column_to_rownames("cavd2") 


gesa1 %>%
  keep(is.numeric) %>%                     
  gather() %>%                             
  ggplot(aes(value)) +                     
  facet_wrap(~ key, scales = "free_y") +   
  geom_density()


####ges.PCA####

ges <- gesa1 %>%  
  rownames_to_column("cavd2") %>%
  filter(cavd2 !="Coyhaique" ) %>%
  column_to_rownames("cavd2")

pca <- prcomp(ges, 
              center = TRUE,
              scale. = TRUE)

summary(pca)
ggbiplot(pca, labels = row.names(ges))

####ges.elegir conglomerados####


res.dist <- get_dist(ges, method = "pearson")
fviz_dist(res.dist, lab_size = 8)

fviz_nbclust(keep(ges, is.numeric), kmeans, nboot = 500, k.max = 6)
fviz_nbclust(keep(ges, is.numeric), kmeans, "wss", nboot = 500, k.max = 6)



kgesk2 <- eclust(ges, "kmeans", k = 2, graph = F, nboot = 500)
kgesk3 <- eclust(ges, "kmeans", k = 3, graph = F, nboot = 500)
kgesk4 <- eclust(ges, "kmeans", k = 4, graph = F, nboot = 500)
kgesk5 <- eclust(ges, "kmeans", k = 5, graph = F, nboot = 500)
kgesk6 <- eclust(ges, "kmeans", k = 6, graph = F, nboot = 500)

kgesh2 <- eclust(ges, "hclust", k = 2, graph = F, nboot = 500)
kgesh3 <- eclust(ges, "hclust", k = 3, graph = F, nboot = 500)
kgesh4 <- eclust(ges, "hclust", k = 4, graph = F, nboot = 500)

fviz_silhouette(kgesk2)
fviz_cluster(kgesk2)
fviz_cluster(kgesh3, axes = c(3, 4))


ges$k2 <- as.factor(kgesk2$cluster)
ges$k3 <- as.factor(kgesk3$cluster)
ges$k4 <- as.factor(kgesk4$cluster)
ges$k5 <- as.factor(kgesk5$cluster)
ges$k6 <- as.factor(kgesk6$cluster)

ges$h2 <- as.factor(kgesh2$cluster)
ges$h3 <- as.factor(kgesh3$cluster)
ges$h4 <- as.factor(kgesh4$cluster)

mds <- keep(ges, is.numeric) %>%
  dist(method = "euclidean") %>%          
  cmdscale() %>%
  as_tibble()
colnames(mds) <- c("Dim.1", "Dim.2")

mds$k2 = ges$k2
mds$k3 = ges$k3
mds$k4 = ges$k4
mds$k5 = ges$k5
mds$k6 = ges$k6

mds$h2 = ges$h2
mds$h3 = ges$h3
mds$h4 = ges$h4

ggscatter(mds, x = "Dim.1", y = "Dim.2", 
          label = rownames(ges),
          size = 1,
          repel = TRUE,
          color = "k3", 
          ellipse = TRUE,
          ellipse.type = "convex"
)


####Macro####

gmacronsa <- macro %>%
  group_by(cavd2, producto) %>%
  dplyr::summarise(n = n()) %>%
  spread(value = n, key = producto, fill = 0) %>%
  mutate(macro = `Articulación con la Oferta de Seguridad Pública` +
           `Inserción del Programa en el Territorio` +
           `Mesas Intersectoriales` +
           `Protocolos Intersectoriales`)

gmacro <- gmacronsa %>%
  modify_if(is.numeric, scale) %>%
  data.frame() %>%
  filter(cavd2 %in% c("Cauquenes", "Las Condes") == F)

gmacro %>%
  keep(is.numeric) %>%                     
  gather() %>%                             
  ggplot(aes(value)) +                     
  facet_wrap(~ key, scales = "free_y") +   
  geom_density()  +   
  scale_x_continuous(limits = c(-4.5, 4.5))

####Gráficos car####


setwd("C:/Users/felip/Google Drive/Clusters CAVD/Gráficos")
setwd("C:/Users/lgarcia/Documents/2019/08 Agosto/Clusters/Gráficos")


carg <- grupo %>%
  mutate(cavd2 = row.names(grupo))

grupoansa <- grupoans %>%
  filter(cavd2 %in% carg$cavd2)

carg[,'n'] <- grupoansa$n 
carg[,'Mujer'] <- grupoansa$Mujer 
carg[,'Delitos.sexuales'] <- grupoansa$Delitos.sexuales 
carg[,'Homicidio'] <- grupoansa$Homicidio 
carg[,'Lesiones'] <- grupoansa$Lesiones 
carg[,'Robos.violentos'] <- grupoansa$Robos.violentos 
carg[,'m.edad'] <- grupoansa$m.edad
carg[,'Cuasidelito'] <- grupoansa$Cuasidelito 
carg[,'po'] <- grupoansa$po 

color.car <- 'IsleofDogs1'
color.car.w <- wes_palette(color.car)


colnames(carg) <- c('Ingresos',
                    'Mujeres (%)',
                    'Delitos sexuales (%)',
                    'Homicidios (%)',
                    'Lesiones (%)',
                    'Robos violentos (%)',
                    'Edad promedio',
                    'Cuasidelitos (%)',
                    'Poblacion objetivo',
                    'k2',
                    'k3',
                    'k4',
                    'k5',
                    "k9",
                    'h2',
                    'h3',
                    'h4',
                    'h5',
                    'h6',
                    'cavd')

carg$Grupo <- carg$k4

gcar1 <-  ggplot(carg, aes(y = carg[, 1],
                           x = reorder(cavd, as.numeric(carg[, 1])),
                           fill = Grupo)) +
  geom_bar(stat = 'identity') +
  scale_y_continuous(breaks = seq(0, 1500, 250)) +
  coord_flip() +
  scale_fill_manual(values = wes_palette('IsleofDogs1')) +
  ggtitle(colnames(carg)[1]) +
  theme_minimal() +
  theme(axis.text.y = element_text(color = color.car.w[carg[ order(carg[[1]]),] %>%
                                                         select(Grupo) %>%
                                                         c() %>%
                                                         unlist()],
                                   size = 6,
                                   vjust = -.03),
        axis.title = element_blank(),
        title = element_text(size = 10),
        axis.text.x = element_text(size = 10))

ggsave("g1.jpeg", gcar1, height = 3.5, width = 6.3, units = "in")

gcar2 <- ggplot(carg, aes(y = carg[, 2],
                          x = reorder(cavd, as.numeric(carg[, 2])),
                          fill = Grupo)) +
  geom_bar(stat = 'identity') +
  scale_y_continuous(breaks = seq(0, 1, .1),
                     labels = percent_format( accuracy = 1),
                     limits = c(0, .8)) +
  coord_flip() +
  scale_fill_manual(values = wes_palette('IsleofDogs1')) +
  ggtitle(colnames(carg)[2]) +
  theme_minimal() +
  theme(axis.text.y = element_text(color = color.car.w[carg[ order(carg[[2]]),] %>%
                                                         select(Grupo) %>%
                                                         c() %>%
                                                         unlist()],
                                   size = 6,
                                   vjust = -.03),
        axis.title = element_blank(),
        title = element_text(size = 10),
        axis.text.x = element_text(size = 10))
ggsave("g3.jpeg", gcar2, height = 3.5, width = 6.3, units = "in")


gcar3 <- ggplot(carg, aes(y = carg[, 3],
                          x = reorder(cavd, as.numeric(carg[, 3])),
                          fill = Grupo)) +
  geom_bar(stat = 'identity') +
  scale_y_continuous(breaks = seq(0, 1, .25),
                     labels = percent_format( accuracy = 1),
                     limits = c(0, 1)) +
  coord_flip() +
  guides(fill = F) +
  scale_fill_manual(values = wes_palette('IsleofDogs1')) +
  ggtitle(colnames(carg)[3]) +
  theme_minimal() +
  theme(axis.text.y = element_text(color = color.car.w[carg[ order(carg[[3]]),] %>%
                                                         select(Grupo) %>%
                                                         c() %>%
                                                         unlist()],
                                   size = 6,
                                   vjust = -.03),
        axis.title = element_blank(),
        title = element_text(size = 10),
        axis.text.x = element_text(size = 10))


gcar4 <- ggplot(carg, aes(y = carg[, 4],
                          x = reorder(cavd, as.numeric(carg[, 4])),
                          fill = Grupo)) +
  geom_bar(stat = 'identity') +
  scale_y_continuous(breaks = seq(0, 1, .25),
                     labels = percent_format( accuracy = 1),
                     limits = c(0, 1)) +
  coord_flip() +
  guides(fill = F) +
  scale_fill_manual(values = wes_palette('IsleofDogs1')) +
  ggtitle(colnames(carg)[4]) +
  theme_minimal() +
  theme(axis.text.y = element_text(color = color.car.w[carg[ order(carg[[4]]),] %>%
                                                         select(Grupo) %>%
                                                         c() %>%
                                                         unlist()],
                                   size = 6,
                                   vjust = -.03),
        axis.title = element_blank(),
        title = element_text(size = 10),
        axis.text.x = element_text(size = 10))

gcar5 <- ggplot(carg, aes(y = carg[, 5],
                          x = reorder(cavd, as.numeric(carg[, 5])),
                          fill = Grupo)) +
  geom_bar(stat = 'identity') +
  scale_y_continuous(breaks = seq(0, 1, .25),
                     labels = percent_format( accuracy = 1),
                     limits = c(0, 1)) +
  coord_flip() +
  guides(fill = F) +
  scale_fill_manual(values = wes_palette('IsleofDogs1')) +
  ggtitle(colnames(carg)[5]) +
  theme_minimal() +
  theme(axis.text.y = element_text(color = color.car.w[carg[ order(carg[[5]]),] %>%
                                                         select(Grupo) %>%
                                                         c() %>%
                                                         unlist()],
                                   size = 6,
                                   vjust = -.03),
        axis.title = element_blank(),
        title = element_text(size = 10),
        axis.text.x = element_text(size = 10))

gcar6 <- ggplot(carg, aes(y = carg[, 6],
                          x = reorder(cavd, as.numeric(carg[, 6])),
                          fill = Grupo)) +
  geom_bar(stat = 'identity') +
  scale_y_continuous(breaks = seq(0, 1, .25),
                     labels = percent_format( accuracy = 1),
                     limits = c(0, 1)) +
  coord_flip() +
  guides(fill = F) +
  scale_fill_manual(values = wes_palette('IsleofDogs1')) +
  ggtitle(colnames(carg)[6]) +
  theme_minimal() +
  theme(axis.text.y = element_text(color = color.car.w[carg[ order(carg[[6]]),] %>%
                                                         select(Grupo) %>%
                                                         c() %>%
                                                         unlist()],
                                   size = 6,
                                   vjust = -.03),
        axis.title = element_blank(),
        title = element_text(size = 10),
        axis.text.x = element_text(size = 10))


gcar7 <- ggplot(carg, aes(y = carg[, 7],
                          x = reorder(cavd, as.numeric(carg[, 7])),
                          fill = Grupo)) +
  geom_bar(stat = 'identity') +
  scale_y_continuous() +
  coord_flip() +
  scale_fill_manual(values = wes_palette('IsleofDogs1')) +
  ggtitle(colnames(carg)[7]) +
  theme_minimal() +
  theme(axis.text.y = element_text(color = color.car.w[carg[ order(carg[[7]]),] %>%
                                                         select(Grupo) %>%
                                                         c() %>%
                                                         unlist()],
                                   size = 6,
                                   vjust = -.03),
        axis.title = element_blank(),
        title = element_text(size = 10),
        axis.text.x = element_text(size = 10))

ggsave("g5.jpeg", gcar7, height = 3.5, width = 6.3, units = "in")


gcar8 <- ggplot(carg, aes(y = carg[, 8],
                          x = reorder(cavd, as.numeric(carg[, 8])),
                          fill = Grupo)) +
  geom_bar(stat = 'identity') +
  scale_y_continuous(breaks = seq(0, 1, .25),
                     labels = percent_format( accuracy = 1),
                     limits = c(0, 1)) +
  coord_flip() +
  guides(fill = F) +
  scale_fill_manual(values = wes_palette('IsleofDogs1')) +
  ggtitle(colnames(carg)[8]) +
  theme_minimal() +
  theme(axis.text.y = element_text(color = color.car.w[carg[ order(carg[[8]]),] %>%
                                                         select(Grupo) %>%
                                                         c() %>%
                                                         unlist()],
                                   size = 6,
                                   vjust = -.03),
        axis.title = element_blank(),
        title = element_text(size = 10),
        axis.text.x = element_text(size = 10))
g4 <- grid.arrange(gcar3, gcar4,
                   gcar5, gcar6, 
                   gcar8, g_legend(gcar1)
)
ggsave("g4.jpeg", g4, height = 8.1, width = 6, units = "in")


gcar9 <- ggplot(carg, aes(y = carg[, 9],
                          x = reorder(cavd, as.numeric(carg[, 9])),
                          fill = Grupo)) +
  geom_bar(stat = 'identity') +
  scale_y_continuous(breaks = seq(0, 10500, 2000)) +
  coord_flip() +
  scale_fill_manual(values = wes_palette('IsleofDogs1')) +
  ggtitle(colnames(carg)[9]) +
  theme_minimal() +
  theme(axis.text.y = element_text(color = color.car.w[carg[ order(carg[[9]]),] %>%
                                                         select(Grupo) %>%
                                                         c() %>%
                                                         unlist()],
                                   size = 6,
                                   vjust = -.03),
        axis.title = element_blank(),
        title = element_text(size = 10),
        axis.text.x = element_text(size = 10))
ggsave("g2.jpeg", gcar9, height = 3.5, width = 6.3, units = "in")


gcar1 <- grid.arrange(gcar1,
                      gcar2,
                      gcar3,
                      gcar4,
                      gcar5,
                      gcar6,
                      gcar7,
                      gcar8,
                      gcar9
)


####Gráficos ges####


gesg <- gesans %>%
  filter(cavd2 %in% rownames(ges)) %>%
  dplyr::select(cavd2,
                colnames(ges)[colnames(ges) %in% colnames(gesans)]) %>%
  full_join(ges %>%
              rownames_to_column("cavd2") %>%
              dplyr::select(cavd2, k2 : colnames(ges)[ncol(ges)]
              ),
            by = "cavd2") %>%
  dplyr::select(-cavd2, cavd2) %>%
  data.frame()

colnames(gesg) <- c('Uso de CI',
                    'Sesiones con asistencia',
                    "Capacidad instalada (AB)",
                    "Capacidad instalada (PS)",
                    "Capacidad instalada (TS)",
                    "Tiempo dedicado a Actividades administrativas",
                    "Tiempo dedicado a Actividades directas",
                    "Tiempo dedicado a Actividades indirectas",
                    'Promedio de casos vigentes diarios',
                    "Duración promedio de casos",
                    "Vía interna (Primera respuesta o Derivación interna)",
                    'Demanda espontanea',
                    'Otros derivantes',
                    'Finaliza T1',
                    'Finaliza T2',
                    'Cierre administrativo',
                    "No adscripción",
                    'k2',
                    'k3',
                    'k4',
                    "k5",
                    "k6",
                    'h2',
                    'h3',
                    'h4',
                    "cavd")

gesg$Grupo <- gesg$k2


color.ges <- 'Cavalcanti1'
color.ges.w <- wes_palette(color.ges)

gges1 <- ggplot(gesg, aes(y = gesg[, 1],
                          x = reorder(cavd, gesg[, 1]),
                          fill = Grupo)) +
  geom_bar(stat = 'identity') +
  scale_y_continuous(labels = percent_format( accuracy = 1)) +
  coord_flip() +
  guides(fill = F) +
  scale_fill_manual(values = wes_palette('Cavalcanti1')) +
  ggtitle(colnames(gesg)[1]) +
  theme_minimal() +
  theme(axis.text.y = element_text(color = color.ges.w[gesg[ order(gesg[[1]]),] %>%
                                                         select(Grupo) %>%
                                                         c() %>%
                                                         unlist()],
                                   size = 14,
                                   vjust = -.03),
        axis.title = element_blank(),
        title = element_text(size = 10),
        axis.text.x = element_text(size = 20))

gges1.l <- ggplot(gesg, aes(y = gesg[, 1],
                            x = reorder(cavd, gesg[, 1]),
                            fill = Grupo)) +
  geom_bar(stat = 'identity') +
  scale_y_continuous(labels = percent_format( accuracy = 1)) +
  coord_flip() +
  scale_fill_manual(values = wes_palette('Cavalcanti1')) +
  ggtitle(colnames(gesg)[1]) +
  theme_minimal() +
  theme(axis.text.y = element_text(color = color.ges.w[gesg[ order(gesg[[1]]),] %>%
                                                         select(Grupo) %>%
                                                         c() %>%
                                                         unlist()],
                                   size = 14,
                                   vjust = -.03),
        axis.title = element_blank(),
        title = element_text(size = 10),
        axis.text.x = element_text(size = 20))


gges2 <- ggplot(gesg, aes(y = gesg[, 2],
                          x = reorder(cavd, gesg[, 2]),
                          fill = Grupo)) +
  geom_bar(stat = 'identity') +
  scale_y_continuous(labels = percent_format( accuracy = 1)) +
  coord_flip() +
  scale_fill_manual(values = wes_palette('Cavalcanti1')) +
  ggtitle(colnames(gesg)[2]) +
  theme_minimal() +
  theme(axis.text.y = element_text(color = color.ges.w[gesg[ order(gesg[[2]]),] %>%
                                                         select(Grupo) %>%
                                                         c() %>%
                                                         unlist()],
                                   size = 7,
                                   vjust = -.03),
        axis.title = element_blank(),
        title = element_text(size = 10),
        axis.text.x = element_text(size = 10))

ggsave("g9.jpeg", gges2, height = 3.5, width = 6, units = "in")

gges3 <- ggplot(gesg, aes(y = gesg[, 3],
                          x = reorder(cavd, as.numeric(gesg[, 3])),
                          fill = Grupo)) +
  geom_bar(stat = 'identity') +
  coord_flip() +
  guides(fill = F) +
  scale_fill_manual(values = wes_palette('Cavalcanti1')) +
  ggtitle(colnames(gesg)[3]) +
  theme_minimal() +
  theme(axis.text.y = element_text(color = color.ges.w[gesg[ order(gesg[[3]]),] %>%
                                                         select(Grupo) %>%
                                                         c() %>%
                                                         unlist()],
                                   size = 6,
                                   vjust = -.03),
        axis.title = element_blank(),
        title = element_text(size = 10),
        axis.text.x = element_text(size = 10))



gges4 <- ggplot(gesg, aes(y = gesg[, 4],
                          x = reorder(cavd, as.numeric(gesg[, 4])),
                          fill = Grupo)) +
  geom_bar(stat = 'identity') +
  coord_flip() +
  guides(fill = F) +
  scale_fill_manual(values = wes_palette('Cavalcanti1')) +
  ggtitle(colnames(gesg)[4]) +
  theme_minimal() +
  theme(axis.text.y = element_text(color = color.ges.w[gesg[ order(gesg[[4]]),] %>%
                                                         select(Grupo) %>%
                                                         c() %>%
                                                         unlist()],
                                   size = 6,
                                   vjust = -.03),
        axis.title = element_blank(),
        title = element_text(size = 10),
        axis.text.x = element_text(size = 10))

gges5 <- ggplot(gesg, aes(y = gesg[, 5],
                          x = reorder(cavd, as.numeric(gesg[, 5])),
                          fill = Grupo)) +
  geom_bar(stat = 'identity') +
  coord_flip() +
  guides(fill = F) +
  scale_fill_manual(values = wes_palette('Cavalcanti1')) +
  ggtitle(colnames(gesg)[5]) +
  theme_minimal() +       
  theme(axis.text.y = element_text(color = color.ges.w[gesg[ order(gesg[[5]]),] %>%
                                                         select(Grupo) %>%
                                                         c() %>%
                                                         unlist()],
                                   size = 6,
                                   vjust = -.03),
        axis.title = element_blank(),
        title = element_text(size = 10),
        axis.text.x = element_text(size = 10))

ggsave("g7.jpeg", grid.arrange(gges3, gges4, gges5, g_legend(gges1.l)
),
height = 6, width = 6, units = "in")


gges6 <- ggplot(gesg, aes(y = gesg[, 6],
                          x = reorder(cavd, as.numeric(gesg[, 6])),
                          fill = Grupo)) +
  geom_bar(stat = 'identity') +
  coord_flip() +
  guides(fill = F) +
  scale_fill_manual(values = wes_palette('Cavalcanti1')) +
  scale_y_continuous(labels = percent_format( accuracy = 1),
                     limits = c(0, 1)) +
  ggtitle("Tiempo dedicado a\nActividades administrativas") +
  theme_minimal() +
  theme(axis.text.y = element_text(color = color.ges.w[gesg[ order(gesg[[6]]),] %>%
                                                         select(Grupo) %>%
                                                         c() %>%
                                                         unlist()],
                                   size = 6,
                                   vjust = -.03),
        axis.title = element_blank(),
        title = element_text(size = 10),
        axis.text.x = element_text(size = 10))

gges7 <- ggplot(gesg, aes(y = gesg[, 7],
                          x = reorder(cavd, as.numeric(gesg[, 7])),
                          fill = Grupo)) +
  geom_bar(stat = 'identity') +
  scale_y_continuous(labels = percent_format( accuracy = 1),
                     limits = c(0, 1)) +
  coord_flip() +
  guides(fill = F) +
  scale_fill_manual(values = wes_palette('Cavalcanti1')) +
  ggtitle("Tiempo dedicado a\nActividades directas") +
  theme_minimal() +
  theme(axis.text.y = element_text(color = color.ges.w[gesg[ order(gesg[[7]]),] %>%
                                                         select(Grupo) %>%
                                                         c() %>%
                                                         unlist()],
                                   size = 6,
                                   vjust = -.03),
        axis.title = element_blank(),
        title = element_text(size = 10),
        axis.text.x = element_text(size = 10))

gges8 <- ggplot(gesg, aes(y = gesg[, 8],
                          x = reorder(cavd, as.numeric(gesg[, 8])),
                          fill = Grupo)) +
  geom_bar(stat = 'identity') +
  scale_y_continuous(labels = percent_format( accuracy = 1),
                     limits = c(0, 1)) +
  coord_flip() +
  guides(fill = F) +
  scale_fill_manual(values = wes_palette('Cavalcanti1')) +
  ggtitle("Tiempo dedicado a\nActividades indirectas") +
  theme_minimal() +
  theme(axis.text.y = element_text(color = color.ges.w[gesg[ order(gesg[[8]]),] %>%
                                                         select(Grupo) %>%
                                                         c() %>%
                                                         unlist()],
                                   size = 6,
                                   vjust = -.03),
        axis.title = element_blank(),
        title = element_text(size = 10),
        axis.text.x = element_text(size = 10))

ggsave("g8.jpeg", grid.arrange(gges6, gges7, gges8, g_legend(gges1.l)
),
height = 7, width = 6, units = "in")


gges9 <- ggplot(gesg, aes(y = gesg[, 9],
                          x = reorder(cavd, as.numeric(gesg[, 9])),
                          fill = Grupo)) +
  geom_bar(stat = 'identity') +
  coord_flip() +
  guides(fill = F) +
  scale_fill_manual(values = wes_palette('Cavalcanti1')) +
  ggtitle(colnames(gesg)[9]) +
  theme_minimal() +
  theme(axis.text.y = element_text(color = color.ges.w[gesg[ order(gesg[[9]]),] %>%
                                                         select(Grupo) %>%
                                                         c() %>%
                                                         unlist()],
                                   size = 14,
                                   vjust = -.03),
        axis.title = element_blank(),
        title = element_text(size = 10),
        axis.text.x = element_text(size = 20))

gges10 <- ggplot(gesg, aes(y = gesg[, 10],
                           x = reorder(cavd, as.numeric(gesg[, 10])),
                           fill = Grupo)) +
  geom_bar(stat = 'identity') +
  coord_flip() +
  guides(fill = F) +
  scale_fill_manual(values = wes_palette('Cavalcanti1')) +
  ggtitle(colnames(gesg)[10]) +
  theme_minimal() +
  theme(axis.text.y = element_text(color = color.ges.w[gesg[ order(gesg[[10]]),] %>%
                                                         select(Grupo) %>%
                                                         c() %>%
                                                         unlist()],
                                   size = 14,
                                   vjust = -.03),
        axis.title = element_blank(),
        title = element_text(size = 10),
        axis.text.x = element_text(size = 20))

gges11 <- ggplot(gesg, aes(y = gesg[, 11],
                           x = reorder(cavd, as.numeric(gesg[, 11])),
                           fill = Grupo)) +
  geom_bar(stat = 'identity') +
  scale_y_continuous(labels = percent_format( accuracy = 1),
                     limits = c(0, 1)) +
  coord_flip() +
  guides(fill = F) +
  scale_fill_manual(values = wes_palette('Cavalcanti1')) +
  ggtitle("Vía interna (Primera Respuesta \n o Derivación interna)") +
  theme_minimal() +
  theme(axis.text.y = element_text(color = color.ges.w[gesg[ order(gesg[[11]]),] %>%
                                                         select(Grupo) %>%
                                                         c() %>%
                                                         unlist()],
                                   size = 6,
                                   vjust = -.03),
        axis.title = element_blank(),
        title = element_text(size = 10),
        axis.text.x = element_text(size = 10))

gges12 <- ggplot(gesg, aes(y = gesg[, 12],
                           x = reorder(cavd, as.numeric(gesg[, 12])),
                           fill = Grupo)) +
  geom_bar(stat = 'identity') +
  scale_y_continuous(labels = percent_format( accuracy = 1),
                     limits = c(0, 1)) +
  coord_flip() +
  guides(fill = F) +
  scale_fill_manual(values = wes_palette('Cavalcanti1')) +
  ggtitle(colnames(gesg)[12]) +
  theme_minimal() +
  theme(axis.text.y = element_text(color = color.ges.w[gesg[ order(gesg[[12]]),] %>%
                                                         select(Grupo) %>%
                                                         c() %>%
                                                         unlist()],
                                   size = 6,
                                   vjust = -.03),
        axis.title = element_blank(),
        title = element_text(size = 10),
        axis.text.x = element_text(size = 10))

gges13 <- ggplot(gesg, aes(y = gesg[, 13],
                           x = reorder(cavd, as.numeric(gesg[, 13])),
                           fill = Grupo)) +
  geom_bar(stat = 'identity') +
  scale_y_continuous(labels = percent_format( accuracy = 1),
                     limits = c(0, 1)) +
  coord_flip() +
  guides(fill = F) +
  scale_fill_manual(values = wes_palette('Cavalcanti1')) +
  ggtitle(colnames(gesg)[13]) +
  theme_minimal() +
  theme(axis.text.y = element_text(color = color.ges.w[gesg[ order(gesg[[13]]),] %>%
                                                         select(Grupo) %>%
                                                         c() %>%
                                                         unlist()],
                                   size = 6,
                                   vjust = -.03),
        axis.title = element_blank(),
        title = element_text(size = 10),
        axis.text.x = element_text(size = 10))

ggsave("g6.jpeg", grid.arrange(gges11, gges12, gges13, g_legend(gges1.l)
),
height = 6, width = 6, units = "in")

gges14 <- ggplot(gesg, aes(y = gesg[, 14],
                           x = reorder(cavd, as.numeric(gesg[, 14])),
                           fill = Grupo)) +
  geom_bar(stat = 'identity') +
  scale_y_continuous(labels = percent_format( accuracy = 1),
                     limits = c(0, 1)) +
  coord_flip() +
  guides(fill = F) +
  scale_fill_manual(values = wes_palette('Cavalcanti1')) +
  ggtitle(colnames(gesg)[14]) +
  theme_minimal() +
  theme(axis.text.y = element_text(color = color.ges.w[gesg[ order(gesg[[14]]),] %>%
                                                         select(Grupo) %>%
                                                         c() %>%
                                                         unlist()],
                                   size = 6,
                                   vjust = -.03),
        axis.title = element_blank(),
        title = element_text(size = 10),
        axis.text.x = element_text(size = 10))

gges15 <- ggplot(gesg, aes(y = gesg[, 15],
                           x = reorder(cavd, as.numeric(gesg[, 15])),
                           fill = Grupo)) +
  geom_bar(stat = 'identity') +
  scale_y_continuous(labels = percent_format( accuracy = 1),
                     limits = c(0, 1)) +
  coord_flip() +
  guides(fill = F) +
  scale_fill_manual(values = wes_palette('Cavalcanti1')) +
  ggtitle(colnames(gesg)[15]) +
  theme_minimal() +
  theme(axis.text.y = element_text(color = color.ges.w[gesg[ order(gesg[[15]]),] %>%
                                                         select(Grupo) %>%
                                                         c() %>%
                                                         unlist()],
                                   size = 6,
                                   vjust = -.03),
        axis.title = element_blank(),
        title = element_text(size = 10),
        axis.text.x = element_text(size = 10))

gges16 <- ggplot(gesg, aes(y = gesg[, 16],
                           x = reorder(cavd, as.numeric(gesg[, 16])),
                           fill = Grupo)) +
  geom_bar(stat = 'identity') +
  scale_y_continuous(labels = percent_format( accuracy = 1),
                     limits = c(0, 1)) +
  coord_flip() +
  guides(fill = F) +
  scale_fill_manual(values = wes_palette('Cavalcanti1')) +
  ggtitle(colnames(gesg)[16]) +
  theme_minimal() +
  theme(axis.text.y = element_text(color = color.ges.w[gesg[ order(gesg[[16]]),] %>%
                                                         select(Grupo) %>%
                                                         c() %>%
                                                         unlist()],
                                   size = 6,
                                   vjust = -.03),
        axis.title = element_blank(),
        title = element_text(size = 10),
        axis.text.x = element_text(size = 10))

gges17 <- ggplot(gesg, aes(y = gesg[, 17],
                           x = reorder(cavd, as.numeric(gesg[, 17])),
                           fill = Grupo)) +
  geom_bar(stat = 'identity') +
  scale_y_continuous(labels = percent_format( accuracy = 1),
                     limits = c(0, 1)) +
  coord_flip() +
  guides(fill = F) +
  scale_fill_manual(values = wes_palette('Cavalcanti1')) +
  ggtitle(colnames(gesg)[17]) +
  theme_minimal() +
  theme(axis.text.y = element_text(color = color.ges.w[gesg[ order(gesg[[17]]),] %>%
                                                         select(Grupo) %>%
                                                         c() %>%
                                                         unlist()],
                                   size = 6,
                                   vjust = -.03),
        axis.title = element_blank(),
        title = element_text(size = 10),
        axis.text.x = element_text(size = 10))

ggsave("g10.jpeg", grid.arrange(gges14, gges15,
                                gges16, gges17, g_legend(gges1.l + theme(legend.position = "bottom")),
                                heights = c(10, 10, 1)), height = 6.5, width = 6, units = "in")


ggges1<- grid.arrange(gges11,
                      gges12,
                      gges13,
                      gges9
)

ggges2 <- grid.arrange(gges1,
                       gges3,
                       gges4,
                       gges5)

ggges3 <- grid.arrange(gges6,
                       gges7,
                       gges8,
                       gges10)


ggges4 <- grid.arrange(gges14,
                       gges15,
                       gges16,
                       gges17
)

ggsave("ges1.jpeg", ggges1, height = 18, width = 32)
ggsave("ges2.jpeg", ggges2, height = 18, width = 32)
ggsave("ges3.jpeg", ggges3, height = 18, width = 32)

#####int.conglomerados####

int <- gesa %>%
  dplyr::select(ab : soc, 
                causas,
                cavd2) %>%
  full_join(gmacro, 
            by = "cavd2") %>%
  column_to_rownames("cavd2") %>%
  dplyr::select(ab, soc, ps, causas, macro)

intf <- int %>%
  rownames_to_column("cavd") %>%
  filter(cavd %in% c("San Fernando") == F) %>%
  column_to_rownames("cavd") 


pca <- prcomp(intf, 
              center = TRUE,
              scale. = TRUE)

summary(pca)
ggbiplot(pca, labels = row.names(intf))



fviz_nbclust(keep(intf, is.numeric), FUNcluster = kmeans, nboot = 500)
fviz_nbclust(keep(intf, is.numeric), FUNcluster = kmeans, "wss", nboot = 500)

fviz_nbclust(keep(intf, is.numeric), FUNcluster = hcut, nboot = 500)
fviz_nbclust(keep(intf, is.numeric), FUNcluster = hcut, "wss", nboot = 500)

kintk2 <- eclust(intf, "kmeans", k = 2, graph = F, nboot = 500)
kintk3 <- eclust(intf, "kmeans", k = 3, graph = F, nboot = 500)
kintk4 <- eclust(intf, "kmeans", k = 4, graph = F, nboot = 500)
kintk5 <- eclust(intf, "kmeans", k = 5, graph = F, nboot = 500)
kintk6 <- eclust(intf, "kmeans", k = 6, graph = F, nboot = 500)

kinth2 <- eclust(intf, "hclust", k = 2, graph = F, nboot = 500)
kinth3 <- eclust(intf, "hclust", k = 3, graph = F, nboot = 500)
kinth4 <- eclust(intf, "hclust", k = 4, graph = F, nboot = 500)
kinth5 <- eclust(intf, "hclust", k = 5, graph = F, nboot = 500)
kinth6 <- eclust(intf, "hclust", k = 6, graph = F, nboot = 500)

intf$k2 <- as.factor(kintk2$cluster)
intf$k3 <- as.factor(kintk3$cluster)
intf$k4 <- as.factor(kintk4$cluster)
intf$k5 <- as.factor(kintk5$cluster)
intf$k6 <- as.factor(kintk6$cluster)

intf$h2 <- as.factor(kinth2$cluster)
intf$h3 <- as.factor(kinth3$cluster)
intf$h4 <- as.factor(kinth4$cluster)
intf$h5 <- as.factor(kinth5$cluster)
intf$h6 <- as.factor(kinth6$cluster)

fviz_cluster(kintk3)
fviz_silhouette(kintk3)

mds <- keep(intf, is.numeric) %>%
  dist(method = "euclidean") %>%          
  cmdscale() %>%
  as_tibble()
colnames(mds) <- c("Dim.1", "Dim.2")

mds$k2 = intf$k2
mds$k3 = intf$k3
mds$k4 = intf$k4
mds$k5 = intf$k5
mds$k6 = intf$k6

mds$h2 = intf$h2
mds$h3 = intf$h3
mds$h4 = intf$h4
mds$h5 = intf$h5
mds$h6 = intf$h6

ggscatter(mds, x = "Dim.1", y = "Dim.2", 
          label = rownames(intf),
          #size = 1,
          repel = TRUE,
          color = "k3", 
          ellipse = TRUE,
          ellipse.type = "convex"
)


####Gráficos int####

intg <- intf %>%
  rownames_to_column("cavd") %>%
  select(-cavd, cavd) %>%
  select(-causas)

gmacrons <- filter(gmacronsa, cavd2 %in% intg$cavd)
gesansb <- filter(gesans, cavd2 %in% intg$cavd)
causasb <- filter(causas, cavd2 %in% intg$cavd)

intg[,'ab'] <- gesansb$ab 
intg[,'soc'] <- gesansb$soc 
intg[,'ps'] <- gesansb$ps 
intg[,'macro'] <- gmacrons$macro 
intg[,'pod'] <- gesansb$pod 
intg[,'pat'] <- gesansb$pat 
intg[,'q'] <- gesansb$q 

intg <-  intg %>%
  select("ab",    "soc",   "ps",    "macro",
            "pod", "pat",   "q" ,   
            "k2",    "k3"  ,  "k4"  ,  "k5",
            "k6" ,   "h2" ,   "h3" ,   "h4",    "h5",    "h6",
         "cavd")

colnames(intg) <- c(
  'Prestaciones juridicas por victima',
  'Prestaciones sociales por victima',
  'Prestaciones psicológicas por victima',
  'Actividades macrosociales',
  'Poderes simples por víctima',
  "Patrocinios de poder por víctima",
  "Querellas por víctima",
  'k2',
  'k3',
  'k4',
  'k5',
  "k6",
  'h2',
  'h3',
  'h4',
  'h5',
  "h6",
  "cavd")

intg$Grupo <- intg$k3

color.int.w <- wes_palette("Moonrise2")

gint1.l <- ggplot(intg, aes(y = intg[, 1],
                            x = reorder(cavd, as.numeric(intg[, 1])),
                            fill = Grupo)) +
  geom_bar(stat = 'identity') +
  scale_y_continuous(limits = c(0, 8.5)) +
  coord_flip() +
  scale_fill_manual(values = color.int.w) +
  ggtitle(colnames(intg)[1]) +
  theme_minimal() +
  theme(axis.text.y = element_text(color = color.int.w[intg[ order(intg[[1]]),] %>%
                                                         select(Grupo) %>%
                                                         c() %>%
                                                         unlist()],
                                   size = 10,
                                   vjust = -.03),
        axis.title = element_blank(),
        title = element_text(size = 10),
        axis.text.x = element_text(size = 20))

gint1 <- ggplot(intg, aes(y = intg[, 1],
                          x = reorder(cavd, as.numeric(intg[, 1])),
                          fill = Grupo)) +
  geom_bar(stat = 'identity') +
  scale_y_continuous(limits = c(0, 8.5)) +
  coord_flip() +
  guides(fill = F) +
  scale_fill_manual(values = color.int.w) +
  ggtitle("Prestaciones jurídicas\npor víctima") +
  theme_minimal() +
  theme(axis.text.y = element_text(color = color.int.w[intg[ order(intg[[1]]),] %>%
                                                         select(Grupo) %>%
                                                         c() %>%
                                                         unlist()],
                                   size = 6,
                                   vjust = -.03),
        axis.title = element_blank(),
        title = element_text(size = 10),
        axis.text.x = element_text(size = 10))


gint2 <- ggplot(intg, aes(y = intg[, 2],
                          x = reorder(cavd, as.numeric(intg[, 2])),
                          fill = Grupo)) +
  geom_bar(stat = 'identity') +
  scale_y_continuous(limits = c(0, 8.5)) +
  coord_flip() +
  guides(fill = F) +
  scale_fill_manual(values = color.int.w) +
  ggtitle("Prestaciones sociales\npor víctima") +
  theme_minimal() +
  theme(axis.text.y = element_text(color = color.int.w[intg[ order(intg[[2]]),] %>%
                                                         select(Grupo) %>%
                                                         c() %>%
                                                         unlist()],
                                   size = 6,
                                   vjust = -.03),
        axis.title = element_blank(),
        title = element_text(size = 10),
        axis.text.x = element_text(size = 10))

gint3 <- ggplot(intg, aes(y = intg[, 3],
                          x = reorder(cavd, as.numeric(intg[, 3])),
                          fill = Grupo)) +
  geom_bar(stat = 'identity') +
  scale_y_continuous(limits = c(0, 8.5)) +
  coord_flip() +
  guides(fill = F) +
  scale_fill_manual(values = color.int.w) +
  ggtitle("Prestaciones\npor víctima") +
  theme_minimal() +
  theme(axis.text.y = element_text(color = color.int.w[intg[ order(intg[[3]]),] %>%
                                                         select(Grupo) %>%
                                                         c() %>%
                                                         unlist()],
                                   size = 6,
                                   vjust = -.03),
        axis.title = element_blank(),
        title = element_text(size = 10),
        axis.text.x = element_text(size = 10))

g11 = grid.arrange(gint1, gint2,
                   gint3, g_legend(gint1.l))

ggsave("g11.jpeg", g11, height = 6.5, width = 6, units = "in")

gint4 <- ggplot(intg, aes(y = intg[, 4],
                          x = reorder(cavd, as.numeric(intg[, 4])),
                          fill = Grupo)) +
  geom_bar(stat = 'identity') +
  coord_flip() +
  guides(fill = F) +
  scale_fill_manual(values = color.int.w) +
  ggtitle(colnames(intg)[4]) +
  theme_minimal() +
  theme(axis.text.y = element_text(color = color.int.w[intg[ order(intg[[4]]),] %>%
                                                         select(Grupo) %>%
                                                         c() %>%
                                                         unlist()],
                                   size = 6,
                                   vjust = -.03),
        axis.title = element_blank(),
        title = element_text(size = 10),
        axis.text.x = element_text(size = 10))

gint5 <- ggplot(intg, aes(y = intg[, 5],
                          x = reorder(cavd, as.numeric(intg[, 5])),
                          fill = Grupo)) +
  geom_bar(stat = 'identity') +
  coord_flip() +
  guides(fill = F) +
  scale_fill_manual(values = color.int.w) +
  ggtitle(colnames(intg)[5]) +
  theme_minimal() +
  theme(axis.text.y = element_text(color = color.int.w[intg[ order(intg[[5]]),] %>%
                                                         select(Grupo) %>%
                                                         c() %>%
                                                         unlist()],
                                   size = 6,
                                   vjust = -.03),
        axis.title = element_blank(),
        title = element_text(size = 10),
        axis.text.x = element_text(size = 10))

gint6 <- ggplot(intg, aes(y = intg[, 6],
                          x = reorder(cavd, as.numeric(intg[, 6])),
                          fill = Grupo)) +
  geom_bar(stat = 'identity') +
  coord_flip() +
  guides(fill = F) +
  scale_fill_manual(values = color.int.w) +
  ggtitle(colnames(intg)[6]) +
  theme_minimal() +
  theme(axis.text.y = element_text(color = color.int.w[intg[ order(intg[[6]]),] %>%
                                                         select(Grupo) %>%
                                                         c() %>%
                                                         unlist()],
                                   size = 6,
                                   vjust = -.03),
        axis.title = element_blank(),
        title = element_text(size = 10),
        axis.text.x = element_text(size = 10))

gint7 <- ggplot(intg, aes(y = intg[, 7],
                          x = reorder(cavd, as.numeric(intg[, 7])),
                          fill = Grupo)) +
  geom_bar(stat = 'identity') +
  coord_flip() +
  guides(fill = F) +
  scale_fill_manual(values = color.int.w) +
  ggtitle(colnames(intg)[5]) +
  theme_minimal() +
  theme(axis.text.y = element_text(color = color.int.w[intg[ order(intg[[7]]),] %>%
                                                         select(Grupo) %>%
                                                         c() %>%
                                                         unlist()],
                                   size = 6,
                                   vjust = -.03),
        axis.title = element_blank(),
        title = element_text(size = 10),
        axis.text.x = element_text(size = 10))

g12 = grid.arrange(gint4, gint5,
                   g_legend(gint1.l + theme(legend.position = "bottom")),
                   nrow = 2, heights = c(10, 1))

ggsave("g12.jpeg", g12, 
       height = 5.5, width = 6, units = "in")


ggplot(intg, aes(y = intg[, 3],
                 x = k)) +
  geom_boxplot()  

ggint1.1 <- grid.arrange(gint1,
                         gint2,
                         gint3)
ggint1.2 <- grid.arrange(gint4,
                         gint5)

ggsave("gint1.jpeg", ggint1.1, height = 8, width = 15)
ggsave("gint2.jpeg", ggint1.2, height = 8, width = 15)

#####Cruce####

cruce <- select(gesg, cavd, kges = Grupo) %>%
  full_join(select(intg, cavd, kint = Grupo), by = 'cavd') %>%
  full_join(select(carg, cavd, kcar = Grupo), by = 'cavd') %>%
  mutate(gf = paste(kges, kint, kcar))

table(cruce$gf)


#Representantes de grupos para sacar el conglomerado para el gráfico

rcar1 <- carg[carg$Grupo == "1", ]$Grupo
rcar2 <- carg[carg$Grupo == "2", ]$Grupo
rcar3 <- carg[carg$Grupo == "3", ]$Grupo
rcar4 <- carg[carg$Grupo == "4", ]$Grupo


rges1 <- gesg[gesg$Grupo == "1", ]$Grupo
rges2 <- gesg[gesg$Grupo == "2", ]$Grupo

rint1 <- intg[intg$Grupo == "1", ]$Grupo
rint2 <- intg[intg$Grupo == "2", ]$Grupo
rint3 <- intg[intg$Grupo == "3", ]$Grupo


gcrucen <- cruce %>%
  group_by(kges, kint, kcar) %>%
  dplyr::summarise(n = n()) %>%
  modify_if(is.factor, as.character) %>%
  ungroup()

crucenn <- cruce %>%
  group_by(gf) %>%
  dplyr::summarise(n = n())

crucenn[crucenn$cavd==crucenn$gf[1], ]$cavd = write.csv(cruce[cruce$gf == crucenn$gf[1], ]$cavd, 
                                                        row.names = F)


gcruce <- gcrucen %>%
  mutate(
    kges = if_else(kges == 1,
                   "Grupo 1",
                   if_else(kges == 2,
                           "Grupo 2",
                           "Sin\nGrupo")),
    kint = if_else(kint == 1,
                   "Grupo 1",
                   if_else(kint == 2,
                           "Grupo 2",
                           if_else(kint == 3,
                                   "Grupo 3",
                                   if_else(is.na(kint) == T,
                                           "Sin\nGrupo",
                                           "NA")))),
    kcar = if_else(kcar == 1,
                   "Grupo 1",
                   if_else(kcar == 2,
                           "Grupo 2",
                           if_else(kcar == 3,
                                   "Grupo 3",
                                   if_else(kcar == 4,
                                           "Grupo 4",
                                           "Sin Grupo"))))  
  ) %>%
  modify_if(is.character, as.factor) %>%
  mutate(kint = fct_explicit_na(kint, "Sin Grupo"))

alu <- ggplot(as.data.frame(gcruce %>%
                              filter(kint != "Sin Grupo" & is.na(kges) == F)),
              aes(y = n, axis1 = kcar, axis2 = kint, axis3 = kges)) +
  geom_alluvium(aes(fill = kcar)) +
  geom_stratum() +
  geom_text(stat = "stratum", label.strata = TRUE, size = 3) +
  scale_x_discrete(limits = c("Contexto", "Intervención", 'Gestión'), expand = c(.05, .05)) +
  scale_fill_manual(values = wes_palette("BottleRocket2")) +
  theme_minimal() +
  guides(fill = F) +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 20))

alu
ggsave("g13.jpeg", alu, height = 5, width = 6, units = "in")

####Gráficos de validación####

g14 = fviz_silhouette(gruk4) +
  scale_fill_manual(values = wes_palette("IsleofDogs1")) +
  scale_colour_manual(values = wes_palette("IsleofDogs1")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, size = 8, color = color.car.w[carg[ order(carg$Grupo),] %>%
                                                                               select(Grupo) %>%
                                                                               c() %>%
                                                                               unlist()]),
        title = element_blank())

ggsave("g14.jpeg", g14, height = 6, width = 6, units = "in")


g15 = fviz_cluster(gruk4, repel = T, main = F)+
  scale_fill_manual(values = wes_palette("IsleofDogs1")) +
  scale_colour_manual(values = wes_palette("IsleofDogs1")) +
  theme_minimal() 

ggsave("g15.jpeg", g15, height = 6, width = 6, units = "in")

g16 = fviz_silhouette(kgesk2) +
  scale_fill_manual(values = wes_palette("Cavalcanti1")) +
  scale_colour_manual(values = wes_palette("Cavalcanti1")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, size = 8, color = color.ges.w[gesg[order(gesg$Grupo),] %>%
                                                                               select(Grupo) %>%
                                                                               c() %>%
                                                                               unlist()]),
        title = element_blank())

ggsave("g16.jpeg", g16, height = 6, width = 6, units = "in")


g17 = fviz_cluster(kgesk2, repel = T, main = F)+
  scale_fill_manual(values = wes_palette("Cavalcanti1")) +
  scale_colour_manual(values = wes_palette("Cavalcanti1")) +
  theme_minimal() 

ggsave("g17.jpeg", g17, height = 6, width = 6, units = "in")

g18 = fviz_silhouette(kintk3) +
  scale_fill_manual(values = wes_palette("Moonrise2")) +
  scale_colour_manual(values = wes_palette("Moonrise2")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, size = 8, color = color.int.w[intg[order(intg$Grupo),] %>%
                                                                               select(Grupo) %>%
                                                                               c() %>%
                                                                               unlist()]),
        title = element_blank())

ggsave("g18.jpeg", g18, height = 6, width = 6, units = "in")


g19 = fviz_cluster(kintk3, repel = T, main = F)+
  scale_fill_manual(values = wes_palette("Moonrise2")) +
  scale_colour_manual(values = wes_palette("Moonrise2")) +
  theme_minimal() 

ggsave("g19.jpeg", g19, height = 6, width = 6, units = "in")


####Gráficos presentación####

####Gráficos presentacion.car####

carg <- carg %>%
  mutate(Grupo = if_else(cavd %in% c("Alto Hospicio", "Angol",  "Antofagasta", "Castro" ,
                                     "Colina" , "Curicó",  "Huechuraba", "Iquique", 
                                     "La Serena", "Osorno", "Puerto Montt", "San Bernardo", 
                                     "Temuco", "Valdivia" ), "4", 
                         if_else(cavd %in% c("Arica", "Concepción", "Copiapó",
                                             "Coquimbo", "Coyhaique", "Lampa", "Melipilla",
                                             "Padre Hurtado", "Rancagua", "San Felipe", "Talca" ), "3", 
                                 if_else(cavd %in% c("La Florida", "Maipú",  "Providencia", "Pudahuel", 
                                                     "Puente Alto", "Recoleta", "Santiago",
                            "Valparaíso"), 
                            "2", 
                            "1"))))

carg <- carg %>%
  mutate(alpha1 = if_else(Grupo == 1, 1, .2),
         alpha2 = if_else(Grupo == 2, 1, .2),
         alpha3 = if_else(Grupo == 3, 1, .2),
         alpha4 = if_else(Grupo == 4, 1, .2))

####Grupo 1.pres.car####

setwd("C:/Users/lgarcia/Documents/2019/12 Diciembre/Graficos/Contexto/Grupo 1")

gcar1 <-  ggplot(carg, aes(y = carg[, 1],
                           x = reorder(cavd, as.numeric(carg[, 1])),
                           fill = Grupo,
                           alpha = alpha1)) +
  geom_bar(stat = 'identity') +
  guides(fill = F, alpha = F) +
  scale_y_continuous(breaks = seq(0, 1500, 250)) +
  coord_flip() +
  scale_fill_manual(values = wes_palette('IsleofDogs1')) +
  ggtitle(colnames(carg)[1]) +
  theme_minimal() +
  theme(axis.text.y = element_text(color = color.car.w[carg[ order(carg[[1]]),] %>%
                                                         select(Grupo) %>%
                                                         c() %>%
                                                         unlist()],
                                   size = 6,
                                   vjust = -.03),
        axis.title = element_blank(),
        title = element_text(size = 10),
        axis.text.x = element_text(size = 10))

gcar2 <- ggplot(carg, aes(y = carg[, 2],
                          x = reorder(cavd, as.numeric(carg[, 2])),
                          fill = Grupo,
                          alpha = alpha1)) +
  geom_bar(stat = 'identity') +   
  guides(fill = F, alpha = F) +
  scale_y_continuous(breaks = seq(0, 1, .1),
                     labels = percent_format( accuracy = 1),
                     limits = c(0, .8)) +
  coord_flip() +
  scale_fill_manual(values = wes_palette('IsleofDogs1')) +
  ggtitle(colnames(carg)[2]) +
  theme_minimal() +
  theme(axis.text.y = element_text(color = color.car.w[carg[ order(carg[[2]]),] %>%
                                                         select(Grupo) %>%
                                                         c() %>%
                                                         unlist()],
                                   size = 6,
                                   vjust = -.03),
        axis.title = element_blank(),
        title = element_text(size = 10),
        axis.text.x = element_text(size = 10))

gcar3 <- ggplot(carg, aes(y = carg[, 3],
                          x = reorder(cavd, as.numeric(carg[, 3])),
                          fill = Grupo,
                          alpha = alpha1)) +
  geom_bar(stat = 'identity') +   
  guides(fill = F, alpha = F) +
  scale_y_continuous(breaks = seq(0, 1, .25),
                     labels = percent_format( accuracy = 1),
                     limits = c(0, 1)) +
  coord_flip() +
  guides(fill = F) +
  scale_fill_manual(values = wes_palette('IsleofDogs1')) +
  ggtitle(colnames(carg)[3]) +
  theme_minimal() +
  theme(axis.text.y = element_text(color = color.car.w[carg[ order(carg[[3]]),] %>%
                                                         select(Grupo) %>%
                                                         c() %>%
                                                         unlist()],
                                   size = 6,
                                   vjust = -.03),
        axis.title = element_blank(),
        title = element_text(size = 10),
        axis.text.x = element_text(size = 10))


gcar4 <- ggplot(carg, aes(y = carg[, 4],
                          x = reorder(cavd, as.numeric(carg[, 4])),
                          fill = Grupo,
                          alpha = alpha1)) +
  geom_bar(stat = 'identity') +   
  guides(fill = F, alpha = F) +
  scale_y_continuous(breaks = seq(0, 1, .25),
                     labels = percent_format( accuracy = 1),
                     limits = c(0, 1)) +
  coord_flip() +
  guides(fill = F) +
  scale_fill_manual(values = wes_palette('IsleofDogs1')) +
  ggtitle(colnames(carg)[4]) +
  theme_minimal() +
  theme(axis.text.y = element_text(color = color.car.w[carg[ order(carg[[4]]),] %>%
                                                         select(Grupo) %>%
                                                         c() %>%
                                                         unlist()],
                                   size = 6,
                                   vjust = -.03),
        axis.title = element_blank(),
        title = element_text(size = 10),
        axis.text.x = element_text(size = 10))

gcar5 <- ggplot(carg, aes(y = carg[, 5],
                          x = reorder(cavd, as.numeric(carg[, 5])),
                          fill = Grupo,
                          alpha = alpha1)) +
  geom_bar(stat = 'identity') +   
  guides(fill = F, alpha = F) +
  scale_y_continuous(breaks = seq(0, 1, .25),
                     labels = percent_format( accuracy = 1),
                     limits = c(0, 1)) +
  coord_flip() +
  guides(fill = F) +
  scale_fill_manual(values = wes_palette('IsleofDogs1')) +
  ggtitle(colnames(carg)[5]) +
  theme_minimal() +
  theme(axis.text.y = element_text(color = color.car.w[carg[ order(carg[[5]]),] %>%
                                                         select(Grupo) %>%
                                                         c() %>%
                                                         unlist()],
                                   size = 6,
                                   vjust = -.03),
        axis.title = element_blank(),
        title = element_text(size = 10),
        axis.text.x = element_text(size = 10))

gcar6 <- ggplot(carg, aes(y = carg[, 6],
                          x = reorder(cavd, as.numeric(carg[, 6])),
                          fill = Grupo,
                          alpha = alpha1)) +
  geom_bar(stat = 'identity') +   
  guides(fill = F, alpha = F) +
  scale_y_continuous(breaks = seq(0, 1, .25),
                     labels = percent_format( accuracy = 1),
                     limits = c(0, 1)) +
  coord_flip() +
  guides(fill = F) +
  scale_fill_manual(values = wes_palette('IsleofDogs1')) +
  ggtitle(colnames(carg)[6]) +
  theme_minimal() +
  theme(axis.text.y = element_text(color = color.car.w[carg[ order(carg[[6]]),] %>%
                                                         select(Grupo) %>%
                                                         c() %>%
                                                         unlist()],
                                   size = 6,
                                   vjust = -.03),
        axis.title = element_blank(),
        title = element_text(size = 10),
        axis.text.x = element_text(size = 10))


gcar7 <- ggplot(carg, aes(y = carg[, 7],
                          x = reorder(cavd, as.numeric(carg[, 7])),
                          fill = Grupo,
                          alpha = alpha1)) +
  geom_bar(stat = 'identity') +   
  guides(fill = F, alpha = F) +
  scale_y_continuous() +
  coord_flip() +
  scale_fill_manual(values = wes_palette('IsleofDogs1')) +
  ggtitle(colnames(carg)[7]) +
  theme_minimal() +
  theme(axis.text.y = element_text(color = color.car.w[carg[ order(carg[[7]]),] %>%
                                                         select(Grupo) %>%
                                                         c() %>%
                                                         unlist()],
                                   size = 6,
                                   vjust = -.03),
        axis.title = element_blank(),
        title = element_text(size = 10),
        axis.text.x = element_text(size = 10))

gcar8 <- ggplot(carg, aes(y = carg[, 8],
                          x = reorder(cavd, as.numeric(carg[, 8])),
                          fill = Grupo,
                          alpha = alpha1)) +
  geom_bar(stat = 'identity') +   
  guides(fill = F, alpha = F) +
  scale_y_continuous(breaks = seq(0, 1, .25),
                     labels = percent_format( accuracy = 1),
                     limits = c(0, 1)) +
  coord_flip() +
  guides(fill = F) +
  scale_fill_manual(values = wes_palette('IsleofDogs1')) +
  ggtitle(colnames(carg)[8]) +
  theme_minimal() +
  theme(axis.text.y = element_text(color = color.car.w[carg[ order(carg[[8]]),] %>%
                                                         select(Grupo) %>%
                                                         c() %>%
                                                         unlist()],
                                   size = 6,
                                   vjust = -.03),
        axis.title = element_blank(),
        title = element_text(size = 10),
        axis.text.x = element_text(size = 10))

gcar9 <- ggplot(carg, aes(y = carg[, 9],
                          x = reorder(cavd, as.numeric(carg[, 9])),
                          fill = Grupo,
                          alpha = alpha1)) +
  geom_bar(stat = 'identity') +   
  guides(fill = F, alpha = F) +
  scale_y_continuous(breaks = seq(0, 10500, 2000)) +
  coord_flip() +
  scale_fill_manual(values = wes_palette('IsleofDogs1')) +
  ggtitle(colnames(carg)[9]) +
  theme_minimal() +
  theme(axis.text.y = element_text(color = color.car.w[carg[ order(carg[[9]]),] %>%
                                                         select(Grupo) %>%
                                                         c() %>%
                                                         unlist()],
                                   size = 6,
                                   vjust = -.03),
        axis.title = element_blank(),
        title = element_text(size = 10),
        axis.text.x = element_text(size = 10))

ggsave("g1_car_1.jpeg", gcar1, height = 5.8, width = 10.44, units = "cm")
ggsave("g2_car_1.jpeg", gcar2, height = 5.8, width = 10.44, units = "cm")
ggsave("g3_car_1.jpeg", gcar3, height = 5.8, width = 10.44, units = "cm")
ggsave("g4_car_1.jpeg", gcar4, height = 5.8, width = 10.44, units = "cm")
ggsave("g5_car_1.jpeg", gcar5, height = 5.8, width = 10.44, units = "cm")
ggsave("g6_car_1.jpeg", gcar6, height = 5.8, width = 10.44, units = "cm")
ggsave("g7_car_1.jpeg", gcar7, height = 5.8, width = 10.44, units = "cm")
ggsave("g8_car_1.jpeg", gcar8, height = 5.8, width = 10.44, units = "cm")
ggsave("g9_car_1.jpeg", gcar9, height = 5.8, width = 10.44, units = "cm")


####Grupo 2.pres.car####

setwd("C:/Users/lgarcia/Documents/2019/12 Diciembre/Graficos/Contexto/Grupo 2")

gcar1 <-  ggplot(carg, aes(y = carg[, 1],
                           x = reorder(cavd, as.numeric(carg[, 1])),
                           fill = Grupo,
                           alpha = alpha2)) +
  geom_bar(stat = 'identity') +
  guides(fill = F, alpha = F) +
  scale_y_continuous(breaks = seq(0, 1500, 250)) +
  coord_flip() +
  scale_fill_manual(values = wes_palette('IsleofDogs1')) +
  ggtitle(colnames(carg)[1]) +
  theme_minimal() +
  theme(axis.text.y = element_text(color = color.car.w[carg[ order(carg[[1]]),] %>%
                                                         select(Grupo) %>%
                                                         c() %>%
                                                         unlist()],
                                   size = 6,
                                   vjust = -.03),
        axis.title = element_blank(),
        title = element_text(size = 10),
        axis.text.x = element_text(size = 10))

gcar2 <- ggplot(carg, aes(y = carg[, 2],
                          x = reorder(cavd, as.numeric(carg[, 2])),
                          fill = Grupo,
                          alpha = alpha2)) +
  geom_bar(stat = 'identity') +   
  guides(fill = F, alpha = F) +
  scale_y_continuous(breaks = seq(0, 1, .1),
                     labels = percent_format( accuracy = 1),
                     limits = c(0, .8)) +
  coord_flip() +
  scale_fill_manual(values = wes_palette('IsleofDogs1')) +
  ggtitle(colnames(carg)[2]) +
  theme_minimal() +
  theme(axis.text.y = element_text(color = color.car.w[carg[ order(carg[[2]]),] %>%
                                                         select(Grupo) %>%
                                                         c() %>%
                                                         unlist()],
                                   size = 6,
                                   vjust = -.03),
        axis.title = element_blank(),
        title = element_text(size = 10),
        axis.text.x = element_text(size = 10))

gcar3 <- ggplot(carg, aes(y = carg[, 3],
                          x = reorder(cavd, as.numeric(carg[, 3])),
                          fill = Grupo,
                          alpha = alpha2)) +
  geom_bar(stat = 'identity') +   
  guides(fill = F, alpha = F) +
  scale_y_continuous(breaks = seq(0, 1, .25),
                     labels = percent_format( accuracy = 1),
                     limits = c(0, 1)) +
  coord_flip() +
  guides(fill = F) +
  scale_fill_manual(values = wes_palette('IsleofDogs1')) +
  ggtitle(colnames(carg)[3]) +
  theme_minimal() +
  theme(axis.text.y = element_text(color = color.car.w[carg[ order(carg[[3]]),] %>%
                                                         select(Grupo) %>%
                                                         c() %>%
                                                         unlist()],
                                   size = 6,
                                   vjust = -.03),
        axis.title = element_blank(),
        title = element_text(size = 10),
        axis.text.x = element_text(size = 10))


gcar4 <- ggplot(carg, aes(y = carg[, 4],
                          x = reorder(cavd, as.numeric(carg[, 4])),
                          fill = Grupo,
                          alpha = alpha2)) +
  geom_bar(stat = 'identity') +   
  guides(fill = F, alpha = F) +
  scale_y_continuous(breaks = seq(0, 1, .25),
                     labels = percent_format( accuracy = 1),
                     limits = c(0, 1)) +
  coord_flip() +
  guides(fill = F) +
  scale_fill_manual(values = wes_palette('IsleofDogs1')) +
  ggtitle(colnames(carg)[4]) +
  theme_minimal() +
  theme(axis.text.y = element_text(color = color.car.w[carg[ order(carg[[4]]),] %>%
                                                         select(Grupo) %>%
                                                         c() %>%
                                                         unlist()],
                                   size = 6,
                                   vjust = -.03),
        axis.title = element_blank(),
        title = element_text(size = 10),
        axis.text.x = element_text(size = 10))

gcar5 <- ggplot(carg, aes(y = carg[, 5],
                          x = reorder(cavd, as.numeric(carg[, 5])),
                          fill = Grupo,
                          alpha = alpha2)) +
  geom_bar(stat = 'identity') +   
  guides(fill = F, alpha = F) +
  scale_y_continuous(breaks = seq(0, 1, .25),
                     labels = percent_format( accuracy = 1),
                     limits = c(0, 1)) +
  coord_flip() +
  guides(fill = F) +
  scale_fill_manual(values = wes_palette('IsleofDogs1')) +
  ggtitle(colnames(carg)[5]) +
  theme_minimal() +
  theme(axis.text.y = element_text(color = color.car.w[carg[ order(carg[[5]]),] %>%
                                                         select(Grupo) %>%
                                                         c() %>%
                                                         unlist()],
                                   size = 6,
                                   vjust = -.03),
        axis.title = element_blank(),
        title = element_text(size = 10),
        axis.text.x = element_text(size = 10))

gcar6 <- ggplot(carg, aes(y = carg[, 6],
                          x = reorder(cavd, as.numeric(carg[, 6])),
                          fill = Grupo,
                          alpha = alpha2)) +
  geom_bar(stat = 'identity') +   
  guides(fill = F, alpha = F) +
  scale_y_continuous(breaks = seq(0, 1, .25),
                     labels = percent_format( accuracy = 1),
                     limits = c(0, 1)) +
  coord_flip() +
  guides(fill = F) +
  scale_fill_manual(values = wes_palette('IsleofDogs1')) +
  ggtitle(colnames(carg)[6]) +
  theme_minimal() +
  theme(axis.text.y = element_text(color = color.car.w[carg[ order(carg[[6]]),] %>%
                                                         select(Grupo) %>%
                                                         c() %>%
                                                         unlist()],
                                   size = 6,
                                   vjust = -.03),
        axis.title = element_blank(),
        title = element_text(size = 10),
        axis.text.x = element_text(size = 10))


gcar7 <- ggplot(carg, aes(y = carg[, 7],
                          x = reorder(cavd, as.numeric(carg[, 7])),
                          fill = Grupo,
                          alpha = alpha2)) +
  geom_bar(stat = 'identity') +   
  guides(fill = F, alpha = F) +
  scale_y_continuous() +
  coord_flip() +
  scale_fill_manual(values = wes_palette('IsleofDogs1')) +
  ggtitle(colnames(carg)[7]) +
  theme_minimal() +
  theme(axis.text.y = element_text(color = color.car.w[carg[ order(carg[[7]]),] %>%
                                                         select(Grupo) %>%
                                                         c() %>%
                                                         unlist()],
                                   size = 6,
                                   vjust = -.03),
        axis.title = element_blank(),
        title = element_text(size = 10),
        axis.text.x = element_text(size = 10))

gcar8 <- ggplot(carg, aes(y = carg[, 8],
                          x = reorder(cavd, as.numeric(carg[, 8])),
                          fill = Grupo,
                          alpha = alpha2)) +
  geom_bar(stat = 'identity') +   
  guides(fill = F, alpha = F) +
  scale_y_continuous(breaks = seq(0, 1, .25),
                     labels = percent_format( accuracy = 1),
                     limits = c(0, 1)) +
  coord_flip() +
  guides(fill = F) +
  scale_fill_manual(values = wes_palette('IsleofDogs1')) +
  ggtitle(colnames(carg)[8]) +
  theme_minimal() +
  theme(axis.text.y = element_text(color = color.car.w[carg[ order(carg[[8]]),] %>%
                                                         select(Grupo) %>%
                                                         c() %>%
                                                         unlist()],
                                   size = 6,
                                   vjust = -.03),
        axis.title = element_blank(),
        title = element_text(size = 10),
        axis.text.x = element_text(size = 10))

gcar9 <- ggplot(carg, aes(y = carg[, 9],
                          x = reorder(cavd, as.numeric(carg[, 9])),
                          fill = Grupo,
                          alpha = alpha2)) +
  geom_bar(stat = 'identity') +   
  guides(fill = F, alpha = F) +
  scale_y_continuous(breaks = seq(0, 10500, 2000)) +
  coord_flip() +
  scale_fill_manual(values = wes_palette('IsleofDogs1')) +
  ggtitle(colnames(carg)[9]) +
  theme_minimal() +
  theme(axis.text.y = element_text(color = color.car.w[carg[ order(carg[[9]]),] %>%
                                                         select(Grupo) %>%
                                                         c() %>%
                                                         unlist()],
                                   size = 6,
                                   vjust = -.03),
        axis.title = element_blank(),
        title = element_text(size = 10),
        axis.text.x = element_text(size = 10))

ggsave("g1_car_2.jpeg", gcar1, height = 5.8, width = 10.44, units = "cm")
ggsave("g2_car_2.jpeg", gcar2, height = 5.8, width = 10.44, units = "cm")
ggsave("g3_car_2.jpeg", gcar3, height = 5.8, width = 10.44, units = "cm")
ggsave("g4_car_2.jpeg", gcar4, height = 5.8, width = 10.44, units = "cm")
ggsave("g5_car_2.jpeg", gcar5, height = 5.8, width = 10.44, units = "cm")
ggsave("g6_car_2.jpeg", gcar6, height = 5.8, width = 10.44, units = "cm")
ggsave("g7_car_2.jpeg", gcar7, height = 5.8, width = 10.44, units = "cm")
ggsave("g8_car_2.jpeg", gcar8, height = 5.8, width = 10.44, units = "cm")
ggsave("g9_car_2.jpeg", gcar9, height = 5.8, width = 10.44, units = "cm")


####Grupo 3.pres.car####

setwd("C:/Users/lgarcia/Documents/2019/12 Diciembre/Graficos/Contexto/Grupo 3")

gcar1 <-  ggplot(carg, aes(y = carg[, 1],
                           x = reorder(cavd, as.numeric(carg[, 1])),
                           fill = Grupo,
                           alpha = alpha3)) +
  geom_bar(stat = 'identity') +
  guides(fill = F, alpha = F) +
  scale_y_continuous(breaks = seq(0, 1500, 250)) +
  coord_flip() +
  scale_fill_manual(values = wes_palette('IsleofDogs1')) +
  ggtitle(colnames(carg)[1]) +
  theme_minimal() +
  theme(axis.text.y = element_text(color = color.car.w[carg[ order(carg[[1]]),] %>%
                                                         select(Grupo) %>%
                                                         c() %>%
                                                         unlist()],
                                   size = 6,
                                   vjust = -.03),
        axis.title = element_blank(),
        title = element_text(size = 10),
        axis.text.x = element_text(size = 10))

gcar2 <- ggplot(carg, aes(y = carg[, 2],
                          x = reorder(cavd, as.numeric(carg[, 2])),
                          fill = Grupo,
                          alpha = alpha3)) +
  geom_bar(stat = 'identity') +   
  guides(fill = F, alpha = F) +
  scale_y_continuous(breaks = seq(0, 1, .1),
                     labels = percent_format( accuracy = 1),
                     limits = c(0, .8)) +
  coord_flip() +
  scale_fill_manual(values = wes_palette('IsleofDogs1')) +
  ggtitle(colnames(carg)[2]) +
  theme_minimal() +
  theme(axis.text.y = element_text(color = color.car.w[carg[ order(carg[[2]]),] %>%
                                                         select(Grupo) %>%
                                                         c() %>%
                                                         unlist()],
                                   size = 6,
                                   vjust = -.03),
        axis.title = element_blank(),
        title = element_text(size = 10),
        axis.text.x = element_text(size = 10))

gcar3 <- ggplot(carg, aes(y = carg[, 3],
                          x = reorder(cavd, as.numeric(carg[, 3])),
                          fill = Grupo,
                          alpha = alpha3)) +
  geom_bar(stat = 'identity') +   
  guides(fill = F, alpha = F) +
  scale_y_continuous(breaks = seq(0, 1, .25),
                     labels = percent_format( accuracy = 1),
                     limits = c(0, 1)) +
  coord_flip() +
  guides(fill = F) +
  scale_fill_manual(values = wes_palette('IsleofDogs1')) +
  ggtitle(colnames(carg)[3]) +
  theme_minimal() +
  theme(axis.text.y = element_text(color = color.car.w[carg[ order(carg[[3]]),] %>%
                                                         select(Grupo) %>%
                                                         c() %>%
                                                         unlist()],
                                   size = 6,
                                   vjust = -.03),
        axis.title = element_blank(),
        title = element_text(size = 10),
        axis.text.x = element_text(size = 10))


gcar4 <- ggplot(carg, aes(y = carg[, 4],
                          x = reorder(cavd, as.numeric(carg[, 4])),
                          fill = Grupo,
                          alpha = alpha3)) +
  geom_bar(stat = 'identity') +   
  guides(fill = F, alpha = F) +
  scale_y_continuous(breaks = seq(0, 1, .25),
                     labels = percent_format( accuracy = 1),
                     limits = c(0, 1)) +
  coord_flip() +
  guides(fill = F) +
  scale_fill_manual(values = wes_palette('IsleofDogs1')) +
  ggtitle(colnames(carg)[4]) +
  theme_minimal() +
  theme(axis.text.y = element_text(color = color.car.w[carg[ order(carg[[4]]),] %>%
                                                         select(Grupo) %>%
                                                         c() %>%
                                                         unlist()],
                                   size = 6,
                                   vjust = -.03),
        axis.title = element_blank(),
        title = element_text(size = 10),
        axis.text.x = element_text(size = 10))

gcar5 <- ggplot(carg, aes(y = carg[, 5],
                          x = reorder(cavd, as.numeric(carg[, 5])),
                          fill = Grupo,
                          alpha = alpha3)) +
  geom_bar(stat = 'identity') +   
  guides(fill = F, alpha = F) +
  scale_y_continuous(breaks = seq(0, 1, .25),
                     labels = percent_format( accuracy = 1),
                     limits = c(0, 1)) +
  coord_flip() +
  guides(fill = F) +
  scale_fill_manual(values = wes_palette('IsleofDogs1')) +
  ggtitle(colnames(carg)[5]) +
  theme_minimal() +
  theme(axis.text.y = element_text(color = color.car.w[carg[ order(carg[[5]]),] %>%
                                                         select(Grupo) %>%
                                                         c() %>%
                                                         unlist()],
                                   size = 6,
                                   vjust = -.03),
        axis.title = element_blank(),
        title = element_text(size = 10),
        axis.text.x = element_text(size = 10))

gcar6 <- ggplot(carg, aes(y = carg[, 6],
                          x = reorder(cavd, as.numeric(carg[, 6])),
                          fill = Grupo,
                          alpha = alpha3)) +
  geom_bar(stat = 'identity') +   
  guides(fill = F, alpha = F) +
  scale_y_continuous(breaks = seq(0, 1, .25),
                     labels = percent_format( accuracy = 1),
                     limits = c(0, 1)) +
  coord_flip() +
  guides(fill = F) +
  scale_fill_manual(values = wes_palette('IsleofDogs1')) +
  ggtitle(colnames(carg)[6]) +
  theme_minimal() +
  theme(axis.text.y = element_text(color = color.car.w[carg[ order(carg[[6]]),] %>%
                                                         select(Grupo) %>%
                                                         c() %>%
                                                         unlist()],
                                   size = 6,
                                   vjust = -.03),
        axis.title = element_blank(),
        title = element_text(size = 10),
        axis.text.x = element_text(size = 10))


gcar7 <- ggplot(carg, aes(y = carg[, 7],
                          x = reorder(cavd, as.numeric(carg[, 7])),
                          fill = Grupo,
                          alpha = alpha3)) +
  geom_bar(stat = 'identity') +   
  guides(fill = F, alpha = F) +
  scale_y_continuous() +
  coord_flip() +
  scale_fill_manual(values = wes_palette('IsleofDogs1')) +
  ggtitle(colnames(carg)[7]) +
  theme_minimal() +
  theme(axis.text.y = element_text(color = color.car.w[carg[ order(carg[[7]]),] %>%
                                                         select(Grupo) %>%
                                                         c() %>%
                                                         unlist()],
                                   size = 6,
                                   vjust = -.03),
        axis.title = element_blank(),
        title = element_text(size = 10),
        axis.text.x = element_text(size = 10))

gcar8 <- ggplot(carg, aes(y = carg[, 8],
                          x = reorder(cavd, as.numeric(carg[, 8])),
                          fill = Grupo,
                          alpha = alpha3)) +
  geom_bar(stat = 'identity') +   
  guides(fill = F, alpha = F) +
  scale_y_continuous(breaks = seq(0, 1, .25),
                     labels = percent_format( accuracy = 1),
                     limits = c(0, 1)) +
  coord_flip() +
  guides(fill = F) +
  scale_fill_manual(values = wes_palette('IsleofDogs1')) +
  ggtitle(colnames(carg)[8]) +
  theme_minimal() +
  theme(axis.text.y = element_text(color = color.car.w[carg[ order(carg[[8]]),] %>%
                                                         select(Grupo) %>%
                                                         c() %>%
                                                         unlist()],
                                   size = 6,
                                   vjust = -.03),
        axis.title = element_blank(),
        title = element_text(size = 10),
        axis.text.x = element_text(size = 10))

gcar9 <- ggplot(carg, aes(y = carg[, 9],
                          x = reorder(cavd, as.numeric(carg[, 9])),
                          fill = Grupo,
                          alpha = alpha3)) +
  geom_bar(stat = 'identity') +   
  guides(fill = F, alpha = F) +
  scale_y_continuous(breaks = seq(0, 10500, 2000)) +
  coord_flip() +
  scale_fill_manual(values = wes_palette('IsleofDogs1')) +
  ggtitle(colnames(carg)[9]) +
  theme_minimal() +
  theme(axis.text.y = element_text(color = color.car.w[carg[ order(carg[[9]]),] %>%
                                                         select(Grupo) %>%
                                                         c() %>%
                                                         unlist()],
                                   size = 6,
                                   vjust = -.03),
        axis.title = element_blank(),
        title = element_text(size = 10),
        axis.text.x = element_text(size = 10))

ggsave("g1_car_3.jpeg", gcar1, height = 5.8, width = 10.44, units = "cm")
ggsave("g2_car_3.jpeg", gcar2, height = 5.8, width = 10.44, units = "cm")
ggsave("g3_car_3.jpeg", gcar3, height = 5.8, width = 10.44, units = "cm")
ggsave("g4_car_3.jpeg", gcar4, height = 5.8, width = 10.44, units = "cm")
ggsave("g5_car_3.jpeg", gcar5, height = 5.8, width = 10.44, units = "cm")
ggsave("g6_car_3.jpeg", gcar6, height = 5.8, width = 10.44, units = "cm")
ggsave("g7_car_3.jpeg", gcar7, height = 5.8, width = 10.44, units = "cm")
ggsave("g8_car_3.jpeg", gcar8, height = 5.8, width = 10.44, units = "cm")
ggsave("g9_car_3.jpeg", gcar9, height = 5.8, width = 10.44, units = "cm")


####Grupo 4.pres.car####

setwd("C:/Users/lgarcia/Documents/2019/12 Diciembre/Graficos/Contexto/Grupo 4")

gcar1 <-  ggplot(carg, aes(y = carg[, 1],
                           x = reorder(cavd, as.numeric(carg[, 1])),
                           fill = Grupo,
                           alpha = alpha4)) +
  geom_bar(stat = 'identity') +
  guides(fill = F, alpha = F) +
  scale_y_continuous(breaks = seq(0, 1500, 250)) +
  coord_flip() +
  scale_fill_manual(values = wes_palette('IsleofDogs1')) +
  ggtitle(colnames(carg)[1]) +
  theme_minimal() +
  theme(axis.text.y = element_text(color = color.car.w[carg[ order(carg[[1]]),] %>%
                                                         select(Grupo) %>%
                                                         c() %>%
                                                         unlist()],
                                   size = 6,
                                   vjust = -.03),
        axis.title = element_blank(),
        title = element_text(size = 10),
        axis.text.x = element_text(size = 10))

gcar2 <- ggplot(carg, aes(y = carg[, 2],
                          x = reorder(cavd, as.numeric(carg[, 2])),
                          fill = Grupo,
                          alpha = alpha4)) +
  geom_bar(stat = 'identity') +   
  guides(fill = F, alpha = F) +
  scale_y_continuous(breaks = seq(0, 1, .1),
                     labels = percent_format( accuracy = 1),
                     limits = c(0, .8)) +
  coord_flip() +
  scale_fill_manual(values = wes_palette('IsleofDogs1')) +
  ggtitle(colnames(carg)[2]) +
  theme_minimal() +
  theme(axis.text.y = element_text(color = color.car.w[carg[ order(carg[[2]]),] %>%
                                                         select(Grupo) %>%
                                                         c() %>%
                                                         unlist()],
                                   size = 6,
                                   vjust = -.03),
        axis.title = element_blank(),
        title = element_text(size = 10),
        axis.text.x = element_text(size = 10))

gcar3 <- ggplot(carg, aes(y = carg[, 3],
                          x = reorder(cavd, as.numeric(carg[, 3])),
                          fill = Grupo,
                          alpha = alpha4)) +
  geom_bar(stat = 'identity') +   
  guides(fill = F, alpha = F) +
  scale_y_continuous(breaks = seq(0, 1, .25),
                     labels = percent_format( accuracy = 1),
                     limits = c(0, 1)) +
  coord_flip() +
  guides(fill = F) +
  scale_fill_manual(values = wes_palette('IsleofDogs1')) +
  ggtitle(colnames(carg)[3]) +
  theme_minimal() +
  theme(axis.text.y = element_text(color = color.car.w[carg[ order(carg[[3]]),] %>%
                                                         select(Grupo) %>%
                                                         c() %>%
                                                         unlist()],
                                   size = 6,
                                   vjust = -.03),
        axis.title = element_blank(),
        title = element_text(size = 10),
        axis.text.x = element_text(size = 10))


gcar4 <- ggplot(carg, aes(y = carg[, 4],
                          x = reorder(cavd, as.numeric(carg[, 4])),
                          fill = Grupo,
                          alpha = alpha4)) +
  geom_bar(stat = 'identity') +   
  guides(fill = F, alpha = F) +
  scale_y_continuous(breaks = seq(0, 1, .25),
                     labels = percent_format( accuracy = 1),
                     limits = c(0, 1)) +
  coord_flip() +
  guides(fill = F) +
  scale_fill_manual(values = wes_palette('IsleofDogs1')) +
  ggtitle(colnames(carg)[4]) +
  theme_minimal() +
  theme(axis.text.y = element_text(color = color.car.w[carg[ order(carg[[4]]),] %>%
                                                         select(Grupo) %>%
                                                         c() %>%
                                                         unlist()],
                                   size = 6,
                                   vjust = -.03),
        axis.title = element_blank(),
        title = element_text(size = 10),
        axis.text.x = element_text(size = 10))

gcar5 <- ggplot(carg, aes(y = carg[, 5],
                          x = reorder(cavd, as.numeric(carg[, 5])),
                          fill = Grupo,
                          alpha = alpha4)) +
  geom_bar(stat = 'identity') +   
  guides(fill = F, alpha = F) +
  scale_y_continuous(breaks = seq(0, 1, .25),
                     labels = percent_format( accuracy = 1),
                     limits = c(0, 1)) +
  coord_flip() +
  guides(fill = F) +
  scale_fill_manual(values = wes_palette('IsleofDogs1')) +
  ggtitle(colnames(carg)[5]) +
  theme_minimal() +
  theme(axis.text.y = element_text(color = color.car.w[carg[ order(carg[[5]]),] %>%
                                                         select(Grupo) %>%
                                                         c() %>%
                                                         unlist()],
                                   size = 6,
                                   vjust = -.03),
        axis.title = element_blank(),
        title = element_text(size = 10),
        axis.text.x = element_text(size = 10))

gcar6 <- ggplot(carg, aes(y = carg[, 6],
                          x = reorder(cavd, as.numeric(carg[, 6])),
                          fill = Grupo,
                          alpha = alpha4)) +
  geom_bar(stat = 'identity') +   
  guides(fill = F, alpha = F) +
  scale_y_continuous(breaks = seq(0, 1, .25),
                     labels = percent_format( accuracy = 1),
                     limits = c(0, 1)) +
  coord_flip() +
  guides(fill = F) +
  scale_fill_manual(values = wes_palette('IsleofDogs1')) +
  ggtitle(colnames(carg)[6]) +
  theme_minimal() +
  theme(axis.text.y = element_text(color = color.car.w[carg[ order(carg[[6]]),] %>%
                                                         select(Grupo) %>%
                                                         c() %>%
                                                         unlist()],
                                   size = 6,
                                   vjust = -.03),
        axis.title = element_blank(),
        title = element_text(size = 10),
        axis.text.x = element_text(size = 10))


gcar7 <- ggplot(carg, aes(y = carg[, 7],
                          x = reorder(cavd, as.numeric(carg[, 7])),
                          fill = Grupo,
                          alpha = alpha4)) +
  geom_bar(stat = 'identity') +   
  guides(fill = F, alpha = F) +
  scale_y_continuous() +
  coord_flip() +
  scale_fill_manual(values = wes_palette('IsleofDogs1')) +
  ggtitle(colnames(carg)[7]) +
  theme_minimal() +
  theme(axis.text.y = element_text(color = color.car.w[carg[ order(carg[[7]]),] %>%
                                                         select(Grupo) %>%
                                                         c() %>%
                                                         unlist()],
                                   size = 6,
                                   vjust = -.03),
        axis.title = element_blank(),
        title = element_text(size = 10),
        axis.text.x = element_text(size = 10))

gcar8 <- ggplot(carg, aes(y = carg[, 8],
                          x = reorder(cavd, as.numeric(carg[, 8])),
                          fill = Grupo,
                          alpha = alpha4)) +
  geom_bar(stat = 'identity') +   
  guides(fill = F, alpha = F) +
  scale_y_continuous(breaks = seq(0, 1, .25),
                     labels = percent_format( accuracy = 1),
                     limits = c(0, 1)) +
  coord_flip() +
  guides(fill = F) +
  scale_fill_manual(values = wes_palette('IsleofDogs1')) +
  ggtitle(colnames(carg)[8]) +
  theme_minimal() +
  theme(axis.text.y = element_text(color = color.car.w[carg[ order(carg[[8]]),] %>%
                                                         select(Grupo) %>%
                                                         c() %>%
                                                         unlist()],
                                   size = 6,
                                   vjust = -.03),
        axis.title = element_blank(),
        title = element_text(size = 10),
        axis.text.x = element_text(size = 10))

gcar9 <- ggplot(carg, aes(y = carg[, 9],
                          x = reorder(cavd, as.numeric(carg[, 9])),
                          fill = Grupo,
                          alpha = alpha4)) +
  geom_bar(stat = 'identity') +   
  guides(fill = F, alpha = F) +
  scale_y_continuous(breaks = seq(0, 10500, 2000)) +
  coord_flip() +
  scale_fill_manual(values = wes_palette('IsleofDogs1')) +
  ggtitle(colnames(carg)[9]) +
  theme_minimal() +
  theme(axis.text.y = element_text(color = color.car.w[carg[ order(carg[[9]]),] %>%
                                                         select(Grupo) %>%
                                                         c() %>%
                                                         unlist()],
                                   size = 6,
                                   vjust = -.03),
        axis.title = element_blank(),
        title = element_text(size = 10),
        axis.text.x = element_text(size = 10))

ggsave("g1_car_4.jpeg", gcar1, height = 5.8, width = 10.44, units = "cm")
ggsave("g2_car_4.jpeg", gcar2, height = 5.8, width = 10.44, units = "cm")
ggsave("g3_car_4.jpeg", gcar3, height = 5.8, width = 10.44, units = "cm")
ggsave("g4_car_4.jpeg", gcar4, height = 5.8, width = 10.44, units = "cm")
ggsave("g5_car_4.jpeg", gcar5, height = 5.8, width = 10.44, units = "cm")
ggsave("g6_car_4.jpeg", gcar6, height = 5.8, width = 10.44, units = "cm")
ggsave("g7_car_4.jpeg", gcar7, height = 5.8, width = 10.44, units = "cm")
ggsave("g8_car_4.jpeg", gcar8, height = 5.8, width = 10.44, units = "cm")
ggsave("g9_car_4.jpeg", gcar9, height = 5.8, width = 10.44, units = "cm")



####Gráficos presentacion.ges####

gesg <- gesg %>%
  mutate(
    Grupo = if_else(cavd %in% c("Concepción", "La Cisterna", "La Florida", 
                                "Maipú", "Providencia", "Pudahuel", "Puente Alto", 
                                "Rancagua", "Recoleta", "Santiago", "Talca", "Temuco" , 
                                "Valparaíso" ), "1", "2"),
    alpha1 = ifelse(Grupo == "1", 1, .2),
    alpha2 = ifelse(Grupo == "2", 1, .2)
  )

####Grupo 1.pres.ges####

setwd("C:/Users/lgarcia/Documents/2019/12 Diciembre/Graficos/Gestión/Grupo 1")

gges1 <- ggplot(gesg, aes(y = gesg[, 1],
                          x = reorder(cavd, gesg[, 1]),
                          fill = Grupo,
                          alpha = alpha1)) +
  geom_bar(stat = 'identity') +
  scale_y_continuous(labels = percent_format( accuracy = 1)) +
  coord_flip() +
  guides(fill = F, alpha = F) +
  scale_fill_manual(values = wes_palette('Cavalcanti1')) +
  ggtitle(colnames(gesg)[1]) +
  theme_minimal() +
  theme(axis.text.y = element_blank(),
        axis.title = element_blank(),
        title = element_text(size = 10),
        axis.text.x = element_text(size = 10))

gges2 <- ggplot(gesg, aes(y = gesg[, 2],
                          x = reorder(cavd, gesg[, 2]),
                          fill = Grupo, alpha = alpha1)) +
  geom_bar(stat = 'identity') +
  scale_y_continuous(labels = percent_format( accuracy = 1)) +
  coord_flip() +
  guides(fill = F, alpha = F) +
  scale_fill_manual(values = wes_palette('Cavalcanti1')) +
  ggtitle("Sesiones con\nasistencia") +
theme_minimal() +
  theme(axis.text.y = element_blank(),
        axis.title = element_blank(),
        title = element_text(size = 10),
        axis.text.x = element_text(size = 10))

gges3 <- ggplot(gesg, aes(y = gesg[, 3],
                          x = reorder(cavd, as.numeric(gesg[, 3])),
                          fill = Grupo, alpha = alpha1)) +
  geom_bar(stat = 'identity') +
  coord_flip() +
  guides(fill = F, alpha = F) +
  scale_fill_manual(values = wes_palette('Cavalcanti1')) +
  ggtitle("Capacidad instalada\n(AB)") +
theme_minimal() +
  theme(axis.text.y = element_blank(),
        axis.title = element_blank(),
        title = element_text(size = 10),
        axis.text.x = element_text(size = 10))



gges4 <- ggplot(gesg, aes(y = gesg[, 4],
                          x = reorder(cavd, as.numeric(gesg[, 4])),
                          fill = Grupo, alpha = alpha1)) +
  geom_bar(stat = 'identity') +
  coord_flip() +
  guides(fill = F, alpha = F) +
  scale_fill_manual(values = wes_palette('Cavalcanti1')) +
  ggtitle("Capacidad instalada\n(PS)") +
  theme_minimal() +
  theme(axis.text.y = element_blank(),
        axis.title = element_blank(),
        title = element_text(size = 10),
        axis.text.x = element_text(size = 10))

gges5 <- ggplot(gesg, aes(y = gesg[, 5],
                          x = reorder(cavd, as.numeric(gesg[, 5])),
                          fill = Grupo, alpha = alpha1)) +
  geom_bar(stat = 'identity') +
  coord_flip() +
  guides(fill = F, alpha = F) +
  scale_fill_manual(values = wes_palette('Cavalcanti1')) +
  ggtitle("Capacidad instalada\n(TS)") +
  theme_minimal() +       
  theme(axis.text.y = element_blank(),
        axis.title = element_blank(),
        title = element_text(size = 10),
        axis.text.x = element_text(size = 10))




gges6 <- ggplot(gesg, aes(y = gesg[, 6],
                          x = reorder(cavd, as.numeric(gesg[, 6])),
                          fill = Grupo, alpha = alpha1)) +
  geom_bar(stat = 'identity') +
  coord_flip() +
  guides(fill = F, alpha = F) +
  scale_fill_manual(values = wes_palette('Cavalcanti1')) +
  scale_y_continuous(labels = percent_format( accuracy = 1),
                     limits = c(0, 1)) +
  ggtitle("Tiempo dedicado a\nActividades administrativas") +
  theme_minimal() +
  theme(axis.text.y = element_blank(),
        axis.title = element_blank(),
        title = element_text(size = 10),
        axis.text.x = element_text(size = 10))

gges7 <- ggplot(gesg, aes(y = gesg[, 7],
                          x = reorder(cavd, as.numeric(gesg[, 7])),
                          fill = Grupo, alpha = alpha1)) +
  geom_bar(stat = 'identity') +
  scale_y_continuous(labels = percent_format( accuracy = 1),
                     limits = c(0, 1)) +
  coord_flip() +
  guides(fill = F, alpha = F) +
  scale_fill_manual(values = wes_palette('Cavalcanti1')) +
  ggtitle("Tiempo dedicado a\nActividades directas") +
  theme_minimal() +
  theme(axis.text.y = element_blank(),
        axis.title = element_blank(),
        title = element_text(size = 10),
        axis.text.x = element_text(size = 10))

gges8 <- ggplot(gesg, aes(y = gesg[, 8],
                          x = reorder(cavd, as.numeric(gesg[, 8])),
                          fill = Grupo, alpha = alpha1)) +
  geom_bar(stat = 'identity') +
  scale_y_continuous(labels = percent_format( accuracy = 1),
                     limits = c(0, 1)) +
  coord_flip() +
  guides(fill = F, alpha = F) +
  scale_fill_manual(values = wes_palette('Cavalcanti1')) +
  ggtitle("Tiempo dedicado a\nActividades indirectas") +
  theme_minimal() +
  theme(axis.text.y = element_blank(),
        axis.title = element_blank(),
        title = element_text(size = 10),
        axis.text.x = element_text(size = 10))



gges9 <- ggplot(gesg, aes(y = gesg[, 9],
                          x = reorder(cavd, as.numeric(gesg[, 9])),
                          fill = Grupo, alpha = alpha1)) +
  geom_bar(stat = 'identity') +
  coord_flip() +
  guides(fill = F, alpha = F) +
  scale_fill_manual(values = wes_palette('Cavalcanti1')) +
  ggtitle("Promedio de casos\nvigentes diarios") +
  theme_minimal() +
  theme(axis.text.y = element_blank(),
        axis.title = element_blank(),
        title = element_text(size = 10),
        axis.text.x = element_text(size = 10))

gges10 <- ggplot(gesg, aes(y = gesg[, 10],
                           x = reorder(cavd, as.numeric(gesg[, 10])),
                           fill = Grupo, alpha = alpha1)) +
  geom_bar(stat = 'identity') +
  coord_flip() +
  guides(fill = F, alpha = F) +
  scale_fill_manual(values = wes_palette('Cavalcanti1')) +
  ggtitle("Duración promedio\nde casos") +
  theme_minimal() +
  theme(axis.text.y = element_blank(),
        axis.title = element_blank(),
        title = element_text(size = 10),
        axis.text.x = element_text(size = 10))

gges11 <- ggplot(gesg, aes(y = gesg[, 11],
                           x = reorder(cavd, as.numeric(gesg[, 11])),
                           fill = Grupo, alpha = alpha1)) +
  geom_bar(stat = 'identity') +
  scale_y_continuous(labels = percent_format( accuracy = 1),
                     limits = c(0, 1)) +
  coord_flip() +
  guides(fill = F, alpha = F) +
  scale_fill_manual(values = wes_palette('Cavalcanti1')) +
  ggtitle("Vía de ingreso\ninterna") +
  theme_minimal() +
  theme(axis.text.y = element_blank(),
        axis.title = element_blank(),
        title = element_text(size = 10),
        axis.text.x = element_text(size = 10))

gges12 <- ggplot(gesg, aes(y = gesg[, 12],
                           x = reorder(cavd, as.numeric(gesg[, 12])),
                           fill = Grupo, alpha = alpha1)) +
  geom_bar(stat = 'identity') +
  scale_y_continuous(labels = percent_format( accuracy = 1),
                     limits = c(0, 1)) +
  coord_flip() +
  guides(fill = F, alpha = F) +
  scale_fill_manual(values = wes_palette('Cavalcanti1')) +
  ggtitle(colnames(gesg)[12]) +
  theme_minimal() +
  theme(axis.text.y = element_blank(),
        axis.title = element_blank(),
        title = element_text(size = 10),
        axis.text.x = element_text(size = 10))

gges13 <- ggplot(gesg, aes(y = gesg[, 13],
                           x = reorder(cavd, as.numeric(gesg[, 13])),
                           fill = Grupo, alpha = alpha1)) +
  geom_bar(stat = 'identity') +
  scale_y_continuous(labels = percent_format( accuracy = 1),
                     limits = c(0, 1)) +
  coord_flip() +
  guides(fill = F, alpha = F) +
  scale_fill_manual(values = wes_palette('Cavalcanti1')) +
  ggtitle(colnames(gesg)[13]) +
  theme_minimal() +
  theme(axis.text.y = element_blank(),
        axis.title = element_blank(),
        title = element_text(size = 10),
        axis.text.x = element_text(size = 10))


gges14 <- ggplot(gesg, aes(y = gesg[, 14],
                           x = reorder(cavd, as.numeric(gesg[, 14])),
                           fill = Grupo, alpha = alpha1)) +
  geom_bar(stat = 'identity') +
  scale_y_continuous(labels = percent_format( accuracy = 1),
                     limits = c(0, 1)) +
  coord_flip() +
  guides(fill = F, alpha = F) +
  scale_fill_manual(values = wes_palette('Cavalcanti1')) +
  ggtitle(colnames(gesg)[14]) +
  theme_minimal() +
  theme(axis.text.y = element_blank(),
        axis.title = element_blank(),
        title = element_text(size = 10),
        axis.text.x = element_text(size = 10))

gges15 <- ggplot(gesg, aes(y = gesg[, 15],
                           x = reorder(cavd, as.numeric(gesg[, 15])),
                           fill = Grupo, alpha = alpha1)) +
  geom_bar(stat = 'identity') +
  scale_y_continuous(labels = percent_format( accuracy = 1),
                     limits = c(0, 1)) +
  coord_flip() +
  guides(fill = F, alpha = F) +
  scale_fill_manual(values = wes_palette('Cavalcanti1')) +
  ggtitle(colnames(gesg)[15]) +
  theme_minimal() +
  theme(axis.text.y = element_blank(),
        axis.title = element_blank(),
        title = element_text(size = 10),
        axis.text.x = element_text(size = 10))

gges16 <- ggplot(gesg, aes(y = gesg[, 16],
                           x = reorder(cavd, as.numeric(gesg[, 16])),
                           fill = Grupo, alpha = alpha1)) +
  geom_bar(stat = 'identity') +
  scale_y_continuous(labels = percent_format( accuracy = 1),
                     limits = c(0, 1)) +
  coord_flip() +
  guides(fill = F, alpha = F) +
  scale_fill_manual(values = wes_palette('Cavalcanti1')) +
  ggtitle(colnames(gesg)[16]) +
  theme_minimal() +
  theme(axis.text.y = element_blank(),
        axis.title = element_blank(),
        title = element_text(size = 10),
        axis.text.x = element_text(size = 10))

gges17 <- ggplot(gesg, aes(y = gesg[, 17],
                           x = reorder(cavd, as.numeric(gesg[, 17])),
                           fill = Grupo, alpha = alpha1)) +
  geom_bar(stat = 'identity') +
  scale_y_continuous(labels = percent_format( accuracy = 1),
                     limits = c(0, 1)) +
  coord_flip() +
  guides(fill = F, alpha = F) +
  scale_fill_manual(values = wes_palette('Cavalcanti1')) +
  ggtitle(colnames(gesg)[17]) +
  theme_minimal() +
  theme(axis.text.y = element_blank(),
        axis.title = element_blank(),
        title = element_text(size = 10),
        axis.text.x = element_text(size = 10))

ggsave("g1_ges_1.jpeg", gges1, height = 5.8, width = 10.44, units = "cm")
ggsave("g1_ges_2.jpeg", gges2, height = 5.8, width = 10.44, units = "cm")
ggsave("g1_ges_3.jpeg", gges3, height = 5.8, width = 10.44, units = "cm")
ggsave("g1_ges_4.jpeg", gges4, height = 5.8, width = 10.44, units = "cm")
ggsave("g1_ges_5.jpeg", gges5, height = 5.8, width = 10.44, units = "cm")
ggsave("g1_ges_6.jpeg", gges6, height = 5.8, width = 10.44, units = "cm")
ggsave("g1_ges_7.jpeg", gges7, height = 5.8, width = 10.44, units = "cm")
ggsave("g1_ges_8.jpeg", gges8, height = 5.8, width = 10.44, units = "cm")
ggsave("g1_ges_9.jpeg", gges9, height = 5.8, width = 10.44, units = "cm")
ggsave("g1_ges_10.jpeg", gges10, height = 5.8, width = 10.44, units = "cm")
ggsave("g1_ges_11.jpeg", gges11, height = 5.8, width = 10.44, units = "cm")
ggsave("g1_ges_12.jpeg", gges12, height = 5.8, width = 10.44, units = "cm")
ggsave("g1_ges_13.jpeg", gges13, height = 5.8, width = 10.44, units = "cm")
ggsave("g1_ges_14.jpeg", gges14, height = 5.8, width = 10.44, units = "cm")
ggsave("g1_ges_15.jpeg", gges15, height = 5.8, width = 10.44, units = "cm")
ggsave("g1_ges_16.jpeg", gges16, height = 5.8, width = 10.44, units = "cm")
ggsave("g1_ges_17.jpeg", gges17, height = 5.8, width = 10.44, units = "cm")

ggsave("g1_ges_1_b.jpeg", gges1, height = 3.8, width = 4.46, units = "cm")
ggsave("g1_ges_2_b.jpeg", gges2, height = 3.8, width = 4.46, units = "cm")
ggsave("g1_ges_3_b.jpeg", gges3, height = 3.8, width = 4.46, units = "cm")
ggsave("g1_ges_4_b.jpeg", gges4, height = 3.8, width = 4.46, units = "cm")
ggsave("g1_ges_5_b.jpeg", gges5, height = 3.8, width = 4.46, units = "cm")
ggsave("g1_ges_6_b.jpeg", gges6, height = 3.8, width = 4.46, units = "cm")
ggsave("g1_ges_7_b.jpeg", gges7, height = 3.8, width = 4.46, units = "cm")
ggsave("g1_ges_8_b.jpeg", gges8, height = 3.8, width = 4.46, units = "cm")
ggsave("g1_ges_9_b.jpeg", gges9, height = 3.8, width = 4.46, units = "cm")
ggsave("g1_ges_10_b.jpeg", gges10, height = 3.8, width = 4.46, units = "cm")
ggsave("g1_ges_11_b.jpeg", gges11, height = 3.8, width = 4.46, units = "cm")
ggsave("g1_ges_12_b.jpeg", gges12, height = 3.8, width = 4.46, units = "cm")
ggsave("g1_ges_13_b.jpeg", gges13, height = 3.8, width = 4.46, units = "cm")
ggsave("g1_ges_14_b.jpeg", gges14, height = 3.8, width = 4.46, units = "cm")
ggsave("g1_ges_15_b.jpeg", gges15, height = 3.8, width = 4.46, units = "cm")
ggsave("g1_ges_16_b.jpeg", gges16, height = 3.8, width = 4.46, units = "cm")
ggsave("g1_ges_17_b.jpeg", gges17, height = 3.8, width = 4.46, units = "cm")


####Grupo 2.pres.ges####

setwd("C:/Users/lgarcia/Documents/2019/12 Diciembre/Graficos/Gestión/Grupo 2")

gges1 <- ggplot(gesg, aes(y = gesg[, 1],
                          x = reorder(cavd, gesg[, 1]),
                          fill = Grupo,
                          alpha = alpha2)) +
  geom_bar(stat = 'identity') +
  scale_y_continuous(labels = percent_format( accuracy = 1)) +
  coord_flip() +
  guides(fill = F, alpha = F) +
  scale_fill_manual(values = wes_palette('Cavalcanti1')) +
  ggtitle(colnames(gesg)[1]) +
  theme_minimal() +
  theme(axis.text.y = element_blank(),
        axis.title = element_blank(),
        title = element_text(size = 10),
        axis.text.x = element_text(size = 10))

gges2 <- ggplot(gesg, aes(y = gesg[, 2],
                          x = reorder(cavd, gesg[, 2]),
                          fill = Grupo, alpha = alpha2)) +
  geom_bar(stat = 'identity') +
  scale_y_continuous(labels = percent_format( accuracy = 1)) +
  coord_flip() +
  guides(fill = F, alpha = F) +
  scale_fill_manual(values = wes_palette('Cavalcanti1')) +
  ggtitle("Sesiones con\nasistencia") +
  theme_minimal() +
  theme(axis.text.y = element_blank(),
        axis.title = element_blank(),
        title = element_text(size = 10),
        axis.text.x = element_text(size = 10))

gges3 <- ggplot(gesg, aes(y = gesg[, 3],
                          x = reorder(cavd, as.numeric(gesg[, 3])),
                          fill = Grupo, alpha = alpha2)) +
  geom_bar(stat = 'identity') +
  coord_flip() +
  guides(fill = F, alpha = F) +
  scale_fill_manual(values = wes_palette('Cavalcanti1')) +
  ggtitle("Capacidad instalada\n(AB)") +
  theme_minimal() +
  theme(axis.text.y = element_blank(),
        axis.title = element_blank(),
        title = element_text(size = 10),
        axis.text.x = element_text(size = 10))



gges4 <- ggplot(gesg, aes(y = gesg[, 4],
                          x = reorder(cavd, as.numeric(gesg[, 4])),
                          fill = Grupo, alpha = alpha2)) +
  geom_bar(stat = 'identity') +
  coord_flip() +
  guides(fill = F, alpha = F) +
  scale_fill_manual(values = wes_palette('Cavalcanti1')) +
  ggtitle("Capacidad instalada\n(PS)") +
  theme_minimal() +
  theme(axis.text.y = element_blank(),
        axis.title = element_blank(),
        title = element_text(size = 10),
        axis.text.x = element_text(size = 10))

gges5 <- ggplot(gesg, aes(y = gesg[, 5],
                          x = reorder(cavd, as.numeric(gesg[, 5])),
                          fill = Grupo, alpha = alpha2)) +
  geom_bar(stat = 'identity') +
  coord_flip() +
  guides(fill = F, alpha = F) +
  scale_fill_manual(values = wes_palette('Cavalcanti1')) +
  ggtitle("Capacidad instalada\n(TS)") +
  theme_minimal() +       
  theme(axis.text.y = element_blank(),
        axis.title = element_blank(),
        title = element_text(size = 10),
        axis.text.x = element_text(size = 10))




gges6 <- ggplot(gesg, aes(y = gesg[, 6],
                          x = reorder(cavd, as.numeric(gesg[, 6])),
                          fill = Grupo, alpha = alpha2)) +
  geom_bar(stat = 'identity') +
  coord_flip() +
  guides(fill = F, alpha = F) +
  scale_fill_manual(values = wes_palette('Cavalcanti1')) +
  scale_y_continuous(labels = percent_format( accuracy = 1),
                     limits = c(0, 1)) +
  ggtitle("Tiempo dedicado a\nActividades administrativas") +
  theme_minimal() +
  theme(axis.text.y = element_blank(),
        axis.title = element_blank(),
        title = element_text(size = 10),
        axis.text.x = element_text(size = 10))

gges7 <- ggplot(gesg, aes(y = gesg[, 7],
                          x = reorder(cavd, as.numeric(gesg[, 7])),
                          fill = Grupo, alpha = alpha2)) +
  geom_bar(stat = 'identity') +
  scale_y_continuous(labels = percent_format( accuracy = 1),
                     limits = c(0, 1)) +
  coord_flip() +
  guides(fill = F, alpha = F) +
  scale_fill_manual(values = wes_palette('Cavalcanti1')) +
  ggtitle("Tiempo dedicado a\nActividades directas") +
  theme_minimal() +
  theme(axis.text.y = element_blank(),
        axis.title = element_blank(),
        title = element_text(size = 10),
        axis.text.x = element_text(size = 10))

gges8 <- ggplot(gesg, aes(y = gesg[, 8],
                          x = reorder(cavd, as.numeric(gesg[, 8])),
                          fill = Grupo, alpha = alpha2)) +
  geom_bar(stat = 'identity') +
  scale_y_continuous(labels = percent_format( accuracy = 1),
                     limits = c(0, 1)) +
  coord_flip() +
  guides(fill = F, alpha = F) +
  scale_fill_manual(values = wes_palette('Cavalcanti1')) +
  ggtitle("Tiempo dedicado a\nActividades indirectas") +
  theme_minimal() +
  theme(axis.text.y = element_blank(),
        axis.title = element_blank(),
        title = element_text(size = 10),
        axis.text.x = element_text(size = 10))



gges9 <- ggplot(gesg, aes(y = gesg[, 9],
                          x = reorder(cavd, as.numeric(gesg[, 9])),
                          fill = Grupo, alpha = alpha2)) +
  geom_bar(stat = 'identity') +
  coord_flip() +
  guides(fill = F, alpha = F) +
  scale_fill_manual(values = wes_palette('Cavalcanti1')) +
  ggtitle("Promedio de casos\nvigentes diarios") +
  theme_minimal() +
  theme(axis.text.y = element_blank(),
        axis.title = element_blank(),
        title = element_text(size = 10),
        axis.text.x = element_text(size = 10))

gges10 <- ggplot(gesg, aes(y = gesg[, 10],
                           x = reorder(cavd, as.numeric(gesg[, 10])),
                           fill = Grupo, alpha = alpha2)) +
  geom_bar(stat = 'identity') +
  coord_flip() +
  guides(fill = F, alpha = F) +
  scale_fill_manual(values = wes_palette('Cavalcanti1')) +
  ggtitle("Duración promedio\nde casos") +
  theme_minimal() +
  theme(axis.text.y = element_blank(),
        axis.title = element_blank(),
        title = element_text(size = 10),
        axis.text.x = element_text(size = 10))

gges11 <- ggplot(gesg, aes(y = gesg[, 11],
                           x = reorder(cavd, as.numeric(gesg[, 11])),
                           fill = Grupo, alpha = alpha2)) +
  geom_bar(stat = 'identity') +
  scale_y_continuous(labels = percent_format( accuracy = 1),
                     limits = c(0, 1)) +
  coord_flip() +
  guides(fill = F, alpha = F) +
  scale_fill_manual(values = wes_palette('Cavalcanti1')) +
  ggtitle("Vía de ingreso\ninterna") +
  theme_minimal() +
  theme(axis.text.y = element_blank(),
        axis.title = element_blank(),
        title = element_text(size = 10),
        axis.text.x = element_text(size = 10))

gges12 <- ggplot(gesg, aes(y = gesg[, 12],
                           x = reorder(cavd, as.numeric(gesg[, 12])),
                           fill = Grupo, alpha = alpha2)) +
  geom_bar(stat = 'identity') +
  scale_y_continuous(labels = percent_format( accuracy = 1),
                     limits = c(0, 1)) +
  coord_flip() +
  guides(fill = F, alpha = F) +
  scale_fill_manual(values = wes_palette('Cavalcanti1')) +
  ggtitle(colnames(gesg)[12]) +
  theme_minimal() +
  theme(axis.text.y = element_blank(),
        axis.title = element_blank(),
        title = element_text(size = 10),
        axis.text.x = element_text(size = 10))

gges13 <- ggplot(gesg, aes(y = gesg[, 13],
                           x = reorder(cavd, as.numeric(gesg[, 13])),
                           fill = Grupo, alpha = alpha2)) +
  geom_bar(stat = 'identity') +
  scale_y_continuous(labels = percent_format( accuracy = 1),
                     limits = c(0, 1)) +
  coord_flip() +
  guides(fill = F, alpha = F) +
  scale_fill_manual(values = wes_palette('Cavalcanti1')) +
  ggtitle(colnames(gesg)[13]) +
  theme_minimal() +
  theme(axis.text.y = element_blank(),
        axis.title = element_blank(),
        title = element_text(size = 10),
        axis.text.x = element_text(size = 10))


gges14 <- ggplot(gesg, aes(y = gesg[, 14],
                           x = reorder(cavd, as.numeric(gesg[, 14])),
                           fill = Grupo, alpha = alpha2)) +
  geom_bar(stat = 'identity') +
  scale_y_continuous(labels = percent_format( accuracy = 1),
                     limits = c(0, 1)) +
  coord_flip() +
  guides(fill = F, alpha = F) +
  scale_fill_manual(values = wes_palette('Cavalcanti1')) +
  ggtitle(colnames(gesg)[14]) +
  theme_minimal() +
  theme(axis.text.y = element_blank(),
        axis.title = element_blank(),
        title = element_text(size = 10),
        axis.text.x = element_text(size = 10))

gges15 <- ggplot(gesg, aes(y = gesg[, 15],
                           x = reorder(cavd, as.numeric(gesg[, 15])),
                           fill = Grupo, alpha = alpha2)) +
  geom_bar(stat = 'identity') +
  scale_y_continuous(labels = percent_format( accuracy = 1),
                     limits = c(0, 1)) +
  coord_flip() +
  guides(fill = F, alpha = F) +
  scale_fill_manual(values = wes_palette('Cavalcanti1')) +
  ggtitle(colnames(gesg)[15]) +
  theme_minimal() +
  theme(axis.text.y = element_blank(),
        axis.title = element_blank(),
        title = element_text(size = 10),
        axis.text.x = element_text(size = 10))

gges16 <- ggplot(gesg, aes(y = gesg[, 16],
                           x = reorder(cavd, as.numeric(gesg[, 16])),
                           fill = Grupo, alpha = alpha2)) +
  geom_bar(stat = 'identity') +
  scale_y_continuous(labels = percent_format( accuracy = 1),
                     limits = c(0, 1)) +
  coord_flip() +
  guides(fill = F, alpha = F) +
  scale_fill_manual(values = wes_palette('Cavalcanti1')) +
  ggtitle(colnames(gesg)[16]) +
  theme_minimal() +
  theme(axis.text.y = element_blank(),
        axis.title = element_blank(),
        title = element_text(size = 10),
        axis.text.x = element_text(size = 10))

gges17 <- ggplot(gesg, aes(y = gesg[, 17],
                           x = reorder(cavd, as.numeric(gesg[, 17])),
                           fill = Grupo, alpha = alpha2)) +
  geom_bar(stat = 'identity') +
  scale_y_continuous(labels = percent_format( accuracy = 1),
                     limits = c(0, 1)) +
  coord_flip() +
  guides(fill = F, alpha = F) +
  scale_fill_manual(values = wes_palette('Cavalcanti1')) +
  ggtitle(colnames(gesg)[17]) +
  theme_minimal() +
  theme(axis.text.y = element_blank(),
        axis.title = element_blank(),
        title = element_text(size = 10),
        axis.text.x = element_text(size = 10))

ggsave("g2_ges_1.jpeg", gges1, height = 5.8, width = 10.44, units = "cm")
ggsave("g2_ges_2.jpeg", gges2, height = 5.8, width = 10.44, units = "cm")
ggsave("g2_ges_3.jpeg", gges3, height = 5.8, width = 10.44, units = "cm")
ggsave("g2_ges_4.jpeg", gges4, height = 5.8, width = 10.44, units = "cm")
ggsave("g2_ges_5.jpeg", gges5, height = 5.8, width = 10.44, units = "cm")
ggsave("g2_ges_6.jpeg", gges6, height = 5.8, width = 10.44, units = "cm")
ggsave("g2_ges_7.jpeg", gges7, height = 5.8, width = 10.44, units = "cm")
ggsave("g2_ges_8.jpeg", gges8, height = 5.8, width = 10.44, units = "cm")
ggsave("g2_ges_9.jpeg", gges9, height = 5.8, width = 10.44, units = "cm")
ggsave("g2_ges_10.jpeg", gges10, height = 5.8, width = 10.44, units = "cm")
ggsave("g2_ges_11.jpeg", gges11, height = 5.8, width = 10.44, units = "cm")
ggsave("g2_ges_12.jpeg", gges12, height = 5.8, width = 10.44, units = "cm")
ggsave("g2_ges_13.jpeg", gges13, height = 5.8, width = 10.44, units = "cm")
ggsave("g2_ges_14.jpeg", gges14, height = 5.8, width = 10.44, units = "cm")
ggsave("g2_ges_15.jpeg", gges15, height = 5.8, width = 10.44, units = "cm")
ggsave("g2_ges_16.jpeg", gges16, height = 5.8, width = 10.44, units = "cm")
ggsave("g2_ges_17.jpeg", gges17, height = 5.8, width = 10.44, units = "cm")

ggsave("g2_ges_1_b.jpeg", gges1, height = 3.8, width = 4.46, units = "cm")
ggsave("g2_ges_2_b.jpeg", gges2, height = 3.8, width = 4.46, units = "cm")
ggsave("g2_ges_3_b.jpeg", gges3, height = 3.8, width = 4.46, units = "cm")
ggsave("g2_ges_4_b.jpeg", gges4, height = 3.8, width = 4.46, units = "cm")
ggsave("g2_ges_5_b.jpeg", gges5, height = 3.8, width = 4.46, units = "cm")
ggsave("g2_ges_6_b.jpeg", gges6, height = 3.8, width = 4.46, units = "cm")
ggsave("g2_ges_7_b.jpeg", gges7, height = 3.8, width = 4.46, units = "cm")
ggsave("g2_ges_8_b.jpeg", gges8, height = 3.8, width = 4.46, units = "cm")
ggsave("g2_ges_9_b.jpeg", gges9, height = 3.8, width = 4.46, units = "cm")
ggsave("g2_ges_10_b.jpeg", gges10, height = 3.8, width = 4.46, units = "cm")
ggsave("g2_ges_11_b.jpeg", gges11, height = 3.8, width = 4.46, units = "cm")
ggsave("g2_ges_12_b.jpeg", gges12, height = 3.8, width = 4.46, units = "cm")
ggsave("g2_ges_13_b.jpeg", gges13, height = 3.8, width = 4.46, units = "cm")
ggsave("g2_ges_14_b.jpeg", gges14, height = 3.8, width = 4.46, units = "cm")
ggsave("g2_ges_15_b.jpeg", gges15, height = 3.8, width = 4.46, units = "cm")
ggsave("g2_ges_16_b.jpeg", gges16, height = 3.8, width = 4.46, units = "cm")
ggsave("g2_ges_17_b.jpeg", gges17, height = 3.8, width = 4.46, units = "cm")



####Gráficos presentacion.int####

intg <- intg %>%
  mutate(Grupo = if_else(cavd %in% c("Castro", "Huechuraba", "La Unión", 
                                     "Punta Arenas"), "1",
                         if_else(cavd %in% c("Angol", "Arica",     "Colina", "Copiapó",   
                                             "Coyhaique", "La Serena", "Linares",   
                                             "Osorno", "Rancagua", "Recoleta" , 
                                             "Talca"), "2", "3")
                         ),
    alpha1 = if_else(Grupo == 1, 1, .2),
         alpha2 = if_else(Grupo == 2, 1, .2),
         alpha3 = if_else(Grupo == 3, 1, .2)
         )

####Grupo1.pres.int####

setwd("C:/Users/lgarcia/Documents/2019/12 Diciembre/Graficos/Intervención/Grupo 1")

gint1 <- ggplot(intg, aes(y = intg[, 1],
                          x = reorder(cavd, as.numeric(intg[, 1])),
                          fill = Grupo, 
                          alpha = alpha1)) +
  geom_bar(stat = 'identity') +
  scale_y_continuous(limits = c(0, 8.5)) +
  coord_flip() +
  guides(fill = F, alpha = F) +
  scale_fill_manual(values = color.int.w) +
  ggtitle("Prestaciones jurídicas\npor víctima") +
  theme_minimal() +
  theme(axis.text.y = element_text(color = color.int.w[intg[ order(intg[[1]]),] %>%
                                                         select(Grupo) %>%
                                                         c() %>%
                                                         unlist()],
                                   size = 6,
                                   vjust = -.03),
        axis.title = element_blank(),
        title = element_text(size = 10),
        axis.text.x = element_text(size = 10))


gint2 <- ggplot(intg, aes(y = intg[, 2],
                          x = reorder(cavd, as.numeric(intg[, 2])),
                          fill = Grupo, alpha = alpha1)) +
  geom_bar(stat = 'identity') +
  scale_y_continuous(limits = c(0, 8.5)) +
  coord_flip() +
  guides(fill = F, alpha = F) +
  scale_fill_manual(values = color.int.w) +
  ggtitle("Prestaciones sociales\npor víctima") +
  theme_minimal() +
  theme(axis.text.y = element_text(color = color.int.w[intg[ order(intg[[2]]),] %>%
                                                         select(Grupo) %>%
                                                         c() %>%
                                                         unlist()],
                                   size = 6,
                                   vjust = -.03),
        axis.title = element_blank(),
        title = element_text(size = 10),
        axis.text.x = element_text(size = 10))

gint3 <- ggplot(intg, aes(y = intg[, 3],
                          x = reorder(cavd, as.numeric(intg[, 3])),
                          fill = Grupo, alpha = alpha1)) +
  geom_bar(stat = 'identity') +
  scale_y_continuous(limits = c(0, 8.5)) +
  coord_flip() +
  guides(fill = F, alpha = F) +
  scale_fill_manual(values = color.int.w) +
  ggtitle("Prestaciones\npor víctima") +
  theme_minimal() +
  theme(axis.text.y = element_text(color = color.int.w[intg[ order(intg[[3]]),] %>%
                                                         select(Grupo) %>%
                                                         c() %>%
                                                         unlist()],
                                   size = 6,
                                   vjust = -.03),
        axis.title = element_blank(),
        title = element_text(size = 10),
        axis.text.x = element_text(size = 10))


gint4 <- ggplot(intg, aes(y = intg[, 4],
                          x = reorder(cavd, as.numeric(intg[, 4])),
                          fill = Grupo, alpha = alpha1)) +
  geom_bar(stat = 'identity') +
  coord_flip() +
  guides(fill = F, alpha = F) +
  scale_fill_manual(values = color.int.w) +
  ggtitle(colnames(intg)[4]) +
  theme_minimal() +
  theme(axis.text.y = element_text(color = color.int.w[intg[ order(intg[[4]]),] %>%
                                                         select(Grupo) %>%
                                                         c() %>%
                                                         unlist()],
                                   size = 6,
                                   vjust = -.03),
        axis.title = element_blank(),
        title = element_text(size = 10),
        axis.text.x = element_text(size = 10))

gint5 <- ggplot(intg, aes(y = intg[, 5],
                          x = reorder(cavd, as.numeric(intg[, 5])),
                          fill = Grupo, alpha = alpha1)) +
  geom_bar(stat = 'identity') +
  coord_flip() +
  guides(fill = F, alpha = F) +
  scale_fill_manual(values = color.int.w) +
  ggtitle(colnames(intg)[5]) +
  theme_minimal() +
  theme(axis.text.y = element_text(color = color.int.w[intg[ order(intg[[5]]),] %>%
                                                         select(Grupo) %>%
                                                         c() %>%
                                                         unlist()],
                                   size = 6,
                                   vjust = -.03),
        axis.title = element_blank(),
        title = element_text(size = 10),
        axis.text.x = element_text(size = 10))

gint6 <- ggplot(intg, aes(y = intg[, 6],
                          x = reorder(cavd, as.numeric(intg[, 6])),
                          fill = Grupo, alpha = alpha1)) +
  geom_bar(stat = 'identity') +
  coord_flip() +
  guides(fill = F, alpha = F) +
  scale_fill_manual(values = color.int.w) +
  ggtitle(colnames(intg)[6]) +
  theme_minimal() +
  theme(axis.text.y = element_text(color = color.int.w[intg[ order(intg[[6]]),] %>%
                                                         select(Grupo) %>%
                                                         c() %>%
                                                         unlist()],
                                   size = 6,
                                   vjust = -.03),
        axis.title = element_blank(),
        title = element_text(size = 10),
        axis.text.x = element_text(size = 10))

gint7 <- ggplot(intg, aes(y = intg[, 7],
                          x = reorder(cavd, as.numeric(intg[, 7])),
                          fill = Grupo, alpha = alpha1)) +
  geom_bar(stat = 'identity') +
  coord_flip() +
  guides(fill = F, alpha = F) +
  scale_fill_manual(values = color.int.w) +
  ggtitle(colnames(intg)[5]) +
  theme_minimal() +
  theme(axis.text.y = element_text(color = color.int.w[intg[ order(intg[[7]]),] %>%
                                                         select(Grupo) %>%
                                                         c() %>%
                                                         unlist()],
                                   size = 6,
                                   vjust = -.03),
        axis.title = element_blank(),
        title = element_text(size = 10),
        axis.text.x = element_text(size = 10))

ggsave("g1_int_1.jpeg", gint1, height = 5.8, width = 10.44, units = "cm")
ggsave("g1_int_2.jpeg", gint2, height = 5.8, width = 10.44, units = "cm")
ggsave("g1_int_3.jpeg", gint3, height = 5.8, width = 10.44, units = "cm")
ggsave("g1_int_4.jpeg", gint4, height = 5.8, width = 10.44, units = "cm")
ggsave("g1_int_5.jpeg", gint5, height = 5.8, width = 10.44, units = "cm")
ggsave("g1_int_6.jpeg", gint6, height = 5.8, width = 10.44, units = "cm")
ggsave("g1_int_7.jpeg", gint7, height = 5.8, width = 10.44, units = "cm")


####Grupo2.pres.int####

setwd("C:/Users/lgarcia/Documents/2019/12 Diciembre/Graficos/Intervención/Grupo 2")

gint1 <- ggplot(intg, aes(y = intg[, 1],
                          x = reorder(cavd, as.numeric(intg[, 1])),
                          fill = Grupo, 
                          alpha = alpha2)) +
  geom_bar(stat = 'identity') +
  scale_y_continuous(limits = c(0, 8.5)) +
  coord_flip() +
  guides(fill = F, alpha = F) +
  scale_fill_manual(values = color.int.w) +
  ggtitle("Prestaciones jurídicas\npor víctima") +
  theme_minimal() +
  theme(axis.text.y = element_text(color = color.int.w[intg[ order(intg[[1]]),] %>%
                                                         select(Grupo) %>%
                                                         c() %>%
                                                         unlist()],
                                   size = 6,
                                   vjust = -.03),
        axis.title = element_blank(),
        title = element_text(size = 10),
        axis.text.x = element_text(size = 10))


gint2 <- ggplot(intg, aes(y = intg[, 2],
                          x = reorder(cavd, as.numeric(intg[, 2])),
                          fill = Grupo, alpha = alpha2)) +
  geom_bar(stat = 'identity') +
  scale_y_continuous(limits = c(0, 8.5)) +
  coord_flip() +
  guides(fill = F, alpha = F) +
  scale_fill_manual(values = color.int.w) +
  ggtitle("Prestaciones sociales\npor víctima") +
  theme_minimal() +
  theme(axis.text.y = element_text(color = color.int.w[intg[ order(intg[[2]]),] %>%
                                                         select(Grupo) %>%
                                                         c() %>%
                                                         unlist()],
                                   size = 6,
                                   vjust = -.03),
        axis.title = element_blank(),
        title = element_text(size = 10),
        axis.text.x = element_text(size = 10))

gint3 <- ggplot(intg, aes(y = intg[, 3],
                          x = reorder(cavd, as.numeric(intg[, 3])),
                          fill = Grupo, alpha = alpha2)) +
  geom_bar(stat = 'identity') +
  scale_y_continuous(limits = c(0, 8.5)) +
  coord_flip() +
  guides(fill = F, alpha = F) +
  scale_fill_manual(values = color.int.w) +
  ggtitle("Prestaciones\npor víctima") +
  theme_minimal() +
  theme(axis.text.y = element_text(color = color.int.w[intg[ order(intg[[3]]),] %>%
                                                         select(Grupo) %>%
                                                         c() %>%
                                                         unlist()],
                                   size = 6,
                                   vjust = -.03),
        axis.title = element_blank(),
        title = element_text(size = 10),
        axis.text.x = element_text(size = 10))

g11 = grid.arrange(gint1, gint2,
                   gint3, g_legend(gint1.l))

ggsave("g11.jpeg", g11, height = 6.5, width = 6, units = "in")

gint4 <- ggplot(intg, aes(y = intg[, 4],
                          x = reorder(cavd, as.numeric(intg[, 4])),
                          fill = Grupo, alpha = alpha2)) +
  geom_bar(stat = 'identity') +
  coord_flip() +
  guides(fill = F, alpha = F) +
  scale_fill_manual(values = color.int.w) +
  ggtitle(colnames(intg)[4]) +
  theme_minimal() +
  theme(axis.text.y = element_text(color = color.int.w[intg[ order(intg[[4]]),] %>%
                                                         select(Grupo) %>%
                                                         c() %>%
                                                         unlist()],
                                   size = 6,
                                   vjust = -.03),
        axis.title = element_blank(),
        title = element_text(size = 10),
        axis.text.x = element_text(size = 10))

gint5 <- ggplot(intg, aes(y = intg[, 5],
                          x = reorder(cavd, as.numeric(intg[, 5])),
                          fill = Grupo, alpha = alpha2)) +
  geom_bar(stat = 'identity') +
  coord_flip() +
  guides(fill = F, alpha = F) +
  scale_fill_manual(values = color.int.w) +
  ggtitle(colnames(intg)[5]) +
  theme_minimal() +
  theme(axis.text.y = element_text(color = color.int.w[intg[ order(intg[[5]]),] %>%
                                                         select(Grupo) %>%
                                                         c() %>%
                                                         unlist()],
                                   size = 6,
                                   vjust = -.03),
        axis.title = element_blank(),
        title = element_text(size = 10),
        axis.text.x = element_text(size = 10))

gint6 <- ggplot(intg, aes(y = intg[, 6],
                          x = reorder(cavd, as.numeric(intg[, 6])),
                          fill = Grupo, alpha = alpha2)) +
  geom_bar(stat = 'identity') +
  coord_flip() +
  guides(fill = F, alpha = F) +
  scale_fill_manual(values = color.int.w) +
  ggtitle(colnames(intg)[6]) +
  theme_minimal() +
  theme(axis.text.y = element_text(color = color.int.w[intg[ order(intg[[6]]),] %>%
                                                         select(Grupo) %>%
                                                         c() %>%
                                                         unlist()],
                                   size = 6,
                                   vjust = -.03),
        axis.title = element_blank(),
        title = element_text(size = 10),
        axis.text.x = element_text(size = 10))

gint7 <- ggplot(intg, aes(y = intg[, 7],
                          x = reorder(cavd, as.numeric(intg[, 7])),
                          fill = Grupo, alpha = alpha2)) +
  geom_bar(stat = 'identity') +
  coord_flip() +
  guides(fill = F, alpha = F) +
  scale_fill_manual(values = color.int.w) +
  ggtitle(colnames(intg)[5]) +
  theme_minimal() +
  theme(axis.text.y = element_text(color = color.int.w[intg[ order(intg[[7]]),] %>%
                                                         select(Grupo) %>%
                                                         c() %>%
                                                         unlist()],
                                   size = 6,
                                   vjust = -.03),
        axis.title = element_blank(),
        title = element_text(size = 10),
        axis.text.x = element_text(size = 10))

ggsave("g2_int_1.jpeg", gint1, height = 5.8, width = 10.44, units = "cm")
ggsave("g2_int_2.jpeg", gint2, height = 5.8, width = 10.44, units = "cm")
ggsave("g2_int_3.jpeg", gint3, height = 5.8, width = 10.44, units = "cm")
ggsave("g2_int_4.jpeg", gint4, height = 5.8, width = 10.44, units = "cm")
ggsave("g2_int_5.jpeg", gint5, height = 5.8, width = 10.44, units = "cm")
ggsave("g2_int_6.jpeg", gint6, height = 5.8, width = 10.44, units = "cm")
ggsave("g2_int_7.jpeg", gint7, height = 5.8, width = 10.44, units = "cm")



####Grupo3.pres.int####

setwd("C:/Users/lgarcia/Documents/2019/12 Diciembre/Graficos/Intervención/Grupo 3")

gint1 <- ggplot(intg, aes(y = intg[, 1],
                          x = reorder(cavd, as.numeric(intg[, 1])),
                          fill = Grupo, 
                          alpha = alpha3)) +
  geom_bar(stat = 'identity') +
  scale_y_continuous(limits = c(0, 8.5)) +
  coord_flip() +
  guides(fill = F, alpha = F) +
  scale_fill_manual(values = color.int.w) +
  ggtitle("Prestaciones jurídicas\npor víctima") +
  theme_minimal() +
  theme(axis.text.y = element_text(color = color.int.w[intg[ order(intg[[1]]),] %>%
                                                         select(Grupo) %>%
                                                         c() %>%
                                                         unlist()],
                                   size = 6,
                                   vjust = -.03),
        axis.title = element_blank(),
        title = element_text(size = 10),
        axis.text.x = element_text(size = 10))


gint2 <- ggplot(intg, aes(y = intg[, 2],
                          x = reorder(cavd, as.numeric(intg[, 2])),
                          fill = Grupo, alpha = alpha3)) +
  geom_bar(stat = 'identity') +
  scale_y_continuous(limits = c(0, 8.5)) +
  coord_flip() +
  guides(fill = F, alpha = F) +
  scale_fill_manual(values = color.int.w) +
  ggtitle("Prestaciones sociales\npor víctima") +
  theme_minimal() +
  theme(axis.text.y = element_text(color = color.int.w[intg[ order(intg[[2]]),] %>%
                                                         select(Grupo) %>%
                                                         c() %>%
                                                         unlist()],
                                   size = 6,
                                   vjust = -.03),
        axis.title = element_blank(),
        title = element_text(size = 10),
        axis.text.x = element_text(size = 10))

gint3 <- ggplot(intg, aes(y = intg[, 3],
                          x = reorder(cavd, as.numeric(intg[, 3])),
                          fill = Grupo, alpha = alpha3)) +
  geom_bar(stat = 'identity') +
  scale_y_continuous(limits = c(0, 8.5)) +
  coord_flip() +
  guides(fill = F, alpha = F) +
  scale_fill_manual(values = color.int.w) +
  ggtitle("Prestaciones\npor víctima") +
  theme_minimal() +
  theme(axis.text.y = element_text(color = color.int.w[intg[ order(intg[[3]]),] %>%
                                                         select(Grupo) %>%
                                                         c() %>%
                                                         unlist()],
                                   size = 6,
                                   vjust = -.03),
        axis.title = element_blank(),
        title = element_text(size = 10),
        axis.text.x = element_text(size = 10))

g11 = grid.arrange(gint1, gint2,
                   gint3, g_legend(gint1.l))

ggsave("g11.jpeg", g11, height = 6.5, width = 6, units = "in")

gint4 <- ggplot(intg, aes(y = intg[, 4],
                          x = reorder(cavd, as.numeric(intg[, 4])),
                          fill = Grupo, alpha = alpha3)) +
  geom_bar(stat = 'identity') +
  coord_flip() +
  guides(fill = F, alpha = F) +
  scale_fill_manual(values = color.int.w) +
  ggtitle(colnames(intg)[4]) +
  theme_minimal() +
  theme(axis.text.y = element_text(color = color.int.w[intg[ order(intg[[4]]),] %>%
                                                         select(Grupo) %>%
                                                         c() %>%
                                                         unlist()],
                                   size = 6,
                                   vjust = -.03),
        axis.title = element_blank(),
        title = element_text(size = 10),
        axis.text.x = element_text(size = 10))

gint5 <- ggplot(intg, aes(y = intg[, 5],
                          x = reorder(cavd, as.numeric(intg[, 5])),
                          fill = Grupo, alpha = alpha3)) +
  geom_bar(stat = 'identity') +
  coord_flip() +
  guides(fill = F, alpha = F) +
  scale_fill_manual(values = color.int.w) +
  ggtitle(colnames(intg)[5]) +
  theme_minimal() +
  theme(axis.text.y = element_text(color = color.int.w[intg[ order(intg[[5]]),] %>%
                                                         select(Grupo) %>%
                                                         c() %>%
                                                         unlist()],
                                   size = 6,
                                   vjust = -.03),
        axis.title = element_blank(),
        title = element_text(size = 10),
        axis.text.x = element_text(size = 10))

gint6 <- ggplot(intg, aes(y = intg[, 6],
                          x = reorder(cavd, as.numeric(intg[, 6])),
                          fill = Grupo, alpha = alpha3)) +
  geom_bar(stat = 'identity') +
  coord_flip() +
  guides(fill = F, alpha = F) +
  scale_fill_manual(values = color.int.w) +
  ggtitle(colnames(intg)[6]) +
  theme_minimal() +
  theme(axis.text.y = element_text(color = color.int.w[intg[ order(intg[[6]]),] %>%
                                                         select(Grupo) %>%
                                                         c() %>%
                                                         unlist()],
                                   size = 6,
                                   vjust = -.03),
        axis.title = element_blank(),
        title = element_text(size = 10),
        axis.text.x = element_text(size = 10))

gint7 <- ggplot(intg, aes(y = intg[, 7],
                          x = reorder(cavd, as.numeric(intg[, 7])),
                          fill = Grupo, alpha = alpha3)) +
  geom_bar(stat = 'identity') +
  coord_flip() +
  guides(fill = F, alpha = F) +
  scale_fill_manual(values = color.int.w) +
  ggtitle(colnames(intg)[5]) +
  theme_minimal() +
  theme(axis.text.y = element_text(color = color.int.w[intg[ order(intg[[7]]),] %>%
                                                         select(Grupo) %>%
                                                         c() %>%
                                                         unlist()],
                                   size = 6,
                                   vjust = -.03),
        axis.title = element_blank(),
        title = element_text(size = 10),
        axis.text.x = element_text(size = 10))

ggsave("g3_int_1.jpeg", gint1, height = 5.8, width = 10.44, units = "cm")
ggsave("g3_int_2.jpeg", gint2, height = 5.8, width = 10.44, units = "cm")
ggsave("g3_int_3.jpeg", gint3, height = 5.8, width = 10.44, units = "cm")
ggsave("g3_int_4.jpeg", gint4, height = 5.8, width = 10.44, units = "cm")
ggsave("g3_int_5.jpeg", gint5, height = 5.8, width = 10.44, units = "cm")
ggsave("g3_int_6.jpeg", gint6, height = 5.8, width = 10.44, units = "cm")
ggsave("g3_int_7.jpeg", gint7, height = 5.8, width = 10.44, units = "cm")

