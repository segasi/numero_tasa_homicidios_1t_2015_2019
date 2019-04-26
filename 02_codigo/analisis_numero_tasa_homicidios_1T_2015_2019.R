### Paquetes ----
library(pacman)
p_load(cowplot, extrafont, ggcal, ggrepel, grid, gridExtra, ineq, janitor, kableExtra, knitr, lubridate, readxl, rmarkdown, scales, sf, svglite, tidyverse, treemapify, wesanderson, zoo)

### Setup general ----
Sys.setlocale("LC_ALL", "es_ES.UTF-8") 
options(scipen = 9999)
theme_set(theme_gray())

### Definir tema de gráficas ----
tema <-  theme_minimal() +
  theme(text = element_text(family = "Didact Gothic Regular", color = "grey35"),
        plot.title = element_text(size = 28, face = "bold", margin = margin(10,0,20,0), family = "Trebuchet MS Bold", color = "grey25"),
        plot.subtitle = element_text(size = 16, face = "bold", colour = "#666666", margin = margin(0, 0, 20, 0), family = "Didact Gothic Regular"),
        plot.caption = element_text(hjust = 0, size = 15),
        panel.grid = element_line(linetype = 2), 
        panel.grid.minor = element_blank(),
        legend.position = "bottom",
        legend.title = element_text(size = 16, face = "bold", family = "Trebuchet MS Bold"),
        legend.text = element_text(size = 14, family = "Didact Gothic Regular"),
        legend.title.align = 0.5,
        axis.title = element_text(size = 18, hjust = 1, face = "bold", margin = margin(0,0,0,0), family = "Didact Gothic Regular"),
        axis.text = element_text(size = 16, face = "bold", family = "Didact Gothic Regular"))


### Importar datos ----

# CONAPO - Proyecciones de poblacón 

# Fuente: Población a mitad de año. Para la República Mexicana el periodo es de 1950-2050, para las entidades federativas el periodo es de 1970-2050, CONAPO, url: http://www.conapo.gob.mx/work/models/CONAPO/Datos_Abiertos/Proyecciones2018/pob_mit_proyecciones.csv (consultada el 14 de marzo)

poblacion <- 
  read_delim("Google/R/10 recursos/datos/conapo/pob_mit_proyecciones.csv", "," , locale = locale(encoding = "latin1")) %>% 
  clean_names()

# SNSP - Incidencia delictiva 

# Fuente: Base de datos de incidencia delictiva del fuero común, SNSP, url: https://bit.ly/2viCyUS

incidencia <- 
  read_excel("Google/R/10 recursos/datos/snsp/Estatal Delitos - marzo 2019.xlsx") %>% 
  clean_names()


### Preparar datos para análisis ----

# Calcular proyecciones de población nacional para los años 2015, 2016, 2017, 2018 y 2019 ----

pob_nal <- # Creo *pob_nal* para guardar el resultado
  poblacion %>% 
  filter(ano > 2014 & ano < 2020, 
         entidad == "República Mexicana") %>% 
  group_by(ano) %>% 
  summarise(pob_tot = sum(poblacion)) %>% 
  ungroup()

# Calcular el número acumulado de homicidios dolosos en el primer trimestre de los años 2015, 2016, 2017, 2018 y 2019

hd_nal <- # Creo *hd_nal* para guardar el resultado
  incidencia %>% 
  filter(subtipo_de_delito == "Homicidio doloso") %>% 
  select(ano, enero:diciembre) %>% 
  gather(key = "mes",
         value = "numero",
         -ano) %>% 
  filter(mes %in% c("enero", "febrero", "marzo")) %>% 
  group_by(ano) %>% 
  summarise(num_acumulado = sum(numero)) %>% 
  ungroup()
