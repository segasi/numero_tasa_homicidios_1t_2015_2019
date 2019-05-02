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
  read_delim("01_datos/pob_mit_proyecciones.csv", "," , locale = locale(encoding = "latin1")) %>% 
  clean_names()

# SNSP - Incidencia delictiva 

# Fuente: Base de datos de incidencia delictiva del fuero común, SNSP, url: https://bit.ly/2viCyUS

incidencia <- 
  read_excel("01_datos/Estatal Delitos - marzo 2019.xlsx") %>% 
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

### Unir datos de homicidios y población ----
hd_nal <- 
  hd_nal %>% 
  left_join(pob_nal, by = "ano")

### Calcular tasas y cambios porcentuales ----
hd_nal <- 
  hd_nal %>% 
  mutate(tasa_trimestral = (num_acumulado/pob_tot)*1e5,
         tasa_trimestral_anualizada = (num_acumulado/pob_tot)*1e5*4, 
         cambio_porcentual_absoluto = ((num_acumulado - lag(num_acumulado))/lag(num_acumulado))*100,
         cambio_porcentual_tasa = ((tasa_trimestral - lag(tasa_trimestral))/lag(tasa_trimestral))*100)


### Gráfica del número absoluto de homicidios en el primer trimestre de cada año, 2015-2019 ----

hd_nal %>% 
  ggplot(aes(x = ano, y = num_acumulado)) +
  geom_col(fill = "grey70") +
  geom_text(aes(label = comma(num_acumulado)), color = "white", 
            size = 6, vjust = 1.4, fontface = "bold") +
  labs(title = str_wrap(str_to_upper("número acumulado de carpetas de investigación por homicidios doloso en el primer trimestre de cada año, 2015-2019"), width = 60),
       x = "\n",
       y = "\n",
       caption = str_wrap("\nSebastián Garrido de Sierra / @segasi / Fuente: Base de datos de incidencia delictiva del fuero común, SNSP, url: https://bit.ly/2viCyUS", width = 150)) +
  scale_y_continuous(expand = c(0, 0)) +
  tema +
  theme(panel.grid.major = element_blank(),
        axis.text.x = element_text(face = "bold", size = 20),
        axis.text.y = element_blank(),
        axis.title.y = element_blank()) +
  ggsave("03_graficas/numero_homicidios_primer_trimestre_2015_2019.png", width = 13, height = 10, dpi = 200)


### Gráfica de la tasa trimestral anualizada de homicidios por cada 100 mil habitantes, 2015-2019 ----

hd_nal %>% 
  ggplot(aes(x = ano, y = tasa_trimestral_anualizada)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = round(tasa_trimestral_anualizada, 2)), color = "white", 
            size = 6, vjust = 1.4, fontface = "bold") +
  labs(title = str_wrap(str_to_upper("tasa trimestral anualizada de homicidios por cada 100 mil habitantes, 2015-2019"), width = 60),
       subtitle = "Datos correspondientes al primer trimestre de cada año",
       x = "\n",
       y = "\n",
       caption = str_wrap("\nSebastián Garrido de Sierra / @segasi / Fuentes: Base de datos de incidencia delictiva del fuero común, SNSP, url: https://bit.ly/2viCyUS y proyecciones poblacionales a mitad de año de CONAPO, url: https://bit.ly/2GF9ayo", width = 150)) +
  scale_y_continuous(expand = c(0, 0)) +
  tema +
  theme(panel.grid.major = element_blank(),
        axis.text.x = element_text(face = "bold", size = 20),
        axis.text.y = element_blank(),
        axis.title.y = element_blank()) +
  ggsave("03_graficas/tasa_homicidios_primer_trimestre_2015_2019.png", width = 13, height = 10, dpi = 200)


### Gráfica del cambio interanual del número asoluto de homicidios y la tasa de homicidios por cada 100 mil habitantes, 2015-2019 ----

hd_nal %>% 
  select(ano, starts_with("cambio")) %>% 
  gather(key = "variable",
         value = "valor",
         -ano) %>% 
  mutate(variable = case_when(variable == "cambio_porcentual_absoluto" ~ "Número absoluto",
                              variable == "cambio_porcentual_tasa" ~ "  Tasa"),
         etiqueta_texto = ifelse(ano == 2019, variable, "")) %>% 
  ggplot(aes(ano, valor, group = variable, color = variable)) +
  geom_line(size = 2) +
  geom_text(aes(label = etiqueta_texto), hjust = -0.1, family = "Didact Gothic Regular", color = "grey35", size = 6) +
  scale_x_continuous(limits = c(2015.9, 2019.3), 
                     labels = c("1T \'16 vs. 1T \'15", "1T \'17 vs. 1T \'16",
                                "1T \'18 vs. 1T \'17", "1T \'19 vs. 1T \'18")) +
  scale_y_continuous(expand = c(0, 0), breaks = seq(0, 30, 5), limits = c(0, 32)) +
  scale_color_manual(values = c("steelblue", "grey70")) +
  labs(title = str_wrap(str_to_upper("cambio porcentual interanual del número absoluto de homicidios y la tasa de homicidios por cada 100 mil habitantes, 2015-2019"), width = 70),
       subtitle = "La gráfica muestra el cambio porcentual de ambas variables en el primer trimestre de cada año respecto al primer trimestre del año previo",  
       x = "\n",
       y = "Cambio %\n",
       caption = str_wrap("\nSebastián Garrido de Sierra / @segasi / Fuentes: Base de datos de incidencia delictiva del fuero común, SNSP, url: https://bit.ly/2viCyUS y proyecciones poblacionales a mitad de año de CONAPO, url: https://bit.ly/2GF9ayo", width = 170)) +
  tema +
  theme(legend.position = "none") +
  ggsave("03_graficas/cambios_porcentuales_numero_y_tasa_homicidios_primer_trimestre_2015_2019.png", width = 16, height = 10, dpi = 200)
  

### Número de carpetas de investigación por homicidio doloso en cada estado en marzo de 2019 ----
incidencia %>% 
  filter(subtipo_de_delito == "Homicidio doloso") %>% 
  select(ano, entidad, enero:diciembre) %>% 
  gather(key = "mes",
         value = "numero",
         -c(ano, entidad)) %>%
  filter(ano == 2019, mes == "marzo") %>%
  group_by(ano, mes, entidad) %>% 
  summarise(num_acumulado = sum(numero)) %>% 
  ungroup() %>% 
  arrange(-num_acumulado) %>% 
  print(n = Inf)


### Cálculo de la tasa de carpetas de investigación por homicidio doloso por cada 100k habitantes en Puebla y Morelos, en marzo de 2019 ----  
poblacion %>% 
  filter(ano > 2018 & ano < 2020, 
         entidad %in% c("Puebla", "Morelos")) %>% 
  group_by(entidad) %>%  
  summarise(pob_tot = sum(poblacion)) %>% 
  ungroup() %>% 
  mutate(ci_homicdios = c(82, 100),
         tasa = (ci_homicdios/pob_tot)*1e5)