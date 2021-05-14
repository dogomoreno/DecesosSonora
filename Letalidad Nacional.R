# Paquetes

library(tidyverse)
library(janitor)
library(extrafont)
library(scales)
library(geofacet)
library(showtext)
library(lubridate)
library(ggtext)

# Datos diarios
covid.url <- "http://datosabiertos.salud.gob.mx/gobmx/salud/datos_abiertos/datos_abiertos_covid19.zip"
covid.archivo <- "./01Datos/datos_abiertos_covid19.zip"
covid.dic.conceptos.url <- "http://datosabiertos.salud.gob.mx/gobmx/salud/datos_abiertos/diccionario_datos_covid19.zip"
covid.dic.conceptos.archivo <- "./01Datos/diccionario.zip"

# Función de descarga
if(!file.exists(covid.archivo)){
  download.file(covid.url, destfile = covid.archivo)  
  unzip(covid.archivo, exdir = "./01Datos")
  download.file(covid.dic.conceptos.url, destfile = covid.dic.conceptos.archivo)  
  unzip(covid.dic.conceptos.archivo, exdir = "./01Datos/diccionario_datos_covid19/")
}

# Función para clasificar el número de semana con año semanas
isodate <- function (x = Sys.Date()) {
  xday <- ISOdate(year(x), month(x), day(x), tz = tz(x))
  dn <- 1 + (wday(x) + 5)%%7
  nth <- xday + ddays(4 - dn)
  jan1 <- ISOdate(year(nth), 1, 1, tz = tz(x))
  return(sprintf("%s/%02d", format(nth, "%y"), 1 + (nth - jan1)%/%ddays(7)))
}

#Tema para gráfico

fuente <- "Elaboración Luis Armando Moreno (@dogomoreno) con información de Datos Abiertos de la Secretaría de Salud del Gobierno Federal al 13/05/2021\nwww.luisarmandomoreno.com"

temaejes <- theme(axis.line = element_line(linetype = "solid"), plot.margin = margin(10, 25, 10, 25),
                  plot.title = element_markdown(family = "Lato Black", size = 40),  
                  plot.subtitle = element_text(family = "Lato Light", size = 12, color = "black"), legend.title = element_blank(),
                  strip.text = element_text(family = "Lato Black", size = 12, color = "black", hjust=0 ),
                  axis.text = element_text(family = "Lato", size =8),
                  plot.background = element_blank(),
                  axis.title.x = element_text(family = "Lato", size = 12, hjust=1),
                  axis.title.y = element_text(family = "Lato Light", size = 12, hjust=1), 
                  plot.caption = element_text(family = "Lato", size = 11),
                  legend.text = element_blank(),
                  legend.position = "none", plot.title.position = 'plot', plot.caption.position = 'plot')


# Días consecutivos
Fecha <- data.frame (fecha_sintomas = seq(as.Date('2020-01-01'), as.Date('2021-05-13'), by = 'days')) %>%  
  mutate(semana = isoweek(fecha_sintomas), año = year(fecha_sintomas), dia=weekdays(fecha_sintomas)) %>% mutate(semana_sintomas=isodate(fecha_sintomas)) %>% 
  group_by(semana_sintomas) %>% mutate(semana=max(fecha_sintomas))


# Carga de datos
mx <- read_csv("01Datos/210513COVID19MEXICO.csv", 
                                 col_types = cols(FECHA_ACTUALIZACION = col_skip(), 
                                                  ORIGEN = col_skip(), SECTOR = col_skip(), 
                                                  ENTIDAD_UM = col_skip(), ENTIDAD_NAC = col_skip(), 
                                                  MUNICIPIO_RES = col_skip(), TIPO_PACIENTE = col_skip(), 
                                                  INTUBADO = col_skip(), NEUMONIA = col_skip(), 
                                                  NACIONALIDAD = col_skip(), EMBARAZO = col_skip(), 
                                                  HABLA_LENGUA_INDIG = col_skip(), 
                                                  INDIGENA = col_skip(), DIABETES = col_skip(), 
                                                  EPOC = col_skip(), ASMA = col_skip(), 
                                                  INMUSUPR = col_skip(), HIPERTENSION = col_skip(), 
                                                  OTRA_COM = col_skip(), CARDIOVASCULAR = col_skip(), 
                                                  OBESIDAD = col_skip(), RENAL_CRONICA = col_skip(), 
                                                  TABAQUISMO = col_skip(), OTRO_CASO = col_skip(), 
                                                  TOMA_MUESTRA_LAB = col_skip(), RESULTADO_LAB = col_skip(), 
                                                  TOMA_MUESTRA_ANTIGENO = col_skip(), 
                                                  RESULTADO_ANTIGENO = col_skip(), 
                                                  MIGRANTE = col_skip(), PAIS_NACIONALIDAD = col_skip(), 
                                                  PAIS_ORIGEN = col_skip(), UCI = col_skip())) %>% 
  clean_names()

# Procesamiento de variables
mx <- 
  mx %>% 
  mutate(sexo = case_when(sexo == 1 ~ "Mujer",
                          sexo == 2 ~"Hombre",
                          sexo == 99 ~ "No especificado"),
         clasificacion_final_simple = case_when(clasificacion_final  == 1 ~ "Caso confirmado",
                                                clasificacion_final  == 2 ~ "Caso confirmado",
                                                clasificacion_final  == 3 ~ "Caso confirmado",
                                                clasificacion_final  == 4 ~ "Descartado",
                                                clasificacion_final  == 5 ~ "Descartado",
                                                clasificacion_final  == 6 ~ "Caso sospechoso",
                                                clasificacion_final  == 7 ~ "Descartado"),
         clasificacion_final = case_when(clasificacion_final  == 1 ~ "Caso de covid-19 confirmado por asociación clínica epidemiológica",
                                         clasificacion_final  == 2 ~ "Caso de covid-19 confirmado por comité de dictaminación",
                                         clasificacion_final  == 3 ~ "Caso de covid-19 confirmado por laboratorio",
                                         clasificacion_final  == 4 ~ "Inválido por laboratorio",
                                         clasificacion_final  == 5 ~ "No realizado por laboratorio",
                                         clasificacion_final  == 6 ~ "Caso sospechoso",
                                         clasificacion_final  == 7 ~ "Negativo a SARS-CoV-2"),
         entidad_residencia = case_when(entidad_res == "01" ~ "AGUASCALIENTES",
                                        entidad_res == "02" ~ "BAJA CALIFORNIA",
                                        entidad_res == "03" ~ "BAJA CALIFORNIA SUR",
                                        entidad_res == "04" ~ "CAMPECHE",
                                        entidad_res == "05" ~ "COAHUILA",
                                        entidad_res == "06" ~ "COLIMA",
                                        entidad_res == "07" ~ "CHIAPAS",
                                        entidad_res == "08" ~ "CHIHUAHUA",
                                        entidad_res == "09" ~ "CIUDAD DE MÉXICO",
                                        entidad_res == "10" ~ "DURANGO",
                                        entidad_res == "11" ~ "GUANAJUATO",
                                        entidad_res == "12" ~ "GUERRERO",
                                        entidad_res == "13" ~ "HIDALGO",
                                        entidad_res == "14" ~ "JALISCO",
                                        entidad_res == "15" ~ "MÉXICO",
                                        entidad_res == "16" ~ "MICHOACÁN",
                                        entidad_res == "17" ~ "MORELOS",
                                        entidad_res == "18" ~ "NAYARIT",
                                        entidad_res == "19" ~ "NUEVO LEÓN",
                                        entidad_res == "20" ~ "OAXACA",
                                        entidad_res == "21" ~ "PUEBLA",
                                        entidad_res == "22" ~ "QUERÉTARO",
                                        entidad_res == "23" ~ "QUINTANA ROO",
                                        entidad_res == "24" ~ "SAN LUIS POTOSÍ",
                                        entidad_res == "25" ~ "SINALOA",
                                        entidad_res == "26" ~ "SONORA",
                                        entidad_res == "27" ~ "TABASCO",
                                        entidad_res == "28" ~ "TAMAULIPAS",
                                        entidad_res == "29" ~ "TLAXCALA",
                                        entidad_res == "30" ~ "VERACRUZ",
                                        entidad_res == "31" ~ "YUCATÁN",
                                        entidad_res == "32" ~ "ZACATECAS",
                                        entidad_res == "36" ~ "ESTADOS UNIDOS MEXICANOS",
                                        entidad_res == "97" ~ "NO APLICA",
                                        entidad_res == "98" ~ "SE IGNORA",
                                        entidad_res == "99" ~ "NO ESPECIFICADO"),
         entidad_residencia = str_to_title(entidad_residencia),
         entidad_residencia = str_replace(entidad_residencia, " De ", " de "))

## Días desde el inicio de síntomas
mx <- 
  mx %>% mutate(dias_sintomas=as.numeric(Sys.Date()-fecha_sintomas)) 

## Casos Activos
mx <- 
  mx %>% mutate(activo=if_else(dias_sintomas>13,"No", "Sí"))

## Decesos
mx <- 
  mx %>% mutate(deceso=if_else(is.na(fecha_def),"No", "Sí"))

## Días en acudir a la Unidad Médica
mx <- mx %>% mutate (días_atención = (as.Date(fecha_ingreso)-as.Date(fecha_sintomas)))

## Mayor 60

mx <- mx %>% mutate (mayor60 = if_else(edad>=60,"Sí", "No"))

mx <- mx %>% mutate(semana_sintomas = paste0(year(fecha_sintomas),"-",week(fecha_sintomas))) %>% left_join(Fecha, by="fecha_sintomas")

mxna <- mx %>%  filter(is.na(semana))


# Filtrado de información
mxcd <- mx %>% 
  filter(clasificacion_final_simple=="Caso confirmado", mayor60=="Sí" ) %>% 
  group_by(entidad_res, entidad_residencia, semana) %>% summarise(casos=sum(clasificacion_final_simple=="Caso confirmado"), decesos=sum(deceso=="Sí")) %>% 
  mutate(letalidad= round(decesos*100/casos, 1)) %>% mutate(code=as.character(as.double(entidad_res)))

# Gráfico
cdedosfilt <- mxcd %>% filter (semana > as.Date("2020-12-13") & semana < as.Date("2021-04-25"))
letasem <- ggplot(cdedosfilt) + 
  geom_area(aes(x=semana, y= letalidad), fill= "#D075A3", alpha=0.1) +
  geom_line(aes(x=semana, y= letalidad), color= "#993366", linetype= "solid", size=.75, arrow=arrow(type="open", length=unit(0.10,"cm"))) +
  #geom_point(aes(x=semana, y= letalidad), fill="#993366", size=1 , shape=21, color="white", stroke=0.5) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(limits = c(0,65)) +
facet_geo(~ code, 
          grid = mx_state_grid2, 
          label = "name",
          scales = "fixed") + 
  theme_minimal() +
  temaejes + theme(strip.background = element_blank(), axis.line = element_blank()) +
  labs(y = "Decesos por cada 100 casos", 
       x = NULL,legend= NULL, title  = "<span style = 'font-size:14pt'>Covid-19 en México:</span><br><span style = 'color:#993366';>Letalidad en adultos mayores</span>", 
       subtitle= "Decesos por cada 100 casos confirmados en mayores de 60 años según semana de inicio de síntomas \nSemanas comprendidas del 14/12/2020 al 25/04/2021\n", caption =fuente) 

letasem

ggsave("03Gráficos/letalidadsemanal.png",letasem, width = 10 * (16/9), height = 10, type = "cairo", dpi = 300)
