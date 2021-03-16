# Paquetes

library(tidyverse)
library(readxl)
library(janitor)
library(zip)
library(extrafont)
library(showtext)
library(ggtext)
library("Cairo")


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

## Código de @segasi (Sebastián Garrido) para la conversión de la información de codificada (el cual actualicé a la nueva clasificación de la SSA)

# Lectura de datos
mx <- read_csv("01Datos/210315COVID19MEXICO.csv") %>% # Cambiar nombre de archivo según la fecha a cargar
  clean_names()

# Filtramos la entidad objetivo para hacer más eficiente el proceso
son <- mx %>% filter(entidad_res==26)

# Catálogo de municipios
cve_mpo <- 
  read_excel("./01Datos/diccionario_datos_covid19/201128 Catalogos.xlsx", 
             sheet = "Catálogo MUNICIPIOS") %>% 
  clean_names() %>% 
  mutate(cve_mpo = str_c(clave_entidad, clave_municipio))

# Convertir valores numéricos en texto 
son <- son %>% 
  mutate(origen = case_when(origen == 1 ~ "USMER",
                            origen == 2 ~ "Fuera de USMER",
                            origen == 3 ~ "No especificado"),
         sector = case_when(sector == 1 ~ "Cruz Roja",
                            sector == 2 ~ "DIF",
                            sector == 3 ~ "Estatal",
                            sector == 4 ~ "IMSS",
                            sector == 5 ~ "IMSS-Bienestar",
                            sector == 6 ~ "ISSSTE",
                            sector == 7 ~ "Municipal",
                            sector == 8 ~ "PEMEX",
                            sector == 9 ~ "Privada",
                            sector == 10 ~ "SEDENA",
                            sector == 11 ~ "SEMAR",
                            sector == 12 ~ "SSA",
                            sector == 13 ~ "Universitario",
                            sector == 99 ~ "No especificado"),
         sexo = case_when(sexo == 1 ~ "Mujer",
                          sexo == 2 ~"Hombre",
                          sexo == 99 ~ "No especificado"),
         tipo_paciente = case_when(tipo_paciente == 1 ~ "Ambulatorio",
                                   tipo_paciente == 2 ~ "Hospitalizado",
                                   tipo_paciente == 99 ~ "No especificado"),
         nacionalidad = case_when(nacionalidad == 1 ~ "Mexicana",
                                  nacionalidad == 2 ~ "Extranjera",
                                  nacionalidad == 99 ~ "No especificada"),
         toma_muestra_antigeno = case_when(resultado_antigeno  == 1 ~ "Positivo SARS-CoV-2",
                                           resultado_lab  == 2 ~ "Negativo SARS-CoV-2",
                                           resultado_lab  == 97 ~ "No aplica, caso sin muestra"),
         resultado_lab = case_when(resultado_lab  == 1 ~ "Positivo SARS-CoV-2",
                                   resultado_lab  == 2 ~ "No positivo SARS-CoV-2",
                                   resultado_lab  == 3 ~ "Resultado pendiente",
                                   resultado_lab  == 4 ~ "Resultado no adecuado",
                                   resultado_lab  == 97 ~ "No aplica, caso sin muestra"),
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
         entidad_nac = case_when(entidad_nac == "01" ~ "AGUASCALIENTES",
                                 entidad_nac == "02" ~ "BAJA CALIFORNIA",
                                 entidad_nac == "03" ~ "BAJA CALIFORNIA SUR",
                                 entidad_nac == "04" ~ "CAMPECHE",
                                 entidad_nac == "05" ~ "COAHUILA",
                                 entidad_nac == "06" ~ "COLIMA",
                                 entidad_nac == "07" ~ "CHIAPAS",
                                 entidad_nac == "08" ~ "CHIHUAHUA",
                                 entidad_nac == "09" ~ "CIUDAD DE MÉXICO",
                                 entidad_nac == "10" ~ "DURANGO",
                                 entidad_nac == "11" ~ "GUANAJUATO",
                                 entidad_nac == "12" ~ "GUERRERO",
                                 entidad_nac == "13" ~ "HIDALGO",
                                 entidad_nac == "14" ~ "JALISCO",
                                 entidad_nac == "15" ~ "MÉXICO",
                                 entidad_nac == "16" ~ "MICHOACÁN",
                                 entidad_nac == "17" ~ "MORELOS",
                                 entidad_nac == "18" ~ "NAYARIT",
                                 entidad_nac == "19" ~ "NUEVO LEÓN",
                                 entidad_nac == "20" ~ "OAXACA",
                                 entidad_nac == "21" ~ "PUEBLA",
                                 entidad_nac == "22" ~ "QUERÉTARO",
                                 entidad_nac == "23" ~ "QUINTANA ROO",
                                 entidad_nac == "24" ~ "SAN LUIS POTOSÍ",
                                 entidad_nac == "25" ~ "SINALOA",
                                 entidad_nac == "26" ~ "SONORA",
                                 entidad_nac == "27" ~ "TABASCO",
                                 entidad_nac == "28" ~ "TAMAULIPAS",
                                 entidad_nac == "29" ~ "TLAXCALA",
                                 entidad_nac == "30" ~ "VERACRUZ",
                                 entidad_nac == "31" ~ "YUCATÁN",
                                 entidad_nac == "32" ~ "ZACATECAS",
                                 entidad_nac == "36" ~ "ESTADOS UNIDOS MEXICANOS",
                                 entidad_nac == "97" ~ "NO APLICA",
                                 entidad_nac == "98" ~ "SE IGNORA",
                                 entidad_nac == "99" ~ "NO ESPECIFICADO"),
         entidad_nac = str_to_title(entidad_nac),
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
         entidad_residencia = str_replace(entidad_residencia, " De ", " de "),
         entidad_uni_med = case_when(entidad_um == "01" ~ "AGUASCALIENTES",
                                     entidad_um == "02" ~ "BAJA CALIFORNIA",
                                     entidad_um == "03" ~ "BAJA CALIFORNIA SUR",
                                     entidad_um == "04" ~ "CAMPECHE",
                                     entidad_um == "05" ~ "COAHUILA",
                                     entidad_um == "06" ~ "COLIMA",
                                     entidad_um == "07" ~ "CHIAPAS",
                                     entidad_um == "08" ~ "CHIHUAHUA",
                                     entidad_um == "09" ~ "CIUDAD DE MÉXICO",
                                     entidad_um == "10" ~ "DURANGO",
                                     entidad_um == "11" ~ "GUANAJUATO",
                                     entidad_um == "12" ~ "GUERRERO",
                                     entidad_um == "13" ~ "HIDALGO",
                                     entidad_um == "14" ~ "JALISCO",
                                     entidad_um == "15" ~ "MÉXICO",
                                     entidad_um == "16" ~ "MICHOACÁN",
                                     entidad_um == "17" ~ "MORELOS",
                                     entidad_um == "18" ~ "NAYARIT",
                                     entidad_um == "19" ~ "NUEVO LEÓN",
                                     entidad_um == "20" ~ "OAXACA",
                                     entidad_um == "21" ~ "PUEBLA",
                                     entidad_um == "22" ~ "QUERÉTARO",
                                     entidad_um == "23" ~ "QUINTANA ROO",
                                     entidad_um == "24" ~ "SAN LUIS POTOSÍ",
                                     entidad_um == "25" ~ "SINALOA",
                                     entidad_um == "26" ~ "SONORA",
                                     entidad_um == "27" ~ "TABASCO",
                                     entidad_um == "28" ~ "TAMAULIPAS",
                                     entidad_um == "29" ~ "TLAXCALA",
                                     entidad_um == "30" ~ "VERACRUZ",
                                     entidad_um == "31" ~ "YUCATÁN",
                                     entidad_um == "32" ~ "ZACATECAS",
                                     entidad_um == "36" ~ "ESTADOS UNIDOS MEXICANOS",
                                     entidad_um == "97" ~ "NO APLICA",
                                     entidad_um == "98" ~ "SE IGNORA",
                                     entidad_um == "99" ~ "NO ESPECIFICADO"),
         entidad_uni_med = str_to_title(entidad_uni_med),
         entidad_uni_med = str_replace(entidad_uni_med, " De ", " de "),
         intubado = case_when(intubado == 1 ~ "Sí",
                              intubado == 2 ~ "No",
                              intubado == 97 ~ "No aplica",
                              intubado == 98 ~ "Se ignora",
                              intubado == 99 ~ "No especificado"),
         neumonia = case_when(neumonia == 1 ~ "Sí",
                              neumonia == 2 ~ "No",
                              neumonia == 97 ~ "No aplica",
                              neumonia == 98 ~ "Se ignora",
                              neumonia == 99 ~ "No especificado"),
         embarazo = case_when(embarazo == 1 ~ "Sí",
                              embarazo == 2 ~ "No",
                              embarazo == 97 ~ "No aplica",
                              embarazo == 98 ~ "Se ignora",
                              embarazo == 99 ~ "No especificado"),
         habla_lengua_indig = case_when(habla_lengua_indig == 1 ~ "Sí",
                                        habla_lengua_indig == 2 ~ "No",
                                        habla_lengua_indig== 97 ~ "No aplica", 
                                        habla_lengua_indig== 98 ~ "Se ignora",
                                        habla_lengua_indig == 99 ~ "No especificado"),
         indigena = case_when(indigena == 1 ~ "Sí",
                              indigena == 2 ~ "No"),
         diabetes = case_when(diabetes == 1 ~ "Sí",
                              diabetes == 2 ~ "No",
                              diabetes == 97 ~ "No aplica",
                              diabetes == 98 ~ "Se ignora",
                              diabetes == 99 ~ "No especificado"),
         epoc = case_when(epoc == 1 ~ "Sí",
                          epoc == 2 ~ "No",
                          epoc == 97 ~ "No aplica",
                          epoc == 98 ~ "Se ignora",
                          epoc == 99 ~ "No especificado"),
         asma = case_when(asma == 1 ~ "Sí",
                          asma == 2 ~ "No",
                          asma == 97 ~ "No aplica",
                          asma == 98 ~ "Se ignora",
                          asma == 99 ~ "No especificado"),
         inmusupr = case_when(inmusupr == 1 ~ "Sí",
                              inmusupr == 2 ~ "No",
                              inmusupr == 97 ~ "No aplica",
                              inmusupr == 98 ~ "Se ignora",
                              inmusupr == 99 ~ "No especificado"),
         hipertension = case_when(hipertension == 1 ~ "Sí",
                                  hipertension == 2 ~ "No",
                                  hipertension == 97 ~ "No aplica",
                                  hipertension == 98 ~ "Se ignora",
                                  hipertension == 99 ~ "No especificado"),
         otra_com = case_when(otra_com == 1 ~ "Sí",
                              otra_com == 2 ~ "No",
                              otra_com == 97 ~ "No aplica",
                              otra_com == 98 ~ "Se ignora",
                              otra_com == 99 ~ "No especificado"),
         cardiovascular = case_when(cardiovascular == 1 ~ "Sí",
                                    cardiovascular == 2 ~ "No",
                                    cardiovascular == 97 ~ "No aplica",
                                    cardiovascular == 98 ~ "Se ignora",
                                    cardiovascular == 99 ~ "No especificado"),
         obesidad = case_when(obesidad == 1 ~ "Sí",
                              obesidad == 2 ~ "No",
                              obesidad == 97 ~ "No aplica",
                              obesidad == 98 ~ "Se ignora",
                              obesidad == 99 ~ "No especificado"),
         renal_cronica = case_when(renal_cronica == 1 ~ "Sí",
                                   renal_cronica == 2 ~ "No",
                                   renal_cronica == 97 ~ "No aplica",
                                   renal_cronica == 98 ~ "Se ignora",
                                   renal_cronica == 99 ~ "No especificado"),
         tabaquismo = case_when(tabaquismo == 1 ~ "Sí",
                                tabaquismo == 2 ~ "No",
                                tabaquismo == 97 ~ "No aplica",
                                tabaquismo == 98 ~ "Se ignora",
                                tabaquismo == 99 ~ "No especificado"),
         otro_caso = case_when(otro_caso == 1 ~ "Sí",
                               otro_caso == 2 ~ "No",
                               otro_caso == 97 ~ "No aplica",
                               otro_caso == 98 ~ "Se ignora",
                               otro_caso == 99 ~ "No especificado"),
         uci = case_when(uci == 1 ~ "Sí",
                         uci == 2 ~ "No",
                         uci == 97 ~ "No aplica",
                         uci == 98 ~ "Se ignora",
                         uci == 99 ~ "No especificado"),
         cve_mpo = str_c(entidad_res, municipio_res))


# Unir catálogo de municipios y genera variable "municipio" ----
son <- 
  son %>%
  left_join(cve_mpo %>% select(municipio, cve_mpo), 
            by = "cve_mpo") %>%
  mutate(municipio = str_to_title(municipio),
         municipio = str_replace(municipio, " De ", " de "),
         municipio = str_replace(municipio, " Del ", " del "),
         municipio = str_replace(municipio, " Los ", " los "),
         municipio = str_replace(municipio, " La ", " la ")) 

## Generación de columnas adicionales

# Días desde el inicio de síntomas
son <- 
  son %>% mutate(dias_sintomas=as.numeric(Sys.Date()-fecha_sintomas)) 

# Casos Activos
son <- 
  son %>% mutate(activo=if_else(dias_sintomas>13,"No", "Sí"))

# Decesos
son <- 
  son %>% mutate(deceso=if_else(is.na(fecha_def),"No", "Sí"))

# Días en acudir a la Unidad Médica
son <- 
  son %>% mutate (días_atención = (as.Date(fecha_ingreso)-as.Date(fecha_sintomas)))

# Tiene comorbilidades
son <- 
  son %>% mutate (comorbilidades = if_else((diabetes=="Sí"|
                                                 epoc=="Sí"|
                                                 hipertension=="Sí"|
                                                 asma=="Sí"|
                                                 inmusupr=="Sí"|
                                                 otra_com=="Sí"|
                                                 cardiovascular=="Sí"|
                                                 obesidad=="Sí"|
                                                 renal_cronica=="Sí"|
                                                 tabaquismo=="Sí"), 
                                              "Sí", "No"))
son <- 
  son %>% mutate (mayor60 = if_else(edad>=60,"Sí", "No"))


write.csv(son,(str_c("02Resultados/SONCOVID", str_replace_all(str_replace_all(str_replace_all(Sys.Date(), "\\:", "_"), "-", "_"), " ", "_"),".csv")))

## Gráfica de Decesos

# Filtramos decesos  confirmados y sospechosos
son_decesos <- son %>%  
  select(id_registro, clasificacion_final_simple, deceso, fecha_def) %>% 
  filter(clasificacion_final_simple!="Descartado", deceso=="Sí") 

ggplot(son_decesos, aes(fecha_def, deceso)) +
  geom_jitter(aes(color=clasificacion_final_simple), alpha=0.4, width = 0.9, height = 0.5, size=0.5) + # Cada punto es un deceso
  geom_text(aes(x = as.Date("2020-02-15"), y = "Sí",
                label = "Cada círculo representa a una\npersona fallecida por covid-19"), stat = "unique", family = "Lato Black", #texto aclaratorio
            size = 2.5, color = "#993366")+ 
  geom_vline(xintercept=as.Date("2020-06-01"), linetype="dashed", color = "red", size=0.5) + # Líneas de cambio de semáforo 
  geom_vline(xintercept=as.Date("2020-07-20"), linetype="dashed", color = "orange", size=0.5) +
  geom_vline(xintercept=as.Date("2020-08-31"), linetype="dashed", color = "yellow", size=0.5) +
  geom_vline(xintercept=as.Date("2020-11-09"), linetype="dashed", color = "orange", size=0.5) +
  geom_vline(xintercept=as.Date("2021-02-15"), linetype="dashed", color = "yellow", size=0.5) +
  geom_vline(xintercept=as.Date("2021-03-15"), linetype="dashed", color = "green", size=0.5) +
  scale_x_date(date_breaks = "1 month", date_labels = "%B") +
  scale_color_manual(values=c("#993366", "#58BCBC"))+ 
  guides(colour = guide_legend(nrow = 1)) + # Leyenda en un renglón 
  theme_minimal() + theme (plot.title = element_markdown(family = "Lato Black", size = 25),  # element_markdown para uso de texto enriquecido en título con ggtext
                           plot.subtitle = element_text(family = "Lato Light", size = 10, color = "gray50"), legend.title = element_blank(),
                           strip.text = element_text(family = "Lato Black", size = 8),
                           axis.text.x = element_text(family = "Lato", size =7), axis.text.y = element_blank(),
                           plot.background = element_rect(fill = "white", color = "black", size = 2.5),
                           axis.title.x = element_text(family = "Lato Light", size = 8, hjust=0),
                           axis.title.y = element_text(family = "Lato Light", size =7, hjust=1), 
                           plot.caption = element_text(family = "Lato", size = 8, color = "#58BCBC"), panel.grid.major.x = element_line(size=0.2), 
                           panel.grid.major.y = element_blank(), legend.key.height = unit (0.2, "cm"), legend.key.width = unit (0.2, "cm"),
                           legend.text = element_text(family = "Lato", size = 8),legend.position = c(0,0.95), legend.justification="left",
                           panel.grid.minor= element_blank(), plot.margin = unit(c(0.5,0.5,0.3,0.5), "cm"), legend.margin=margin(t = 0, unit='cm')) +
  labs(y = NULL, 
       x = NULL,legend= NULL, 
       title  = "<span style = 'font-size:16pt'>A un año del primer caso confirmado de Covid-19,</span><br><span style = 'color:#993366'; 'font-size:25pt'>6,362 sonorenses</span> nos hacen falta", 
       subtitle= "Decesos confirmados y sospechosos con residencia en la Entidad por fecha de ocurridos.\nCorte al 15 de marzo de 2021.", 
       caption ="Elaboración: Luis Armando Moreno (@dogomoreno) con información de la Secretaría de Salud federal") # ggtext para texto enriquecido en título

ggsave("03Gráficos/DecesosSonora.png", width = 5 * (16/9), height = 5, type = "cairo", dpi = 300)

