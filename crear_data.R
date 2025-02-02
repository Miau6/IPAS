#####Código para crear data en rds de todas las fuentes####
library(tidyverse)
library(lubridate)
library(readxl)
library(janitor)
#vamos a crear un rds con las tas fuentes. 
list.files("../datos")
#####
#CONAPO
conapo <- read.csv("../datos/00_Pob_Inicio_1950_2070.csv", 
                   fileEncoding = "LATIN1"
                   ) %>%  clean_names() %>%
  mutate(across(where(is.character), ~ str_to_title(.)),) %>% 
  filter(ano >= 2015, ano <= 2024)


# Crear grupos quinquenales de edad con una categoría adicional para todas las edades
conapo <- conapo %>% 
  mutate(grupo_edad = cut(edad, breaks = seq(0, 100, by = 5), include.lowest = TRUE, right = FALSE, 
                          labels = paste(seq(0, 95, by = 5), seq(4, 99, by = 5), sep = "-")))
conapo$grupo_edad <- factor(conapo$grupo_edad, levels = c("Todas las edades", levels(conapo$grupo_edad)))


#####
#nacimientos
nacimientos <- read_excel("../datos/nacimientos.xlsx") %>% 
  clean_names()



#####
#muertes maternas
muertes_maternas <- read_excel("../datos/muertes_maternas.xlsx") %>% 
  clean_names() %>% 
  gather(año, Total, defunciones_2005:defunciones_2015
         ) %>% 
  mutate(año=as.integer(str_remove_all(año, "defunciones_"))) %>%
  filter(año>=2015)


#####
#causas obstetricas
causas_obs <- list()
años <- c(2015:2024)
for(i in 1:length(años)){
  
  causas_obs[[i]] <- read_excel("../datos/causas_obs.xlsx", 
                           sheet = paste0(años[i])
                           ) %>% 
    clean_names() %>% 
    mutate(año=años[i])
  
}
total_causas_obs <- bind_rows(causas_obs) %>% 
  relocate(año, .after = "unidad_medica") %>% 
  gather(tipo, Total, 
         ic1_parto_unico_espontaneo:ic8_causas_obstetricas_indirectas) %>% 
  mutate(tipo=str_remove_all(tipo, "ic._")) %>% 
  group_by(unidad_medica, año, tipo) %>% 
  summarise(Total=sum(Total, na.rm = T))


#causas_obstetricas menores de 20
causas_obs_20 <- list()
años <- c(2015:2024)
for(i in 1:length(años)){
  
  causas_obs_20[[i]] <- read_excel("../datos/causas_obs_20.xlsx", 
                                sheet = paste0(años[i])
  ) %>% 
    clean_names() %>% 
    mutate(año=años[i])
  
}
total_causas_obs_20 <- bind_rows(causas_obs_20) %>% 
  relocate(año, .after = "unidad_medica") %>% 
  gather(tipo, Total, 
         ic1_parto_unico_espontaneo:ic8_causas_obstetricas_indirectas) %>% 
  mutate(tipo=str_remove_all(tipo, "ic._")) %>% 
  group_by(unidad_medica, año, tipo) %>% 
  summarise(Total=sum(Total, na.rm = T))


causas_obs_juntas <- total_causas_obs %>% 
  left_join(total_causas_obs_20,
            by=c("unidad_medica", "año", "tipo")) %>% 
  gather(gr_edad, Total, Total.x:Total.y) %>% 
  mutate(gr_edad=case_when(
    gr_edad=="Total.x" ~ "Todas edades", 
    T ~ "Menores 20"
  ))

#####
#APEO
años_apeo <- años <- c(2017:2024)

apeo <- list()
for(i in 1:length(años)){
  
  apeo[[i]] <- read_excel("../datos/APEO.xlsx", 
                                   sheet = paste0(años_apeo[i])
  ) %>% 
    clean_names() %>% 
    mutate(año=años_apeo[i])
  
}
total_apeo <- bind_rows(apeo) %>% 
  relocate(año, .after = "unidad_medica") %>% 
  gather(tipo, Total, 
         hormonal_oral_parto:implante_subdermico_doble_varilla_parto) %>% 
  mutate(tipo=str_remove_all(tipo, "ic._"), 
         atencion=case_when(
           grepl("parto", tipo) ~ "Parto", 
           T ~ "Aborto"
         ), 
         tipo=str_remove(tipo, "_parto|_aborto")
         
         ) %>% 
  group_by(unidad_medica, año, tipo, atencion) %>% 
  summarise(Total=sum(Total, na.rm = T))


#APEO menores de 20
años_apeo <- años <- c(2017:2024)

apeo_20 <- list()
for(i in 1:length(años)){
  
  apeo_20[[i]] <- read_excel("../datos/APEO_20.xlsx", 
                          sheet = paste0(años_apeo[i])
  ) %>% 
    clean_names() %>% 
    mutate(año=años_apeo[i])
  
}
total_apeo_20 <- bind_rows(apeo_20) %>% 
  relocate(año, .after = "unidad_medica") %>% 
  gather(tipo, Total, 
         hormonal_oral_parto:implante_subdermico_doble_varilla_parto) %>% 
  mutate(tipo=str_remove_all(tipo, "ic._"), 
         atencion=case_when(
           grepl("parto", tipo) ~ "Parto", 
           T ~ "Aborto"
         ), 
         tipo=str_remove(tipo, "_parto|_aborto")
         
  ) %>% 
  group_by(unidad_medica, año, tipo, atencion) %>% 
  summarise(Total=sum(Total, na.rm = T))%>% ungroup()



apeo_juntas <- total_apeo %>% ungroup() %>% 
  left_join(total_apeo_20,
            by=c("unidad_medica", "año", "tipo", "atencion")) %>% 
  gather(gr_edad, Total, Total.x:Total.y) %>% 
  mutate(gr_edad=case_when(
    gr_edad=="Total.x" ~ "Todas edades", 
    T ~ "Menores 20"
  )) %>% 
  filter(tipo!="dispositivo_intrauterino")

#####
#Secretariado
#

secretariado <- read_excel("../datos/Estatal-Delitos-2015-2024_dic2024.xlsx") %>% 
  clean_names() %>% 
  filter(subtipo_de_delito %in% c("Homicidio doloso", "Feminicidio", 
                                  "Abuso sexual", "Acoso sexual", 
                                  "Violación simple", "Violación equiparada", 
                                  "Violencia familiar" , 
                                  "Violencia de género en todas sus modalidades distinta a la violencia familiar"
                                  )) %>% 
  gather(mes, total, enero:diciembre) %>% 
    mutate(subtipo_de_delito=case_when(
      grepl("Violación", subtipo_de_delito) ~ "Violación", 
      grepl("género", subtipo_de_delito) ~ "Violencia de género", 
      T ~ subtipo_de_delito
    ), 
    fecha_inicio=ymd(paste0(ano, "-",mes, "-01"))
    ) %>% 
    group_by(fecha_inicio, clave_ent, 
             entidad, subtipo_de_delito) %>% 
    summarise(total=sum(total)) %>% ungroup()


#####
#creamos rds

data_total <- list(
  conapo=conapo, 
  muertes_maternas=muertes_maternas, 
  total_causas_obs=causas_obs_juntas,
  apeo=apeo_juntas, 
  secretariado=secretariado
)


write_rds(data_total, "data_ipas.rds")
