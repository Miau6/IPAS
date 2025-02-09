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
  mutate(grupo_edad = cut(edad, breaks = seq(0, 110, by = 5), include.lowest = TRUE, right = FALSE, 
                          labels = paste(seq(0, 105, by = 5), seq(4, 110, by = 5), sep = "-")))
conapo$grupo_edad <- factor(conapo$grupo_edad, levels = c("Todas las edades", levels(conapo$grupo_edad)))


#####
#nacimientos

nacimientos <- list()
for (i in 2015:2024) {
  nacimientos[[i]] <- read_excel("../datos/nacimientos.xlsx", 
                            sheet = paste0(i)) %>% 
    clean_names() %>% 
    mutate(año=i)
}

total_nacimientos <- bind_rows(nacimientos) %>% 
  mutate(entidad_um_parto=substr(entidad_um_parto,4,nchar(entidad_um_parto))) %>% 
  filter(!grepl("otal|APLI", entidad_um_parto))

total_nacimientos <- total_nacimientos %>% 
  clean_names() %>% 
  mutate(across(where(is.character), ~ str_to_title(.))) %>% 
  rename(total=nacimientos,
         entidad=entidad_um_parto) %>% 
  mutate(entidad = case_when(
    str_detect(entidad, "Ciudad De México") ~ "Ciudad de México",
    str_detect(entidad, "Veracruz") ~ "Veracruz",
    str_detect(entidad, "Coahuila") ~ "Coahuila",
    str_detect(entidad, "Michoac.n") ~ "Michoacán",
    TRUE ~ entidad
  ))


#####
#muertes maternas
muertes_maternas <- read_excel("../datos/muertes_maternas.xlsx") %>% 
  clean_names() %>% 
  gather(año, Total, defunciones_2005:defunciones_2015
         ) %>% 
  mutate(año=as.integer(str_remove_all(año, "defunciones_"))) %>%
  filter(año>=2015)


muertes_maternas_20 <- read_excel("../datos/muertes_maternas.xlsx", 
                                  sheet = 2) %>% 
  clean_names() %>% 
  gather(año, Total, defunciones_2005:defunciones_2023
  ) %>% 
  mutate(año=as.integer(str_remove_all(año, "defunciones_"))) %>%
  filter(año>=2015) %>% 
  replace_na(list(Total=0))


total_muertes_maternas <- muertes_maternas %>% 
  left_join(muertes_maternas_20,
            by=c("entidad_de_defuncion", "año")) %>% 
  gather(gr_edad, Total, Total.x:Total.y) %>% 
  mutate(gr_edad=case_when(
    gr_edad=="Total.x" ~ "Todas edades", 
    T ~ "Menores 20"
  ))

total_muertes_maternas <- total_muertes_maternas %>% 
  clean_names() %>%
  mutate(across(where(is.character), ~ str_to_title(.)),
         entidad_de_defuncion = str_remove(entidad_de_defuncion, "[0-9]+"),
         entidad_de_defuncion = str_trim(entidad_de_defuncion),
         entidad_de_defuncion=case_when(
           str_detect(entidad_de_defuncion, "Veracruz") ~ "Veracruz",
           str_detect(entidad_de_defuncion, "Ciudad De Mexico") ~ "Ciudad de México",
           str_detect(entidad_de_defuncion, "Michoacan") ~ "Michoacán",
           str_detect(entidad_de_defuncion, "Coahuila") ~ "Coahuila",
           str_detect(entidad_de_defuncion, "Mexico") ~ "México",
           T~entidad_de_defuncion)) %>% 
  rename(entidad = entidad_de_defuncion)

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
  summarise(Total=sum(Total, na.rm = T)) %>% ungroup()


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


causas_obs_juntas <- causas_obs_juntas %>% clean_names() %>% 
  group_by(unidad_medica, ano, tipo, gr_edad) %>%
  summarise(total = sum(total, na.rm = TRUE)) %>%
  mutate(unidad_medica = "Nacional") %>% ungroup() %>% 
  bind_rows(causas_obs_juntas %>% clean_names() %>% 
              ungroup()) %>% 
  select(unidad_medica, ano, tipo, gr_edad, total) %>% 
  mutate(across(where(is.character), ~ str_to_title(.)),
         unidad_medica=str_to_title(unidad_medica),
         unidad_medica=case_when(
           unidad_medica %in% c("Ciudad De México", "Distrito Federal")~"Ciudad de México",
           T~unidad_medica),
         tipo = gsub("_", " ", tipo),
         gr_edad=case_when(
           gr_edad=="Todas Edades"~"Todas las edades",
           gr_edad=="Menores 20"~"Menores de 20",
           T~gr_edad)) %>% 
  mutate(tipo=str_to_sentence(tipo))

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



apeo_juntas <- apeo_juntas %>% clean_names() %>% 
  filter(ano %in% c (2017,2018,2019,2020,2021,2022,2023)) %>% 
  
  group_by(unidad_medica,ano, tipo, gr_edad) %>%
  summarise(total = sum(total, na.rm = TRUE)) %>%
  mutate(unidad_medica = "Nacional") %>% 
  bind_rows(apeo_juntas %>% clean_names()) %>% 
  mutate(across(where(is.character), ~ str_to_title(.)),
         unidad_medica=str_to_title(unidad_medica),
         unidad_medica=case_when(
           unidad_medica %in% c("Ciudad De México", "Distrito Federal")~"Ciudad de México",
           T~unidad_medica),
         tipo = gsub("_", " ", tipo),
         gr_edad=case_when(
           gr_edad=="Todas Edades"~"Todas las edades",
           gr_edad=="Menores 20"~"Menores de 20",
           T~gr_edad
         ),
         tipo=str_to_sentence(tipo)) %>%
  select(unidad_medica, ano, tipo, gr_edad, total)

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


secretariado <- secretariado %>% clean_names() %>% 
  mutate(across(where(is.character), ~ str_to_title(.)),
         entidad = case_when(
           str_detect(entidad, "Ciudad De México") ~ "Ciudad de México",
           str_detect(entidad, "Veracruz") ~ "Veracruz",
           str_detect(entidad, "Coahuila") ~ "Coahuila",
           str_detect(entidad, "Michoac.n") ~ "Michoacán",
           TRUE ~ entidad),
         fecha_inicio = format(as.Date(fecha_inicio, format = "%Y/%m/%d"), "%Y"),
         fecha_inicio= as.integer(fecha_inicio),
         subtipo_de_delito=str_to_sentence(subtipo_de_delito)) %>% 
  rename(fecha=fecha_inicio,
         delito=subtipo_de_delito) %>% 
  select(-clave_ent)

#####
#enadid 

enadid_18 <- foreign::read.dbf("../datos/TMujer1_18.dbf" )

enadid_18 <- enadid_18 %>% #clean_names() %>% 
  select(ENT, P5_2_1, P5_9, P5_23,P7_1, P7_2, P7_9,
         P8_38, P8_39_01:P8_39_99,
         TM_USA,
         
         FAC_PER
         ) %>% mutate(año=2018)



enadid_24 <- foreign::read.dbf("../datos/TMujer1_18.dbf" )


enadid_24 <- enadid_24 %>% #clean_names() %>% 
  select(ENT, P5_2_1, P5_9, P5_23,P7_1, P7_2, P7_9,
         P8_38, P8_39_01:P8_39_99,
         TM_USA,
         
         FAC_PER
  ) %>% mutate(año=2024)

total_enadid <- bind_rows(enadid_18, enadid_24)

names(total_enadid) <- c("entidad", "edad", "hijos_nacidos", "abortos", 
                         "embarazo_actualmente", "deseo_embarazo", 
                         "motivo_no_hijos", "edad_primera_relacion_sexual", 
                         "no_uso", "pastillas_anticonceptivas", 
                         "inyecciones_anticonceptivas", "implante_subdermico",
                         "parche_anticonceptivo","DIU", 
                         "condon_masculino", "condon_femenino",
                         "espumas_anticonceptivas", "abstinencia periódica",
                         "coito_interrumpido","dia_siguiente",
                         "otro_metodo", "no_responde", 
                         "actual_metodo", "factor", "año"
                         
                         )












#####
#creamos rds

data_total <- list(
  conapo=conapo, 
  muertes_maternas=total_muertes_maternas, 
  total_causas_obs=causas_obs_juntas,
  apeo=apeo_juntas, 
  secretariado=secretariado, 
  nacimientos=total_nacimientos, 
  enadid=total_enadid
)


write_rds(data_total, "data_ipas.rds")




#####
#código de Nancy
# df_egresos_obs <- df$total_causas_obs %>% 
#   clean_names() %>% 
#   group_by(unidad_medica, ano, tipo, gr_edad) %>%
#   summarise(total = sum(total, na.rm = TRUE)) %>%
#   mutate(unidad_medica = "Nacional") %>% 
#   bind_rows(df$total_causas_obs %>% clean_names()) %>% 
#   select(unidad_medica, ano, tipo, gr_edad, total) %>% 
#   mutate(across(where(is.character), ~ str_to_title(.)),
#          unidad_medica=str_to_title(unidad_medica),
#          unidad_medica=case_when(
#            unidad_medica %in% c("Ciudad De México", "Distrito Federal")~"Ciudad de México",
#            T~unidad_medica),
#          tipo = gsub("_", " ", tipo),
#          gr_edad=case_when(
#            gr_edad=="Todas Edades"~"Todas las edades",
#            gr_edad=="Menores 20"~"Menores de 20",
#            T~gr_edad)) %>% 
#   mutate(tipo=str_to_sentence(tipo))

# Muertes maternas -------------------------------------------------------------

# df_muertes<-df$muertes_maternas %>% 
#   clean_names() %>%
#   mutate(across(where(is.character), ~ str_to_title(.)),
#          entidad_de_defuncion = str_remove(entidad_de_defuncion, "[0-9]+"),
#          entidad_de_defuncion = str_trim(entidad_de_defuncion),
#          entidad_de_defuncion=case_when(
#            str_detect(entidad_de_defuncion, "Veracruz") ~ "Veracruz",
#            str_detect(entidad_de_defuncion, "Ciudad De Mexico") ~ "Ciudad de México",
#            str_detect(entidad_de_defuncion, "Michoacan") ~ "Michoacán",
#            str_detect(entidad_de_defuncion, "Coahuila") ~ "Coahuila",
#            str_detect(entidad_de_defuncion, "Mexico") ~ "México",
#            T~entidad_de_defuncion)) %>% 
#   rename(entidad = entidad_de_defuncion)

# ----------------------------------------------------------------------------

# df_apeo <- df$apeo %>% 
#   clean_names() %>%
#   filter(ano %in% c (2017,2018,2019,2020,2021,2022,2023)) %>% 
#   
#   group_by(unidad_medica,ano, tipo, gr_edad) %>%
#   summarise(total = sum(total, na.rm = TRUE)) %>%
#   mutate(unidad_medica = "Nacional") %>% 
#   bind_rows(df$apeo %>% clean_names()) %>% 
#   mutate(across(where(is.character), ~ str_to_title(.)),
#          unidad_medica=str_to_title(unidad_medica),
#          unidad_medica=case_when(
#            unidad_medica %in% c("Ciudad De México", "Distrito Federal")~"Ciudad de México",
#            T~unidad_medica),
#          tipo = gsub("_", " ", tipo),
#          gr_edad=case_when(
#            gr_edad=="Todas Edades"~"Todas las edades",
#            gr_edad=="Menores 20"~"Menores de 20",
#            T~gr_edad
#          ),
#          tipo=str_to_sentence(tipo)) %>%
#   select(unidad_medica, ano, tipo, gr_edad, total)