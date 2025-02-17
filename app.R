# Librerías para Shiny y personalización de la interfaz
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyjs)
library(shinythemes)
library(shinycssloaders)
library(shinyFeedback)
library(shiny.router)
library(shinyBS)
library(dashboardthemes)
library(shinybusy)
library(slickR)

# Manipulación y limpieza de datos
library(tidyverse)
library(dplyr)
library(readr)
library(readxl)
library(janitor)
library(tibble)
library(data.table)
library(tibbletime)

# Visualización de datos
library(ggplot2)
library(plotly)
library(scales)
library(treemap)
library(treemapify)


# Tipografía y personalización visual
library(extrafont)
library(showtext)

# Manejo de datos temporales
library(lubridate)

# Generación de reportes
library(rmarkdown)
library(latex2exp)

# Tablas interactivas y estilizadas
library(DT)
library(gt)
library(htmltools)

# #-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-


# setwd("~/IPAS/data/ipas/calidad_atencion")
sysfonts::font_add_google(name = "Montserrat", family = "Montserrat")
showtext::showtext_auto()


library(ggplot2)

theme_minimal() +
  theme(
    text = element_text(family = "Montserrat"),
    legend.position = "bottom",
    title = element_text(size = 16, face = "bold", family = "Montserrat"),
    plot.subtitle = element_text(size = 13, face = "plain"),
    plot.caption = element_text(size = 12, face = "plain"),
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.grid = element_blank(),  
    axis.text.x = element_text(size = 13, color = "black", angle = 90, hjust = 1),
    axis.text.y = element_text(size = 13, color = "white"),
    axis.title = element_text(size = 16, color = "black"),
    legend.title = element_text(size = 20, color = "black"),
    legend.text = element_text(color = "black", size = 13),
    plot.margin = margin(0, 0, 0, 0, unit = "pt") 
  ) -> theme_ipas



theme_gg <- function(plot) {
  ggplotly(plot) %>%
    layout(
      margin = list(l = 0, r = 0),
      legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -1.5),
      title = list(y = 0.95))
}




# Cargar datos - - - - - - - - - - - - - - - - - - - - - - -  - - - - - - - - - 
df <- readRDS("data/data_ipas.rds")

df_demografia <- df$conapo

df_egresos_obs <- df$total_causas_obs %>% 
  clean_names() 
# %>% 
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
# 
# Muertes maternas -------------------------------------------------------------

df_muertes<-df$muertes_maternas %>% 
  clean_names() 
# %>%
#   mutate(across(where(is.character), ~ str_to_title(.)),
#          entidad_de_defuncion = str_remove(entidad_de_defuncion, "[0-9]+"),
#          entidad_de_defuncion = str_trim(entidad_de_defuncion),
#          entidad_de_defuncion=case_when(
#     str_detect(entidad_de_defuncion, "Veracruz") ~ "Veracruz",
#     str_detect(entidad_de_defuncion, "Ciudad De Mexico") ~ "Ciudad de México",
#     str_detect(entidad_de_defuncion, "Michoacan") ~ "Michoacán",
#     str_detect(entidad_de_defuncion, "Coahuila") ~ "Coahuila",
#     str_detect(entidad_de_defuncion, "Mexico") ~ "México",
#     T~entidad_de_defuncion)) %>% 
#   rename(entidad = entidad_de_defuncion)

# ----------------------------------------------------------------------------

df_apeo <- df$apeo %>% 
  clean_names() 
# %>%
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

df_nacimiento<-df$nacimientos #%>% 
  # clean_names() %>% 
  # mutate(across(where(is.character), ~ str_to_title(.))) %>% 
  # rename(total=nacimientos,
  #        entidad=entidad_um_parto) %>% 
  # mutate(entidad = case_when(
  #   str_detect(entidad, "Ciudad De México") ~ "Ciudad de México",
  #   str_detect(entidad, "Veracruz") ~ "Veracruz",
  #   str_detect(entidad, "Coahuila") ~ "Coahuila",
  #   str_detect(entidad, "Michoac.n") ~ "Michoacán",
  #   TRUE ~ entidad
  # ))


# violencia--------------------------------------------------------------------
df_violencia <- df$secretariado #%>% 
  # clean_names() %>% 
  # mutate(across(where(is.character), ~ str_to_title(.)),
  #        entidad = case_when(
  #          str_detect(entidad, "Ciudad De México") ~ "Ciudad de México",
  #          str_detect(entidad, "Veracruz") ~ "Veracruz",
  #          str_detect(entidad, "Coahuila") ~ "Coahuila",
  #          str_detect(entidad, "Michoac.n") ~ "Michoacán",
  #          TRUE ~ entidad),
  #        fecha_inicio = format(as.Date(fecha_inicio, format = "%Y/%m/%d"), "%Y"),
  #        fecha_inicio= as.integer(fecha_inicio),
  #        subtipo_de_delito=str_to_sentence(subtipo_de_delito)) %>% 
  # rename(fecha=fecha_inicio,
  #        delito=subtipo_de_delito) %>% 
  # select(-clave_ent)

# enadid-----------------------------------------------------------------------
enadid <- df$enadid

enadid <- enadid %>% 
  filter(edad>=10)

# enadid <- enadid %>% clean_names() %>% 
#   mutate(cve_geo=as.integer(entidad)) %>% 
#   left_join(conapo %>% 
#               select(cve_geo, entidad))

# #-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-


# UI - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
ui <- shinyUI(
# Diseño CSS y HTML -----------------------------------------------------------  
tagList(
  includeCSS("./www/style.css"),
  fluidPage(
    class = 'p-2',
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
              tags$style(HTML("
              
.container-fluid {
        padding-left: 0px !important;
        padding-right: 0px !important;
      }
      .row {
        margin-left: 0px !important;
        margin-right: 0px !important;
      }
      .col-sm-9 {  # Ajusta el mainPanel para ocupar más espacio
        padding-left: 5px !important;
        padding-right: 5px !important;
      }
              
.custom-card {
        text-align: center;
        background-color: #849abf;
        border-radius: 15px;
        padding: 20px;
        box-shadow: 2px 2px 10px rgba(0, 0, 0, 0.1);
        margin-bottom: 20px;
      }
      .custom-button {
        background-color: #1a1866 !important;
        color: #FFFFFF !important;
        border-radius: 10px;
        font-size: 18px;
        padding: 15px;
        width: 100%;
      }
      .custom-button:hover {
        background-color: #5032e6 !important;
        color: #FFFFFF !important;
      }
      .custom-text {
        color: #333333;
        font-weight: bold;
        margin-top: 10px;
      }

              .navbar {
            background-color: #f15a29 !important;
            border: none;
            margin: 0 !important;
            padding: 0 !important;
            width: 100%;
            }
            .navbar .navbar-nav {float: right;,
             background-color: #f15a29 !important;}

            .navbar {
              background-color: #f15a29 !important;
              border: none;
            }
            
            .navbar-nav > li > a, .navbar-nav-right > li > a {
              color: white !important;
              font-size: 14px;
              padding: 15px 20px;
            }
            
            .navbar-nav > li > a:hover, .navbar-nav-right > li > a:hover {
              background-color: #d9534f !important;
              color: white !important;
            }
            
            .navbar-nav > li.active > a, .navbar-nav-right > li.active > a {
              background-color: #f15a29 !important;
              color: white !important;
            }
            
            .navbar-brand {
              color: white !important;
              font-weight: bold;
              font-size: 24px;
            }
              
              ")),
              tags$script(HTML("
              $(document).on('click', '#toggle_usuarias_sidebar', function() {
                var sidebar = $('#usuarias_sidebar');
                var mainPanel = $('.mainPanelClass'); // Clase personalizada para el mainPanel
                
                if (sidebar.is(':visible')) {
                  sidebar.hide(); // Ocultar el sidebar
                  mainPanel.removeClass('main-panel-collapsed').addClass('main-panel-expanded'); // Expandir el mainPanel
                } else {
                  sidebar.show(); // Mostrar el sidebar
                  mainPanel.removeClass('main-panel-expanded').addClass('main-panel-collapsed'); // Contraer el mainPanel
                }
              });
              "))),
    
    add_busy_spinner(onstart = FALSE, spin = "fading-circle", color = "#f15a29"),
    navbarPage(id = "navbar",
      tags$img(src = "Logo_IPAS.png", height = "40px", style = "margin-right:10px; background-color:#f15a29"),
      header = busy_start_up(
        loader = spin_epic("flower", color = "#f15a29"),
        text = "Cargando",
        timeout = 1500,
        color = "#f15a29",
        background = "white"
      ),
      useShinydashboard(),
# Pestañas --------------------------------------------------------------------  

# Inicio ----------------------------------------------------------------------
  tabPanel(
    title = "Inicio", 
    br(),
    fluidRow(
      column(4, 
             div(class = "custom-card",
                 actionButton("btn_demografia", "", 
                              icon = icon("users-line", "fa-3x"), 
                              class = "custom-button"),
                 h4("Ir a Demografía", class = "custom-text")
             )),
      column(4, 
            div(class = "custom-card",
                actionButton("btn_nacimiento", "", 
                             icon = icon("baby-carriage", "fa-3x"), 
                             class = "custom-button"),
                h4("Ir a Nacimientos", class = "custom-text")
            )),
      column(4, 
             div(class = "custom-card",
                 actionButton("btn_egresos", "", 
                              icon = icon("chart-area", "fa-3x"), 
                              class = "custom-button"),
                 h4("Ir a Egresos Obstétricos", class = "custom-text")
             )),
      
      column(4, 
             div(class = "custom-card",
          actionButton("btn_muertes", "", 
                       icon = icon("chart-column", "fa-3x"), 
                       class = "custom-button"),
          h4("Ir a Muertes maternas", class = "custom-text")
      )),
      
      column(4, 
             div(class = "custom-card",
                 actionButton("btn_apeo", "", 
                              icon = icon("hospital", "fa-3x"), 
                              class = "custom-button"),
                 h4("Ir a APEO", class = "custom-text")
             )),
      
      column(4, 
             div(class = "custom-card",
                 actionButton("btn_violencia", "", 
                              icon = icon("magnifying-glass-chart", "fa-3x"), 
                              class = "custom-button"),
                 h4("Ir a violencia", class = "custom-text")
             )), 
      column(4, 
             div(class = "custom-card",
                 actionButton("btn_enadid", "", 
                              icon = icon("house", "fa-3x"), 
                              class = "custom-button"),
                 h4("Ir a Anticoncepción", class = "custom-text")
             ))
      )),
# Demografía ----------------------------------------------------------------------

  tabPanel("Demografía",  br(),
  tabsetPanel(
  tabPanel("Por entidad", br(),   
    sidebarPanel(width = 3,
      sliderInput("demografia_ano", "Selecciona un año:", 
                  min = min(df_demografia$ano), max =max(df_demografia$ano), 
                  value = 2024, step = 1),
      
      selectInput("demografia_grupo_edad", "Selecciona un rango de edad:", 
                  choices = unique(df_demografia$grupo_edad), multiple = T, selected = "Todos las edades"),
      
      selectInput("demografia_sexo", "Selecciona el sexo:", 
                  choices = unique(df_demografia$sexo), multiple = TRUE, selected = c("Hombres", "Mujeres")),
      
      selectInput("demografia_entidad", "Selecciona una entidad:", 
                  choices = unique(df_demografia$entidad), 
                  multiple = TRUE, 
                  selected = setdiff(unique(df_demografia$entidad), "República Mexicana"),
                  selectize = TRUE)
      ),
    mainPanel(width = 9,
      plotlyOutput("gr_demografia", width = "auto", height = "auto"),
              br(),
              DT::dataTableOutput("tabla_demografia")
    )),
        
  tabPanel("Por grupo de edad", br(),
           
           sidebarPanel(width = 3,br(),
             sliderInput("demografia_ano_edad", "Selecciona un año:", 
                         min = min(df_demografia$ano), max = max(df_demografia$ano), 
                         value = max(df_demografia$ano), step = 1),
             
             selectInput("demografia_entidad_edad", "Selecciona una entidad:", 
                         choices = unique(df_demografia$entidad), 
                         multiple = F, 
                         selected = "República Mexicana")
             ),
           mainPanel(plotlyOutput("gr_demografia_edad"),
                     br(),
                     DT::dataTableOutput("tabla_demografia_edad")  # Tabla interactiva
           )))),

#  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  


  tabPanel("Nacimientos", br(),
      sidebarPanel(width = 3, br(),
                   selectInput("nacimiento_ano", "Selecciona un año:", 
                               choices = unique(df_nacimiento$ano), 
                               selected = max(df_nacimiento$ano), multiple = F),
                   selectInput("nacimiento_entidad", "Selecciona una entidad:",
                               choices = unique(df_nacimiento$entidad),
                               multiple = TRUE, 
                               selected = unique(df_nacimiento$entidad))
                   
      ),
      
      mainPanel(
        plotlyOutput("gr_nacimiento"), br(),br(),
        DT::dataTableOutput("tabla_nacimiento")  # Agrega la tabla aquí
        
      )),

#  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  

    tabPanel("Egresos Obstétricos", br(),
             sidebarPanel(width = 3, br(),
               selectInput("egresos_ano", "Selecciona un año:", 
                           choices = unique(df_egresos_obs$ano), 
                           selected = 2024),
               selectInput("egresos_unidad_medica", "Selecciona una entidad:",
                           choices = unique(df_egresos_obs$unidad_medica),
                           multiple = F,
                           selected = c("Nacional")),
               
               selectInput("egresos_grupo_edad", "Selecciona un grupo de edad:", 
                           choices = unique(df_egresos_obs$gr_edad), 
                           selected = "Todas las edades"),
               
               selectInput("egresos_tipo", "Selecciona una causa:", 
                           choices = unique(df_egresos_obs$tipo), 
                           multiple = T, selected = df_egresos_obs$tipo)
             ),
             mainPanel(
               plotlyOutput("grafico_egresos"), br(),br(),
               DT::dataTableOutput("tabla_egresos")  # Agrega la tabla aquí
               
             )),

#  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  

tabPanel("Muertes maternas", br(),
         sidebarPanel(width = 3, br(),
                      selectInput("muertes_ano", "Selecciona un año:", 
                                  choices = unique(df_muertes$ano), 
                                  selected = max(df_muertes$ano), multiple = F),
                      selectInput("muertes_entidad", "Selecciona una entidad:",
                                  choices = unique(df_muertes$entidad),
                                  multiple = TRUE, 
                                  selected = unique(df_muertes$entidad)),
                      selectInput("muertes_edad", "Selecciona un grupo de edad:", 
                                  choices = unique(df_muertes$gr_edad), 
                                  selected = c("Todas Edades", "Menores 20"), multiple = T)
                      
         ),
         
         mainPanel(
           plotlyOutput("gr_muertes"), br(),br(),
           DT::dataTableOutput("tabla_muertes")  # Agrega la tabla aquí

         )),

#  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  

tabPanel("APEO", br(),
         sidebarPanel(width = 3, br(),
                      selectInput("apeo_ano", "Selecciona un año:", 
                                  choices = unique(df_apeo$ano), 
                                  selected = 2023),
                      selectInput("apeo_unidad_medica", "Selecciona una entidad:",
                                  choices = unique(df_apeo$unidad_medica),
                                  multiple = F,
                                  selected = c("Nacional")),
                      
                      selectInput("apeo_grupo_edad", "Selecciona un grupo de edad:", 
                                  choices = unique(df_apeo$gr_edad), 
                                  selected = "Todas las edades"),
                      
                      selectInput("apeo_tipo", "Selecciona una causa:", 
                                  choices = unique(df_apeo$tipo), 
                                  multiple = T, selected = df_apeo$tipo)
         ),
         mainPanel(
           plotlyOutput("grafico_apeo"), br(),br(),
           DT::dataTableOutput("tabla_apeo")  # Agrega la tabla aquí
           
         )),

tabPanel("Violencias", br(),
         sidebarPanel(width = 3,
                      sliderInput("violencia_fecha", "Selecciona un año:", 
                                  min = min(df_violencia$fecha), max =max(df_violencia$fecha), 
                                  value = 2024, step = 1),
                      
                      selectInput("violencia_delito", "Selecciona el o los delitos:", 
                                  choices = unique(df_violencia$delito), multiple = TRUE,
                                  selected = c("Violencia familiar", "Abuso sexual", "Violación")),
                      
                      selectInput("violencia_entidad", "Selecciona una entidad:", 
                                  choices = unique(df_violencia$entidad), 
                                  multiple = TRUE, 
                                  selected = setdiff(unique(df_violencia$entidad), "Nacional"),
                                  selectize = TRUE)
         ),
         mainPanel(width = 9,
                   plotlyOutput("gr_violencia", width = "auto", height = "auto"),
                   br(),
                   DT::dataTableOutput("tabla_violencia")
         )), 
tabPanel("Anticoncepción", br(),
         tabsetPanel(
          
         
         # ), 
         tabPanel("Anticonceptivos", 
                  sidebarPanel(width = 3,
                               selectInput("enadid_año2", "Selecciona un año:", 
                                           choices = c(2018, 2024), 
                                           selected = 2024, multiple = F),
                               
                               selectInput("enadid_entidad2", "Selecciona una entidad:",
                                           choices = sort(unique(enadid$nom_ent)),
                                           multiple = TRUE,
                                           selected = unique(enadid$nom_ent),
                                           selectize = TRUE),
                               selectInput("enadid_edad2", "Selecciona grupos de edad:",
                                           choices = sort(unique(enadid$grupo_edad)),
                                           multiple = TRUE,
                                           selected = unique(enadid$grupo_edad),
                                           selectize = TRUE)
                  ),
                  mainPanel(width = 9,
                           plotlyOutput("gr_uso_anti_primera_vez", width = "auto", height = "auto"),
                            br(),
                            plotlyOutput("gr_uso_anticonceptivo", width = "auto", height = "auto"),
                           br(), 
                           # h3("Tabla de indicador de uso de anticonceptivo en su primera relación sexual"),
                           DT::dataTableOutput("tabla_uso_anti_primera_vez"), br(), 
                           # h3("Tabla de indicador de uso actual de anticonceptivos"),
                           DT::dataTableOutput("tabla_uso_anticonceptivo")
                  )
                  ),
         tabPanel("Indicadores",
                  sidebarPanel(width = 3,
                               selectInput("enadid_año", "Selecciona un año:", 
                                           choices = c(2018, 2024), 
                                           selected = 2024, multiple = F),
                               
                               # selectInput("violencia_delito", "Selecciona el o los delitos:", 
                               #             choices = unique(df_violencia$delito), multiple = TRUE,
                               #             selected = c("Violencia familiar", "Abuso sexual", "Violación")),
                               # 
                               selectInput("enadid_entidad", "Selecciona una entidad:",
                                           choices = sort(unique(enadid$nom_ent)),
                                           multiple = TRUE,
                                           selected = unique(enadid$nom_ent),
                                           selectize = TRUE),
                               selectInput("enadid_edad", "Selecciona grupos de edad:",
                                           choices = sort(unique(enadid$grupo_edad)),
                                           multiple = TRUE,
                                           selected = unique(enadid$grupo_edad),
                                           selectize = TRUE)
                  ),
                  mainPanel(width = 9,
                            plotlyOutput("gr_tasa_fecundidad", width = "auto", height = "auto"),
                            br(),
                            plotlyOutput("gr_prom_abortos", width = "auto", height = "auto"),
                            br(),
                            # h3("Tabla de indicador de fecundidad de los últimos 6 años"),
                            DT::dataTableOutput("tabla_fecundidad"), br(), 
                            # h3("Tabla de indicador de abortos"),
                            DT::dataTableOutput("tabla_abortos")
                            # plotlyOutput("gr_uso_anti_primera_vez", width = "auto", height = "auto"),
                            # br(),
                            # plotlyOutput("gr_uso_anticonceptivo", width = "auto", height = "auto")
                  )), 
         tabPanel("Comparación temporal",
                  sidebarPanel(width = 3,
                               # selectInput("enadid_año2", "Selecciona un año:",
                               #             choices = c(2018, 2024),
                               #             selected = 2024, multiple = F),

                               selectInput("enadid_entidad3", "Selecciona una entidad:",
                                           choices = sort(unique(enadid$nom_ent)),
                                           multiple = F,
                                           selected = unique(enadid$nom_ent),
                                           selectize = TRUE),
                               selectInput("enadid_edad3", "Selecciona grupos de edad:",
                                           choices = sort(unique(enadid$grupo_edad)),
                                           multiple = TRUE,
                                           selected = unique(enadid$grupo_edad),
                                           selectize = TRUE)
                  ),
                  mainPanel(width = 9,
                            plotlyOutput("gr_uso_anti_primera_vez_tiempo", width = "auto", height = "auto"),
                            br(),
                            plotlyOutput("gr_uso_anticonceptivo_tiempo", width = "auto", height = "auto"),
                            br(),
                            plotlyOutput("gr_tasa_fecundidad_tiempo", width = "auto", height = "auto"),
                            br(),
                            plotlyOutput("gr_prom_abortos_tiempo", width = "auto", height = "auto")
                  )
         )
         
                  
                  )
         
         )



  ))))

# #-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-


# Server - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -



server <- function(input, output, session) {
  
  observeEvent(input$btn_demografia, {
    updateNavbarPage(session, "navbar", selected = "Demografía")  # ✅ Usa exactamente el mismo nombre
  })
  
  observeEvent(input$btn_egresos, {
    updateNavbarPage(session, "navbar", selected = "Egresos Obstétricos")  # ✅ Usa exactamente el mismo nombre
  })
  
  observeEvent(input$btn_muertes, {
    updateNavbarPage(session, "navbar", selected = "Muertes maternas")  # ✅ Usa exactamente el mismo nombre
  })
  
  observeEvent(input$btn_apeo, {
    updateNavbarPage(session, "navbar", selected = "APEO")  # ✅ Usa exactamente el mismo nombre
  })
  
  
  observeEvent(input$btn_nacimiento, {
    updateNavbarPage(session, "navbar", selected = "Nacimientos")  # ✅ Usa exactamente el mismo nombre
  })
  
  observeEvent(input$btn_violencia, {
    updateNavbarPage(session, "navbar", selected = "Violencias")  # ✅ Usa exactamente el mismo nombre
  })
  
  observeEvent(input$btn_enadid, {
    updateNavbarPage(session, "navbar", selected = "Anticoncepción")  # ✅ Usa exactamente el mismo nombre
  })
  
  #------------------------------------------------------------------------------

demografia_reactive <- reactive({
  df_demografia %>%
    filter(
      if (is.null(input$demografia_grupo) || input$demografia_grupo == "Todos las edades") TRUE else entidad %in% input$demografia_grupo,
      entidad %in% input$demografia_entidad,
      ano >= input$demografia_ano,
      sexo %in% input$demografia_sexo
    ) 
})

  # Gráfico por entidad
  output$gr_demografia <- renderPlotly({
    
    demografia_reactive() %>% 
      group_by(entidad) %>% 
      mutate(total_entidad = sum(poblacion)) %>%  # Sumar población total por entidad
      group_by(entidad, sexo) %>% 
      summarise(
        poblacion = sum(poblacion), 
        total_entidad = first(total_entidad), 
        .groups = 'drop'
      ) %>% 
      mutate(
        porcentaje = (poblacion / total_entidad) * 100,  # Calcular porcentaje dentro de cada entidad
        porcentaje_texto = paste0(round(porcentaje, 1), "%"),  # Formatear texto del porcentaje
        tooltip_text = paste0(
          "<b>Entidad:</b> ", entidad, "<br>",
          "<b>Sexo:</b> ", sexo, "<br>",
          "<b>Población:</b> ", scales::comma(abs(poblacion)), "<br>",
          "<b>Proporción en la entidad:</b> ", porcentaje_texto
        )) %>% 
      ggplot(aes(x = reorder(entidad, -poblacion), y = poblacion, fill = sexo, text = tooltip_text)) +
      geom_bar(stat = "identity") +
      labs(
        x = "", y = "", fill = ""
      ) +
      scale_y_continuous(labels = scales::comma) +
      theme_minimal() +
      theme_ipas +
      theme(
        legend.position = "bottom",
        axis.text.x = element_text(size = 13, color = "black", angle = 90, hjust = 1)
      ) -> gr_1
    
    ggplotly(gr_1, tooltip = "text") %>%  
      layout(
        title = list(
          text = paste0(
            "<b>Población total por sexo y entidad de residencia habitual</b>",
            "<br><span style='font-size:14px'>Año: ", input$demografia_ano,
            " | Grupo de edad: ", input$demografia_grupo_edad, "</span>"
          ),
          font = list(family = "Montserrat", size = 18, color = "black"),
          x = 0
        ),
        margin = list(l = 0, r = 0, b = 100, t=60), # Espacio para el caption
        legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -.6),
        title_y = 0.2,
        annotations = list(
          list(
            text = "Fuente: elaboración propia con base en los datos de estimaciones de la población de CONAPO | Ipas Lac",
            xref = "paper", yref = "paper",
            x = 0, y = -.9, # Posición debajo del gráfico
            showarrow = FALSE,
            font = list(family = "Montserrat", size = 10, color = "black"),
            align = "left"
          )
        )
      )  

  })
  
  output$tabla_demografia <- DT::renderDataTable({
    
    df_tabla <- demografia_reactive() %>%
      group_by(entidad) %>%
      mutate(total_entidad = sum(poblacion)) %>%  # Calcular total por entidad
      group_by(entidad, sexo) %>%
      summarise(
        poblacion = sum(poblacion),
        total_entidad = first(total_entidad),
        .groups = "drop"
      ) %>%
      mutate(
        porcentaje = (poblacion / total_entidad) * 100  # Cálculo correcto del porcentaje
      ) %>%
      pivot_wider(
        names_from = "sexo",
        values_from = c("poblacion", "porcentaje")
      ) %>%
      arrange(desc(poblacion_Hombres + poblacion_Mujeres))  # Ordenar por población total
    
    datatable(
      df_tabla,
      extensions = 'Buttons',
      options = list(
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
        pageLength = 5,
        autoWidth = TRUE
      ),
      rownames = FALSE,
      colnames = c("Entidad", "Total","Hombres", "Mujeres", "% Hombres", "% Mujeres")
    ) %>%
      formatRound(columns = c(2, 3, 4), digits = 0) %>%  # Población sin decimales
      formatRound(columns = c(5, 6), digits = 1)  # Porcentaje con 1 decimal
  })
  
  
# Gráfico por grupo de edad -------------------------------------------------
  demografia_reactive_edad <- reactive({
    df_demografia %>%
      filter(
        entidad %in% input$demografia_entidad_edad,
        ano >= input$demografia_ano_edad
        )
  })
  
  output$gr_demografia_edad <- renderPlotly({
    # df_demografia %>% 
      demografia_reactive_edad() %>%
      group_by(grupo_edad) %>% 
      mutate(total_grupo = sum(poblacion)) %>% # Calcular total por grupo de edad
      group_by(sexo, grupo_edad) %>% 
      summarise(
        poblacion = sum(poblacion), 
        total_grupo = first(total_grupo), 
        .groups = 'drop'
      ) %>% 
      mutate(
        porcentaje = poblacion / total_grupo * 100,  # Calcular el porcentaje dentro de cada grupo
        poblacion = ifelse(sexo == "Hombres", -poblacion, poblacion),  # Invertir valores para hombres
        porcentaje_texto = paste0(round(porcentaje, 1), "%"),  # Texto del porcentaje
        tooltip_text = paste0(
          "<b>Sexo:</b> ", sexo, "<br>",
          "<b>Grupo de edad:</b> ", grupo_edad, "<br>",
          "<b>Población:</b> ", scales::comma(abs(poblacion)), "<br>",
          "<b>Proporción en el grupo:</b> ", porcentaje_texto)) %>% 
      ggplot(aes(x = grupo_edad, y = poblacion, fill = sexo, text = tooltip_text)) +
      geom_bar(stat = "identity") +
      scale_y_continuous(labels = abs) +
      coord_flip() +
      labs(
        x = "", y = "", fill = ""
        # title = "Pirámide poblacional por grupo de edad y sexo",
        # subtitle = paste("Año:", input$demografia_ano_edad,
        #                  "\nEntidad:", input$demografia_entidad_edad),
        # caption = "Fuente: elaboración propia con base en los datos de estimaciones de la población de CONAPO | Ipas Lac"
      ) +
      scale_y_continuous(labels = scales::comma) +
      theme_minimal() +
      theme_ipas +
      theme(
        legend.position = "bottom",
        axis.text.x = element_text(size = 10, color = "black", angle = 0, hjust = 1)
      ) -> gr_2
    
    ggplotly(gr_2, tooltip = "text") %>%  
      layout(
        title = list(
          text = paste0(
            "<b>Pirámide poblacional por grupo de edad y sexo</b>",
            "<br><span style='font-size:14px'>Año: ", input$demografia_ano_edad,
            " | Entidad: ", input$demografia_entidad_edad, "</span>"
          ),
          font = list(family = "Montserrat", size = 18, color = "black"),
          x = 0
        ),
        margin = list(l = 0, r = 0, b = 60, t=30), # Espacio para el caption
        legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.02),
        title_y = 0.2,
        annotations = list(
          list(
            text = "Fuente: elaboración propia con base en los datos de estimaciones de la población de CONAPO | Ipas Lac",
            xref = "paper", yref = "paper",
            x = 0, y = -0.2, # Posición debajo del gráfico
            showarrow = FALSE,
            font = list(family = "Montserrat", size = 10, color = "black"),
            align = "left"
          )
        )
      ) 
  })
  
 output$tabla_demografia_edad <- DT::renderDataTable({
  demografia_reactive_edad() %>% 
  # df_demografia %>% 
    group_by(grupo_edad) %>% 
    mutate(total_grupo = sum(poblacion)) %>% # Calcular total por grupo de edad
    group_by(sexo, grupo_edad) %>% 
    summarise(poblacion = sum(poblacion), total_grupo = first(total_grupo), .groups = 'drop') %>% 
    mutate(porcentaje = poblacion / total_grupo * 100) %>% # Calcular porcentaje
    pivot_wider(names_from = "sexo", values_from = c("poblacion", "porcentaje")) %>% 
    datatable(
      extensions = 'Buttons',
      options = list(
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
        pageLength = 5,
        autoWidth = TRUE
      ),
      rownames = FALSE,
      colnames = c("Grupo de edad","Total", "Hombres", "Mujeres", "% Hombres", "% Mujeres")
    ) %>% 
    formatRound(columns = c(2, 3, 4), digits = 0) %>% # Formato sin decimales en población
    formatRound(columns = c(5, 6), digits = 1)  # Formato con 1 decimal en porcentaje
})

# -------------------------------------------------------------------------

  egresos_reactive <- reactive({
    df_egresos_obs %>% 
      filter(gr_edad == input$egresos_grupo_edad,
             tipo %in% input$egresos_tipo,
             unidad_medica %in% input$egresos_unidad_medica,
             ano == input$egresos_ano)
    
  })

  
  output$grafico_egresos <- renderPlotly({
    df_treemap <- egresos_reactive() %>%
      group_by(ano, unidad_medica, gr_edad, tipo) %>% 
      summarise(total = sum(total), .groups = "drop") %>% 
      mutate(porcentaje = total / sum(total),
             porcentaje = scales::percent(porcentaje, accuracy = 0.1))
    
    # Lista de treemaps por unidad médica
    treemap_list <- lapply(unique(df_treemap$unidad_medica), function(um) {
      df_um <- df_treemap %>% filter(unidad_medica == um)
      
      plot_ly(
        data = df_um,
        type = "treemap",
        labels = ~paste0("<b>", tipo, "</b><br>", porcentaje),  # Etiquetas en negrita
        parents = NA,  # Elimina niveles innecesarios
        values = ~total,
        textinfo = "label",  # Solo muestra la etiqueta
        hoverinfo = "text",
        text = ~paste0("<b>Tipo:</b> ", tipo, "<br><b>Porcentaje:</b> ", porcentaje), # Hover text con negritas
        marker = list(colors = RColorBrewer::brewer.pal(9, "Set3")),
        textfont = list(family = "Montserrat", size = 14, color = "black") # Fuente Montserrat
      ) %>%
        layout(title = list(
          text = paste("<b>Unidad Médica:</b> ", um),  # Título de cada unidad médica en negritas
          font = list(family = "Montserrat", size = 16, color = "black"),
          x = 0  # Alineación a la izquierda
        ))
    })
    
    subplot(treemap_list, nrows = ceiling(length(treemap_list) / 3), shareX = TRUE, shareY = TRUE) %>%
      layout(
        title = list(
          text = paste("<b>Egresos Hospitalarios</b>",
                       "<br><b>Entidad:</b> ", input$egresos_unidad_medica,
                       "<br><b>Año:</b> ", input$egresos_ano), # Título principal en negritas
          font = list(family = "Montserrat", size = 18, color = "black"),
          x = 0  # Alineación a la izquierda
        ),
        font = list(family = "Montserrat"),  # Aplica la fuente a todo el gráfico
        margin = list(l = 0, r = 00, t = 60, b = 0)
      )
  })
  
  
  

  
  output$tabla_egresos <- DT::renderDataTable({
    egresos_reactive() %>% 
      group_by(ano, unidad_medica, gr_edad, tipo) %>% 
      summarise(total = sum(total), .groups = 'drop') %>% 
      mutate(porcentaje = total / sum(total) * 100) %>% 
      arrange(desc(total)) %>%
      select(unidad_medica, tipo, total, porcentaje) %>% 
      datatable(
        extensions = 'Buttons',
        options = list(
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
          pageLength = 10,
          autoWidth = TRUE
        ),
        rownames = FALSE,
        colnames = c("Unidad Médica", "Tipo", "Total", "Porcentaje") 
      ) %>%
      formatRound(columns = "total", digits = 0) %>%  # Formato sin decimales en total
      formatPercentage(columns = "porcentaje", digits = 1)  # Formato de porcentaje
  })
  
  
# Muertes maternas ------------------------------------------------------------  
  
  muertes_reactive <- reactive({
    df_muertes %>% 
      filter(ano == input$muertes_ano,
             entidad %in% input$muertes_entidad,
             if (is.null(input$muertes_edad) || input$muertes_edad == "Todas las edades") TRUE else gr_edad %in% input$muertes_edad,
      )
    
  })
  
  output$gr_muertes <- renderPlotly({
    muertes_reactive() %>%
      group_by(ano, entidad, gr_edad) %>% 
      summarise(total = sum(total), .groups = 'drop') %>% 
      pivot_wider(
        names_from = gr_edad, 
        values_from = total
      ) %>%
      mutate(
        `Menores 20` = replace_na(`Menores 20`, 0),  # Evita valores NA
        `Todas Edades` = replace_na(`Todas Edades`, 0),
        porcentaje = ifelse(`Todas Edades` == 0, 0, (`Menores 20` / `Todas Edades`) * 100)  # Cálculo correcto
      ) %>%
      ungroup() %>%
      pivot_longer(cols = c(`Todas Edades`, `Menores 20`), names_to = "gr_edad", values_to = "total") %>%
      mutate(tooltip_text = paste0(
        "<b>Entidad:</b> ", entidad, "<br>",
        "<b>Grupo de edad:</b> ", gr_edad, "<br>",
        "<b>Total:</b> ", scales::comma(abs(total)), "<br>",
        "<b>Porcentaje Menores de 20:</b> ", round(porcentaje, 1), "%<br>"
      )) %>%
      ggplot(aes(
        x = reorder(entidad, -total),
        y = total,
        fill = gr_edad,
        color = gr_edad,
        group = gr_edad,
        text = tooltip_text
      )) +
      geom_line() +
      geom_point() +
      labs(
        x = "", y = "", fill = " ", group="", color="",
        title = paste0("Muertes maternas por entidad y grupo de edad, ", input$muertes_ano),
        subtitle = "Comparación de defunciones maternas en diferentes grupos de edad",
        caption = "Fuente: elaboración propia con base en los datos de la Secretaría de Salud | Ipas Lac"
      ) +
      scale_y_continuous(labels = scales::comma) +
      theme_minimal() +
      theme_ipas +
      theme(
        legend.position = "bottom",
        axis.text.x = element_text(size = 13, color = "black", angle = 90, hjust = 1)
      ) -> gr_4
    
    ggplotly(gr_4, tooltip = "text") %>%  
      layout(
        title = list(
          text = paste0(
            "<b>Muertes maternas por entidad y grupo de edad</b>",
            "<br><span style='font-size:14px'>Año: ", input$muertes_ano, "</span>"
          ),
          font = list(family = "Montserrat", size = 18, color = "black"),
          x = 0
        ),
        margin = list(l = 0, r = 0, b = 10, t = 50),
        legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -1.1),
        title_y = 0.9,
        annotations = list(
          list(
            text = "Fuente: elaboración propia con base en los datos de la Secretaría de Salud | Ipas Lac",
            xref = "paper", yref = "paper",
            x = 0, y = -1.5,
            showarrow = FALSE,
            font = list(family = "Montserrat", size = 10, color = "black"),
            align = "left"
          )
        )
      ) 
  })
  
  output$tabla_muertes <- DT::renderDataTable({
    muertes_reactive() %>%
      group_by(ano, entidad, gr_edad) %>%
      summarise(total = sum(total), .groups = 'drop') %>%
      pivot_wider(
        names_from = gr_edad,
        values_from = total
      ) %>%
      select(ano, entidad, `Todas Edades`, `Menores 20`) %>%
      mutate(
        `Menores 20` = replace_na(`Menores 20`, 0),  # Evita valores NA
        `Todas Edades` = replace_na(`Todas Edades`, 0),
        porcentaje = ifelse(`Todas Edades` == 0, 0, (`Menores 20` / `Todas Edades`) * 100)  # Cálculo correcto
      ) %>%
      arrange(entidad) %>%
      datatable(
        extensions = 'Buttons',
        options = list(
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
          pageLength = 10,
          autoWidth = TRUE
        ),
        rownames = FALSE,
        colnames = c(
          "Año", "Entidad",
          "Total - Todas las edades", "Total - Menores de 20",
          "% Menores de 20"
        )
      ) %>%
      formatRound(columns = c(3,4), digits = 0) %>%
      formatRound(columns = c(5), digits = 1)  
  })
  
  
  
# APEO  ------------------------------------------------------------------------
  
  apeo_reactive <- reactive({
    df_apeo %>% 
      filter(gr_edad == input$apeo_grupo_edad,
             tipo %in% input$apeo_tipo,
             unidad_medica %in% input$apeo_unidad_medica,
             ano == input$apeo_ano)
  })
  
  # Generar el gráfico
  output$grafico_apeo <- renderPlotly({
    df_treemap <- apeo_reactive() %>%
      group_by(ano, unidad_medica, gr_edad, tipo) %>% 
      summarise(total = sum(total), .groups = "drop") %>% 
      mutate(
        porcentaje = total / sum(total),
        porcentaje = scales::percent(porcentaje, accuracy = 0.1)
      )
    
    # Verificar si df_treemap está vacío
    if (nrow(df_treemap) == 0) {
      return(NULL)  # Evita que el gráfico se genere si no hay datos
    }
    
    # Crear lista de treemaps por unidad médica
    treemap_list <- lapply(unique(df_treemap$unidad_medica), function(um) {
      df_um <- df_treemap %>% filter(unidad_medica == um)
      
      plot_ly(
        data = df_um,
        type = "treemap",
        labels = ~paste0("<b>", tipo, "</b><br>", porcentaje),
        parents = NA,
        values = ~total,
        textinfo = "label",
        hoverinfo = "text",
        text = ~paste0("<b>Tipo:</b> ", tipo, "<br><b>Porcentaje:</b> ", porcentaje),
        marker = list(colors = RColorBrewer::brewer.pal(9, "Set3")),
        textfont = list(family = "Montserrat", size = 14, color = "black")
      ) %>%
        layout(title = list(
          text = paste("<b>Unidad Médica:</b> ", um),
          font = list(family = "Montserrat", size = 16, color = "black"),
          x = 0
        ))
    })
    
    # Asegurar que treemap_list no esté vacío
    if (length(treemap_list) == 0) {
      return(NULL)  # Evita que `subplot()` falle
    }
    
    # Unir los gráficos en una sola vista
    subplot(treemap_list, nrows = max(1, ceiling(length(treemap_list) / 3)), shareX = TRUE, shareY = TRUE) %>%
      layout(
        title = list(
          text = paste("<b>APEOS por entidad y año</b>",
                       "<br><b>Año:</b> ", input$apeo_ano,
                       "<br><b>Entidad:</b> ", input$apeo_unidad_medica),
          font = list(family = "Montserrat", size = 18, color = "black"),
          x = 0
        ),
        font = list(family = "Montserrat"),
        margin = list(l = 0, r = 0, t = 80, b = 30),
        legend = list(orientation = "h", x = 0.5, y = -0.2, xanchor = "center")
      )
  })
  
  
  output$tabla_apeo <- DT::renderDataTable({
    apeo_reactive() %>% 
      group_by(ano, unidad_medica, gr_edad, tipo) %>% 
      summarise(total = sum(total), .groups = 'drop') %>% 
      mutate(porcentaje = total / sum(total) * 100) %>% 
      arrange(desc(total)) %>%
      select(unidad_medica, tipo, total, porcentaje) %>% 
      datatable(
        extensions = 'Buttons',
        options = list(
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
          pageLength = 10,
          autoWidth = TRUE
        ),
        rownames = FALSE,
        colnames = c("Unidad Médica", "Tipo", "Total", "Porcentaje:") 
      ) %>%
      formatRound(columns = "total", digits = 0) %>%  # Formato sin decimales en total
      formatPercentage(columns = "porcentaje", digits = 1)  # Formato de porcentaje
  })
  
  
# Nacimientos--------------------------------------------------------------------
  
  nacimiento_reactive <- reactive({
    df_nacimiento %>% 
      filter(ano == input$nacimiento_ano,
             entidad %in% input$nacimiento_entidad,
             if (is.null(input$nacimiento_entidad) || input$nacimiento_entidad == "Nacional") TRUE else entidad %in% input$nacimiento_entidad,
      )
    
  })
  
  output$gr_nacimiento <- renderPlotly({
    nacimiento_reactive() %>%
      mutate(tooltip_text = paste0(
        "<b>Año:</b> ", ano, "<br>",
        "<b>Entidad:</b> ", entidad, "<br>",
        "<b>Total:</b> ", scales::comma(abs(total)), "<br>")) %>%
      ggplot(aes(
        x = reorder(entidad, -total),
        y = total,
        fill = total,
        text = tooltip_text
      )) +
      geom_col()+
      labs(
        x = "", y = "", fill = " ",
        title = paste0("Total de nacimientos por entidad y año, ", input$nacimiento_ano),
        caption = "Fuente: elaboración propia con base en los datos de la Secretaría de Salud | Ipas Lac"
      ) +
      scale_y_continuous(labels = scales::comma) +
      theme_minimal() +
      theme_ipas +
      theme(
        legend.position = "bottom",
        axis.text.x = element_text(size = 13, color = "black", angle = 90, hjust = 1)
      ) -> gr_7
    
    ggplotly(gr_7, tooltip = "text") %>%  
      layout(
        title = list(
          text = paste0(
            "<b>Total de nacimientos por entidad y año en menores de 20 años</b>",
            "<br><span style='font-size:14px'>Año: ", input$nacimiento_ano, "</span>"
          ),
          font = list(family = "Montserrat", size = 18, color = "black"),
          x = 0
        ),
        margin = list(l = 0, r = 0, b = 10, t = 50),
        legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -1.1),
        title_y = 0.9,
        annotations = list(
          list(
            text = "Fuente: elaboración propia con base en los datos de la Secretaría de Salud | Ipas Lac",
            xref = "paper", yref = "paper",
            x = 0, y = -1.1,
            showarrow = FALSE,
            font = list(family = "Montserrat", size = 10, color = "black"),
            align = "left"
          )
        )
      ) 
  })
  
  output$tabla_nacimiento <- DT::renderDataTable({
    nacimiento_reactive() %>%
      select(ano, entidad, total) %>% 
      arrange(-total) %>%
      datatable(
        extensions = 'Buttons',
        options = list(
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
          pageLength = 10,
          autoWidth = TRUE
        ),
        rownames = FALSE,
        colnames = c(
          "Año", "Entidad",
          "Total"
        )
      ) %>%
      # formatRound(columns = c(3,4), digits = 0) %>%
      formatRound(columns = c(3), digits = 0)  
  })
  
  
# Violencia --------------------------------------------------------------------
  
  violencia_reactive <- reactive({
    df_violencia %>% 
      filter(
        fecha %in% input$violencia_fecha,
        delito %in% input$violencia_delito,
        entidad %in% input$violencia_entidad
      )
  })
  
  output$gr_violencia <- renderPlotly({
    violencia_reactive() %>%
      group_by(fecha, entidad, delito) %>% 
      summarise(total = sum(total), .groups = "drop") %>% 
      group_by(fecha, entidad) %>%
      mutate(porcentaje = (total / sum(total)) * 100) %>%  # Corrección de porcentaje
      ungroup() %>%
      mutate(tooltip_text = paste0(
        "<b>Año:</b> ", fecha, "<br>",
        "<b>Entidad:</b> ", entidad, "<br>",
        "<b>Delito:</b> ", delito, "<br>",
        "<b>Total:</b> ", scales::comma(abs(total)), "<br>",
        "<b>Porcentaje:</b> ", round(porcentaje, 1), "%<br>"
      )) %>%
      ggplot(aes(
        x = reorder(entidad, -total),
        y = total,
        fill = delito,
        text = tooltip_text  # Agregado para que Plotly lo reconozca
      )) +
      geom_col() +
      labs(
        x = "", y = "", fill = " ",
        title = paste0("Total de carpetas de investigación por tipo de delito y año, ", input$violencia_fecha),
        caption = "Fuente: elaboración propia con base en los datos del SESNSP | Ipas Lac"
      ) +
      scale_y_continuous(labels = scales::comma) +
      theme_minimal() +
      theme_ipas +
      theme(
        legend.position = "bottom",
        axis.text.x = element_text(size = 12, color = "black", angle = 90, hjust = 1)
      ) -> gr_8
    
    ggplotly(gr_8, tooltip = "text") %>%  
      layout(
        title = list(
          text = paste0(
            "<b>Total de carpetas de investigación por tipo de delito y año</b>",
            "<br><span style='font-size:14px'>Año: ", input$violencia_fecha, "</span>"),
          font = list(family = "Montserrat", size = 18, color = "black"), x = 0),
        margin = list(l = 0, r = 0, b = 20, t = 50),
        legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -.7,
                      font=list(size=10)), title_y = 0.9,
        annotations = list(
          list(
            text = "Fuente: elaboración propia con base en los datos del SESNSP | Ipas Lac",
            xref = "paper", yref = "paper",
            x = 0, y = -1,
            showarrow = FALSE,
            font = list(family = "Montserrat", size = 10, color = "black"),
            align = "left"
          ))) 
  })
  
  output$tabla_violencia <- DT::renderDataTable({
    violencia_reactive() %>%
      group_by(fecha, entidad, delito) %>% 
      summarise(total = sum(total), .groups = "drop") %>% 
      group_by(fecha, entidad) %>%
      mutate(porcentaje = (total / sum(total)) * 100) %>%
      arrange(desc(rowSums(across(where(is.numeric))))) %>% 
      datatable(
        extensions = 'Buttons',
        options = list(
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
          pageLength = 10,
          autoWidth = TRUE
        ),
        rownames = FALSE,
        colnames = c("Año", "Entidad", "Delito", "Total", "Porcentaje")
      ) %>%
      formatRound(columns = 4, digits = 0) %>%  
      formatRound(columns = 5, digits = 2)
  })
  
  # ENADID ----------------------------------------------------------------------
  enadid_reactive <- reactive({
    enadid %>% 
      filter(
        ano %in% input$enadid_año, 
        grupo_edad %in% input$enadid_edad, 
        nom_ent %in% input$enadid_entidad
      )
  })
  
  enadid_reactive2 <- reactive({
    enadid %>% 
      filter(
        ano %in% input$enadid_año2, 
        grupo_edad %in% input$enadid_edad2, 
        nom_ent %in% input$enadid_entidad2
      )
  })
  
  enadid_reactive3 <- reactive({
    enadid %>% 
      filter(
        # ano %in% input$enadid_año2, 
        grupo_edad %in% input$enadid_edad3, 
        nom_ent %in% input$enadid_entidad3
      )
  })
  
  output$gr_tasa_fecundidad <- renderPlotly({
    
    data_nacional <- enadid_reactive() %>% 
      filter(edad>=15, edad<=44) %>%
      mutate(hijos_nacidos2 =as.integer(hijos_nacidos2)*factor) %>%
      # group_by(nom_ent) %>%
      summarise(mujeres=sum(factor),
                hijos_vivos=sum(hijos_nacidos2 , na.rm = T)) %>%
      mutate(tasa=hijos_vivos/mujeres, 
             nom_ent="Nacional")
    
    gr_tasa <- enadid_reactive() %>% 
      filter(edad>=15, edad<=44) %>%
      mutate(hijos_nacidos2 =as.integer(hijos_nacidos2)*factor) %>%
      group_by(nom_ent) %>%
      summarise(mujeres=sum(factor),
                hijos_vivos=sum(hijos_nacidos2 , na.rm = T)) %>%
      mutate(tasa=hijos_vivos/mujeres) %>%
      bind_rows(data_nacional) %>% 
      mutate(col_nacional=ifelse(nom_ent=="Nacional", 1, 0)) %>% 
      mutate(tooltip_text = paste0(
        "<b>Entidad:</b> ", nom_ent, "<br>",
        "<b>Tasa:</b> ", scales::comma(tasa, .01), "<br>"
      )) %>%  
      ggplot(aes(reorder(nom_ent, -tasa), tasa, 
                 text=tooltip_text, fill=factor(col_nacional)
                 )) +
      geom_col() +
      
    theme_ipas +
      theme(axis.text.x = element_text(angle = 90), 
            legend.position = "none") +
      scale_fill_manual(values = c("#f9592a", "grey")) +
      labs(x="Entidad", y="Tasa de fecundidad", fill="") 
    
    ggplotly(gr_tasa, tooltip = "text") %>%
      layout(
        title = list(
          text = paste0(
            "<b>Tasa de fecundidad de los últimos 6 años por entidad</b>",
            "<br><span style='font-size:14px'>Año: ", input$enadid_año, "</span>"),
          font = list(family = "Montserrat", size = 18, color = "black"), x = 0),
        margin = list(l = 0, r = 0, b = 20, t = 50),
        legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -.7,
                      font=list(size=10)), title_y = 0.9,
        annotations = list(
          list(
            text = "Fuente: elaboración propia con base en los datos dela ENADID | Ipas Lac",
            xref = "paper", yref = "paper",
            x = 0, y = -1,
            showarrow = FALSE,
            font = list(family = "Montserrat", size = 10, color = "black"),
            align = "left"
          )))
  })
  
  output$gr_prom_abortos <- renderPlotly({
    
    data_nacional <- enadid_reactive() %>% 
      # filter(edad>=15, edad<=44) %>%
      replace_na(list(abortos=0)) %>% 
      mutate(abortos=abortos*factor) %>%
      summarise(mujeres=sum(factor),
                abortos=sum(abortos, na.rm = T)) %>%
      mutate(tasa=abortos/mujeres) %>% 
      mutate(nom_ent="Nacional") %>% ungroup()
    
    gr_tasa <- enadid_reactive() %>% 
      # filter(edad>=15, edad<=44) %>%
      replace_na(list(abortos=0)) %>% 
      mutate(abortos=abortos*factor) %>%
      group_by(nom_ent) %>%
      summarise(mujeres=sum(factor),
                abortos=sum(abortos, na.rm = T)) %>%
      mutate(tasa=abortos/mujeres) %>%
      bind_rows(data_nacional) %>% 
      mutate(tooltip_text = paste0(
        "<b>Entidad:</b> ", nom_ent, "<br>",
        "<b>Tasa:</b> ", scales::comma(tasa, .01), "<br>"
      )) %>%  
      
      mutate(col_nacional=ifelse(nom_ent=="Nacional", 1, 0)) %>% 
      ggplot(aes(reorder(nom_ent, -tasa), tasa, 
                 text=tooltip_text, fill=factor(col_nacional)
      )) +
      geom_col(position = "dodge") +
      theme_ipas +
      theme(axis.text.x = element_text(angle = 90), 
            legend.position = "none") +
      scale_fill_manual(values = c("#b75dea", "grey")) +
      labs(x="Entidad", y="Tasa de abortos", fill="") 
    
    ggplotly(gr_tasa, tooltip = "text") %>%
      layout(
        title = list(
          text = paste0(
            "<b>Tasa de abortos por cada 100 mujeres y por entidad</b>",
            "<br><span style='font-size:14px'>Año: ", input$enadid_año, "</span>"),
          font = list(family = "Montserrat", size = 18, color = "black"), x = 0),
        margin = list(l = 0, r = 0, b = 20, t = 50),
        legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -.7,
                      font=list(size=10)), title_y = 0.9,
        annotations = list(
          list(
            text = "Fuente: elaboración propia con base en los datos dela ENADID | Ipas Lac",
            xref = "paper", yref = "paper",
            x = 0, y = -1,
            showarrow = FALSE,
            font = list(family = "Montserrat", size = 10, color = "black"),
            align = "left"
          )))
  })
  
  output$gr_uso_anti_primera_vez <- renderPlotly({
    
    data_nacional <-  enadid_reactive2() %>% 
      drop_na(edad_primera_relacion_sexual) %>% 
      gather(tipo, Total, no_uso:no_responde) %>% 
      filter(Total==2) %>% 
      mutate(tipo=str_replace_all(tipo, "_", " ")) %>% 
      group_by(tipo) %>%
      summarise(uso_metodos=sum(factor)) %>% #group_by(nom_ent) %>% 
      mutate(Porcentaje=uso_metodos/sum(uso_metodos), 
             nom_ent="Nacional")
    
    gr_tasa <- enadid_reactive2() %>% 
      drop_na(edad_primera_relacion_sexual) %>% 
      gather(tipo, Total, no_uso:no_responde) %>% 
      filter(Total==2) %>% 
      mutate(tipo=str_replace_all(tipo, "_", " ")) %>% 
      group_by(nom_ent, tipo) %>%
      summarise(uso_metodos=sum(factor)) %>% group_by(nom_ent) %>% 
      mutate(Porcentaje=uso_metodos/sum(uso_metodos)) %>% 
      bind_rows(data_nacional) %>% 
      mutate(tooltip_text = paste0(
        "<b>Entidad:</b> ", nom_ent, "<br>",
        "<b>Tipo de anticonceptivo:</b> ", tipo, "<br>",
        # "<b>Total:</b> ", scales::comma(uso_metodos, 1), "<br>",
        "<b>Porcentaje:</b> ", scales::percent(Porcentaje, 1), "<br>"
      )) %>%  
      ggplot(aes(nom_ent, Porcentaje, 
                 text=tooltip_text, fill=tipo
      )) +
      geom_col() +
      labs(x="Entidad", y="Porcentaje de uso\nde anticonceptivos") +
      theme_ipas +
      theme(axis.text.x = element_text(angle = 90), 
            legend.position = "none") +
      scale_y_continuous(labels = percent)
    
    ggplotly(gr_tasa, tooltip = "text") %>%
      layout(
        title = list(
          text = paste0(
            "<b>Total de uso de conceptivos en la primera relación sexual</b>",
            "<br><span style='font-size:14px'>Año: ", input$enadid_año2, "</span>"),
          font = list(family = "Montserrat", size = 18, color = "black"), x = 0),
        margin = list(l = 0, r = 0, b = 20, t = 50),
        legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -.7,
                      font=list(size=10)), title_y = 0.9,
        annotations = list(
          list(
            text = "Fuente: elaboración propia con base en los datos dela ENADID | Ipas Lac",
            xref = "paper", yref = "paper",
            x = 0, y = -1,
            showarrow = FALSE,
            font = list(family = "Montserrat", size = 10, color = "black"),
            align = "left"
          )))
  })
  
  
  output$gr_uso_anticonceptivo <- renderPlotly({
    
    data_nacional <- enadid_reactive2() %>% 
      group_by("tipo"=actual_metodo) %>%
      summarise(uso_metodos=sum(factor)) %>% 
      mutate(Porcentaje=uso_metodos/sum(uso_metodos)) %>% 
      mutate(nom_ent="Nacional") %>% ungroup() 
    
    gr_tasa <- enadid_reactive2() %>% 
      group_by("tipo"=actual_metodo, nom_ent) %>%
      summarise(uso_metodos=sum(factor)) %>% group_by(nom_ent) %>% 
      mutate(Porcentaje=uso_metodos/sum(uso_metodos)) %>% ungroup() %>% 
      bind_rows(data_nacional) %>% 
      mutate(tipo=factor(tipo, 
                         labels = c("OTB", "Vasectomía", "Pastillas anticonceptivas", 
                                    "Inyecciones anticonceptivas",
                                    "Implante anticonceptivo", "Parche anticonceptivo", 
                                    "DIU", "Condón masculino", "Condón femenino"
                                    )
                         )) %>% 
      mutate(tooltip_text = paste0(
        "<b>Entidad:</b> ", nom_ent, "<br>",
        "<b>Tipo de anticonceptivo:</b> ", tipo, "<br>",
        # "<b>Total:</b> ", scales::comma(uso_metodos, 1), "<br>",
        "<b>Porcentaje:</b> ", scales::percent(Porcentaje, 1), "<br>"
      )) %>%  
      ggplot(aes(nom_ent, Porcentaje, 
                 text=tooltip_text, fill=tipo
      )) +
      geom_col() +
      labs(x="Entidad", y="Porcentaje de uso\nde anticonceptivos") +
      theme_ipas +
      theme(axis.text.x = element_text(angle = 90), 
            legend.position = "none") +
      scale_y_continuous(labels = percent)
    
    ggplotly(gr_tasa, tooltip = "text") %>%
      layout(
        title = list(
          text = paste0(
            "<b>Uso de conceptivos actualmente</b>",
            "<br><span style='font-size:14px'>Año: ", input$enadid_año2, "</span>"),
          font = list(family = "Montserrat", size = 18, color = "black"), x = 0),
        margin = list(l = 0, r = 0, b = 20, t = 50),
        legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -.7,
                      font=list(size=10)), title_y = 0.9,
        annotations = list(
          list(
            text = "Fuente: elaboración propia con base en los datos dela ENADID | Ipas Lac",
            xref = "paper", yref = "paper",
            x = 0, y = -1,
            showarrow = FALSE,
            font = list(family = "Montserrat", size = 10, color = "black"),
            align = "left"
          )))
  })
  
  #####
  #en el tiempo #
  output$gr_tasa_fecundidad_tiempo <- renderPlotly({
    
    # data_nacional <- total_enadid %>%
    #   filter(grupo_edad %in% input$enadid_edad3) %>% 
    #   filter(edad>=15, edad<=44) %>%
    #   mutate(hijos_nacidos2 =as.integer(hijos_nacidos2)*factor) %>%
    #   group_by(ano) %>%
    #   summarise(mujeres=sum(factor),
    #             hijos_vivos=sum(hijos_nacidos2 , na.rm = T)) %>%
    #   mutate(tasa=hijos_vivos/mujeres, 
    #          nom_ent="Nacional")
    
    gr_tasa <- enadid_reactive3() %>% 
      filter(edad>=15, edad<=44) %>%
      mutate(hijos_nacidos2 =as.integer(hijos_nacidos2)*factor) %>%
      group_by(nom_ent, ano) %>%
      summarise(mujeres=sum(factor),
                hijos_vivos=sum(hijos_nacidos2 , na.rm = T)) %>%
      mutate(tasa=hijos_vivos/mujeres) %>%
      # bind_rows(data_nacional) %>% 
      mutate(col_nacional=ifelse(nom_ent=="Nacional", 1, 0)) %>% 
      mutate(tooltip_text = paste0(
        "<b>Entidad:</b> ", nom_ent, "<br>",
        "<b>Tasa:</b> ", scales::comma(tasa, .01), "<br>", 
        "<b>Año:</b> ", ano, "<br>"
      )) %>%  
      ggplot(aes(factor(ano), tasa, 
                 text=tooltip_text, fill=factor(ano)
      )) +
      geom_col(position = "dodge") +
      
      theme_ipas +
      theme(#axis.text.x = element_text(angle = 90), 
            legend.position = "none") +
      scale_fill_manual(values = c("#f9592a", "grey")) +
      labs(x="Año", y="Tasa de fecundidad", fill="")+
      theme(axis.text.x = element_text(angle = 0))
    
    ggplotly(gr_tasa, tooltip = "text") %>%
      layout(
        title = list(
          text = paste0(
            "<b>Tasa de fecundidad de los últimos 6 años por entidad</b>",
            "<br><span style='font-size:14px'>Entidad: ", input$enadid_entidad3, "</span>"),
          font = list(family = "Montserrat", size = 18, color = "black"), x = 0),
        margin = list(l = 0, r = 0, b = 20, t = 50),
        legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -.7,
                      font=list(size=10)), title_y = 0.9,
        annotations = list(
          list(
            text = "Fuente: elaboración propia con base en los datos dela ENADID | Ipas Lac",
            xref = "paper", yref = "paper",
            x = 0, y = -1,
            showarrow = FALSE,
            font = list(family = "Montserrat", size = 10, color = "black"),
            align = "left"
          )))
  })
  
  output$gr_prom_abortos_tiempo <- renderPlotly({
    
    # data_nacional <- 
    #   total_enadid %>%
    #   filter(grupo_edad %in% input$enadid_edad3) %>% 
    #   replace_na(list(abortos=0)) %>% 
    #   mutate(abortos=abortos*factor) %>%
    #   group_by(ano) %>% 
    #   summarise(mujeres=sum(factor),
    #             abortos=sum(abortos, na.rm = T)) %>%
    #   mutate(tasa=abortos/mujeres) %>% 
    #   mutate(nom_ent="Nacional") %>% ungroup()
    
    gr_tasa <- enadid_reactive3() %>% 
      # filter(edad>=15, edad<=44) %>%
      replace_na(list(abortos=0)) %>% 
      mutate(abortos=abortos*factor) %>%
      group_by(nom_ent, ano) %>%
      summarise(mujeres=sum(factor),
                abortos=sum(abortos, na.rm = T)) %>%
      mutate(tasa=abortos/mujeres) %>%
      # bind_rows(data_nacional) %>% 
      mutate(tooltip_text = paste0(
        "<b>Entidad:</b> ", nom_ent, "<br>",
        "<b>Tasa:</b> ", scales::comma(tasa, .01), "<br>", 
        "<b>Año:</b> ", ano, "<br>"
      )) %>%  
      
      mutate(col_nacional=ifelse(nom_ent=="Nacional", 1, 0)) %>% 
      ggplot(aes(factor(ano), tasa, 
                 text=tooltip_text, fill=factor(ano)
      )) +
      geom_col(position = "dodge") +
      theme_ipas +
      theme(#axis.text.x = element_text(angle = 90), 
            legend.position = "none") +
      scale_fill_manual(values = c("#b75dea", "grey")) +
      labs(x="Año", y="Tasa de abortos", fill="") +
      theme(axis.text.x = element_text(angle = 0))
    
    ggplotly(gr_tasa, tooltip = "text") %>%
      layout(
        title = list(
          text = paste0(
            "<b>Tasa de abortos por cada 100 mujeres y por entidad</b>",
            "<br><span style='font-size:14px'>Entidad: ", input$enadid_entidad3, "</span>"),
          font = list(family = "Montserrat", size = 18, color = "black"), x = 0),
        margin = list(l = 0, r = 0, b = 20, t = 50),
        legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -.7,
                      font=list(size=10)), title_y = 0.9,
        annotations = list(
          list(
            text = "Fuente: elaboración propia con base en los datos dela ENADID | Ipas Lac",
            xref = "paper", yref = "paper",
            x = 0, y = -1,
            showarrow = FALSE,
            font = list(family = "Montserrat", size = 10, color = "black"),
            align = "left"
          )))
  })
  
  output$gr_uso_anti_primera_vez_tiempo <- renderPlotly({
    
    # data_nacional <-  total_enadid %>%
    #   filter(grupo_edad %in% input$enadid_edad3) %>% 
    #   drop_na(edad_primera_relacion_sexual) %>% 
    #   gather(tipo, Total, no_uso:no_responde) %>% 
    #   filter(Total==2) %>% 
    #   mutate(tipo=str_replace_all(tipo, "_", " ")) %>% 
    #   group_by(tipo, ano) %>%
    #   summarise(uso_metodos=sum(factor)) %>% group_by(ano) %>% 
    #   mutate(Porcentaje=uso_metodos/sum(uso_metodos), 
    #          nom_ent="Nacional")
    
    gr_tasa <- enadid_reactive3() %>% 
      drop_na(edad_primera_relacion_sexual) %>% 
      gather(tipo, Total, no_uso:no_responde) %>% 
      filter(Total==2) %>% 
      mutate(tipo=str_replace_all(tipo, "_", " ")) %>% 
      group_by(nom_ent, tipo, ano) %>%
      summarise(uso_metodos=sum(factor)) %>% group_by(nom_ent, ano) %>% 
      mutate(Porcentaje=uso_metodos/sum(uso_metodos)) %>% 
      # bind_rows(data_nacional) %>% 
      mutate(tooltip_text = paste0(
        "<b>Entidad:</b> ", nom_ent, "<br>",
        "<b>Tipo de anticonceptivo:</b> ", tipo, "<br>",
        # "<b>Total:</b> ", scales::comma(uso_metodos, 1), "<br>",
        "<b>Porcentaje:</b> ", scales::percent(Porcentaje, 1), "<br>", 
        "<b>Año:</b> ", ano, "<br>"
      )) %>%  
      mutate(tipo=str_wrap(tipo, 12)) %>% 
      ggplot(aes(tipo, Porcentaje, 
                 text=tooltip_text, fill=factor(ano)
      )) +
      geom_col(position = "dodge") +
      labs(x="Tipo", y="Porcentaje de uso\nde anticonceptivos") +
      theme_ipas +
      theme(axis.text.x = element_text(angle = 0), 
            legend.position = "none") +
      scale_y_continuous(labels = percent) #+
      # facet_wrap(.~ano)
    
    ggplotly(gr_tasa, tooltip = "text") %>%
      layout(
        title = list(
          text = paste0(
            "<b>Total de uso de conceptivos en la primera relación sexual</b>",
            "<br><span style='font-size:14px'>Entidad: ", input$enadid_entidad3, "</span>"),
          font = list(family = "Montserrat", size = 18, color = "black"), x = 0),
        margin = list(l = 0, r = 0, b = 20, t = 50),
        legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -.7,
                      font=list(size=10)), title_y = 0.9,
        annotations = list(
          list(
            text = "Fuente: elaboración propia con base en los datos dela ENADID | Ipas Lac",
            xref = "paper", yref = "paper",
            x = 0, y = -1,
            showarrow = FALSE,
            font = list(family = "Montserrat", size = 10, color = "black"),
            align = "left"
          )))
  })
  
  
  output$gr_uso_anticonceptivo_tiempo <- renderPlotly({
    
    # data_nacional <- total_enadid %>%
    #   filter(grupo_edad %in% input$enadid_edad3) %>% 
    #   group_by("tipo"=actual_metodo, ano) %>%
    #   summarise(uso_metodos=sum(factor)) %>% group_by(ano) %>% 
    #   mutate(Porcentaje=uso_metodos/sum(uso_metodos)) %>% 
    #   mutate(nom_ent="Nacional") %>% ungroup() 
    
    gr_tasa <- enadid_reactive3() %>% 
      group_by("tipo"=actual_metodo, nom_ent, ano) %>%
      summarise(uso_metodos=sum(factor)) %>% group_by(nom_ent, ano) %>% 
      mutate(Porcentaje=uso_metodos/sum(uso_metodos)) %>% ungroup() %>% 
      # bind_rows(data_nacional) %>% 
      complete(tipo=c(1:9
      ), 
      ano=unique(enadid$ano), 
      nom_ent=input$enadid_entidad3,
      fill=list(uso_metodos=0, Porcentaje=0)
      ) %>% 
      mutate(tipo=factor(tipo, 
                         labels = c("OTB", "Vasectomía", "Pastillas anticonceptivas", 
                                    "Inyecciones anticonceptivas",
                                    "Implante anticonceptivo", "Parche anticonceptivo", 
                                    "DIU", "Condón masculino", "Condón femenino"
                         )
      )) %>% 
      mutate(tooltip_text = paste0(
        "<b>Entidad:</b> ", nom_ent, "<br>",
        "<b>Tipo de anticonceptivo:</b> ", tipo, "<br>",
        # "<b>Total:</b> ", scales::comma(uso_metodos, 1), "<br>",
        "<b>Porcentaje:</b> ", scales::percent(Porcentaje, 1), "<br>", 
        "<b>Año:</b> ", ano, "<br>"
      )) %>%  
      mutate(tipo=str_wrap(tipo, 12)) %>% 
      ggplot(aes(tipo, Porcentaje, 
                 text=tooltip_text, fill=factor(ano)
      )) +
      geom_col(position = "dodge") +
      labs(x="Tipo", y="Porcentaje de uso\nde anticonceptivos") +
      theme_ipas +
      theme(axis.text.x = element_text(angle = 0), 
            legend.position = "none") +
      scale_y_continuous(labels = percent) #  +
      # facet_wrap(.~ano)
    
    ggplotly(gr_tasa, tooltip = "text") %>%
      layout(
        title = list(
          text = paste0(
            "<b>Uso de conceptivos actualmente</b>",
            "<br><span style='font-size:14px'>Entidad: ", input$enadid_entidad3, "</span>"),
          font = list(family = "Montserrat", size = 18, color = "black"), x = 0),
        margin = list(l = 0, r = 0, b = 20, t = 50),
        legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -.7,
                      font=list(size=10)), title_y = 0.9,
        annotations = list(
          list(
            text = "Fuente: elaboración propia con base en los datos dela ENADID | Ipas Lac",
            xref = "paper", yref = "paper",
            x = 0, y = -1,
            showarrow = FALSE,
            font = list(family = "Montserrat", size = 10, color = "black"),
            align = "left"
          )))
  })
  
  #####
  #tablas 
  output$tabla_fecundidad <- DT::renderDataTable({
    tabla_nacional <- enadid %>% 
      filter(edad>=15, edad<=44) %>%
      mutate(hijos_nacidos2 =as.integer(hijos_nacidos2)*factor) %>%
      group_by(ano) %>%
      summarise(mujeres=sum(factor),
                hijos_vivos=sum(hijos_nacidos2 , na.rm = T)) %>%
      mutate(tasa=hijos_vivos/mujeres, 
             nom_ent="Nacional")

      tabla_estatal <- enadid_reactive() %>% 
        filter(edad>=15, edad<=44) %>%
        mutate(hijos_nacidos2 =as.integer(hijos_nacidos2)*factor) %>%
        group_by(nom_ent, ano) %>%
        summarise(mujeres=sum(factor),
                  hijos_vivos=sum(hijos_nacidos2 , na.rm = T)) %>%
        mutate(tasa=hijos_vivos/mujeres) %>%
        bind_rows(tabla_nacional)
      
      tabla_estatal %>%
        arrange(desc(rowSums(across(where(is.numeric))))) %>%
        datatable(
          extensions = 'Buttons',
          options = list(
            dom = 'Bfrtip',
            buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
            pageLength = 10,
            autoWidth = TRUE
          ),
          rownames = FALSE,
          colnames = c("Entidad", "Año", "Total de mujeres",
                       "Hijos vivos", "Tasa"), 
          caption = htmltools::tags$caption(
            style = 'font-size: 25px; color: black;', # Personaliza el tamaño y color
            "Tabla de indicador de fecundidad de los últimos 6 años"
          )
        ) %>%
        formatRound(columns = 4, digits = 0) %>%
        formatRound(columns = 5, digits = 2)
      

    
    
  })
  
  output$tabla_abortos <- DT::renderDataTable({
    tabla_nacional <- enadid %>% 
      # filter(edad>=15, edad<=44) %>%
      replace_na(list(abortos=0)) %>% 
      mutate(abortos=abortos*factor) %>%
      group_by(ano) %>% 
      summarise(mujeres=sum(factor),
                abortos=sum(abortos, na.rm = T)) %>%
      mutate(tasa=abortos/mujeres) %>% 
      mutate(nom_ent="Nacional") %>% ungroup()
    
    tabla_estatal <- enadid_reactive() %>% 
      # filter(edad>=15, edad<=44) %>%
      replace_na(list(abortos=0)) %>% 
      mutate(abortos=abortos*factor) %>%
      group_by(nom_ent, ano) %>%
      summarise(mujeres=sum(factor),
                abortos=sum(abortos, na.rm = T)) %>%
      mutate(tasa=abortos/mujeres) %>%
      bind_rows(tabla_nacional)
    
    tabla_estatal %>% 
      arrange(desc(rowSums(across(where(is.numeric))))) %>% 
      datatable(
        extensions = 'Buttons',
        options = list(
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
          pageLength = 10,
          autoWidth = TRUE
        ),
        rownames = FALSE,
        colnames = c("Entidad", "Año", "Total de mujeres",
                     "Total de abortos", "Tasa"),
        caption = htmltools::tags$caption(
          style = 'font-size: 25px; color: black;', # Personaliza el tamaño y color
          "Tabla de indicador de abortos"
        )
      ) %>%
      formatRound(columns = 4, digits = 0) %>%  
      formatRound(columns = 5, digits = 2)
    
  })
  
  output$tabla_uso_anti_primera_vez <- DT::renderDataTable({
    tabla_nacional <-  enadid %>% 
      drop_na(edad_primera_relacion_sexual) %>% 
      gather(tipo, Total, no_uso:no_responde) %>% 
      filter(Total==2) %>% 
      mutate(tipo=str_replace_all(tipo, "_", " ")) %>% 
      group_by(ano, tipo) %>%
      summarise(uso_metodos=sum(factor)) %>% #group_by(nom_ent) %>% 
      mutate(Porcentaje=uso_metodos/sum(uso_metodos), 
             nom_ent="Nacional")
    
    tabla_estatal <- enadid %>% 
      drop_na(edad_primera_relacion_sexual) %>% 
      gather(tipo, Total, no_uso:no_responde) %>% 
      filter(Total==2) %>% 
      mutate(tipo=str_replace_all(tipo, "_", " ")) %>% 
      group_by(ano, nom_ent, tipo) %>%
      summarise(uso_metodos=sum(factor)) %>% group_by(nom_ent) %>% 
      mutate(Porcentaje=uso_metodos/sum(uso_metodos)) %>% 
      bind_rows(tabla_nacional)
    
    tabla_estatal %>% 
      arrange(desc(rowSums(across(where(is.numeric))))) %>% 
      datatable(
        extensions = 'Buttons',
        options = list(
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
          pageLength = 10,
          autoWidth = TRUE
        ),
        rownames = FALSE,
        colnames = c("Año", "Entidad", "Tipo de anticonceptivo",
                     "Total",
                     "Porcentaje"), 
        caption = htmltools::tags$caption(
          style = 'font-size: 25px; color: black;', # Personaliza el tamaño y color
          "Tabla de indicador de uso de anticonceptivos en la primera relación sexual"
        )
      ) %>%
      formatRound(columns = 4, digits = 0) %>%  
      formatRound(columns = 5, digits = 2)
  })
  
  output$tabla_uso_anticonceptivo <- DT::renderDataTable({
    tabla_nacional <- enadid %>% 
      group_by(ano, "tipo"=actual_metodo) %>%
      summarise(uso_metodos=sum(factor)) %>% group_by(ano) %>% 
      mutate(Porcentaje=uso_metodos/sum(uso_metodos)) %>% 
      mutate(nom_ent="Nacional") %>% ungroup() 
    
    tabla_estatal <- enadid %>% 
      group_by(ano, "tipo"=actual_metodo, nom_ent) %>%
      summarise(uso_metodos=sum(factor)) %>% group_by(ano, nom_ent) %>% 
      mutate(Porcentaje=uso_metodos/sum(uso_metodos)) %>% ungroup() %>% 
      bind_rows(tabla_nacional) %>% 
      mutate(tipo=factor(tipo, 
                         labels = c("OTB", "Vasectomía", "Pastillas anticonceptivas", 
                                    "Inyecciones anticonceptivas",
                                    "Implante anticonceptivo", "Parche anticonceptivo", 
                                    "DIU", "Condón masculino", "Condón femenino"
                         )
      )) %>% 
      # mutate(tooltip_text = paste0(
      #   "<b>Entidad:</b> ", nom_ent, "<br>",
      #   "<b>Tipo de anticonceptivo:</b> ", tipo, "<br>",
      #   # "<b>Total:</b> ", scales::comma(uso_metodos, 1), "<br>",
      #   "<b>Porcentaje:</b> ", scales::percent(Porcentaje, 1), "<br>"
      # )) %>%
      relocate(nom_ent, .before = "tipo")
    
    tabla_estatal %>% 
      arrange(desc(rowSums(across(where(is.numeric))))) %>% 
      datatable(
        extensions = 'Buttons',
        options = list(
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
          pageLength = 10,
          autoWidth = TRUE
        ),
        rownames = FALSE,
        colnames = c("Año", "Entidad", "Tipo de anticonceptivo",
                     "Total",
                     "Porcentaje"), 
        caption = htmltools::tags$caption(
          style = 'font-size: 25px; color: black;', # Personaliza el tamaño y color
          "Tabla de indicador de uso actual de anticonceptivos"
        )
      ) %>%
      formatRound(columns = 4, digits = 0) %>%  
      formatRound(columns = 5, digits = 2)
  })
  
}



# Lanzar App
shinyApp(ui = ui, server = server)
