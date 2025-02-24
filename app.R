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

df_demografia <- df$conapo %>% 
  mutate(entidad=case_when(
    entidad=="República Mexicana"~"Nacional",
    T~entidad
  ))

df_egresos_obs <- df$total_causas_obs %>% 
  clean_names() %>% 
  mutate(tipo=case_when(
    tipo=="Parto unespontaneo"~"Parto único espontáneo",
    tipo=="Hemorragia obstetrica"~"Hemorragia",
    tipo=="Edema proteinuria y trastornos hipertensivos en el embarazo"~"Enfermedad hipertensiva del embarazo",
    T~tipo
  ))

 
# Muertes maternas -------------------------------------------------------------

df_muertes<-df$muertes_maternas %>% 
  clean_names() 


# ----------------------------------------------------------------------------

df_apeo <- df$apeo %>% 
  clean_names() %>% 
  filter(!ano==2019)

df_nacimiento<-df$nacimientos

df_nacimiento_nacional <- df$nacimientos %>% 
  group_by(ano) %>% 
  summarise(total=sum(total)) %>% 
  cbind(entidad=c("Nacional")) %>% 
  select(entidad, total, ano)

df_nacimiento <- bind_rows(df_nacimiento, df_nacimiento_nacional)

# violencia--------------------------------------------------------------------
df_violencia <- df$secretariado 

df_violencia<-df_violencia %>% 
  group_by(fecha,delito) %>% 
  summarise(total=sum(total)) %>% 
  cbind(entidad=c("Nacional")) %>% 
  select(fecha, entidad, delito, total) %>% 
  rbind(df_violencia)


# enadid-----------------------------------------------------------------------
enadid <- df$enadid

enadid <- enadid %>% 
  filter(edad>=10)

# endadid_fecundidad -------
enadid_fecundidad <- df$enadid_fecundidad

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
                                                  
                                                  # selectInput("demografia_grupo_edad", "Selecciona un rango de edad:", 
                                                  #             choices = unique(df_demografia$grupo_edad), multiple = T, selected = "Todos las edades"),
                                                  
                                                  selectInput("demografia_sexo", "Selecciona el sexo:", 
                                                              choices = unique(df_demografia$sexo), multiple = TRUE, selected = c("Hombres", "Mujeres")),
                                                  
                                                  selectInput("demografia_entidad", "Selecciona una entidad:", 
                                                              choices = unique(df_demografia$entidad), 
                                                              multiple = TRUE, 
                                                              selected = setdiff(unique(df_demografia$entidad), "Nacional"),
                                                              selectize = TRUE)
                                     ),
                                     mainPanel(width = 9,
                                               plotOutput("gr_demografia"),
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
                                                              selected = "Nacional")
                                     ),
                                     mainPanel(plotOutput("gr_demografia_edad"),
                                               br(),
                                               DT::dataTableOutput("tabla_demografia_edad")  # Tabla interactiva
                                     )),
                            tabPanel("Por año", br(),
                                     
                                     sidebarPanel(width = 3, br(),
                                           selectInput("demografia_ano_ano", "Selecciona un año:", 
                                                       choices = unique(df_demografia$ano), 
                                                       multiple = TRUE, 
                                                       selected = unique(df_demografia$ano)),
                                           
                                           selectInput("demografia_sexo_ano", "Selecciona el sexo:", 
                                                       choices = unique(df_demografia$sexo), 
                                                       multiple = TRUE, 
                                                       selected = c("Hombres", "Mujeres")),
                                           
                                           selectInput("demografia_entidad_ano", "Selecciona una entidad:", 
                                                       choices = unique(df_demografia$entidad), 
                                                       multiple = F, 
                                                       selected = "Nacional")
                                     ),
                                     mainPanel(plotOutput("gr_demografia_ano"),
                                               br(),
                                               DT::dataTableOutput("tabla_demografia_ano")  # Tabla interactiva
                                     )
                                     )
                            )),
                 
                 #  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  
                 
                 
                 tabPanel("Nacimientos", br(),
                        tabsetPanel(
                          tabPanel("Entidad", br(),
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
                            plotOutput("gr_nacimiento"), br(),br(),
                            DT::dataTableOutput("tabla_nacimiento")  # Agrega la tabla aquí
                            
                          )),
                          
                          tabPanel("Año", br(),
                                   sidebarPanel(width = 3, br(),
                                                selectInput("nacimiento_ano_ano", "Selecciona un año:", 
                                                            choices = unique(df_nacimiento$ano), 
                                                            selected = c(df_nacimiento$ano), multiple = T),
                                                selectInput("nacimiento_entidad_ano", "Selecciona una entidad:",
                                                            choices = unique(df_nacimiento$entidad),
                                                            multiple = F, 
                                                            selected = unique(df_nacimiento$entidad))
                                   ),
                                   
                                   mainPanel(
                                     plotOutput("gr_nacimiento_ano"), br(),br(),
                                     DT::dataTableOutput("tabla_nacimiento_ano")  # Agrega la tabla aquí
                                     
                                   )),
                          
                          tabPanel("Tasas global de fecundidad", br(),
                                   sidebarPanel(width = 3,
                                                selectInput("enadid_año", "Selecciona un año:", 
                                                            choices = c(2018, 2024), 
                                                            selected = 2024, multiple = F),
                                                
                                                # selectInput("violencia_delito", "Selecciona el o los delitos:", 
                                                #             choices = unique(df_violencia$delito), multiple = TRUE,
                                                #             selected = c("Violencia familiar", "Abuso sexual", "Violación")),
                                                # 
                                                selectInput("enadid_entidad", "Selecciona una entidad:",
                                                            choices = sort(unique(enadid_fecundidad$nom_ent)),
                                                            multiple = TRUE,
                                                            selected = unique(enadid_fecundidad$nom_ent),
                                                            selectize = TRUE)#,
                                                # selectInput("enadid_edad", "Selecciona grupos de edad:",
                                                #             choices = sort(unique(enadid_fecundidad$grupo_edad)),
                                                #             multiple = TRUE,
                                                #             selected = unique(enadid_fecundidad$grupo_edad),
                                                #             selectize = TRUE)
                                   ),
                                   mainPanel(width = 9,
                                             plotOutput("gr_tasa_fecundidad"),
                                             br(),
                                             # plotOutput("gr_prom_abortos"),
                                             # br(),
                                             # h3("Tabla de indicador de fecundidad de los últimos 6 años"),
                                             DT::dataTableOutput("tabla_fecundidad"), br(), 
                                             # h3("Tabla de indicador de abortos"),
                                             # DT::dataTableOutput("tabla_abortos")
                                             # plotOutput("gr_uso_anti_primera_vez"),
                                             # br(),
                                             # plotOutput("gr_uso_anticonceptivo")
                                   ))
                          
                          
                        )),
                 #  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  
                 
                 tabPanel("Egresos Obstétricos", br(),
                          tabsetPanel(
                          tabPanel("Egresos",br(),
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
                            plotOutput("grafico_egresos"), br(),br(),
                            DT::dataTableOutput("tabla_egresos")  # Agrega la tabla aquí
                            
                          )),
                          tabPanel("Año", br(),
                                   sidebarPanel(width = 3, br(),
                                                selectInput("egresos_ano_ano", "Selecciona un año:", 
                                                            choices = unique(df_egresos_obs$ano), 
                                                            multiple = T,
                                                            selected = unique(df_egresos_obs$ano)),
                                                selectInput("egresos_unidad_medica_ano", "Selecciona una entidad:",
                                                            choices = unique(df_egresos_obs$unidad_medica),
                                                            multiple = F,
                                                            selected = c("Nacional")),
                                                
                                                selectInput("egresos_grupo_edad_ano", "Selecciona un grupo de edad:", 
                                                            choices = unique(df_egresos_obs$gr_edad), 
                                                            selected = "Todas las edades")#,
                                                
                                                # selectInput("egresos_tipo_ano", "Selecciona una causa:", 
                                                #             choices = unique(df_egresos_obs$tipo), 
                                                #             multiple = T, selected = df_egresos_obs$tipo)
                                   ),
                                   mainPanel(
                                     plotOutput("grafico_egresos_ano"), br(),br(),
                                     DT::dataTableOutput("tabla_egresos_ano")  # Agrega la tabla aquí
                                     
                                   )),
                          tabPanel("Entidad", br(),
                                   sidebarPanel(width = 3, br(),
                                                selectInput("egresos_ano_entidad", "Selecciona un año:", 
                                                            choices = unique(df_egresos_obs$ano), 
                                                            multiple = F,
                                                            selected = unique(df_egresos_obs$ano)),
                                                selectInput("egresos_unidad_medica_entidad", "Selecciona una entidad:",
                                                            choices = unique(df_egresos_obs$unidad_medica),
                                                            multiple = T,
                                                            selected = c("Nacional", "Aguascalientes")),
                                                
                                                selectInput("egresos_grupo_edad_entidad", "Selecciona un grupo de edad:", 
                                                            choices = unique(df_egresos_obs$gr_edad), 
                                                            selected = "Todas las edades")#,
                                                
                                                # selectInput("egresos_tipo_entidad", "Selecciona una causa:", 
                                                #             choices = unique(df_egresos_obs$tipo), 
                                                #             multiple = T, selected = df_egresos_obs$tipo)
                                   ),
                                   mainPanel(
                                     plotOutput("grafico_egresos_entidad"), br(),br(),
                                     DT::dataTableOutput("tabla_egresos_entidad")  # Agrega la tabla aquí
                                     
                                   ))
                          )),
                 
                 #  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  
                 
                 tabPanel("Muertes maternas", br(),
                          tabsetPanel(
                            tabPanel("Total",br(),
                          sidebarPanel(width = 3, br(),
                                       selectInput("muertes_ano", "Selecciona un año:", 
                                                   choices = unique(df_muertes$ano), 
                                                   selected = max(df_muertes$ano), multiple = F),
                                       selectInput("muertes_entidad", "Selecciona una entidad:",
                                                   choices = unique(df_muertes$entidad),
                                                   multiple = TRUE, 
                                                   selected = unique(df_muertes$entidad))#,
                                       # selectInput("muertes_edad", "Selecciona un grupo de edad:", 
                                       #             choices = unique(df_muertes$gr_edad), 
                                       #             selected = c("Todas Edades", "Menores 20"), multiple = T)
                          ),
                          
                          mainPanel(
                            plotOutput("gr_muertes"), br(),br(),
                            DT::dataTableOutput("tabla_muertes")  # Agrega la tabla aquí
                            
                          )),
                          tabPanel("Mortalidad materna (pendiente)",br()))),
                 #  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  
                 
                 tabPanel("APEO", br(),
                          tabsetPanel(
                            tabPanel("Tipo",br(),
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
                                                   selected = "Todas las edades")#,
                                       
                                       # selectInput("apeo_tipo", "Selecciona una causa:", 
                                       #             choices = unique(df_apeo$tipo), 
                                       #             multiple = T, selected = df_apeo$tipo)
                          ),
                          mainPanel(
                            plotOutput("grafico_apeo"), br(),br(),
                            DT::dataTableOutput("tabla_apeo")  # Agrega la tabla aquí
                            
                          )),
                          
                          
                        tabPanel("Año",br(),
                                  sidebarPanel(width = 3, br(),
                                               selectInput("apeo_ano_ano", "Selecciona un año:", 
                                                           choices = unique(df_apeo$ano),
                                                           multiple = T,
                                                           selected = unique(df_apeo$ano)),
                                               selectInput("apeo_unidad_medica_ano", "Selecciona una entidad:",
                                                           choices = unique(df_apeo$unidad_medica),
                                                           multiple = F,
                                                           selected = c("Nacional")),
                                               
                                               selectInput("apeo_grupo_edad_ano", "Selecciona un grupo de edad:", 
                                                           choices = unique(df_apeo$gr_edad), 
                                                           selected = "Todas las edades")#,
                                               
                                               # selectInput("apeo_tipo_ano", "Selecciona una causa:",
                                               #             choices = unique(df_apeo$tipo),
                                               #             multiple = T, selected = df_apeo$tipo)
                                  ),
                                  mainPanel(
                                    plotOutput("grafico_apeo_ano"), br(),br(),
                                    DT::dataTableOutput("tabla_apeo_ano")  # Agrega la tabla aquí
                                    
                                  )), 
                        
                        tabPanel("Entidad",br(),
                                 sidebarPanel(width = 3, br(),
                                              selectInput("apeo_ano_entidad", "Selecciona un año:", 
                                                          choices = unique(df_apeo$ano),
                                                          multiple = F,
                                                          selected = unique(df_apeo$ano)),
                                              selectInput("apeo_unidad_medica_entidad", "Selecciona una entidad:",
                                                          choices = unique(df_apeo$unidad_medica),
                                                          multiple = T,
                                                          selected = c("Nacional", "Aguascalientes")),
                                              
                                              selectInput("apeo_grupo_edad_entidad", "Selecciona un grupo de edad:", 
                                                          choices = unique(df_apeo$gr_edad), 
                                                          selected = "Todas las edades"),
                                              
                                              selectInput("apeo_tipo_entidad", "Selecciona una causa:",
                                                          choices = unique(df_apeo$tipo),
                                                          multiple = T, selected = df_apeo$tipo)
                                 ),
                                 mainPanel(
                                   plotOutput("grafico_apeo_entidad"), br(),br(),
                                   DT::dataTableOutput("tabla_apeo_entidad")  # Agrega la tabla aquí
                                   
                                 ))
                        )),
                 
                 tabPanel("Violencias", br(),
                          tabsetPanel(
                            tabPanel("Entidad", br(),
                          sidebarPanel(width = 3,
                                       sliderInput("violencia_fecha", "Selecciona un año:", 
                                                   min = min(df_violencia$fecha), max =max(df_violencia$fecha), 
                                                   value = 2024, step = 1),
                                       
                                       # selectInput("violencia_delito", "Selecciona el o los delitos:", 
                                       #             choices = unique(df_violencia$delito), multiple = TRUE,
                                       #             selected = c("Violencia familiar", "Abuso sexual", "Violación")),
                                       # 
                                       selectInput("violencia_entidad", "Selecciona una entidad:", 
                                                   choices = unique(df_violencia$entidad), 
                                                   multiple = TRUE, 
                                                   selected = setdiff(unique(df_violencia$entidad), "Nacional"),
                                                   selectize = TRUE)
                          ),
                          mainPanel(width = 9,
                                    plotOutput("gr_violencia"),
                                    br(),
                                    DT::dataTableOutput("tabla_violencia")
                          )),
                        tabPanel("Año",
                                 tabPanel("Año", br(),
                                          sidebarPanel(width = 3,
                                                       selectInput("violencia_fecha_ano", "Selecciona un año:", 
                                                                   choices = sort(unique(na.omit(df_violencia$fecha)), decreasing = TRUE),
                                                                   multiple = T,
                                                                   selected = unique(df_violencia$fecha)  # ✅ Año más reciente
                                                       ),
                                                       
                                                       # selectInput("violencia_delito_ano", "Selecciona el o los delitos:", 
                                                       #             choices = unique(df_violencia$delito), 
                                                       #             multiple = TRUE,
                                                       #             selected = unique(df_violencia$delito)),
                                                       
                                                       selectInput("violencia_entidad_ano", "Selecciona una entidad:", 
                                                                   choices = unique(df_violencia$entidad), 
                                                                   multiple = F, 
                                                                   selected = c("Nacional"),
                                                                   selectize = TRUE)
                                          ),
                          mainPanel(width = 9,
                                    plotOutput("gr_violencia_ano"),
                                    br(),
                                    DT::dataTableOutput("tabla_violencia_ano")
                          ))))),
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
                                               plotOutput("gr_uso_anti_primera_vez"),
                                               br(),
                                               plotOutput("gr_uso_anticonceptivo"),
                                               br(), 
                                               # h3("Tabla de indicador de uso de anticonceptivo en su primera relación sexual"),
                                               DT::dataTableOutput("tabla_uso_anti_primera_vez"), br(), 
                                               # h3("Tabla de indicador de uso actual de anticonceptivos"),
                                               DT::dataTableOutput("tabla_uso_anticonceptivo")
                                     )
                            ),
                            # tabPanel("Indicadores",
                            #          sidebarPanel(width = 3,
                            #                       selectInput("enadid_año", "Selecciona un año:", 
                            #                                   choices = c(2018, 2024), 
                            #                                   selected = 2024, multiple = F),
                            #                       
                            #                       # selectInput("violencia_delito", "Selecciona el o los delitos:", 
                            #                       #             choices = unique(df_violencia$delito), multiple = TRUE,
                            #                       #             selected = c("Violencia familiar", "Abuso sexual", "Violación")),
                            #                       # 
                            #                       selectInput("enadid_entidad", "Selecciona una entidad:",
                            #                                   choices = sort(unique(enadid$nom_ent)),
                            #                                   multiple = TRUE,
                            #                                   selected = unique(enadid$nom_ent),
                            #                                   selectize = TRUE),
                            #                       selectInput("enadid_edad", "Selecciona grupos de edad:",
                            #                                   choices = sort(unique(enadid$grupo_edad)),
                            #                                   multiple = TRUE,
                            #                                   selected = unique(enadid$grupo_edad),
                            #                                   selectize = TRUE)
                            #          ),
                            #          mainPanel(width = 9,
                            #                    plotOutput("gr_tasa_fecundidad"),
                            #                    br(),
                            #                    plotOutput("gr_prom_abortos"),
                            #                    br(),
                            #                    # h3("Tabla de indicador de fecundidad de los últimos 6 años"),
                            #                    DT::dataTableOutput("tabla_fecundidad"), br(), 
                            #                    # h3("Tabla de indicador de abortos"),
                            #                    DT::dataTableOutput("tabla_abortos")
                            #                    # plotOutput("gr_uso_anti_primera_vez"),
                            #                    # br(),
                            #                    # plotOutput("gr_uso_anticonceptivo")
                            #          )), 
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
                                               plotOutput("gr_uso_anti_primera_vez_tiempo"),
                                               br(),
                                               plotOutput("gr_uso_anticonceptivo_tiempo"),
                                               br()#,
                                               # plotOutput("gr_tasa_fecundidad_tiempo"),
                                               # br(),
                                               # plotOutput("gr_prom_abortos_tiempo")
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
      filter(entidad %in% input$demografia_entidad,
             # grupo_edad %in% input$demografia_grupo_edad,
             ano >= input$demografia_ano,
             sexo %in% input$demografia_sexo
      ) 
  })
  
  # Gráfico por entidad
  output$gr_demografia <- renderPlot({
    
    demografia_reactive() %>%
      group_by(entidad, sexo) %>% 
      summarise(poblacion = sum(poblacion),.groups = 'drop') %>% 
      group_by(entidad) %>% 
      mutate(total_entidad = sum(poblacion),
             porcentaje = poblacion / total_entidad) %>%
      ungroup() %>% 
      ggplot(aes(x = reorder(entidad, -total_entidad), y = total_entidad, fill = sexo)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = paste0(round(porcentaje * 100, 1), "%")), 
                position = position_stack(vjust = 0.5), size = 3, color = "white") +  
      labs(x = "", y = "", fill = "",
           title = "Población total por sexo y entidad de residencia habitual",
           subtitle = paste("Año:", input$demografia_ano),
           caption = paste0("Fuente: elaboración propia con base en los datos de estimaciones de la población de CONAPO | Ipas Lac",
                            "\n Fecha de consulta: ", Sys.Date())) +
      scale_y_continuous(labels = scales::comma) +
      theme_minimal() +
      theme_ipas+
      theme(
        legend.position = "bottom",
        axis.text.x = element_text(size = 13, color = "black", angle = 90, hjust = 1)
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
  
  output$gr_demografia_edad <- renderPlot({
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
        porcentaje_texto = paste0(round(porcentaje, 1), "%"),  # Etiqueta en porcentaje
        label_pos = ifelse(sexo == "Hombres", poblacion - max(abs(poblacion)) * 0.05, poblacion + max(abs(poblacion)) * 0.05) # Ajustar posición
      ) %>% 
      ggplot(aes(x = grupo_edad, y = poblacion, fill = sexo)) +
      geom_bar(stat = "identity") +
      geom_text(aes(y = label_pos, label = porcentaje_texto, hjust = ifelse(sexo == "Hombres", 1.1, -0.1)), 
                size = 3.5, 
                color = "black") +
      scale_y_continuous(labels = abs) +
      coord_flip() +
      labs(
        x = "", y = "", fill = "",
        title = "Pirámide poblacional por grupo de edad y sexo",
        subtitle = paste("Año:", input$demografia_ano_edad,
                         "\nEntidad:", input$demografia_entidad_edad),
        caption = paste0("Fuente: elaboración propia con base en los datos de estimaciones de la población de CONAPO | Ipas Lac",
                                  "\n Fecha de consulta: ", Sys.Date())
      ) +
      scale_y_continuous(labels = scales::comma) +
      theme_minimal() +
      theme_ipas +
      theme(
        legend.position = "bottom",
        axis.text.x = element_text(size = 10, color = "black", angle = 0, hjust = 1)
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
  
# Demografía año ---------------------------------------------------------------  
  
  demografia_reactive_ano <- reactive({
    df_demografia %>%
      filter(
        # if (is.null(input$demografia_grupo_ano) || input$demografia_grupo_ano == "Todos las edades") TRUE else entidad %in% input$demografia_grupo_ano,
        entidad %in% input$demografia_entidad_ano,
        ano %in% input$demografia_ano_ano
        # sexo %in% input$demografia_sexo_ano
      ) 
  })
  
  # Gráfico por entidad y año
  output$gr_demografia_ano <- renderPlot({
    req(demografia_reactive_ano())  # Asegura que los datos están disponibles
    
    demografia_reactive_ano() %>% 
      group_by(ano, entidad, sexo) %>% 
      summarise(poblacion = sum(poblacion), .groups = 'drop') %>% 
      group_by(ano) %>% 
      mutate(
        total_poblacion_ano = sum(poblacion),   # Suma total de población por año
        porcentaje = (poblacion / total_poblacion_ano) * 100,  # Calcula el porcentaje
        porcentaje_texto = paste0(round(porcentaje, 1), "%"),  # Etiqueta en porcentaje
        etiqueta_texto = paste0(scales::comma(poblacion), "\n (", porcentaje_texto, ")") # Total + porcentaje
      ) %>% 
      ungroup() %>% 
      ggplot(aes(x = as.factor(ano), y = poblacion, fill = sexo)) +
      geom_bar(stat = "identity") +
      
      # Etiquetas dentro de cada barra con el total y porcentaje
      geom_text(aes(label = etiqueta_texto), 
                position = position_stack(vjust = 0.5), 
                size =4, 
                color = "white") +
      
      # Agregar total en la parte superior de cada barra de año
      geom_text(aes(y = total_poblacion_ano, label = scales::comma(total_poblacion_ano)), 
                vjust = -0.5, 
                size = 4, 
                fontface = "bold") +
      
      labs(
        x = "", y = "", fill = "",
        title = "Total de la población por año y sexo",
        subtitle = paste("Año:", input$demografia_ano_edad,
                         "\nEntidad:", input$demografia_entidad_edad),
        caption = paste0("Fuente: elaboración propia con base en los datos de estimaciones de la población de CONAPO | Ipas Lac",
                         "\n Fecha de consulta: ", Sys.Date())
      ) +
      scale_y_continuous(labels = scales::comma) + # Formato con comas para miles
      scale_x_discrete(expand = expansion(mult = c(0, 0.05))) +  # Eliminar espacio extra en X
      theme_minimal() +
      theme_ipas +
      theme(
        legend.position = "bottom",
        axis.text.x = element_text(size = 13, color = "black", angle=0, hjust = 0.5)
      )
  })
  
  
  
  output$tabla_demografia_ano <- DT::renderDataTable({

 demografia_reactive_ano() %>% 
      # df_demografia %>% 
      group_by(ano, entidad, sexo) %>% 
      summarise(poblacion = sum(poblacion), .groups = 'drop') %>% 
      group_by(ano) %>% 
      mutate(
        total_poblacion_ano = sum(poblacion),   # Suma total de población por año
        porcentaje = (poblacion / total_poblacion_ano) * 100,  # Calcula el porcentaje
        porcentaje = round(porcentaje, 2)  # Redondear a 2 decimales
      ) %>% 
      ungroup() %>% 
      pivot_wider(
        names_from = "sexo",
        values_from = c("poblacion", "porcentaje")
      ) %>%
      arrange(desc(poblacion_Hombres + poblacion_Mujeres)) %>%   # Ordenar por población total
      select(-entidad) %>% 
    datatable(
      extensions = 'Buttons',
      options = list(
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
        pageLength = 5,
        autoWidth = TRUE
      ),
      rownames = FALSE,
      colnames = c("Año", "Total","Hombres", "Mujeres", "% Hombres", "% Mujeres")
    ) %>%
      formatRound(columns = c(2, 3, 4), digits = 0) %>%  # Población sin decimales
      formatRound(columns = c(5, 6), digits = 1)  # Porcentaje con 1 decimal
  })

  
#Egresos -------------------------------------------------------------------------
  
  egresos_reactive <- reactive({
    df_egresos_obs %>% 
      filter(gr_edad == input$egresos_grupo_edad,
             unidad_medica %in% input$egresos_unidad_medica,
             ano == input$egresos_ano)
    
  })
  
  output$grafico_egresos <- renderPlot({

    egresos_reactive() %>%
      group_by(ano, unidad_medica, gr_edad, tipo) %>% 
      summarise(total = sum(total), .groups = "drop") %>% 
      group_by(unidad_medica) %>% 
      mutate(
        porcentaje = total / sum(total) * 100,
        etiqueta = paste0(tipo, "\n", scales::comma(total), " (", round(porcentaje, 1), "%)")
      ) %>% 
      ungroup() %>% 
    ggplot(aes(area = total, fill = tipo, label = etiqueta, subgroup = unidad_medica)) +
      geom_treemap() +
      geom_treemap_text(fontface = "bold", color = "white", place = "left", grow = F, fontface = "plain") +  # Etiquetas en el centro
      # scale_fill_brewer(palette = "Set3") +  # Paleta de colores atractiva
      labs(
        title = "Egresos hospitalarios por unidad médica, grupo de edad y tipo",
        subtitle = paste("Año:", input$egresos_ano),  # Año dinámico
        fill = "",
        caption = paste0("\nFuente: Elaboración propia con datos de la Secretaría de Salud | Ipas Lac",
                         "\nFecha de consulta: ", Sys.Date())) +
      theme_minimal() +
      theme_ipas +
      theme(
        legend.position = "bottom")
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
      formatRound(columns = "porcentaje", digits = 1)  # Formato de porcentaje
  })
  
  
  
  #Egresos año-------------------------------------------------------------------------
  
  egresos_reactive_ano <- reactive({
    df_egresos_obs %>% 
      filter(gr_edad == input$egresos_grupo_edad_ano,
             unidad_medica %in% input$egresos_unidad_medica_ano,
             ano %in% input$egresos_ano_ano)
    
  })
  
  output$grafico_egresos_ano <- renderPlot({
    egresos_reactive_ano() %>%
      group_by(ano, tipo) %>%  # Agrupamos por año y tipo de egreso
      summarise(total = sum(total), .groups = "drop") %>% 
      group_by(ano) %>% 
      mutate(
        total_ano = sum(total),  # Total de egresos en cada año
        porcentaje = (total / total_ano) * 100,  # Calculamos el porcentaje
        porcentaje_text = paste0(round(porcentaje, 1), "%")  # Formato de porcentaje
      ) %>% 
      ungroup() %>% 
      ggplot(aes(x = as.factor(ano), y = porcentaje, fill = tipo)) +
      geom_col() + 
      geom_text(aes(label = porcentaje_text), 
                position = position_stack(vjust = 0.5),  # Centra los textos en cada barra apilada
                size = 4, color="white",
                fontface = "plain")+
      labs(
        title = "Egresos hospitalarios por año y tipo",
        subtitle = paste("Año:", input$egresos_ano_ano),  # Año dinámico
        fill = "", x="", y="",
        caption = paste0("\nFuente: Elaboración propia con datos de la Secretaría de Salud | Ipas Lac",
                         "\nFecha de consulta: ", Sys.Date())) +
      scale_y_continuous(labels = scales::percent_format(scale = 1)) +  # Eje Y en porcentaje
      theme_minimal() +
      theme_ipas +
      theme(
        legend.position = "bottom",
        legend.text = element_text(size = 8),  # Reducir tamaño de la leyenda
        axis.text.x = element_text(size = 13, color = "black", angle = 0, hjust = 1)
      ) 
  })
  
  
  
  output$tabla_egresos_ano <- DT::renderDataTable({
    egresos_reactive_ano() %>%
      group_by(ano, tipo) %>%  # Agrupamos por año y tipo de egreso
      summarise(total = sum(total), .groups = "drop") %>% 
      group_by(ano) %>% 
      mutate(
        total_ano = sum(total),  # Total de egresos en cada año
        porcentaje = (total / total_ano) * 100,  # Calculamos el porcentaje
        porcentaje_text = paste0(round(porcentaje, 1), "%")  # Formato de porcentaje
      ) %>% 
      select(ano, tipo, total, porcentaje) %>% 
      datatable(
        extensions = 'Buttons',
        options = list(
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
          pageLength = 8,
          autoWidth = TRUE
        ),
        rownames = FALSE,
        colnames = c("Año", "Tipo", "Total", "Porcentaje") 
      ) %>%
      formatRound(columns = "total", digits = 0) %>%  # Formato sin decimales en total
      formatRound(columns = "porcentaje", digits = 1)  # Formato de porcentaje
  })
  
  
  #Egresos entidad-------------------------------------------------------------------------
  
  egresos_reactive_entidad <- reactive({
    df_egresos_obs %>% 
      filter(gr_edad == input$egresos_grupo_edad_entidad,
             unidad_medica %in% input$egresos_unidad_medica_entidad,
             ano %in% input$egresos_ano_entidad)
    
  })
  
  output$grafico_egresos_entidad <- renderPlot({
    egresos_reactive_entidad() %>%
      group_by(unidad_medica, ano, tipo) %>%  # Agrupamos por entidad y tipo de egreso
      summarise(total = sum(total), .groups = "drop") %>% 
      group_by(unidad_medica) %>%  # Calculamos el total por entidad
      mutate(
        total_entidad = sum(total),  # Total de egresos en cada unidad médica
        porcentaje = (total / total_entidad) * 100,  # Calculamos el porcentaje
        porcentaje_text = paste0(round(porcentaje, 1), "%")  # Formato de porcentaje
      ) %>% 
      ungroup() %>%
      ggplot(aes(x = as.factor(unidad_medica), y = porcentaje, fill = tipo)) +
      geom_bar(stat = "identity", position = "stack") +
      geom_text(aes(label = porcentaje_text), 
                position = position_stack(vjust = 0.5),  # Centra los textos en cada barra apilada
                size = 4, color="white",
                fontface = "plain")+
      labs(
        title = "Egresos hospitalarios por entdad y tipo",
        subtitle = paste("Año:", input$egresos_ano_entidad),  # Año dinámico
        fill = "", x="", y="",
        caption = paste0("\nFuente: Elaboración propia con datos de la Secretaría de Salud | Ipas Lac",
                         "\nFecha de consulta: ", Sys.Date())) +
      scale_y_continuous(labels = scales::percent_format(scale = 1)) +  # Eje Y en porcentaje
      theme_minimal() +
      theme_ipas +
      theme(
        legend.position = "bottom",
        legend.text = element_text(size = 8),  # Reducir tamaño de la leyenda
        axis.text.x = element_text(size = 13, color = "black", angle = 0, hjust = 1)
      )
    
  })
  
  output$tabla_egresos_entidad <- DT::renderDataTable({
    egresos_reactive_entidad() %>%
      group_by(unidad_medica, ano, tipo) %>%  # Agrupamos por entidad y tipo de egreso
      summarise(total = sum(total), .groups = "drop") %>% 
      group_by(unidad_medica) %>%  # Calculamos el total por entidad
      mutate(
        total_entidad = sum(total),  # Total de egresos en cada unidad médica
        porcentaje = (total / total_entidad) * 100,  # Calculamos el porcentaje
        porcentaje_text = paste0(round(porcentaje, 1), "%")  # Formato de porcentaje
      ) %>% 
      ungroup() %>%
      select(unidad_medica, ano, tipo, total, porcentaje) %>% 
      datatable(
        extensions = 'Buttons',
        options = list(
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
          pageLength = 8,
          autoWidth = TRUE
        ),
        rownames = FALSE,
        colnames = c("Entidad", "Año", "Tipo", "Total", "Porcentaje") 
      ) %>%
      formatRound(columns = "total", digits = 0) %>%  # Formato sin decimales en total
      formatRound(columns = "porcentaje", digits = 1)  # Formato de porcentaje
  })
  
  
  
  
  # Muertes maternas ------------------------------------------------------------  
  
  muertes_reactive <- reactive({
    df_muertes %>% 
      filter(ano == input$muertes_ano,
             entidad %in% input$muertes_entidad,
             # if (is.null(input$muertes_edad) || input$muertes_edad == "Todas las edades") TRUE else gr_edad %in% input$muertes_edad,
      )
    
  })
  
  output$gr_muertes <- renderPlot({
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
      ggplot(aes(
        x = reorder(entidad, -total),
        y = total,
        fill = gr_edad,
        color = gr_edad,
        group = gr_edad
      )) +
      geom_line(linewidth = 1.5) +
      geom_point(size = 2.5) +
      labs(
        x = "", y = "", fill = "", group="", color="",
        title = paste0("Muertes maternas por entidad y grupo de edad, "),
        subtitle = paste0("Año: ", input$muertes_ano),
        caption = paste0("Fuente: elaboración propia con base en los datos de la Secretaría de Salud | Ipas Lac",
                         "\nFecha de consulta: ", Sys.Date())
      ) +
      scale_y_continuous(labels = scales::comma) +
      theme_minimal() +
      theme_ipas +
      theme(
        legend.position = "bottom",
        axis.text.x = element_text(size = 13, color = "black", angle = 90, hjust = 1)
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
  
  
  
# APEO tipo  ------------------------------------------------------------------------
  
  apeo_reactive <- reactive({
    df_apeo %>% 
      filter(gr_edad == input$apeo_grupo_edad,
             # tipo %in% input$apeo_tipo,
             unidad_medica %in% input$apeo_unidad_medica,
             ano == input$apeo_ano)
  })
  
  output$grafico_apeo <- renderPlot({
   apeo_reactive() %>%
      group_by(ano, unidad_medica, gr_edad, tipo) %>% 
      summarise(total = sum(total), .groups = "drop") %>% 
      mutate(
        porcentaje = total / sum(total)  # Mantiene como numérico para cálculo
      ) %>% 
      ggplot(aes(area = total, fill = tipo, label = paste0(tipo, "\n", scales::percent(porcentaje, accuracy = 0.1)))) +
      geom_treemap() +
      geom_treemap_text(fontface = "plain", colour = "white", place = "left", grow = F) +
      # scale_fill_brewer(palette = "Set3") +  # Mejora la paleta de colores
      facet_wrap(~ unidad_medica) +  # Faceta por unidad médica
      labs(
        title = "Distribución de APEO por tipo",
        subtitle = paste("Año:", input$apeo_ano, "| Grupo de edad:", input$apeo_grupo_edad),
        fill = "", x="", y="",
        caption = paste0("Fuente: Secretaría de Salud | Ipas Lac \nFecha de consulta: ", Sys.Date())
      ) +
      theme_minimal() +
      theme_ipas
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
      formatRound(columns = "porcentaje", digits = 1)  # Formato de porcentaje
  })
  

  
  # APEO año  ------------------------------------------------------------------------
  
  apeo_reactive_ano <- reactive({
    df_apeo %>% 
      filter(gr_edad == input$apeo_grupo_edad_ano,
             # tipo %in% input$apeo_tipo_ano,
             unidad_medica %in% input$apeo_unidad_medica_ano,
             ano %in% input$apeo_ano_ano)
  })
  
  # Generar el gráfico
  output$grafico_apeo_ano <- renderPlot({
    apeo_reactive_ano() %>%
      group_by(ano, tipo) %>%  
      summarise(total = sum(total), .groups = "drop") %>% 
      group_by(ano) %>% 
      mutate(
        total_ano = sum(total),
        porcentaje = (total / total_ano) * 100,  
        porcentaje_text = paste0(round(porcentaje, 1), "%")  # Formatear como porcentaje
      ) %>% 
      ungroup() %>% 
      ggplot(aes(x = as.factor(ano), y = total, fill = tipo, group = tipo)) +
      geom_col(stat="identity") +  
      geom_text(aes(label = porcentaje_text), 
                position = position_stack(vjust = 0.5),  # Centra los textos en cada barra apilada
                size = 4, color="white",
                fontface = "plain")+
      labs(
        title = "Distribución de APEO por año",
        subtitle = paste("Año:", input$apeo_ano_ano, "\nGrupo de edad:", input$apeo_grupo_edad_ano,
                         "\nEntidad:", input$apeo_unidad_medica_ano),
        fill = "", x="", y="",
        caption = paste0("Fuente: Secretaría de Salud | Ipas Lac \nFecha de consulta: ", Sys.Date())
      ) +
      scale_y_continuous(labels = scales::percent_format(scale = 1)) +  
      theme_minimal() +
      theme_ipas +
      theme(
        legend.position = "bottom",
        legend.text = element_text(size = 8), 
        axis.text.x = element_text(size = 13, color = "black", angle = 0, hjust = 0.5)
      ) 
  })
  
  
  output$tabla_apeo_ano <- DT::renderDataTable({
    apeo_reactive_ano() %>% 
      group_by(ano, tipo) %>%  
      summarise(total = sum(total), .groups = "drop") %>% 
      group_by(ano) %>% 
      mutate(
        total_ano = sum(total),
        porcentaje = (total / total_ano) * 100) %>% 
      datatable(
        extensions = 'Buttons',
        options = list(
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
          pageLength = 10,
          autoWidth = TRUE
        ),
        rownames = FALSE,
        colnames = c("Año","Tipo", "Total","Total año", "Porcentaje:")
      ) %>%
      formatRound(columns = "total", digits = 0) %>%  # Formato sin decimales en total
      formatRound(columns = "porcentaje", digits = 1)  # Formato de porcentaje
  })
  
  
  
  # APEO entidad  ------------------------------------------------------------------------
  
  apeo_reactive_entidad <- reactive({
    df_apeo %>%
      filter(gr_edad == input$apeo_grupo_edad_entidad,
             # tipo %in% input$apeo_tipo,
             unidad_medica %in% input$apeo_unidad_medica_entidad,
             ano %in% input$apeo_ano_entidad)
  })

  # Generar el gráfico
  output$grafico_apeo_entidad <- renderPlot({
    apeo_reactive_entidad() %>%
      group_by(unidad_medica, ano, tipo) %>%
      summarise(total = sum(total), .groups = "drop") %>%
      group_by(unidad_medica) %>%  
      mutate(
        total_entidad = sum(total),  
        porcentaje = (total / total_entidad) * 100,  
        porcentaje_text = paste0(round(porcentaje, 1), "%")  
      ) %>%
      ungroup() %>%
      
      ggplot(aes(x = reorder(unidad_medica, -total_entidad), y = porcentaje, fill = tipo)) +
      geom_col(stat = "identity") +
      geom_text(aes(label = porcentaje_text), 
               position = position_stack(vjust = 0.5),  # Centra los textos en cada barra apilada
               size = 4, color="white",
               fontface = "plain")+
      labs(
        title = "Distribución de APEO entidad y por año",
        subtitle = paste("Año:", input$apeo_ano_entidad, "\nGrupo de edad:", input$apeo_grupo_edad_entidad),
        fill = "", x="", y="",
        caption = paste0("Fuente: Secretaría de Salud | Ipas Lac \nFecha de consulta: ", Sys.Date())
      ) +
      scale_y_continuous(labels = scales::percent_format(scale = 1)) +
      theme_minimal() +
      theme_ipas +
      theme(
        legend.position = "bottom",
        legend.text = element_text(size = 8),
        axis.text.x = element_text(size = 13, color = "black", angle = 0, hjust = 0.5)
      )
  })


  # Tabla de datos asegurando que cada entidad sume 100%
  output$tabla_apeo_entidad <- DT::renderDataTable({
    apeo_reactive_entidad() %>%
      group_by(unidad_medica, ano, tipo) %>%
      summarise(total = sum(total), .groups = "drop") %>%
      group_by(unidad_medica) %>%  # ✅ Normalizar dentro de cada entidad
      mutate(
        total_entidad = sum(total),
        porcentaje = (total / total_entidad) * 100
      ) %>%
      datatable(
        extensions = 'Buttons',
        options = list(
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
          pageLength = 10,
          autoWidth = TRUE
        ),
        rownames = FALSE,
        colnames = c("Entidad", "Año", "Tipo", "Total", "Total en entidad", "Porcentaje (%)")
      ) %>%
      formatRound(columns = "total", digits = 0) %>%  # ✅ Formato sin decimales en total
      formatRound(columns = "porcentaje", digits = 1)  # ✅ Formato de porcentaje
  })


# Nacimientos--------------------------------------------------------------------
  
  nacimiento_reactive <- reactive({
    df_nacimiento %>% 
      filter(ano == input$nacimiento_ano,
             entidad %in% input$nacimiento_entidad
      )
    
  })
  
  output$gr_nacimiento <- renderPlot({
    nacimiento_reactive() %>%
      ggplot(aes(
        x = reorder(entidad, -total),
        y = total,
        fill = total)) +
      geom_col()+
      geom_text(aes(label = scales::comma(total)), angle=90 , 
                hjust = -0.2,  
                size = 4,  
                color = "black") +
      labs(
        x = "", y = "", fill = " ",
        title = paste0("Total de nacimientos por entidad y año"),
        subtitle = paste0("Año: ", input$nacimiento_ano),
        caption = paste0("Fuente: elaboración propia con base en los datos de la Secretaría de Salud | Ipas Lac",
                         " ", Sys.Date())) +
      scale_y_continuous(labels = scales::comma) +
      guides(fill = guide_legend(override.aes = list(size = 10))) + 
      theme_minimal() +
      theme_ipas +
      theme(
        legend.position = "bottom",
        axis.text.x = element_text(size = 13, color = "black", angle = 90, hjust = 1)
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
      formatRound(columns = c(3), digits = 0)  
  })
  
  
  # Nacimientos años ----------------------------------------------------------
  
  nacimiento_reactive_ano <- reactive({
    df_nacimiento %>% 
      filter(ano%in%input$nacimiento_ano_ano,
             entidad%in%input$nacimiento_entidad_ano 
      )
  })
  
  output$gr_nacimiento_ano <- renderPlot({
    nacimiento_reactive_ano() %>%
      ggplot(aes(
        x = as.factor(ano),
        y = total,
        fill = total,
      )) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = scales::comma(total)), angle=0 , 
                hjust = 0.5,  vjust=-0.7,
                size = 4,  
                color = "black") +
      labs(
        x = "", y = "", fill = " ",
        title = paste0("Total de nacimientos por año y entidad"),
        subtitle = paste0("Entidad: ", input$nacimiento_entidad),
        caption = paste0("Fuente: elaboración propia con base en los datos de la Secretaría de Salud | Ipas Lac",
                         "Fecha de consulta: ", Sys.Date())) +
      scale_y_continuous(labels = scales::comma) +
      guides(fill = guide_legend(override.aes = list(size = 10))) + 
      scale_y_continuous(labels = scales::comma) +
      theme_minimal() +
      theme_ipas +
      theme(
        legend.position = "none",
        axis.text.x = element_text(size = 13, color = "black", angle = 0, hjust = 0.5)
      ) 
 
  })
  
  output$tabla_nacimiento_ano <- DT::renderDataTable({
    nacimiento_reactive_ano() %>%
      select(ano, total) %>% 
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
          "Año",
          "Total"
        )
      ) %>%
      # formatRound(columns = c(3,4), digits = 0) %>%
      formatRound(columns = c(2), digits = 0)  
  })
  
  
# tasas de fecundidad:--------------------------------------------------------
  
  enadid_reactive <- reactive({
    enadid_fecundidad %>% 
      filter(
        ano %in% input$enadid_año, 
        #grupo_edad %in% input$enadid_edad, 
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
  

  
  output$gr_tasa_fecundidad <- renderPlot({
    
    # data_nacional <- enadid_reactive() %>% 
    #   summarise(tasa=sum(tasa)/32) %>%
    #   mutate(#tasa=hijos_vivos/mujeres, 
    #          nom_ent="Nacional")
    
    
    enadid_fecundidad %>% 
      filter(
        ano %in% input$enadid_año, 
        #grupo_edad %in% input$enadid_edad, 
         nom_ent %in% input$enadid_entidad
      ) %>% 
      # mutate(hijos_nacidos2 =as.integer(hijos_nacidos2)*factor) %>%
      # group_by(nom_ent) %>%
      # summarise(mujeres=sum(factor),
      #           hijos_vivos=sum(hijos_nacidos2 , na.rm = T)) %>%
      # mutate(tasa=hijos_vivos/mujeres) %>%
      # bind_rows(data_nacional) %>% 
      mutate(col_nacional=ifelse(nom_ent=="Nacional", 1, 0)) %>% 
      ggplot(aes(reorder(nom_ent, -tasa), tasa, 
                 fill=factor(col_nacional)
      )) +
      geom_col() +
      geom_text(aes(label = scales::comma(tasa, .01)), angle=90 , 
                hjust = 1.1,  
                size = 6,  
                color = "black") +
      labs(
        x = "", y = "", fill = " ",
        title = paste0("Tasa de fecundidad de los últimos 6 años por entidad"),
        subtitle = paste0("Año: ", input$enadid_año),
        caption = paste0("Fuente: elaboración propia con base en los datos dela ENADID | Ipas Lac",
                         "\nFecha de consulta: ", Sys.Date())) +
      theme_ipas +
      theme(axis.text.x = element_text(angle = 90), 
            legend.position = "none") +
      scale_fill_manual(values = c("#f9592a", "grey")) +
      labs(x="", y="", fill="") 
   
  })
  
  # output$gr_prom_abortos <- renderPlot({
  #   
  #   data_nacional <- enadid_reactive() %>% 
  #     # filter(edad>=15, edad<=44) %>%
  #     replace_na(list(abortos=0)) %>% 
  #     mutate(abortos=abortos*factor) %>%
  #     summarise(mujeres=sum(factor),
  #               abortos=sum(abortos, na.rm = T)) %>%
  #     mutate(tasa=abortos/mujeres) %>% 
  #     mutate(nom_ent="Nacional") %>% ungroup()
  #   
  #  enadid_reactive() %>% 
  #     # filter(edad>=15, edad<=44) %>%
  #     replace_na(list(abortos=0)) %>% 
  #     mutate(abortos=abortos*factor) %>%
  #     group_by(nom_ent) %>%
  #     summarise(mujeres=sum(factor),
  #               abortos=sum(abortos, na.rm = T)) %>%
  #     mutate(tasa=abortos/mujeres) %>%
  #     bind_rows(data_nacional) %>% 
  #     mutate(tooltip_text = paste0(
  #       "<b>Entidad:</b> ", nom_ent, "<br>",
  #       "<b>Tasa:</b> ", scales::comma(tasa, .01), "<br>"
  #     )) %>%  
  #     
  #     mutate(col_nacional=ifelse(nom_ent=="Nacional", 1, 0)) %>% 
  #     ggplot(aes(reorder(nom_ent, -tasa), tasa, 
  #                text=tooltip_text, fill=factor(col_nacional)
  #     )) +
  #     geom_col(position = "dodge") +
  #     theme_ipas +
  #     theme(axis.text.x = element_text(angle = 90), 
  #           legend.position = "none") +
  #     scale_fill_manual(values = c("#b75dea", "grey")) +
  #     labs(x="", y="", fill="") 
  #   
  #  
  # })
  
  
  # Violencia entidad--------------------------------------------------------------------
  
  violencia_reactive <- reactive({
    df_violencia %>% 
      filter(
        fecha %in% input$violencia_fecha,
        # delito %in% input$violencia_delito,
        entidad %in% input$violencia_entidad
      )
  })
  
  output$gr_violencia <- renderPlot({
    violencia_reactive() %>%
      group_by(fecha, entidad, delito) %>%
      summarise(total = sum(total), .groups = "drop") %>%
      group_by(fecha, entidad) %>%
      mutate(
        total_entidad = sum(total),  # ✅ Total por entidad
        porcentaje = (total / total_entidad) * 100  # ✅ Cada entidad suma 100%
      ) %>%
      ungroup() %>%
      
      ggplot(aes(
        x = reorder(entidad, -total_entidad),  # ✅ Orden por total de la entidad
        y = total,
        fill = delito)) +
      geom_col() +
      geom_text(aes(label = paste0(round(porcentaje * 1, 1), "%")), 
               position = position_stack(vjust = 0.5), size = 3, color = "white") + 
      
      labs(
        x = "", y = "", fill = " ",
        title = paste0("Total de carpetas de investigación por tipo de delito y año, "), 
        subtitle=paste0("\nAño: ",input$violencia_fecha),
        caption = paste0("\nFuente: elaboración propia con base en los datos del SESNSP | Ipas Lac",
                         "\nFecha de consulta: ", Sys.Date())
      ) +
      scale_y_continuous(labels = scales::comma) +
      theme_minimal() +
      theme_ipas +
      theme(
        legend.position = "bottom",
        axis.text.x = element_text(size = 12, color = "black", angle = 90, hjust = 1),
        legend.text = element_text(size=9),
      ) 
  })
  
  # Tabla asegurando que cada entidad sume 100% y agregando total por entidad
  output$tabla_violencia <- DT::renderDataTable({
    datos <- violencia_reactive() %>%
      group_by(fecha, entidad, delito) %>%
      summarise(total = sum(total), .groups = "drop") %>%
      group_by(fecha, entidad) %>%
      mutate(
        total_entidad = sum(total),  # ✅ Total de delitos por entidad
        porcentaje = (total / total_entidad) * 100  # ✅ Normalizar para que sume 100%
      ) %>%
      ungroup()
    
    # Agregar la fila de totales por entidad
    datos_totales <- datos %>%
      group_by(fecha, entidad) %>%
      summarise(
        delito = "TOTAL",
        total = sum(total),
        total_entidad = sum(total_entidad) / n_distinct(delito),  # Promedio del total
        porcentaje = 100,  # ✅ Cada entidad suma 100%
        .groups = "drop"
      )
    
    # Unir los datos de delitos con los totales por entidad
    datos_finales <- bind_rows(datos, datos_totales) %>%
      arrange(fecha, entidad, desc(delito != "TOTAL"))  # ✅ Ordenar dejando "TOTAL" al final
    
    datatable(
      datos_finales,
      extensions = 'Buttons',
      options = list(
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
        pageLength = 10,
        autoWidth = TRUE
      ),
      rownames = FALSE,
      colnames = c("Año", "Entidad", "Delito", "Total", "Total por entidad", "Porcentaje (%)")
    ) %>%
      formatRound(columns = c("total", "total_entidad"), digits = 0) %>%
      formatRound(columns = "porcentaje", digits = 1)
  })

  
  # Violencia año--------------------------------------------------------------------
  
  violencia_reactive_ano <- reactive({
    df_violencia %>% 
      filter(
        fecha %in% input$violencia_fecha_ano,
        # delito %in% input$violencia_delito,
        entidad %in% input$violencia_entidad_ano
      )
  })
  
  output$gr_violencia_ano <- renderPlot({
    violencia_reactive_ano() %>%
      group_by(fecha, entidad, delito) %>%
      summarise(total = sum(total), .groups = "drop") %>%
      group_by(fecha) %>%
      mutate(
        total_anual = sum(total),  # ✅ Total de delitos por año
        porcentaje = (total / total_anual) * 100  # ✅ Cada año suma 100%
      ) %>%
      ungroup() %>%
      ggplot(aes(
        x = as.factor(fecha),  # ✅ Los años en el eje X
        y = total,
        fill = delito
        )) +
      geom_col() +
      geom_text(aes(label = paste0(round(porcentaje * 1, 1), "%")), 
               position = position_stack(vjust = 0.5), size = 3, color = "white") + 
      labs(
        x = "", y = "", fill = "",
        title = paste0("Total de carpetas de investigación por tipo de delito y entidad"),
        subtitle = paste0("\nEntidad: ", input$violencia_entidad_ano),
        caption = paste0("Fuente: elaboración propia con base en los datos del SESNSP | Ipas Lac",
                         "\nFecha de consulta: ", Sys.Date())
      ) +
      scale_y_continuous(labels = scales::comma) +
      theme_minimal() +
      theme_ipas +
      theme(
        legend.position = "bottom",
        axis.text.x = element_text(size = 9, color = "black", angle = 0, hjust = 1),
        legend.text = element_text(size=9),
      ) 
  })
  
  output$tabla_violencia_ano <- DT::renderDataTable({
    datos <- violencia_reactive_ano() %>%
      group_by(fecha, entidad, delito) %>%
      summarise(total = sum(total), .groups = "drop") %>%
      group_by(fecha) %>%
      mutate(
        total_anual = sum(total), 
        porcentaje = (total / total_anual) * 100  
      ) %>%
      ungroup()
    
    # Agregar la fila de totales por año
    datos_totales <- datos %>%
      group_by(fecha) %>%
      summarise(
        entidad = "TOTAL",
        delito = "TOTAL",
        total = sum(total),
        total_anual = sum(total_anual) / n_distinct(delito),
        porcentaje = 100,  
        .groups = "drop"
      )
    
    # Unir los datos de delitos con los totales por año
    datos_finales <- bind_rows(datos, datos_totales) %>%
      arrange(fecha, entidad, desc(delito != "TOTAL")) 
    
    datatable(
      datos_finales,
      extensions = 'Buttons',
      options = list(
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
        pageLength = 10,
        autoWidth = TRUE
      ),
      rownames = FALSE,
      colnames = c("Año", "Entidad", "Delito", "Total", "Total por año", "Porcentaje (%)")
    ) %>%
      formatRound(columns = c("total", "total_anual"), digits = 0) %>%
      formatRound(columns = "porcentaje", digits = 1)
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
  
  output$gr_uso_anti_primera_vez <- renderPlot({
    
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
      labs(x="", y="") +
      geom_text(data=. %>% 
                  filter(Porcentaje>0.05),
                aes(label=scales::percent(Porcentaje, 1)), 
                position = position_stack(vjust = .8), size=5
      ) +
      theme_ipas +
      theme(axis.text.x = element_text(angle = 90), 
            legend.position = "none") +
      scale_y_continuous(labels = percent)
    
    gr_tasa
    
  })
  
  
  output$gr_uso_anticonceptivo <- renderPlot({
    
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
                 text=tooltip_text, fill=tipo, grouo=tipo
      )) +
      geom_col() +
      labs(x="", y="") +
      geom_text(data=. %>% 
                  filter(Porcentaje>0.05),
                  aes(label=scales::percent(Porcentaje, 1)), 
                position = position_stack(vjust = .8), size=5
                ) +
      theme_ipas +
      theme(axis.text.x = element_text(angle = 90), 
            legend.position = "none") +
      scale_y_continuous(labels = percent)
    
    gr_tasa
    
  })
  
  #####
  #en el tiempo #
  output$gr_tasa_fecundidad_tiempo <- renderPlot({
    
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
      labs(x="", y="", fill="")+
      theme_ipas +
      geom_text(aes(label=tasa)) +
      theme(#axis.text.x = element_text(angle = 90), 
        legend.position = "none") +
      scale_fill_manual(values = c("#f9592a", "grey")) +
      labs(x="", y="", fill="")+
      theme(axis.text.x = element_text(angle = 0))
    
    gr_tasa
    
  })
  
  # output$gr_prom_abortos_tiempo <- renderPlot({
  #   
  #   # data_nacional <- 
  #   #   total_enadid %>%
  #   #   filter(grupo_edad %in% input$enadid_edad3) %>% 
  #   #   replace_na(list(abortos=0)) %>% 
  #   #   mutate(abortos=abortos*factor) %>%
  #   #   group_by(ano) %>% 
  #   #   summarise(mujeres=sum(factor),
  #   #             abortos=sum(abortos, na.rm = T)) %>%
  #   #   mutate(tasa=abortos/mujeres) %>% 
  #   #   mutate(nom_ent="Nacional") %>% ungroup()
  #   
  #   gr_tasa <- enadid_reactive3() %>% 
  #     # filter(edad>=15, edad<=44) %>%
  #     replace_na(list(abortos=0)) %>% 
  #     mutate(abortos=abortos*factor) %>%
  #     group_by(nom_ent, ano) %>%
  #     summarise(mujeres=sum(factor),
  #               abortos=sum(abortos, na.rm = T)) %>%
  #     mutate(tasa=abortos/mujeres) %>%
  #     # bind_rows(data_nacional) %>% 
  #     mutate(tooltip_text = paste0(
  #       "<b>Entidad:</b> ", nom_ent, "<br>",
  #       "<b>Tasa:</b> ", scales::comma(tasa, .01), "<br>", 
  #       "<b>Año:</b> ", ano, "<br>"
  #     )) %>%  
  #     
  #     mutate(col_nacional=ifelse(nom_ent=="Nacional", 1, 0)) %>% 
  #     ggplot(aes(factor(ano), tasa, 
  #                text=tooltip_text, fill=factor(ano)
  #     )) +
  #     geom_col(position = "dodge") +
  #     labs(x="", y="", fill="")+
  #     
  #     theme_ipas +
  #     theme(#axis.text.x = element_text(angle = 90), 
  #       legend.position = "none") +
  #     scale_fill_manual(values = c("#b75dea", "grey")) +
  #     labs(x="", y="", fill="") +
  #     theme(axis.text.x = element_text(angle = 0))
  #   
  # })
  
  output$gr_uso_anti_primera_vez_tiempo <- renderPlot({
    
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
      mutate(tipo=str_wrap(tipo, 5),
             as.factor(ano)) %>% 
      ggplot(aes(x=reorder(tipo, -Porcentaje), Porcentaje, 
                 text=tooltip_text, fill=factor(ano)
      )) +

      geom_col(position = "dodge") +
      geom_text(aes(label=percent(Porcentaje, 1)), 
                position = position_dodge(width = 1), 
                size=8
      ) +
      labs(x="", y="", fill="") +
      theme_ipas +
      theme(axis.text.x = element_text(angle = 40, size = 9), 
            legend.position = "bottom") +
      scale_y_continuous(labels = percent) #+
    
    gr_tasa
    
  })
  
  
  output$gr_uso_anticonceptivo_tiempo <- renderPlot({
   
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
      ggplot(aes(reorder(tipo, -Porcentaje), Porcentaje, 
                 text=tooltip_text, fill=factor(ano)
      )) +

      geom_col(position = "dodge") +
      geom_text(aes(label=percent(Porcentaje, 1)), 
                position = position_dodge(width = 1), 
                size=8
      ) +
      labs(x="", y="", fill="") +
      theme_ipas +
      theme(axis.text.x = element_text(angle = 40, size = 10), 
            legend.position = "bottom") +
      scale_y_continuous(labels = percent) #  +
    
    gr_tasa

  })
  
  #####
  #tablas 
  output$tabla_fecundidad <- DT::renderDataTable({
    # tabla_nacional <- enadid %>% 
    #   filter(edad>=15, edad<=44) %>%
    #   mutate(hijos_nacidos2 =as.integer(hijos_nacidos2)*factor) %>%
    #   group_by(ano) %>%
    #   summarise(mujeres=sum(factor),
    #             hijos_vivos=sum(hijos_nacidos2 , na.rm = T)) %>%
    #   mutate(tasa=hijos_vivos/mujeres, 
    #          nom_ent="Nacional")
    # 
    # tabla_estatal <- enadid_reactive() %>% 
    #   filter(edad>=15, edad<=44) %>%
    #   mutate(hijos_nacidos2 =as.integer(hijos_nacidos2)*factor) %>%
    #   group_by(nom_ent, ano) %>%
    #   summarise(mujeres=sum(factor),
    #             hijos_vivos=sum(hijos_nacidos2 , na.rm = T)) %>%
    #   mutate(tasa=hijos_vivos/mujeres) %>%
    #   bind_rows(tabla_nacional)
    
    enadid_fecundidad %>%
      # arrange(desc(rowSums(across(where(is.numeric))))) %>%
      arrange(desc(tasa)) %>%
      mutate(tasa=comma(tasa, .01)) %>% 
      datatable(
        extensions = 'Buttons',
        options = list(
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
          pageLength = 10,
          autoWidth = TRUE
        ),
        rownames = FALSE,
        # colnames = c("Entidad", "Año", "Total de mujeres",
        #              "Hijos vivos", "Tasa"), 
        colnames = c("Año", "Entidad", "Tasa"), 
        caption = htmltools::tags$caption(
          style = 'font-size: 25px; color: black;', # Personaliza el tamaño y color
          "Tabla de indicador de fecundidad de los últimos 6 años"
        )
      )
    
    
    
    
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
      mutate(Porcentaje=uso_metodos/sum(uso_metodos)*100) %>% 
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
      mutate(Porcentaje=uso_metodos/sum(uso_metodos)*100) %>% 
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