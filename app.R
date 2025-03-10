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
library(RColorBrewer)


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

paleta<-c("#328781","#414487ff","#2a788eff","#7ad151ff","#44016aff","#fde725ff",
          "#22a855af", "#bf642c", "#872f65", "#872626","#968306")

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
    entidad=="Ciudad De México"~"Ciudad de México",
    entidad=="Mexico"~"Estado de México",
    T~entidad
  ))
# ---------------------------------------------------------------------------

df_egresos_obs <- df$total_causas_obs %>% 
  clean_names() %>% 
  mutate(tipo=case_when(
    tipo=="Parto unespontaneo"~"Parto único espontáneo",
    tipo=="Hemorragia obstetrica"~"Hemorragia",
    tipo=="Edema proteinuria y trastornos hipertensivos en el embarazo"~"Enfermedad hipertensiva del embarazo",
    T~tipo),
    unidad_medica=case_when(
      unidad_medica=="Mexico"~"Estado de México",
      T~unidad_medica
    ))

 
# Muertes maternas -------------------------------------------------------------

df_muertes<-df$muertes_maternas %>% 
  clean_names() %>% 
  mutate(gr_edad = ifelse(is.na(gr_edad), 0, gr_edad),
         entidad=case_when(
    entidad=="México"~"Estado de México",
    T~entidad))


df_muertes <- df_muertes %>% 
  group_by(ano,gr_edad) %>% 
  summarise(total=sum(total, na.rm=T)) %>% 
  cbind(entidad="Nacional") %>% 
  select(entidad, ano, gr_edad, total) %>% 
  rbind(df_muertes) %>% 
  pivot_wider(
    names_from = "gr_edad",
    values_from = "total"
  ) %>% 
  mutate(`Mayor e igual a 20`= (`Todas Edades`-`Menores 20`)) %>% 
  select(-`Todas Edades`) %>% 
  pivot_longer(cols=3:4,
               names_to = "gr_edad",
               values_to = "total")


# APEO-------------------------------------------------------------------------

df_apeo <- df$apeo %>% 
  clean_names() %>% 
  filter(!ano==2019) 

# nacimiento ------------------------------------------------------------------
df_nacimiento<-df$nacimientos %>% 
  mutate(entidad=case_when(
    entidad=="Mexico"~"Estado de México",
    T~entidad
  ))

df_nacimiento_nacional <- df$nacimientos %>% 
  group_by(ano) %>% 
  summarise(total=sum(total)) %>% 
  cbind(entidad=c("Nacional")) %>% 
  select(entidad, total, ano)

df_nacimiento <- bind_rows(df_nacimiento, df_nacimiento_nacional)

# violencia--------------------------------------------------------------------
df_violencia <- df$secretariado %>% 
  mutate(entidad=case_when(
    entidad=="México"~"Estado de México",
    T~entidad
  ))

df_violencia<-df_violencia %>% 
  group_by(fecha,delito) %>% 
  summarise(total=sum(total)) %>% 
  cbind(entidad=c("Nacional")) %>% 
  select(fecha, entidad, delito, total) %>% 
  rbind(df_violencia)


# enadid-----------------------------------------------------------------------
enadid <- df$enadid %>% 
  mutate(nom_ent=case_when(
    nom_ent=="México"~"Estado de México",
    T~nom_ent
  ))

enadid <- enadid %>% 
  filter(edad>=10)

# endadid_fecundidad -----------------------------------------------------------

enadid_fecundidad <- df$enadid_fecundidad

# endadid_fecundidad especifica -----------------------------------------------------------
enadid_fecundidad_especifica <- df$enadid_fecundidad_especifica

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
  .dt-head-rotate {
    text-align: left !important;
    white-space: nowrap;
    transform: rotate(90deg);
    transform-origin: left bottom;
    height: 60px;
    vertical-align: bottom;
  }

.rotate-text {
    text-align: left !important;
    white-space: nowrap;
    transform: rotate(90deg);
    transform-origin: left bottom;
    height: 100px;
}
  
 table.dataTable tbody td {
    font-size: 10px !important;
  }
  table.dataTable thead th {
    font-size: 10px !important;
  }
              
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
                            tabPanel("Por año", br(),
                                     
                                     sidebarPanel(width = 3, br(),
                                                  selectInput("demografia_ano_ano", "Selecciona un año:", 
                                                              choices = unique(df_demografia$ano), 
                                                              multiple = TRUE, 
                                                              selected = unique(df_demografia$ano)),
                                                  
                                                  # selectInput("demografia_sexo_ano", "Selecciona el sexo:", 
                                                  #             choices = unique(df_demografia$sexo), 
                                                  #             multiple = TRUE, 
                                                  #             selected = c("Hombres", "Mujeres")),
                                                  
                                                  selectInput("demografia_entidad_ano", "Selecciona una entidad:", 
                                                              choices = unique(df_demografia$entidad), 
                                                              multiple = F, 
                                                              selected = "Nacional")
                                     ),
                                     mainPanel(plotOutput("gr_demografia_ano"),
                                               br(),
                                               DT::dataTableOutput("tabla_demografia_ano")  # Tabla interactiva
                                     )
                            ),
                            tabPanel("Por entidad", br(),   
                                     sidebarPanel(width = 3,
                                                  sliderInput("demografia_ano", "Selecciona un año:", 
                                                              min = min(df_demografia$ano), max =max(df_demografia$ano), 
                                                              value = 2024, step = 1),
                                                  
                                                  # selectInput("demografia_grupo_edad", "Selecciona un rango de edad:", 
                                                  #             choices = unique(df_demografia$grupo_edad), multiple = T, selected = "Todos las edades"),
                                                  
                                                  # selectInput("demografia_sexo", "Selecciona el sexo:", 
                                                  #             choices = unique(df_demografia$sexo), multiple = TRUE, selected = c("Hombres", "Mujeres")),
                                                  
                                                  # selectInput("demografia_entidad", "Selecciona una entidad:", 
                                                  #             choices = unique(df_demografia$entidad), 
                                                  #             multiple = TRUE, 
                                                  #             selected = setdiff(unique(df_demografia$entidad), "Nacional"),
                                                  #             selectize = TRUE)
                                                  checkboxInput("toggle", "Seleccionar/deseleccionar todo", value = T),
                                                  awesomeCheckboxGroup(
                                                    inputId = "demografia_entidad",
                                                    label = "Selecciona una entidad:",
                                                    choices = unique(df_demografia$entidad),  # Incluye todas las opciones
                                                    selected = unique(df_demografia$entidad)[unique(df_demografia$entidad) != "Nacional"],  # Excluye "Nacional" de la selección
                                                    status = "warning"
                                                  )
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
                                     ))

                            )),
                 
                 #  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  
                 
                 
                 tabPanel("Nacimientos", br(),
                        tabsetPanel(
                          tabPanel("Por año", br(),
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
                          tabPanel("Por entidad", br(),
                          sidebarPanel(width = 3, br(),
                                       selectInput("nacimiento_ano", "Selecciona un año:", 
                                                   choices = unique(df_nacimiento$ano), 
                                                   selected = max(df_nacimiento$ano), multiple = F),
                                       checkboxInput("toggle_nac", "Seleccionar/deseleccionar todo", value = TRUE),
                                       
                                       awesomeCheckboxGroup(
                                         inputId = "nacimiento_entidad",
                                         label = "Selecciona una entidad:",
                                         
                                         # Reordena las opciones colocando "Nacional" al inicio
                                         choices = c("Nacional", setdiff(unique(df_nacimiento$entidad), "Nacional")), 
                                         
                                         # Preselecciona todas excepto "Nacional"
                                         selected = setdiff(unique(df_nacimiento$entidad), "Nacional"),  
                                         
                                         status = "warning"
                                       )
                          ),
                          
                          mainPanel(
                            plotOutput("gr_nacimiento"), br(),br(),
                            DT::dataTableOutput("tabla_nacimiento")  # Agrega la tabla aquí
                            
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
                                                # selectInput("enadid_entidad", "Selecciona una entidad:",
                                                #             choices = sort(unique(enadid_fecundidad$nom_ent)),
                                                #             multiple = TRUE,
                                                #             selected = unique(enadid_fecundidad$nom_ent),
                                                #             selectize = TRUE)#,
                                                # selectInput("enadid_edad", "Selecciona grupos de edad:",
                                                #             choices = sort(unique(enadid_fecundidad$grupo_edad)),
                                                #             multiple = TRUE,
                                                #             selected = unique(enadid_fecundidad$grupo_edad),
                                                #             selectize = TRUE)
                                                checkboxInput("toggle_fec", "Seleccionar/deseleccionar todo", value = TRUE),
                                                
                                                awesomeCheckboxGroup(
                                                  inputId = "fecundidad_entidad",
                                                  label = "Selecciona una entidad:",
                                                  
                                                  # Reordena las opciones colocando "Nacional" al inicio
                                                  choices = c("Nacional", setdiff(unique(enadid_fecundidad$nom_ent), "Nacional")), 
                                                  
                                                  # Preselecciona todas excepto "Nacional"
                                                  selected = setdiff(unique(enadid_fecundidad$nom_ent), "Nacional"),  
                                                  
                                                  status = "warning"
                                                )
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
                                   )), 
                          tabPanel("Tasas especifica de fecundidad", br(),
                                   sidebarPanel(width = 3,
                                                selectInput("enadid_edad", "Selecciona un año:", 
                                                            choices = unique(enadid_fecundidad_especifica$tipo), 
                                                            selected = unique(enadid_fecundidad_especifica$tipo)[1], multiple = F),
                                                
                                                # selectInput("violencia_delito", "Selecciona el o los delitos:", 
                                                #             choices = unique(df_violencia$delito), multiple = TRUE,
                                                #             selected = c("Violencia familiar", "Abuso sexual", "Violación")),
                                                # 
                                                # selectInput("enadid_entidad", "Selecciona una entidad:",
                                                #             choices = sort(unique(enadid_fecundidad$nom_ent)),
                                                #             multiple = TRUE,
                                                #             selected = unique(enadid_fecundidad$nom_ent),
                                                #             selectize = TRUE)#,
                                                # selectInput("enadid_edad", "Selecciona grupos de edad:",
                                                #             choices = sort(unique(enadid_fecundidad$grupo_edad)),
                                                #             multiple = TRUE,
                                                #             selected = unique(enadid_fecundidad$grupo_edad),
                                                #             selectize = TRUE)
                                                checkboxInput("toggle_fec2", "Seleccionar/deseleccionar todo", value = TRUE),
                                                
                                                awesomeCheckboxGroup(
                                                  inputId = "fecundidad_entidad2",
                                                  label = "Selecciona una entidad:",
                                                  
                                                  # Reordena las opciones colocando "Nacional" al inicio
                                                  choices = c("Nacional", setdiff(unique(enadid_fecundidad_especifica$nom_ent), "Nacional")), 
                                                  
                                                  # Preselecciona todas excepto "Nacional"
                                                  selected = setdiff(unique(enadid_fecundidad_especifica$nom_ent), "Nacional"),  
                                                  
                                                  status = "warning"
                                                )
                                   ),
                                   mainPanel(width = 9,
                                             plotOutput("gr_tasa_fecundidad_esp"),
                                             br(),
                                             # plotOutput("gr_prom_abortos"),
                                             # br(),
                                             # h3("Tabla de indicador de fecundidad de los últimos 6 años"),
                                             DT::dataTableOutput("tabla_fecundidad_esp"), br(), 
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
                            tabPanel("Por año", br(),
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
                                       plotOutput("gr_egresos_ano", height = 650), br(),br(),
                                       DT::dataTableOutput("tabla_egresos_ano")  # Agrega la tabla aquí
                                       
                                     )),
                            tabPanel("Por entidad", br(),
                                     sidebarPanel(width = 3, br(),
                                                  selectInput("egresos_ano_entidad", "Selecciona un año:", 
                                                              choices = unique(df_egresos_obs$ano), 
                                                              multiple = F,
                                                              selected = unique(df_egresos_obs$ano)),

                                                  
                                                  selectInput("egresos_grupo_edad_entidad", "Selecciona un grupo de edad:", 
                                                              choices = unique(df_egresos_obs$gr_edad), 
                                                              selected = "Todas las edades"),
                                                  
                                                  tags$p("Etiquetas de datos", style = "font-weight: bold;"),
                                                  checkboxInput("mostrar_etiquetas", "Mostrar / quitar etiquetas", value = TRUE),
                                                  tags$p("Entidades", style = "font-weight: bold;"),
                                                  checkboxInput("toggle_entidades", "Todas las entidades", value = F),
                                                  checkboxGroupInput("egresos_unidad_medica_entidad", 
                                                                     label = NULL,
                                                                     choices = unique(df_egresos_obs$unidad_medica),
                                                                     selected = c("Nacional", "Aguascalientes"))
                                     ),
                                     mainPanel(
                                       plotOutput("gr_egresos_entidad"), br(),br(),
                                       DT::dataTableOutput("tabla_egresos_entidad")  # Agrega la tabla aquí
                                       
                                     )),
                          tabPanel("Por tipo",br(),
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
                                      
                          ),
                          mainPanel(
                            plotOutput("gr_egresos"), br(),br(),
                            DT::dataTableOutput("tabla_egresos")  # Agrega la tabla aquí
                            
                          ))
                          
                          )),
                 
                 #  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  
                 
                 tabPanel("Muertes maternas", br(),
                          tabsetPanel(
                            tabPanel("Por año",br(),
                          sidebarPanel(width = 3, br(),
                                       selectInput("muertes_ano_ano", "Selecciona un año:", 
                                                   choices = unique(df_muertes$ano), 
                                                   selected = unique(df_muertes$ano), multiple = T),
                                      
                                        # selectInput("muertes_ano_edad", "Selecciona grupo de edad:",
                                        #            choices = unique(df_muertes$gr_edad),
                                        #            selected = unique(df_muertes$gr_edad), multiple = T),

                                       selectInput("muertes_ano_entidad", "Selecciona una entidad:",
                                                   choices = unique(df_muertes$entidad),
                                                   selected = c("Nacional"), multiple = F)
                          ),
                          
                          mainPanel(
                            plotOutput("gr_muertes_ano"), br(),br(),
                            DT::dataTableOutput("tabla_muertes_ano")  # Agrega la tabla aquí
                            
                          )),
                          
                           tabPanel("Por entidad",br(),
                                    sidebarPanel(width = 3, br(),
                                                 selectInput("muertes_ano", "Selecciona un año:", 
                                                             choices = unique(df_muertes$ano), 
                                                             selected = max(df_muertes$ano), multiple = F),
                                                 checkboxInput("toggle_muertes", "Seleccionar/deseleccionar todo", value = TRUE),  # Botón para seleccionar todo
                                                 checkboxGroupInput("muertes_entidad", "Selecciona una entidad:",
                                                                    choices = unique(df_muertes$entidad),
                                                                    selected = "Nacional") 
                                                 # selectInput("muertes_edad", "Selecciona un grupo de edad:", 
                                                 #             choices = unique(df_muertes$gr_edad), 
                                                 #             selected = c("Mayor e igual a 20", "Menores 20"), multiple = T)
                                    ),
                                    
                                    mainPanel(
                                      plotOutput("gr_muertes"), br(),br(),
                                      DT::dataTableOutput("tabla_muertes")  # Agrega la tabla aquí
                                      
                                    )),
                tabPanel("Mortalidad materna (pendiente)",br()))),
                 #  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  
                 
                 tabPanel("APEO", br(),
                      tabsetPanel(
                        tabPanel("Por año",br(),
                                  sidebarPanel(width = 3, br(),
                                               selectInput("apeo_ano_ano", "Selecciona un año:", 
                                                           choices = unique(df_apeo$ano),
                                                           multiple = T,
                                                           selected = unique(df_apeo$ano)),
                                               selectInput("apeo_unidad_medica_ano", "Selecciona una entidad:",
                                                           choices = unique(df_apeo$unidad_medica),
                                                           multiple = F,
                                                           selected = c("Nacional")),
                                               
                                               selectInput("apeo_atencion_ano", "Selecciona el tipo de atención:", 
                                                           choices = unique(df_apeo$atencion), 
                                                           selected = unique(df_apeo$atencion), multiple = T),
                                               
                                               selectInput("apeo_grupo_edad_ano", "Selecciona un grupo de edad:", 
                                                           choices = unique(df_apeo$gr_edad), 
                                                           selected = "Todas las edades"),
                                               tags$p("Etiquetas de datos", style = "font-weight: bold;"),
                                               checkboxInput("mostrar_etiquetas_apeo", "Mostrar / quitar etiquetas", value = TRUE),
                                               
                                               # selectInput("apeo_tipo_ano", "Selecciona una causa:",
                                               #             choices = unique(df_apeo$tipo),
                                               #             multiple = T, selected = df_apeo$tipo)
                                  ),
                                  mainPanel(
                                    plotOutput("grafico_apeo_ano", height = "600"), br(),br(),
                                    DT::dataTableOutput("tabla_apeo_ano")  # Agrega la tabla aquí
                                    
                                  )), 
                        
                        tabPanel("Por entidad",br(),
                                 sidebarPanel(width = 3, br(),
                                              selectInput("apeo_ano_entidad", "Selecciona un año:", 
                                                          choices = unique(df_apeo$ano),
                                                          multiple = F,
                                                          selected = unique(df_apeo$ano)),
                                              
                                              selectInput("apeo_grupo_edad_entidad", "Selecciona un grupo de edad:", 
                                                          choices = unique(df_apeo$gr_edad), 
                                                          selected = "Todas las edades"),
                                              
                                              selectInput("apeo_atencion_entidad", "Selecciona el tipo de atención:", 
                                                          choices = unique(df_apeo$atencion), 
                                                          selected = unique(df_apeo$atencion), multiple = T),
                                              
                                              
                                              # Venus 
                                              tags$p("Etiquetas de datos", style = "font-weight: bold;"),
                                              checkboxInput("mostrar_etiquetas_apeo_ent", "Mostrar / quitar etiquetas", value = TRUE),
                                              tags$p("Entidades", style = "font-weight: bold;"),
                                              checkboxInput("toggle_entidades_apeo", "Todas las entidades", value = F),
                                              checkboxGroupInput("apeo_unidad_medica_entidad", 
                                                                 label = NULL,
                                                                 choices = unique(df_apeo$unidad_medica),
                                                                 selected = c("Nacional", "Aguascalientes"))
                                   
                                 ),
                                 mainPanel(
                                   plotOutput("grafico_apeo_entidad", height = "600"), br(),br(),
                                   DT::dataTableOutput("tabla_apeo_entidad")  # Agrega la tabla aquí
                                   
                                 )),
                        tabPanel("Por tipo",br(),
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
                                              
                                              selectInput("apeo_atencion", "Selecciona el tipo de atención:", 
                                                          choices = unique(df_apeo$atencion), 
                                                          selected = unique(df_apeo$atencion), multiple = T),
                                              
                                 ),
                                 mainPanel(
                                   plotOutput("grafico_apeo"), br(),br(),
                                   DT::dataTableOutput("tabla_apeo")  
                                 ))
                        )),
                 
                 tabPanel("Violencias", br(),
                          tabsetPanel(
                            tabPanel("Entidad", br(),
                          sidebarPanel(width = 3,
                                       sliderInput("violencia_fecha", "Selecciona un año:", 
                                                   min = min(df_violencia$fecha), max =max(df_violencia$fecha), 
                                                   value = 2024, step = 1),
                                       
                                       tags$p("Etiquetas de datos", style = "font-weight: bold;"),
                                       checkboxInput("mostrar_etiquetas_violencia_1", "Mostrar / quitar etiquetas", value = F),
                                       tags$p("Entidades", style = "font-weight: bold;"),
                                       
                                       checkboxInput("toggle_entidades_violencia_1", "Todas las entidades", value = T),
                                       checkboxGroupInput("violencia_entidad", 
                                                          label = NULL,
                                                          choices = unique(df_violencia$entidad),
                                                          selected = setdiff(unique(df_violencia$entidad), "Nacional"))
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
                                                                   selected = unique(df_violencia$fecha)
                                                       ),
                                                       checkboxInput("mostrar_etiquetas_violencia_2", "Mostrar etiquetas de datos", value = TRUE),
                                                       
                                                       
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
                
# ------------------------------------------------------------------------------      

                 tabPanel("Anticoncepción", br(),
                          tabsetPanel(tabPanel("Anticonceptivos",br(), 
                                     sidebarPanel(width = 3,
                                                  selectInput("enadid_año2", "Selecciona un año:", 
                                                              choices = c(2018, 2024), 
                                                              selected = 2024, multiple = F),
                                                  
                                                  checkboxInput("toggle_entidades_enadid_1", "Todas las entidades", value = T),
                                                  checkboxGroupInput("enadid_entidad2", 
                                                                     label = NULL,
                                                                     choices = c("Nacional", unique(sort(enadid$nom_ent))),
                                                                     selected = c(unique(sort(enadid$nom_ent))))
                                     ),
                                     mainPanel(width = 9,
                                               plotOutput("gr_uso_anti_primera_vez", height = "500"),
                                               br(),
                                               DT::dataTableOutput("tabla_uso_anti_primera_vez"), br(), 
                                               br(),
                                               plotOutput("gr_uso_anticonceptivo", height = "500"),
                                               br(), 
                                               DT::dataTableOutput("tabla_uso_anticonceptivo")
                            )),

                            tabPanel("Comparación temporal",br(),
                                     sidebarPanel(width = 3,
                                                  # selectInput("enadid_edad3", "Selecciona grupos de edad:",
                                                  #             choices = sort(unique(enadid$grupo_edad)),
                                                  #             multiple = TRUE,
                                                  #             selected = unique(enadid$grupo_edad),
                                                  #             selectize = TRUE),
                                                  
                                                  selectInput("enadid_entidad3", "Selecciona una entidad:",
                                                              choices = sort(unique(enadid$nom_ent)),
                                                              multiple = F,
                                                              selected = unique(enadid$nom_ent),
                                                              selectize = TRUE)),
                                     mainPanel(width = 9,
                                               plotOutput("gr_uso_anti_primera_vez_tiempo"),
                                               br(),
                                               DT::dataTableOutput("tabla_uso_anti_primera_vez_tiempo"), br(), 
                                               
                                               plotOutput("gr_uso_anticonceptivo_tiempo"),
                                               br(),
                                               DT::dataTableOutput("tabla_uso_anticonceptivo_tiempo"), br()
                                               # plotOutput("gr_tasa_fecundidad_tiempo"),
                                               # br(),
                                               # plotOutput("gr_prom_abortos_tiempo")
                                     ))))
                 
                 
                 
      ))))

# #-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-


# Server - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -



server <- function(input, output, session) {
  observeEvent(input$toggle, {
    if (input$toggle) {
      # Al marcar, se seleccionan todas las entidades
      updateAwesomeCheckboxGroup(
        session = session,
        inputId = "demografia_entidad",
        selected = unique(df_demografia$entidad)
      )
    } else {
      # Al desmarcar, se deselecciona todo
      updateAwesomeCheckboxGroup(
        session = session,
        inputId = "demografia_entidad",
        selected = character(0)
      )
    }
  })
  
  
  observeEvent(input$toggle_nac, {
    if (input$toggle_nac) {
      # Al marcar, se seleccionan todas las entidades
      updateAwesomeCheckboxGroup(
        session = session,
        inputId = "nacimiento_entidad",
        selected = unique(df_nacimiento$entidad)
      )
    } else {
      # Al desmarcar, se deselecciona todo
      updateAwesomeCheckboxGroup(
        session = session,
        inputId = "nacimiento_entidad",
        selected = character(0)
      )
    }
  })
  
  observeEvent(input$toggle_fec, {
    if (input$toggle_fec) {
      # Al marcar, se seleccionan todas las entidades
      updateAwesomeCheckboxGroup(
        session = session,
        inputId = "fecundidad_entidad",
        selected = unique(enadid_fecundidad$nom_ent)
      )
    } else {
      # Al desmarcar, se deselecciona todo
      updateAwesomeCheckboxGroup(
        session = session,
        inputId = "fecundidad_entidad",
        selected = character(0)
      )
    }
  })
  
  observeEvent(input$toggle_fec2, {
    if (input$toggle_fec2) {
      # Al marcar, se seleccionan todas las entidades
      updateAwesomeCheckboxGroup(
        session = session,
        inputId = "fecundidad_entidad2",
        selected = unique(enadid_fecundidad_especifica$nom_ent)
      )
    } else {
      # Al desmarcar, se deselecciona todo
      updateAwesomeCheckboxGroup(
        session = session,
        inputId = "fecundidad_entidad2",
        selected = character(0)
      )
    }
  })
  
  observeEvent(input$toggle_obs_tipo, {
    if (input$toggle_obs_tipo) {
      # Si el toggle está activado, selecciona todas las opciones
      updateAwesomeCheckboxGroup(
        session = session,
        inputId = "egresos_tipo",
        selected = unique(df_egresos_obs$tipo)
      )
    } else {
      # Si el toggle está desactivado, deselecciona todas las opciones
      updateAwesomeCheckboxGroup(
        session = session,
        inputId = "egresos_tipo",
        selected = character(0)
      )
    }
  })
  
  observeEvent(input$toggle_entidades, {
    if (input$toggle_entidades) {
      updateCheckboxGroupInput(session, 
                               inputId = "egresos_unidad_medica_entidad", 
                               selected = unique(df_egresos_obs$unidad_medica))
    } else {
      updateCheckboxGroupInput(session, 
                               inputId = "egresos_unidad_medica_entidad", 
                               selected = c("Nacional", "Aguascalientes"))
    }
  })
  
  
  observeEvent(input$toggle_muertes, {
    if (input$toggle_muertes) {
      updateCheckboxGroupInput(
        session = session,
        inputId = "muertes_entidad",
        selected = unique(df_muertes$entidad)  # Marca todas las entidades
      )
    } else {
      updateCheckboxGroupInput(
        session = session,
        inputId = "muertes_entidad",
        selected = character(0)  # Deselecciona todas las entidades
      )
    }
  })
  
  observeEvent(input$toggle_entidades_apeo, {
    if (input$toggle_entidades_apeo) {
      updateCheckboxGroupInput(session, 
                               inputId = "apeo_unidad_medica_entidad", 
                               selected = unique(df_apeo$unidad_medica))
    } else {
      updateCheckboxGroupInput(session, 
                               inputId = "apeo_unidad_medica_entidad", 
                               selected = c("Nacional", "Aguascalientes"))
    }
  })
  
  # ---------------------------------------------------------------------------

  
  
  observeEvent(input$toggle_entidades_violencia_1, {
    if (input$toggle_entidades_violencia_1) {
      # Al marcar, se seleccionan todas las entidades
      updateAwesomeCheckboxGroup(
        session = session,
        inputId = "violencia_entidad",
        selected = unique(df_violencia$entidad)
      )
    } else {
      # Al desmarcar, se deselecciona todo
      updateAwesomeCheckboxGroup(
        session = session,
        inputId = "violencia_entidad",
        selected = character(0)
      )
    }
  })
  
  
  
  
  observeEvent(input$toggle_entidades_enadid_1, {
    if (input$toggle_entidades_enadid_1) {
      # Al marcar, se seleccionan todas las entidades
      updateAwesomeCheckboxGroup(
        session = session,
        inputId = "enadid_entidad2",
        selected = unique(sort(enadid$nom_ent))
      )
    } else {
      # Al desmarcar, se deselecciona todo
      updateAwesomeCheckboxGroup(
        session = session,
        inputId = "enadid_entidad2",
        selected = character(0)
      )
    }
  })
  
    
##############################################################################
  
  
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
             ano >= input$demografia_ano
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
           caption = paste0("Fuente: Datos de estimaciones de la población de CONAPO | Ipas Lac",
                            "\n Fecha de consulta: ", Sys.Date())) +
      scale_y_continuous(labels = scales::comma) +
      scale_fill_manual(values = paleta)+
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
        # paging = FALSE,
        paging = FALSE,
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
    demografia_reactive_edad() %>%
      group_by(sexo, grupo_edad) %>% 
      summarise(
        poblacion = sum(poblacion),
        .groups = 'drop'
      ) %>%
      mutate(
        total_global = sum(abs(poblacion)),
        porcentaje = abs(poblacion) / total_global * 100,
        porcentaje_texto = paste0(round(porcentaje, 1), "%"),
        poblacion_sign = ifelse(sexo == "Hombres", -poblacion, poblacion),
        grupo_edad_label = paste("", grupo_edad)  # Formato para la etiqueta
      ) %>%
      ungroup() %>%
      mutate(
        max_val = max(abs(poblacion_sign)),
        label_pos = ifelse(sexo == "Hombres",
                           poblacion_sign - max_val * 0.05,
                           poblacion_sign + max_val * 0.05),
        text_pos = -max(abs(poblacion_sign)) * 1.5  # **Más espacio en la izquierda**
      ) %>%
      select(-max_val, -total_global) %>% 
      ggplot(aes(x = grupo_edad, y = poblacion_sign, fill = sexo)) +
      
      geom_bar(stat = "identity") +
      
      # **Etiquetas de los grupos de edad en el extremo izquierdo con más espacio**
      geom_text(aes(y = text_pos, label = grupo_edad_label), 
                size = 4, 
                fontface = "bold", 
                color = "black",
                hjust = 0) +  # Alineado a la derecha, más separado de las barras
      
      geom_text(aes(y = label_pos, label = porcentaje_texto, 
                    hjust = ifelse(sexo == "Hombres", 1.1, -0.1)), 
                size = 3.5, 
                color = "black") +
      
      coord_flip() +
      labs(
        x = "", y = "", fill = "",
        title = "Pirámide poblacional por grupo de edad, sexo y entidad de residencia habitual",
        subtitle = paste("Año:", input$demografia_ano_edad,
                         "\nEntidad:", input$demografia_entidad_edad, "\n"),
        caption = paste0("Fuente: Datos de estimaciones de la población de CONAPO | Ipas Lac",
                         "\nFecha de consulta: ", Sys.Date())
      ) +
      scale_y_continuous(labels = function(x) scales::comma(abs(x))) +
      scale_x_discrete(position = "left") +  # Coloca los rangos de edad a la izquierda
      
      scale_fill_manual(values=paleta)+
      theme_minimal() +
      theme_ipas +
      theme(
        legend.position = "bottom",
        axis.text.x = element_text(size = 10, color = "black", angle = 0, hjust = 1),
        # **Líneas punteadas de fondo en los subejes X**
        panel.grid.major.x = element_line(color = "gray70", linetype = "dashed", size = 0.5),
        panel.grid.minor.x = element_line(color = "gray85", linetype = "dashed", size = 0.3),
        
        # Mantiene las líneas del eje Y normales
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank()
      )
  })
  
  
  output$tabla_demografia_edad <- DT::renderDataTable({
    demografia_reactive_edad() %>%
      group_by(sexo, grupo_edad) %>% 
      summarise(
        poblacion = sum(poblacion),
        .groups = 'drop'
      ) %>%
      mutate(
        total_global = sum(abs(poblacion)),
        porcentaje = abs(poblacion) / total_global * 100) %>%
      ungroup() %>%
      pivot_wider(names_from = "sexo", values_from = c("poblacion", "porcentaje")) %>% 
      datatable(
        extensions = 'Buttons',
        options = list(
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
          paging = FALSE,
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
        entidad %in% input$demografia_entidad_ano,
        ano %in% input$demografia_ano_ano
        # sexo %in% input$demografia_sexo_ano
      ) 
  })
  
  # Gráfico por entidad y año
  output$gr_demografia_ano <- renderPlot({
    req(demografia_reactive_ano())  # Asegura que los datos están disponibles
    
    datos <- demografia_reactive_ano() %>% 
      group_by(ano, entidad, sexo) %>% 
      summarise(poblacion = sum(poblacion), .groups = 'drop') %>% 
      group_by(ano) %>% 
      mutate(
        total_poblacion_ano = sum(poblacion),   # Suma total de población por año
        porcentaje = (poblacion / total_poblacion_ano) * 100,  # Calcula el porcentaje
        porcentaje_texto = paste0(round(porcentaje, 1), "%"),  # Etiqueta en porcentaje
        etiqueta_texto = paste0(scales::comma(poblacion), "\n (", porcentaje_texto, ")") # Total + porcentaje
      ) %>% 
      ungroup() 
    
    # Obtener el mínimo y máximo año de los datos
    min_ano <- min(datos$ano, na.rm = TRUE)
    max_ano <- max(datos$ano, na.rm = TRUE)
    
    ggplot(datos, aes(x = as.factor(ano), y = poblacion, fill = sexo)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = etiqueta_texto), 
                position = position_stack(vjust = 0.5), 
                size = 4, 
                color = "white") +
      geom_text(aes(y = total_poblacion_ano, label = scales::comma(total_poblacion_ano)), 
                vjust = -0.5, 
                size = 4, 
                fontface = "bold") +
      scale_fill_manual(values=paleta)+
      labs(
        x = "", y = "", fill = "",
        title = "Total de la población por año, sexo y entidad de residencia habitual",
        subtitle = paste("Años:", min_ano, "a", max_ano,
                         "\nEntidad:", input$demografia_entidad_ano),
        caption = paste0("Fuente: Datos de estimaciones de la población de CONAPO | Ipas Lac",
                         "\n Fecha de consulta: ", Sys.Date())) +
      scale_y_continuous(labels = scales::comma) + 
      scale_x_discrete(expand = expansion(mult = c(0, 0.05))) + 
      theme_minimal() +
      theme_ipas +
      theme(legend.position = "bottom",
            axis.text.x = element_text(size = 13, color = "black", angle=0, hjust = 0.5))
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
        paging = FALSE,
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
             # tipo %in% input$egresos_tipo,
             ano == input$egresos_ano)
    
  })
  
  
  output$gr_egresos <- renderPlot({
    egresos_reactive() %>%
      group_by(ano, unidad_medica, gr_edad, tipo) %>% 
      summarise(total = sum(total), .groups = "drop") %>% 
      group_by(unidad_medica) %>% 
      mutate(
        porcentaje = total / sum(total) * 100,
        etiqueta = paste0(scales::comma(total), " (", round(porcentaje, 1), "%)"),
        tipo = str_wrap(tipo, 15)
      ) %>% 
      ungroup() %>% 
      ggplot(aes(x = reorder(tipo, -total), y = total, fill = total)) +
      geom_col(show.legend = F) +
      
      # Ajustar etiquetas para que no se corten
      geom_label(aes(label = etiqueta), color = "white", vjust = 0, size = 4, show.legend = F) + 
      
      scale_fill_gradient(
        high = "#414487ff", 
        low = "#22a855af",
        label = comma
      ) +
      labs(
        title = "Egresos hospitalarios por causas obstétricas por año, entidad y grupo de edad",
        subtitle = paste("Año:", input$egresos_ano,
                         "\nEntidad:", input$egresos_unidad_medica,
                         "\nGrupo de edad:", input$egresos_grupo_edad),  
        fill = "", x = "", y = "", 
        caption = paste0("\nFuente: Datos de la Secretaría de Salud | Ipas Lac",
                         "\nFecha de consulta: ", Sys.Date())
      ) +
      theme_minimal() +
      theme_ipas +
      theme(
        axis.text.x = element_text(size = 13, color = "black", angle = 0, hjust = 0.5),
        legend.position = "bottom"
        ) +
      scale_y_continuous(expand = expansion(mult = 0.15)) +
      
      guides(fill = guide_legend(ncol = 3))
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
          paging = FALSE,
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
  
 
  output$gr_egresos_ano <- renderPlot({
    datos <- egresos_reactive_ano() %>%
      group_by(ano, tipo) %>%  
      summarise(total = sum(total), .groups = "drop") %>% 
      group_by(ano) %>% 
      mutate(
        total_ano = sum(total),  
        porcentaje = (total / total_ano) * 100,  
        porcentaje_text = paste0(round(porcentaje, 1), "%")  
      ) %>% 
      ungroup()
    
    min_ano <- min(datos$ano, na.rm = TRUE)
    max_ano <- max(datos$ano, na.rm = TRUE)
    
    ggplot(datos, aes(x = as.factor(ano), y = porcentaje, fill = tipo)) +
      geom_col() + 
      geom_label(aes(label = porcentaje_text, group = tipo), 
                 position = position_stack(vjust = 0.5),  
                 size = 4, color="white",
                 fontface = "plain", show.legend = F)+
      scale_fill_brewer(palette = "Set2", direction = 1) +
      labs(
        title = "Egresos hospitalarios causas obstétricas, por año, edad y entidad",
        subtitle = paste0("Entidad: ", input$egresos_unidad_medica_ano,
                          "\nPeriodo: ", min_ano, " - ", max_ano,
                          "\nGrupo de edad: ", input$egresos_grupo_edad_ano
                          ),  
        fill = "", x="", y="",
        caption = paste0("\nFuente: Datos de la Secretaría de Salud | Ipas Lac",
                         "\nFecha de consulta: ", Sys.Date())) +
      scale_y_continuous(labels = scales::percent_format(scale = 1)) +  
      theme_minimal() +
      theme_ipas +
      theme(
        legend.position = "bottom",
        legend.text = element_text(size = 8),  
        axis.text.x = element_text(size = 13, color = "black", angle = 0, hjust = 0.5)
      ) +
      guides(fill = guide_legend(ncol = 4))
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
          paging = FALSE,
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
  
  output$gr_egresos_entidad <- renderPlot({
    datos <- egresos_reactive_entidad() %>%
      group_by(unidad_medica, ano, tipo) %>%
      summarise(total = sum(total), .groups = "drop") %>%
      group_by(unidad_medica) %>%
      mutate(
        total_entidad = sum(total),
        porcentaje = (total / total_entidad) * 100,
        porcentaje_text = paste0(round(porcentaje, 1), "%"),
        unidad_medica = factor(unidad_medica, levels = unique(unidad_medica))
      ) %>%
      ungroup()
    
    p <- ggplot(datos, aes(x = unidad_medica, y = porcentaje, fill = tipo)) +
      geom_bar(stat = "identity", position = "stack", show.legend = F) +
      scale_fill_brewer(palette = "Set2", direction = -1) +
      labs(
        title = "Egresos hospitalarios por causas obstétricas por entidad y tipo",
        subtitle = paste("Año:", input$egresos_ano_entidad,
                         "\nGrupo de edad:", input$egresos_grupo_edad_entidad),
        fill = "", x = "", y = "",
        caption = paste0("\nFuente: Datos de la Secretaría de Salud | Ipas Lac",
                         "\nFecha de consulta: ", Sys.Date())
      ) +
      coord_flip() +
      scale_y_continuous(labels = scales::percent_format(scale = 1)) +
      theme_minimal() +
      theme_ipas +
      theme(
        legend.position = "right",
        axis.text.y = element_text(size = 10, color = "black"),
        axis.text.x = element_text(angle = 0)
      ) +
      guides(fill = guide_legend(ncol = 4))
    
    # Agregar etiquetas de datos solo si el checkbox está activado
    if (input$mostrar_etiquetas) {
      p <- p + geom_text(aes(label = porcentaje_text, group = tipo), 
                         position = position_stack(vjust = 0.5),
                         size = 4, color = "white", fontface = "plain")
    }
    
    p  # Devolver el gráfico
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
          paging = FALSE,
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
        `Mayor e igual a 20` = replace_na(`Mayor e igual a 20`, 0),
        porcentaje = ifelse(`Mayor e igual a 20` == 0, 0, (`Menores 20` / `Mayor e igual a 20`) * 100)  # Cálculo correcto
      ) %>%
      ungroup() %>%
      pivot_longer(cols = c(`Mayor e igual a 20`, `Menores 20`), names_to = "gr_edad", values_to = "total") %>%
      ggplot(aes(
        x = reorder(entidad, -total),
        y = total,
        fill = gr_edad,
        color = gr_edad,
        group = gr_edad
      )) +
      geom_line(linewidth = 1.5) +
      geom_point(size = 2.5) +
      scale_fill_manual(values=paleta)+
      scale_color_manual(values=paleta)+
      labs(
        x = "", y = "", fill = "", group="", color="",
        title = paste0("Total de muertes maternas por entidad y grupo de edad, "),
        subtitle = paste0("Año: ", input$muertes_ano),
        caption = paste0("Fuente: Datos de la Secretaría de Salud | Ipas Lac",
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
      select(ano, entidad, `Mayor e igual a 20`, `Menores 20`) %>%
      mutate(
        `Menores 20` = replace_na(`Menores 20`, 0),  # Evita valores NA
        `Mayor e igual a 20` = replace_na(`Mayor e igual a 20`, 0),
        porcentaje = ifelse(`Mayor e igual a 20` == 0, 0, (`Menores 20` / `Mayor e igual a 20`) * 100)  # Cálculo correcto
      ) %>%
      arrange(-`Mayor e igual a 20`) %>%
      datatable(
        extensions = 'Buttons',
        options = list(
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
          paging = FALSE,
          autoWidth = TRUE
        ),
        rownames = FALSE,
        colnames = c(
          "Año", "Entidad",
          "Total de muertes maternas", "Menores de 20",
          "Porcentaje de muertes en menores de 20"
        )
      ) %>%
      formatRound(columns = c(3,4), digits = 0) %>%
      formatRound(columns = c(5), digits = 1)
  })

  
  # muertes maternas por año ---------------------------------------------
  muertes_reactive_ano <- reactive({
    df_muertes %>% 
      filter(ano %in% input$muertes_ano_ano,
             # gr_edad %in% input$muertes_ano_edad,
             entidad %in% input$muertes_ano_entidad
             
             )
  })
  
  output$gr_muertes_ano <- renderPlot({
    
    # req(muertes_reactive_ano())
    # df_muertes_1 <- muertes_reactive_ano()
    
    # Calcular el total por año
    df_total_ano <- muertes_reactive_ano() %>%
      group_by(ano) %>%
      summarise(total_ano = sum(total, na.rm = TRUE), .groups = 'drop')
    
    muertes_reactive_ano() %>%
      group_by(ano, gr_edad) %>%
      summarise(total = sum(total, na.rm=T), .groups = 'drop') %>%
      ggplot(aes(x = factor(ano), y = total)) +  
      geom_col(aes(fill=gr_edad)) +
      geom_label(
        aes(label = comma(total, 1), fill=gr_edad, group = gr_edad),
        position = position_stack(vjust = 0.5),
        size = 4, color="white",
        fontface = "plain", show.legend = F
      ) +
      geom_text(
        data = df_total_ano,
        aes(x = as.factor(ano), y = total_ano, label = comma(total_ano, 1)),
        position= position_stack(vjust = 1.1) ,
        size = 5, color = "black",
        fontface = "bold"
      ) +
      scale_fill_manual(values=paleta) +
      labs(
        title = "Total de muertes maternas por año, edad y entidad",
        subtitle = paste0("Entidad: ", input$muertes_ano_entidad,
                          "\nAño: ", min(input$muertes_ano_ano), " a ", max(input$muertes_ano_ano)
                          # "\nGrupo de edad: ", paste(input$muertes_ano_edad, collapse = " y ")
                          
                          ),
        fill = "", x="", y="",
        caption = paste0("\nFuente: Datos de la Secretaría de Salud | Ipas Lac",
                         "\nFecha de consulta: ", Sys.Date())) +
      scale_y_continuous(labels = scales::comma) +  # Cambio de formato para números en eje Y
      theme_minimal() +
      theme_ipas +
      theme(
        legend.position = "bottom",
        legend.text = element_text(size = 12),
        axis.text.x = element_text(size = 13, color = "black", angle = 0, hjust = 0.5)
      ) +
      guides(fill = guide_legend(ncol = 4))
  })
  
  
  # tablas muertes año---------------------------------------------------------
  
  
  output$tabla_muertes_ano <- DT::renderDataTable({
    muertes_reactive_ano() %>%
      # df_muertes %>% 
      group_by(ano, entidad, gr_edad) %>%
      summarise(total = sum(total), .groups = "drop") %>%
      group_by(ano) %>%
      mutate(
        total_ano = sum(total),
        porcentaje = (total / total_ano) * 100,
        porcentaje_text = paste0(round(porcentaje, 1), "%")
      ) %>%
      ungroup() %>%
      select(ano, entidad, gr_edad, total, porcentaje) %>%
      pivot_wider(names_from = "gr_edad",
                  values_from=c("total", "porcentaje")) %>% 
      mutate(Total=(`total_Mayor e igual a 20`+`total_Menores 20`)) %>% 
      select(ano, entidad, Total, `total_Mayor e igual a 20`,
             `total_Menores 20`, `porcentaje_Mayor e igual a 20`,`porcentaje_Menores 20`) %>% 
      datatable(
        extensions = 'Buttons',
        options = list(
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
          paging = FALSE,
          autoWidth = TRUE),
        rownames = FALSE,
        colnames = c(
          "Año",
          "Entidad",
          "Total",
          "Mayor e igual a 20",
          "Menores de 20",
          "% mayor e igual a 20",
          "% menores de 20")
        ) %>%
      formatRound(columns = c(3:5), digits = 0) %>%
      formatRound(columns = c(6:7), digits = 1)
  })
  
  
# APEO tipo  ------------------------------------------------------------------------
  
  apeo_reactive <- reactive({
    df_apeo %>% 
      filter(gr_edad == input$apeo_grupo_edad,
             atencion %in% input$apeo_atencion,
             unidad_medica %in% input$apeo_unidad_medica,
             ano == input$apeo_ano)
  })
  
  output$grafico_apeo <- renderPlot({
    colores <- colorRampPalette(brewer.pal(9, "Set2"))(12)  
    
   apeo_reactive() %>%
      group_by(ano, unidad_medica, gr_edad, tipo) %>% 
      summarise(total = sum(total), .groups = "drop") %>% 
      mutate(
        porcentaje = total / sum(total)  # Mantiene como numérico para cálculo
      ) %>% 
      ggplot(aes(area = total, fill = tipo, 
                 label = paste0(tipo, "\n", scales::percent(porcentaje, accuracy = 0.1)))) +
      geom_treemap() +
      geom_treemap_text(fontface = "plain", colour = "white", place = "left", grow = F) +
     scale_fill_manual(values = colores) +  # Aplicar la paleta extendida
     facet_wrap(~ unidad_medica) +  
      labs(
        title = "Distribución de APEO por tipo, entidad, año y tipo de atención",
        subtitle = paste("Año:", input$apeo_ano, 
                         "\nGrupo de edad:", input$apeo_grupo_edad,
                         "\nTipo de atención:", paste(input$apeo_atencion, collapse = " y ")
        ),
        fill = "", x="", y="",
        caption = paste0("Fuente: Secretaría de Salud | Ipas Lac 
                         \nFecha de consulta: ", Sys.Date())
      ) +
      theme_minimal() +
      theme_ipas+
      guides(fill = guide_legend(ncol = 4))
    
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
          paging = FALSE,
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
             atencion %in% input$apeo_atencion_ano,
             unidad_medica %in% input$apeo_unidad_medica_ano,
             ano %in% input$apeo_ano_ano)
  })
  
  
  # Generar el gráfico con porcentaje total en cada barra
  output$grafico_apeo_ano <- renderPlot({
    colores <- colorRampPalette(brewer.pal(9, "Set2"))(12)  
    
    df <- apeo_reactive_ano() %>%
      group_by(ano, tipo) %>%  
      summarise(total = sum(total), .groups = "drop") %>% 
      group_by(ano) %>% 
      mutate(
        total_ano = sum(total),
        porcentaje = (total / total_ano) * 100,  # Convertir a porcentaje
        porcentaje_text = paste0(round(porcentaje, 1), "%")  # Formatear como porcentaje
      ) %>% 
      ungroup()
    
    p <- ggplot(df, aes(x = as.factor(ano), y = porcentaje, fill = tipo)) +
      geom_col(stat = "identity", position = "fill") +  # Usa position = "fill" para que sume 100%
      scale_fill_manual(values = colores) +  # Aplicar la paleta extendida
      labs(
        title = "Distribución de APEO por tipo, entidad, año y tipo de atención",
        subtitle = paste("Entidad:", paste(input$apeo_unidad_medica_ano, collapse = ", "),
                         "\nAño:", min(input$apeo_ano_ano), "a ", max(input$apeo_ano_ano),
                         "\n\nGrupo de edad:", paste(input$apeo_grupo_edad_ano, collapse = ", "),
                         "\nTipo de atención:", paste(input$apeo_atencion_ano, collapse = " y ")
                         
                         
                         ),
        fill = "", 
        x = "", 
        y = "",
        caption = paste0("Fuente: Secretaría de Salud | Ipas Lac \nFecha de consulta: ", Sys.Date())
      ) +
      scale_y_continuous(labels = scales::percent_format(scale = 1)) +  # Eje Y como porcentaje
      theme_minimal() +
      theme_ipas +
      theme(
        legend.position = "bottom",
        legend.text = element_text(size = 10), 
        axis.text.x = element_text(size = 13, color = "black", angle = 0, hjust = 0.5)
      ) +
      guides(fill = guide_legend(ncol = 4))
    
    # Agregar etiquetas solo si el usuario selecciona "Sí"
    if (input$mostrar_etiquetas_apeo) {
      p <- p + geom_label(
        aes(label = porcentaje_text, group = tipo), 
        position = position_fill(vjust = 0.5),  # Centra los textos en la barra normalizada
        size = 4, color="white",
        fontface = "plain", show.legend = FALSE
      )
    }
    
    p  # Retornar el gráfico
  })
  
  
  
  output$tabla_apeo_ano <- DT::renderDataTable({
    apeo_reactive_ano() %>%
      # filter(ano %in% input$apeo_ano_entidad) %>%  # Filtrar los años seleccionados
      group_by(ano, tipo, atencion) %>%
      summarise(total = sum(total, na.rm = TRUE), .groups = "drop") %>%
      ungroup() %>%
      group_by(ano) %>%
      mutate(
        porcentaje = (total / sum(total)) * 100  # Calcular porcentaje correctamente
      ) %>%
      ungroup() %>%
      pivot_wider(names_from = "atencion", values_from = c("total", "porcentaje")) %>%
      
      # Agregar una fila de total por cada año
      bind_rows(
        group_by(., ano) %>%
          summarise(across(where(is.numeric), sum, na.rm = TRUE)) %>%
          mutate(tipo = "Total") # Etiquetar como Total
      ) %>% 
      datatable(
        extensions = 'Buttons',
        options = list(
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
          paging = FALSE,
          autoWidth = TRUE
        ),
        rownames = FALSE,
        colnames = c("Año","Tipo", "Total de aborto","Total de parto", "Porcentaje de aborto","Porcentaje de parto")
      ) %>%
      formatRound(columns = 3:4, digits = 0) %>%  # Formato sin decimales en total
      formatRound(columns = 5:6, digits = 1)  # Formato de porcentaje
  })
  
  
  
  # APEO entidad  ------------------------------------------------------------------------
  
  apeo_reactive_entidad <- reactive({
    df_apeo %>%
      filter(gr_edad == input$apeo_grupo_edad_entidad,
             atencion %in% input$apeo_atencion_entidad,
             unidad_medica %in% input$apeo_unidad_medica_entidad,
             ano %in% input$apeo_ano_entidad)
  })

 
  output$grafico_apeo_entidad <- renderPlot({
    colores <- colorRampPalette(brewer.pal(9, "Set2"))(12)  
    
    df <- apeo_reactive_entidad() %>%
      group_by(unidad_medica, ano, tipo) %>%
      summarise(total = sum(total), .groups = "drop") %>%
      group_by(unidad_medica) %>%  
      mutate(
        total_entidad = sum(total),  
        porcentaje = (total / total_entidad) * 100,  
        porcentaje_text = paste0(round(porcentaje, 1), "%")  
      ) %>%
      ungroup()
    
    p <- ggplot(df, aes(x = unidad_medica, y = porcentaje, fill = tipo)) +
      geom_col(stat = "identity") +
      scale_fill_manual(values = colores) +  # Aplicar la paleta extendida
      labs(
        title = "Distribución de APEO por entidad, por año y grupo de edad",
        subtitle = paste("Año:", paste(input$apeo_ano_entidad, collapse = ", "), 
                         "\nGrupo de edad:", paste(input$apeo_grupo_edad_entidad, collapse = ", "),
                         "\nTipo de atención:", paste(input$apeo_atencion_entidad, collapse = " y ")),
        fill = "", x = "Unidad Médica", y = "Porcentaje",
        caption = paste0("Fuente: Secretaría de Salud | Ipas Lac \nFecha de consulta: ", Sys.Date())
      ) +
      scale_y_continuous(labels = scales::percent_format(scale = 1)) +
      coord_flip() +
      theme_minimal() +
      theme_ipas +
      theme(
        legend.position = "bottom",
        legend.text = element_text(size = 8),
        axis.text.y = element_text(size = 10, color = "black"),  # Asegurar que se vean bien los nombres
        axis.text.x = element_text(size = 13, color = "black", angle = 0)
      )
    
    # Agregar etiquetas solo si el usuario selecciona "Sí"
    if (input$mostrar_etiquetas_apeo_ent) {
      p <- p + geom_text(
        aes(label = porcentaje_text), 
        position = position_stack(vjust = 0.5),  # Centra los textos en cada barra apilada
        size = 4, color = "white",
        fontface = "plain"
      )
    }
    
    p  # Retornar el gráfico
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
          paging = FALSE,
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
    datos <- nacimiento_reactive()
    
    ggplot(datos, aes(
      x = reorder(entidad, -total),
      y = total,
      fill = total)) +
      geom_col() +
      geom_text(data = subset(datos, entidad == "Nacional"),
                aes(label = scales::comma(total)), 
                angle = 90,  
                hjust = 1.3,  # Más hacia la izquierda dentro de la barra
                size = 4,  
                color = "white") +
      geom_text(data = subset(datos, entidad != "Nacional"),
                aes(label = scales::comma(total)), 
                angle = 90,  
                hjust = -0.2,
                size = 4,  
                color = "black") +
      
      scale_fill_gradient(
        high = "#414487ff", 
        low = "#22a855af",
        label = comma) +
      labs(
        x = "", y = "", fill = " ",
        title = "Total de nacimientos por entidad y año",
        subtitle = paste0("Año: ", input$nacimiento_ano),
        caption = paste0("Fuente: Datos del SINAC de la Secretaría de Salud | Ipas Lac",
                         "\nFecha de consulta: ", Sys.Date())) +
            scale_y_continuous(labels = scales::comma, 
                         limits = c(0, max(datos$total) * 1.1), 
                         expand = expansion(mult = c(0, 0.15))) + 
      
      guides(fill = guide_legend(override.aes = list(size = 10))) + 
      
      theme_minimal() +
      theme_ipas +
      theme(
        legend.position = "bottom",
        axis.text.x = element_text(size = 13, color = "black", angle = 90, hjust = 1)
        # ,
        # plot.margin = margin(10, 10, 40, 10)
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
          paging = FALSE,
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
    datos <- nacimiento_reactive_ano()
    min_ano <- min(datos$ano, na.rm = TRUE)
    max_ano <- max(datos$ano, na.rm = TRUE)
    
    datos <- datos %>%
      arrange(ano) %>%
      mutate(variacion = (total / lag(total) - 1) * 100)
    
    ggplot(datos, aes(x = as.factor(ano))) +
      geom_bar(aes(y = total, fill = total), stat = "identity") +
      geom_text(aes(y = total, label = scales::comma(total)), 
                angle = 0,  
                hjust = 0.5,  
                vjust = -0.7,
                size = 4,  
                color = "black") +
      geom_label(aes(y = max(datos$total, na.rm = TRUE) * 1.05, 
                    label = ifelse(is.na(variacion), "", paste0(round(variacion, 1), "%")),
                    color = ifelse(variacion >= 0, "#246b26", "#a11616")),  # Color verde si es positivo, rojo si es negativo
                vjust = 0.2,  
                size = 4,
                fontface = "bold") +
      scale_color_identity() +  # Permite que ggplot use los colores definidos en aes(color = ...)
      labs(
        x = "", 
        y = "",
        fill = "",
        title = "Total de nacimientos por año y entidad",
        subtitle = paste0("Entidad: ", input$nacimiento_entidad, 
                          "\nAño: ", min_ano, " - ", max_ano),
        caption = paste0("\n*Los valores en rojo y verde corresponden a la variación anual",
                         "\nFuente: Datos del SINAC de la Secretaría de Salud | Ipas Lac", 
                         "\nFecha de consulta: ", Sys.Date())) +
      
      scale_y_continuous(labels = scales::comma) +
      
      scale_fill_gradient(
        high = "#414487ff", 
        low = "#22a855af",
        label = comma) +
      
      guides(fill = guide_legend(override.aes = list(size = 10))) + 
      
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
      arrange(ano) %>%
      mutate(variacion = (total / lag(total) - 1) * 100) %>% 
      arrange(-total) %>%
      datatable(
        extensions = 'Buttons',
        options = list(
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
          paging = FALSE,
          autoWidth = TRUE
        ),
        rownames = FALSE,
        colnames = c(
          "Año",
          "Total",
          "Variación"
        )
      ) %>%
      # formatRound(columns = c(3,4), digits = 0) %>%
      formatRound(columns = c(3), digits = 2) %>%  
      formatRound(columns = c(2), digits = 0) 
    
  })
  
  
# tasas de fecundidad:--------------------------------------------------------
  
  enadid_reactive <- reactive({
    enadid_fecundidad %>% 
      filter(
        ano %in% input$enadid_año, 
        #grupo_edad %in% input$enadid_edad, 
        nom_ent %in% input$fecundidad_entidad
      )
  })
  
  enadid_reactive_esp <- reactive({
    enadid_fecundidad_especifica %>% 
      filter(
        tipo %in% input$enadid_edad, 
        #grupo_edad %in% input$enadid_edad, 
        nom_ent %in% input$fecundidad_entidad2
      )
  })
  
  
  enadid_reactive2 <- reactive({
    enadid %>% 
      filter(
        ano %in% input$enadid_año2, 
        # grupo_edad %in% input$enadid_edad2, 
        nom_ent %in% input$enadid_entidad2
      )
  })
  
  enadid_reactive3 <- reactive({
    enadid %>% 
      filter(
        # ano %in% input$enadid_año2,
        # grupo_edad %in% input$enadid_edad3, 
        nom_ent %in% input$enadid_entidad3)
  })
  

  
  output$gr_tasa_fecundidad <- renderPlot({
   
    
    enadid_reactive() %>% 
      mutate(col_nacional=ifelse(nom_ent=="Nacional", 1, 0)) %>% 
      ggplot(aes(reorder(nom_ent, -tasa), tasa, 
                 fill=factor(col_nacional)
      )) +
      scale_fill_manual(values=paleta)+
      geom_col() +
      geom_text(aes(label = scales::comma(tasa, .01)), angle=90 , 
                hjust = 1.1,  
                size = 6,  
                color = "black") +
      labs(
        x = "", y = "", fill = " ",
        title = paste0("Tasa de fecundidad de los últimos 6 años por entidad"),
        subtitle = paste0("Año: ", input$enadid_año2),
        caption = paste0("Fuente: elaboración propia con base en los datos dela ENADID | Ipas Lac",
                         "\nFecha de consulta: ", Sys.Date())) +
      theme_ipas +
      theme(axis.text.x = element_text(angle = 90), 
            legend.position = "none") +
      scale_fill_manual(values = c("#f9592a", "grey")) +
      labs(x="", y="", fill="") 
   
  })
  
  
  
  
  #gráfica de tasa de fecundidad
  output$gr_tasa_fecundidad_esp <- renderPlot({
    
    
    enadid_reactive_esp() %>% 
      rename(tasa=Total) %>% 
      mutate(col_nacional=ifelse(nom_ent=="Nacional", 1, 0)) %>% 
      ggplot(aes(reorder(nom_ent, -tasa), tasa, 
                 fill=factor(col_nacional)
      )) +
      scale_fill_manual(values=paleta)+
      geom_col() +
      geom_text(data=. %>% filter(tasa>0),
                aes(label = scales::comma(tasa, .01)), angle=90 , 
                hjust = 1.1,  
                size = 6,  
                color = "black") +
      labs(
        x = "", y = "", fill = " ",
        title = paste0("Tasa de fecundidad especifica de los últimos 6 años por entidad"),
        subtitle = paste0("Grupo de edad: ", input$enadid_edad),
        caption = paste0("Fuente: elaboración propia con base en los datos dela ENADID | Ipas Lac",
                         "\nFecha de consulta: ", Sys.Date())) +
      theme_ipas +
      theme(axis.text.x = element_text(angle = 90), 
            legend.position = "none") +
      scale_fill_manual(values = c("#22a855af", "grey")) +
      labs(x="", y="", fill="") 
    
  })
  

  
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
    p <- violencia_reactive() %>%
      group_by(fecha, entidad, delito) %>%
      summarise(total = sum(total), .groups = "drop") %>%
      group_by(fecha, entidad) %>%
      mutate(
        total_entidad = sum(total),  
        porcentaje = (total / total_entidad) * 100) %>%
      ungroup() %>%
      
      ggplot(aes(
        x = reorder(entidad, -total_entidad),  
        y = total,
        fill = delito)) +
      geom_col() +
      labs(
        x = "", y = "", fill = " ",
        title = paste0("Total de carpetas de investigación por tipo de delito y año, "), 
        subtitle = paste0("\nAño: ", input$violencia_fecha),
        caption = paste0("\nFuente: elaboración propia con base en los datos del SESNSP | Ipas Lac",
                         "\nFecha de consulta: ", Sys.Date())
      ) +
      scale_y_continuous(labels = scales::comma) +
      scale_fill_brewer(palette = "Set2", direction = -1)+
      theme_minimal() +
      theme_ipas +
      theme(
        legend.position = "bottom",
        axis.text.x = element_text(size = 12, color = "black", angle = 90, hjust = 1),
        legend.text = element_text(size = 9)
      )
    
    # Agregar etiquetas solo si el usuario selecciona "Sí"
    if (input$mostrar_etiquetas_violencia_1) {
      p <- p + geom_text(
        aes(label = paste0(round(porcentaje, 1), "%")), 
        position = position_stack(vjust = 0.5), 
        size = 3, color = "white"
      )
    }
    
    p  # Retornar el gráfico
  })
  
  
   
  # Tabla asegurando que cada entidad sume 100% y agregando total por entidad
  output$tabla_violencia <- DT::renderDataTable({
    
    violencia_reactive() %>%
    # df_violencia %>%
      group_by(fecha, entidad, delito) %>%
      summarise(total = sum(total), .groups = "drop") %>%
      # Calcular el total de delitos por entidad en cada fecha
      group_by(fecha, entidad) %>%
      mutate(total_entidad = sum(total)) %>%
      mutate(porcentaje = (total / total_entidad) * 100) %>%
      ungroup() %>% 
      pivot_wider(
        names_from = "delito",
        values_from = c("total", "porcentaje")
      ) %>% 
      select(fecha, entidad, total_entidad, 
             `total_Violencia familiar`,`porcentaje_Violencia familiar`,
             `total_Violación`,`porcentaje_Violación`,
             `total_Homicidio doloso`,`porcentaje_Homicidio doloso`,
             `total_Abuso sexual`,`porcentaje_Abuso sexual`,
             `total_Acoso sexual`,`porcentaje_Acoso sexual`,
             `total_Feminicidio`,`porcentaje_Feminicidio`,
             `total_Violencia de género`,`porcentaje_Violencia de género`
             ) %>% 
      datatable(
        extensions = 'Buttons',
        options = list(
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
          paging = FALSE,
          autoWidth = TRUE,
          class = "compact", # Reduce el padding
          columnDefs = list(
            list(targets = "_all", className = "dt-body-small") # Reduce el tamaño de fuente
          )
        ),
      rownames = FALSE,
      colnames = c("Año", "Entidad", "Total entidad",
                   "Violencia familiar", "%",
                   "Violación", "%",
                   "Homicidio doloso", "%",
                   "Abuso sexual", "%",
                   "Acoso sexual", "%",
                   "Feminicidio", "%",
                   "Violencia de género", "%")
    )  %>%
      formatRound(columns = c(3:4,6,8,10,12,14,16), digits = 0) %>%
      formatRound(columns = c(5,7,9,11,13,15,17), digits = 1)
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
      # 🔹 Agregar etiquetas solo si input$mostrar_etiquetas es TRUE
      { if (input$mostrar_etiquetas_violencia_2) geom_text(aes(label = paste0(round(porcentaje, 1), "%")), 
                                               position = position_stack(vjust = 0.5), 
                                               size = 3, color = "white") else NULL } +
      labs(
        x = "", y = "", fill = "",
        title = paste0("Total de carpetas de investigación por tipo de delito y entidad"),
        subtitle = paste0("\nEntidad: ", input$violencia_entidad_ano,
                          "\nAño: ", min(input$violencia_fecha_ano), " a ", max(input$violencia_fecha_ano)),
        caption = paste0("Fuente: elaboración propia con base en los datos del SESNSP | Ipas Lac",
                         "\nFecha de consulta: ", Sys.Date())
      ) +
      scale_y_continuous(labels = scales::comma) +
      scale_fill_brewer(palette = "Set2", direction = -1) +
      theme_minimal() +
      theme_ipas +
      theme(
        legend.position = "bottom",
        axis.text.x = element_text(size = 9, color = "black", angle = 0, hjust = 1),
        legend.text = element_text(size=9)
      ) 
  })
  
  
  
  
  output$tabla_violencia_ano <- DT::renderDataTable({
    datos <- violencia_reactive_ano() %>%
  # df_violencia %>%
      group_by(fecha, delito) %>%
      summarise(total = sum(total), .groups = "drop") %>%
      group_by(fecha) %>%
    mutate(total_entidad = sum(total)) %>%
    mutate(porcentaje = (total / total_entidad) * 100) %>%
    ungroup() %>% 
    pivot_wider(
      names_from = "delito",
      values_from = c("total", "porcentaje")
    ) %>% 
    select(fecha, total_entidad, 
           `total_Violencia familiar`,`porcentaje_Violencia familiar`,
           `total_Violación`,`porcentaje_Violación`,
           `total_Homicidio doloso`,`porcentaje_Homicidio doloso`,
           `total_Abuso sexual`,`porcentaje_Abuso sexual`,
           `total_Acoso sexual`,`porcentaje_Acoso sexual`,
           `total_Feminicidio`,`porcentaje_Feminicidio`,
           `total_Violencia de género`,`porcentaje_Violencia de género`
    ) %>%
    datatable(
      extensions = 'Buttons',
      options = list(
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
        paging = FALSE,
        autoWidth = TRUE
      ),
      rownames = FALSE,
      colnames = c("Año", "Total",
                   "Violencia familiar", "%",
                   "Violación", "%",
                   "Homicidio doloso", "%",
                   "Abuso sexual", "%",
                   "Acoso sexual", "%",
                   "Feminicidio", "%",
                   "Violencia de género", "%")
    )  %>%
      formatRound(columns = c(2:3,5,7,9,11,13,15), digits = 0) %>%
      formatRound(columns = c(4,6,8,10,12,14,16), digits = 1)
  })
  
    
  
# ENADID ----------------------------------------------------------------------

  
  # Gráfico 1 anticonceptivo----------------------------------------------------
  
  output$gr_uso_anti_primera_vez <- renderPlot({
    colores <- colorRampPalette(brewer.pal(9, "Set2"))(14)  
    
    
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
      ggplot(aes(nom_ent, Porcentaje, #text=tooltip_text, 
                 fill=tipo)) +
      geom_col() +
      labs(x="", y="") +
      geom_text(data=. %>% 
                  filter(Porcentaje>0.05),
                aes(label=scales::percent(Porcentaje, 1)), 
                position = position_stack(vjust = .8), size=4,color="white") +
      labs(title="Proporción de uso y tipo de anticonceptivo en la primera relación sexual",
           subtitle = paste("Año: ", input$enadid_año2),
           x="", y="", fill="")+
      scale_fill_manual(values = rev(colores)) +  # Aplicar la paleta extendida
      theme_ipas +
      theme(axis.text.x = element_text(angle = 90), 
            legend.position = "bottom",
            legend.text = element_text(size = 9)) +
      scale_y_continuous(labels = percent)
    
    gr_tasa
    
  })
  
  
# Gráfico 2 -------------------------------------------------------------------
  
  output$gr_uso_anticonceptivo <- renderPlot({
    colores <- colorRampPalette(brewer.pal(9, "Set2"))(14)  
    
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
                                    "DIU", "Condón masculino", "Condón femenino"))) %>%
      ggplot(aes(nom_ent, Porcentaje,fill=tipo, group=tipo)) +
      geom_col() +
      labs(x="", y="", fill="",
           title = "Proporción de uso y tipo de anticonceptivo en la actualidad",
           paste0("Año: ", input$enadid_año2)) +
      geom_text(data=. %>% 
                  filter(Porcentaje>0.05),
                  aes(label=scales::percent(Porcentaje, 1)), 
                position = position_stack(vjust = .8), size=4, color="white"
                ) +
       scale_fill_manual(values = rev(colores)) +  # Aplicar la paleta extendida
      theme_ipas +
      theme(axis.text.x = element_text(angle = 90), 
            legend.position = "bottom",
            legend.text = element_text(size = 9)) +
      scale_y_continuous(labels = percent)
    
    gr_tasa
    
  })
  
 

  
# Grafico temporal----------------------------------------------------------------
  
  output$gr_uso_anti_primera_vez_tiempo <- renderPlot({
    
    enadid_reactive3() %>% 
      drop_na(edad_primera_relacion_sexual) %>% 
      gather(tipo, Total, no_uso:no_responde) %>% 
      filter(Total==2) %>% 
      mutate(tipo=str_replace_all(tipo, "_", " ")) %>% 
      group_by(nom_ent, tipo, ano) %>%
      summarise(uso_metodos=sum(factor)) %>% group_by(nom_ent, ano) %>% 
      mutate(Porcentaje=uso_metodos/sum(uso_metodos)) %>% 
      mutate(tipo=str_wrap(tipo, 5),
             as.factor(ano)) %>% 
      ggplot(aes(x=reorder(tipo, -Porcentaje), Porcentaje, fill=factor(ano))) +

      geom_col(position = "dodge") +
      geom_label(aes(label=percent(Porcentaje, 1)), 
                position = position_dodge(width = 1), 
                size=4, colour="white", show.legend = F) +
      scale_fill_manual(values = paleta)+
      labs(x="", y="", fill="",
           title="Proporción de uso y tipo de anticonceptivo en la primera relación sexual, por años",
           subtitle = paste0("Entidad: ", input$enadid_entidad3)) +
      theme_ipas +
      theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size = 9), 
            legend.position = "bottom",
            legend.text = element_text(size = 9)) +
      scale_y_continuous(labels = percent)
  })
  
  
  output$tabla_uso_anti_primera_vez_tiempo <- DT::renderDataTable({
    enadid_reactive3() %>% 
      drop_na(edad_primera_relacion_sexual) %>% 
      gather(tipo, Total, no_uso:no_responde) %>% 
      filter(Total == 2) %>% 
      mutate(tipo = str_replace_all(tipo, "_", " ")) %>% 
      group_by(nom_ent, tipo, ano) %>%
      summarise(uso_metodos = sum(factor), .groups = "drop") %>%
      group_by(nom_ent, ano) %>% 
      mutate(Porcentaje = uso_metodos / sum(uso_metodos) * 100) %>%  # Convertir a porcentaje
      select(nom_ent, ano, tipo, Porcentaje) %>%  # Seleccionar solo las columnas relevantes
      pivot_wider(names_from = "ano",
                  values_from = "Porcentaje") %>% 
      datatable(
        extensions = 'Buttons',
        options = list(
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
          paging = FALSE,        # Desactiva paginación para mostrar todo con scroll
          # scrollX = TRUE,        # Habilita desplazamiento horizontal
          # scrollY = "400px",     # Habilita desplazamiento vertical
          autoWidth = TRUE
        ),
        rownames = FALSE,
        colnames = c("Entidad", "Método anticonceptivo", "2018", "2024")
      ) %>%
      formatRound(columns = 3:4, digits = 1)  # Redondear el porcentaje a 1 decimal
  })
  
  #gráfico temporal ------------------------------------------------------------
  
  output$gr_uso_anticonceptivo_tiempo <- renderPlot({
   
    enadid_reactive3() %>% 
      group_by("tipo"=actual_metodo, nom_ent, ano) %>%
      summarise(uso_metodos=sum(factor)) %>% group_by(nom_ent, ano) %>% 
      mutate(Porcentaje=uso_metodos/sum(uso_metodos)) %>% ungroup() %>% 
      complete(tipo=c(1:9), 
      ano=unique(enadid$ano), 
      nom_ent=input$enadid_entidad3,
      fill=list(uso_metodos=0, Porcentaje=0)) %>% 
      mutate(tipo=factor(tipo, 
                         labels = c("OTB", "Vasectomía", "Pastillas anticonceptivas", 
                                    "Inyecciones anticonceptivas",
                                    "Implante anticonceptivo", "Parche anticonceptivo", 
                                    "DIU", "Condón masculino", "Condón femenino"))) %>% 
      mutate(tipo=str_wrap(tipo, 12)) %>% 
      ggplot(aes(reorder(tipo, -Porcentaje), Porcentaje, fill=factor(ano))) +
      geom_col(position = "dodge") +
      geom_label(aes(label=percent(Porcentaje, 1)), 
                 position = position_dodge(width = 0.9), 
                 size=4, color="white", show.legend = F
      ) +
      scale_fill_manual(values=paleta)+
      labs(x="", y="", fill="",
           title="Proporción de uso y tipo de anticonceptivo en la actualidad, por años",
           subtitle = paste0("Entidad: ", input$enadid_entidad3)) +
      theme_ipas +
      theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size = 9), 
            legend.position = "bottom",
            legend.text = element_text(size = 9)) +
      scale_y_continuous(labels = percent)
  })
  
  output$tabla_uso_anticonceptivo_tiempo <- DT::renderDataTable({
    enadid_reactive3() %>% 
      group_by(tipo = actual_metodo, nom_ent, ano) %>%
      summarise(uso_metodos = sum(factor), .groups = "drop") %>% 
      group_by(nom_ent, ano) %>% 
      mutate(Porcentaje = uso_metodos / sum(uso_metodos) * 100) %>% 
      ungroup() %>% 
      complete(
        tipo = c(1:9), 
        ano = unique(enadid$ano), 
        nom_ent = input$enadid_entidad3,
        fill = list(uso_metodos = 0, Porcentaje = 0)
      ) %>% 
      mutate(tipo = factor(tipo, 
                           labels = c("OTB", "Vasectomía", "Pastillas anticonceptivas", 
                                      "Inyecciones anticonceptivas",
                                      "Implante anticonceptivo", "Parche anticonceptivo", 
                                      "DIU", "Condón masculino", "Condón femenino"))) %>% 
      select(nom_ent, ano, tipo, Porcentaje) %>%  # Seleccionar columnas clave
      pivot_wider(names_from = "ano", values_from = "Porcentaje") %>% 
      datatable(
        extensions = 'Buttons',
        options = list(
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
          paging = FALSE,        # Desactiva paginación para usar scroll
          autoWidth = TRUE
        ),
        rownames = FALSE,
        colnames = c("Entidad", "Método anticonceptivo", sort(unique(enadid_reactive3()$ano)))  # Automático
      ) %>%
      formatRound(columns = 3:4, digits = 1)  # Redondear porcentajes
  })
  
  
  # tabla antinconceptivo 1
  output$tabla_uso_anti_primera_vez <- DT::renderDataTable({
    tabla_nacional <-  enadid_reactive2() %>%
      # enadid %>%
      drop_na(edad_primera_relacion_sexual) %>% 
      gather(tipo, Total, no_uso:no_responde) %>% 
      filter(Total==2) %>% 
      mutate(tipo=str_replace_all(tipo, "_", " ")) %>% 
      group_by(ano, tipo) %>%
      summarise(uso_metodos=sum(factor)) %>% #group_by(nom_ent) %>% 
      mutate(Porcentaje=uso_metodos/sum(uso_metodos)*100, 
             nom_ent="Nacional")
    
    tabla_estatal <- enadid_reactive2() %>%
    # enadid %>% 
      drop_na(edad_primera_relacion_sexual) %>% 
      gather(tipo, Total, no_uso:no_responde) %>% 
      filter(Total==2) %>% 
      mutate(tipo=str_replace_all(tipo, "_", " ")) %>% 
      group_by(ano, nom_ent, tipo) %>%
      summarise(uso_metodos=sum(factor)) %>% group_by(nom_ent) %>% 
      mutate(Porcentaje=uso_metodos/sum(uso_metodos)*100) %>% 
      bind_rows(tabla_nacional) %>% 
      select(-c(uso_metodos, ano)) %>% 
      pivot_wider(names_from="tipo",
                  values_from = c("Porcentaje")) %>%  
      datatable(
        extensions = 'Buttons',
        options = list(
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
          paging = FALSE,
          autoWidth = TRUE,
          scrollX = T,        # Habilita desplazamiento horizontal
          scrollY = "400px",
          columnDefs = list(
            list(targets = "_all", className = "dt-body-small head-rotate") # Reduce el tamaño de fuente
          )
        ),
        rownames = FALSE,
        caption = htmltools::tags$caption(
          style = 'font-size: 155px; color: black;')
      ) %>%
      formatRound(columns = 2:15, digits = 1)
  })
  
  output$tabla_uso_anticonceptivo <- DT::renderDataTable({
    tabla_nacional <- enadid_reactive2() %>% 
      group_by(ano, "tipo"=actual_metodo) %>%
      summarise(uso_metodos=sum(factor)) %>% group_by(ano) %>% 
      mutate(Porcentaje=uso_metodos/sum(uso_metodos)*100) %>% 
      mutate(nom_ent="Nacional") %>% ungroup() 
    
    # tabla_estatal <- 
      enadid_reactive2() %>% 
      group_by(ano, "tipo"=actual_metodo, nom_ent) %>%
      summarise(uso_metodos=sum(factor)) %>% group_by(ano, nom_ent) %>% 
      mutate(Porcentaje=uso_metodos/sum(uso_metodos)) %>% ungroup() %>% 
      bind_rows(tabla_nacional) %>%
      select(!c(ano, uso_metodos)) %>% 
        mutate(
          Porcentaje=round(Porcentaje, 2),
          tipo=factor(tipo,
                           labels = c("OTB", "Vasectomía", "Pastillas anticonceptivas",
                                      "Inyecciones anticonceptivas",
                                      "Implante anticonceptivo", "Parche anticonceptivo",
                                      "DIU", "Condón masculino", "Condón femenino"
                           )
        )) %>%
      pivot_wider(
        names_from = "tipo",
        values_from = "Porcentaje", 
        values_fill = 0
        ) %>% 

      # relocate(nom_ent, .before = "tipo") %>%
    # tabla_estatal %>%
    arrange(desc(rowSums(across(where(is.numeric))))) %>%
      # datatable(
      #   extensions = 'Buttons',
      #   options = list(
      #     dom = 'Bfrtip',
      #     buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
      #     paging = FALSE,
      #     autoWidth = TRUE
      #   ),
      #   rownames = FALSE,
      #   # colnames = c("Año", "Entidad", "Tipo de anticonceptivo",
      #   #              "Total",
      #   #              "Porcentaje"), 
      #   caption = htmltools::tags$caption(
      #     style = 'font-size: 15px; color: black;', # Personaliza el tamaño y color
      #     "Tabla de indicador de uso actual de anticonceptivos"
      #   )
      # ) 
        datatable(
          extensions = 'Buttons',
          options = list(
            dom = 'Bfrtip',
            buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
            paging = FALSE,
            autoWidth = TRUE,
            scrollX = T,        # Habilita desplazamiento horizontal
            scrollY = "400px",
            columnDefs = list(
              list(targets = "_all", className = "dt-body-small head-rotate") # Reduce el tamaño de fuente
            )
          ),
          rownames = FALSE,
          caption = htmltools::tags$caption(
            style = 'font-size: 155px; color: black;')
        ) %>%
        formatRound(columns = 2:9, digits = 2)
    # %>%
    #   formatRound(columns = 4, digits = 0) %>%  
    #   formatRound(columns = 5, digits = 2)
  })
  
  output$tabla_fecundidad <- DT::renderDataTable({
    # tabla_nacional <- enadid_fecundidad %>%
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
    
    enadid_reactive() %>%
      # arrange(desc(rowSums(across(where(is.numeric))))) %>%
      arrange(desc(tasa)) %>%
      mutate(tasa=comma(tasa, .01)) %>% 
      # datatable(
      #   extensions = 'Buttons',
      #   options = list(
      #     dom = 'Bfrtip',
      #     buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
      #     pageLength = 10,
      #     autoWidth = TRUE
      #   ),
      #   rownames = FALSE,
      #   # colnames = c("Entidad", "Año", "Total de mujeres",
      #   #              "Hijos vivos", "Tasa"), 
      #   colnames = c("Año", "Entidad", "Tasa"), 
      #   caption = htmltools::tags$caption(
      #     style = 'font-size: 25px; color: black;', # Personaliza el tamaño y color
      #     "Tabla de indicador de fecundidad de los últimos 6 años"
      #   )
      # )
    datatable(
      extensions = 'Buttons',
      options = list(
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
        paging = FALSE,
        autoWidth = TRUE,
        # scrollX = T,        # Habilita desplazamiento horizontal
        scrollY = "400px",
        columnDefs = list(
          list(targets = "_all", className = "dt-body-small head-rotate") # Reduce el tamaño de fuente
        )
      ),
      rownames = FALSE,
      colnames = c("Año", "Entidad", "Tasa"),
      caption = htmltools::tags$caption(
        style = 'font-size: 155px; color: black;')
    ) %>%
      formatRound(columns = 3, digits = 2)
    
    
    
    
  })
  
  output$tabla_fecundidad_esp <- DT::renderDataTable({
    # tabla_nacional <- enadid_fecundidad %>%
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
    
    enadid_reactive_esp() %>%
      rename(tasa=Total) %>% 
      # arrange(desc(rowSums(across(where(is.numeric))))) %>%
      arrange(desc(tasa)) %>%
      mutate(tasa=comma(tasa, .01)) %>% 
      # datatable(
      #   extensions = 'Buttons',
      #   options = list(
      #     dom = 'Bfrtip',
      #     buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
      #     pageLength = 10,
      #     autoWidth = TRUE
      #   ),
      #   rownames = FALSE,
      #   # colnames = c("Entidad", "Año", "Total de mujeres",
      #   #              "Hijos vivos", "Tasa"), 
    #   colnames = c("Año", "Entidad", "Tasa"), 
    #   caption = htmltools::tags$caption(
    #     style = 'font-size: 25px; color: black;', # Personaliza el tamaño y color
    #     "Tabla de indicador de fecundidad de los últimos 6 años"
    #   )
    # )
    datatable(
      extensions = 'Buttons',
      options = list(
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
        paging = FALSE,
        autoWidth = TRUE,
        # scrollX = T,        # Habilita desplazamiento horizontal
        scrollY = "400px",
        columnDefs = list(
          list(targets = "_all", className = "dt-body-small head-rotate") # Reduce el tamaño de fuente
        )
      ),
      rownames = FALSE,
      colnames = c("Año", "Entidad", "Grupo de edad",  "Tasa"),
      caption = htmltools::tags$caption(
        style = 'font-size: 155px; color: black;')
    ) %>%
      formatRound(columns = 4, digits = 2)
    
    
    
    
  })
  
}

# Lanzar App
shinyApp(ui = ui, server = server)