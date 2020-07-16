library(shiny)
library(rsconnect)
library(shiny)
library(shinythemes)
library(tidyverse)
library(magrittr)
library(lubridate)
library(plotly)
library(xts)
library(dygraphs)
library(rnaturalearth)
library(ggplot2)
library(reticulate)
library(knitr)
library(tidyverse)
library(gghighlight)
library(magrittr)
library(readxl)
library(readr)
library(ape)
library(ggdendro)
library(bookdown)
# require(rgdal)
require(ggplot2)
library(sf)
# library(ggrepel)
library(janitor)
# library(dplyr)
library(bcrypt)
library(shiny)
library(tibble)
library(shinymanager)


# Define UI for random distribution application
# shinyUI(pageWithSidebar(
#         # Application title
#         headerPanel("Análisis de los datos COVID-19 en Chile"),
ui <- fluidPage(
        titlePanel("Análisis de la situación Covid-19 en Chile"),
        
        
        
        theme = shinytheme("sandstone"),
        
        # file1 es el nombre de nuestra pseudobasededatos:
        sidebarPanel(
                fileInput(
                        'file1',
                        'Seleccione un archivo CSV',
                        accept = c('text/csv', 'text/comma-separated-values,text/plain', '.csv')
                ),
                checkboxInput('header', 'Cabecera', TRUE),
                radioButtons(
                        'sep',
                        'Separador',
                        c(
                                Coma = ',',
                                'Punto y coma' = ';',
                                Tabulador = '\t'
                        ),
                        'Comma'
                ),
                radioButtons(
                        'quote',
                        'Comillas',
                        c(
                                None = '',
                                'Comillas dobles' = '"',
                                'Comillas simples' = "'"
                        ),
                        'Double Quote'
                ),
                
                tags$hr(),
                
                dateInput("date1", "Fecha de inicio", value = "2020-01-24"),
                dateInput("date2", "Fecha de fin", value = today()),
                
                uiOutput("Comuna"),
                
                tags$hr(),
                downloadButton("cons", "Descargue la tabla"),
                
                tags$hr(),
                
                br()
        ),
        
        # Show a tabset that includes a plot, summary, and table view
        # of the generated distribution
        mainPanel(
                tabsetPanel(
                        
            

               
                        
                        tabPanel("Los datos", tableOutput("contents")),
                        
                        # tabsetPanel(
                        #         tabPanel("Panel 1.1"),
                        #         tabPanel("Panel 1.2")
                        # ),
                        
                        tabPanel("Los datos R_0", tableOutput("tableR0")),
                        tabPanel("La matemática del R0", "", value = 2,
                                 uiOutput("Link_math_r0")),
                        # tabPanel("Estadisticas basicas", verbatimTextOutput("summary")),
                        
                        tabPanel("Mapas", tabsetPanel(
                                type = "tabs",
                                
                                tabPanel(
                                        "Región de Arica y Parinacota", "", value = 2,
                                        uiOutput("Link_map_ayp")),
                                
                                tabPanel("Región de Tarapacá", "", value = 2,
                                         uiOutput("Link_map_tarap")),
                                
                                tabPanel("Región de Antofagasta", "", value = 2,
                                         uiOutput("Link_map_antof")),
                                
                                tabPanel("Región de Atacama", "", value = 2,
                                         uiOutput("Link_map_ataca")),
                                
                                tabPanel("Región de Coquimbo", "", value = 2,
                                         uiOutput("Link_map_coqui")),
                                
                                tabPanel("Región de Valparaíso", "", value = 2,
                                         uiOutput("Link_map_valpa")),
                                
                                tabPanel("Región Metropolitana", "", value = 2,
                                         uiOutput("Link_map_metro")),
                                
                                tabPanel("Región del Libertador Bernardo O'Higgins", "", value = 2,
                                         uiOutput("Link_map_liber")),
                                
                                tabPanel("Región del Maule", "", value = 2,
                                         uiOutput("Link_map_maule")),
                                
                                tabPanel("Región del Ñuble", "", value = 2,
                                         uiOutput("Link_map_nuble")),
                                
                                tabPanel("Región del Bío-bío", "", value = 2,
                                         uiOutput("Link_map_biobi")),
                                
                                tabPanel("Región de la Araucanía", "", value = 2,
                                         uiOutput("Link_map_laara")),
                            
                                tabPanel("Región de los Ríos", "", value = 2,
                                         uiOutput("Link_map_losri")),
                                
                                tabPanel("Región de los Lagos", "", value = 2,
                                         uiOutput("Link_map_losla")),
                                
                                tabPanel("Región de Aysén", "", value = 2,
                                         uiOutput("Link_map_aysen")),
                                
                                tabPanel("Región de Magallanes", "", value = 2,
                                         uiOutput("Link_map_magal"))
                     
                                
                                
                                
                                           
                        )),
                        
                        
                        # quiero que estos datos desplieguen una analisis a a nivel nacional:
                        # tabPanel("datos del dummy", tableOutput("table")),
                        
                        
                        
                        
                        
                        
                        tabPanel("R_0", plotOutput("plot3")),
                        
                        
                        
                        
                        
                        tabPanel("Gráfica de la evolución de la pandemia", plotlyOutput("plot4")),
                        

                        
                        tabPanel("Ranking", "", value = 2,
                                 uiOutput("Link_ranking")),
                        
                        
                        
                        
                        tabPanel(
                                "Gráfica de la evolución de la pandemia por cada 100.000 hbtes.",
                                plotlyOutput("plot5")
                        )
                        #  tableOutput("contents")
                        
                )
        )
)
        ###########################################
        # Importante para la autentivcacion!
        # ui <- secure_app(ui)
        ###########################################