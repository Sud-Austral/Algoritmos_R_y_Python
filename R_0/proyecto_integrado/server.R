# datos de poblacion por comuna
# https://github.com/MinCiencia/Datos-COVID19/blob/master/output/producto38/CasosFallecidosPorComuna.csv

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
# clearlibrary(dplyr)
library(janitor)
library(dplyr)
library(readxl)
library(ggplot2)
library(gganimate)

# la siguiente linea impide el despliegue de advertencias en forma local:
options(warn = -1)

library(tidyverse)
library(gghighlight)
library(magrittr)
library(readr)
library(ape)
library(ggdendro)
library(rmarkdown)
library(xlsx)
library(bcrypt)
library(shiny)
library(tibble)
library(shinymanager)
# Define server logic for random distribution application
shinyServer(function(input, output) {
        
        
        ###########################################
        # Importante para la autentivcacion!
        
        # credentials <- data.frame(
        #         user = c("Patricio", "shinymanager"),
        #         password = c("Incitatus", "12345"),
        #         stringsAsFactors = FALSE
        # )   
        # 
        # 
        # res_auth <- secure_server(
        #         check_credentials = check_credentials(credentials)
        # )
        ###########################################        
        
        
        output$Link_map_ayp <- renderUI({
                a("ir al mapa", href="https://rpubs.com/Chris_666/Link_map_ayp", target="_blank")
        })
        
        output$Link_map_tarap <- renderUI({
                a("ir al mapa", href="https://rpubs.com/Chris_666/tarap", target="_blank")
        })
        
        output$Link_map_antof <- renderUI({
                a("ir al mapa", href="https://rpubs.com/Chris_666/antof", target="_blank")
        })
        
        output$Link_map_ataca <- renderUI({
                a("ir al mapa", href="https://rpubs.com/Chris_666/ataca", target="_blank")
        })
        
        output$Link_map_coqui <- renderUI({
                a("ir al mapa", href="https://rpubs.com/Chris_666/coqui", target="_blank")
        })
        
        output$Link_map_valpa <- renderUI({
                a("ir al mapa", href="https://rpubs.com/Chris_666/valpa", target="_blank")
        })
        
        output$Link_map_metro <- renderUI({
                a("ir al mapa", href="https://rpubs.com/Chris_666/metro", target="_blank")
        })
        
        output$Link_map_liber <- renderUI({
                a("ir al mapa", href="https://rpubs.com/Chris_666/liber", target="_blank")
        })
        
        output$Link_map_maule <- renderUI({
                a("ir al mapa", href="https://rpubs.com/Chris_666/maule", target="_blank")
        })
        
        output$Link_map_nuble <- renderUI({
                a("ir al mapa", href="https://rpubs.com/Chris_666/nuble", target="_blank")
        })
        
        output$Link_map_biobi <- renderUI({
                a("ir al mapa", href="https://rpubs.com/Chris_666/biobi", target="_blank")
        })
        
        output$Link_map_laara <- renderUI({
                a("ir al mapa", href="https://rpubs.com/Chris_666/laara", target="_blank")
        })
        
        output$Link_map_losri <- renderUI({
                a("ir al mapa", href="https://rpubs.com/Chris_666/losri", target="_blank")
        })
        
        output$Link_map_losla <- renderUI({
                a("ir al mapa", href="https://rpubs.com/Chris_666/losla", target="_blank")
        })
        
        output$Link_map_aysen<- renderUI({
                a("ir al mapa", href="https://rpubs.com/Chris_666/aysen", target="_blank")
        })
        
        output$Link_map_magal<- renderUI({
                a("ir al mapa", href="https://rpubs.com/Chris_666/magal", target="_blank")
        })
        
        
        output$Link_ranking<- renderUI({
                a("ir al ranking", href="https://rpubs.com/Chris_666/barras_moviles", target="_blank")
        })
        
        
        output$Link_math_r0 <- renderUI({
                a("ir a la matematica del r0", href="https://rpubs.com/Chris_666/Link_math_r0", target="_blank")
        })
        

        
        
        output$auth_output <- renderPrint({
                reactiveValuesToList(res_auth)
        })
        
        # Reactive expression to generate the requested distribution. This is
        # called whenever the inputs change. The renderers defined
        # below then all use the value computed from this expression
        
        # PasswordDF <- data.frame(username = c("alpha", "beta", "gamma"),
        #                          HashedPassword = c(hashpw("artichoke"), hashpw("crabapple"), hashpw("eggplant")),
        #                          group = c("A", "B", "C"),
        #                          stringsAsFactors = FALSE
        # )
        # 
        # Dat <- tibble(group = c(rep("A", 3), rep("B", 3), rep("C", 3)),
        #               x = rep(1:3, 3),
        #               y = c(2,3,4,14,13,12,23,22,24))
        # 
        # 
        # filteredDat <- eventReactive(input$button, {
        #         if (input$user %in% PasswordDF$username && 
        #             checkpw(input$PW, 
        #                     hash = PasswordDF[PasswordDF$username == input$user, "HashedPassword"])) {
        #                 Dat %>% filter(group == PasswordDF[PasswordDF$username == input$user, "group"])
        #         } else data.frame(x = 0, y = 0)
        # })
        # output$plot <- renderPlot({ plot(filteredDat()$x, filteredDat()$y, main = isolate(input$user), type = "b") })
        # 
        
        

        
        
        
        data <- reactive({
                dist <- switch(
                        input$dist,
                        norm = rnorm,
                        unif = runif,
                        lnorm = rlnorm,
                        exp = rexp,

                        rnorm
                )
                
                dist(input$n)
        })
        
        # Generate a plot of the data. Also uses the inputs to build the
        # plot label. Note that the dependencies on both the inputs and
        # the 'data' reactive expression are both tracked, and all expressions
        # are called in the sequence implied by the dependency graph
        output$plot <- renderPlot({
                dist <- input$dist
                n <- input$n
                
                hist(data(),
                     main = paste('r', dist, '(', n, ')', sep = ''))
        })
        
        # Generate a summary of the data
        output$summary <- renderPrint({
                summary(data())
        })
        
        # Generate an HTML table view of the data
        output$table <- renderTable({
                data.frame(x = data())
        })
        
        ###########################
        # esto funciona bien:
        ###########################
        output$contents <- renderTable({
                # input$file1 will be NULL initially. After the user selects and uploads a
                # file, it will be a data frame with 'name', 'size', 'type', and 'datapath'
                # columns. The 'datapath' column will contain the local filenames where the
                # data can be found.
                
                inFile <- input$file1
                
                if (is.null(inFile))
                        return(NULL)
                
                covid19 <- read.csv(
                                inFile$datapath,
                                header = input$header,
                                sep = input$sep,
                                quote = input$quote, stringsAsFactors=FALSE, fileEncoding="latin1"
                        )
                
                covid19$Fecha <- excel_numeric_to_date(covid19$Fecha)
                
                Comunas <- covid19 %>% select(Comuna) %>% arrange(Comuna) %>% unique()
                selectInput("Comuna", "Seleccione la Comuna:", choices = Comunas)
                
                return(covid19 %>%  filter(Comuna == input$Comuna) %>% filter(between(Fecha, input$date1, input$date2)))
                

                
        })
        
        output$Comuna <- renderUI({
                
                inFile <- input$file1
                
                if (is.null(inFile))
                        return(NULL)
                
                covid19 <- read.csv(
                        inFile$datapath,
                        header = input$header,
                        sep = input$sep,
                        quote = input$quote, stringsAsFactors=FALSE, fileEncoding="latin1"
                )
                
                Comunas <- covid19 %>% select(Comuna) %>% arrange(Comuna) %>% unique()
                selectInput("Comuna", "Seleccione la Comuna:", choices = Comunas)
        })
        
        
        output$plot3 <- renderPlot({
                
                theme_set(theme_bw())
                a <- c()
                
                
                inFile <- input$file1
                
                if (is.null(inFile))
                        return(NULL)
                
                covid19 <- read.csv(
                        inFile$datapath,
                        header = input$header,
                        sep = input$sep,
                        quote = input$quote, stringsAsFactors=FALSE, fileEncoding="latin1"
                )
                
                covid19$Fecha <- excel_numeric_to_date(covid19$Fecha)
                Comunas <- covid19 %>% select(Comuna) %>% arrange(Comuna) %>% unique()
                selectInput("Comuna", "Seleccione la Comuna:", choices = Comunas)
                
                
                
                comuna_en_cuestion <- covid19 %>%  filter(Comuna == input$Comuna) %>% filter(between(Fecha, input$date1, input$date2))
                
                longitud = nrow(comuna_en_cuestion)
                
                # comuna_en_cuestion = data.frame(comuna_en_cuestion)
                
                
                # population <- 10000
                population = as.numeric(comuna_en_cuestion$Poblacion) %>% unique()
                
                # population = comuna_en_cuestion$Poblacion %>% unique()
                
                
                infectados.por.dia = aggregate(comuna_en_cuestion$Casos_Diarios ~ comuna_en_cuestion$Fecha, FUN =
                                                       sum)
                
                fallecidos.por.dia = aggregate(comuna_en_cuestion$Fallecidos_Diarios ~ comuna_en_cuestion$Fecha, FUN = sum)
                infectados.por.dia2 =infectados.por.dia[,2] + fallecidos.por.dia[,2]
                
                recuperados.por.dia = aggregate(comuna_en_cuestion$Recuperados_Diarios ~
                                                        comuna_en_cuestion$Fecha, FUN = sum)
                
                
                susceptibles.por.dia = population - infectados.por.dia2 - recuperados.por.dia[,2]
                
                
                
                tabla.comuna = data.frame(unique(comuna_en_cuestion$Fecha), susceptibles.por.dia, infectados.por.dia2,recuperados.por.dia[,2])
                
                names(tabla.comuna) = c("Fecha", "Susceptibles", "Infectados", "Recuperados")
                
                x = tabla.comuna$Recuperados
                y = population*log(tabla.comuna$Susceptibles)
                summary(lm(y~x))
                
                m <- seq(2, longitud)
                
                for(i in m) {
                        xx<- x[1:i]
                        yy<- y[1:i]
                        estimacion.R0 = -summary(lm(yy ~ xx))$coefficients[2]
                        
                        a[i] <- estimacion.R0
                }
                
                
                
               
                
                com <- comuna_en_cuestion$Comuna %>% unique()
                
                x <-  1:longitud
                
                eee <- data.frame(x,a)
                
                output$tableR0 <- renderTable(eee)
                
                p <- ggplot(
                        eee,
                        aes(x, a)
                ) + geom_line(colour="#ff0000" , size=1) +
                        labs(x = "Día de la infección", y = "R_0") +
                        theme(legend.position = "top")
                p + geom_line() + geom_point(colour ="#CC6666") + transition_reveal(x)
                p

                
                

                
                
                
                
                
        })
        
        
        
        
        
        output$plot4 <- renderPlotly({
                
                inFile <- input$file1
                
                if (is.null(inFile))
                        
                        return(NULL)
                
                covid19 <- read.csv(
                        inFile$datapath,
                        header = input$header,
                        sep = input$sep,
                        quote = input$quote, stringsAsFactors=FALSE, fileEncoding="latin1"
                )
                
                covid19$Fecha <- excel_numeric_to_date(covid19$Fecha)
                
                Comunas <- covid19 %>% select(Comuna) %>% arrange(Comuna) %>% unique()
                
                selectInput("Comuna", "Seleccione la Comuna:", choices = Comunas)
                
                covid19_graficas <- covid19 %>%  filter(Comuna == input$Comuna) %>% filter(between(Fecha, input$date1, input$date2))
                
                # comun <- input$Comuna
                
                lafecha <- covid19_graficas$Fecha
                muertes_acum <- covid19_graficas$Muertes.Acumuladas
                casos_activos_acum <- covid19_graficas$Casos.Activos
                recuperados_acum <- covid19_graficas$Recuperados.Acumulados
                casos_acumulados <- covid19_graficas$Casos.Acumulados
                
                # se construye un dataframe
                data <- data.frame(lafecha, muertes_acum, casos_activos_acum, recuperados_acum, casos_acumulados)
                
                # se define el grafico padre:
                fig <- plot_ly(data, x = ~lafecha, y = ~muertes_acum, type = 'bar', name = 'Muertes acumuladas',
                               marker = list(color = 'rgb(0, 0, 0)')) %>%   
                        layout(margin = list(l = 0, r = 0, b = 50, t = 90, pad = 4))
                
                # # Barras de casos_activos
                fig <- fig %>% add_trace(y = ~casos_activos_acum, name = 'Casos activos', marker = list(color = 'rgb(0, 0, 255)'))
                # Barras de recuperados_acum
                fig <- fig %>% add_trace(y = ~recuperados_acum, name = 'Casos recuperados acumulados', marker = list(color = 'rgb(0, 128, 0)'))
                
                # se anade una linea:
                fig <- fig %>% add_trace(x = ~lafecha, y = ~casos_acumulados, name = 'Casos acumulados', type = 'scatter', mode = 'lines', line = list(color = 'rgb(255, 0, 0)', width = 1))
                
                fig <- fig %>% layout(title = 'Analisis del Covid19 en las comunas de Chile',
                                      xaxis = list(
                                              title = "",
                                              tickfont = list(
                                                      size = 14,
                                                      color = 'rgb(107, 107, 107)')),
                                      yaxis = list(
                                              title = 'Casos',
                                              titlefont = list(
                                                      size = 16,
                                                      color = 'rgb(107, 107, 107)'),
                                              tickfont = list(
                                                      size = 14,
                                                      color = 'rgb(107, 107, 107)')),
                                      legend = list(x = 0, y = 1, bgcolor = 'rgba(255, 255, 255, 0)', bordercolor = 'rgba(255, 255, 255, 0)'),
                                      barmode = 'group', bargap = 0.15, bargroupgap = 0.1)
                fig
                
        })

        
        # Gráfica de la evolución de la pandemia por cada 100.000 hbtes.
        output$plot5 <- renderPlotly({

                inFile <- input$file1
                
                if (is.null(inFile))
                        return(NULL)
                
                covid19 <- read.csv(
                        inFile$datapath,
                        header = input$header,
                        sep = input$sep,
                        quote = input$quote, stringsAsFactors=FALSE, fileEncoding="latin1"
                )
                
                covid19$Fecha <- excel_numeric_to_date(covid19$Fecha)
                
                Comunas <- covid19 %>% select(Comuna) %>% arrange(Comuna) %>% unique()
                selectInput("Comuna", "Seleccione la Comuna:", choices = Comunas)
                
                covid19_graficas <- covid19 %>%  filter(Comuna == input$Comuna) %>% filter(between(Fecha, input$date1, input$date2))
                population = as.numeric(covid19_graficas$Poblacion) %>% unique()
                # comun <- input$Comuna
                
                lafecha <- covid19_graficas$Fecha
                
                
                muertes_acum <- covid19_graficas$Muertes.Acumuladas*100000/population
                
                
                casos_activos_acum <- covid19_graficas$Casos.Activos*100000/population
                recuperados_acum <- covid19_graficas$Recuperados.Acumulados*100000/population
                casos_acumulados <- covid19_graficas$Casos.Acumulados*100000/population
                
                # se construye un dataframe
                data <- data.frame(lafecha, muertes_acum, casos_activos_acum, recuperados_acum, casos_acumulados)
                
                # se define el grafico padre:
                fig <- plot_ly(data, x = ~lafecha, y = ~muertes_acum, type = 'bar', name = 'Muertes acumuladas',
                               marker = list(color = 'rgb(0, 0, 0)')) %>%   
                        layout(margin = list(l = 0, r = 0, b = 50, t = 90, pad = 4))
                
                # # Barras de casos_activos_acum
                fig <- fig %>% add_trace(y = ~casos_activos_acum, name = 'Casos activos', marker = list(color = 'rgb(0, 0, 255)'))
                # Barras de recuperados_acum
                fig <- fig %>% add_trace(y = ~recuperados_acum, name = 'Casos recuperados acumulados', marker = list(color = 'rgb(0, 128, 0)'))
                
                # se anade una linea:
                fig <- fig %>% add_trace(x = ~lafecha, y = ~casos_acumulados, name = 'Casos acumulados', type = 'scatter', mode = 'lines', line = list(color = 'rgb(255, 0, 0)', width = 1))
                
                fig <- fig %>% layout(title = 'Analisis del Covid19 en las comunas de Chile',
                                      xaxis = list(
                                              title = "",
                                              tickfont = list(
                                                      size = 14,
                                                      color = 'rgb(107, 107, 107)')),
                                      yaxis = list(
                                              title = 'Casos',
                                              titlefont = list(
                                                      size = 16,
                                                      color = 'rgb(107, 107, 107)'),
                                              tickfont = list(
                                                      size = 14,
                                                      color = 'rgb(107, 107, 107)')),
                                      legend = list(x = 0, y = 1, bgcolor = 'rgba(255, 255, 255, 0)', bordercolor = 'rgba(255, 255, 255, 0)'),
                                      barmode = 'group', bargap = 0.15, bargroupgap = 0.1)
                fig
                
        })
        
        
        
})




# output$Comuna <- renderUI({
#         Comunas <- mydata() %>% select(Comuna) %>% arrange(Comuna) %>% unique()
#         selectInput("Comuna", "Seleccione la Comuna:", choices = Comunas)
# })





# mydata <- reactive({
#         # Cargamos la base de datos de DataIntelligence
#         covid19 <- covid19x
#         # covid19cod_comun <-
#         #   read_xlsx("data_20_mayo_sec.xlsx")
#         # Arreglamos el formato de la fecha:
#         covid19$Fecha <- excel_numeric_to_date(covid19$Fecha)
#         return(covid19 %>% filter(between(Fecha, input$date1, input$date2)))
#         
#         output$cons <- downloadHandler(
#                 filename = function() {
#                         paste(input$Comuna, Sys.Date(), ".csv", sep = "")
#                 },
#                 content = function(file) {
#                         write.csv(file %>% filter(Comuna == input$Comuna), file)
#                 }
#         )
#         
#         
# 
# })


