################################################################################################################
# ESPAÑOL
# COBIApp - Convertir datos de monitoreo a diferentes formatos
#
# EL CÓDIGO
# El código que permite que la aplicación se desarrolle está escrito en R. El paquete `shiny` permite escribir
# en lenguaje común de R y transforma lo que diseñamos en lenguaje html. El código se compone de 3 partes
# principales: ui, server y shinyApp. En ui se diseña la "user interface". Aquí solamente entran en juego bloques
# de programación que dan la apariencia a la aplicación y crean los diversos campos de entrada y salida. Esta
# sección solamente genera el ambiente de trabajo que ve el usuario. server es la sección del código que hace
# los cálculos, manipulaciones y procesos necesarios. En este caso, la transformación de formatos se lleva a
# a cabo en esta sección. Finalmente, la sección de shinyApp "conecta" al ambiente de trabajo con los procesos
# que debe realizar. El código se comenta en inglés.
#
# LA APLICACIÓN
# La aplicación se desarrolla con el propósito de proveer un medio eficiente para convertir datos entre formatos
# pre-establecidos. Cada oficina de COBI (o cada persona) suele preferir utilizar diferentes formatos para
# analizar sus datos rápidamente. Mientras que para análisis rigurosos se debe mantener un formato unificado,
# en ocasiones es útil contar con formatos distintos que permitan realizar análisis más específicos. Un ejemplo
# de este tipo de análisis es aquellos que se presentan en las asambleas a los pescadores.
#
# CRÉDITOS
# Licencia: MIT.
# Desarrollada y mantenida por Juan Carlos Villaseñor Derbez (jvillasenor@bren.ucsb.edu)
# 
# ENGLISH
# COBIApp - Converts monitoring data across multiple formats
# 
# CODE
# This code is writen in R. The package `shiny` allows you to write in simple R languaje and then transforms it
# to html code. The code is made up by three main parts: ui, server, and shinyApp. ui is where we design the user
# interface. Here we just provide the building blocks that give the appearance to our app by creating the input
# and output fields.This section only generates the workplace that the user sees. server is the section of code
# that makes the calculations, manipulations, and processes. In this case, the transformation of formates takes
# place in this section. Finaly, the shinyApp section connects the user interface and the processes (ui and
# server).
# 
# THIS APP
# This app was developed seeking to provide a fast mathod to convert databases across standardized formats.
# Each of the COBI offices (or personnel) tend to preffer different formats to perform analysis on their data.
# While data for rigurous analysis must have a single, unified format, this app allows users to have data in 
# a different format needed to perforsm fast and short analyses. An example of this analysis is what COBI
# presents to fishers in the yearly "Asambleas".
# 
# CREDITS
# License: MIT
# Developed and maintained by: Juan Carlos Villaseñor Derbez (jvillasenor@bren.ucsb.edu)
#
################################################################################################################

library(shiny)  # Load shiny package
library(readxl) # Load package to read excel files
library(tidyr)  # Load tidyr package
library(dplyr)  # Load dplyr package
library(MPAtools) # Load MPAtools package
library(reshape) # Load reshape library

# Generate the usier interface with ui
ui <- fluidPage(                                                             # Page can be used in different devices
  theme = "cerulean.css",
  titlePanel("COBIApp - Convertir datos de monitoreo a diferentes formatos (BETA)"),# Title for the page
  sidebarLayout(                                                             # Establishes a layout
    sidebarPanel(                                                            # Creates a sidebar
      h2("Opciones de Entrada"),                                             # Header 1
      # Input format field
      selectInput(inputId='tipoin',                                         # Set an input Id
                  label='Formato de entrada',                               # Label that user sees
                  choices=c("A",                                            # Format A
                            "B",                                            # Format B
                            "C"),                                           # Format C
                  "A"),                                                     # Set A as default
      # Input separator
      selectInput(inputId='sepin',                                          # Establish an input dd
                   label='Separador',                                        # Label that user sees
                   choices=c("Coma"=',',                                     # Establish allowed separators
                             "Punto y coma"=';',                             # Semicolon
                             "Tabulación"='\t',                              # Tab
                             "Espacio"=" "),                                 # Space
                   ','),                                                     # Default to comma
      h2("Opciones de Salida"),                                              # Header 2
      # Output format field
      selectInput(inputId='tipoout',                                         # Establish input Id
                  label='Formato de salida',                                # Label that user sees
                  choices=c("A",                                            # Format A
                            "B",                                            # Format B
                            "C"),                                           # Format C
                  "A"),                                                     # default to A
      # Input file field
      fileInput(inputId ="dataset",                                          # Establish an input Id
                label = "Seleccionar archivo",                               # Label that a user sees
                accept = c(                                                  # Establish allowed file formats
                  ".csv",                                                    # allow csv format
                  ".tsv",                                                    # allow tsv format
                  ".xls",                                                    # allow xcel format (old)
                  ".xlsx"                                                    # allow excel format (new)
                )),
     h2("Ejemplos de Formatos"),                                       # Label for tab
      downloadButton('downloadA',                                # Button to download data
                     'Formato A'),
      downloadButton('downloadB',
                     'Formato B'),
      downloadButton('downloadC',
                     'Formato C'),
     p("Se descargan en formato de valores separados por coma (*.csv). Al abrirlos en Excel:"),
     p("1) seleccionar la columna A"),
     p("2) Ir a la pestaña de 'Datos'"),
     p("3) Seleccionar 'Texto en columnas'"),
     p("4) Seleccionar 'delimitados' y dar click en siguiente"),
     p("5) Seleccionar 'coma' y dar click en finalizar"),
     p(),
     a("Ver el manual", href="http://jcvdav.github.io/COBIApp_Manual.pdf", target="_blank")
      ),
    # Main panel structure
    mainPanel(h2("Vista Previa de Datos de Salida"),                                               # Label for tab
               sliderInput(inputId="filas",
                           label="Indique número de filas",
                           min=0,
                           max=100,
                           value=10),
               tableOutput("table"),                                         # Generate field for the output
               downloadButton('downloadData',                                # Button to download data
                              'Descargar')                                  # Label of button
      )
    )
  )

options(shiny.maxRequestSize = 100*1024^2)                                   #Tamaño máximo para archivos 100 Mb.
  

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  FormatoA=read.csv("www/A.csv", sep=";")
  FormatoB=read.csv("www/B.csv", sep=";")
  FormatoC=read.csv("www/C.csv", sep=";")
  
  output$downloadA <- downloadHandler(
    filename = function(){paste("FormatoA", ".csv")},
    content = function(file) {
      write.csv(FormatoA, file, row.names=F)
    }
  )
  
  output$downloadB <- downloadHandler(
    filename = function(){paste("FormatoB", ".csv")},
    content = function(file) {
      write.csv(FormatoB, file, row.names=F)
    }
  )
  
  output$downloadC <- downloadHandler(
    filename = function(){paste("FormatoC", ".csv")},
    content = function(file) {
      write.csv(FormatoC, file, row.names=F)
    }
  )
  
  
  datasetInput=reactive({
    inFile <- input$dataset
    if (is.null(inFile))
      return(NULL)
    dataset=read.csv(inFile$datapath, sep = input$sepin)
    
    if (input$tipoin=="A"){
      a = dataset
      if (input$tipoout=="A"){
        return(a)
      } else if (input$tipoout=="B"){
        a2b(a)
      } else if (input$tipoout=="C"){
        a2c(a)
      }
    }  else if (input$tipoin=="B"){
      b=dataset
      if (input$tipoout=="A"){
        b2a(b)
      } else if (input$tipoout=="B"){
        return(b)
      } else if (input$tipoout=="C"){
        b2c(b)
      }
     } else if (input$tipoin=="C"){
      c=dataset
      if (input$tipoout=="A"){
        c2a(c)
      } else if (input$tipoout=="B"){
        c2b(c)
      } else if (input$tipoout=="C"){
        return(c)
      }
    }
    
  })
  output$table <- renderTable({
    head(datasetInput(), input$filas)
  })
  
  output$downloadData <- downloadHandler(
    filename = function(){paste(input$dataset)},
    content = function(file) {
      write.csv(datasetInput(), file, row.names=F)
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)

