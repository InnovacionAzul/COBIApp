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
library(tidyr)
library(dplyr)

# Generate the usier interface with ui
ui <- fluidPage(                                                             # Page can be used in different devices
  theme = "cerulean.css",
  titlePanel("COBIApp - Convertir datos de monitoreo a diferentes formatos"),# Title for the page
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
                ))
      ),
    # Main panel structure
    mainPanel(
      tabsetPanel(                                                           # Wraps multiple tabs
      tabPanel("Ejemplos de Formatos",                                       # Label for tab
               img(src="formatos.jpg",                                       # Load image of example
                   width="600px")),                                          # Set size of image
      tabPanel("Vista Previa",                                               # Label for tab
               sliderInput(inputId="filas",
                           label="Indique número de filas",
                           min=0,
                           max=100,
                           value=10),
               tableOutput("table"),                                         # Generate field for the output
               downloadButton('downloadData',                                # Button to download data
                              'Descargar'))                                  # Label of button
      )
      )
    )
  )

options(shiny.maxRequestSize = 100*1024^2)                                   #Tamaño máximo para archivos 100 Mb.
  

# Define server logic required to draw a histogram
server <- function(input, output) {
  datasetInput=reactive({
    inFile <- input$dataset
    if (is.null(inFile))
      return(NULL)
    dataset=read.csv(inFile$datapath, sep = input$sepin)
    
    if (input$tipoin=="A"){
      a = dataset
      if (input$tipoout=="A"){
        a=a
        return(a)
      } else if (input$tipoout=="B"){
        b=a
        b$row=1:nrow(b)
        b=b%>%
          select(-Total) %>%
          spread(Talla, X.100) %>%
          select(-row) %>%
          gather(ClaseTalla, Abundancia, -c(1:23)) %>%
          filter(Abundancia>0)
        #Las líneas de abajo asignan los nombres correctos a las celdas, e inlcuyen los promedios que deben de ser utilizados:
        
        ## Lo hacemos para la columna Talla (la que se usa en el análisis)
        b$Talla=as.numeric(b$ClaseTalla)
        b$Talla[b$ClaseTalla=="X0a5"]=2.5
        b$Talla[b$ClaseTalla=="X6a10"]=8.5
        b$Talla[b$ClaseTalla=="X11a20"]=15.5
        b$Talla[b$ClaseTalla=="X21a30"]=25.5
        b$Talla[b$ClaseTalla=="X31a40"]=35.5
        
        ## Y lo hacemos para la columna ClaseTalla
        b$ClaseTalla[b$ClaseTalla=="X0a5"]="0a5"
        b$ClaseTalla[b$ClaseTalla=="X6a10"]="6a10"
        b$ClaseTalla[b$ClaseTalla=="X11a20"]="11a20"
        b$ClaseTalla[b$ClaseTalla=="X21a30"]="21a30"
        b$ClaseTalla[b$ClaseTalla=="X31a40"]="31a40"
        b$ClaseTalla[b$Talla>=41]=">40"
        
        return(b)
        
      } else if (input$tipoout=="C"){
        
      }
    } #else if (input$tipoin=="B"){
    #   x[1]="B"
    #   if (input$tipoout=="A"){
    #     x[2]="A"
    #   } else if (input$tipoout=="B"){
    #     x[2]="B"
    #   } else if (input$tipoout=="C"){
    #     x[2]="C"
    #   }
    # } else if (input$tipoin=="C"){
    #   x[1]="C"
    #   if (input$tipoout=="A"){
    #     x[2]="A"
    #   } else if (input$tipoout=="B"){
    #     x[2]="B"
    #   } else if (input$tipoout=="C"){
    #     x[2]="C"
    #   }
    # }
    
  })
  output$table <- renderTable({
    head(datasetInput(), input$filas)
  })
  
  output$downloadData <- downloadHandler(
    filename = function(){paste(input$dataset)},
    content = function(file) {
      write.csv(datasetInput(), file)
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)

