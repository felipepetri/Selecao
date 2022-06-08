library(shiny)
library(shinydashboard)
library(tidyverse)
library(readxl)
library(rgdal)
library(lubridate)
library(kableExtra)
library(ggpubr)
library(plotly)
library(rlist)
library(DT)
library(janitor)
library(arules)
library(httr)
library(jsonlite)
library(shinyjs)

# Define UI for application that draws a histogram
dashboardPage(
  dashboardHeader(title="Dash PS Oper"),
  dashboardSidebar(
    useShinyjs(),
    sidebarMenu(
      menuItem("Clientes por região/Cidade",tabName="clientes",icon=icon("globe-americas")),
      menuItem("Fanquias",tabName="franquias",icon=icon("building")),
      menuItem("Desafio",tabName = "desafio",icon=icon("dumbbell")),
      downloadButton("download","Baixar Relatório Completo"),
      # menuItem("Calls por Analista",tabName = "analista",icon=icon("user-friends"),
      #          selectInput("intervalo2","Selecionar Período",
      #                      choices=list("Semanal"="weeks",
      #                                   "Mensal"="months",
      #                                   "Trimestral"="3*months",
      #                                   "Semestral"="6*months",
      #                                   "Anual"="years")),
      #          actionButton("butao2","Aplicar Alterações")),
      tags$head(tags$style(HTML('
        /* logo */
        .skin-blue .main-header .logo {
                              background-color: #013672;
                              }

        /* logo when hovered */
        .skin-blue .main-header .logo:hover {
                              background-color: #013672;
                              }

        /* navbar (rest of the header) */
        .skin-blue .main-header .navbar {
                              background-color: #013672;
                              }        

        /* main sidebar */
        .skin-blue .main-sidebar {
                              background-color: #013672;
                              }

        /* active selected tab in the sidebarmenu */
        .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                              background-color: #013672;
                              }

        /* other links in the sidebarmenu */
        .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                              background-color: #013672;
                              color: white;
                              }

        /* other links in the sidebarmenu when hovered */
         .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
                              background-color: #00AF47;
                              }
        /* toggle button when hovered  */                    
         .skin-blue .main-header .navbar .sidebar-toggle:hover{
                              background-color: #00AF47;
                              }
        
        /* Abilitar scroll */                      
        .content-wrapper { overflow: auto; }')))
    )
  ),
  dashboardBody(
    tabItems(tabItem(tabName="clientes",
                    "Por Cidade",
                    plotlyOutput("mapa",
                                 height="600px"),
                    "Por Região",
                    plotOutput("clientes_regiao",
                               height="600px")),
           tabItem(tabName="franquias",
                    "",
                    dataTableOutput("franquias1"),
                    dataTableOutput("franquias2"),
                    dataTableOutput("franquias3")),
           tabItem(tabName="desafio",
                    "",
                    dataTableOutput("desafio")))
  )
)
  
