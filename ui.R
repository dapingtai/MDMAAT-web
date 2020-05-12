library(shinydashboard)
library(shinycssloaders)
library(shinyWidgets)
library(plotly)
library(RMySQL)
library(RSQLite)
library(dplyr)
library(ggplot2)
library(crosstalk)
library(visNetwork)
library(igraph)
library(survival)
library(reshape2)
library(digest)
library(limma)
library(waiter)
library(matrixStats)
library(clusterProfiler)
library(org.Hs.eg.db)
library(DOSE)
library(readxl)
######################
### header
######################
header <- dashboardHeader(
  title = tagList(
    tags$span(
      class = "logo-lg", "MIB PROJECTS"
    ),
    tags$span(
      class = "logo-mini", "M"
    )
  ),
  titleWidth = 350
)

######################
### SIDEBAR
######################
sidebar <- dashboardSidebar(width = 350,
                            menuItemOutput('sidebarpanel')
)
######################
### BODY
######################
body <- dashboardBody(
  tags$script(HTML("$('body').addClass('sidebar-mini');")),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ),
  useSweetAlert(),
  use_waitress(),
  uiOutput('body')
)
######################
### put together
######################
ui <- dashboardPage(skin = "blue", title = "MDMAAT",
                    header, sidebar, body)
