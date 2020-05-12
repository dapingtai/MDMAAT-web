server <- function(input, output, session) {
  ######################
  ### Server Setting
  ######################
  setwd('/srv/shiny-server/MDMAAT/')
  library(ggplot2)
  library(RMySQL)
  if (length(dbListConnections( dbDriver( drv = "MySQL"))) > 10) {
    lapply( dbListConnections( dbDriver( drv = "MySQL"))[-(1:10)], dbDisconnect)
  }
  connect = dbConnect(MySQL(), dbname = "MicroarrayData", user = "root", password = "root", host = "localhost")
  mydb <- dbListTables(connect)
  ######################
  ### header
  ######################
  load('Rdata/affy.info.Rdata')
  load('Rdata/meth.info.Rdata')
  load('Rdata/m430.info.Rdata')
  load('Rdata/mepic.info.Rdata')
  load('Rdata/scrna.info.Rdata')
  ######################
  ### LOGIN
  ######################
  source('login.R', local = TRUE)
  ######################
  ### SIDEBAR
  ######################
  source('sidebar.R', local = TRUE)
  ######################
  ### BODY
  ######################
  source('body.R', local = TRUE)
  ######################
  ### PROJECTS
  ######################
  source('projects/EXAMPLE', local = TRUE)
  source('projects/EXAMPLE2', local = TRUE)
  source('projects/EXAMPLE3', local = TRUE)
  source('projects/EXAMPLE4', local = TRUE)
  ######################
  ### ResetPassword
  ######################
  source('changepw.R', local = TRUE)
  
  session$allowReconnect(TRUE)
}
