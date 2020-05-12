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
  login_info <- read.table("login_info.text", row.names=1, stringsAsFactors = F, sep="=")
  connect = dbConnect(MySQL(), dbname = login_info["arraydb",], user = login_info["user",], password = login_info["password",], host = login_info["host",])
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
  source('projects/EXAMPLE.R', local = TRUE)
  ######################
  ### ResetPassword
  ######################
  source('changepw.R', local = TRUE)
  
  session$allowReconnect(TRUE)
}
