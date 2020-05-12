login.page <- paste(
  isolate(session$clientData$url_protocol),
  "//",
  isolate(session$clientData$url_hostname),
  ":",
  isolate(session$clientData$url_port),
  "/MDMAAT",
  sep = ""
)

USER <- reactiveValues(Logged = F)
observe({
  if (USER$Logged == FALSE) {
    if (!is.null(input$Login)) {
      if (input$Login > 0) {
        Username <- isolate(input$userName)
        Password <- isolate(input$passwd)
        Password2 <-digest(Password, "md5", serialize = FALSE)
        con = dbConnect(RMySQL::MySQL(), user = login_info["user",], password = login_info["password",], host = login_info["host",], dbname = login_info["dbname",])
        GetuserInfo <- paste("Select uid from User WHERE `Username`= '", Username, 
                             "' and `Password`= '", Password2, 
                             "' and `MDMAAT`= '", 1, 
                             "' ", sep = "")
        Checkuid <- nrow(dbGetQuery(conn=con, statement=GetuserInfo))
        if (Checkuid != 0){
          USER$Logged <- TRUE
        }
	dbDisconnect(con)
      }
    }
  }
})

output$pass <- renderText({
  o <- ' '
  if (USER$Logged == FALSE) {
    if (!is.null(input$Login)) {
      if (input$Login > 0) {
        o <- "Invalid Username or Password!"
      }
    }
  }
  o
})
