CHPW <- reactiveValues(Changedxx = F)
observe({
  if (CHPW$Changedxx == FALSE) {
    if (!is.null(input$Updatepasswd)) {
      if (input$Updatepasswd > 0) {
	Currentpw1 <- digest(isolate(input$Currentpw), "md5", serialize = FALSE)
	Newpw1 <- digest(isolate(input$Newpw), "md5", serialize = FALSE)
        Confirmpw1 <- digest(isolate(input$Confirmpw), "md5", serialize = FALSE)
	conn = dbConnect(RMySQL::MySQL(), user = login_info["user",], password = login_info["password",], host = login_info["host",], dbname = login_info["dbname",])
	GetuserPw <- paste("Select Password from User WHERE `Username` = '", input$userName ,"' ", sep = "")
	CheckPw <- dbGetQuery(conn, statement=GetuserPw)
	if (Currentpw1 == CheckPw){
           if ((isolate(input$Newpw) != '') & (isolate(input$Confirmpw) != '')){
	      if ((Currentpw1 != Newpw1)&&(Newpw1 == Confirmpw1)){
	         NewuserPW <- paste ("Update `User` SET `Password` = '",Newpw1,"' WHERE `Username`='",input$userName,"'",sep="")
	         dbSendQuery(conn,statement=NewuserPW)
		 CHPW$Changedxx <- TRUE
		 dbDisconnect(conn)
		 session$reload()
      		 return()
	      }
	   }
	}
      }
    }
  }
})

output$Comparepw <- renderText({
  oo <- ' '
  #oo <- input$Updatepasswd
  if (CHPW$Changedxx == FALSE) {
    if (!is.null(input$Updatepasswd)) {
      if (input$Updatepasswd > 0) {
        oo <- "Invalid Password or Invalid New Password!"
	#oo <- input$Updatepasswd
      }
    }
  }
  oo 
})
