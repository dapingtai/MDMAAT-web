##############
# SIDERBAR
##############
output$TEMPLATE <- renderMenu({
  menuItem("TEMPLATE", icon = icon("database", lib = "font-awesome"), tabName = "TEMPLATE")
})

##############
# PUTTOGETHER
##############
output$sidebarpanel <- renderMenu({                
  if (USER$Logged) {
    sidebarMenu(
      menuItem("HOME", icon = icon("home", lib = "glyphicon"), tabName = "HOME"),
      menuItem("PROJECTS", icon = icon("tasks"), tabName = "PROJECTS", startExpanded = TRUE,
               menuItemOutput('TEMPLATE')
      ),
      menuItem("CHANGE PASSWORD", icon = icon("user-secret", lib = "font-awesome"), tabName = "Resetpwd", startExpanded = FALSE),
      sidebarUserPanel(
        span("You have logged in as ", strong(isolate(input$userName))),
        subtitle = a(icon("sign-out"), "SIGN OUT", href = login.page)
      )
    )
  } else {
    sidebarMenu(
      menuItem("LOGIN", icon = icon("sign-in"), tabName = "HOME", selected = TRUE),
      textInput("userName", span(' ', tagList(icon("user", lib = "font-awesome"), "   Username"))),
      passwordInput("passwd", span(' ', tagList(icon("key", lib = "font-awesome"), "Password"))),
      actionBttn(size = 'sm', style = 'fill', "Login", "Log in"),
      h4(textOutput("pass"))
    )
  }
})
