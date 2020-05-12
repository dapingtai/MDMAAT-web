output$body <- renderUI({
  if (USER$Logged) {
    tabItems(
      tabItem(tabName = "HOME",class="active",
              fluidRow(
                box(width = 12, title = strong("WELCOME TO MIB PROJECTS AND DATABASES WEBAPP"), collapsible = FALSE, status = "success", solidHeader = TRUE,
                    column(12, offset = 1, img(src='MIB.png', align = "center", width = '80%')),
                    h4('This web app is created for tracking the project progress and providing a simple viewer for the processed data.
                       Each project page consists of five panels:'),
                    h4(strong('1. OVERVIEW')), 
                    h4('It shows the Title, Study aims, Strategy of analysis, Tentative finding, participants and progress of the project.'),
                    h4(strong('2. PLATFORMS')), 
                    h4('It shows some QC figures for the data. You can select which platform of the data you want to view.'),
                    h4(strong('3. SAMPLES')), 
                    h4('It shows the list of samples and all the information of them. You can select multiple samples you want to view.'),
                    h4(strong('4. FEATURES')), 
                    h4('It shows the list of features information from the platform you selected. You can select one feature you want to view.'),
                    h4(strong('5. DATA VIEWER')), 
                    h4('It shows the data of the feature from the samples you selected. You can sort or color the data by any sample variable.'),
                    p('Contact Tzu-Hang Yuan ',
                      a('yuan.tzu.h@gmail.com'), 
                      ' for progress update'), 
                    p('Contact Sheng-Hui Lu ', 
                      a('shlu@stat.sinica.edu.tw'), 
                      ' for technical issues'), 
                    p('Contact Hao Ho ', 
                      a('hho@stat.sinica.edu.tw'), 
                      ' for other issues')
                    )
              ),
              p('2019 Mathematics In Biology Lab', align = 'right'),
              p('Institute of Statistical Science Academia Sinica', align = 'right')
      ),
      ######################
      ### TEMPLATE
      ######################
      tabItem(
        tabName = "TEMPLATE",
        h2(strong("TEMPLATE")),
        fluidRow(
          box(collapsible = TRUE,
              title = strong("OVERVIEW"), width = 12, solidHeader = TRUE, status = "warning",
              p(strong('Title:')), 
              p(textOutput('TEMPLATE.title')),
              p(strong('Study aims:')), 
              p(textOutput('TEMPLATE_aim')),
              p(strong('Strategy of analysis:')), 
              p(textOutput('TEMPLATE_strategy')),
              p(strong('Tentative findings:')), 
              p(textOutput('TEMPLATE_finding')),
              p(strong('Participants:')), 
              p(textOutput('TEMPLATE_participant')),
              p(strong('Progress:')),
              div(style = 'overflow-x: scroll', DT::dataTableOutput('TEMPLATE_progress'))
          )
        ),
        fluidRow(
          box(collapsible = TRUE,
              title = strong("PLATFROMS"), width = 12, solidHeader = TRUE, status = "primary",
              uiOutput('TEMPLATE_select_platform'),
              fluidRow(
                column(3, offset = 0, img(src='TEMPLATE/Plot/ScaleFactor.png', align = "center", width = '95%')),
                column(3, offset = 0, fluidRow( 
                  img(src='TEMPLATE/Plot/Hclust0.png', align = "center", width = '95%'),
                  img(src='TEMPLATE/Plot/PCplot0.png', align = "center", width = '95%')
                )),
                column(2, offset = 0, img(src='TEMPLATE/Plot/Density.png', align = "center", width = '90%')),
                column(3, offset = 0, fluidRow(
                  img(src='TEMPLATE/Plot/Hclust.png', align = "center", width = '95%'),
                  img(src='TEMPLATE/Plot/PCplot.png', align = "center", width = '95%')
                ))
              )
          )
        ),
        fluidRow(
          box(collapsible = TRUE,
              title = strong("SAMPLES"), width = 12, solidHeader = TRUE, status = "primary",
              div(style = 'overflow-x: scroll', DT::dataTableOutput('TEMPLATE_select_sample'))
          )
        ),
        fluidRow(
          box(collapsible = TRUE,
              title = strong("FEATURES"), width = 12, solidHeader = TRUE, status = "primary",
              div(style = 'overflow-x: scroll', DT::dataTableOutput('TEMPLATE_select_variable'))
          )
        ),
        fluidRow(
          box(collapsible = TRUE,
              title = strong("DATA VIEWER"), width = 12, solidHeader = TRUE, status = "success",
              fluidRow(
                column(6, uiOutput('TEMPLATE_orderby')),
                column(6, uiOutput('TEMPLATE_colorby'))
              ),
              plotOutput('TEMPLATE_select_data_bar'),
              div(style = 'overflow-x: scroll', DT::dataTableOutput('TEMPLATE_select_data'))
          )
        ),
        fluidRow(
          box(collapsible = TRUE,
              title = strong("GROUP COMPARISON"), width = 12, solidHeader = TRUE, status = NULL,
              fluidRow(
                column(2, actionBttn(inputId = "TEMPLATE_Select_Info", label = "How to select", style = "stretch", color = "primary")),
                column(2, actionBttn(inputId = "TEMPLATE_Download_Info", label = "How to download", style = "stretch", color = "primary"))
              ),
              tabsetPanel(
                tabPanel("Group A",
                         downloadButton("TEMPLATE_download_groupA", "Download GropA", icon = icon("download")), 
                         p(),
                         div(style = 'overflow-x: scroll', DT::dataTableOutput('TEMPLATE_select_groupA'))),
                tabPanel("Group B",
                         downloadButton("TEMPLATE_download_groupB", "Download GropB", icon = icon("download")), 
                         p(),
                         div(style = 'overflow-x: scroll', DT::dataTableOutput("TEMPLATE_select_groupB"))),
                tabPanel("Summary",
                         h2(strong("Summary")),
                         h4(textOutput("TEMPLATE_groupA_number")),
                         h4(textOutput("TEMPLATE_groupB_number")),
                         fluidRow(
                          column(3, h4(textOutput("TEMPLATE_pair_number"))),
                          column(9, h4(uiOutput("TEMPLATE_pair_check"))),
                          ),
                         uiOutput("TEMPLATE_choice_method"),
                         actionBttn(inputId = "TEMPLATE_draw", label = "Start Analysis", style = "fill", color = "primary"),
                         fluidRow(
                           column(6, h4(code("VolcanoPlot")), plotOutput("TEMPLATE_DEvolcanoplot") %>% withSpinner(color="#0dc5c1")),
                           column(6, h4(code("PlotMD")), plotOutput("TEMPLATE_DEplotMD") %>% withSpinner(color="#0dc5c1"))
                         )
                ),
                tabPanel("Result",
                         downloadButton("TEMPLATE_download_result", "Download All Result", icon = icon("download")), 
                         p(),
                         div(style = 'overflow-x: scroll', DT::dataTableOutput("TEMPLATE_select_limmaoutput"))
                )
              )
          )
        ),
        fluidRow(
          box(collapsible = TRUE,
              title = strong("GROUP COMPARISON VIEWER"), width = 12, solidHeader = TRUE, status = "success",
              tabsetPanel(
                tabPanel("Barplot",
                         fluidRow(
                            column(6, uiOutput('TEMPLATE_limma_orderby')),
                            column(6, uiOutput('TEMPLATE_limma_colorby'))
                         ),
                         verbatimTextOutput("TEMPLATE_limmadatabar_nocall"),
                         plotOutput('TEMPLATE_select_limmadata_bar')
                ),
                tabPanel("Boxplot",
                         plotlyOutput('TEMPLATE_boxplotly')
                ),
                tabPanel("Enrichment Analysis",
                         actionBttn(inputId = "TEMPLATE_start_enrichment", label = "Start Enrichment Analysis", style = "fill", color = "primary"),
                         p(),
                         fluidRow(
                            column(6,
                                fluidRow(
                                  column(1,
                                    dropdownButton(
                                      tags$h3("Go Selection"),
                                      selectInput(inputId = "TEMPLATE_ontology_choice", label = "Gene Ontology", choices = list("BP" = "BP", "CC" = "CC", "MF" = "MF")),
                                      selectInput(inputId = "TEMPLATE_GOplot_choice", label = list(icon("chart-bar"), "Plot Type"), 
                                                  choices = list("Dotplot" = "dotplot", "Barplot" = "barplot")),
                                      sliderTextInput(inputId = "TEMPLATE_GO_pvalue", label = list(icon("filter"), "DE Filter(Pvalue)"), choices = c(0.05, 0.01, 0.005, 0.001), grid = TRUE),
                                      sliderInput(inputId = "TEMPLATE_GO_cutoff", label = list(icon("cut"), "GO PvalueCutoff"), min = 0.01, max = 1, value = 0.05),
                                      circle = TRUE, status = "danger", size = "sm",
                                      icon = icon("cogs"), width = "300px",
                                      tooltip = tooltipOptions(title = "Click to check Go selection !")
                                    )
                                  ),
                                  column(5, h3(code("EnrichmentGO")))
                                ),
                                plotOutput('TEMPLATE_enrichGO_dotplot') %>% withSpinner(color="#FF5511")
                            ),
                            column(6,
                                fluidRow(
                                  column(1,
                                    dropdownButton(
                                      tags$h3("KEGG Selection"),
                                      selectInput(inputId = "TEMPLATE_KEGG_choice", label = "Type", choices = list("KEGG" = "kegg")),
                                      selectInput(inputId = "TEMPLATE_KEGGplot_choice", label = list(icon("chart-bar"), "Plot Type"), 
                                                  choices = list("Dotplot" = "dotplot", "Barplot" = "barplot")),
                                      sliderTextInput(inputId = "TEMPLATE_KEGG_pvalue", label = list(icon("filter"), "DE Filter(Pvalue)"), choices = c(0.05, 0.01, 0.005, 0.001), grid = TRUE),
                                      sliderInput(inputId = "TEMPLATE_KEGG_cutoff", label = list(icon("cut"), "KEGG PvalueCutoff"), min = 0.01, max = 1, value = 0.05),  
                                      circle = TRUE, status = "danger", size = "sm",
                                      icon = icon("cogs"), width = "300px",
                                      tooltip = tooltipOptions(title = "Click to check KEGG selection !")
                                    )
                                  ),
                                  column(5, h3(code("EnrichmentKEGG")))
                                ),
                                plotOutput('TEMPLATE_enrichKEGG_dotplot') %>% withSpinner(color="#FF5511")
                            )   
                         )
                )
              )
          )
        ),
        p('2019 Mathematics In Biology Lab', align = 'right'),
        p('Institute of Statistical Science Academia Sinica', align = 'right')
      ),
      ######################
      ### Resetpw
      ######################
      tabItem(tabName = "Resetpwd",
           #column(12,h2(strong("Change Password"))),
           ####title = strong("Hi, ",input$userName)
              fluidRow(
                box(width = 6,
                    title = strong("Reset password"), status = "success",solidHeader = TRUE,
                    h4(strong("Username: ",input$userName)),
                    passwordInput("Currentpw",label=h4(strong("Current Password:")),value=""),
                    passwordInput("Newpw",label=h4(strong("New Password:")),value=""),
                    passwordInput("Confirmpw",label=h4(strong("Confirm New Password:")),value=""),
                    actionBttn(size = 'sm', style = 'fill', "Updatepasswd", "Update Password"),
                    h4(textOutput("Comparepw"), style="color:red")
                )
              )
      )
    )
  } else {
    fluidRow(
      column(12,
             fluidRow(
               box(width = 12, title = strong("WELCOME TO MIB PROJECTS AND DATABASES WEBAPP"), collapsible = FALSE, status = "success", solidHeader = TRUE,
                   column(12, offset = 1, img(src='MIB.png', align = "center", width = '80%')),
                   h4('This web app is created for tracking the project progress and providing a simple viewer for the processed data.
                       Each project page consists of five panels:'),
                   h4(strong('1. OVERVIEW')), 
                   h4('It shows the Title, Study aims, Strategy of analysis, Tentative finding, participants and progress of the project.'),
                   h4(strong('2. PLATFORMS')), 
                   h4('It shows some QC figures for the data. You can select which platform of the data you want to view.'),
                   h4(strong('3. SAMPLES')), 
                   h4('It shows the list of samples and all the information of them. You can select multiple samples you want to view.'),
                   h4(strong('4. FEATURES')), 
                   h4('It shows the list of features information from the platform you selected. You can select one feature you want to view.'),
                   h4(strong('5. DATA VIEWER')), 
                   h4('It shows the data of the feature from the samples you selected. You can sort or color the data by any sample variable.'),
                   p('Contact Tzu-Hang Yuan ',
                     a('yuan.tzu.h@gmail.com'), 
                     ' for progress update'), 
                   p('Contact Sheng-Hui Lu ', 
                     a('shlu@stat.sinica.edu.tw'), 
                     ' for technical issues'), 
                   p('Contact Hao Ho ', 
                     a('hho@stat.sinica.edu.tw'), 
                     ' for other issues')
               )
             ),
             p('2019 Mathematics In Biology Lab', align = 'right'),
             p('Institute of Statistical Science Academia Sinica', align = 'right'))
    )
  }
})

