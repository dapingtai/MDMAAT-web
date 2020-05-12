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
      ### EXAMPLE
      ######################
      tabItem(
        tabName = "EXAMPLE",
        h2(strong("EXAMPLE")),
        fluidRow(
          box(collapsible = TRUE,
              title = strong("OVERVIEW"), width = 12, solidHeader = TRUE, status = "warning",
              p(strong('Title:')), 
              p(textOutput('EXAMPLE.title')),
              p(strong('Study aims:')), 
              p(textOutput('EXAMPLE_aim')),
              p(strong('Strategy of analysis:')), 
              p(textOutput('EXAMPLE_strategy')),
              p(strong('Tentative findings:')), 
              p(textOutput('EXAMPLE_finding')),
              p(strong('Participants:')), 
              p(textOutput('EXAMPLE_participant')),
              p(strong('Progress:')),
              div(style = 'overflow-x: scroll', DT::dataTableOutput('EXAMPLE_progress'))
          )
        ),
        fluidRow(
          box(collapsible = TRUE,
              title = strong("PLATFROMS"), width = 12, solidHeader = TRUE, status = "primary",
              uiOutput('EXAMPLE_select_platform'),
              fluidRow(
                column(3, offset = 0, img(src='EXAMPLE/Plot/ScaleFactor.png', align = "center", width = '95%')),
                column(3, offset = 0, fluidRow( 
                  img(src='EXAMPLE/Plot/Hclust0.png', align = "center", width = '95%'),
                  img(src='EXAMPLE/Plot/PCplot0.png', align = "center", width = '95%')
                )),
                column(2, offset = 0, img(src='EXAMPLE/Plot/Density.png', align = "center", width = '90%')),
                column(3, offset = 0, fluidRow(
                  img(src='EXAMPLE/Plot/Hclust.png', align = "center", width = '95%'),
                  img(src='EXAMPLE/Plot/PCplot.png', align = "center", width = '95%')
                ))
              )
          )
        ),
        fluidRow(
          box(collapsible = TRUE,
              title = strong("SAMPLES"), width = 12, solidHeader = TRUE, status = "primary",
              div(style = 'overflow-x: scroll', DT::dataTableOutput('EXAMPLE_select_sample'))
          )
        ),
        fluidRow(
          box(collapsible = TRUE,
              title = strong("FEATURES"), width = 12, solidHeader = TRUE, status = "primary",
              div(style = 'overflow-x: scroll', DT::dataTableOutput('EXAMPLE_select_variable'))
          )
        ),
        fluidRow(
          box(collapsible = TRUE,
              title = strong("DATA VIEWER"), width = 12, solidHeader = TRUE, status = "success",
              fluidRow(
                column(6, uiOutput('EXAMPLE_orderby')),
                column(6, uiOutput('EXAMPLE_colorby'))
              ),
              plotOutput('EXAMPLE_select_data_bar'),
              div(style = 'overflow-x: scroll', DT::dataTableOutput('EXAMPLE_select_data'))
          )
        ),
        fluidRow(
          box(collapsible = TRUE,
              title = strong("GROUP COMPARISON"), width = 12, solidHeader = TRUE, status = NULL,
              fluidRow(
                column(2, actionBttn(inputId = "EXAMPLE_Select_Info", label = "How to select", style = "stretch", color = "primary")),
                column(2, actionBttn(inputId = "EXAMPLE_Download_Info", label = "How to download", style = "stretch", color = "primary"))
              ),
              tabsetPanel(
                tabPanel("Group A",
                         downloadButton("EXAMPLE_download_groupA", "Download GropA", icon = icon("download")), 
                         p(),
                         div(style = 'overflow-x: scroll', DT::dataTableOutput('EXAMPLE_select_groupA'))),
                tabPanel("Group B",
                         downloadButton("EXAMPLE_download_groupB", "Download GropB", icon = icon("download")), 
                         p(),
                         div(style = 'overflow-x: scroll', DT::dataTableOutput("EXAMPLE_select_groupB"))),
                tabPanel("Summary",
                         h2(strong("Summary")),
                         h4(textOutput("EXAMPLE_groupA_number")),
                         h4(textOutput("EXAMPLE_groupB_number")),
                         fluidRow(
                          column(3, h4(textOutput("EXAMPLE_pair_number"))),
                          column(9, h4(uiOutput("EXAMPLE_pair_check"))),
                          ),
                         uiOutput("EXAMPLE_choice_method"),
                         actionBttn(inputId = "EXAMPLE_draw", label = "Start Analysis", style = "fill", color = "primary"),
                         fluidRow(
                           column(6, h4(code("VolcanoPlot")), plotOutput("EXAMPLE_DEvolcanoplot") %>% withSpinner(color="#0dc5c1")),
                           column(6, h4(code("PlotMD")), plotOutput("EXAMPLE_DEplotMD") %>% withSpinner(color="#0dc5c1"))
                         )
                ),
                tabPanel("Result",
                         downloadButton("EXAMPLE_download_result", "Download All Result", icon = icon("download")), 
                         p(),
                         div(style = 'overflow-x: scroll', DT::dataTableOutput("EXAMPLE_select_limmaoutput"))
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
                            column(6, uiOutput('EXAMPLE_limma_orderby')),
                            column(6, uiOutput('EXAMPLE_limma_colorby'))
                         ),
                         verbatimTextOutput("EXAMPLE_limmadatabar_nocall"),
                         plotOutput('EXAMPLE_select_limmadata_bar')
                ),
                tabPanel("Boxplot",
                         plotlyOutput('EXAMPLE_boxplotly')
                ),
                tabPanel("Enrichment Analysis",
                         actionBttn(inputId = "EXAMPLE_start_enrichment", label = "Start Enrichment Analysis", style = "fill", color = "primary"),
                         p(),
                         fluidRow(
                            column(6,
                                fluidRow(
                                  column(1,
                                    dropdownButton(
                                      tags$h3("Go Selection"),
                                      selectInput(inputId = "EXAMPLE_ontology_choice", label = "Gene Ontology", choices = list("BP" = "BP", "CC" = "CC", "MF" = "MF")),
                                      selectInput(inputId = "EXAMPLE_GOplot_choice", label = list(icon("chart-bar"), "Plot Type"), 
                                                  choices = list("Dotplot" = "dotplot", "Barplot" = "barplot")),
                                      sliderTextInput(inputId = "EXAMPLE_GO_pvalue", label = list(icon("filter"), "DE Filter(Pvalue)"), choices = c(0.05, 0.01, 0.005, 0.001), grid = TRUE),
                                      sliderInput(inputId = "EXAMPLE_GO_cutoff", label = list(icon("cut"), "GO PvalueCutoff"), min = 0.01, max = 1, value = 0.05),
                                      circle = TRUE, status = "danger", size = "sm",
                                      icon = icon("cogs"), width = "300px",
                                      tooltip = tooltipOptions(title = "Click to check Go selection !")
                                    )
                                  ),
                                  column(5, h3(code("EnrichmentGO")))
                                ),
                                plotOutput('EXAMPLE_enrichGO_dotplot') %>% withSpinner(color="#FF5511")
                            ),
                            column(6,
                                fluidRow(
                                  column(1,
                                    dropdownButton(
                                      tags$h3("KEGG Selection"),
                                      selectInput(inputId = "EXAMPLE_KEGG_choice", label = "Type", choices = list("KEGG" = "kegg")),
                                      selectInput(inputId = "EXAMPLE_KEGGplot_choice", label = list(icon("chart-bar"), "Plot Type"), 
                                                  choices = list("Dotplot" = "dotplot", "Barplot" = "barplot")),
                                      sliderTextInput(inputId = "EXAMPLE_KEGG_pvalue", label = list(icon("filter"), "DE Filter(Pvalue)"), choices = c(0.05, 0.01, 0.005, 0.001), grid = TRUE),
                                      sliderInput(inputId = "EXAMPLE_KEGG_cutoff", label = list(icon("cut"), "KEGG PvalueCutoff"), min = 0.01, max = 1, value = 0.05),  
                                      circle = TRUE, status = "danger", size = "sm",
                                      icon = icon("cogs"), width = "300px",
                                      tooltip = tooltipOptions(title = "Click to check KEGG selection !")
                                    )
                                  ),
                                  column(5, h3(code("EnrichmentKEGG")))
                                ),
                                plotOutput('EXAMPLE_enrichKEGG_dotplot') %>% withSpinner(color="#FF5511")
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
      ### EXAMPLE2
      ######################
      tabItem(
        tabName = "EXAMPLE2",
        h2(strong("EXAMPLE2")),
        fluidRow(
          box(collapsible = TRUE,
              title = strong("OVERVIEW"), width = 12, solidHeader = TRUE, status = "warning",
              p(strong('Title:')), 
              p(textOutput('EXAMPLE2.title')),
              p(strong('Study aims:')), 
              p(textOutput('EXAMPLE2_aim')),
              p(strong('Strategy of analysis:')), 
              p(textOutput('EXAMPLE2_strategy')),
              p(strong('Tentative findings:')), 
              p(textOutput('EXAMPLE2_finding')),
              p(strong('Participants:')), 
              p(textOutput('EXAMPLE2_participant')),
              p(strong('Progress:')),
              div(style = 'overflow-x: scroll', DT::dataTableOutput('EXAMPLE2_progress'))
          )
        ),
        fluidRow(
          box(collapsible = TRUE,
              title = strong("PLATFROMS"), width = 12, solidHeader = TRUE, status = "primary",
              uiOutput('EXAMPLE2_select_platform'),
              fluidRow(
                column(3, offset = 0, img(src='EXAMPLE2/Plot/ScaleFactor.png', align = "center", width = '95%')),
                column(3, offset = 0, fluidRow( 
                  img(src='EXAMPLE2/Plot/Hclust0.png', align = "center", width = '95%'),
                  img(src='EXAMPLE2/Plot/PCplot0.png', align = "center", width = '95%')
                )),
                column(2, offset = 0, img(src='EXAMPLE2/Plot/Density.png', align = "center", width = '90%')),
                column(3, offset = 0, fluidRow(
                  img(src='EXAMPLE2/Plot/Hclust.png', align = "center", width = '95%'),
                  img(src='EXAMPLE2/Plot/PCplot.png', align = "center", width = '95%')
                ))
              )
          )
        ),
        fluidRow(
          box(collapsible = TRUE,
              title = strong("SAMPLES"), width = 12, solidHeader = TRUE, status = "primary",
              div(style = 'overflow-x: scroll', DT::dataTableOutput('EXAMPLE2_select_sample'))
          )
        ),
        fluidRow(
          box(collapsible = TRUE,
              title = strong("FEATURES"), width = 12, solidHeader = TRUE, status = "primary",
              div(style = 'overflow-x: scroll', DT::dataTableOutput('EXAMPLE2_select_variable'))
          )
        ),
        fluidRow(
          box(collapsible = TRUE,
              title = strong("DATA VIEWER"), width = 12, solidHeader = TRUE, status = "success",
              fluidRow(
                column(6, uiOutput('EXAMPLE2_orderby')),
                column(6, uiOutput('EXAMPLE2_colorby'))
              ),
              plotOutput('EXAMPLE2_select_data_bar'),
              div(style = 'overflow-x: scroll', DT::dataTableOutput('EXAMPLE2_select_data'))
          )
        ),
        fluidRow(
          box(collapsible = TRUE,
              title = strong("GROUP COMPARISON"), width = 12, solidHeader = TRUE, status = NULL,
              fluidRow(
                column(2, actionBttn(inputId = "EXAMPLE2_Select_Info", label = "How to select", style = "stretch", color = "primary")),
                column(2, actionBttn(inputId = "EXAMPLE2_Download_Info", label = "How to download", style = "stretch", color = "primary"))
              ),
              tabsetPanel(
                tabPanel("Group A",
                         downloadButton("EXAMPLE2_download_groupA", "Download GropA", icon = icon("download")), 
                         p(),
                         div(style = 'overflow-x: scroll', DT::dataTableOutput('EXAMPLE2_select_groupA'))),
                tabPanel("Group B",
                         downloadButton("EXAMPLE2_download_groupB", "Download GropB", icon = icon("download")), 
                         p(),
                         div(style = 'overflow-x: scroll', DT::dataTableOutput("EXAMPLE2_select_groupB"))),
                tabPanel("Summary",
                         h2(strong("Summary")),
                         h4(textOutput("EXAMPLE2_groupA_number")),
                         h4(textOutput("EXAMPLE2_groupB_number")),
                         fluidRow(
                          column(3, h4(textOutput("EXAMPLE2_pair_number"))),
                          column(9, h4(uiOutput("EXAMPLE2_pair_check"))),
                          ),
                         uiOutput("EXAMPLE2_choice_method"),
                         actionBttn(inputId = "EXAMPLE2_draw", label = "Start Analysis", style = "fill", color = "primary"),
                         fluidRow(
                           column(6, h4(code("VolcanoPlot")), plotOutput("EXAMPLE2_DEvolcanoplot") %>% withSpinner(color="#0dc5c1")),
                           column(6, h4(code("PlotMD")), plotOutput("EXAMPLE2_DEplotMD") %>% withSpinner(color="#0dc5c1"))
                         )
                ),
                tabPanel("Result",
                         downloadButton("EXAMPLE2_download_result", "Download All Result", icon = icon("download")), 
                         p(),
                         div(style = 'overflow-x: scroll', DT::dataTableOutput("EXAMPLE2_select_limmaoutput"))
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
                            column(6, uiOutput('EXAMPLE2_limma_orderby')),
                            column(6, uiOutput('EXAMPLE2_limma_colorby'))
                         ),
                         verbatimTextOutput("EXAMPLE2_limmadatabar_nocall"),
                         plotOutput('EXAMPLE2_select_limmadata_bar')
                ),
                tabPanel("Boxplot",
                         plotlyOutput('EXAMPLE2_boxplotly')
                ),
                tabPanel("Enrichment Analysis",
                         actionBttn(inputId = "EXAMPLE2_start_enrichment", label = "Start Enrichment Analysis", style = "fill", color = "primary"),
                         p(),
                         fluidRow(
                            column(6,
                                fluidRow(
                                  column(1,
                                    dropdownButton(
                                      tags$h3("Go Selection"),
                                      selectInput(inputId = "EXAMPLE2_ontology_choice", label = "Gene Ontology", choices = list("BP" = "BP", "CC" = "CC", "MF" = "MF")),
                                      selectInput(inputId = "EXAMPLE2_GOplot_choice", label = list(icon("chart-bar"), "Plot Type"), 
                                                  choices = list("Dotplot" = "dotplot", "Barplot" = "barplot")),
                                      sliderTextInput(inputId = "EXAMPLE2_GO_pvalue", label = list(icon("filter"), "DE Filter(Pvalue)"), choices = c(0.05, 0.01, 0.005, 0.001), grid = TRUE),
                                      sliderInput(inputId = "EXAMPLE2_GO_cutoff", label = list(icon("cut"), "GO PvalueCutoff"), min = 0.01, max = 1, value = 0.05),
                                      circle = TRUE, status = "danger", size = "sm",
                                      icon = icon("cogs"), width = "300px",
                                      tooltip = tooltipOptions(title = "Click to check Go selection !")
                                    )
                                  ),
                                  column(5, h3(code("EnrichmentGO")))
                                ),
                                plotOutput('EXAMPLE2_enrichGO_dotplot') %>% withSpinner(color="#FF5511")
                            ),
                            column(6,
                                fluidRow(
                                  column(1,
                                    dropdownButton(
                                      tags$h3("KEGG Selection"),
                                      selectInput(inputId = "EXAMPLE2_KEGG_choice", label = "Type", choices = list("KEGG" = "kegg")),
                                      selectInput(inputId = "EXAMPLE2_KEGGplot_choice", label = list(icon("chart-bar"), "Plot Type"), 
                                                  choices = list("Dotplot" = "dotplot", "Barplot" = "barplot")),
                                      sliderTextInput(inputId = "EXAMPLE2_KEGG_pvalue", label = list(icon("filter"), "DE Filter(Pvalue)"), choices = c(0.05, 0.01, 0.005, 0.001), grid = TRUE),
                                      sliderInput(inputId = "EXAMPLE2_KEGG_cutoff", label = list(icon("cut"), "KEGG PvalueCutoff"), min = 0.01, max = 1, value = 0.05),  
                                      circle = TRUE, status = "danger", size = "sm",
                                      icon = icon("cogs"), width = "300px",
                                      tooltip = tooltipOptions(title = "Click to check KEGG selection !")
                                    )
                                  ),
                                  column(5, h3(code("EnrichmentKEGG")))
                                ),
                                plotOutput('EXAMPLE2_enrichKEGG_dotplot') %>% withSpinner(color="#FF5511")
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
      ### EXAMPLE3
      ######################
      tabItem(
        tabName = "EXAMPLE3",
        h2(strong("EXAMPLE3")),
        fluidRow(
          box(collapsible = TRUE,
              title = strong("OVERVIEW"), width = 12, solidHeader = TRUE, status = "warning",
              p(strong('Title:')), 
              p(textOutput('EXAMPLE3.title')),
              p(strong('Study aims:')), 
              p(textOutput('EXAMPLE3_aim')),
              p(strong('Strategy of analysis:')), 
              p(textOutput('EXAMPLE3_strategy')),
              p(strong('Tentative findings:')), 
              p(textOutput('EXAMPLE3_finding')),
              p(strong('Participants:')), 
              p(textOutput('EXAMPLE3_participant')),
              p(strong('Progress:')),
              div(style = 'overflow-x: scroll', DT::dataTableOutput('EXAMPLE3_progress'))
          )
        ),
        fluidRow(
          box(collapsible = TRUE,
              title = strong("PLATFROMS"), width = 12, solidHeader = TRUE, status = "primary",
              uiOutput('EXAMPLE3_select_platform'),
              fluidRow(
                column(3, offset = 0, img(src='EXAMPLE3/Plot/ScaleFactor.png', align = "center", width = '95%')),
                column(3, offset = 0, fluidRow( 
                  img(src='EXAMPLE3/Plot/Hclust0.png', align = "center", width = '95%'),
                  img(src='EXAMPLE3/Plot/PCplot0.png', align = "center", width = '95%')
                )),
                column(2, offset = 0, img(src='EXAMPLE3/Plot/Density.png', align = "center", width = '90%')),
                column(3, offset = 0, fluidRow(
                  img(src='EXAMPLE3/Plot/Hclust.png', align = "center", width = '95%'),
                  img(src='EXAMPLE3/Plot/PCplot.png', align = "center", width = '95%')
                ))
              )
          )
        ),
        fluidRow(
          box(collapsible = TRUE,
              title = strong("SAMPLES"), width = 12, solidHeader = TRUE, status = "primary",
              div(style = 'overflow-x: scroll', DT::dataTableOutput('EXAMPLE3_select_sample'))
          )
        ),
        fluidRow(
          box(collapsible = TRUE,
              title = strong("FEATURES"), width = 12, solidHeader = TRUE, status = "primary",
              div(style = 'overflow-x: scroll', DT::dataTableOutput('EXAMPLE3_select_variable'))
          )
        ),
        fluidRow(
          box(collapsible = TRUE,
              title = strong("DATA VIEWER"), width = 12, solidHeader = TRUE, status = "success",
              fluidRow(
                column(6, uiOutput('EXAMPLE3_orderby')),
                column(6, uiOutput('EXAMPLE3_colorby'))
              ),
              plotOutput('EXAMPLE3_select_data_bar'),
              div(style = 'overflow-x: scroll', DT::dataTableOutput('EXAMPLE3_select_data'))
          )
        ),
        fluidRow(
          box(collapsible = TRUE,
              title = strong("GROUP COMPARISON"), width = 12, solidHeader = TRUE, status = NULL,
              fluidRow(
                column(2, actionBttn(inputId = "EXAMPLE3_Select_Info", label = "How to select", style = "stretch", color = "primary")),
                column(2, actionBttn(inputId = "EXAMPLE3_Download_Info", label = "How to download", style = "stretch", color = "primary"))
              ),
              tabsetPanel(
                tabPanel("Group A",
                         downloadButton("EXAMPLE3_download_groupA", "Download GropA", icon = icon("download")), 
                         p(),
                         div(style = 'overflow-x: scroll', DT::dataTableOutput('EXAMPLE3_select_groupA'))),
                tabPanel("Group B",
                         downloadButton("EXAMPLE3_download_groupB", "Download GropB", icon = icon("download")), 
                         p(),
                         div(style = 'overflow-x: scroll', DT::dataTableOutput("EXAMPLE3_select_groupB"))),
                tabPanel("Summary",
                         h2(strong("Summary")),
                         h4(textOutput("EXAMPLE3_groupA_number")),
                         h4(textOutput("EXAMPLE3_groupB_number")),
                         fluidRow(
                          column(3, h4(textOutput("EXAMPLE3_pair_number"))),
                          column(9, h4(uiOutput("EXAMPLE3_pair_check"))),
                          ),
                         uiOutput("EXAMPLE3_choice_method"),
                         actionBttn(inputId = "EXAMPLE3_draw", label = "Start Analysis", style = "fill", color = "primary"),
                         fluidRow(
                           column(6, h4(code("VolcanoPlot")), plotOutput("EXAMPLE3_DEvolcanoplot") %>% withSpinner(color="#0dc5c1")),
                           column(6, h4(code("PlotMD")), plotOutput("EXAMPLE3_DEplotMD") %>% withSpinner(color="#0dc5c1"))
                         )
                ),
                tabPanel("Result",
                         downloadButton("EXAMPLE3_download_result", "Download All Result", icon = icon("download")), 
                         p(),
                         div(style = 'overflow-x: scroll', DT::dataTableOutput("EXAMPLE3_select_limmaoutput"))
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
                            column(6, uiOutput('EXAMPLE3_limma_orderby')),
                            column(6, uiOutput('EXAMPLE3_limma_colorby'))
                         ),
                         verbatimTextOutput("EXAMPLE3_limmadatabar_nocall"),
                         plotOutput('EXAMPLE3_select_limmadata_bar')
                ),
                tabPanel("Boxplot",
                         plotlyOutput('EXAMPLE3_boxplotly')
                ),
                tabPanel("Enrichment Analysis",
                         actionBttn(inputId = "EXAMPLE3_start_enrichment", label = "Start Enrichment Analysis", style = "fill", color = "primary"),
                         p(),
                         fluidRow(
                            column(6,
                                fluidRow(
                                  column(1,
                                    dropdownButton(
                                      tags$h3("Go Selection"),
                                      selectInput(inputId = "EXAMPLE3_ontology_choice", label = "Gene Ontology", choices = list("BP" = "BP", "CC" = "CC", "MF" = "MF")),
                                      selectInput(inputId = "EXAMPLE3_GOplot_choice", label = list(icon("chart-bar"), "Plot Type"), 
                                                  choices = list("Dotplot" = "dotplot", "Barplot" = "barplot")),
                                      sliderTextInput(inputId = "EXAMPLE3_GO_pvalue", label = list(icon("filter"), "DE Filter(Pvalue)"), choices = c(0.05, 0.01, 0.005, 0.001), grid = TRUE),
                                      sliderInput(inputId = "EXAMPLE3_GO_cutoff", label = list(icon("cut"), "GO PvalueCutoff"), min = 0.01, max = 1, value = 0.05),
                                      circle = TRUE, status = "danger", size = "sm",
                                      icon = icon("cogs"), width = "300px",
                                      tooltip = tooltipOptions(title = "Click to check Go selection !")
                                    )
                                  ),
                                  column(5, h3(code("EnrichmentGO")))
                                ),
                                plotOutput('EXAMPLE3_enrichGO_dotplot') %>% withSpinner(color="#FF5511")
                            ),
                            column(6,
                                fluidRow(
                                  column(1,
                                    dropdownButton(
                                      tags$h3("KEGG Selection"),
                                      selectInput(inputId = "EXAMPLE3_KEGG_choice", label = "Type", choices = list("KEGG" = "kegg")),
                                      selectInput(inputId = "EXAMPLE3_KEGGplot_choice", label = list(icon("chart-bar"), "Plot Type"), 
                                                  choices = list("Dotplot" = "dotplot", "Barplot" = "barplot")),
                                      sliderTextInput(inputId = "EXAMPLE3_KEGG_pvalue", label = list(icon("filter"), "DE Filter(Pvalue)"), choices = c(0.05, 0.01, 0.005, 0.001), grid = TRUE),
                                      sliderInput(inputId = "EXAMPLE3_KEGG_cutoff", label = list(icon("cut"), "KEGG PvalueCutoff"), min = 0.01, max = 1, value = 0.05),  
                                      circle = TRUE, status = "danger", size = "sm",
                                      icon = icon("cogs"), width = "300px",
                                      tooltip = tooltipOptions(title = "Click to check KEGG selection !")
                                    )
                                  ),
                                  column(5, h3(code("EnrichmentKEGG")))
                                ),
                                plotOutput('EXAMPLE3_enrichKEGG_dotplot') %>% withSpinner(color="#FF5511")
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
      ### EXAMPLE4
      ######################
      tabItem(
        tabName = "EXAMPLE4",
        h2(strong("EXAMPLE4")),
        fluidRow(
          box(collapsible = TRUE,
              title = strong("OVERVIEW"), width = 12, solidHeader = TRUE, status = "warning",
              p(strong('Title:')), 
              p(textOutput('EXAMPLE4.title')),
              p(strong('Study aims:')), 
              p(textOutput('EXAMPLE4_aim')),
              p(strong('Strategy of analysis:')), 
              p(textOutput('EXAMPLE4_strategy')),
              p(strong('Tentative findings:')), 
              p(textOutput('EXAMPLE4_finding')),
              p(strong('Participants:')), 
              p(textOutput('EXAMPLE4_participant')),
              p(strong('Progress:')),
              div(style = 'overflow-x: scroll', DT::dataTableOutput('EXAMPLE4_progress'))
          )
        ),
        fluidRow(
          box(collapsible = TRUE,
              title = strong("PLATFROMS"), width = 12, solidHeader = TRUE, status = "primary",
              uiOutput('EXAMPLE4_select_platform'),
              fluidRow(
                column(3, offset = 0, img(src='EXAMPLE4/Plot/ScaleFactor.png', align = "center", width = '95%')),
                column(3, offset = 0, fluidRow( 
                  img(src='EXAMPLE4/Plot/Hclust0.png', align = "center", width = '95%'),
                  img(src='EXAMPLE4/Plot/PCplot0.png', align = "center", width = '95%')
                )),
                column(2, offset = 0, img(src='EXAMPLE4/Plot/Density.png', align = "center", width = '90%')),
                column(3, offset = 0, fluidRow(
                  img(src='EXAMPLE4/Plot/Hclust.png', align = "center", width = '95%'),
                  img(src='EXAMPLE4/Plot/PCplot.png', align = "center", width = '95%')
                ))
              )
          )
        ),
        fluidRow(
          box(collapsible = TRUE,
              title = strong("SAMPLES"), width = 12, solidHeader = TRUE, status = "primary",
              div(style = 'overflow-x: scroll', DT::dataTableOutput('EXAMPLE4_select_sample'))
          )
        ),
        fluidRow(
          box(collapsible = TRUE,
              title = strong("FEATURES"), width = 12, solidHeader = TRUE, status = "primary",
              div(style = 'overflow-x: scroll', DT::dataTableOutput('EXAMPLE4_select_variable'))
          )
        ),
        fluidRow(
          box(collapsible = TRUE,
              title = strong("DATA VIEWER"), width = 12, solidHeader = TRUE, status = "success",
              fluidRow(
                column(6, uiOutput('EXAMPLE4_orderby')),
                column(6, uiOutput('EXAMPLE4_colorby'))
              ),
              plotOutput('EXAMPLE4_select_data_bar'),
              div(style = 'overflow-x: scroll', DT::dataTableOutput('EXAMPLE4_select_data'))
          )
        ),
        fluidRow(
          box(collapsible = TRUE,
              title = strong("GROUP COMPARISON"), width = 12, solidHeader = TRUE, status = NULL,
              fluidRow(
                column(2, actionBttn(inputId = "EXAMPLE4_Select_Info", label = "How to select", style = "stretch", color = "primary")),
                column(2, actionBttn(inputId = "EXAMPLE4_Download_Info", label = "How to download", style = "stretch", color = "primary"))
              ),
              tabsetPanel(
                tabPanel("Group A",
                         downloadButton("EXAMPLE4_download_groupA", "Download GropA", icon = icon("download")), 
                         p(),
                         div(style = 'overflow-x: scroll', DT::dataTableOutput('EXAMPLE4_select_groupA'))),
                tabPanel("Group B",
                         downloadButton("EXAMPLE4_download_groupB", "Download GropB", icon = icon("download")), 
                         p(),
                         div(style = 'overflow-x: scroll', DT::dataTableOutput("EXAMPLE4_select_groupB"))),
                tabPanel("Summary",
                         h2(strong("Summary")),
                         h4(textOutput("EXAMPLE4_groupA_number")),
                         h4(textOutput("EXAMPLE4_groupB_number")),
                         fluidRow(
                          column(3, h4(textOutput("EXAMPLE4_pair_number"))),
                          column(9, h4(uiOutput("EXAMPLE4_pair_check"))),
                          ),
                         uiOutput("EXAMPLE4_choice_method"),
                         actionBttn(inputId = "EXAMPLE4_draw", label = "Start Analysis", style = "fill", color = "primary"),
                         fluidRow(
                           column(6, h4(code("VolcanoPlot")), plotOutput("EXAMPLE4_DEvolcanoplot") %>% withSpinner(color="#0dc5c1")),
                           column(6, h4(code("PlotMD")), plotOutput("EXAMPLE4_DEplotMD") %>% withSpinner(color="#0dc5c1"))
                         )
                ),
                tabPanel("Result",
                         downloadButton("EXAMPLE4_download_result", "Download All Result", icon = icon("download")), 
                         p(),
                         div(style = 'overflow-x: scroll', DT::dataTableOutput("EXAMPLE4_select_limmaoutput"))
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
                            column(6, uiOutput('EXAMPLE4_limma_orderby')),
                            column(6, uiOutput('EXAMPLE4_limma_colorby'))
                         ),
                         verbatimTextOutput("EXAMPLE4_limmadatabar_nocall"),
                         plotOutput('EXAMPLE4_select_limmadata_bar')
                ),
                tabPanel("Boxplot",
                         plotlyOutput('EXAMPLE4_boxplotly')
                ),
                tabPanel("Enrichment Analysis",
                         actionBttn(inputId = "EXAMPLE4_start_enrichment", label = "Start Enrichment Analysis", style = "fill", color = "primary"),
                         p(),
                         fluidRow(
                            column(6,
                                fluidRow(
                                  column(1,
                                    dropdownButton(
                                      tags$h3("Go Selection"),
                                      selectInput(inputId = "EXAMPLE4_ontology_choice", label = "Gene Ontology", choices = list("BP" = "BP", "CC" = "CC", "MF" = "MF")),
                                      selectInput(inputId = "EXAMPLE4_GOplot_choice", label = list(icon("chart-bar"), "Plot Type"), 
                                                  choices = list("Dotplot" = "dotplot", "Barplot" = "barplot")),
                                      sliderTextInput(inputId = "EXAMPLE4_GO_pvalue", label = list(icon("filter"), "DE Filter(Pvalue)"), choices = c(0.05, 0.01, 0.005, 0.001), grid = TRUE),
                                      sliderInput(inputId = "EXAMPLE4_GO_cutoff", label = list(icon("cut"), "GO PvalueCutoff"), min = 0.01, max = 1, value = 0.05),
                                      circle = TRUE, status = "danger", size = "sm",
                                      icon = icon("cogs"), width = "300px",
                                      tooltip = tooltipOptions(title = "Click to check Go selection !")
                                    )
                                  ),
                                  column(5, h3(code("EnrichmentGO")))
                                ),
                                plotOutput('EXAMPLE4_enrichGO_dotplot') %>% withSpinner(color="#FF5511")
                            ),
                            column(6,
                                fluidRow(
                                  column(1,
                                    dropdownButton(
                                      tags$h3("KEGG Selection"),
                                      selectInput(inputId = "EXAMPLE4_KEGG_choice", label = "Type", choices = list("KEGG" = "kegg")),
                                      selectInput(inputId = "EXAMPLE4_KEGGplot_choice", label = list(icon("chart-bar"), "Plot Type"), 
                                                  choices = list("Dotplot" = "dotplot", "Barplot" = "barplot")),
                                      sliderTextInput(inputId = "EXAMPLE4_KEGG_pvalue", label = list(icon("filter"), "DE Filter(Pvalue)"), choices = c(0.05, 0.01, 0.005, 0.001), grid = TRUE),
                                      sliderInput(inputId = "EXAMPLE4_KEGG_cutoff", label = list(icon("cut"), "KEGG PvalueCutoff"), min = 0.01, max = 1, value = 0.05),  
                                      circle = TRUE, status = "danger", size = "sm",
                                      icon = icon("cogs"), width = "300px",
                                      tooltip = tooltipOptions(title = "Click to check KEGG selection !")
                                    )
                                  ),
                                  column(5, h3(code("EnrichmentKEGG")))
                                ),
                                plotOutput('EXAMPLE4_enrichKEGG_dotplot') %>% withSpinner(color="#FF5511")
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
