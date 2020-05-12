##############################################OverView######################################################
output$EXAMPLE3_title <- renderText({
  as.character(readxl::read_xlsx('www/ProjectProgress.xlsx', sheet = 'EXAMPLE3', col_names = FALSE)[[2]][1])
})

output$EXAMPLE3_aim <- renderText({
  as.character(readxl::read_xlsx('www/ProjectProgress.xlsx', sheet = 'EXAMPLE3', col_names = FALSE)[[2]][2])
})

output$EXAMPLE3_strategy <- renderText({
  as.character(readxl::read_xlsx('www/ProjectProgress.xlsx', sheet = 'EXAMPLE3', col_names = FALSE)[[2]][3])
})

output$EXAMPLE3_finding <- renderText({
  as.character(readxl::read_xlsx('www/ProjectProgress.xlsx', sheet = 'EXAMPLE3', col_names = FALSE)[[2]][4])
})

output$EXAMPLE3_participant <- renderText({
  as.character(readxl::read_xlsx('www/ProjectProgress.xlsx', sheet = 'EXAMPLE3', col_names = FALSE)[[2]][5])
})

output$EXAMPLE3_progress <- DT::renderDataTable({
  x <- readxl::read_xlsx('www/ProjectProgress.xlsx', sheet = 'EXAMPLE3')
  summ <- as.character(x[[2]][1:4])
  names(summ) <- as.character(x[[1]][1:4])
  prog <- data.frame(x[-(1:5),])
  colnames(prog) <- as.character(x[5,])
  prog$Document <- paste0('<a href="', prog$Document, '" target="_blank">', 'link', '</a>')
  DT::datatable(prog, rownames = FALSE, escape = FALSE)
})
##########################################Select EXP In MySQL################################################
output$EXAMPLE3_select_platform <- renderUI({
  platform.name <- c('Affy Human Genome U133 Plus 2.0 Array', 'Affy Mouse Genome 430 2.0 Array', 
                     'Infinium Human Methylation 450K BeadChip', 'Infinium MethylationEPIC BeadChip')
  names(platform.name) <- c('AFFY', 'M430', 'METH', 'MEPIC')
  platforms <- sub('_.*', '', mydb[grep('EXAMPLE3_EXP', mydb)])
  names(platforms) <- platform.name[platforms]
  selectInput(inputId = 'EXAMPLE3_select_platform', label = 'Select a platform', 
              choices = as.list(platforms))
})

EXAMPLE3_selected_platform <- reactive({
  input$EXAMPLE3_select_platform
})

EXAMPLE3_list_sample <- reactive({
  if (length(EXAMPLE3_selected_platform()) > 0){
    query <- paste0('SELECT * FROM ', paste0(EXAMPLE3_selected_platform(), '_', 'EXAMPLE3', '_EXP'))
    dbGetQuery(connect, query)
  }
})

EXAMPLE3_list_variable <- reactive({
  if (length(EXAMPLE3_selected_platform()) > 0){
    if (EXAMPLE3_selected_platform() == 'AFFY') {info <- affy.info[, c(1:8, 9:10)]}
    if (EXAMPLE3_selected_platform() == 'METH') {info <- meth.info[, c(1:8, 9:11)]}
    if (EXAMPLE3_selected_platform() == 'M430') {info <- m430.info[, c(1:8, 9:10)]}
    if (EXAMPLE3_selected_platform() == 'MEPIC') {info <- mepic.info[, c(1:8, 8:10)]}
    info
  }
})
##########################################Select Gene Data In MYSQL##########################################
EXAMPLE3_selected_data <- reactive({
  if (length(EXAMPLE3_selected_platform()) > 0){
    if (length(EXAMPLE3_selected_sample()) > 0){
      if (length(EXAMPLE3_selected_variable()) > 0){
        ids <- EXAMPLE3_selected_variable()[, 'ID']
        query <- paste0('SELECT * FROM ', EXAMPLE3_selected_platform(), "_EXAMPLE3_DATA WHERE `ID` IN ('", ids, "');")
        tmp <- dbGetQuery(connect, query)
        as.numeric(tmp[1, -1])
      }
    }
  }
})

EXAMPLE3_selected_limmadata <- reactive({
  if (length(EXAMPLE3_selected_platform()) > 0){
    if (length(EXAMPLE3_selected_sample()) > 0){
      if (length(EXAMPLE3_selected_variable()) > 0){
        ids <- rownames(EXAMPLE3_selected_limmaoutput())
        query <- paste0('SELECT * FROM ', EXAMPLE3_selected_platform(), "_EXAMPLE3_DATA WHERE `ID` IN ('", ids, "');")
        tmp <- dbGetQuery(connect, query)
        tmp[1, -1]
      }
    }
  }
})

####All Gene Data####
EXAMPLE3_list_data <- reactive({
  if (length(EXAMPLE3_selected_platform()) > 0){
    if (length(EXAMPLE3_selected_sample()) > 0){
      if (length(EXAMPLE3_selected_variable()) > 0){
        aquery <- paste0("SELECT * FROM ", EXAMPLE3_selected_platform(), "_EXAMPLE3_DATA;")
        atmp<-dbGetQuery(connect, aquery)
        atmp
      }
    }
  }
})
#################################################Sample######################################################
output$EXAMPLE3_select_sample <- DT::renderDataTable({
  if (length(EXAMPLE3_list_sample()) > 0){
    DT::datatable(EXAMPLE3_list_sample(), selection = list(mode = 'multiple', selected = c(1:nrow(EXAMPLE3_list_sample())), target = 'row') , 
                  filter = 'top', rownames = FALSE, escape = FALSE)
  }
})

EXAMPLE3_selected_sample <- reactive({
  if (length(EXAMPLE3_list_sample()) > 0){
    EXAMPLE3_list_sample()[input$EXAMPLE3_select_sample_rows_selected, , drop = FALSE]
  }
})

################################################Feature#####################################################
output$EXAMPLE3_select_variable <- DT::renderDataTable({
  if (length(EXAMPLE3_list_variable()) > 0){
    DT::datatable(EXAMPLE3_list_variable(), selection = list(mode = 'single', selected = 1, target = 'row') , 
                  filter = 'top', rownames = FALSE, escape = FALSE)
  }
})

EXAMPLE3_selected_variable <- reactive({
  if (length(EXAMPLE3_list_variable()) > 0){
    EXAMPLE3_list_variable()[input$EXAMPLE3_select_variable_rows_selected, , drop = FALSE]
  }
})
################################################Data VIEW####################################################
output$EXAMPLE3_orderby <- renderUI({
  if (length(EXAMPLE3_list_sample()) > 0){
    selectInput(inputId = 'EXAMPLE3_orderby', label = 'Ordered by', 
                choices = as.list(colnames(EXAMPLE3_list_sample())))
  }
})

output$EXAMPLE3_colorby <- renderUI({
  if (length(EXAMPLE3_list_sample()) > 0){
    selectInput(inputId = 'EXAMPLE3_colorby', label = 'Colored by', 
                choices = as.list(colnames(EXAMPLE3_list_sample())))
  }
})

output$EXAMPLE3_limma_orderby <- renderUI({
  if (length(EXAMPLE3_list_sample()) > 0){
    pickerInput(inputId = 'EXAMPLE3_limma_orderby', label = 'Limma ordered by', 
                choices = as.list(c(colnames(EXAMPLE3_list_sample()),"Group")))
  }
})

output$EXAMPLE3_limma_colorby <- renderUI({
  if (length(EXAMPLE3_list_sample()) > 0){
    pickerInput(inputId = 'EXAMPLE3_limma_colorby', label = 'Limma colored by', 
                choices = as.list(c(colnames(EXAMPLE3_list_sample()),"Group")))
  }
})

############################################Group Comparsion#################################################
####Manipulate Info####
observeEvent(input$EXAMPLE3_Select_Info, {
  sendSweetAlert(
    session = session,
    title = "How to select",
    text = tags$span(
            "1.Push Ctrl to select/deselect single row",
            tags$br(),
            "2.Push Shift to select/deselect multiple row"
            ),
    type = "info"
  )
})

observeEvent(input$EXAMPLE3_Download_Info, {
  sendSweetAlert(
    session = session,
    title = "How to download",
    text = tags$span(
      "TYPE1. Push 'Download GroupA/GroupB' button to download rawdata on selected sample",
      tags$br(),
      "TYPE2. Push 'csv/excel' button to download the table data",
      tags$br(),
      "TYPE3. Push 'Download All Result' button to download all results"
    ),
    type = "info"
  )
})
####Group A####
output$EXAMPLE3_select_groupA <- DT::renderDataTable(server = FALSE, {
  if (length(EXAMPLE3_list_sample()) > 0){
    DT::datatable(EXAMPLE3_selected_sample(), extensions = c("Buttons", "Select"), 
                  options = list(dom = "lBftrip", buttons = list("selectAll","selectNone","csv","excel"), select = TRUE),
                  selection = "none" , 
                  filter = 'top', rownames = FALSE, escape = FALSE)
  }
})

EXAMPLE3_selected_groupA <- reactive({
  if (length(EXAMPLE3_list_sample()) > 0){
    if (length(EXAMPLE3_selected_sample()) > 0){
      EXAMPLE3_selected_sample()[input$EXAMPLE3_select_groupA_rows_selected, , drop = FALSE]
    }
  }
})


####GroupA unselected#### 
EXAMPLE3_unselected_groupA <- reactive({
  if (length(EXAMPLE3_list_sample()) > 0){
    if (length(EXAMPLE3_selected_sample()) > 0){
        EXAMPLE3_selected_sample()[-input$EXAMPLE3_select_groupA_rows_selected, , drop = FALSE]
    }
  }
})

####Group B####
output$EXAMPLE3_select_groupB <- DT::renderDataTable(server = FALSE, {
  if (length(EXAMPLE3_list_sample()) > 0){
    if (length(EXAMPLE3_selected_sample()) > 0){
      if(length(input$EXAMPLE3_select_groupA_rows_selected) > 0){
        DT::datatable(EXAMPLE3_unselected_groupA(), extensions = c("Buttons", "Select"),
                      options = list(dom = "lBftrip", buttons = list("selectAll","selectNone","csv","excel"), select = TRUE),
                      selection = "none" , 
                       filter = 'top', rownames = FALSE, escape = FALSE)
      }
    }
  }
})

EXAMPLE3_selected_groupB <- reactive({
  if (length(EXAMPLE3_list_sample()) > 0){
    if (length(EXAMPLE3_selected_sample()) > 0){
      if (length(rownames(EXAMPLE3_unselected_groupA())) > 0){
          EXAMPLE3_unselected_groupA()[input$EXAMPLE3_select_groupB_rows_selected, , drop = FALSE]
      }
    }
  }
})
##### GroupA GroupB Download #####
EXAMPLE3_download_groupA_data <- reactive({
  download_groupA <- EXAMPLE3_selected_groupA_genedata()
  rownames(download_groupA) <- rownames(EXAMPLE3_list_variable())
  download_groupA <- cbind(EXAMPLE3_list_variable()[,c("GeneSym")], download_groupA)
  return(download_groupA)
})

output$EXAMPLE3_download_groupA <- downloadHandler(
  filename = function(){"GropA_genedata.csv"},
  content = function(file){
    write.csv(EXAMPLE3_download_groupA_data(), file)
  }
)

EXAMPLE3_download_groupB_data <- reactive({
  download_groupB <- EXAMPLE3_selected_groupB_genedata()
  rownames(download_groupB) <- rownames(EXAMPLE3_list_variable())
  download_groupB <- cbind(EXAMPLE3_list_variable()[,c("GeneSym")], download_groupB)
  return(download_groupB)
})

output$EXAMPLE3_download_groupB <- downloadHandler(
  filename = function(){"GropB_genedata.csv"},
  content = function(file){
    write.csv(EXAMPLE3_download_groupB_data(), file)
  }
)
####A B Summary ####
output$EXAMPLE3_groupA_number <- renderText({
  paste0("Sample in GroupA : ",length(rownames(EXAMPLE3_selected_groupA())))
})

output$EXAMPLE3_groupB_number <- renderText({
  if(length(rownames(EXAMPLE3_selected_groupA()))==0){
    paste0("Sample in groupB : ",0)
  }else{
    paste0("Sample in groupB : ",length(rownames(EXAMPLE3_selected_groupB())))
  }
})
####Find AB Pair####
EXAMPLE3_find_pair <- reactive({
  if (length(EXAMPLE3_selected_groupA())>0 && length(EXAMPLE3_selected_groupB())>0){
    GroupA <- EXAMPLE3_selected_groupA()
    GroupA_index <- duplicated(GroupA$Patient.ID)
    GroupA_name <- GroupA[!GroupA_index,c("Patient.ID","Sample.Name")]
    GroupB <- EXAMPLE3_selected_groupB()
    GroupB_index <- duplicated(GroupB$Patient.ID)
    GroupB_name <- GroupB[!GroupB_index,c("Patient.ID","Sample.Name")]
    GroupAB_name <- inner_join(GroupA_name, GroupB_name, by = "Patient.ID", suffix = c(".groupA", ".groupB"))
    GroupAB_name
  }
})

EXAMPLE3_count_pair <- reactive({
  if (length(rownames(EXAMPLE3_selected_groupA()))>0 && length(rownames(EXAMPLE3_selected_groupB()))>0){
    paircount<-length(rownames(EXAMPLE3_find_pair()))
  }else{
    paircount<-0
  }
  paircount
})

####Pair Summary####
output$EXAMPLE3_pair_number <- renderText({
  paste0("Patient in both group : ",EXAMPLE3_count_pair())
})
####Pair Warning####
output$EXAMPLE3_pair_check <- renderUI({
  if(EXAMPLE3_count_pair()>3){
    prettyToggle(inputId = "EXAMPLE3_paircheck_button",
                 label_on = "Patient in both group more than 3", 
                 icon_on = icon("check"),
                 status_on = "info",
                 status_off = "info", 
                 label_off = "Patient in both group more than 3",
                 icon_off = icon("check"))
  }else{
    prettyToggle(inputId = "EXAMPLE3_paircheck_button",
                 label_on = "Patient in both group less than 3", 
                 icon_on = icon("remove"),
                 status_on = "warning",
                 status_off = "warning", 
                 label_off = "Patient in both group less than 3",
                 icon_off = icon("remove"))
  }
})
####Pair decide method####
output$EXAMPLE3_choice_method <- renderUI({
  if (length(EXAMPLE3_list_sample()) > 0){
    pickerInput(inputId = "EXAMPLE3_method", label = "ChoiceMethod:", choices = EXAMPLE3_decide_method(),
                choicesOpt = list(icon = c("glyphicon-arrow-right","glyphicon-arrow-right")))
  }
})

EXAMPLE3_decide_method <- reactive({
  if (EXAMPLE3_count_pair() > 3){
    method <- c("Unpair DE analysis(Limma)", "Pair DE analysis(Limma)")
  }else{
    method <- c("Unpair DE analysis(Limma)")
  } 
    method
})

##########Gene select data##########
EXAMPLE3_selected_genedata <- reactive({
  if (length(EXAMPLE3_list_sample()) > 0 && length(EXAMPLE3_list_data()) > 0){
    EXAMPLE3_list_data <- EXAMPLE3_list_data()[,-1]
    EXAMPLE3_list_data[,input$EXAMPLE3_select_sample_rows_selected, drop = FALSE]
  }
})

EXAMPLE3_selected_groupA_genedata <- reactive({
  if (length(EXAMPLE3_list_sample()) > 0 && length(EXAMPLE3_list_data()) > 0){
    if (length(EXAMPLE3_selected_groupA()) > 0 && length(EXAMPLE3_selected_genedata()) >0){
      EXAMPLE3_selected_genedata()[,input$EXAMPLE3_select_groupA_rows_selected, drop = FALSE]
    }
  }
})
EXAMPLE3_unselected_groupA_genedata <- reactive({
  if (length(EXAMPLE3_list_sample()) > 0 && length(EXAMPLE3_list_data()) > 0){
    if (length(EXAMPLE3_selected_groupA()) > 0 && length(EXAMPLE3_selected_genedata()) >0){
      EXAMPLE3_selected_genedata()[,-input$EXAMPLE3_select_groupA_rows_selected, drop = FALSE]
    }
  }
})
EXAMPLE3_selected_groupB_genedata <- reactive({
  if (length(EXAMPLE3_list_sample()) > 0 && length(EXAMPLE3_list_data()) > 0){
    if (length(EXAMPLE3_selected_groupA()) > 0 && length(EXAMPLE3_selected_genedata()) >0){
      EXAMPLE3_unselected_groupA_genedata()[,input$EXAMPLE3_select_groupB_rows_selected, drop = FALSE]
    }
  }
})


####Limma#####
EXAMPLE3_limma_fitC <- eventReactive(input$EXAMPLE3_draw, {
  if (length(EXAMPLE3_selected_groupA_genedata())>0 && length(EXAMPLE3_selected_groupB_genedata())>0){
    groupA_gene <- EXAMPLE3_selected_groupA_genedata()
    groupB_gene <- EXAMPLE3_selected_groupB_genedata()
    if(input$EXAMPLE3_method=="Unpair DE analysis(Limma)"){
      groupAB_gene <- data.frame(cbind(groupA_gene,groupB_gene))
      rownames(groupAB_gene)<-EXAMPLE3_list_data()[,1]
      group_list <- factor(c(rep(1,length(colnames(groupA_gene))),rep(0,length(colnames(groupB_gene))))) 
      design <- model.matrix(~group_list-1)
      colnames(design)<-c("GA","GB")
      contrast_matrix <- makeContrasts("GA-GB", levels = design)
      fit <- lmFit(groupAB_gene, design)
      fitC <- contrasts.fit(fit, contrast_matrix)
      fitC <- eBayes(fitC)
      fitC
    }
    else if(input$EXAMPLE3_method=="Pair DE analysis(Limma)"){
      pair_data <- EXAMPLE3_find_pair()
      groupA_pair_name <- pair_data$Sample.Name.groupA 
      groupB_pair_name <- pair_data$Sample.Name.groupB
      groupA_final_name <- groupA_pair_name %>% gsub(" ",replacement=".", .) %>% gsub("-",replacement=".", .)
      groupB_final_name <- groupB_pair_name %>% gsub(" ",replacement=".", .) %>% gsub("-",replacement=".", .)
      groupAB_name <- c(groupA_final_name, groupB_final_name)
      pair_number<- as.integer(EXAMPLE3_count_pair())
      groupAB_gene <- data.frame(cbind(groupA_gene, groupB_gene), check.names = FALSE)
      groupAB_gene <- groupAB_gene[, groupAB_name]
      rownames(groupAB_gene)<-EXAMPLE3_list_data()[,1]
      sibship <- factor(rep(1:pair_number, 2))
      treat <- factor(c(rep("GA", pair_number),rep("GB", pair_number)), levels = c("GA","GB"))
      design_pair <- model.matrix(~sibship+treat)
      fit_pair <- lmFit(groupAB_gene, design_pair)
      fit_pair <- eBayes(fit_pair)
      fit_pair <- fit_pair[, pair_number+1]
      fit_pair
    }
  }
})

EXAMPLE3_limma_output <- reactive({
  limma.output <- signif(topTable(EXAMPLE3_fitC_dataset(), n=length(rownames(EXAMPLE3_list_data()))), digits = 3)
  GroupA_means <- rowMeans(as.matrix(EXAMPLE3_selected_groupA_genedata()))
  GroupB_means <- rowMeans(as.matrix(EXAMPLE3_selected_groupB_genedata()))
  GroupAB_means <- signif(data.frame(GroupA_means, GroupB_means), digits = 3)
  genenames <- EXAMPLE3_list_variable()
  rownames(GroupAB_means) <- rownames(genenames)
  limma.output <- cbind(genenames[rownames(limma.output), c("GeneSym")], GroupAB_means[rownames(limma.output),], limma.output[,-c(3,6)])
  colnames(limma.output)[1] <- "GeneSym"
  limma.output
})

output$EXAMPLE3_select_limmaoutput <- DT::renderDataTable({
  if(length(input$EXAMPLE3_select_groupA_rows_selected) > 0 && length(input$EXAMPLE3_select_groupB_rows_selected) > 0)
    DT::datatable(EXAMPLE3_limma_output(), extensions = "Buttons", options = list(dom = "lBftrip", buttons = list("colvis","csv","excel","pdf","print")),
                  selection = list(mode = 'single', selected = 1, target = 'row'), 
                  filter = 'top', rownames = TRUE, escape = FALSE)
})

output$EXAMPLE3_download_result <- downloadHandler(
  filename = function(){"EXAMPLE3_DE_Result.csv"},
  content = function(file){
    write.csv(EXAMPLE3_limma_output(), file)
  }
)

EXAMPLE3_selected_limmaoutput <- reactive({
  if (length(EXAMPLE3_list_variable()) > 0){
    EXAMPLE3_limma_output()[input$EXAMPLE3_select_limmaoutput_rows_selected, , drop = FALSE]
  }
})
###################################################Plot#####################################################
#####Feature DATA VIEW####
output$EXAMPLE3_select_data_bar <- renderPlot({
  if (length(EXAMPLE3_selected_platform()) > 0){
    if (length(EXAMPLE3_selected_sample()) > 0){
      if (length(EXAMPLE3_selected_variable()) > 0){
        if (length(EXAMPLE3_selected_data()) > 0){
          par(mar = c(13, 4, 4, 2))
          x <- EXAMPLE3_selected_data()[input$EXAMPLE3_select_sample_rows_selected]
          y <- EXAMPLE3_list_sample()[input$EXAMPLE3_select_sample_rows_selected, ]
          z <- EXAMPLE3_list_variable()[input$EXAMPLE3_select_variable_rows_selected, ]
          u <- y[, input$EXAMPLE3_orderby]
          v <- y[, input$EXAMPLE3_colorby]
          x <- x[order(u)]
          y <- y[order(u), ]
          v <- factor(v[order(u)])
          plot(x, type = 'h', lwd = 10, xlab = '', ylab = 'Value', axes = FALSE,
               col = rainbow(length(levels(v)))[factor(v)],
               main = paste0(z[1, -1],collapse = ', '))
          abline(h = seq(0, 20, by = 0.2), lty = 3, col = 'gray')
          axis(1, at = 1:length(EXAMPLE3_selected_data()[input$EXAMPLE3_select_sample_rows_selected]), 
               y[, 1], las = 2)
          axis(2, at = seq(0, 20, by = 0.2), seq(0, 20, by = 0.2), las = 2)
        }
      }
    }
  }
})

############### Waiter ####################
EXAMPLE3_Vol_w <- call_waitress("#EXAMPLE3_DEvolcanoplot", theme = "overlay-percent")
EXAMPLE3_MD_w <- call_waitress("#EXAMPLE3_DEplotMD", theme = "overlay-percent")
EXAMPLE3_Barview_w <- call_waitress("#EXAMPLE3_select_limmadata_bar", theme = "line")
EXAMPLE3_Boxview_w <- call_waitress("#EXAMPLE3_boxplotly", theme = "line")
#注意Waiter tag命名不可有"."否則影響Js的ID #tag
EXAMPLE3_fitC_dataset <- eventReactive(input$EXAMPLE3_draw, {
  EXAMPLE3_Vol_w$start()
  EXAMPLE3_Vol_w$auto(5, 150)
  EXAMPLE3_MD_w$start()
  EXAMPLE3_MD_w$auto(5, 150)
  EXAMPLE3_Barview_w$start(h3(strong("Analysis Stuff....")))
  EXAMPLE3_Barview_w$auto(5,200)
  EXAMPLE3_Boxview_w$start(h3(strong("Creating Boxplot.....")))
  EXAMPLE3_Boxview_w$auto(5,200)
  Sys.sleep(1)
  EXAMPLE3_Vol_w$close()
  EXAMPLE3_MD_w$close()
  EXAMPLE3_Barview_w$close()
  EXAMPLE3_Boxview_w$close()
  return(EXAMPLE3_limma_fitC())
})

EXAMPLE3_enrichGo_w <- call_waitress("#EXAMPLE3_enrichGO_dotplot", theme = "overlay-percent")
EXAMPLE3_enrichKEGG_w <- call_waitress("#EXAMPLE3_enrichKEGG_dotplot", theme = "overlay-percent")
EXAMPLE3_result_dataset <- eventReactive(input$EXAMPLE3_start_enrichment, {
  EXAMPLE3_enrichGo_w$start()
  EXAMPLE3_enrichKEGG_w$start()
  EXAMPLE3_enrichGo_w$auto(5,200)
  EXAMPLE3_enrichKEGG_w$auto(5,200)
  Sys.sleep(1)
  EXAMPLE3_enrichGo_w$close()
  EXAMPLE3_enrichKEGG_w$close()
  return(EXAMPLE3_limma_output())
})

##########COMPARISON DIAGRAM##############
output$EXAMPLE3_DEvolcanoplot <- renderPlot({
  volcanoplot(EXAMPLE3_fitC_dataset(), highlight = TRUE)
  abline(v=c(-1,-log2(1.5),log2(1.5),1), lty = c(1, 2, 2, 1))
  abline(h=c(-log10(0.05), -log10(0.01)), lty = c(1, 2))
})

output$EXAMPLE3_DEplotMD <- renderPlot({
  plotMD(EXAMPLE3_fitC_dataset(), status = decideTests(EXAMPLE3_fitC_dataset(), p.value=0.05), 
         main = colnames(EXAMPLE3_fitC_dataset()))
})

#########Comparsion Viewer ############
output$EXAMPLE3_limmadatabar_nocall <- renderText({
  if (length(input$EXAMPLE3_select_limmaoutput_rows_selected) < 1){
    o <- 'To begin this analysis, please allow the step from
         "1.Choose groupA & B"
         "2.Check sumarry and start analysis"
         "3.Choose target gene from analysis result"'
  }else{
    o <- ""
  }
  o
})
###groupAB plot data###
EXAMPLE3_groupAB_data <- reactive({
  groupA_row_select <- EXAMPLE3_selected_groupA()
  groupB_row_select <- EXAMPLE3_selected_groupB()
  groupA_row_select[,"Group"] <- "GroupA"
  groupB_row_select[,"Group"] <- "GroupB"
  groupAB_row_select <- data.frame(rbind(groupA_row_select, groupB_row_select))
  return(groupAB_row_select)
})

###bar###
output$EXAMPLE3_select_limmadata_bar <- renderPlot({
  if (length(EXAMPLE3_selected_platform()) > 0){
    if (length(EXAMPLE3_selected_sample()) > 0){
      if (length(EXAMPLE3_selected_variable()) > 0){
        if (length(input$EXAMPLE3_select_groupA_rows_selected) > 0 && length(input$EXAMPLE3_select_groupB_rows_selected) > 0){
          if (length(input$EXAMPLE3_select_limmaoutput_rows_selected) > 0){
            par(mar = c(13, 4, 4, 2))
            groupAB.name.selected <- c(colnames(EXAMPLE3_selected_groupA_genedata()),colnames(EXAMPLE3_selected_groupB_genedata()))
            x <- as.numeric(EXAMPLE3_selected_limmadata()[,groupAB.name.selected])
            y <- EXAMPLE3_groupAB_data()
            z <- EXAMPLE3_limma_output()[input$EXAMPLE3_select_limmaoutput_rows_selected, ]
            u <- y[, input$EXAMPLE3_limma_orderby]
            v <- y[, input$EXAMPLE3_limma_colorby]
            x <- x[order(u)]
            y <- y[order(u), ]
            v <- factor(v[order(u)])
            plot(x, type = 'h', lwd = 10, xlab = '', ylab = 'Value', axes = FALSE,
                col = rainbow(length(levels(v)))[factor(v)],
                main = paste0(z[1,],collapse = ', '))
            abline(h = seq(0, 20, by = 0.2), lty = 3, col = 'gray')
            axis(1, at = 1:length(x), y[, 1], las = 2)
            axis(2, at = seq(0, 20, by = 0.2), seq(0, 20, by = 0.2), las = 2)
          }
        }
      }
    }
  }
})

###box##
output$EXAMPLE3_boxplotly <- renderPlotly({
  if (length(input$EXAMPLE3_select_groupA_rows_selected) > 0 && length(input$EXAMPLE3_select_groupB_rows_selected) > 0){
    if (length(input$EXAMPLE3_select_limmaoutput_rows_selected) > 0){
      groupAB.name.selected <- c(colnames(EXAMPLE3_selected_groupA_genedata()),colnames(EXAMPLE3_selected_groupB_genedata()))
      groupAB.row.select <- EXAMPLE3_groupAB_data()
      gene.express <- as.numeric(EXAMPLE3_selected_limmadata()[,groupAB.name.selected])
      groupAB.row.select[,"GeneExpress"] <- gene.express
      plot_ly(groupAB.row.select, y=~GeneExpress, color = ~Group, type = "box", boxpoints = "all", pointpos = -2, jitter = 3)
    }
  }
})

############### Enrichment Analysis ################
###enrichGo plot###
EXAMPLE3_GO_data <- reactive({
  data <- EXAMPLE3_result_dataset()
  filterdata <- subset(data, P.Value < input$EXAMPLE3_GO_pvalue)
  bitrdata <- bitr(filterdata$GeneSym, fromType = "SYMBOL", toType = c("ENSEMBL", "ENTREZID"), OrgDb = "org.Hs.eg.db")
  input_ontology <- input$EXAMPLE3_ontology_choice
  ego <- enrichGO(gene          = bitrdata$ENTREZID,
                  universe      = names(data(geneList, package = "DOSE")),
                  OrgDb         = org.Hs.eg.db,
                  ont           = input_ontology,
                  pAdjustMethod = "BH",
                  pvalueCutoff  = input$EXAMPLE3_GO_cutoff,
                  qvalueCutoff  = input$EXAMPLE3_GO_cutoff,
                  readable      = FALSE)
  return(ego)
})

output$EXAMPLE3_enrichGO_dotplot <- renderPlot({
  ego <- EXAMPLE3_GO_data()
if (input$EXAMPLE3_GOplot_choice=="dotplot"){
    dotplot(ego, title = paste0("EnrichmentGO ",input$EXAMPLE3_ontology_choice," Dotplot"))
  }else if(input$EXAMPLE3_GOplot_choice=="barplot"){
    barplot(ego, showCategory=10,title=paste0("EnrichmentGO ",input$EXAMPLE3_ontology_choice," Barplot"))
  }
})
###enrichKEGG plot###
EXAMPLE3_KEGG_data <- reactive({
  data <- EXAMPLE3_result_dataset()
  filterdata <- subset(data, P.Value < input$EXAMPLE3_KEGG_pvalue)
  bitrdata <- bitr(filterdata$GeneSym, fromType = "SYMBOL", toType = c("ENSEMBL", "ENTREZID"), OrgDb = "org.Hs.eg.db")
  input_kegg <- input$EXAMPLE3_KEGG_choice
  ego_k <- enrichKEGG(gene           = bitrdata$ENTREZID,
                      keyType        = input_kegg,
                      organism       = 'hsa',
                      pvalueCutoff   = input$EXAMPLE3_KEGG_cutoff,
                      pAdjustMethod  = "BH",
                      qvalueCutoff   = input$EXAMPLE3_KEGG_cutoff)
  return(ego_k)
})

output$EXAMPLE3_enrichKEGG_dotplot <- renderPlot({
  ego_k <- EXAMPLE3_KEGG_data()
  if(input$EXAMPLE3_KEGGplot_choice=="dotplot"){
    dotplot(ego_k, title = paste0("Enrichment ","KEGG"," Dotplot"))
  }else if(input$EXAMPLE3_KEGGplot_choice=="barplot"){
    barplot(ego_k, showCategory=10, title=paste0("EnrichmentKEGG ","KEGG"," Barplot"))
  }
})

