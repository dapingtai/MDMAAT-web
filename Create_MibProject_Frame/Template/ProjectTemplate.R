##############################################OverView######################################################
output$TEMPLATE_title <- renderText({
  as.character(readxl::read_xlsx('www/ProjectProgress.xlsx', sheet = 'TEMPLATE', col_names = FALSE)[[2]][1])
})

output$TEMPLATE_aim <- renderText({
  as.character(readxl::read_xlsx('www/ProjectProgress.xlsx', sheet = 'TEMPLATE', col_names = FALSE)[[2]][2])
})

output$TEMPLATE_strategy <- renderText({
  as.character(readxl::read_xlsx('www/ProjectProgress.xlsx', sheet = 'TEMPLATE', col_names = FALSE)[[2]][3])
})

output$TEMPLATE_finding <- renderText({
  as.character(readxl::read_xlsx('www/ProjectProgress.xlsx', sheet = 'TEMPLATE', col_names = FALSE)[[2]][4])
})

output$TEMPLATE_participant <- renderText({
  as.character(readxl::read_xlsx('www/ProjectProgress.xlsx', sheet = 'TEMPLATE', col_names = FALSE)[[2]][5])
})

output$TEMPLATE_progress <- DT::renderDataTable({
  x <- readxl::read_xlsx('www/ProjectProgress.xlsx', sheet = 'TEMPLATE')
  summ <- as.character(x[[2]][1:4])
  names(summ) <- as.character(x[[1]][1:4])
  prog <- data.frame(x[-(1:5),])
  colnames(prog) <- as.character(x[5,])
  prog$Document <- paste0('<a href="', prog$Document, '" target="_blank">', 'link', '</a>')
  DT::datatable(prog, rownames = FALSE, escape = FALSE)
})
##########################################Select EXP In MySQL################################################
output$TEMPLATE_select_platform <- renderUI({
  platform.name <- c('Affy Human Genome U133 Plus 2.0 Array', 'Affy Mouse Genome 430 2.0 Array', 
                     'Infinium Human Methylation 450K BeadChip', 'Infinium MethylationEPIC BeadChip')
  names(platform.name) <- c('AFFY', 'M430', 'METH', 'MEPIC')
  platforms <- sub('_.*', '', mydb[grep('TEMPLATE_EXP', mydb)])
  names(platforms) <- platform.name[platforms]
  selectInput(inputId = 'TEMPLATE_select_platform', label = 'Select a platform', 
              choices = as.list(platforms))
})

TEMPLATE_selected_platform <- reactive({
  input$TEMPLATE_select_platform
})

TEMPLATE_list_sample <- reactive({
  if (length(TEMPLATE_selected_platform()) > 0){
    query <- paste0('SELECT * FROM ', paste0(TEMPLATE_selected_platform(), '_', 'TEMPLATE', '_EXP'))
    dbGetQuery(connect, query)
  }
})

TEMPLATE_list_variable <- reactive({
  if (length(TEMPLATE_selected_platform()) > 0){
    if (TEMPLATE_selected_platform() == 'AFFY') {info <- affy.info[, c(1:8, 9:10)]}
    if (TEMPLATE_selected_platform() == 'METH') {info <- meth.info[, c(1:8, 9:11)]}
    if (TEMPLATE_selected_platform() == 'M430') {info <- m430.info[, c(1:8, 9:10)]}
    if (TEMPLATE_selected_platform() == 'MEPIC') {info <- mepic.info[, c(1:8, 8:10)]}
    info
  }
})
##########################################Select Gene Data In MYSQL##########################################
TEMPLATE_selected_data <- reactive({
  if (length(TEMPLATE_selected_platform()) > 0){
    if (length(TEMPLATE_selected_sample()) > 0){
      if (length(TEMPLATE_selected_variable()) > 0){
        ids <- TEMPLATE_selected_variable()[, 'ID']
        query <- paste0('SELECT * FROM ', TEMPLATE_selected_platform(), "_TEMPLATE_DATA WHERE `ID` IN ('", ids, "');")
        tmp <- dbGetQuery(connect, query)
        as.numeric(tmp[1, -1])
      }
    }
  }
})

TEMPLATE_selected_limmadata <- reactive({
  if (length(TEMPLATE_selected_platform()) > 0){
    if (length(TEMPLATE_selected_sample()) > 0){
      if (length(TEMPLATE_selected_variable()) > 0){
        ids <- rownames(TEMPLATE_selected_limmaoutput())
        query <- paste0('SELECT * FROM ', TEMPLATE_selected_platform(), "_TEMPLATE_DATA WHERE `ID` IN ('", ids, "');")
        tmp <- dbGetQuery(connect, query)
        tmp[1, -1]
      }
    }
  }
})

####All Gene Data####
TEMPLATE_list_data <- reactive({
  if (length(TEMPLATE_selected_platform()) > 0){
    if (length(TEMPLATE_selected_sample()) > 0){
      if (length(TEMPLATE_selected_variable()) > 0){
        aquery <- paste0("SELECT * FROM ", TEMPLATE_selected_platform(), "_TEMPLATE_DATA;")
        atmp<-dbGetQuery(connect, aquery)
        atmp
      }
    }
  }
})
#################################################Sample######################################################
output$TEMPLATE_select_sample <- DT::renderDataTable({
  if (length(TEMPLATE_list_sample()) > 0){
    DT::datatable(TEMPLATE_list_sample(), selection = list(mode = 'multiple', selected = c(1:nrow(TEMPLATE_list_sample())), target = 'row') , 
                  filter = 'top', rownames = FALSE, escape = FALSE)
  }
})

TEMPLATE_selected_sample <- reactive({
  if (length(TEMPLATE_list_sample()) > 0){
    TEMPLATE_list_sample()[input$TEMPLATE_select_sample_rows_selected, , drop = FALSE]
  }
})

################################################Feature#####################################################
output$TEMPLATE_select_variable <- DT::renderDataTable({
  if (length(TEMPLATE_list_variable()) > 0){
    DT::datatable(TEMPLATE_list_variable(), selection = list(mode = 'single', selected = 1, target = 'row') , 
                  filter = 'top', rownames = FALSE, escape = FALSE)
  }
})

TEMPLATE_selected_variable <- reactive({
  if (length(TEMPLATE_list_variable()) > 0){
    TEMPLATE_list_variable()[input$TEMPLATE_select_variable_rows_selected, , drop = FALSE]
  }
})
################################################Data VIEW####################################################
output$TEMPLATE_orderby <- renderUI({
  if (length(TEMPLATE_list_sample()) > 0){
    selectInput(inputId = 'TEMPLATE_orderby', label = 'Ordered by', 
                choices = as.list(colnames(TEMPLATE_list_sample())))
  }
})

output$TEMPLATE_colorby <- renderUI({
  if (length(TEMPLATE_list_sample()) > 0){
    selectInput(inputId = 'TEMPLATE_colorby', label = 'Colored by', 
                choices = as.list(colnames(TEMPLATE_list_sample())))
  }
})

output$TEMPLATE_limma_orderby <- renderUI({
  if (length(TEMPLATE_list_sample()) > 0){
    pickerInput(inputId = 'TEMPLATE_limma_orderby', label = 'Limma ordered by', 
                choices = as.list(c(colnames(TEMPLATE_list_sample()),"Group")))
  }
})

output$TEMPLATE_limma_colorby <- renderUI({
  if (length(TEMPLATE_list_sample()) > 0){
    pickerInput(inputId = 'TEMPLATE_limma_colorby', label = 'Limma colored by', 
                choices = as.list(c(colnames(TEMPLATE_list_sample()),"Group")))
  }
})

############################################Group Comparsion#################################################
####Manipulate Info####
observeEvent(input$TEMPLATE_Select_Info, {
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

observeEvent(input$TEMPLATE_Download_Info, {
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
output$TEMPLATE_select_groupA <- DT::renderDataTable(server = FALSE, {
  if (length(TEMPLATE_list_sample()) > 0){
    DT::datatable(TEMPLATE_selected_sample(), extensions = c("Buttons", "Select"), 
                  options = list(dom = "lBftrip", buttons = list("selectAll","selectNone","csv","excel"), select = TRUE),
                  selection = "none" , 
                  filter = 'top', rownames = FALSE, escape = FALSE)
  }
})

TEMPLATE_selected_groupA <- reactive({
  if (length(TEMPLATE_list_sample()) > 0){
    if (length(TEMPLATE_selected_sample()) > 0){
      TEMPLATE_selected_sample()[input$TEMPLATE_select_groupA_rows_selected, , drop = FALSE]
    }
  }
})


####GroupA unselected#### 
TEMPLATE_unselected_groupA <- reactive({
  if (length(TEMPLATE_list_sample()) > 0){
    if (length(TEMPLATE_selected_sample()) > 0){
        TEMPLATE_selected_sample()[-input$TEMPLATE_select_groupA_rows_selected, , drop = FALSE]
    }
  }
})

####Group B####
output$TEMPLATE_select_groupB <- DT::renderDataTable(server = FALSE, {
  if (length(TEMPLATE_list_sample()) > 0){
    if (length(TEMPLATE_selected_sample()) > 0){
      if(length(input$TEMPLATE_select_groupA_rows_selected) > 0){
        DT::datatable(TEMPLATE_unselected_groupA(), extensions = c("Buttons", "Select"),
                      options = list(dom = "lBftrip", buttons = list("selectAll","selectNone","csv","excel"), select = TRUE),
                      selection = "none" , 
                       filter = 'top', rownames = FALSE, escape = FALSE)
      }
    }
  }
})

TEMPLATE_selected_groupB <- reactive({
  if (length(TEMPLATE_list_sample()) > 0){
    if (length(TEMPLATE_selected_sample()) > 0){
      if (length(rownames(TEMPLATE_unselected_groupA())) > 0){
          TEMPLATE_unselected_groupA()[input$TEMPLATE_select_groupB_rows_selected, , drop = FALSE]
      }
    }
  }
})
##### GroupA GroupB Download #####
TEMPLATE_download_groupA_data <- reactive({
  download_groupA <- TEMPLATE_selected_groupA_genedata()
  rownames(download_groupA) <- rownames(TEMPLATE_list_variable())
  download_groupA <- cbind(TEMPLATE_list_variable()[,c("GeneSym")], download_groupA)
  return(download_groupA)
})

output$TEMPLATE_download_groupA <- downloadHandler(
  filename = function(){"GropA_genedata.csv"},
  content = function(file){
    write.csv(TEMPLATE_download_groupA_data(), file)
  }
)

TEMPLATE_download_groupB_data <- reactive({
  download_groupB <- TEMPLATE_selected_groupB_genedata()
  rownames(download_groupB) <- rownames(TEMPLATE_list_variable())
  download_groupB <- cbind(TEMPLATE_list_variable()[,c("GeneSym")], download_groupB)
  return(download_groupB)
})

output$TEMPLATE_download_groupB <- downloadHandler(
  filename = function(){"GropB_genedata.csv"},
  content = function(file){
    write.csv(TEMPLATE_download_groupB_data(), file)
  }
)
####A B Summary ####
output$TEMPLATE_groupA_number <- renderText({
  paste0("Sample in GroupA : ",length(rownames(TEMPLATE_selected_groupA())))
})

output$TEMPLATE_groupB_number <- renderText({
  if(length(rownames(TEMPLATE_selected_groupA()))==0){
    paste0("Sample in groupB : ",0)
  }else{
    paste0("Sample in groupB : ",length(rownames(TEMPLATE_selected_groupB())))
  }
})
####Find AB Pair####
TEMPLATE_find_pair <- reactive({
  if (length(TEMPLATE_selected_groupA())>0 && length(TEMPLATE_selected_groupB())>0){
    GroupA <- TEMPLATE_selected_groupA()
    GroupA_index <- duplicated(GroupA$Patient.ID)
    GroupA_name <- GroupA[!GroupA_index,c("Patient.ID","Sample.Name")]
    GroupB <- TEMPLATE_selected_groupB()
    GroupB_index <- duplicated(GroupB$Patient.ID)
    GroupB_name <- GroupB[!GroupB_index,c("Patient.ID","Sample.Name")]
    GroupAB_name <- inner_join(GroupA_name, GroupB_name, by = "Patient.ID", suffix = c(".groupA", ".groupB"))
    GroupAB_name
  }
})

TEMPLATE_count_pair <- reactive({
  if (length(rownames(TEMPLATE_selected_groupA()))>0 && length(rownames(TEMPLATE_selected_groupB()))>0){
    paircount<-length(rownames(TEMPLATE_find_pair()))
  }else{
    paircount<-0
  }
  paircount
})

####Pair Summary####
output$TEMPLATE_pair_number <- renderText({
  paste0("Patient in both group : ",TEMPLATE_count_pair())
})
####Pair Warning####
output$TEMPLATE_pair_check <- renderUI({
  if(TEMPLATE_count_pair()>3){
    prettyToggle(inputId = "TEMPLATE_paircheck_button",
                 label_on = "Patient in both group more than 3", 
                 icon_on = icon("check"),
                 status_on = "info",
                 status_off = "info", 
                 label_off = "Patient in both group more than 3",
                 icon_off = icon("check"))
  }else{
    prettyToggle(inputId = "TEMPLATE_paircheck_button",
                 label_on = "Patient in both group less than 3", 
                 icon_on = icon("remove"),
                 status_on = "warning",
                 status_off = "warning", 
                 label_off = "Patient in both group less than 3",
                 icon_off = icon("remove"))
  }
})
####Pair decide method####
output$TEMPLATE_choice_method <- renderUI({
  if (length(TEMPLATE_list_sample()) > 0){
    pickerInput(inputId = "TEMPLATE_method", label = "ChoiceMethod:", choices = TEMPLATE_decide_method(),
                choicesOpt = list(icon = c("glyphicon-arrow-right","glyphicon-arrow-right")))
  }
})

TEMPLATE_decide_method <- reactive({
  if (TEMPLATE_count_pair() > 3){
    method <- c("Unpair DE analysis(Limma)", "Pair DE analysis(Limma)")
  }else{
    method <- c("Unpair DE analysis(Limma)")
  } 
    method
})

##########Gene select data##########
TEMPLATE_selected_genedata <- reactive({
  if (length(TEMPLATE_list_sample()) > 0 && length(TEMPLATE_list_data()) > 0){
    TEMPLATE_list_data <- TEMPLATE_list_data()[,-1]
    TEMPLATE_list_data[,input$TEMPLATE_select_sample_rows_selected, drop = FALSE]
  }
})

TEMPLATE_selected_groupA_genedata <- reactive({
  if (length(TEMPLATE_list_sample()) > 0 && length(TEMPLATE_list_data()) > 0){
    if (length(TEMPLATE_selected_groupA()) > 0 && length(TEMPLATE_selected_genedata()) >0){
      TEMPLATE_selected_genedata()[,input$TEMPLATE_select_groupA_rows_selected, drop = FALSE]
    }
  }
})
TEMPLATE_unselected_groupA_genedata <- reactive({
  if (length(TEMPLATE_list_sample()) > 0 && length(TEMPLATE_list_data()) > 0){
    if (length(TEMPLATE_selected_groupA()) > 0 && length(TEMPLATE_selected_genedata()) >0){
      TEMPLATE_selected_genedata()[,-input$TEMPLATE_select_groupA_rows_selected, drop = FALSE]
    }
  }
})
TEMPLATE_selected_groupB_genedata <- reactive({
  if (length(TEMPLATE_list_sample()) > 0 && length(TEMPLATE_list_data()) > 0){
    if (length(TEMPLATE_selected_groupA()) > 0 && length(TEMPLATE_selected_genedata()) >0){
      TEMPLATE_unselected_groupA_genedata()[,input$TEMPLATE_select_groupB_rows_selected, drop = FALSE]
    }
  }
})


####Limma#####
TEMPLATE_limma_fitC <- eventReactive(input$TEMPLATE_draw, {
  if (length(TEMPLATE_selected_groupA_genedata())>0 && length(TEMPLATE_selected_groupB_genedata())>0){
    groupA_gene <- TEMPLATE_selected_groupA_genedata()
    groupB_gene <- TEMPLATE_selected_groupB_genedata()
    if(input$TEMPLATE_method=="Unpair DE analysis(Limma)"){
      groupAB_gene <- data.frame(cbind(groupA_gene,groupB_gene))
      rownames(groupAB_gene)<-TEMPLATE_list_data()[,1]
      group_list <- factor(c(rep(1,length(colnames(groupA_gene))),rep(0,length(colnames(groupB_gene))))) 
      design <- model.matrix(~group_list-1)
      colnames(design)<-c("GA","GB")
      contrast_matrix <- makeContrasts("GA-GB", levels = design)
      fit <- lmFit(groupAB_gene, design)
      fitC <- contrasts.fit(fit, contrast_matrix)
      fitC <- eBayes(fitC)
      fitC
    }
    else if(input$TEMPLATE_method=="Pair DE analysis(Limma)"){
      pair_data <- TEMPLATE_find_pair()
      groupA_pair_name <- pair_data$Sample.Name.groupA 
      groupB_pair_name <- pair_data$Sample.Name.groupB
      groupA_final_name <- groupA_pair_name %>% gsub(" ",replacement=".", .) %>% gsub("-",replacement=".", .)
      groupB_final_name <- groupB_pair_name %>% gsub(" ",replacement=".", .) %>% gsub("-",replacement=".", .)
      groupAB_name <- c(groupA_final_name, groupB_final_name)
      pair_number<- as.integer(TEMPLATE_count_pair())
      groupAB_gene <- data.frame(cbind(groupA_gene, groupB_gene), check.names = FALSE)
      groupAB_gene <- groupAB_gene[, groupAB_name]
      rownames(groupAB_gene)<-TEMPLATE_list_data()[,1]
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

TEMPLATE_limma_output <- reactive({
  limma.output <- signif(topTable(TEMPLATE_fitC_dataset(), n=length(rownames(TEMPLATE_list_data()))), digits = 3)
  GroupA_means <- rowMeans(as.matrix(TEMPLATE_selected_groupA_genedata()))
  GroupB_means <- rowMeans(as.matrix(TEMPLATE_selected_groupB_genedata()))
  GroupAB_means <- signif(data.frame(GroupA_means, GroupB_means), digits = 3)
  genenames <- TEMPLATE_list_variable()
  rownames(GroupAB_means) <- rownames(genenames)
  limma.output <- cbind(genenames[rownames(limma.output), c("GeneSym")], GroupAB_means[rownames(limma.output),], limma.output[,-c(3,6)])
  colnames(limma.output)[1] <- "GeneSym"
  limma.output
})

output$TEMPLATE_select_limmaoutput <- DT::renderDataTable({
  if(length(input$TEMPLATE_select_groupA_rows_selected) > 0 && length(input$TEMPLATE_select_groupB_rows_selected) > 0)
    DT::datatable(TEMPLATE_limma_output(), extensions = "Buttons", options = list(dom = "lBftrip", buttons = list("colvis","csv","excel","pdf","print")),
                  selection = list(mode = 'single', selected = 1, target = 'row'), 
                  filter = 'top', rownames = TRUE, escape = FALSE)
})

output$TEMPLATE_download_result <- downloadHandler(
  filename = function(){"TEMPLATE_DE_Result.csv"},
  content = function(file){
    write.csv(TEMPLATE_limma_output(), file)
  }
)

TEMPLATE_selected_limmaoutput <- reactive({
  if (length(TEMPLATE_list_variable()) > 0){
    TEMPLATE_limma_output()[input$TEMPLATE_select_limmaoutput_rows_selected, , drop = FALSE]
  }
})
###################################################Plot#####################################################
#####Feature DATA VIEW####
output$TEMPLATE_select_data_bar <- renderPlot({
  if (length(TEMPLATE_selected_platform()) > 0){
    if (length(TEMPLATE_selected_sample()) > 0){
      if (length(TEMPLATE_selected_variable()) > 0){
        if (length(TEMPLATE_selected_data()) > 0){
          par(mar = c(13, 4, 4, 2))
          x <- TEMPLATE_selected_data()[input$TEMPLATE_select_sample_rows_selected]
          y <- TEMPLATE_list_sample()[input$TEMPLATE_select_sample_rows_selected, ]
          z <- TEMPLATE_list_variable()[input$TEMPLATE_select_variable_rows_selected, ]
          u <- y[, input$TEMPLATE_orderby]
          v <- y[, input$TEMPLATE_colorby]
          x <- x[order(u)]
          y <- y[order(u), ]
          v <- factor(v[order(u)])
          plot(x, type = 'h', lwd = 10, xlab = '', ylab = 'Value', axes = FALSE,
               col = rainbow(length(levels(v)))[factor(v)],
               main = paste0(z[1, -1],collapse = ', '))
          abline(h = seq(0, 20, by = 0.2), lty = 3, col = 'gray')
          axis(1, at = 1:length(TEMPLATE_selected_data()[input$TEMPLATE_select_sample_rows_selected]), 
               y[, 1], las = 2)
          axis(2, at = seq(0, 20, by = 0.2), seq(0, 20, by = 0.2), las = 2)
        }
      }
    }
  }
})

############### Waiter ####################
TEMPLATE_Vol_w <- call_waitress("#TEMPLATE_DEvolcanoplot", theme = "overlay-percent")
TEMPLATE_MD_w <- call_waitress("#TEMPLATE_DEplotMD", theme = "overlay-percent")
TEMPLATE_Barview_w <- call_waitress("#TEMPLATE_select_limmadata_bar", theme = "line")
TEMPLATE_Boxview_w <- call_waitress("#TEMPLATE_boxplotly", theme = "line")
#注意Waiter tag命名不可有"."否則影響Js的ID #tag
TEMPLATE_fitC_dataset <- eventReactive(input$TEMPLATE_draw, {
  TEMPLATE_Vol_w$start()
  TEMPLATE_Vol_w$auto(5, 150)
  TEMPLATE_MD_w$start()
  TEMPLATE_MD_w$auto(5, 150)
  TEMPLATE_Barview_w$start(h3(strong("Analysis Stuff....")))
  TEMPLATE_Barview_w$auto(5,200)
  TEMPLATE_Boxview_w$start(h3(strong("Creating Boxplot.....")))
  TEMPLATE_Boxview_w$auto(5,200)
  Sys.sleep(1)
  TEMPLATE_Vol_w$close()
  TEMPLATE_MD_w$close()
  TEMPLATE_Barview_w$close()
  TEMPLATE_Boxview_w$close()
  return(TEMPLATE_limma_fitC())
})

TEMPLATE_enrichGo_w <- call_waitress("#TEMPLATE_enrichGO_dotplot", theme = "overlay-percent")
TEMPLATE_enrichKEGG_w <- call_waitress("#TEMPLATE_enrichKEGG_dotplot", theme = "overlay-percent")
TEMPLATE_result_dataset <- eventReactive(input$TEMPLATE_start_enrichment, {
  TEMPLATE_enrichGo_w$start()
  TEMPLATE_enrichKEGG_w$start()
  TEMPLATE_enrichGo_w$auto(5,200)
  TEMPLATE_enrichKEGG_w$auto(5,200)
  Sys.sleep(1)
  TEMPLATE_enrichGo_w$close()
  TEMPLATE_enrichKEGG_w$close()
  return(TEMPLATE_limma_output())
})

##########COMPARISON DIAGRAM##############
output$TEMPLATE_DEvolcanoplot <- renderPlot({
  volcanoplot(TEMPLATE_fitC_dataset(), highlight = TRUE)
  abline(v=c(-1,-log2(1.5),log2(1.5),1), lty = c(1, 2, 2, 1))
  abline(h=c(-log10(0.05), -log10(0.01)), lty = c(1, 2))
})

output$TEMPLATE_DEplotMD <- renderPlot({
  plotMD(TEMPLATE_fitC_dataset(), status = decideTests(TEMPLATE_fitC_dataset(), p.value=0.05), 
         main = colnames(TEMPLATE_fitC_dataset()))
})

#########Comparsion Viewer ############
output$TEMPLATE_limmadatabar_nocall <- renderText({
  if (length(input$TEMPLATE_select_limmaoutput_rows_selected) < 1){
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
TEMPLATE_groupAB_data <- reactive({
  groupA_row_select <- TEMPLATE_selected_groupA()
  groupB_row_select <- TEMPLATE_selected_groupB()
  groupA_row_select[,"Group"] <- "GroupA"
  groupB_row_select[,"Group"] <- "GroupB"
  groupAB_row_select <- data.frame(rbind(groupA_row_select, groupB_row_select))
  return(groupAB_row_select)
})

###bar###
output$TEMPLATE_select_limmadata_bar <- renderPlot({
  if (length(TEMPLATE_selected_platform()) > 0){
    if (length(TEMPLATE_selected_sample()) > 0){
      if (length(TEMPLATE_selected_variable()) > 0){
        if (length(input$TEMPLATE_select_groupA_rows_selected) > 0 && length(input$TEMPLATE_select_groupB_rows_selected) > 0){
          if (length(input$TEMPLATE_select_limmaoutput_rows_selected) > 0){
            par(mar = c(13, 4, 4, 2))
            groupAB.name.selected <- c(colnames(TEMPLATE_selected_groupA_genedata()),colnames(TEMPLATE_selected_groupB_genedata()))
            x <- as.numeric(TEMPLATE_selected_limmadata()[,groupAB.name.selected])
            y <- TEMPLATE_groupAB_data()
            z <- TEMPLATE_limma_output()[input$TEMPLATE_select_limmaoutput_rows_selected, ]
            u <- y[, input$TEMPLATE_limma_orderby]
            v <- y[, input$TEMPLATE_limma_colorby]
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
output$TEMPLATE_boxplotly <- renderPlotly({
  if (length(input$TEMPLATE_select_groupA_rows_selected) > 0 && length(input$TEMPLATE_select_groupB_rows_selected) > 0){
    if (length(input$TEMPLATE_select_limmaoutput_rows_selected) > 0){
      groupAB.name.selected <- c(colnames(TEMPLATE_selected_groupA_genedata()),colnames(TEMPLATE_selected_groupB_genedata()))
      groupAB.row.select <- TEMPLATE_groupAB_data()
      gene.express <- as.numeric(TEMPLATE_selected_limmadata()[,groupAB.name.selected])
      groupAB.row.select[,"GeneExpress"] <- gene.express
      plot_ly(groupAB.row.select, y=~GeneExpress, color = ~Group, type = "box", boxpoints = "all", pointpos = -2, jitter = 3)
    }
  }
})

############### Enrichment Analysis ################
###enrichGo plot###
TEMPLATE_GO_data <- reactive({
  data <- TEMPLATE_result_dataset()
  filterdata <- subset(data, P.Value < input$TEMPLATE_GO_pvalue)
  bitrdata <- bitr(filterdata$GeneSym, fromType = "SYMBOL", toType = c("ENSEMBL", "ENTREZID"), OrgDb = "org.Hs.eg.db")
  input_ontology <- input$TEMPLATE_ontology_choice
  ego <- enrichGO(gene          = bitrdata$ENTREZID,
                  universe      = names(data(geneList, package = "DOSE")),
                  OrgDb         = org.Hs.eg.db,
                  ont           = input_ontology,
                  pAdjustMethod = "BH",
                  pvalueCutoff  = input$TEMPLATE_GO_cutoff,
                  qvalueCutoff  = input$TEMPLATE_GO_cutoff,
                  readable      = FALSE)
  return(ego)
})

output$TEMPLATE_enrichGO_dotplot <- renderPlot({
  ego <- TEMPLATE_GO_data()
if (input$TEMPLATE_GOplot_choice=="dotplot"){
    dotplot(ego, title = paste0("EnrichmentGO ",input$TEMPLATE_ontology_choice," Dotplot"))
  }else if(input$TEMPLATE_GOplot_choice=="barplot"){
    barplot(ego, showCategory=10,title=paste0("EnrichmentGO ",input$TEMPLATE_ontology_choice," Barplot"))
  }
})
###enrichKEGG plot###
TEMPLATE_KEGG_data <- reactive({
  data <- TEMPLATE_result_dataset()
  filterdata <- subset(data, P.Value < input$TEMPLATE_KEGG_pvalue)
  bitrdata <- bitr(filterdata$GeneSym, fromType = "SYMBOL", toType = c("ENSEMBL", "ENTREZID"), OrgDb = "org.Hs.eg.db")
  input_kegg <- input$TEMPLATE_KEGG_choice
  ego_k <- enrichKEGG(gene           = bitrdata$ENTREZID,
                      keyType        = input_kegg,
                      organism       = 'hsa',
                      pvalueCutoff   = input$TEMPLATE_KEGG_cutoff,
                      pAdjustMethod  = "BH",
                      qvalueCutoff   = input$TEMPLATE_KEGG_cutoff)
  return(ego_k)
})

output$TEMPLATE_enrichKEGG_dotplot <- renderPlot({
  ego_k <- TEMPLATE_KEGG_data()
  if(input$TEMPLATE_KEGGplot_choice=="dotplot"){
    dotplot(ego_k, title = paste0("Enrichment ","KEGG"," Dotplot"))
  }else if(input$TEMPLATE_KEGGplot_choice=="barplot"){
    barplot(ego_k, showCategory=10, title=paste0("EnrichmentKEGG ","KEGG"," Barplot"))
  }
})

