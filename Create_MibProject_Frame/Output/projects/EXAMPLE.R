##############################################OverView######################################################
output$EXAMPLE_title <- renderText({
  as.character(readxl::read_xlsx('www/ProjectProgress.xlsx', sheet = 'EXAMPLE', col_names = FALSE)[[2]][1])
})

output$EXAMPLE_aim <- renderText({
  as.character(readxl::read_xlsx('www/ProjectProgress.xlsx', sheet = 'EXAMPLE', col_names = FALSE)[[2]][2])
})

output$EXAMPLE_strategy <- renderText({
  as.character(readxl::read_xlsx('www/ProjectProgress.xlsx', sheet = 'EXAMPLE', col_names = FALSE)[[2]][3])
})

output$EXAMPLE_finding <- renderText({
  as.character(readxl::read_xlsx('www/ProjectProgress.xlsx', sheet = 'EXAMPLE', col_names = FALSE)[[2]][4])
})

output$EXAMPLE_participant <- renderText({
  as.character(readxl::read_xlsx('www/ProjectProgress.xlsx', sheet = 'EXAMPLE', col_names = FALSE)[[2]][5])
})

output$EXAMPLE_progress <- DT::renderDataTable({
  x <- readxl::read_xlsx('www/ProjectProgress.xlsx', sheet = 'EXAMPLE')
  summ <- as.character(x[[2]][1:4])
  names(summ) <- as.character(x[[1]][1:4])
  prog <- data.frame(x[-(1:5),])
  colnames(prog) <- as.character(x[5,])
  prog$Document <- paste0('<a href="', prog$Document, '" target="_blank">', 'link', '</a>')
  DT::datatable(prog, rownames = FALSE, escape = FALSE)
})
##########################################Select EXP In MySQL################################################
output$EXAMPLE_select_platform <- renderUI({
  platform.name <- c('Affy Human Genome U133 Plus 2.0 Array', 'Affy Mouse Genome 430 2.0 Array', 
                     'Infinium Human Methylation 450K BeadChip', 'Infinium MethylationEPIC BeadChip')
  names(platform.name) <- c('AFFY', 'M430', 'METH', 'MEPIC')
  platforms <- sub('_.*', '', mydb[grep('EXAMPLE_EXP', mydb)])
  names(platforms) <- platform.name[platforms]
  selectInput(inputId = 'EXAMPLE_select_platform', label = 'Select a platform', 
              choices = as.list(platforms))
})

EXAMPLE_selected_platform <- reactive({
  input$EXAMPLE_select_platform
})

EXAMPLE_list_sample <- reactive({
  if (length(EXAMPLE_selected_platform()) > 0){
    query <- paste0('SELECT * FROM ', paste0(EXAMPLE_selected_platform(), '_', 'EXAMPLE', '_EXP'))
    dbGetQuery(connect, query)
  }
})

EXAMPLE_list_variable <- reactive({
  if (length(EXAMPLE_selected_platform()) > 0){
    if (EXAMPLE_selected_platform() == 'AFFY') {info <- affy.info[, c(1:8, 9:10)]}
    if (EXAMPLE_selected_platform() == 'METH') {info <- meth.info[, c(1:8, 9:11)]}
    if (EXAMPLE_selected_platform() == 'M430') {info <- m430.info[, c(1:8, 9:10)]}
    if (EXAMPLE_selected_platform() == 'MEPIC') {info <- mepic.info[, c(1:8, 8:10)]}
    info
  }
})
##########################################Select Gene Data In MYSQL##########################################
EXAMPLE_selected_data <- reactive({
  if (length(EXAMPLE_selected_platform()) > 0){
    if (length(EXAMPLE_selected_sample()) > 0){
      if (length(EXAMPLE_selected_variable()) > 0){
        ids <- EXAMPLE_selected_variable()[, 'ID']
        query <- paste0('SELECT * FROM ', EXAMPLE_selected_platform(), "_EXAMPLE_DATA WHERE `ID` IN ('", ids, "');")
        tmp <- dbGetQuery(connect, query)
        as.numeric(tmp[1, -1])
      }
    }
  }
})

EXAMPLE_selected_limmadata <- reactive({
  if (length(EXAMPLE_selected_platform()) > 0){
    if (length(EXAMPLE_selected_sample()) > 0){
      if (length(EXAMPLE_selected_variable()) > 0){
        ids <- rownames(EXAMPLE_selected_limmaoutput())
        query <- paste0('SELECT * FROM ', EXAMPLE_selected_platform(), "_EXAMPLE_DATA WHERE `ID` IN ('", ids, "');")
        tmp <- dbGetQuery(connect, query)
        tmp[1, -1]
      }
    }
  }
})

####All Gene Data####
EXAMPLE_list_data <- reactive({
  if (length(EXAMPLE_selected_platform()) > 0){
    if (length(EXAMPLE_selected_sample()) > 0){
      if (length(EXAMPLE_selected_variable()) > 0){
        aquery <- paste0("SELECT * FROM ", EXAMPLE_selected_platform(), "_EXAMPLE_DATA;")
        atmp<-dbGetQuery(connect, aquery)
        atmp
      }
    }
  }
})
#################################################Sample######################################################
output$EXAMPLE_select_sample <- DT::renderDataTable({
  if (length(EXAMPLE_list_sample()) > 0){
    DT::datatable(EXAMPLE_list_sample(), selection = list(mode = 'multiple', selected = c(1:nrow(EXAMPLE_list_sample())), target = 'row') , 
                  filter = 'top', rownames = FALSE, escape = FALSE)
  }
})

EXAMPLE_selected_sample <- reactive({
  if (length(EXAMPLE_list_sample()) > 0){
    EXAMPLE_list_sample()[input$EXAMPLE_select_sample_rows_selected, , drop = FALSE]
  }
})

################################################Feature#####################################################
output$EXAMPLE_select_variable <- DT::renderDataTable({
  if (length(EXAMPLE_list_variable()) > 0){
    DT::datatable(EXAMPLE_list_variable(), selection = list(mode = 'single', selected = 1, target = 'row') , 
                  filter = 'top', rownames = FALSE, escape = FALSE)
  }
})

EXAMPLE_selected_variable <- reactive({
  if (length(EXAMPLE_list_variable()) > 0){
    EXAMPLE_list_variable()[input$EXAMPLE_select_variable_rows_selected, , drop = FALSE]
  }
})
################################################Data VIEW####################################################
output$EXAMPLE_orderby <- renderUI({
  if (length(EXAMPLE_list_sample()) > 0){
    selectInput(inputId = 'EXAMPLE_orderby', label = 'Ordered by', 
                choices = as.list(colnames(EXAMPLE_list_sample())))
  }
})

output$EXAMPLE_colorby <- renderUI({
  if (length(EXAMPLE_list_sample()) > 0){
    selectInput(inputId = 'EXAMPLE_colorby', label = 'Colored by', 
                choices = as.list(colnames(EXAMPLE_list_sample())))
  }
})

output$EXAMPLE_limma_orderby <- renderUI({
  if (length(EXAMPLE_list_sample()) > 0){
    pickerInput(inputId = 'EXAMPLE_limma_orderby', label = 'Limma ordered by', 
                choices = as.list(c(colnames(EXAMPLE_list_sample()),"Group")))
  }
})

output$EXAMPLE_limma_colorby <- renderUI({
  if (length(EXAMPLE_list_sample()) > 0){
    pickerInput(inputId = 'EXAMPLE_limma_colorby', label = 'Limma colored by', 
                choices = as.list(c(colnames(EXAMPLE_list_sample()),"Group")))
  }
})

############################################Group Comparsion#################################################
####Manipulate Info####
observeEvent(input$EXAMPLE_Select_Info, {
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

observeEvent(input$EXAMPLE_Download_Info, {
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
output$EXAMPLE_select_groupA <- DT::renderDataTable(server = FALSE, {
  if (length(EXAMPLE_list_sample()) > 0){
    DT::datatable(EXAMPLE_selected_sample(), extensions = c("Buttons", "Select"), 
                  options = list(dom = "lBftrip", buttons = list("selectAll","selectNone","csv","excel"), select = TRUE),
                  selection = "none" , 
                  filter = 'top', rownames = FALSE, escape = FALSE)
  }
})

EXAMPLE_selected_groupA <- reactive({
  if (length(EXAMPLE_list_sample()) > 0){
    if (length(EXAMPLE_selected_sample()) > 0){
      EXAMPLE_selected_sample()[input$EXAMPLE_select_groupA_rows_selected, , drop = FALSE]
    }
  }
})


####GroupA unselected#### 
EXAMPLE_unselected_groupA <- reactive({
  if (length(EXAMPLE_list_sample()) > 0){
    if (length(EXAMPLE_selected_sample()) > 0){
        EXAMPLE_selected_sample()[-input$EXAMPLE_select_groupA_rows_selected, , drop = FALSE]
    }
  }
})

####Group B####
output$EXAMPLE_select_groupB <- DT::renderDataTable(server = FALSE, {
  if (length(EXAMPLE_list_sample()) > 0){
    if (length(EXAMPLE_selected_sample()) > 0){
      if(length(input$EXAMPLE_select_groupA_rows_selected) > 0){
        DT::datatable(EXAMPLE_unselected_groupA(), extensions = c("Buttons", "Select"),
                      options = list(dom = "lBftrip", buttons = list("selectAll","selectNone","csv","excel"), select = TRUE),
                      selection = "none" , 
                       filter = 'top', rownames = FALSE, escape = FALSE)
      }
    }
  }
})

EXAMPLE_selected_groupB <- reactive({
  if (length(EXAMPLE_list_sample()) > 0){
    if (length(EXAMPLE_selected_sample()) > 0){
      if (length(rownames(EXAMPLE_unselected_groupA())) > 0){
          EXAMPLE_unselected_groupA()[input$EXAMPLE_select_groupB_rows_selected, , drop = FALSE]
      }
    }
  }
})
##### GroupA GroupB Download #####
EXAMPLE_download_groupA_data <- reactive({
  download_groupA <- EXAMPLE_selected_groupA_genedata()
  rownames(download_groupA) <- rownames(EXAMPLE_list_variable())
  download_groupA <- cbind(EXAMPLE_list_variable()[,c("GeneSym")], download_groupA)
  return(download_groupA)
})

output$EXAMPLE_download_groupA <- downloadHandler(
  filename = function(){"GropA_genedata.csv"},
  content = function(file){
    write.csv(EXAMPLE_download_groupA_data(), file)
  }
)

EXAMPLE_download_groupB_data <- reactive({
  download_groupB <- EXAMPLE_selected_groupB_genedata()
  rownames(download_groupB) <- rownames(EXAMPLE_list_variable())
  download_groupB <- cbind(EXAMPLE_list_variable()[,c("GeneSym")], download_groupB)
  return(download_groupB)
})

output$EXAMPLE_download_groupB <- downloadHandler(
  filename = function(){"GropB_genedata.csv"},
  content = function(file){
    write.csv(EXAMPLE_download_groupB_data(), file)
  }
)
####A B Summary ####
output$EXAMPLE_groupA_number <- renderText({
  paste0("Sample in GroupA : ",length(rownames(EXAMPLE_selected_groupA())))
})

output$EXAMPLE_groupB_number <- renderText({
  if(length(rownames(EXAMPLE_selected_groupA()))==0){
    paste0("Sample in groupB : ",0)
  }else{
    paste0("Sample in groupB : ",length(rownames(EXAMPLE_selected_groupB())))
  }
})
####Find AB Pair####
EXAMPLE_find_pair <- reactive({
  if (length(EXAMPLE_selected_groupA())>0 && length(EXAMPLE_selected_groupB())>0){
    GroupA <- EXAMPLE_selected_groupA()
    GroupA_index <- duplicated(GroupA$Patient.ID)
    GroupA_name <- GroupA[!GroupA_index,c("Patient.ID","Sample.Name")]
    GroupB <- EXAMPLE_selected_groupB()
    GroupB_index <- duplicated(GroupB$Patient.ID)
    GroupB_name <- GroupB[!GroupB_index,c("Patient.ID","Sample.Name")]
    GroupAB_name <- inner_join(GroupA_name, GroupB_name, by = "Patient.ID", suffix = c(".groupA", ".groupB"))
    GroupAB_name
  }
})

EXAMPLE_count_pair <- reactive({
  if (length(rownames(EXAMPLE_selected_groupA()))>0 && length(rownames(EXAMPLE_selected_groupB()))>0){
    paircount<-length(rownames(EXAMPLE_find_pair()))
  }else{
    paircount<-0
  }
  paircount
})

####Pair Summary####
output$EXAMPLE_pair_number <- renderText({
  paste0("Patient in both group : ",EXAMPLE_count_pair())
})
####Pair Warning####
output$EXAMPLE_pair_check <- renderUI({
  if(EXAMPLE_count_pair()>3){
    prettyToggle(inputId = "EXAMPLE_paircheck_button",
                 label_on = "Patient in both group more than 3", 
                 icon_on = icon("check"),
                 status_on = "info",
                 status_off = "info", 
                 label_off = "Patient in both group more than 3",
                 icon_off = icon("check"))
  }else{
    prettyToggle(inputId = "EXAMPLE_paircheck_button",
                 label_on = "Patient in both group less than 3", 
                 icon_on = icon("remove"),
                 status_on = "warning",
                 status_off = "warning", 
                 label_off = "Patient in both group less than 3",
                 icon_off = icon("remove"))
  }
})
####Pair decide method####
output$EXAMPLE_choice_method <- renderUI({
  if (length(EXAMPLE_list_sample()) > 0){
    pickerInput(inputId = "EXAMPLE_method", label = "ChoiceMethod:", choices = EXAMPLE_decide_method(),
                choicesOpt = list(icon = c("glyphicon-arrow-right","glyphicon-arrow-right")))
  }
})

EXAMPLE_decide_method <- reactive({
  if (EXAMPLE_count_pair() > 3){
    method <- c("Unpair DE analysis(Limma)", "Pair DE analysis(Limma)")
  }else{
    method <- c("Unpair DE analysis(Limma)")
  } 
    method
})

##########Gene select data##########
EXAMPLE_selected_genedata <- reactive({
  if (length(EXAMPLE_list_sample()) > 0 && length(EXAMPLE_list_data()) > 0){
    EXAMPLE_list_data <- EXAMPLE_list_data()[,-1]
    EXAMPLE_list_data[,input$EXAMPLE_select_sample_rows_selected, drop = FALSE]
  }
})

EXAMPLE_selected_groupA_genedata <- reactive({
  if (length(EXAMPLE_list_sample()) > 0 && length(EXAMPLE_list_data()) > 0){
    if (length(EXAMPLE_selected_groupA()) > 0 && length(EXAMPLE_selected_genedata()) >0){
      EXAMPLE_selected_genedata()[,input$EXAMPLE_select_groupA_rows_selected, drop = FALSE]
    }
  }
})
EXAMPLE_unselected_groupA_genedata <- reactive({
  if (length(EXAMPLE_list_sample()) > 0 && length(EXAMPLE_list_data()) > 0){
    if (length(EXAMPLE_selected_groupA()) > 0 && length(EXAMPLE_selected_genedata()) >0){
      EXAMPLE_selected_genedata()[,-input$EXAMPLE_select_groupA_rows_selected, drop = FALSE]
    }
  }
})
EXAMPLE_selected_groupB_genedata <- reactive({
  if (length(EXAMPLE_list_sample()) > 0 && length(EXAMPLE_list_data()) > 0){
    if (length(EXAMPLE_selected_groupA()) > 0 && length(EXAMPLE_selected_genedata()) >0){
      EXAMPLE_unselected_groupA_genedata()[,input$EXAMPLE_select_groupB_rows_selected, drop = FALSE]
    }
  }
})


####Limma#####
EXAMPLE_limma_fitC <- eventReactive(input$EXAMPLE_draw, {
  if (length(EXAMPLE_selected_groupA_genedata())>0 && length(EXAMPLE_selected_groupB_genedata())>0){
    groupA_gene <- EXAMPLE_selected_groupA_genedata()
    groupB_gene <- EXAMPLE_selected_groupB_genedata()
    if(input$EXAMPLE_method=="Unpair DE analysis(Limma)"){
      groupAB_gene <- data.frame(cbind(groupA_gene,groupB_gene))
      rownames(groupAB_gene)<-EXAMPLE_list_data()[,1]
      group_list <- factor(c(rep(1,length(colnames(groupA_gene))),rep(0,length(colnames(groupB_gene))))) 
      design <- model.matrix(~group_list-1)
      colnames(design)<-c("GA","GB")
      contrast_matrix <- makeContrasts("GA-GB", levels = design)
      fit <- lmFit(groupAB_gene, design)
      fitC <- contrasts.fit(fit, contrast_matrix)
      fitC <- eBayes(fitC)
      fitC
    }
    else if(input$EXAMPLE_method=="Pair DE analysis(Limma)"){
      pair_data <- EXAMPLE_find_pair()
      groupA_pair_name <- pair_data$Sample.Name.groupA 
      groupB_pair_name <- pair_data$Sample.Name.groupB
      groupA_final_name <- groupA_pair_name %>% gsub(" ",replacement=".", .) %>% gsub("-",replacement=".", .)
      groupB_final_name <- groupB_pair_name %>% gsub(" ",replacement=".", .) %>% gsub("-",replacement=".", .)
      groupAB_name <- c(groupA_final_name, groupB_final_name)
      pair_number<- as.integer(EXAMPLE_count_pair())
      groupAB_gene <- data.frame(cbind(groupA_gene, groupB_gene), check.names = FALSE)
      groupAB_gene <- groupAB_gene[, groupAB_name]
      rownames(groupAB_gene)<-EXAMPLE_list_data()[,1]
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

EXAMPLE_limma_output <- reactive({
  limma.output <- signif(topTable(EXAMPLE_fitC_dataset(), n=length(rownames(EXAMPLE_list_data()))), digits = 3)
  GroupA_means <- rowMeans(as.matrix(EXAMPLE_selected_groupA_genedata()))
  GroupB_means <- rowMeans(as.matrix(EXAMPLE_selected_groupB_genedata()))
  GroupAB_means <- signif(data.frame(GroupA_means, GroupB_means), digits = 3)
  genenames <- EXAMPLE_list_variable()
  rownames(GroupAB_means) <- rownames(genenames)
  limma.output <- cbind(genenames[rownames(limma.output), c("GeneSym")], GroupAB_means[rownames(limma.output),], limma.output[,-c(3,6)])
  colnames(limma.output)[1] <- "GeneSym"
  limma.output
})

output$EXAMPLE_select_limmaoutput <- DT::renderDataTable({
  if(length(input$EXAMPLE_select_groupA_rows_selected) > 0 && length(input$EXAMPLE_select_groupB_rows_selected) > 0)
    DT::datatable(EXAMPLE_limma_output(), extensions = "Buttons", options = list(dom = "lBftrip", buttons = list("colvis","csv","excel","pdf","print")),
                  selection = list(mode = 'single', selected = 1, target = 'row'), 
                  filter = 'top', rownames = TRUE, escape = FALSE)
})

output$EXAMPLE_download_result <- downloadHandler(
  filename = function(){"EXAMPLE_DE_Result.csv"},
  content = function(file){
    write.csv(EXAMPLE_limma_output(), file)
  }
)

EXAMPLE_selected_limmaoutput <- reactive({
  if (length(EXAMPLE_list_variable()) > 0){
    EXAMPLE_limma_output()[input$EXAMPLE_select_limmaoutput_rows_selected, , drop = FALSE]
  }
})
###################################################Plot#####################################################
#####Feature DATA VIEW####
output$EXAMPLE_select_data_bar <- renderPlot({
  if (length(EXAMPLE_selected_platform()) > 0){
    if (length(EXAMPLE_selected_sample()) > 0){
      if (length(EXAMPLE_selected_variable()) > 0){
        if (length(EXAMPLE_selected_data()) > 0){
          par(mar = c(13, 4, 4, 2))
          x <- EXAMPLE_selected_data()[input$EXAMPLE_select_sample_rows_selected]
          y <- EXAMPLE_list_sample()[input$EXAMPLE_select_sample_rows_selected, ]
          z <- EXAMPLE_list_variable()[input$EXAMPLE_select_variable_rows_selected, ]
          u <- y[, input$EXAMPLE_orderby]
          v <- y[, input$EXAMPLE_colorby]
          x <- x[order(u)]
          y <- y[order(u), ]
          v <- factor(v[order(u)])
          plot(x, type = 'h', lwd = 10, xlab = '', ylab = 'Value', axes = FALSE,
               col = rainbow(length(levels(v)))[factor(v)],
               main = paste0(z[1, -1],collapse = ', '))
          abline(h = seq(0, 20, by = 0.2), lty = 3, col = 'gray')
          axis(1, at = 1:length(EXAMPLE_selected_data()[input$EXAMPLE_select_sample_rows_selected]), 
               y[, 1], las = 2)
          axis(2, at = seq(0, 20, by = 0.2), seq(0, 20, by = 0.2), las = 2)
        }
      }
    }
  }
})

############### Waiter ####################
EXAMPLE_Vol_w <- call_waitress("#EXAMPLE_DEvolcanoplot", theme = "overlay-percent")
EXAMPLE_MD_w <- call_waitress("#EXAMPLE_DEplotMD", theme = "overlay-percent")
EXAMPLE_Barview_w <- call_waitress("#EXAMPLE_select_limmadata_bar", theme = "line")
EXAMPLE_Boxview_w <- call_waitress("#EXAMPLE_boxplotly", theme = "line")
#注意Waiter tag命名不可有"."否則影響Js的ID #tag
EXAMPLE_fitC_dataset <- eventReactive(input$EXAMPLE_draw, {
  EXAMPLE_Vol_w$start()
  EXAMPLE_Vol_w$auto(5, 150)
  EXAMPLE_MD_w$start()
  EXAMPLE_MD_w$auto(5, 150)
  EXAMPLE_Barview_w$start(h3(strong("Analysis Stuff....")))
  EXAMPLE_Barview_w$auto(5,200)
  EXAMPLE_Boxview_w$start(h3(strong("Creating Boxplot.....")))
  EXAMPLE_Boxview_w$auto(5,200)
  Sys.sleep(1)
  EXAMPLE_Vol_w$close()
  EXAMPLE_MD_w$close()
  EXAMPLE_Barview_w$close()
  EXAMPLE_Boxview_w$close()
  return(EXAMPLE_limma_fitC())
})

EXAMPLE_enrichGo_w <- call_waitress("#EXAMPLE_enrichGO_dotplot", theme = "overlay-percent")
EXAMPLE_enrichKEGG_w <- call_waitress("#EXAMPLE_enrichKEGG_dotplot", theme = "overlay-percent")
EXAMPLE_result_dataset <- eventReactive(input$EXAMPLE_start_enrichment, {
  EXAMPLE_enrichGo_w$start()
  EXAMPLE_enrichKEGG_w$start()
  EXAMPLE_enrichGo_w$auto(5,200)
  EXAMPLE_enrichKEGG_w$auto(5,200)
  Sys.sleep(1)
  EXAMPLE_enrichGo_w$close()
  EXAMPLE_enrichKEGG_w$close()
  return(EXAMPLE_limma_output())
})

##########COMPARISON DIAGRAM##############
output$EXAMPLE_DEvolcanoplot <- renderPlot({
  volcanoplot(EXAMPLE_fitC_dataset(), highlight = TRUE)
  abline(v=c(-1,-log2(1.5),log2(1.5),1), lty = c(1, 2, 2, 1))
  abline(h=c(-log10(0.05), -log10(0.01)), lty = c(1, 2))
})

output$EXAMPLE_DEplotMD <- renderPlot({
  plotMD(EXAMPLE_fitC_dataset(), status = decideTests(EXAMPLE_fitC_dataset(), p.value=0.05), 
         main = colnames(EXAMPLE_fitC_dataset()))
})

#########Comparsion Viewer ############
output$EXAMPLE_limmadatabar_nocall <- renderText({
  if (length(input$EXAMPLE_select_limmaoutput_rows_selected) < 1){
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
EXAMPLE_groupAB_data <- reactive({
  groupA_row_select <- EXAMPLE_selected_groupA()
  groupB_row_select <- EXAMPLE_selected_groupB()
  groupA_row_select[,"Group"] <- "GroupA"
  groupB_row_select[,"Group"] <- "GroupB"
  groupAB_row_select <- data.frame(rbind(groupA_row_select, groupB_row_select))
  return(groupAB_row_select)
})

###bar###
output$EXAMPLE_select_limmadata_bar <- renderPlot({
  if (length(EXAMPLE_selected_platform()) > 0){
    if (length(EXAMPLE_selected_sample()) > 0){
      if (length(EXAMPLE_selected_variable()) > 0){
        if (length(input$EXAMPLE_select_groupA_rows_selected) > 0 && length(input$EXAMPLE_select_groupB_rows_selected) > 0){
          if (length(input$EXAMPLE_select_limmaoutput_rows_selected) > 0){
            par(mar = c(13, 4, 4, 2))
            groupAB.name.selected <- c(colnames(EXAMPLE_selected_groupA_genedata()),colnames(EXAMPLE_selected_groupB_genedata()))
            x <- as.numeric(EXAMPLE_selected_limmadata()[,groupAB.name.selected])
            y <- EXAMPLE_groupAB_data()
            z <- EXAMPLE_limma_output()[input$EXAMPLE_select_limmaoutput_rows_selected, ]
            u <- y[, input$EXAMPLE_limma_orderby]
            v <- y[, input$EXAMPLE_limma_colorby]
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
output$EXAMPLE_boxplotly <- renderPlotly({
  if (length(input$EXAMPLE_select_groupA_rows_selected) > 0 && length(input$EXAMPLE_select_groupB_rows_selected) > 0){
    if (length(input$EXAMPLE_select_limmaoutput_rows_selected) > 0){
      groupAB.name.selected <- c(colnames(EXAMPLE_selected_groupA_genedata()),colnames(EXAMPLE_selected_groupB_genedata()))
      groupAB.row.select <- EXAMPLE_groupAB_data()
      gene.express <- as.numeric(EXAMPLE_selected_limmadata()[,groupAB.name.selected])
      groupAB.row.select[,"GeneExpress"] <- gene.express
      plot_ly(groupAB.row.select, y=~GeneExpress, color = ~Group, type = "box", boxpoints = "all", pointpos = -2, jitter = 3)
    }
  }
})

############### Enrichment Analysis ################
###enrichGo plot###
EXAMPLE_GO_data <- reactive({
  data <- EXAMPLE_result_dataset()
  filterdata <- subset(data, P.Value < input$EXAMPLE_GO_pvalue)
  bitrdata <- bitr(filterdata$GeneSym, fromType = "SYMBOL", toType = c("ENSEMBL", "ENTREZID"), OrgDb = "org.Hs.eg.db")
  input_ontology <- input$EXAMPLE_ontology_choice
  ego <- enrichGO(gene          = bitrdata$ENTREZID,
                  universe      = names(data(geneList, package = "DOSE")),
                  OrgDb         = org.Hs.eg.db,
                  ont           = input_ontology,
                  pAdjustMethod = "BH",
                  pvalueCutoff  = input$EXAMPLE_GO_cutoff,
                  qvalueCutoff  = input$EXAMPLE_GO_cutoff,
                  readable      = FALSE)
  return(ego)
})

output$EXAMPLE_enrichGO_dotplot <- renderPlot({
  ego <- EXAMPLE_GO_data()
if (input$EXAMPLE_GOplot_choice=="dotplot"){
    dotplot(ego, title = paste0("EnrichmentGO ",input$EXAMPLE_ontology_choice," Dotplot"))
  }else if(input$EXAMPLE_GOplot_choice=="barplot"){
    barplot(ego, showCategory=10,title=paste0("EnrichmentGO ",input$EXAMPLE_ontology_choice," Barplot"))
  }
})
###enrichKEGG plot###
EXAMPLE_KEGG_data <- reactive({
  data <- EXAMPLE_result_dataset()
  filterdata <- subset(data, P.Value < input$EXAMPLE_KEGG_pvalue)
  bitrdata <- bitr(filterdata$GeneSym, fromType = "SYMBOL", toType = c("ENSEMBL", "ENTREZID"), OrgDb = "org.Hs.eg.db")
  input_kegg <- input$EXAMPLE_KEGG_choice
  ego_k <- enrichKEGG(gene           = bitrdata$ENTREZID,
                      keyType        = input_kegg,
                      organism       = 'hsa',
                      pvalueCutoff   = input$EXAMPLE_KEGG_cutoff,
                      pAdjustMethod  = "BH",
                      qvalueCutoff   = input$EXAMPLE_KEGG_cutoff)
  return(ego_k)
})

output$EXAMPLE_enrichKEGG_dotplot <- renderPlot({
  ego_k <- EXAMPLE_KEGG_data()
  if(input$EXAMPLE_KEGGplot_choice=="dotplot"){
    dotplot(ego_k, title = paste0("Enrichment ","KEGG"," Dotplot"))
  }else if(input$EXAMPLE_KEGGplot_choice=="barplot"){
    barplot(ego_k, showCategory=10, title=paste0("EnrichmentKEGG ","KEGG"," Barplot"))
  }
})

