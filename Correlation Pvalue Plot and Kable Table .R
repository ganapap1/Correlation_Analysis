#################################################################################
#Loading required libraries
#################################################################################
library(shiny)
library(shinydashboard)  # for Dashboard
library(shinydashboardPlus)
library(shinyWidgets)    # for sendSweetAlert function
library(shinyBS)         # for bsTooltip function
library(shinyalert)      # for alert message very nice format
library(DT)              # for using %>% which works as a pipe in R code
library(shinyjs)         # for DT:: datatable output and render
library(dplyr)           # select functions are covered in the library it is used while selecting and deleting a row

library(Rmisc)           # to calculate confidence interval
library(kableExtra)      # to handle kbl tables and scroll_box function
library(Hmisc)           # Compute correlation matrix
library(tidyr)           # to use replace_na function
library(corrplot)        # rcorr function to calcuate correlation circle plot
library(RColorBrewer)    # coloring of correlation circle plot
library(xtable)          # nicely formatted tables Kable - HTML
library(DT)              # for using %>% which works as a pipe in R code
library(compare)         # comparing two dataframe columns and also isTRUE() is from compare package
library(psych)           # to get pair panel correlation plot

#By default, Shiny limits file uploads to 5MB per file. You can modify this limit by using the shiny.maxRequestSize option.
# where you got: https://shiny.rstudio.com/articles/upload.html#:~:text=By%20default%2C%20Shiny%20limits%20file%20uploads%20to%205MB,of%20app.R%20would%20increase%20the%20limit%20to%2030MB.
options(shiny.maxRequestSize = 30*1024^2)


#################################################################################
#Actionbutton style function 30px and width 100px
#################################################################################
styleButtonBlue<- function(){
  "white-space: normal;
                        text-align:center;
                        color: #ffffff; 
                        background-color:#4682B4;
                        border-color: #ffffff;
                        border-width:2px;
                        height:30px;
                        width:100px;
                        font-size: 13px;"
}




#################################################################################
#Actionbutton style function 50px and width 150px
#################################################################################
styleTallButtonBlue<- function(){
  "white-space: normal;
                        text-align:center;
                        color: #ffffff; 
                        background-color:#4682B4;
                        border-color: #ffffff;
                        border-width:2px;
                        height:50px;
                        width:150px;
                        font-size: 13px;"
}



###############################################################
##  Define URL variables that are using in the dashboard
###############################################################
urlcorre1 <- a(HTML(paste('<h6>',"1.Knowledge Bank from Mvorganizing.org: Correlation, Hypothesis & Significance",'<h6>')),
               href="https://www.mvorganizing.org/what-is-a-correlational-hypothesis/")

               
urlcorre2 <- a(HTML(paste('<h6>',"2.Statistics Online Support-SOS: Correlation and Linear Regression",'<h6>')),
                   href="http://sites.utexas.edu/sos/guided/inferential/numeric/bivariate/cor/")

urlcorre3 <- a(HTML(paste('<h6>',"3.Khan Academy: Correlation and Causation",'<h6>')),
               href="https://www.khanacademy.org/test-prep/praxis-math/praxis-math-lessons/gtp--praxis-math--lessons--statistics-and-probability/a/gtp--praxis-math--article--correlation-and-causation--lesson#:~:text=Correlation%20means%20there%20is%20a%20relationship%20or%20pattern,that%20one%20event%20causes%20another%20event%20to%20occur.")



###################################################################
## Transparent colors  where you got: https://www.dataanalytics.org.uk/make-transparent-colors-in-r/
## Mark Gardener 2015
## www.dataanalytics.org.uk
##################################################################

t_col <- function(color, percent = NULL, name = NULL) {
  #      color = color name
  #    percent = % transparency
  #       name = an optional name for the color
  
  ## Get RGB values for named color
  rgb.val <- col2rgb(color)
  
  ## Make new color using input color as base and alpha set by transparency
  t.col <- rgb(rgb.val[1], rgb.val[2], rgb.val[3],
               max = 255,
               alpha = (100 - percent) * 255 / 100,
               names = name)
  
  ## Save the color
  invisible(t.col)
}
## END


#we know only %in% but there is a custom function for NOT IN.  here is that
'%!in%' <- Negate('%in%')  #where you got #https://r-lang.com/not-in-r/


#################################################################################
# header which is part of ui 
#################################################################################
header <- shinydashboardPlus::dashboardHeader(
  title = "Correlation Analysis & Significance",
  titleWidth = '475px',
  tags$li(
    tags$head(tags$style(HTML('.navbar-custom-menu {width:500; float: left!important;}'))),  ## where you got: https://stackoverflow.com/questions/60861004/move-logo-to-the-left-side-of-dashboard-header-in-shiny-dashboard
    actionButton(inputId = "Previous", label = icon("arrow-left")),
    actionButton(inputId = 'mydropdown',label =  HTML(paste('<h4><b>',"Tab Menu",'</b>')),style=styleTallButtonBlue()),
    actionButton(inputId = "Next", label = icon("arrow-right")),
    class = "dropdown"
  )
)



#################################################################################
# controlbar and sidebar which is part of ui 
#################################################################################
sidebar <- shinydashboardPlus::dashboardSidebar(
  id = 'msidebarid',
  collapsed = TRUE,
  minified = FALSE,  #this option will completely close the side bar and expand the header text
  useShinyjs(),
  sidebarMenu(id = "tabs",
              menuItem('Overview',
                       tabName = 'taboverview',
                       icon = icon('line-chart')),
              menuItem('Dataset',
                       tabName = 'tabdataset',
                       icon = icon('line-chart')),
              menuItem('Correlation & Significance',
                       tabName = 'tabcorrelation',
                       icon = icon('line-chart')),
              menuItem('P-Value & Significance',
                       tabName = 'tabpvalue',
                       icon = icon('line-chart')),
              menuItem('Correlation Circle Plot',
                       tabName = 'tabcircleplot',
                       icon = icon('line-chart')),
              menuItem('Correlation Pair-Panel Plot',
                       tabName = 'tabpairpanelplot',
                       icon = icon('line-chart')),
              menuItem('CI for Correlation',
                       tabName = 'tabCIforCorr',
                       icon = icon('line-chart')),
              menuItem('Data Table Review',
                       tabName = 'tabdata',
                       icon = icon('line-chart'))
  )#sidebar menu
)



#################################################################################
# dashboard body and sidebar which is part of ui 
#################################################################################
body <- dashboardBody(
  shinyjs::useShinyjs(),
  useShinyalert(),
  setShadow(class = "dropdown-menu"),
  #Remove icon which displays or hides the left sidebar in shinydashboard for a certain tabPanel
  # Note: 'sidebar-toggle' is a class name (i.e. HTML class attribute) of Shiny icon 
  tags$script("document.getElementsByClassName('sidebar-toggle')[0].style.visibility = 'hidden';"),   #where you got: https://stackoverflow.com/questions/60895511/remove-icon-which-displays-or-hides-the-left-sidebar-in-shinydashboard-for-a-cer
  tabItems(
    tabItem(
      tabName ="taboverview",
      align = "center",
      box(
        id = "boxtaboverview",
        width = 12,
        height = 500,
        title = 'Happy Learning :: Correlation & Significance',
        status = "warning",
        solidHeader = TRUE,
        collapsible = FALSE,
        uiOutput(outputId = 'mTopicOverview')
      ) # box closure
      
    ),
    tabItem(
      tabName = "tabdataset",
      align='center',
      tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible;}"))),
      fluidRow(
        box(
          width = 6,
          height = "475px",
          align = "center",
          title = "Dataset & Column selection",
          status = "warning",
          solidHeader = TRUE,
          collapsible = FALSE,
          HTML(paste('<h5><b>','Dataset Upload & Cleansing','</b>')),
          br(),
          actionButton(inputId = "mFileImport", label = "Upload & Modify",style=styleTallButtonBlue()),
          tags$hr(),  # Horizontal line ----
          HTML(paste('<h5><b>','Select columns to split dataset','</b><h5>',
                     "As you select; columns will be moved to first dataset to calculate correlation between these two.",
                     "if you select all columns, correlation output matrix will have rows and columns with same fields")),
          selectInput(inputId = "mfirstdfcolumn",label = NULL,choices = NULL,selected = NULL,multiple = TRUE,width = '100%'),
          tags$hr(),  # Horizontal line ----
          HTML(paste('<h5><b>','Two Options','</b><h5>',
                     "Either to select all columns or select limited number of columns one by one")),
          br(),
          splitLayout(cellWidths = c("50%","50%"),
                      actionButton(inputId = 'mselectALLbtn',label = "Select ALL Columns",style=styleTallButtonBlue()),
                      actionButton(inputId = 'mselectOneByOnebtn',label = "Select Columns Individually",style=styleTallButtonBlue()),
                      bsTooltip("mselectALLbtn", paste("In this option, correlation will be calculated based on original dataset.",
                                                       "All elements / columns from original dataset will be as rows and columns",
                                                       "in Correlation output matrix"),
                                placement = "right", options = list(container = "body")),
                      bsTooltip("mselectOneByOnebtn", paste("Correlation matrix will be different in this option, when you select columns one by one,",
                                                            "the correlation will be calculated and reported between elements in first dataset and elements in second dataset"),
                                placement = "right", options = list(container = "body"))
                      
          )
        ), #box closure

        box(
          id = 'mdatastrBox', 
          width = 6,
          height = 475,
          align = "center",
          title = "Data structure selected",
          status = "warning",
          solidHeader = TRUE,
          collapsible = FALSE,
          column(width = 5,
                 HTML(paste('<b>',"Columns in First dataset",'</b>')),
                 verbatimTextOutput(outputId = "mfirstdfstructure")
          ),
          column(width = 2,
                 br(),br(),
                 actionButton(inputId = "justforarrow1",label = "",icon = icon("arrow-alt-circle-left"),width = 50),
                 br(),
                 actionButton(inputId = "justforarrow2",label = "",icon = icon("arrow-alt-circle-right"),width = 50)),
          column(width = 5,
                 HTML(paste('<b>',"Columns in Second dataset",'</b>')),
                 verbatimTextOutput(outputId = "mseconddfstructure")
          ),
          div(style = "height:363px;")  #box height formatting https://community.rstudio.com/t/controlling-the-height-of-fluidrow-in-shiny/4968
        ) #box closure
      ), #fluidrow closure
      fluidRow(
        box(
          width = 6,
          height = "450px",
          align = "center",
          title = "First Dataset",
          status = "warning",
          solidHeader = TRUE,
          collapsible = FALSE,
          DT::dataTableOutput('mtblfirst',height = "375px")
        ), #box closure
        box(
          width = 6,
          height = "450px",
          align = "center",
          title = "Second Dataset",
          status = "warning",
          solidHeader = TRUE,
          collapsible = FALSE,
          DT::dataTableOutput('mtblsecond',height = "375px")
        ) #box closure
      ) #fluidrow closure
    ),
    tabItem(
      tabName = "tabcorrelation",
      align = "center",
      HTML(paste('<h4><b>',"Correlation Matrix with Significance",'</b><h6>',
                 'Norms of Significance:',paste('if(p <= .001, ***) if(p <= .01, **) if(p <= .05, *), if(p <= .1, .,otherwise blank); ',
                                                '<br>',"Color code: blue represents significant (<= 0.05) and yellow represents NOT significant"),'<h5>')),
      htmlOutput('mcorrelationwithsignif')
    ),#tabitem
    tabItem(
      tabName = "tabpvalue",
      align = "center",
      HTML(paste('<h4><b>',"P-Value Matrix with Significance",'</b><h6>',
                 'Norms of Significance:',paste('if(p <= .001, ***) if(p <= .01, **) if(p <= .05, *), if(p <= .1, .,otherwise blank); ',
                                                '<br>',"Color code: blue represents significant (<= 0.05) and yellow represents NOT significant"),'<h5>')),
      htmlOutput('mcorrpvaluetext')
    ),
    tabItem(
      tabName = "tabcircleplot",
      align = "center",
      HTML(paste('<h4><b>',"Correlation Circle Plot",'</b><h6>',
                 'ALL Columns of Original dataset or only with selected columns','<h5>')),
      plotOutput('mcorrelationplotCircle',height = 450,width = '100%')
    ),
    tabItem(
      tabName = "tabpairpanelplot",
      align = "center",
      HTML(paste('<h4><b>',"Correlation Pair-Panel Plot",'</b><h6>',
                 'ALL Columns of Original dataset or only with selected columns','<h5>')),
      plotOutput('mcorrelationpairpanel',height = 475,width = '100%')
    ),
    tabItem(
      tabName = "tabCIforCorr",
      align = "center",
      
      column(
        width = 12,
        align = "center",
        fluidRow(
          box(
            width = 3,
            height = 500,
            title = "Variables",
            status = "warning",
            solidHeader = TRUE,
            collapsible = FALSE,
            x <- uiOutput('radio_Btns')
            #actionButton(inputId = 'mbtnRadiodelete',label = "Delete Selected Variable",style = styleButtonBlue())
          ),
          uiOutput(outputId = "mtableorplot")
          
        ) #fluid Row Closure
      ) #column closure
    ),
    tabItem(
      tabName = "tabdata",
      align = "center",
      HTML(paste('<h4><b>',"Original uploaded Dataset",'</b><h6>',
                 'Being Correlation Analysis, while uploading dataset, columns other numeric columns will excluded','<h5>')),
      box(
        width = 12,
        height = 500,
        align = "center",
        title = NULL,
        status = "warning",
        solidHeader = FALSE,
        collapsible = FALSE,
        DT::dataTableOutput(outputId = 'tbldataset',height = 400,width = '100%')
      ) #box closure
    )#tabitem
  )#tabitems
) # dashboardBody closure


ui <- dashboardPage(
  header = header,
  sidebar = sidebar,
  body = body
)



#################################################################################
# server function starts here 
#################################################################################

server <- function(input, output, session) {
  ##this is to hide right side bar
  shinyjs::addCssClass(selector = "body", class = "sidebar-collapse")
  # onevent("mouseenter", "sidebarCollapsed", shinyjs::removeCssClass(selector = "body", class = "sidebar-collapse"))
  # onevent("mouseleave", "sidebarCollapsed", shinyjs::addCssClass(selector = "body", class = "sidebar-collapse"))
  
  vmy <- reactiveValues(mydata=NULL,correlation_matrix=NULL,df_types=NULL)
  
  #################################################################################
  # File dataset upload code starts here
  #################################################################################
  
  observeEvent(input$mFileImport,{
    showModal(
      modalDialog(
        size = 'm',
        column(   
          width = 6,
          offset = 0,
          align = "center",
          fluidRow(
            box(
              width = 12,
              height = 425,
              align = "center",
              title = "Uplaod Dataset",
              status = "warning",
              solidHeader = TRUE,
              collapsible = FALSE,
              # Input: Select a file ----
              fileInput("file",
                        label = "Select: csv, xls, xlsx, rds ",
                        multiple = FALSE,
                        accept = c("text/csv/txt/Excel",
                                   "text/comma-separated-values,text/plain/excel",
                                   ".csv",".xls",".xlsx",".rds")),
              
              # Horizontal line ----
              #tags$hr(),
              column(
                width = 5,
                offset = 1,
                align = "left",
                fluidRow(
                  # Input: Checkbox if file has header ----
                  checkboxInput("header", "Header", TRUE),
                  
                  # Input: Select separator ----
                  radioButtons("sep", "Separator",
                               choices = c(Comma = ",",
                                           Semicolon = ";",
                                           Tab = "\t"),
                               selected = ",")
                )
              ),
              column(
                width = 5,
                offset = 0,
                align = "left",
                fluidRow(
                  br(),
                  br(),
                  # Input: Select quotes ----
                  radioButtons("quote", "Quote",
                               choices = c(None = "",
                                           "Double Qot." = '"',
                                           "Single Qot." = "'"),
                               selected = '"')
                )
              )
            ), #box closure
            HTML("click outside this box to exit")
          )# fluidRow closure
          
        ), #column
        column(
          width = 6,
          offset = 0,
          align = "center",
          fluidRow(
            box(
              width = 12,
              height = '250px',
              align = "center",
              title = "Modify Dataset",
              status = "warning",
              solidHeader = TRUE,
              collapsible = FALSE,
              DT::dataTableOutput("dt",height = 260),
              tags$style(HTML('table.dataTable tr.selected td{background-color: pink !important;}')),
              useShinyjs(),
              extendShinyjs(text = paste0("shinyjs.resetDTClick = function() { Shiny.onInputChange('dt_cell_clicked', null); }"),functions = c('foo','bar')),
              textOutput("mselectedvariable"),
              br(),
              actionButton(inputId = 'mbtndelete',label = "Delete Selected Variable",style = styleTallButtonBlue())
            ) #box closure
          )# fluidRow closure
        ), #column closure
        easyClose = TRUE
      )
    )
  })
  
  observeEvent(input$file,{
    ext <- tools::file_ext(input$file$name)
    # when reading semicolon separated files,
    # having a comma separator causes `read.csv` to error
    
    if (ext == "rds"){
      vmy$mydata <- as.data.frame(readRDS(input$file$datapath))  # got from https://readxl.tidyverse.org/
    }
    else if (ext == "xls" || ext == 'xlsx'){
      vmy$mydata <- as.data.frame(readxl::read_excel(input$file$datapath))  # got from https://readxl.tidyverse.org/
    }
    else if (ext == "csv"){
      tryCatch({            #avoid rscript showing error initially before execution,Skipping error in for-loop where you got:https://stackoverflow.com/questions/14748557/skipping-error-in-for-loop
        
        vmy$mydata <- as.data.frame(read.csv(input$file$datapath,
                                             header = input$header,
                                             sep = input$sep,
                                             quote = input$quote)
        )
      },error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))})
    }
    else{
      shinyalert("Oops!", "valid files are only csv, excel or rds", type = "error")
      return()
    }
    if ((ncol(dplyr::select_if(vmy$mydata,is.numeric))==0)==TRUE) {
      shinyalert(title ="Numeric columns missing..!" ,text = "There is no numeric columns in this dataset.  Select some other dataset")
      return()
    }
    if ((ncol(dplyr::select_if(vmy$mydata,is.numeric))==1)==TRUE) {
      shinyalert(title ="Single numeric column dataset..!" ,text = "There is only one numeric column in this dataset, we cannot calculate correlation.  select some other")
      return()
    }
    vmy$mydata <- dplyr::select_if(vmy$mydata,is.numeric)
    mnumericcolname <-  names(dplyr::select_if(vmy$mydata,is.numeric)) 
    for (i in mnumericcolname){
      vmy$mydata[i] <- round(vmy$mydata[i],2)
    }
    vmy$mydata <- na.omit(vmy$mydata)
    vmy$mydata <- vmy$mydata[complete.cases(vmy$mydata), ]
    row.names(vmy$mydata) <- 1:nrow(vmy$mydata)
    ttemp <- vmy$mydata
    vmy$originaldf <- ttemp
    fncreatedftype()
    fnRadioGrpBtn()
  }) 
  
 
  #################################################################################
  # File dataset MODIFY code starts here
  #################################################################################
  
  fncreatedftype <- function(){
    vmy$df_types <- data.frame("col_types" = unlist(lapply(vmy$originaldf, typeof)))
    vmy$df_types$Var_name <- rownames(vmy$df_types)
    row.names(vmy$df_types) <- NULL
    vmy$df_types <-vmy$df_types %>% dplyr::select(-col_types, everything())
  }
  
  
  output$dt <- DT::renderDataTable({
    DT::datatable(vmy$df_types,
                  class ='cell-border stripe compact white-space: nowrap', #where you got this multiple classes: https://rstudio.github.io/DT/
                  escape=FALSE,
                  rownames = FALSE,
                  width = NULL,
                  height = NULL,
                  editable = FALSE,
                  selection = list(mode = "single", target = 'row'),
                  fillContainer = getOption("DT.fillContainer", FALSE),
                  options = list(dom = 't',ordering=FALSE, pageLength = -1,class="compact",
                                 initComplete = JS(
                                   "function(settings, json) {",
                                   "$(this.api().table().header()).css({'background-color': '#808080', 'color': '#fff','font-size':'12px'});",
                                   "}")
                  ) 
    ) %>%  formatStyle(columns = colnames(.$x$data), `font-size` = '11px')   #where you got: https://stackoverflow.com/questions/44101055/changing-font-size-in-r-datatables-dt
    
  })
  
  
  output$mselectedvariable <-  renderText({
    if(length(input$dt_cell_clicked) != 0){
      clicked_list <- input$dt_cell_clicked
      HTML(paste("selected:",vmy$df_types[clicked_list$row,1],'\n',"Type:",vmy$df_types[clicked_list$row,2]))
      
    }
  })
  

  #################################################################################
  # File dataset MODIFY code ENDS here
  #################################################################################
  
  
  
  
  
  #################################################################################
  # File dataset DELETE VARIABLE code starts here
  #################################################################################
  
  ### delete selected column
  ### this is warning messge for deleting
  observeEvent(input$mbtndelete,{
    showModal(
      if(length(vmy$df_types[input$dt_cell_clicked$row,1])>0 ){
        modalDialog(
          title = "Warning",
          paste("Are you sure delete variable:",vmy$df_types[input$dt_cell_clicked$row,1] ),
          footer = tagList(
            modalButton("Cancel"),
            actionButton("ok", "Yes")
          ), easyClose = TRUE)
      }else{
        modalDialog(
          title = "Warning",
          paste("Please select the variable that you want to delete!" ),easyClose = TRUE
        )
      }
      
    )
  })
  
  
  ### If user say OK, then delete the selected rows
  observeEvent(input$ok, {
    if ((ncol(vmy$mydata)==0)==TRUE) {
      removeModal()
      shinyalert(title ="Numeric columns missing..!" ,text = "If you delete this column, there would not be any numeric columns, hence you cannot delete this column")
      return()
    }
    if ((ncol(vmy$mydata)==2)==TRUE) {
      removeModal()
      shinyalert(title ="Single numeric column dataset..!" ,text = "If you delete this column, there will be only one numeric column in this dataset and we cannot calculate correlation. Hence you cannot delete this column")
      return()
    }
    
    temp <- dplyr::select(vmy$mydata,-vmy$df_types[input$dt_cell_clicked$row,1])
    vmy$mydata <- temp
    
    temp <- dplyr::select(vmy$originaldf,-vmy$df_types[input$dt_cell_clicked$row,1])
    vmy$originaldf <- temp
    
    removeModal()
    fncreatedftype()
    fnRadioGrpBtn()
    updateSelectInput(session,inputId = 'mfirstdfcolumn',choices = colnames(vmy$originaldf))
    click(id = "mFileImport",asis = TRUE)
  })
  
  #################################################################################
  # File dataset DELETE VARIABLE code ENDS here
  #################################################################################

  
  
  
  #################################################################################
  # dividing dataset into two to construct correlation
  #################################################################################

  observeEvent(input$mselectALLbtn,{
    if (length(input$file)==0){
      shinyalert("Oops!", "Hi first browse and select dataset ...!", type = "error")
      return()
    }
    updateSelectInput(session,inputId = 'mfirstdfcolumn',choices = colnames(vmy$originaldf),selected = colnames(vmy$originaldf))
  })
  
  
  observeEvent(input$mselectOneByOnebtn,{
    if (length(input$file)==0){
      shinyalert("Oops!", "Hi first browse and select dataset ...!", type = "error")
      return()
    }
    updateSelectInput(session,inputId = 'mfirstdfcolumn',choices = colnames(vmy$originaldf),selected = NULL)
  })
  

  observeEvent(input$mfirstdfcolumn,{
    if (length(input$mfirstdfcolumn)== (ncol(vmy$mydata)-1)){
      shinyalert("Oops!", "You cannot move anymore columns.  if you want correlation between all columns Click Select ALL columns button", type = "error")
      fnCorrelationProcessing() 
      fnPvalueProcessing()
    }
    if (length(input$mfirstdfcolumn)>=1){
      output$mfirstdfstructure <- renderPrint({
        HTML(cat(colnames(vmy$mfirstdf()),sep="\n"))
      })
      output$mseconddfstructure <- renderPrint({
        HTML(cat(colnames(vmy$mseconddf()),sep="\n"))
      })
      
      aaa <- vmy$mydata[colnames(vmy$mfirstdf())]
      bbb <- vmy$mydata[colnames(vmy$mseconddf())]
      if (isTRUE(dplyr::all_equal(vmy$mfirstdf(),vmy$mydata))){
        temp <- data.frame(aaa)        
      }
      else{
        temp <- data.frame(aaa,bbb)        
      }
      vmy$mydata <- temp
      fnCorrelationProcessing()
      fnPvalueProcessing()
    }
  })
  
  
  vmy$mfirstdf <-reactive({
    tryCatch({ 
      moptionsTXT <-  input$mfirstdfcolumn 
      mfirstdf <- vmy$mydata[moptionsTXT]
     }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
    return(mfirstdf)
  })
  
  
  vmy$mseconddf <-reactive({
    '%!in%' <- Negate('%in%')  #where you got #https://r-lang.com/not-in-r/
    tryCatch({ 
      if (isTRUE(dplyr::all_equal(vmy$mfirstdf(),vmy$mydata))){
        mseconddf <- vmy$mydata
      }
      else{
        mseconddf <- vmy$mydata
        ccdd <-  input$mfirstdfcolumn
        moptionsTXT <- names(vmy$mydata)[which(names(vmy$mydata) %!in% ccdd)]
        mseconddf <- vmy$mydata[moptionsTXT]
      }
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
    return(mseconddf)
  })
  
  
  
  output$tbldataset <- DT::renderDataTable({
    if (length(input$file)==0){
      shinyalert("Oops!", "Hi first browse and select dataset ...!", type = "error")
      return()
    }
    DT::datatable(vmy$mydata,
                  class ='cell-border stripe compact white-space: nowrap', 
                  escape= FALSE,
                  rownames = FALSE,
                  editable = FALSE,
                  selection = list(mode = "single", selected = c(1), target = 'row',color='pink'),
                  fillContainer = getOption("DT.fillContainer", FALSE),
                  options = list(
                    lengthMenu = list(c(15, 25, 50,-1), c('15', '25','50' ,'All')),
                    paging = TRUE,
                    lenthChange=TRUE,
                    searching = FALSE,
                    fixedColumns = FALSE,
                    autoWidth = TRUE,
                    ordering = FALSE,
                    class="compact",
                    initComplete = JS(
                      "function(settings, json) {",
                      "$(this.api().table().header()).css({'background-color': '#437ead', 'color': '#fff','font-size':'11px'});",  #where you got: https://stackoverflow.com/questions/44101055/changing-font-size-in-r-datatables-dt
                      "$(this.api().table().body()).css({'background-color': '#808080', 'color': '#fff','font-size':'11px'});",
                      "$(this.api().table().footer()).css({'background-color': '#808080', 'color': '#fff','font-size':'11px'});",
                      "}")
                  )
    )%>%my.styleallrows() 
  })
  
  my.styleallrows <- function(.) formatStyle(., columns=0, target= 'row',color = 'black', 
                                             backgroundColor = '#ffffed',font ='50%',
                                             fontWeight ='normal',lineHeight='80%')
  my.styleonecolumn <- function(.) formatStyle(., columns=c("var_name"), target= 'cell',color = 'black',
                                               backgroundColor = '#ffffed',
                                               fontWeight ='bold',lineHeight='70%')
  

  #######- above multi table datatable end
  

  output$mtblfirst <- DT::renderDataTable({
    if (length(vmy$mfirstdf())==0){
      return()
    }
    DT::datatable(vmy$mfirstdf(),
                  class ='cell-border stripe compact white-space: nowrap',
                  escape= FALSE,
                  rownames = FALSE,
                  editable = FALSE,
                  selection = list(mode = "single", selected = c(1), target = 'row',color='pink'),
                  fillContainer = getOption("DT.fillContainer", FALSE),
                  options = list(
                    lengthMenu = list(c(15, 25, 50,-1), c('15', '25','50' ,'All')),
                    paging = TRUE,
                    lenthChange=TRUE,
                    searching = FALSE,
                    fixedColumns = FALSE,
                    autoWidth = TRUE,
                    ordering = FALSE,
                    class="compact",
                    initComplete = JS(
                      "function(settings, json) {",
                      "$(this.api().table().header()).css({'background-color': '#808080', 'color': '#fff','font-size':'11px'});",  #where you got: https://stackoverflow.com/questions/44101055/changing-font-size-in-r-datatables-dt
                      "$(this.api().table().body()).css({'background-color': '#808080', 'color': '#fff','font-size':'11px'});",
                      "$(this.api().table().footer()).css({'background-color': '#808080', 'color': '#fff','font-size':'11px'});",
                      "}")
                  )
    )%>%my.styleallrows()
  })
  
  
  
  output$mtblsecond <- DT::renderDataTable({
    if (length(input$file)==0 | is.null(input$mfirstdfcolumn)==TRUE){
      return()
    }
    
    DT::datatable(vmy$mseconddf(),
                  class ='cell-border stripe compact white-space: nowrap', 
                  escape= FALSE,
                  rownames = FALSE,
                  editable = FALSE,
                  selection = list(mode = "single", selected = c(1), target = 'row',color='pink'),
                  fillContainer = getOption("DT.fillContainer", FALSE),
                  options = list(
                    lengthMenu = list(c(15, 25, 50,-1), c('15', '25','50' ,'All')),
                    paging = TRUE,
                    lenthChange=TRUE,
                    searching = FALSE,
                    fixedColumns = FALSE,
                    autoWidth = TRUE,
                    ordering = FALSE,
                    class="compact",
                    initComplete = JS(
                      "function(settings, json) {",
                      "$(this.api().table().header()).css({'background-color': '#808080', 'color': '#fff','font-size':'11px'});",  #where you got: https://stackoverflow.com/questions/44101055/changing-font-size-in-r-datatables-dt
                      "$(this.api().table().body()).css({'background-color': '#808080', 'color': '#fff','font-size':'11px'});",
                      "$(this.api().table().footer()).css({'background-color': '#808080', 'color': '#fff','font-size':'11px'});",
                      "}")
                  )
    )%>%my.styleallrows() 
  })
  
  
  
  ############# organising data for correlation table START here  
  fnCorrelationProcessing <- function(){
    library(xtable)
    yyCorreSign <-fncorstars(vmy$mydata)
    
    yyCorreP <- round(data.frame(vmy$correlation_matrix$P),3)  # get p-value 
    library(tidyr) # to use replace_na function
    # yyCorreP <-yyCorreP %>%
    #   mutate_all(~replace_na(., 1))
    
    mtransparentcol <- t_col("pink", perc = 100, name = "lt.pink")
    n<- ncol(yyCorreSign)
    tt <- names(yyCorreSign)
    for (c in tt){
      for (nn in 1:nrow(yyCorreSign)){
        yyCorreSign[nn,c]<- HTML(cell_spec(
          yyCorreSign[nn,c],format = "html", 
          color = ifelse(yyCorreSign[nn,c]=="",mtransparentcol,'black'), 
          align = "c", 
          angle = 360,
          background = 
            if (yyCorreSign[nn,c]==""){
              mtransparentcol
            }
          else if  (yyCorreP[nn,c] == 1){
            mtransparentcol
          }
          else if (yyCorreP[nn,c] <= 0.05){
            "#C8E1FE" #blue
          }
          else {
            "#FFDF7F" #amber
          }
        )
        )
      }
    }

     yyCorreSign <- yyCorreSign %>% dplyr::filter(rownames(yyCorreSign) %in% colnames(vmy$mseconddf()))
     yyCorreSign <- yyCorreSign[input$mfirstdfcolumn]

    n<-ncol(yyCorreSign)
    output$mcorrelationwithsignif <- renderUI({
      HTML( kbl(yyCorreSign, escape = FALSE,align=c(rep('c',times=n)),   #align center all columns including header in table align=c(rep('c',times=n)) https://stackoverflow.com/questions/41365502/aligning-columns-with-knitr-kable-function
                caption =NULL,linesep = "\\addlinespace",
                table.attr = "style='width:30%;'")%>%
              kable_styling(font_size = 16, position = "center", html_font = "Cambria",fixed_thead = TRUE) %>%
              kable_paper("striped", full_width = FALSE) %>%
              column_spec(1, color = "red",background = '#D3D3D3')%>%  #for formatting first column in table
              row_spec(0:nrow(yyCorreSign), angle = 360,bold=TRUE, color = "red",background = '#D3D3D3',font_size = 16)%>%  #for formatting table header
              column_spec(1, bold = FALSE,width = "1.3in",color='red') %>%
              column_spec((1:n+1), bold = FALSE,border_left = TRUE,border_right = TRUE,width = "1.3in",color='white') %>%
              scroll_box(width = "100%", height = "475px")
      )
      
    })
    
    fnConfInterval()
  }
  
  
  ############# organising data for correlation table END here
  
  
  
  
  
  fnPvalueProcessing <- function(){
    library(xtable)
    yyCorreSign <-fnpvaluestars(vmy$mydata)
    
    ############# organising data for correlation table START here
    
    yyCorreP <- round(data.frame(vmy$correlation_matrix$P),3)  # get p-value o
    # all columns: update NA in entire dataframe with 1
    library(tidyr) # to use replace_na function
    # yyCorreP <-yyCorreP %>%
    #   mutate_all(~replace_na(., 1))
    
    mtransparentcol <- t_col("pink", perc = 100, name = "lt.pink")
    n<- ncol(yyCorreSign)
    tt <- names(yyCorreSign)
    for (c in tt){
      for (nn in 1:nrow(yyCorreSign)){
        yyCorreSign[nn,c]<- HTML(cell_spec(
          yyCorreSign[nn,c],format = "html", 
          color = ifelse(yyCorreSign[nn,c]=="",mtransparentcol,'black'), 
          align = "c", 
          angle = 360,
          background = 
            if (yyCorreSign[nn,c]==""){
              mtransparentcol
            }
          else if  (yyCorreP[nn,c] == 1){
            mtransparentcol
          }
          else if (yyCorreP[nn,c] <= 0.05){
            "#C8E1FE" #blue
          }
          else {
            "#FFDF7F" #amber
          }
        )
        )
      }
    }
    
    yyCorreSign <- yyCorreSign %>% dplyr::filter(rownames(yyCorreSign) %in% colnames(vmy$mseconddf()))
    yyCorreSign <- yyCorreSign[input$mfirstdfcolumn]
    
    n<-ncol(yyCorreSign)
    output$mcorrpvaluetext <- renderUI({
      HTML( kbl(yyCorreSign, escape = FALSE,align=c(rep('c',times=n)),   #align center all columns including header in table align=c(rep('c',times=n)) https://stackoverflow.com/questions/41365502/aligning-columns-with-knitr-kable-function
                caption =NULL,linesep = "\\addlinespace",
                table.attr = "style='width:30%;'")%>%
              kable_styling(font_size = 16, position = "center", html_font = "Cambria",fixed_thead = TRUE) %>%
              kable_paper("striped", full_width = FALSE) %>%
              column_spec(1, color = "red",background = '#D3D3D3')%>%  #for formatting first column in table
              row_spec(0:nrow(yyCorreSign), angle = 360,bold=TRUE, color = "red",background = '#D3D3D3',font_size = 16)%>%  #for formatting table header
              column_spec(1, bold = FALSE,width = "1.3in",color='red') %>%
              column_spec((1:n+1), bold = FALSE,border_left = TRUE,border_right = TRUE,width = "1.3in",color='white') %>%
              scroll_box(width = "100%", height = "475px")
      )
      
    })
    

  }

    ############# organising data for P Value table END here 
  
  
    output$mTopicOverview <- renderUI({
    fluidRow(
      column(width = 6,
             align= 'justify',
             tags$div(
               tags$p(
                 useShinyjs(),
                 HTML(paste('<h5><b>',"Correlation and Linear Regression:",'</b><br><h6>')),
                 HTML(paste('<h5>',
                            "Correlation and linear regression are the most commonly used techniques for investigating the", 
                            "relationship between two or more continuous variables. In correlation we are looking for a", 
                            "linear association between two variables, and the strength of the association is summarized by", 
                            "the correlation coefficient. A correlation analysis provides information on the strength and", 
                            "direction of the linear relationship between two variables, while a simple linear regression", 
                            "analysis estimates parameters in a linear equation that can be used to predict values of", 
                            "one variable based on the other. ")),
                 br(),br(),
                 HTML(paste('<h5><b>',"Correlation - Positive / Negative / Zero:",'</b><br><h6>')),
                 HTML(paste('<h5>',
                            "The Pearson correlation coefficient, r, can take on values between -1 and 1. The further away r is", 
                            "from zero, the stronger the linear relationship between the two variables. The sign of r corresponds to", 
                            "the direction of the relationship. If r is positive, then as one variable increases, the other tends to increase.", 
                            "If r is negative, then as one variable increases, the other tends to decrease. A perfect linear", 
                            "relationship will have r = -1 or r = 1; if r is zero, there is no linear relationship between the variables.")),
                 br(),br(),
                 HTML(paste('<h5><b>',"Correlation Study - Hypothesis:",'</b><br><h5>')),
                 HTML(paste('<h5>',
                            "In Correlation study, the NULL HYPOTHESIS is that there is NO RELATIONSHIP between the two", 
                            "measures in question. A p-value of 0.05 or lower is often considered to be statistically significant and we reject",
                            "null hypothesis and say there is a linear relationship between the variables under the study.",  
                            "If p-value is more than 0.05, we fail to reject Null hypothesis." ))
                 
               ))),
      column(
        width = 6,
        align='justify',

        HTML(paste('<h5><b>',"Correlation and Causation:",'</b><br><h5>')),
        HTML(paste('<h5>',
                   "Correlation means there is a relationship or pattern between the values of two variables.   Causation", 
                   "means that one event causes another event to occur. Correlation does not talk about causation, eg strong", 
                   "correlation or relationship could be coincidental, or a third factor may be causing both variables to change." )),
        br(),br(),
        HTML(paste('<h5><b>',"References: Thanks to",'</b><h5>')),
        HTML(paste(urlcorre1,urlcorre2,urlcorre3)),
        br(),br(),
        HTML(paste('<h5><b>',"About me:",'</b><br><h5>')),
        HTML(paste0('<h5>',"I am a Chartered Accountant having 25+ years of experience in Finance & Accounting.",
                    " The Data visualization and Data Science are always at the back of my mind.",
                    " I am a 'Tableau Desktop Certified Associate and working in 'R' with specific reference to Shiny App.",'<br>',
                    " In the process of sharing knowledge; ", 
                    "I have a channel in YouTube on R Shiny App.  Copy the link and paste in browser to view", '<br>',
                    'https://www.youtube.com/channel/UCDmEAmoLuyE0h61aGpthGvA/videos',br(),
                    "Happy Learning"
        ))
             
      )
    )
  })
  

  # x is a matrix containing the data
  # method : correlation method. "pearson"" or "spearman"" is supported
  # removeTriangle : remove upper or lower triangle
  # results :  if "html" or "latex"
  # the results will be displayed in html or latex format
  fncorstars <-function(x, method=c("pearson", "spearman"), removeTriangle=c("upper", "lower"),
                        result=c("none", "html", "latex")){
    #Compute correlation matrix
    x <- as.matrix(x)
    vmy$correlation_matrix<-rcorr(x, type=method[1])
    
    R <- vmy$correlation_matrix$r # Matrix of correlation coeficients
    p <- vmy$correlation_matrix$P # Matrix of p-value 

    ## Define notions for significance levels; spacing is important.
    mystars <- ifelse(p <= .001, "***", ifelse(p <= .01, "** ", ifelse(p <= .05, "*  ", ifelse(p <= .1, ".  ", "    "))))
    
    ## trunctuate the correlation matrix to two decimal
    R <- format(round(cbind(rep(-1.11, ncol(x)), R), 3))[,-1]

    ## build a new matrix that includes the correlations with their apropriate stars
    Rnew <- matrix(paste(R, mystars, sep=""), ncol=ncol(x))
    diag(Rnew) <- paste(diag(R), " ", sep="")
    rownames(Rnew) <- colnames(x)
    colnames(Rnew) <- paste(colnames(x), "", sep="")

    ## remove upper triangle of correlation matrix
    if(removeTriangle[1]=="upper"){
      Rnew <- as.matrix(Rnew)
      Rnew[upper.tri(Rnew, diag = TRUE)] <- ""
      Rnew <- as.data.frame(Rnew)
    }

    ## remove lower triangle of correlation matrix
    else if(removeTriangle[1]=="lower"){
      Rnew <- as.matrix(Rnew)
      Rnew[lower.tri(Rnew, diag = TRUE)] <- ""
      Rnew <- as.data.frame(Rnew)
    }


    ## remove last column and return the correlation matrix
    # Rnew <- cbind(Rnew[1:length(Rnew)-1]) ## GP removed to tally rows and columns same length
    if (result[1]=="none") return(Rnew)
    else{
      if(result[1]=="html") print(xtable(Rnew), type="html")
      else print(xtable(Rnew), type="latex")
    }
  }
  
  
  
  fnpvaluestars <-function(x, method=c("pearson", "spearman"), removeTriangle=c("upper", "lower"),
                        result=c("none", "html", "latex")){
    #Compute correlation matrix
    x <- as.matrix(x)
    vmy$correlation_matrix<-rcorr(x, type=method[1])
    
    R <- vmy$correlation_matrix$r # Matrix of correlation coeficients
    p <- vmy$correlation_matrix$P # Matrix of p-value 
    
    
    ## Define notions for significance levels; spacing is important.
    mystars <- ifelse(p <= .001, "***", ifelse(p <= .01, "** ", ifelse(p <= .05, "*  ", ifelse(p <= .1, ".  ", "    "))))
    
  
    ## trunctuate the correlation matrix to two decimal
    p <- format(round(cbind(rep(-1.11, ncol(x)), p), 3))[,-1]

    ## build a new matrix that includes the correlations with their apropriate stars
    Rnew <- matrix(paste(p, mystars, sep=""), ncol=ncol(x))
    diag(Rnew) <- paste(diag(p), " ", sep="")
    rownames(Rnew) <- colnames(x)
    colnames(Rnew) <- paste(colnames(x), "", sep="")
    
    ## remove upper triangle of correlation matrix
    if(removeTriangle[1]=="upper"){
      Rnew <- as.matrix(Rnew)
      Rnew[upper.tri(Rnew, diag = TRUE)] <- ""
      Rnew <- as.data.frame(Rnew)
    }
    
    ## remove lower triangle of correlation matrix
    else if(removeTriangle[1]=="lower"){
      Rnew <- as.matrix(Rnew)
      Rnew[lower.tri(Rnew, diag = TRUE)] <- ""
      Rnew <- as.data.frame(Rnew)
    }
    
    ## remove last column and return the correlation matrix
    # Rnew <- cbind(Rnew[1:length(Rnew)-1]) ## GP removed to tally rows and columns same length
    if (result[1]=="none") return(Rnew)
    else{
      if(result[1]=="html") print(xtable(Rnew), type="html")
      else print(xtable(Rnew), type="latex")
    }
  }
  
  
  
  output$mcorrelationplotCircle <- renderPlot({
    if (length(input$file)==0){
      shinyalert("Oops!", "Hi first browse and select dataset ...!", type = "error")
      return()
    }
    if (length(input$mfirstdfcolumn)==0){
      shinyalert("Oops!", "Please select column(s) in Dataset screen ...!", type = "error")
      return()
    }
    library(corrplot)
    library(RColorBrewer)
    M <-cor(vmy$mfirstdf())
    corrplot(M, type="upper", order="hclust",
             col=brewer.pal(n=8, name="RdYlBu"))
  })
  
  
  
   output$mcorrelationpairpanel <- renderPlot({
     if (length(input$file)==0){
       shinyalert("Oops!", "Hi first browse and select dataset ...!", type = "error")
       return()
     }
     if (length(input$mfirstdfcolumn)==0){
       shinyalert("Oops!", "Please select column(s) in Dataset screen ...!", type = "error")
       return()
     }

   library(psych)
   pairs.panels(vmy$mfirstdf(),
                smooth = TRUE,      # If TRUE, draws loess smooths
                scale = FALSE,      # If TRUE, scales the correlation text font
                density = FALSE,    # If TRUE, adds density plots and histograms
                ellipses = TRUE,    # If TRUE, draws ellipses
                method = "pearson", # Correlation method (also "spearman" or "kendall")
                pch = 21,           # pch symbol
                lm = FALSE,         # If TRUE, plots linear fit rather than the LOESS (smoothed) fit
                cor = TRUE,         # If TRUE, reports correlations
                jiggle = FALSE,     # If TRUE, data points are jittered
                factor = 2,         # Jittering factor
                smoother = TRUE,    # If TRUE, then smooth.scatter the data points -- slow but pretty with lots of subjects
                digits = 2,         # the number of digits to show
                cex.cor = 0.8,       # If this is specified, this will change the size of the text in the correlations. 
                hist.col = 4,       # Histograms color
                stars = TRUE)       # If TRUE, adds significance level with stars
 })


   
observeEvent(input$moverviewBTN,{
  updateTabsetPanel(session = session, inputId = "tabs",selected = "taboverview")
  removeModal()
})
observeEvent(input$mdatatabBTN,{
  updateTabsetPanel(session = session, inputId = "tabs",selected = "tabdataset")
  if (length(input$file)==0){
    click(id = "mFileImport",asis = TRUE)
  }
  removeModal()
})
observeEvent(input$mcorrelationBTN,{
  updateTabsetPanel(session = session, inputId = "tabs",selected = "tabcorrelation")
  removeModal()
})
observeEvent(input$mpvalueBTN,{
  updateTabsetPanel(session = session, inputId = "tabs",selected = "tabpvalue")
  removeModal()
})
observeEvent(input$mCorrCircleBTN,{
  updateTabsetPanel(session = session, inputId = "tabs",selected = "tabcircleplot")
  removeModal()
})
observeEvent(input$mCorrPairBTN,{
  updateTabsetPanel(session = session, inputId = "tabs",selected = "tabpairpanelplot")
  removeModal()
})
observeEvent(input$mDatasetBTN,{
  updateTabsetPanel(session = session, inputId = "tabs",selected = "tabdata")
  removeModal()
})
observeEvent(input$mCIforCorrBTN,{
  updateTabsetPanel(session = session, inputId = "tabs",selected = "tabCIforCorr")
  removeModal()
})


observeEvent(input$mydropdown,{
  showModal(
    modalDialog(
      size = 's',
      column(   
        width = 12,
        HTML(paste('<h4><b>',"Tab Menu",'</b>')),
        align = "center",
        fluidRow(
          HTML(paste('<h4>',actionLink(inputId = 'moverviewBTN',label = "Overview: Correlation"))),
          HTML(paste('<h4>',actionLink(inputId = 'mdatatabBTN',label = "Data Processing"))),
          
          HTML(paste('<h4>',actionLink(inputId = 'mcorrelationBTN',label = "Correlation Table"))),
          HTML(paste('<h4>',actionLink(inputId = 'mpvalueBTN',label = "P-value Table"))),
          
          HTML(paste('<h4>',actionLink(inputId = 'mCorrCircleBTN',label = "Correlation Circle Plot"))),
          HTML(paste('<h4>',actionLink(inputId = 'mCorrPairBTN',label = "Correlation Pair Plot"))),
          
          HTML(paste('<h4>',actionLink(inputId = 'mCIforCorrBTN',label = "CI for Correlation Plot"))),
          HTML(paste('<h4>',actionLink(inputId = 'mDatasetBTN',label = "View Dataset Table")))
          
        )# fluidRow closure
        
      ), #column
      easyClose = TRUE
    )
  )
})


fnConfInterval <- function(){
# Confidence Interval for a Correlation Coefficient Calculator
# A confidence interval for a correlation coefficient is a range of values that is likely to contain a population correlation coefficient with a certain level of confidence.

mrrr <- vmy$correlation_matrix$r # Matrix of correlation coeficients
mppp <- vmy$correlation_matrix$P # Matrix of p-value 

library(psychometric) # to calcualte confidence interval load package with function  #https://www.r-bloggers.com/2010/11/how-to-calculate-confidence-intervals-of-correlations-with-r/

# mrrr <- as.matrix(mrrr)
# mrrr[upper.tri(mrrr, diag = TRUE)] <- ""
# mrrr <- as.data.frame(mrrr)


vmy$ci_matrix <- data.frame(V1 = 'text',
                  V2 = 'text',
                  Corr = 0,
                  n    = 0,
                  #pval = 0,
                  CI   = 0,
                  LL  = 0,
                  UL  = 0)[-1,]


for (c in colnames(mrrr)){
  for (i in rownames(mrrr)){
    if (mrrr[i,c] != ""){
      vmy$ci_matrix <- vmy$ci_matrix%>%add_row(
        V1 = c,
        V2 = i,
        Corr = as.numeric(mrrr[i,c]),
        n    = nrow(vmy$mydata),
        #pval = as.numeric(mppp[i,c]),
        LL  =  CIr(r=as.numeric(mrrr[i,c]), n = nrow(vmy$mydata), level = .95)[1],
        UL  =  CIr(r=as.numeric(mrrr[i,c]), n = nrow(vmy$mydata), level = .95)[2],
        CI  =  ( CIr(r=as.numeric(mrrr[i,c]), n = nrow(vmy$mydata), level = .95)[2]-
                   CIr(r=as.numeric(mrrr[i,c]), n = nrow(vmy$mydata), level = .95)[1]  )/2
      )
    }
  }
}

library("tibble")   # this as_data_frame function is from tibble package
dtemp <- as_data_frame(vmy$ci_matrix)
vmy$ci_matrix <- dtemp%>%dplyr::filter(V1!=V2)


}


fnRadioGrpBtn <- function(){
  output$radio_Btns <- renderUI({
    # options <- colnames(vmy$mydata) # The options are dynamically generated on the server
    options <- unique(vmy$ci_matrix$V1) # The options are dynamically generated on the server
  
    radioGroupButtons(
      inputId = "radioreply",
      label = "Select variable",
      choices = options,
      individual = TRUE,
      size = "sm", #size options are "xl", "sm","normal","lg"
      checkIcon = list(
        yes = tags$i(class = "fa fa-circle",
                     style = "color: red"),
        no = tags$i(class = "fa fa-circle-o",
                    style = "color: steelblue"))
    )# radioGroupbtn closure
  }) # renderUI closure
} #function closure



  output$mtableorplot <- renderUI({
    box(
      width = 9,
      height = 500,
      title = textOutput("mplottitle"),
      status = "warning",
      solidHeader = TRUE,
      collapsible = FALSE,
      
      plotOutput('mmultiplot', height = 450)
    ) #box closure
  })    


output$mmultiplot <-renderPlot({
  par(mfrow = c(1, 1))
  
  library(metan)
  library(dplyr)
  tryCatch({ 
  vmy$ci_matrix%>%dplyr::filter(V1 == input$radioreply)%>%
    plot_ci()
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
})





### delete selected rows part
### this is warning messge for deleting
observeEvent(input$mbtnRadiodelete,{
  showModal(
    if(length(input$radioreply)>=1 ){
      modalDialog(
        title = "Warning",
        paste("Are you sure delete variable:",input$radioreply ),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("mRadiok", "Yes")
        ), easyClose = TRUE)
    }else{
      modalDialog(
        title = "Warning",
        paste("Please select the variable that you want to delete!" ),easyClose = TRUE
      )
    }
    
  )
})


### If user say OK, then delete the selected rows
observeEvent(input$mRadiok, {
  #temp <- select(vmy$ci_matrix,-c(input$radioreply))
  temp <- vmy$ci_matrix%>%dplyr::filter(V2 != input$radioreply)
  vmy$ci_matrix <- temp
  removeModal()
  
  options <- unique(vmy$ci_matrix$V1) # The options are dynamically generated on the server
  updateRadioGroupButtons(session,
                          inputId = "radioreply",
                          choices = options,
                          size = "sm", #size options are "xl", "sm","normal","lg"
                          checkIcon = list(
                            yes = tags$i(class = "fa fa-circle",
                                         style = "color: red"),
                            no = tags$i(class = "fa fa-circle-o",
                                        style = "color: steelblue"))
  )
})


# here we fix the Title for each plot
output$mplottitle <- renderText({
  paste("CI plot between",input$radioreply,"and ALL other variables")
})


###################### right left arrow for the next and previous tab  where you got: https://stackoverflow.com/questions/44309328/generic-button-for-go-to-next-and-previous-tabitem-shiny

global <- reactiveValues(tab_id = "")
tab_id <- c('taboverview','tabdataset','tabcorrelation','tabpvalue',
            'tabcircleplot', 'tabpairpanelplot','tabCIforCorr','tabdata')
Current <- reactiveValues(
  Tab = "taboverview"
)


observeEvent(
  input[["tabs"]],
  {
    Current$Tab <- input[["tabs"]]
  }
)

observeEvent(
  input[["Previous"]],
  {
    tab_id_position <- match(Current$Tab, tab_id) - 1
    if (isTRUE(tab_id_position == 0)) tab_id_position <- length(tab_id)
    Current$Tab <- tab_id[tab_id_position]
    updateTabItems(session, "tabs", tab_id[tab_id_position]) 
  }
)

observeEvent(
  input[["Next"]],
  {
    tab_id_position <- match(Current$Tab, tab_id) + 1
    if (isTRUE(tab_id_position > length(tab_id))) tab_id_position <- 1
    Current$Tab <- tab_id[tab_id_position]
    updateTabItems(session, "tabs", tab_id[tab_id_position]) 
  }
)

###### end of code for go next and previous





} #server closure
shinyApp(ui, server)

