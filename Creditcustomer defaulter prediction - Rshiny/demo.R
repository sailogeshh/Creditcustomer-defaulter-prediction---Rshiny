library(shiny)
library(shinydashboard)
library(shinythemes)
library(shinyanimate)
library(shinyBS)
library(shinyjs)
library(plotly)
library(readxl)
library(shinyWidgets)
library(DT)
library(magrittr)
library(anomalize) #tidy anomaly detectiom
library(tidyverse) #tidyverse packages like dplyr, ggplot, tidyr
library(coindeskr) #bitcoin price extraction from coindesk
library(ggthemes) 
library(ggpubr)
library(ggplot2)
library(forecast)
library(tseries)
library(shinyBS)
library(shiny)
library(shinydashboard)
library(randomForest)
library(DT)
set.seed(03)
load("radomforestmodel.rda")


ui <- fluidPage(
  tags$style(HTML('body {font-family:"Verdana",Georgia,Serif; background-color:white}')),
  theme = shinytheme("united"),
  withAnim(),
  #setBackgroundImage(src = "w.jpg"),
  tags$head(
    tags$style(type = 'text/css', 
               HTML('
                    .navbar-default .navbar-brand{color: ;}
                    .tab-panel{ background-color: #; color: #}
                    .navbar-default .navbar-nav > .active > a, 
                    .navbar-default .navbar-nav > .active > a:focus, 
                    .navbar-default .navbar-nav > .active > a:hover {
                    color: #e6e6e6;
                    background-color: #;
                    
                    }')
                                                )
               ),
  
  tags$style(HTML(".navbar  {
                  background-color:#005380; }
                  
                  .navbar .navbar-nav {float: right; margin-right: 35px;
                  margin-top: 26px;
                  color: #; 
                  font-size: 18px; 
                  background-color: #; }
                  
                  .navbar.navbar-default.navbar-static-top{ 
                  color: #; 
                  font-size: 23px; 
                  background-color: # ;}
                  
                  .navbar .navbar-header {
                  float: left;
                  background-color: # ;}
                  
                  .navbar-default .navbar-brand { color: #e6e6e6; 
                  margin-top: 10px;
                  font-size: 24px; 
                  background-color: # ;} 
                  
                  ")),
  tags$style(type="text/css",
             "#well0{
             padding: 100px;
             background: white;
             border: 1px;
             box-shadow:2px 2px;}"),
  tags$style(type="text/css",
             "#well2{
             padding: 100px;
             background: #;
             border: 1px;
             box-shadow:2px 2px;}"),
  tags$style(type="text/css",
             "#well8{
             padding: 100px;
             background: #;
             border: 1px;
             box-shadow: 2px 2px;}"),
  tags$style(type="text/css",
             "#rrr{
             padding: 100px;
             background: #;
             border: 0px;
             box-shadow: 0px 0px;}"),
  tags$head(
    tags$style(HTML("
                    input[type=\"number\"] {
                    font-size: 20px;height:50px;
                    }
                    
                    "))
    ),
  #tags$style(type='text/css','#qq {background-color: #d2d3d4; color: #004264;font-size:120%}'),
  #tags$style(type='text/css','#qqq {background-color: #d2d3d4; color: #004264;font-size:120%}'),
  #tags$style(type='text/css','#qqqq {background-color: #d2d3d4; color: #004264;font-size:120%}'),
  #tags$style(type='text/css','#qqqqq {background-color: #d2d3d4; color: #004264;font-size:120%}'),
  
  tags$head(HTML("<title>Credit Analytics</title> <link rel='icon' type='image/gif/png' href='t.png'>")),
  navbarPage(id="tabset",tags$li(class = "dropdown",
                                 tags$style(".navbar {min-height:100px }")
  ),
  #title = ,position = "fixed-top",selected = "Upload",inverse = TRUE,
  title = tags$div(img(src="log.png","SeaportAI(Analytics|Robotics)", style="margin-top: -4px;margin-left: 30px;", height = 60)),position = "fixed-top",selected = "Upload",inverse = F,
  tabPanel(title = "Upload",icon = icon("upload"),
           
           fluidPage(
             
             tags$style(" #modal1 .modal-header {background-color:#; border-top-left-radius: 0px; border-top-right-radius: 0px}"),
             
             tags$style(type="text/css",
                        ".shiny-output-error { visibility: hidden; }",
                        ".shiny-output-error:before { visibility: hidden; }"
             ),
             tags$head(tags$style("#pppp{color:black; font-size:35px; font-style:italic; text-align=center;
                                  overflow-y:scroll; max-height: 300px; background: ghostwhite;}")),
             tags$head(tags$style("#roi{color:black; font-size:35px; font-style:italic; text-align=center;
                                  overflow-y:scroll; max-height: 300px; background: ghostwhite;}")),
             
             
             br(),
             br(),
             
             
             column(7,
                    
                    # tags$h3(strong(em("Aim of this Analysi(s):")),style="text-align:center;color:#004264;font-size:180%"),br(),
                    # tags$div(h4("The identification of rare items, events or observations which raise suspicions",style="text-align:center;color:dimgrey"),align="center"),
                    # tags$div(h4("by differing significantly from the majority of the data.",style="text-align:center;color:dimgrey"),align="center"),
                     br(),br(),br(),br(),br(),br(),br(),
                    tags$div(id = 'logo1',img(src="ee.png",height='75%',width='75%'),align="center")
             ),
             
             br(),
             br(),
             
             column(5,
                    
                    
                    bootstrapPage(useShinyjs(),
                                  br(),
                                  
                                  tags$h3(strong(em("Credit Customer Analytics")),style="text-align:center;color:#024b74;font-size:180%"),
                                  
                                  
                                  tags$div(id = 'logo2',img(src="c.jpg",height='60%',width='60%'),align="center"),
                                  
                                  
                                  br(),
                                  withAnim(),
                                  
                                  uiOutput('fileupload'), 
                                  uiOutput('checkbox'),
                                  uiOutput("button"),
                                  uiOutput("helptext"),
                                  br(),
                                  br(),
                                  bsPopover(id="check",title = "",content = "Note: I accept the SeaportAI Terms & Conditions.. Show the Analyse button",placement = "right"),
                                  tags$div(bsButton("reset", label = "Reset ?", icon =   icon("repeat",lib = "glyphicon"),block = F, style="danger",size = "small"),align="center"),
                                  
                                  
                                  #tags$h1(actionButton("myuser","Logout",icon=icon("user")),style="text-align:center"),
                                  br(),
                                  
                                  tags$div(class = "header", checked = NA,style="text-align:center;color:#929292;font-size:100%",
                                           tags$tbody("Need Help ?"),
                                           tags$a(href = "http://seaportai.com/contact-us/", "Contact Us...")
                                  )
                    )
             )
             
             
             
             )),
  
  
  # tabPanel(title = strong("|")), 
  # 
  # 
  # tabPanel(title = "Output",
  #          br(),
  #          br(),
  #          br(),
  #          br(),
  #          br(),
  #          br(),
  #          
  #          fluidRow(
  #            
  #            fluidPage(theme="bootstrap.min.css",
  #                      
  #                      
  #                      tags$style(HTML("
  #                                      .tabbable > .nav > li > a                  {background-color: aqua;  color:black}
  #                                      .tabbable > .nav > li > a[data-value='t1'] {background-color: red;   color:white}
  #                                      .tabbable > .nav > li > a[data-value='t2'] {background-color: blue;  color:white}
  #                                      .tabbable > .nav > li > a[data-value='t3'] {background-color: green; color:white}
  #                                      .tabbable > .nav > li[class=active]    > a {background-color: black; color:white}
  #                                      ")),
  #                      
  #                      
  #                      
  #                      
  #                      tabBox( width = 2000, height = 5500 ,
  #                              
  #                              
  #                              
  #                              tabPanel("Output", 
  #                                       # p("The aim of this application is to predict the churn Churn for the given data.",
  #                                       #             style="font-family:'Euphemia';font-size:11pt"), 
  #                                       wellPanel(  dataTableOutput("y_cap"))
  #                                     ), 
  #                              
  #                              
  #                             
  #                                
  #                                tabPanel("No Default", 
  #                                         wellPanel( dataTableOutput("Not_Defaulter"))
  #                              ), 
  #                              
  #                              
  #                             
  #                                
  #                                tabPanel("Default",  
  #                                         wellPanel( dataTableOutput("Defaulter"))
  #                              ) 
  #                              
  #                              
  #                              
  #                      ))),verbatimTextOutput("dataInfo")
  # ),
  
  
 tabPanel(title = strong("|")),         
  
  
  navbarMenu("More",icon = icon("plus-square"),
             tabPanel(
               tags$div(tags$a(href="javascript:history.go(0)",bsButton("logoutadmin", label = "Logout", icon =   icon("repeat",lib = "glyphicon"),block = F, style="success"),style="text-align:center"),align="center"),
               br()
             )
  ))
        )
        


server <- function(input, output, session) {
  options(shiny.maxRequestSize=50*1024^2)
  
  # observeEvent(input$confirmation, {
  #   if(input$confirmation==TRUE){
  #     updateTabItems(session, "tabset",selected = "Output")
  #   }
  # }) 
  
  observeEvent(input$reset,{
    reset(id = "file")
  })
  
  output[["fileupload"]] <- renderUI({
    input$reset
    tags$div(fileInput("file",label = tags$h4(strong(em("Upload data..")),style="color:#004264;font-size:160%"),accept=c('.csv','.txt')),align="center")
    
  })
  
  output[["checkbox"]] <- renderUI({
    input$reset
    tags$div(checkboxInput("check",tags$a(href = "http://seaportai.com/privacy-policy/", "Terms & Conditions",style="color:green;"),value = TRUE),align="center")
    
  })
  
  output[["button"]] <- renderUI({
    if(input$check==TRUE){
      tags$div(bsButton("analyse",strong("Lets Go..!"),icon = icon("refresh"),style = "primary",size="medium"),
               style="color:white;font-weight:100%;",align="center")
    }
  })
  
  
  output[["helptext"]] <- renderUI({
    if(input$check==TRUE){
      tags$div(helpText("To get results, click the 'Lets go!' button...",style="text-align:center"),align="center")
    }
  })
  
  
  observe(addHoverAnim(session, 'logo1', 'pulse'))
  observe(addHoverAnim(session, 'logo2', 'pulse'))
  observe(addHoverAnim(session, 'analyse', 'shake'))
  observe(addHoverAnim(session, 'reset', 'shake'))
  
  
  observeEvent(input$analyse, {
    confirmSweetAlert(
      session = session,
      inputId = "confirmation",
      type = "warning",
      title = "Are you sure the data was uploaded ?",
      btn_labels = c("Nope", "Yep"),
      danger_mode = TRUE
    )
  })
  
  observeEvent(input$confirmation, {
    if(input$confirmation==TRUE){
    showModal(tags$div(id="modal1", modalDialog(
      inputId = 'Dialog1', 
      title = HTML('<span style="color:#222222; font-size: 20px; font-weight:bold; font-family:sans-serif ">Output<span>
                   <button type = "button" class="close" data-dismiss="modal" ">
                   <span style="color:white; ">x <span>
                   </button> '),
      footer = modalButton("Close"),
      size = "l",
      dataTableOutput("outdata"),
      easyClose = T
      )))
    }
  })
  
  observeEvent(input$confirmation, {
    if(input$confirmation==TRUE){
      output[["outdata"]]<- renderDataTable({
        datatable(pred(),extensions = c('Buttons', 'Scroller'),
                  options = list(
                    dom = 'Bfrtip',
                    deferRender = TRUE,
                    scrollY = 500,
                    scroller = TRUE,
                    buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
                  ),filter = "top")
      })
    }
  })
  
############################################# Data ###############################################################################  
  
  pred<-reactive({
    file1 <- input$file
    if(is.null(file1)) {return(NULL)}
    file2 <- read.csv(file1$datapath,header=TRUE)
    withProgress(message='Loading table',value=30,{
      n<-10
      
      for(i in 1:n){
        incProgress(1/n,detail=paste("Doing Part", i, "out of", n))
        Sys.sleep(0.1)
      }
      
      colnames(file2)=ifelse(colnames(file2)=="PartnerID","CustomerID",
                             
                             
                             ifelse(colnames(file2)=="years_of_business","Age",
                                    ifelse(colnames(file2)=="days_of_credit","Tenure",
                                           ifelse(colnames(file2)=="Totaloutstanding","Balance",
                                                  ifelse(colnames(file2)=="CRISIL_rating","NumOfProducts",
                                                         ifelse(colnames(file2)=="ownership_changed","HasCrCard",
                                                                ifelse(colnames(file2)=="creditamount","EstimatedSalary",
                                                                       ifelse(colnames(file2)=="partial_payment","IsActiveMember",
                                                                              colnames((file2))))))))))
      
      set.seed(0)
      file2$Gender<-as.factor(sample(c("Female","Male"),length(file2$CustomerID),replace=T))
      file2$Geography<-as.factor(sample(c("France","Spain","Germany"),length(file2$CustomerID),replace=T))
      file2$NumOfProducts<-sample(c(1,2,3,4),length(file2$CustomerID),replace=T)
    })
    testing=predict(rf,file2[,-c(1,2,13,14)],type="prob")
    Prediction=ifelse(testing[,2]<0.3,"Not_Defaulter","Defaulter")
    Probability <- testing[,2]
    colnames(file2)[1]<-"PartnerID"
    final_table<-data.frame(file2[,c(1,2)],Prediction,Probability)
    final_table
  })
 
  ############################################################################################################################### 
  
  
  
}

shinyApp(ui, server)