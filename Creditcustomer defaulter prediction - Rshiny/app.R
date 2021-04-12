if(!require("pacman"))install.packages('pacman')
if(!require("shiny"))install.packages('shiny')
if(!require("shinydashboard"))install.packages('shinydashboard')
if(!require("shinythemes"))install.packages('shinythemes')
if(!require("shinyBS"))install.packages('shinyBS')
if(!require("shinyjs"))install.packages('shinyjs')
if(!require("plotly"))install.packages('plotly')
if(!require("readxl"))install.packages('readxl')
if(!require("shinyWidgets"))install.packages('shinyWidgets')
if(!require("DT"))install.packages('DT')
if(!require("magrittr"))install.packages('magrittr')
if(!require("anomalize"))install.packages('anomalize')
if(!require("tidyverse"))install.packages('tidyverse')
if(!require("coindeskr"))install.packages('coindeskr')
if(!require("ggthemes"))install.packages('ggthemes')
if(!require("ggpubr"))install.packages('ggpubr')
if(!require("ggplot2"))install.packages('ggplot2')
if(!require("forecast"))install.packages('forecast')
if(!require("tseries"))install.packages('tseries')
if(!require("shinyBS"))install.packages('shinyBS')
if(!require("shiny"))install.packages('shiny')
if(!require("shinydashboard"))install.packages('shinydashboard')
if(!require("randomForest"))install.packages('randomForest')

library(shiny)
library(shinydashboard)
library(shinythemes)
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

gm= tags$h3(strong("Good Morning",style="color:#446e9b"))
ga= tags$h3(strong("Good Afternoon",style="color:#446e9b"))
ge= tags$h3(strong("Good Evening",style="color:#446e9b"))

#===========
## FUNCTIONS
#===========
## SIMPLE GREETING
good_time <- function(){
    
    ## LOAD PACKAGE
    require(lubridate, quietly = T)
    
    ## ISOLATE currHour
    currhour = hour(now())
    
    
    ## RUN LOGIC
    if(currhour < 12){
        return(gm)
    } 
    else if(currhour >= 12 & currhour < 17){
        return(ga)
    }
    else if( currhour >= 17){
        return(ge)  
    }
}



## STARTING LOGGED VALUE; LET'S CHANGE THAT!
Logged = FALSE;

#tags$div(img(src="wel.png",height='50%',width='50%'),align="center"),
#====
# UI
#====
## make login screen
ui1 <- function(){
    
    tagList(tags$style(HTML('body {font-family:"Verdana",Georgia,Serif; background-color:#446e9b}')),
        div(id="container",align="center",
            div(id = "login",
                # make login panel
                wellPanel(id="well",style = "overflow-y: ;width:100%;height:100%",
                          
                          HTML(paste0('
                                <h2>
                                Hello, ', 
                                      good_time(),
                                      '</h2>',
                                      '<h3>
                                <br>You are in Admin page.</br>
                                </h3>')
                          ),
                          br(),
                          br(),
                          tags$div(textInput("userName", "Username",width = "100%"),align="left"),
                          br(),
                          tags$div(passwordInput("passwd", "Password",width = "100%"),align="left"),
                          br(),
                          # button
                          tags$div(actionButton("Login", "Log in"),align="center"),
                          # login error message
                          tags$div(uiOutput("message"),align="center")
                )
                
            )
        ),
        # css for container
        tags$style(type = "text/css", 
                   "#container{
                   display: flex;
                   justify-content: center;
                   margin-top: 150px;
                   }"),
        # css for login well panel
        tags$style(type="text/css", "
                   #login,{
                   font-size:14px; 
                   width: 360px;}"),
        # well panel
        tags$style(type="text/css",
                   "#well{
                    padding: 50px;
                    background: white;
                   border: 1px;
                   box-shadow: ;}"),
        # welcome text css
        tags$style(type = 'text/css',
                   "h2, h3{
                   color: #525252;}"),
        # input fields
        tags$style(type="text/css",
                   "#userName, #passwd{
                        box-shadow: none;
                        outline:none;
                        border: none;
                        padding-left: 0;
                        border-bottom: 2px solid #446e9b;
                        border-radius: 0;
                   }
                   #userName:focus, #passwd:focus{
                   box-shadow: 0px 10px 10px -5px lightgray;
                   }"),
        # button css
        tags$style(type='text/css',
                   "#Login{
                    outline: none;
                   margin-left: 0px;
                   width: 100px;
                   font-size: 12pt;
                   background: transparent;
                   border: 2px solid #446e9b;
                   color: #446e9b;
                   border-radius: 10px;
                   transition: 0.8s ease-in-out;
                   }
                   #Login:hover{
                   background: #446e9b;
                   color: white;}"),
        # error box - fadeOut animation
        tags$style(type="text/css",
                   "@-webkit-keyframes fadeOut {
                        from {
                            opacity: 1;
                        }
                        to {
                            opacity: 0;
                        }
                    }
                    @keyframes fadeOut {
                        from {
                            opacity: 1;
                        }
                        to {
                            opacity: 0;
                        }
                    }"),
        tags$style(type="text/css",
                   "#error-box{
                   margin-top: 20px;
                   margin-left: 0px;
                   padding: 5px 10px 5px 10px;
                   text-align: center;
                   width: 325px;
                   color: white;
                   background: #ef3b2c;
                   border: 1px solid #ef3b2c;
                   border-radius: 5px;
                   -webkit-animation: fadeOut;
                   animation: fadeOut;
                   opacity: 0;
                   animation-duration: 15s;}")
    )
}

#=========
# PRINT UI
#=========
ui = (uiOutput("page"))

#========
# SERVER
#========

server = shinyServer(function(input, output,session){
  
  options(shiny.maxRequestSize=30*1024^2) 
  
    
   users <- data.frame(User="credit",Password="customer")
    ## BEGIN BUILD LOG IN SCREEN
    USER <- reactiveValues(Logged = Logged)
    
    ## ERROR CHECKING
    observeEvent(input$Login,{
        
        ## output error message
        output$message <- renderUI({
            if(!is.null(input$Login)){
                my_username <- length(users$User[grep(pattern = input$userName, x = users$User)])
                my_password <- length(users$User[grep(pattern = input$passwd, x = users$Password)])
                if(input$Login > 0){
                    if(my_username < 1 ||  my_password < 1){
                        HTML("<div id='error-box'>
                             Sorry, that's not the right username or password. Please, 
                             try again. If you continue to have problems,
                             <a href='https://github.com/sailogeshh/'>
                             <u>Contact..</u></a>
                             </div>")
                    }
                }
            }
        })
        
        ## CHECK INPUT
        if (USER$Logged == FALSE) {
            if (!is.null(input$Login)) {
                if (input$Login > 0) {
                    Username <- isolate(input$userName)
                    Password <- isolate(input$passwd)
                    Id.username <- which(users$User == Username)
                    Id.password <- which(users$Password == Password)
                    if (length(Id.username) > 0 & length(Id.password) > 0) {
                        if (Id.username %in% Id.password) {
                            USER$Logged <- TRUE
                        }
                    }
                }
            }
        }
    })
 
    ## Make UI
    observe({
        # What to do when logged = F
        if (USER$Logged == FALSE) {
            output$page <- renderUI({
                div(class="outer",do.call(bootstrapPage,c("",ui1())))
            })
        }
        
        # Render UI when logged = T
        if (USER$Logged == TRUE) 
        {
            ## get the current user's authorization level 
            user_log <- toupper(input$userName)
            
            # if admin ("input.SELECT == 1 || input.FED == 2" )
            if(user_log == "CREDIT" ){
                output$page <- renderUI({
           ###################################################### ADMIN UI PAGE ###################################################################################################################
                  fluidPage(
                    tags$style(HTML('body {font-family:"Verdana",Georgia,Serif; background-color:white}')),
                    theme = shinytheme("united"),
                    
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
                   
                    tags$head(HTML("<title>Credit Analytics</title> <link rel='icon' type='image/gif/png' href='t.png'>")),
                    navbarPage(id="tabset",tags$li(class = "dropdown",
                                                   tags$style(".navbar {min-height:100px }")
                    ),
                    #title = ,position = "fixed-top",selected = "Upload",inverse = TRUE,
                    title = tags$div(img(src="log.png","Customer Defaulter Prediction", style="margin-top: -4px;margin-left: 30px;", height = 60)),position = "fixed-top",selected = "Upload",inverse = F,
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
                                                    
                                                    
                                                    uiOutput('fileupload'), 
                                                    uiOutput('checkbox'),
                                                    uiOutput("button"),
                                                    uiOutput("helptext"),
                                                    br(),
                                                    br(),
                                                    bsPopover(id="check",title = "",content = "Note: I accept the Terms & Conditions.. Show the Analyse button",placement = "right"),
                                                    tags$div(bsButton("reset", label = "Reset ?", icon =   icon("repeat",lib = "glyphicon"),block = F, style="danger",size = "small"),align="center"),
                                                    
                                                    
                                                    #tags$h1(actionButton("myuser","Logout",icon=icon("user")),style="text-align:center"),
                                                    br(),
                                                    
                                                    tags$div(class = "header", checked = NA,style="text-align:center;color:#929292;font-size:100%",
                                                             tags$tbody("Need Help ?"),
                                                             tags$a(href = "https://github.com/sailogeshh", "Contact Us...")
                                                    )
                                      )
                               )
                               
                               
                               
                               )),
                    
                    
                   
                    tabPanel(title = strong("|")),         
                    
                    
                    navbarMenu("More",icon = icon("plus-square"),
                               tabPanel(
                                 tags$div(tags$a(href="javascript:history.go(0)",bsButton("logoutadmin", label = "Logout", icon =   icon("repeat",lib = "glyphicon"),block = F, style="success"),style="text-align:center"),align="center"),
                                 br()
                               )
                    ))
                    )
                    #########################################################################################################################################################################
                      
                       
                
                })
            }
            
            # if standard user
            else{
                output$page <- renderUI({
                    
                   
                })
            }
        }
    })
    
    ####################################################### server #############################################################################################
 
    
    
    observeEvent(input$reset,{
      reset(id = "file")
    })
    
    output[["fileupload"]] <- renderUI({
      input$reset
      tags$div(fileInput("file",label = tags$h4(strong(em("Upload data..")),style="color:#004264;font-size:160%"),accept=c('.csv','.txt')),align="center")
      
    })
    
    output[["checkbox"]] <- renderUI({
      input$reset
      tags$div(checkboxInput("check",tags$a(href = "https://github.com/sailogeshh", "Terms & Conditions",style="color:green;"),value = TRUE),align="center")
      
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
    
    
    #########################################################################################################################################################################
    
    
    
    
}) # END SHINYAPP

#======
# RUN
#======
shinyApp(ui = ui, server = server)
