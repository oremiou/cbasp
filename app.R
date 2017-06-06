# rsconnect::deployApp('C:/Users/efthimiou/Desktop/post doc/PROJECT/APPLIED PROJECTS/CBASP IPD NMA/cbasp shiny/CBASP')
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
##############################################################################
library(shiny)
library(shinyjs)
# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(
  shinyjs::inlineCSS(list(body = "color:DarkBlue")),
  # Application title
  titlePanel(h1("Predicting severity")),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    
    sidebarPanel(
      h2("Input patient characteristics"),
      sliderInput("sev0",
                  "Baseline depression severity (HAMD24 score):",
                  min = 15,
                  max = 40,
                  value = 26)
      
      ,  sliderInput("IDS",
                     "Baseline anxiety severity (IDS anxiety/arousal factor score):",
                     min = 5,
                     max = 25,
                     value = 12)
      
      ,  sliderInput("age",
                     "Age in years:",
                     min = 25,
                     max = 65,
                     value = 45)
     
      ,checkID <-  checkboxInput("PriorMed", "Prior medication",FALSE)
      ,checkID <-  checkboxInput("emotion", "History of emotional or physical neglect", FALSE) 
      ,
      
      selectInput("married", label = "Marital status", 
                  choices = list("married/defacto/in relationship" = 1, "single" = 2,
                                 "widowed/divorced/separated" = 3), selected = 2),
      
      selectInput("diagnos", label = "Primary diagnosis depression type ", 
                  choices = list("Chronic major depression" = 1, "Dysthymia" = 2,
                                 "Recurrent major depression with incomplete interepisode recovery" = 3), selected = 1)
      
      
    ), 
    
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("tt1"), htmlOutput("text1")   , htmlOutput("text2") 
      , htmlOutput("text3")  , htmlOutput("text4"), htmlOutput("text5"), htmlOutput("text6"), htmlOutput("text7"), htmlOutput("text8"), htmlOutput("text9")
      , tags$head(tags$style("#text1{color: red;
                             font-size: 15px;
                             font-style: bold;
                             }
                             #text2{color: blue;
                             font-size: 15px;
                             font-style: bold;
                             }
                             #text3{color: purple;
                             font-size: 15px;
                             font-style: bold;
                             }
                             #text4{color: black;
                             font-size: 20px;
                             font-style: bold;
                             }
                              #text5{color: black;
                             font-size: 20px;
                             font-style: bold;
                              }
                             #text6{color: black;
                             font-size: 20px;
                             font-style: bold;
                             }
                             #text7{color: green;
                             font-size: 20px;
                             font-style: bold;
                             }
                             #text8{color: green;
                             font-size: 20px;
                             font-style: bold;
                             }
                             #text9{color: green;
                             font-size: 20px;
                             font-style: bold;
                             }"
      )
      )
      
      
      
      
      
      
      
      )
      )
    ))

############################################################################################################################################################

# Define server logic required to draw a histogram
server <- shinyServer(function(input, output) {
  
  a0=-0.226
  b1=0.042
  b2=0.389
  b3=0.0675
  b4=-0.0256
  b5=0.049
  b6=0.101
  b7=0.0625
  
  g1=-0.0561
  g2=0.168
  g3=0.0288
  g4=0.0855
  Mu=-0.095
  
  
  D=matrix(c(.358,.298,.256,.257,0.287, .422, 
             -0.0335,-0.0547,0.0182,0.123,0.319,0.379), ncol = 2)
  
  
  T1=c(0,2,4,6,8,10,12)
  
  ### for treatment 2
  
  
  
  output$tt1<-renderPlot({ 
    IDS=(input$IDS-12)/5
    sev0=(input$sev0-20)/8
    emotion=input$emotion
    prior=input$PriorMed
    
    sev2=c(rep(0,6)) 
    for (i in 1:6){
      sev2[i]= (Mu+ a0*(i)+(b1*IDS+b2*sev0+b3*prior+b4*sev0*sev0+b5*sev0*emotion+b6*sev0*prior+b7*IDS*prior))*8+20
    }
    S2=c(input$sev0,sev2[1],sev2[2],sev2[3],sev2[4],sev2[5],sev2[6])
    
    
    sev1=c(rep(0,6)) 
    for (i in 1:6){
      sev1[i]= (Mu+ a0*(i)+(b1*IDS+b2*sev0+b3*prior+b4*sev0*sev0+b5*sev0*emotion+b6*sev0*prior+b7*IDS*prior)+D[i,1]+(g1*sev0*sev0+g2*IDS*sev0))*8+20
      
    }
    
    S1=c(input$sev0,sev1[1],sev1[2],sev1[3],sev1[4],sev1[5],sev1[6])
    
    
    sev3=c(rep(0,6)) 
    for (i in 1:6){
      sev3[i]= (Mu+ a0*(i)+(b1*IDS+b2*sev0+b3*prior+b4*sev0*sev0+b5*sev0*emotion+b6*sev0*prior+b7*IDS*prior)+D[i,2]+(g3*sev0+g4*prior*sev0))*8+20
      
    }
    
    S3=c(input$sev0,sev3[1],sev3[2],sev3[3],sev3[4],sev3[5],sev3[6])
    
    
    
    
    # Data generation
    x  <- T1
    y1 <- S1
    y2 <- S2
    y3  <- S3
    df<-data.frame(T1,S1,S2,S3)
    require(ggplot2)
    
    g <- ggplot(df, aes(x))+ geom_line(aes(y=S1), colour="red")+ coord_cartesian(xlim=c(0, 12), ylim=c(0, 40)) + geom_line(aes(y=S2), colour="purple")+ylab("Severity (Hamilton rating scale)") + xlab("Weeks")+geom_line(aes(y=S3), colour="blue")
    if(  (input$sev0 >30 & input$IDS <10)|(input$sev0 <20 & input$IDS >20) )  {} else {g}   
    
    
  })
  
  
  logText1 <- reactive({
    if ((input$sev0 >30 & input$IDS <10)|(input$sev0 <20 & input$IDS >20) ) {
      return("Chosen patient characteristics lie outside the capabilities of our prediction model. Please choose different values for HAMD24 and/or IDS")
    }
    else return("red: CBASP       ")
  })
  
  logText2 <- reactive({
    if ((input$sev0 >30 & input$IDS <10)|(input$sev0 <20 & input$IDS >20) ) {
      return("")
    }
    else return("blue: medications        ")
  })
  logText3 <- reactive({
    if ((input$sev0 >30 & input$IDS <10)|(input$sev0 <20 & input$IDS >20) ) {
      return("")
    }
    else return("purple: combination") ##
    
  })
  
  
  dropout1 <- reactive({
    db1=0.11
    db2=-0.16
    db3=-0.07
    db4=-0.09
    db5=0.20
    db6=0.42
    db7=0.43
    db8=-0.16
    db9=-0.22
    db10=-0.38
    dg1=-0.39
    dg2=-0.87
    dg3=-0.13
    dg4=0.09
    dMu=-1.75
    dd1=0.71
    dd2=0.57
    sev0=(input$sev0-20)/8
    ag=(input$age-43.8)/11.3
    lprob1=dMu+db1*sev0+db2*ag+db3*input$PriorMed+db4*sev0*sev0+db5*ag*ag+db6*(input$diagnos==1)*sev0+db7*(input$diagnos==2)*sev0+db8*ag*(input$married==2)+db9*ag*(input$diagnos==1)+db10*(input$married==1)*input$PriorMed +dg1*ag*ag+dg2*ag*(input$diagnos==1)+dd1
    dp1=exp(lprob1)/(1+exp(lprob1))
    dpp1=format(100*dp1,digits=2)
    return(dpp1)
  })
  dropout2 <- reactive({
  db1=0.11
  db2=-0.16
  db3=-0.07
  db4=-0.09
  db5=0.20
  db6=0.42
  db7=0.43
  db8=-0.16
  db9=-0.22
  db10=-0.38
  dg1=-0.39
  dg2=-0.87
  dg3=-0.13
  dg4=0.09
  dMu=-1.75
  dd1=0.71
  dd2=0.57
  dsev0=(input$sev0-20)/8
  ag=(input$age-43.8)/11.3
  lprob2=dMu+db1*dsev0+db2*ag+db3*input$PriorMed+db4*dsev0*dsev0+db5*ag*ag+db6*(input$diagnos==1)*dsev0+db7*(input$diagnos==2)*dsev0+db8*ag*(input$married==2)+db9*ag*(input$diagnos==1)+db10*(input$married==1)*input$PriorMed
  dp2=exp(lprob2)/(1+exp(lprob2))
  dpp2=format(100*dp2,digits=2)
  return(dpp2)
  })
  dropout3 <- reactive({
    db1=0.11
    db2=-0.16
    db3=-0.07
    db4=-0.09
    db5=0.20
    db6=0.42
    db7=0.43
    db8=-0.16
    db9=-0.22
    db10=-0.38
    dg1=-0.39
    dg2=-0.87
    dg3=-0.13
    dg4=0.09
    dMu=-1.75
    dd1=0.71
    dd3=0.57
    sev0=(input$sev0-20)/8
    ag=(input$age-43.8)/11.3
    lprob3=dMu+db1*sev0+db2*ag+db3*input$PriorMed+db4*sev0*sev0+db5*ag*ag+db6*(input$diagnos==1)*sev0+db7*(input$diagnos==2)*sev0+db8*ag*(input$married==2)+db9*ag*(input$diagnos==1)+db10*(input$married==1)*input$PriorMed +dg3*sev0*sev0+dg4*ag*sev0+dd3
    dp3=exp(lprob3)/(1+exp(lprob3))
    dpp3=format(100*dp3,digits=2)
    return(dpp3)
  })
  
  
  output$text1 <- renderText({
    paste(logText1())
  })
  output$text2 <- renderText({
    paste(logText2())
  })
  output$text3 <- renderText({
    paste(logText3())
  })
  output$text4 <- renderText({
    paste("Propability of dropping out within 12 weeks, CBASP:",dropout1(),"%")
  })
  output$text5 <- renderText({
    paste("Propability of dropping out within 12 weeks, COMBINATION:",dropout2(),"%")
  })
  output$text6 <- renderText({
    paste("Propability of dropping out within 12 weeks, MEDS:",dropout3(),"%")
  })
  
  
  
  response1 <- reactive({
    rb1=-0.35
    rb2=0.35
    rb3=-0.40
    rb4=-0.06
    rb5=-0.08
    rb6=0.13
    rb7=-0.26
    rb8=-0.49

    rg1=0.62
    rg2=0.39
    rg3=-0.82
    rg4=0.13
    rg5=-0.43
    rg6=0.12
    
    rMu=0.68
    rd1=-1.27
    rd2=-0.41
    IDS=(input$IDS-12)/5
    sev0=(input$sev0-20)/8
    emotion=input$emotion
    prior=input$PriorMed
    
    rlprob1=rMu+rb1*IDS+rb2*sev0+rb3*prior+rb4*sev0*sev0+rb5*IDS*IDS+rb6*sev0*IDS+rb7*emotion*IDS+rb8*emotion*prior+rg1*IDS+rg2*sev0*sev0+rg3*IDS*sev0+rg4*IDS*prior+rd1
    rp1=exp(rlprob1)/(1+exp(rlprob1))
    rpp1=format(100*rp1,digits=2)
    return(rpp1)
  })
  
  response2 <- reactive({
    rb1=-0.35
    rb2=0.35
    rb3=-0.40
    rb4=-0.06
    rb5=-0.08
    rb6=0.13
    rb7=-0.26
    rb8=-0.49
    
    rg1=0.62
    rg2=0.39
    rg3=-0.82
    rg4=0.13
    rg5=-0.43
    rg6=0.12
    
    rMu=0.68
    rd1=-1.27
    rd2=-0.41
    IDS=(input$IDS-12)/5
    sev0=(input$sev0-20)/8
    emotion=input$emotion
    prior=input$PriorMed
    
    rlprob2=rMu+rb1*IDS+rb2*sev0+rb3*prior+rb4*sev0*sev0+rb5*IDS*IDS+rb6*sev0*IDS+rb7*emotion*IDS+rb8*emotion*prior
    rp2=exp(rlprob2)/(1+exp(rlprob2))
    rpp2=format(100*rp2,digits=2)
    return(rpp2)
  })
  response3 <- reactive({
    rb1=-0.35
    rb2=0.35
    rb3=-0.40
    rb4=-0.06
    rb5=-0.08
    rb6=0.13
    rb7=-0.26
    rb8=-0.49
    
    rg1=0.62
    rg2=0.39
    rg3=-0.82
    rg4=0.13
    rg5=-0.43
    rg6=0.12
    
    rMu=0.68
    rd1=-1.27
    rd2=-0.41
    IDS=(input$IDS-12)/5
    sev0=(input$sev0-20)/8
    emotion=input$emotion
    prior=input$PriorMed
    
    rlprob3=rMu+rb1*IDS+rb2*sev0+rb3*prior+rb4*sev0*sev0+rb5*IDS*IDS+rb6*sev0*IDS+rb7*emotion*IDS+rb8*emotion*prior+rg5*prior+rg6*IDS*sev0+rd2
    rp3=exp(rlprob3)/(1+exp(rlprob3))
    rpp3=format(100*rp3,digits=2)
    return(rpp3)
  })
 
  
  
  output$text7 <- renderText({
    paste("Propability of responding in 12 weeks, CBASP:",response1(),"%")
  })
  output$text8 <- renderText({
    paste("Propability of responding in 12 weeks, COMBINATION:",response2(),"%")
  })
  output$text9 <- renderText({
    paste("Propability of responding in 12 weeks, MEDS:",response3(),"%")
  })
  
  
})

# Run the application 
shinyApp(ui = ui, server = server)


#rsconnect::deployApp('C:/Users/efthimiou/Desktop/post doc/PROJECT/APPLIED PROJECTS/CBASP IPD NMA/cbasp shiny/CBASP')

