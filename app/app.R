#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Rshiny ideas from on https://gallery.shinyapps.io/multi_regression/
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  rm(list=ls())
  set.seed(2345)
  library(mvtnorm) 
  library(rms)
  library(ggplot2)
  library(shiny) 
  library(nlme)
  library(MASS)
  require(tidyverse)
  library(shinyWidgets)
  library(lme4)
  library(DT)
  
  options(max.print=1000000)
  fig.width <- 1200
  fig.height <- 550
  fig.height2 <- 450
  library(shinythemes)        # more funky looking apps
  p1 <- function(x) {formatC(x, format="f", digits=1)}
  p2 <- function(x) {formatC(x, format="f", digits=2)}
  options(width=100)
   
  
  # function to create longitudinal data
  
  is.even <- function(x){ x %% 2 == 0 }

 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 
ui <- fluidPage(theme = shinytheme("journal"), #https://www.rdocumentation.org/packages/shinythemes/versions/1.1.2
                # paper
             #   useShinyalert(),  # Set up shinyalert
                setBackgroundColor(
                    color = c( "#2171B5", "#F7FBFF"), 
                    gradient = "linear",
                    direction = "bottom"
                ),
                h3("Simulating longitudinal data analysing and estimating a treatment effect"),
                shinyUI(pageWithSidebar(
                    headerPanel(" "),
                    
                    
                    
                    #sidebarPanel( 
                      
                      sidebarPanel( width=3 ,
                                    tags$style(type="text/css", ".span8 .well { background-color: #00FFFF; }"),
                      
                      
                        div(p("xxxxxxxxxxx")),
                        
                        div(
                            
                            actionButton(inputId='ab1', label="R code",   icon = icon("th"), 
                                         onclick ="window.open('https://raw.githubusercontent.com/eamonn2014/biochemistry-and-haematology/master/heam_biochem/app.R', '_blank')"),   
                            actionButton("resample", "Simulate a new sample"),
                            br(), br(),
                            tags$style(".well {background-color:#b6aebd ;}"), 
                            
                            div(h5(tags$span(style="color:blue", "Select the parameters using the sliders below"))),
                          
                                
                                p(" "),
                            
                            div(("  
                           xxxxxxxxxxxxxxxxxx ")),
                            br(),
                            
                            tags$head(
                              tags$style(HTML('#ab1{background-color:orange}'))
                            ),
                            
                            tags$head(
                              tags$style(HTML('#resample{background-color:orange}'))
                            ),
                            
                            
                            
                            # selectInput("Plot",
                            #             strong("1. Select which biochemistry test to present"),
                            #             choices=biochemistry),

                            selectInput("Plot1",
                                        div(h5(tags$span(style="color:blue", "Select plot"))),
                                        choices=c("Overall","Individual" )),
                            
                            
                            textInput('vec1', 
                                      div(h5(tags$span(style="color:blue", "Select patient. If '2 select plot' 'Individual' is selected, enter sample ID(s) (comma delimited); 
                                      enter 999 to show all profiles; If 'Individual all tests' is selected, all test results for the first ID only are presented"))),
                                       "1,2,3,4"),
                            
                            
                            
                            #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                            
                            sliderInput("N",
                                        div(h5(tags$span(style="color:blue", "total number of subjects"))),
                                        min=2, max=500, step=1, value=200, ticks=FALSE),
                            
                            sliderInput("J",
                                        div(h5(tags$span(style="color:blue", "Maximum visit in data simulation including baseline"))),
                                        min=3, max=10, step=1, value=8, ticks=FALSE),
                            
                            sliderInput("trt.effect",
                                        div(h5(tags$span(style="color:blue", "Treatment effect"))),
                                        min = -10, max = 10, value = c(-1), step=1, ticks=FALSE),
                            
                            sliderInput("interaction",    
                                        div(h5(tags$span(style="color:blue", "Treatment time interaction"))),
                                        min = -1, max = 1, value = c(-.2), step=.1, ticks=FALSE),
                            
                            sliderInput("beta0", 
                                        div(h5(tags$span(style="color:blue", "Average intercept"))),
                                              min = -10, max = 10, step=0.5, value = c(10), ticks=FALSE) ,
                            
                            sliderInput("beta1", 
                                        div(h5(tags$span(style="color:blue", "Average slope"))),
                                        min = -5, max =5, step=.5, value = c(0),ticks=FALSE),
                            
                            sliderInput("q",   
                                        div(h5(tags$span(style="color:blue", "True intercept SD"))),
                                        min = .1, max = 10, value = c(.5), step=.5, ticks=FALSE),
                            
                            sliderInput("s",      
                                        div(h5(tags$span(style="color:blue", "True slope SD"))),
                                        min = .01, max = 10, value = c(.01),step=.01,  ticks=FALSE),
                            
                            sliderInput("r", 
                                        div(h5(tags$span(style="color:blue", "True intercept slope correlation"))),
                                        min = -1, max = 1, value = c(.9), step=0.05, ticks=FALSE),
                            
                            sliderInput("sigma",  
                                        div(h5(tags$span(style="color:blue",  "True error SD"  ))),
                                        min =.01, max = 10, value = c(.1), step=.1, ticks=FALSE),
                            
                            sliderInput("time.ref",
                                        div(h5(tags$span(style="color:blue", "Estimate treatment effect at this visit"))),
                                        min=1, max=10, step=1, value=4, ticks=FALSE),
                           
                           
                           
                           
                           
                           
                           
                  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                            
                 
                            div(p( strong("References:"))),  

                            tags$a(href = "https://en.wikipedia.org/wiki/Anscombe%27s_quartet", "[1] Anscombe's quartet"),
                            div(p(" ")),
                            tags$a(href = "https://en.wikipedia.org/wiki/Comprehensive_metabolic_panel", "[2] Comprehensive metabolic panel"),
                            div(p(" ")),
                            tags$a(href = "https://ggplot2.tidyverse.org/reference/geom_boxplot.html", "[3] Boxplots using ggplot2"),
                            div(p(" ")),
                            tags$a(href = "https://twitter.com/f2harrell/status/1220700181496320001", "[4] Purpose of RCT"),
                            div(p(" ")),
                            
                            
                        )
                        
                    ),
                    
                    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~tab panels
                    mainPanel(
                        
                        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                        #    tabsetPanel(type = "tabs", 
                        navbarPage(       
                            #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
                            tags$style(HTML(" 
                            .navbar-default .navbar-brand {color: cyan;}
                            .navbar-default .navbar-brand:hover {color: blue;}
                            .navbar { background-color: #b6aebd;}
                            .navbar-default .navbar-nav > li > a {color:black;}
                            .navbar-default .navbar-nav > .active > a,
                            .navbar-default .navbar-nav > .active > a:focus,
                            .navbar-default .navbar-nav > .active > a:hover {color: pink;background-color: purple;}
                            .navbar-default .navbar-nav > li > a:hover {color: black;background-color:yellow;text-decoration:underline;}
                            .navbar-default .navbar-nav > li > a[data-value='t1'] {color: red;background-color: pink;}
                            .navbar-default .navbar-nav > li > a[data-value='t2'] {color: blue;background-color: lightblue;}
                            .navbar-default .navbar-nav > li > a[data-value='t3'] {color: green;background-color: lightgreen;}
     
                   ")), 
                            #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~end of section to add colour     
                            tabPanel("Plot and lmer fit", 
                                     #    h2("Plotting the data"),
                                     div(plotOutput("reg.plot1", width=fig.width, height=fig.height)),  
                                     
                                     h3(" "),
                                     div(class="span7", verbatimTextOutput("reg.summary")),
                                     p(strong(
                                         "xxxxxxxxxxx")) ,
                                     
                                     #                     
                                     p(strong("Txxxxxxxxxxxxxxxxx
                           ")),
                                     
                                     
                            ) ,
                            #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                            tabPanel("Gls fit", value=3, 
                                     #  div( verbatimTextOutput("table2")),
                                     h4("xxxxxxxxxxxxxxxxxxx"),#
                                     h6("xxxxxxxxxxxxxxxx"),
                                     div(class="span7", verbatimTextOutput("reg.summary1")),
                                     div(class="span7", verbatimTextOutput("reg.summary2")),
                                   #  DT::dataTableOutput("table2"),
                                     #h6("This is superior to a plain rtf output in that this can be sorted and filtered on the fly."),
                                     # tags$head(tags$style("#dummy table {background-color: red; }", media="screen", type="text/css")),
                                     
                            ) ,
                            #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                            tabPanel("Plot of the treatment effect estimates", value=6, 
                                     h4("Modelling"),
                                     p(strong("xxxxxxxxxxxxxxx")),
                                     div(plotOutput("reg.plot2", width=fig.width, height=fig.height)),  
                                     div(plotOutput("reg.plot3", width=fig.width, height=fig.height)),  
                                   #  div(class="span7", verbatimTextOutput("reg.summaryx")),
                                  #   div(class="span7", verbatimTextOutput("table4")),
                                    # div(class="span7", verbatimTextOutput("reg.summary2")),
                            ) ,
                            
                            #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                            tabPanel("Plot of the treatment effect estimates", 
                                     
                                     div(class="span7", verbatimTextOutput("reg.summaryb1")),
                                     div(class="span7", verbatimTextOutput("reg.summaryb2")),
                                     div(class="span7", verbatimTextOutput("reg.summaryb3")),
                                     #  h4("Plot of the treatment effect estimates"),
                                    # div(plotOutput("reg.plote", width=fig.width, height=fig.height2)),  
                                    # div(DT::dataTableOutput("reg.summary4"), style = "font-size: 110%; width: 75%")
                                     
                                     
                            ) ,
                            #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                            tabPanel("Plot of the treatment effect estimates2", 
                                     
                                     div(plotOutput("reg.plot2b", width=fig.width, height=fig.height)),  
                                     div(plotOutput("reg.plot3b", width=fig.width, height=fig.height)),  
                                     div(plotOutput("reg.plot4b", width=fig.width, height=fig.height)),  
                                     #  h4("Plot of the treatment effect estimates"),
                                     #  h4("Plot of the treatment effect estimates"),
                                     # div(plotOutput("reg.plote", width=fig.width, height=fig.height2)),  
                                     # div(DT::dataTableOutput("reg.summary4"), style = "font-size: 110%; width: 75%")
                                     
                                     
                            ) ,
                            
                            #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                            tabPanel("selecying patient", 
                                     
                                     div(plotOutput("reg.plot33", width=fig.width, height=fig.height)),  
                                   #  div(plotOutput("reg.plot3b", width=fig.width, height=fig.height)),  
                                  #   div(plotOutput("reg.plot4b", width=fig.width, height=fig.height)),  
                                     #  h4("Plot of the treatment effect estimates"),
                                     #  h4("Plot of the treatment effect estimates"),
                                     # div(plotOutput("reg.plote", width=fig.width, height=fig.height2)),  
                                     # div(DT::dataTableOutput("reg.summary4"), style = "font-size: 110%; width: 75%")
                                     
                                     
                            ) ,
                            
                            #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                            tabPanel("Diagnostics",
                                     h4("Four residual plots to check for absence of trends in central tendency and in variability"),
                                    div(plotOutput("res.diag", width=fig.width, height=fig.height)),       
                                     p(strong("Upper left panel shows the baseline score on the x-axis. 
                                              Upper right panel shows shows time on the x-axis, note though the selected reference level is plotted first.
                                              Bottom left panel is the QQ plot for checking normality of residuals from the GLS fit.
                                              Bottom right panel is the histogram for checking normality of residuals from the GLS fit with ~N(mean=0, sd=GLS model sigma) curve superimposed.
                                              ")),
                            ),
                            
                            #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                            tabPanel("Data listing", value=3, 
                                     #  h4("Data listing"),
                                     h6("This is superior to a plain rtf output in that this can be sorted and filtered on the fly."),
                                    DT::dataTableOutput("table1"),
                                     
                                     
                            ) 
                            #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                        )
                        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                    )
                    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~end tab panels 
                    
                )
                )
)

server <- shinyServer(function(input, output   ) {
    
    
    # --------------------------------------------------------------------------
    # This is where a new sample is instigated 
    random.sample <- reactive({
        
        # Dummy line to trigger off button-press
        foo <-      input$resample
        
        N <-     input$N
        beta0 <-  input$beta0 
        beta1 <-  input$beta1
        sigma <-  input$sigma
        q <-  input$q
        s <-  input$s
        r <-  input$r
        J <-  input$J
        trt <- input$trt.effect 
        interaction = input$interaction
        
        time.ref <-  input$time.ref
        
        return(list( N=N,  beta0=beta0, beta1=beta1, sigma=sigma, q=q, s=s, r=r, J=J, time.ref=time.ref ,
                     interaction=interaction, trt=trt
                     )) 
  
        
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     })
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
    #  start creating data
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
    
    make.data <- reactive({
        
        sample <- random.sample()
        
        N        <-  sample$N 
        intercept <-  sample$beta0 
        slope    <-  sample$beta1
        sigma    <-  sample$sigma
        q        <-  sample$q # standard deviations for the intercept 
        s        <-  sample$s # standard deviations for slope
        r        <-  sample$r # random effects correlation of slope internet
        J        <-  sample$J
        time.ref <-  sample$time.ref
        interaction<-  sample$interaction
        trt   <-  sample$trt
        
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # set up patients and random treatment assignment
        
        unit.df <- data.frame(unit = c(1:N), treat = sample(0:1,   N, replace=TRUE) )  
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        
        unit.df <-  within(unit.df, {
            E.alpha.given.treat <-  intercept + trt  * treat      # trt is the true treatment effect
            E.beta.given.treat  <-  slope + interaction * treat   # interaction effect
        })
     
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # covariance matrix for random effects
        
        cov.matrix <- matrix(c(q^2, r * q * s, r * q * s, s^2), nrow = 2, byrow = TRUE)
        
        random.effects <- rmvnorm(N, mean = c(0, 0), sigma = cov.matrix)
        
        # add random effects
        unit.df$alpha <- unit.df$E.alpha.given.treat + random.effects[, 1]
        unit.df$beta <-  unit.df$E.beta.given.treat  + random.effects[, 2]
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # structure of experimental design and time points
 
        x.grid = seq(0, 8, by = 8/J)[0:8]
        
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # set up unbalanced visits everyone at first visit but randomly end up to max after that
        
        p <- round(runif(N,2,J))                                                 # last visit for each person
        
        M= sum(p)                                                                # Total number of observations 
        
        unit <- sort(rep(c(1:N), times=p))                                       # id for each person
        
        j <- as.vector(unlist(tapply(X=unit, INDEX=list( unit), FUN=seq_along))) # count with each person
        
        time <- j-1                                                              # first visit , baseline becomes 0
        
        within.unit.df <- data.frame(cbind(unit, j, time))                       # create a data frame
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~end unbalanced
        
        # merge design and dataand create response
        
        flat.df = merge(unit.df, within.unit.df)
        flat.df <-  within(flat.df, y <-  alpha + time * beta + error * rnorm(n = M))
        flat.df$treat <- as.factor(flat.df$treat)
        flat.df$treat <- ifelse(flat.df$treat %in% 1, "Active","Placebo" )
        
        return(list(  flat.df = flat.df,  random.effects= random.effects, p=p) )
        
    }) 
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # creating baseline variable, I want treatment effect to start after baseline,
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    make.data2 <- reactive({
      
      sample <- random.sample()

      N        <-  sample$N 
      flat.df        <- make.data()$flat.df
      random.effects <- make.data()$random.effects
      p <- make.data()$p

      nbaseline <- flat.df[(flat.df$time !=0),]  
      
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # all observations that are baseline
      baseline <- flat.df[(flat.df$time ==0),]  
      # overwrite so no treatment effect manifests at baseline
      baseline$alpha <- intercept + random.effects[, 1]
      baseline$beta <-  slope     + random.effects[, 2]
      
      # use tmp later for a plot
      tmp <- baseline <- within(baseline, y <-  alpha + 0 * beta + error * rnorm(n = N) )
      
      baseline <- baseline[,c("unit","y"   )] 
      names(baseline) <- c("unit","baseline"  )  # rename y to baseline
      
      # merge baseline and no baseline
      both <- merge (baseline , nbaseline  , all=TRUE)
      
      d <-  both[, c("unit", "baseline", "treat", "time", "y")]
      d$time<-factor(d$time)
      d$treat<-factor(d$treat)
      d$time <- relevel(d$time, ref=time.ref)
      
      # just put in random countries so no association
      #d$country <-  factor(sort(rep(sample(1:8 ),   N, times=J-1)))   # balanced
      d$country <- factor(rep(sample(1:8 , N, replace=T), times=p-1))  # unbalanced
      
      return(list( d=d , tmp=tmp, nbaseline=nbaseline) )
      
    }) 
 
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # run lmer and gls (and get contrasts) on the data were treatment effect starts after time 0
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    fit.regression.base<- reactive({
      
      d<- make.data2()$d
      
      my.lmer <-  lmer(y ~  country + baseline * treat + time * treat + (1 + as.numeric(time) | unit), data = d)
      
      ddz <<- datadist(d)  # need the double in this environ <<
      options(datadist='ddz')
      
      
      (fit.res <-  
          tryCatch(Gls(y  ~ country + baseline * treat + time * treat ,
                       correlation=corSymm(form = ~as.numeric(time) | unit) ,
                       weights=varIdent(form=~1|time),
                       d, x=TRUE,
                       na.action=na.exclude ),  
                   error=function(e) e)
      ) 
      
      J <-  input$J
      time. <- rep(1:(J-1))
      
      # k1 <- contrast(fit, list(time=time.,  treat = "Placebo", baseline=median(d$baseline)),
      #                list(time=time.,  treat =  "Active", baseline=median(d$baseline)))
      # 
      # match model output
      k1 <- contrast(fit.res, list(time=time.,  treat = "Placebo", baseline=0, country=1),
                              list(time=time.,  treat =  "Active", baseline=0, country=1))
      
      x <- as.data.frame(k1[c('time', 'Contrast', 'Lower', 'Upper')]) 
      
      namez <- c("Follow-up Visit", "Placebo - Active estimate", "Lower 95%CI","Upper 95%CI")
      
      names(x) <- namez

      return(list( fit.lmer= summary(my.lmer) , fit.res=fit.res , x=x))
      
    })   
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    output$reg.summaryb1 <- renderPrint({
      
      summary <- fit.regression.base()$fit.res
      
      return(list(summary))
      
    })  
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    output$reg.summaryb2 <- renderPrint({
      
      summary <- fit.regression.base()$fit.lmer
      
      return(list(summary))
      
    })  
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    output$reg.summaryb3 <- renderPrint({
      
      summary <- fit.regression.base()$x
      
      return(list(summary))
      
    })  
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    
  
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # spaghetti plot of the data in which trt effect starts at baseline 
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    output$reg.plot1 <- renderPlot({         
        
        flat.df <- make.data()$flat.df
        
        ggplot(flat.df,   aes (x = time, y = y, group = unit, color = treat)) +
            geom_line() + geom_point() + ylab("response") + xlab("visit") +
            stat_summary(fun=mean,geom="line", colour="black",lwd=1,aes(group=treat ) ) +
            # geom_smooth(method=lm, se=FALSE, fullrange=TRUE )+
            # scale_shape_manual(values=c(3, 16))+ 
            scale_color_manual(values=c('#999999','#E69F00'))+
            theme(legend.position="top") +
            xlim(0, J) +
            scale_x_continuous(breaks=c(0:J)) 
        
    }) 
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # fit lmer regression on the data in which trt effect starts at baseline
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    fit.regression0 <- reactive({
        
        flat.df <- make.data()$flat.df
        
        my.lmer <-  lmer(y ~    time * treat + (1 + time | unit), data = flat.df)

        return(list(fit.res= summary(my.lmer) ))
        
    })     
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # model output
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    output$reg.summary <- renderPrint({
        
        summary <- fit.regression0()$fit.res
        
        return(list(summary))
        
    })  
    
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # fit gls regression on the data in which trt effect starts at baseline and get contrasts over time
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    fit.regression.gls0 <- reactive({
      
      flat.df <- make.data()$flat.df
      ## new is this what we expect
    
      time.ref <-  input$time.ref
      
      tmp <- flat.df
      
      tmp$j <- factor(tmp$j)
      tmp$j <- relevel(tmp$j, ref=time.ref)

      #table(tmp$j, tmp$time)
      tmp$j <- as.factor(tmp$j )
      
      ddz <<- datadist(tmp)  # need the double in rshiny environ <<
      options(datadist='ddz')
      
      #j works but not time consecutive integer error in gls?
      #table(tmp$j, tmp$time)
      #tmp$j <- as.factor(tmp$j )
      
      fit.res <- NULL
      (fit.res <-
          tryCatch(Gls(y  ~ treat + j * treat ,
                       correlation=corSymm(form = ~as.numeric(j) | unit) ,
                       weights=varIdent(form=~1|j),
                       tmp, x=TRUE,
                       na.action=na.exclude ),
                   error=function(e) e)
      )
      
     fit <-  fit.res
      
      J <-  input$J
       time. <- rep(1:(J))
      
      k1a <- rms::contrast(fit, list(j=time.,  treat = "Active"  ),
                                list(j=time.,  treat = "Placebo" ))
      
      #k1a
      
      
      return(list(fit.res= fit.res , k1a=k1a ))
      
    })     
    
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # plot of treatment effect for data at which trt effect starts at baseline
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    output$reg.plot2 <- renderPlot({         
      
      k1a <-  fit.regression.gls0()$k1a
       
      J <-  input$J
      time. <- rep(1:(J))
      k1a <- as.data.frame(k1a[c(1,2,4,5)])
      
      mi <- floor(min(k1a$Lower))
      ma <- ceiling(max(k1a$Upper))
      
      names(k1a) <- (c( "Time",'Contrast', 'Lower', 'Upper'))
      
      xl <- xlab( 'Follow up visit (Visit 1 is baseline)')
      
      ggplot (k1a, aes(x=time. , y=Contrast, group=1)) + geom_point () + geom_line () +
        ylim(mi,ma) +
        #   xlim(1, input$V-1) +
        xlim(1, J) +
        scale_x_continuous(breaks=c(time.)) +
        
        ylab( 'Placebo - Active')+ xl +
        geom_errorbar(aes(ymin=Lower, ymax=Upper ), width =0) +
        ggtitle(paste0("Outcome measure "," \ntreatment effect estimate at each visit with 95% CI")) +
        geom_hline(aes(yintercept = 0, colour = 'red'), linetype="dashed") +
        theme_bw() +
        theme(legend.position="none") +
        theme(#panel.background=element_blank(),
          # axis.text.y=element_blank(),
          # axis.ticks.y=element_blank(),
          # https://stackoverflow.com/questions/46482846/ggplot2-x-axis-extreme-right-tick-label-clipped-after-insetting-legend
          # stop axis being clipped
          plot.title=element_text(size = 18), plot.margin = unit(c(5.5,12,5.5,5.5), "pt"),
          legend.text=element_text(size=14),
          legend.title=element_text(size=14),
          legend.position="none",
          axis.text.x  = element_text(size=15),
          axis.text.y  = element_text(size=15),
          axis.line.x = element_line(color="black"),
          axis.line.y = element_line(color="black"),
          plot.caption=element_text(hjust = 0, size = 7),
          strip.text.x = element_text(size = 16, colour = "black", angle = 0),
          axis.title.y = element_text(size = rel(1.5), angle = 90),
          axis.title.x = element_text(size = rel(1.5), angle = 0),
          panel.grid.major.x = element_line(color = "grey80", linetype="dotted", size = 1),
          panel.grid.major.y = element_line(color = "grey80", linetype="dotted", size = 1),
          strip.background = element_rect(colour = "black", fill = "#ececf0"),
          panel.background = element_rect(fill = '#ececf0', colour = '#ececf0'),
          plot.background = element_rect(fill = '#ececf0', colour = '#ececf0')
        )
      
      
      
      
    }) 
    
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # boxplots of data at which trt effect starts at baseline
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    output$reg.plot3 <- renderPlot({         
      
      flat.df <- make.data()$flat.df
      d <- flat.df
      
      d$trt <- d$treat
      d$rep <- d$unit
      d$yij <- d$y
      d$time <- factor(d$time)
      # lets get counts to put in ribbons
      d$trt <- factor(d$trt)
      dx <- unique(d[,c("rep","trt")])
      table(dx$trt)
      n <- as.vector(table(dx$trt))
      levels(d$trt) <- c(paste0("Active N=",n[1]), paste0("Placebo N=",n[2])) 
      
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      pd <- position_dodge(.4)
      pr1=NULL
      pr1 <- ggplot(d,aes(x=time ,y=yij,color=trt, fill=trt ))  + 
        stat_boxplot(geom = "errorbar", width = 0.3) +
        geom_boxplot( outlier.colour = NA  ) +  # removed fill=NA
        geom_line(aes(group=rep), position = pd,  alpha=0.6, linetype="dotted")   + 
        scale_size_manual( values = c( 1) ) +
        geom_point(aes(fill=trt, group=rep), pch=1, size=1, alpha=0.3, position = pd ) +
        stat_summary(fun=mean, geom="point", shape=3, size=2, colour="black", stroke=1.5,
                     position=pd, show.legend=FALSE) +
        scale_color_manual(name = "Treatment", values = c("blue", "darkgreen")) +
        scale_fill_manual(name = "Treatment", values = c("lightblue", "green")) +
        facet_wrap(~trt , ncol=2)    +
        labs(caption = "- The upper whisker is located at the smaller of the maximum y value and Q3 + 1.5xIQR, whereas the lower whisker is located at the larger of the smallest y value and Q1 - 1.5xIQR\n- The median is the horizontal line inside each box and the mean denoted by the cross\n -Individual patient profiles are denoted by dotted lines\n- A small amount of jitter is added to the data to aid visualisation.") +
        
        geom_text(data = d %>% group_by( time, trt) %>%
                    dplyr::summarise(Count = n()) %>%
                    ungroup %>%
                    mutate(yij=min((d$yij)) - 0.05 * diff(range((d$yij)))),
                  aes(label = paste0("n = ", Count)),
                  position = pd, size=3, show.legend = FALSE) 
      
      
      print(pr1 + labs(y="Response", x = 'Follow up vists (baseline not shown)') +    
              ggtitle(paste0("There are N=",
                             length(unique(d$rep)),  
                             " patients with data at baseline, presenting all patient profiles, with boxplots and the number of patient values at each visit") ) +
              theme_bw() +
              theme(legend.position="none") +
              theme(#panel.background=element_blank(),
                # axis.text.y=element_blank(),
                # axis.ticks.y=element_blank(),
                # https://stackoverflow.com/questions/46482846/ggplot2-x-axis-extreme-right-tick-label-clipped-after-insetting-legend
                # stop axis being clipped
                plot.title=element_text(size = 18), plot.margin = unit(c(5.5,12,5.5,5.5), "pt"),
                legend.text=element_text(size=14),
                legend.title=element_text(size=14),
                legend.position="none",
                axis.text.x  = element_text(size=15),
                axis.text.y  = element_text(size=15),
                axis.line.x = element_line(color="black"),
                axis.line.y = element_line(color="black"),
                plot.caption=element_text(hjust = 0, size = 11),
                strip.text.x = element_text(size = 16, colour = "black", angle = 0),
                axis.title.y = element_text(size = rel(1.5), angle = 90),
                axis.title.x = element_text(size = rel(1.5), angle = 0),
                strip.background = element_rect(colour = "black", fill = "#ececf0"),
                panel.background = element_rect(fill = '#ececf0', colour = '#ececf0'),
                plot.background = element_rect(fill = '#ececf0', colour = '#ececf0'),#
              ) 
      )
      #   input$Plot
      
   })
      
      
      
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # plot of treatment effect for data at which trt effect starts after baseline
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
      output$reg.plot2b <- renderPlot({         
        
        k1a <- fit.regression.base()$x
        fit <- fit.regression.base()$fit.res
        J <-  input$J
        time. <- rep(1:(J-1))
      
        
        k1a <- contrast(fit, list(time=time.,  treat = 'Placebo'),
                             list(time=time.,  treat = 'Active'))
        
        k1a <- as.data.frame(k1a[c(3,4,6,7)])
        
        mi <- floor(min(k1a$Lower))
        ma <- ceiling(max(k1a$Upper))
        
        names(k1a) <- (c( "Time",'Contrast', 'Lower', 'Upper'))
        
        k1 <- as.data.frame(k1a)
        
        xl <- xlab( 'Follow up visit (baseline not shown)')
        
        ggplot (k1, aes(x=time. , y=Contrast, group=1)) + geom_point () + geom_line () +
          ylim(mi,ma) +
          #   xlim(1, input$V-1) +
          xlim(1, J) +
          scale_x_continuous(breaks=c(time.)) +
          
          ylab( 'Placebo - Active')+ xl +
          geom_errorbar(aes(ymin=Lower, ymax=Upper ), width =0) +
          ggtitle(paste0("Outcome measure "," \ntreatment effect estimate at each visit with 95% CI")) +
          geom_hline(aes(yintercept = 0, colour = 'red'), linetype="dashed") +
          theme_bw() +
          theme(legend.position="none") +
          theme(#panel.background=element_blank(),
            # axis.text.y=element_blank(),
            # axis.ticks.y=element_blank(),
            # https://stackoverflow.com/questions/46482846/ggplot2-x-axis-extreme-right-tick-label-clipped-after-insetting-legend
            # stop axis being clipped
            plot.title=element_text(size = 18), plot.margin = unit(c(5.5,12,5.5,5.5), "pt"),
            legend.text=element_text(size=14),
            legend.title=element_text(size=14),
            legend.position="none",
            axis.text.x  = element_text(size=15),
            axis.text.y  = element_text(size=15),
            axis.line.x = element_line(color="black"),
            axis.line.y = element_line(color="black"),
            plot.caption=element_text(hjust = 0, size = 7),
            strip.text.x = element_text(size = 16, colour = "black", angle = 0),
            axis.title.y = element_text(size = rel(1.5), angle = 90),
            axis.title.x = element_text(size = rel(1.5), angle = 0),
            panel.grid.major.x = element_line(color = "grey80", linetype="dotted", size = 1),
            panel.grid.major.y = element_line(color = "grey80", linetype="dotted", size = 1),
            strip.background = element_rect(colour = "black", fill = "#ececf0"),
            panel.background = element_rect(fill = '#ececf0', colour = '#ececf0'),
            plot.background = element_rect(fill = '#ececf0', colour = '#ececf0')
          )
        
      }) 
      
      
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # spaghetti plot of the data in which trt effect starts after baseline 
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

          output$reg.plot3b <- renderPlot({         
        
        
        tmp <- make.data2()$tmp
        nbaseline <- make.data2()$nbaseline
      
        all <- rbind(tmp, nbaseline)
        
        all$time <- as.numeric(as.character(all$time ))
        
        ggplot(all,   aes (x = time, y = y, group = unit, color = treat)) +
          geom_line() + geom_point() + ylab("response") + xlab("visit") +
          stat_summary(fun=mean,geom="line", colour="black",lwd=1,aes(group=treat ) ) +
          # geom_smooth(method=lm, se=FALSE, fullrange=TRUE )+
          # scale_shape_manual(values=c(3, 16))+ 
          scale_color_manual(values=c('#999999','#E69F00'))+
          theme(legend.position="top") +
          xlim(0, J) +
          scale_x_continuous(breaks=c(0:J)) 
        
    
      }) 
      
      
      
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # boxplots of data at which trt effect starts after baseline 
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          
      output$reg.plot4b <- renderPlot({         
        
        
        tmp <- make.data2()$tmp
        nbaseline <- make.data2()$nbaseline
        
        all <- rbind(tmp, nbaseline)
        
        #####################################################################################################
        
        d <- all  # add this to shown baslein
        d$trt <- d$treat
        d$rep <- d$unit
        d$yij <- d$y
        d$time <- factor(d$time)
        
        # lets get counts to put in ribbons
        d$trt <- factor(d$trt)
        dx <- unique(d[,c("rep","trt")])
        table(dx$trt)
        n <- as.vector(table(dx$trt))
        levels(d$trt) <- c(paste0("Active N=",n[1]), paste0("Placebo N=",n[2])) 
        
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        pd <- position_dodge(.4)
        pr1=NULL
        pr1 <- ggplot(d,aes(x=time ,y=yij,color=trt, fill=trt ))  + 
          stat_boxplot(geom = "errorbar", width = 0.3) +
          geom_boxplot( outlier.colour = NA  ) +  # removed fill=NA
          geom_line(aes(group=rep), position = pd,  alpha=0.6, linetype="dotted")   + 
          scale_size_manual( values = c( 1) ) +
          geom_point(aes(fill=trt, group=rep), pch=1, size=1, alpha=0.3, position = pd ) +
          stat_summary(fun=mean, geom="point", shape=3, size=2, colour="black", stroke=1.5,
                       position=pd, show.legend=FALSE) +
          scale_color_manual(name = "Treatment", values = c("blue", "darkgreen")) +
          scale_fill_manual(name = "Treatment", values = c("lightblue", "green")) +
          facet_wrap(~trt , ncol=2)    +
          labs(caption = "- The upper whisker is located at the smaller of the maximum y value and Q3 + 1.5xIQR, whereas the lower whisker is located at the larger of the smallest y value and Q1 - 1.5xIQR\n- The median is the horizontal line inside each box and the mean denoted by the cross\n -Individual patient profiles are denoted by dotted lines\n- A small amount of jitter is added to the data to aid visualisation.") +
          
          geom_text(data = d %>% group_by( time, trt) %>%
                      dplyr::summarise(Count = n()) %>%
                      ungroup %>%
                      mutate(yij=min((d$yij)) - 0.05 * diff(range((d$yij)))),
                    aes(label = paste0("n = ", Count)),
                    position = pd, size=3, show.legend = FALSE) 
        
        
        print(pr1 + labs(y="Response", x = 'Follow up vists (baseline not shown)') +    
                ggtitle(paste0("There are N=",
                               length(unique(d$rep)),  
                               " patients with data at baseline, presenting all patient profiles, with boxplots and the number of patient values at each visit") ) +
                theme_bw() +
                theme(legend.position="none") +
                theme(#panel.background=element_blank(),
                  # axis.text.y=element_blank(),
                  # axis.ticks.y=element_blank(),
                  # https://stackoverflow.com/questions/46482846/ggplot2-x-axis-extreme-right-tick-label-clipped-after-insetting-legend
                  # stop axis being clipped
                  plot.title=element_text(size = 18), plot.margin = unit(c(5.5,12,5.5,5.5), "pt"),
                  legend.text=element_text(size=14),
                  legend.title=element_text(size=14),
                  legend.position="none",
                  axis.text.x  = element_text(size=15),
                  axis.text.y  = element_text(size=15),
                  axis.line.x = element_line(color="black"),
                  axis.line.y = element_line(color="black"),
                  plot.caption=element_text(hjust = 0, size = 11),
                  strip.text.x = element_text(size = 16, colour = "black", angle = 0),
                  axis.title.y = element_text(size = rel(1.5), angle = 90),
                  axis.title.x = element_text(size = rel(1.5), angle = 0),
                  strip.background = element_rect(colour = "black", fill = "#ececf0"),
                  panel.background = element_rect(fill = '#ececf0', colour = '#ececf0'),
                  plot.background = element_rect(fill = '#ececf0', colour = '#ececf0'),#
                ) 
        )
    
      }) 
      
   output$reg.summary1 <- renderPrint({
      
      summary <- fit.regression.gls0()$fit.res
      
      return(list(summary))
      
    })  
    
    output$reg.summary2 <- renderPrint({
      
      summary <- fit.regression.gls0()$k1a
      
      return(list(summary))
      
    })  
    
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # boxplots of data at which trt effect starts after baseline allwing highlighting of selected patients
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    # --------------------------------------------------------------------------
    # -----------------------------------------------OVERALL PLOT
    # ---------------------------------------------------------------------------
    
    output$reg.plot33 <- renderPlot({ 

      tmp <- make.data2()$tmp
      nbaseline <- make.data2()$nbaseline
      all <- rbind(tmp, nbaseline)
      d <- all  # add this to shown baslein
      d$trt <- d$treat
      d$rep <- d$unit
      d$yij <- d$y
      d$time <- factor(d$time)
      
        if (input$Plot1 == "Overall") {

        
         # lets get counts to put in ribbons
          d$trt <- factor(d$trt)
          dx <- unique(d[,c("rep","trt")])
          table(dx$trt)
          n <- as.vector(table(dx$trt))
          levels(d$trt) <- c(paste0("Active N=",n[1]), paste0("Placebo N=",n[2])) 
          

            #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          pd <- position_dodge(.4)
          pr1=NULL
          pr1 <- ggplot(d,aes(x=time ,y=yij,color=trt, fill=trt ))  + 
            stat_boxplot(geom = "errorbar", width = 0.3) +
            geom_boxplot( outlier.colour = NA  ) +  # removed fill=NA
            geom_line(aes(group=rep), position = pd,  alpha=0.6, linetype="dotted")   + 
            scale_size_manual( values = c( 1) ) +
            geom_point(aes(fill=trt, group=rep), pch=1, size=1, alpha=0.3, position = pd ) +
            stat_summary(fun=mean, geom="point", shape=3, size=2, colour="black", stroke=1.5,
                         position=pd, show.legend=FALSE) +
            scale_color_manual(name = "Treatment", values = c("blue", "darkgreen")) +
            scale_fill_manual(name = "Treatment", values = c("lightblue", "green")) +
            facet_wrap(~trt , ncol=2)    +
            labs(caption = "- The upper whisker is located at the smaller of the maximum y value and Q3 + 1.5xIQR, whereas the lower whisker is located at the larger of the smallest y value and Q1 - 1.5xIQR\n- The median is the horizontal line inside each box and the mean denoted by the cross\n -Individual patient profiles are denoted by dotted lines\n- A small amount of jitter is added to the data to aid visualisation.") +
            
            geom_text(data = d %>% group_by( time, trt) %>%
                        dplyr::summarise(Count = n()) %>%
                        ungroup %>%
                        mutate(yij=min((d$yij)) - 0.05 * diff(range((d$yij)))),
                      aes(label = paste0("n = ", Count)),
                      position = pd, size=3, show.legend = FALSE) 


          print(pr1 + labs(y="Response", x = 'Follow up vists (baseline not shown)') +    
                  ggtitle(paste0("There are N=",
                                 length(unique(d$rep)),  
                                 " patients with data at baseline, presenting all patient profiles, with boxplots and the number of patient values at each visit") ) +
                  theme_bw() +
                  theme(legend.position="none") +
                  theme(#panel.background=element_blank(),
                    # axis.text.y=element_blank(),
                    # axis.ticks.y=element_blank(),
                    # https://stackoverflow.com/questions/46482846/ggplot2-x-axis-extreme-right-tick-label-clipped-after-insetting-legend
                    # stop axis being clipped
                    plot.title=element_text(size = 18), plot.margin = unit(c(5.5,12,5.5,5.5), "pt"),
                    legend.text=element_text(size=14),
                    legend.title=element_text(size=14),
                    legend.position="none",
                    axis.text.x  = element_text(size=15),
                    axis.text.y  = element_text(size=15),
                    axis.line.x = element_line(color="black"),
                    axis.line.y = element_line(color="black"),
                    plot.caption=element_text(hjust = 0, size = 11),
                    strip.text.x = element_text(size = 16, colour = "black", angle = 0),
                    axis.title.y = element_text(size = rel(1.5), angle = 90),
                    axis.title.x = element_text(size = rel(1.5), angle = 0),
                    strip.background = element_rect(colour = "black", fill = "#ececf0"),
                    panel.background = element_rect(fill = '#ececf0', colour = '#ececf0'),
                    plot.background = element_rect(fill = '#ececf0', colour = '#ececf0'),#
                  ) 
          )

            #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Individual profiles
            #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        }  else  if (input$Plot1 == "Individual") {


            i <- as.numeric(unlist(strsplit(input$vec1,",")))

            d$trt <- factor(d$trt)
            dx <- unique(d[,c("rep","trt")])
            table(dx$trt)
            n <- as.vector(table(dx$trt))
            levels(d$trt) <- c(paste0("Active N=",n[1]), paste0("Placebo N=",n[2])) 
            
            
            #  if 999 is entered all subjects are shown

            if("999" %in% i) {

                dd<-d

            } else {

                dd <- d[d$rep %in% i,]
            }

            sel <- unique(dd$trt) #if only one arm , dont show the empty arm
            d <- d[d$trt %in% sel,]

            dx <- unique(dd[,c("rep","trt")])

            nn <- as.vector(table(dx$trt))

            levels(d$trt)  <- levels(dd$trt) <-
                c(paste0("Active N=",n[1], " with " ,nn[1]," patient profile(s) shown"),
                  paste0("Placebo N=",n[2], " with " ,nn[2]," patient profile(s) shown"))



            pd <- position_dodge(.4)
            pr1=NULL
            pr1<-ggplot(d,aes(x=time ,y=yij,color=trt, fill=trt )) +
                stat_boxplot(geom = "errorbar", width = 0.3) +
                geom_boxplot( outlier.colour = NA) +#,alpha=0.1, color="lightblue",)  +
                geom_line(data = dd,
                          aes(group=rep,x = time, y = yij),  size = .6, linetype="dashed") +
                scale_size_manual( values = c( 1) ) +
                geom_point(data=dd, aes(fill=trt, group=rep),
                           pch=19, size=3, colour='black',alpha=0.7, position = pd ) +
                geom_point(aes(fill=trt, group=rep),
                           pch=1, size=2, alpha=0.2, position = pd ) +
                stat_summary(fun.y=mean, geom="point", shape=3, size=2, colour="black", stroke=1.5,
                             position=pd, show.legend=FALSE) +
                scale_color_manual(name = "Treatment", values = c("blue", "darkgreen") ) +
                scale_fill_manual(name = "Treatment", values = c("lightblue", "green") ) +
                facet_wrap(~trt , ncol=2)    +
                labs(caption = "- The upper whisker is located at the smaller of the maximum y value and Q3 + 1.5xIQR, whereas the lower whisker is located at the larger of the smallest y value and Q1 – 1.5xIQR\n- The median is the horizontal line inside each box and the mean denoted by the cross\n -Individual patient profiles are denoted by dotted lines\n- A small amount of jitter is added to the data to aid visualisation.")+

                geom_text(data = dd %>% group_by( time, trt) %>%
                              summarise(Count = n()) %>%
                              ungroup %>%
                              mutate(yij=min((d$yij)) - 0.05 * diff(range((d$yij)))),
                          aes(label = paste0("n = ", Count)),
                          position = pd, size=5, show.legend = FALSE)

            print(pr1 + labs(y="Response", x = 'Visit (0 denotes the baseline visit)') +

                      ggtitle(paste0("There are N=",
                                     length(unique(d$rep)),
                                     " patients with data at baseline, presenting selected patient profiles") ) +
                      theme_bw() +
                      theme(legend.position="none") +
                      theme(#panel.background=element_blank(),
                          # axis.text.y=element_blank(),
                          # axis.ticks.y=element_blank(),
                          # https://stackoverflow.com/questions/46482846/ggplot2-x-axis-extreme-right-tick-label-clipped-after-insetting-legend
                          # stop axis being clipped
                          plot.title=element_text(size = 18), plot.margin = unit(c(5.5,12,5.5,5.5), "pt"),
                          legend.text=element_text(size=14),
                          legend.title=element_text(size=14),
                          legend.position="none",
                          axis.text.x  = element_text(size=15),
                          axis.text.y  = element_text(size=15),
                          axis.line.x = element_line(color="black"),
                          axis.line.y = element_line(color="black"),
                          plot.caption=element_text(hjust = 0, size = 11),
                          strip.text.x = element_text(size = 16, colour = "black", angle = 0),
                          axis.title.y = element_text(size = rel(1.5), angle = 90),
                          axis.title.x = element_text(size = rel(1.5), angle = 0),
                          strip.background = element_rect(colour = "black", fill = "#ececf0"),
                          panel.background = element_rect(fill = '#ececf0', colour = '#ececf0'),
                          plot.background = element_rect(fill = '#ececf0', colour = '#ececf0'),#
                      )
            )

        }   

      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # end of boxplots of data at which trt effect starts after baseline allwing highlighting of selected patients
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
    }) 
    
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Diagnostics using data at which trt effect starts after baseline
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

   output$res.diag  <- renderPlot({

       fit <- fit.regression.base()$fit.res

       d<- make.data2()$d

      # target <- input$Plot
     #  d <- d[d$test %in% target,]
       d2 <- d

       d2$resid <- r <- resid(fit)

       d2$fitted <- fitted(fit)

       yl <- ylab('Residuals')

       xl <- xlab("time")

       p1 <- ggplot(d2 , aes(x=fitted , y=resid)) + geom_point (   colour="#69b3a2") + yl

       p3 <- ggplot(d2 , aes(x=time , y=resid )) +  geom_point ( colour="#69b3a2") + yl  + xl +
           stat_summary(fun.data ="mean_sdl", geom='smooth')

       p4 <- ggplot(d2 , aes(sample=resid )) + stat_qq(colour="#69b3a2") +
           geom_abline(intercept=mean(r), slope=sd(r)  ,  colour="black") +
           xlab('Residuals')   +
           ggtitle( " ")

       # p5 <- d2 %>%
       #   ggplot( aes(x=r)) +
       #   geom_histogram( fill="#69b3a2", color="#e9ecef", alpha=0.9) + #binwidth=1,
       #  theme(
       #     plot.title = element_text(size=15)
       #   )
       library(gridExtra)
       library(grid)
       df <- data.frame(Residuals = r)
       p5 <- ggplot(df, aes(x = Residuals)) +
           geom_histogram(aes(y =..density..),
                          #breaks = seq(-50, 50, by = 2),
                          colour = "black",
                          fill = "#69b3a2") +
           stat_function(fun = dnorm, args = list(mean = 0, sd = sigma(fit)  ))

       grid.arrange(p1,  p3, p4,p5, ncol=2,

                    top = textGrob(paste0(input$Plot, " GLS model fit diagnostics"),gp=gpar(fontsize=20,font=3)))
       #+
       #main=paste0(input$Plot, "GLS model fit diagnostics")  #

   })
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
    # listing of simulated data
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
    output$table1 <- DT::renderDataTable({

        foo<- make.data2()$d
         
        foo$time <- relevel(foo$time , ref=1)
        
        foo <- plyr::arrange(foo, unit, time)

        names(foo) <- c("Patient","Baseline", "Treatment", "Visit","Response", "Country")

        rownames(foo) <- NULL
        

          datatable(foo,

                  rownames = TRUE,

                  options = list(
                      searching = TRUE,
                      pageLength = 16,
                      paging=TRUE,
                      lengthMenu = FALSE ,
                      lengthChange = FALSE,
                      autoWidth = FALSE
                       #colReorder = TRUE,
                    #  deferRender = TRUE
                      #scrollY = 600,
                     # scroller = T
                  )) %>%
    
            formatRound(
               columns= c("Baseline", "Response"), 
                digits=4 )
        
      
        
    })
    # 
    # #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
    # # summary stats
    # #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
    # 
    # output$table2 = DT::renderDataTable({
    #     
    #     foo<- make.data()$d1
    #     
    #     target <- input$Plot
    #     
    #     foo <- foo[foo$test %in% target,]
    #     
    #     f<-plyr::ddply(foo, c("test", "memorypar","tailindex"), summarise,
    #                    min=min(hillest),mean = mean(hillest), sd = sd(hillest, na.rm=TRUE),
    #                    sem = sd(hillest)/sqrt(length(hillest)),  Q1=quantile(hillest, 0.25)    , 
    #                    median=median(hillest),   Q3=quantile(hillest, 0.75)  , max=max(hillest)  )
    #     
    #     names(f) <- c("Biochemistry test",  "Visit", "Treatment","Minimum", "Mean" , "SD", "SE", "Q1","Median","Q3", "Maximum")
    #     
    #     rownames(f) <- NULL
    #     
    #     
    #     library(DT)
    #     datatable(f,   
    #               rownames = TRUE,
    #               options = list(
    #                   searching = TRUE,
    #                   pageLength = input$V-1,
    #                   paging=FALSE,
    #                   lengthMenu = FALSE ,
    #                   lengthChange = FALSE,
    #                   autoWidth = FALSE
    #                   # colReorder = TRUE,
    #                   # deferRender = TRUE,
    #                   # scrollY = 200,
    #                   # scroller = T
    #               ))  %>%
    #         formatRound(
    #             columns= c("Minimum", "Mean" , "SD", "SE", "Q1","Median","Q3", "Maximum"), digits=c(2)  )
    #     
    #     
    # })
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
    # model output
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
    # output$reg.summary <- renderPrint({
    #     
    #     summary <- fit.regression0()$fit.res
    #     
    #     return(list(summary))
    #     
    # })  
    # 
    # output$reg.summaryx <- renderPrint({
    #     
    #     summary <- input$Plot
    #     
    #     return(list(summary))
    #     
    # })  
    # 
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
})

# Run the application 
shinyApp(ui = ui, server = server)