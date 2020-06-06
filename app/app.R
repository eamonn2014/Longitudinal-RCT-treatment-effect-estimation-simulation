#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Rshiny ideas from on https://gallery.shinyapps.io/multi_regression/
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
set.seed(2345)
library(mvtnorm) 
library(rms)
library(ggplot2)
library(shiny) 
library(nlme)
library(MASS)
require(tidyverse)
library(shinyWidgets)

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
                h3("Presenting the results of diagnostic tests routinely ordered to determine general health status"),
                shinyUI(pageWithSidebar(
                    headerPanel(" "),
                    
                    sidebarPanel( 
                        div(p("xxxxxxxxxxx")),
                        
                        div(
                            
                            actionButton(inputId='ab1', label="R code",   icon = icon("th"), 
                                         onclick ="window.open('https://raw.githubusercontent.com/eamonn2014/biochemistry-and-haematology/master/heam_biochem/app.R', '_blank')"),   
                            actionButton("resample", "Simulate a new sample"),
                            br(), br(),
                            
                            div(strong("Select the parameters using the sliders below"),p(" ")),
                            
                            div(("  
                           xxxxxxxxxxxxxxxxxx ")),
                            br(),
                            # selectInput("Plot",
                            #             strong("1. Select which biochemistry test to present"),
                            #             choices=biochemistry),

                            selectInput("Plot1",
                                        strong("2. Select plot"),
                                        choices=c("Overall","Individual","Individual all tests")),
                            
                            
                            textInput('vec1', 
                                      strong("3. Select patient. If '2 select plot' 'Individual' is selected, enter sample ID(s) (comma delimited); 
                                      enter 999 to show all profiles; If 'Individual all tests' is selected, all test results for the first ID only are presented"), "1,2,3,4"),
                            
                            
                            
                            #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                            
                            sliderInput("N",
                                        "No of subjects",
                                        min=2, max=500, step=1, value=600, ticks=FALSE),
                            
                            sliderInput("beta0", "Average intercept", 
                                        min = -10, max = 10, step=0.5, value = c(10), ticks=FALSE) ,
                            
                            sliderInput("beta1", "Average slope",
                                        min = -5, max =5, step=.5, value = c(0),ticks=FALSE),
                            
                            sliderInput("sigma", "True error SD", #    
                                        min =.01, max = 10, value = c(.1), step=.1, ticks=FALSE),
                            
                            sliderInput("q", "True intercept SD", #   
                                        min = .1, max = 10, value = c(.5), step=.5, ticks=FALSE),
                            
                            sliderInput("s", "True slope SD", #    
                                        min = .01, max = 10, value = c(.01),step=.01,  ticks=FALSE),
                            
                            sliderInput("r", "True intercept slope correlation", #   
                                        min = -1, max = 1, value = c(.9), step=0.05, ticks=FALSE),
                            
                           sliderInput("J",
                                        strong("4. Maximum visit number in data simulation including baseline"),
                                        min=3, max=10, step=1, value=8, ticks=FALSE),
                            
                            sliderInput("time.ref",
                                        strong( "5. Estimate treatment effect at this visit"),
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
                            tabPanel("Plotting the data", 
                                     #    h2("Plotting the data"),
                                     div(plotOutput("reg.plot1", width=fig.width, height=fig.height)),  
                                     
                                     h3(" "),
                                     
                                     p(strong(
                                         "xxxxxxxxxxx")) ,
                                     
                                     #                     
                                     p(strong("Txxxxxxxxxxxxxxxxx
                           ")),
                                     
                                     
                            ) ,
                            #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                            tabPanel("Summary statistics", value=3, 
                                     #  div( verbatimTextOutput("table2")),
                                     h4("xxxxxxxxxxxxxxxxxxx"),#
                                     h6("xxxxxxxxxxxxxxxx"),
                                   #  DT::dataTableOutput("table2"),
                                     #h6("This is superior to a plain rtf output in that this can be sorted and filtered on the fly."),
                                     # tags$head(tags$style("#dummy table {background-color: red; }", media="screen", type="text/css")),
                                     
                            ) ,
                            #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                            tabPanel("Statistical modelling", value=6, 
                                     h4("Modelling"),
                                     p(strong("xxxxxxxxxxxxxxx")),
                                   #  div(class="span7", verbatimTextOutput("reg.summaryx")),
                                  #   div(class="span7", verbatimTextOutput("table4")),
                                    # div(class="span7", verbatimTextOutput("reg.summary2")),
                            ) ,
                            
                            #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                            tabPanel("Plot of the treatment effect estimates", 
                                     #  h4("Plot of the treatment effect estimates"),
                                    # div(plotOutput("reg.plote", width=fig.width, height=fig.height2)),  
                                    # div(DT::dataTableOutput("reg.summary4"), style = "font-size: 110%; width: 75%")
                                     
                                     
                            ) ,
                            #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                            
                            #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                            
                            #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                            tabPanel("Diagnostics",
                                     h4("Four residual plots to check for absence of trends in central tendency and in variability"),
                                   #  div(plotOutput("res.plot", width=fig.width, height=fig.height)),       
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
                                    # DT::dataTableOutput("table1"),
                                     
                                     
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
        time.ref <-  input$time.ref
        
        return(list( N=N,  beta0=beta0, beta1=beta1, sigma=sigma, q=q, s=s, r=r, J=J, time.ref=time.ref )) 
  
        
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     })
    
    
    make.data <- reactive({
        
        sample <- random.sample()
        
      
        N        <-  600
        beta0    <-  10
        beta1    <-  0
        sigma    <-  0.1
        q        <-  .5 # standard deviations for the intercept 
        s        <-  .01 # standard deviations for slope
        r        <-  .05 # random effects correlation of slope internet
        J        <-  8
        time.ref <-  4
        
        N        <-  sample$N 
        beta0    <-  sample$beta0 
        beta1    <-  sample$beta1
        sigma    <-  sample$sigma
        q        <-  sample$q # standard deviations for the intercept 
        s        <-  sample$s # standard deviations for slope
        r        <-  sample$r # random effects correlation of slope internet
        J        <-  sample$J
        time.ref <-  sample$time.ref
        
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
        # structure of experimental design
        # time points
        
        #M = J * N  # Total number of observations  <- PROBLEM HERE
        x.grid = seq(0, 8, by = 8/J)[0:8]
        
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # set up unbalanced visits everyone at first visit but randomly end up to max after that
        
        p <- round(runif(N,2,J))                                                 # last visit for each person
        
        M= sum(p)                                                                #  Total number of observations 
        
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
        
        return(list(  flat.df = flat.df) )
        
    }) 
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    #---------------------------------------------------------------------------
    #---------------------------------------------------------------------------
    #---------------------------------------------------------------------------
    # Plot the estimated trt effect  
    
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
    
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    fit.regression <- reactive({
        
        d <- make.data()$d1
        
        target <- input$Plot
        
        d <- d[d$test %in% target,]
        
        d$time <- d$memorypar
        
        require(rms)
        
        d$time <- relevel(d$time, ref=input$VV)
        
        d$trt <- factor(d$tailindex) 
        
        d$yij <- d$hillest
        
        # new~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        #convert time=1 to baseline var
        d <- d[, c("test", "rep", "time", "trt", "yij")]
        d$id=1:nrow(d)  # add index
        baseline <- d[d$time %in% 0,] # create baseline removing baseline
        names(baseline) <-  c("test", "rep", "time", "trt", "baseline", "id")
        baseline$id <- NULL
        baseline$time <- NULL
        d2 <- d[!d$time ==0,]         # create follow up
        both <- merge (baseline , d2   , all=TRUE)
        both$rep <- as.numeric(as.character(both$rep))
        both <- plyr::arrange(both, rep, time)
        both$time <- as.numeric(as.character(both$time))
        d <- both
        d$time<-factor(d$time)
        d$time <- relevel(d$time, ref=input$VV)
        z<-d
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        
        ddz <<- datadist(d)  # need the double in this environ <<
        
        options(datadist='ddz')
        
        
        fit.res <-  
            tryCatch(Gls(yij  ~ baseline+ time * trt ,
                         correlation=corSymm(form=~ as.numeric(time)|rep),
                         weights=varIdent(form=~1|time),
                         d, x=TRUE,
                         na.action=na.exclude ), 
                     error=function(e) e)
        
        return(list(fit.res=fit.res , target = target, z=z ))
    })     
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # treatment effect estimate
    output$reg.summary4 = DT::renderDataTable({
        
        f <- fit.regression()
        
        fit <- f$fit.res
        
        time. <- rep(1:(input$V-1))
        
        k1 <- contrast(fit, list(time=time.,  trt = 'Placebo'),
                       list(time=time.,  trt = 'Active'))
        
        
        x <- as.data.frame(k1[c('time', 'Contrast', 'Lower', 'Upper')]) 
        
        namez <- c("Follow-up Visit", "Placebo - Active estimate", "Lower 95%CI","Upper 95%CI")
        
        names(x) <- namez
        
        library(DT)
        
        #https://datatables.net/reference/option/
        datatable(x,   
                  
                  rownames = FALSE,
                  
                  options = list(
                      searching = FALSE,
                      pageLength = input$V-1,
                      paging=FALSE,
                      lengthMenu = FALSE ,
                      lengthChange = FALSE,
                      autoWidth = TRUE
                      # colReorder = TRUE,
                      # deferRender = TRUE,
                      # scrollY = 200,
                      # scroller = T
                  ))  %>%
            formatRound(
                columns=c(namez), digits=c(0,2,2,2)  )
        
        
    })     
    
  
    # --------------------------------------------------------------------------
    # -----------------------------------------------OVERALL PLOT
    # ---------------------------------------------------------------------------
    
    output$reg.plot3 <- renderPlot({         
        
        d <- make.data()$d1
        
        if (input$Plot1 == "Overall") {
            
            target <- input$Plot
            
            d <- d[d$test %in% target,]
            
            # lets get counts to put in ribbons
            d$tailindex <- factor(d$tailindex)
            dx <- unique(d[,c("rep","tailindex")])
            table(dx$tailindex)
            n <- as.vector(table(dx$tailindex))
            levels(d$tailindex) <- c(paste0("Active N=",n[1]), paste0("Placebo N=",n[2])) 
            
            #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            pd <- position_dodge(.4)
            pr1=NULL
            pr1 <- ggplot(d,aes(x=memorypar ,y=hillest,color=tailindex, fill=tailindex ))  + 
                stat_boxplot(geom = "errorbar", width = 0.3) +
                geom_boxplot( outlier.colour = NA  ) +  # removed fill=NA
                geom_line(aes(group=rep), position = pd,  alpha=0.6, linetype="dotted")   + 
                scale_size_manual( values = c( 1) ) +
                geom_point(aes(fill=tailindex, group=rep), pch=1, size=1, alpha=0.3, position = pd ) +
                stat_summary(fun.y=mean, geom="point", shape=3, size=2, colour="black", stroke=1.5,
                             position=pd, show.legend=FALSE) +
                scale_color_manual(name = "Treatment", values = c("blue", "darkgreen")) +
                scale_fill_manual(name = "Treatment", values = c("lightblue", "green")) +
                facet_wrap(~tailindex , ncol=2)    +
                labs(caption = "- The upper whisker is located at the smaller of the maximum y value and Q3 + 1.5xIQR, whereas the lower whisker is located at the larger of the smallest y value and Q1 – 1.5xIQR\n- The median is the horizontal line inside each box and the mean denoted by the cross\n -Individual patient profiles are denoted by dotted lines\n- A small amount of jitter is added to the data to aid visualisation.") +
                
                geom_text(data = d %>% group_by( memorypar, tailindex) %>%
                              summarise(Count = n()) %>%
                              ungroup %>%
                              mutate(hillest=min((d$hillest)) - 0.05 * diff(range((d$hillest)))),
                          aes(label = paste0("n = ", Count)),
                          position = pd, size=3, show.legend = FALSE) 
            
            
            print(pr1 + labs(y=target, x = 'Visit (0 denotes the baseline visit)') +    
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
            
            target <- input$Plot
            
            d <- d[d$test %in% target,]
            
            d$tailindex <- factor(d$tailindex)
            dx <- unique(d[,c("rep","tailindex")])
            table(dx$tailindex)
            n <- as.vector(table(dx$tailindex))
            levels(d$tailindex) <- c(paste0("Active N=",n[1]), paste0("Placebo N=",n[2])) 
            #  if 999 is entered all subjects are shown
            
            if("999" %in% i) {
                
                dd<-d
                
            } else {
                
                dd <- d[d$rep %in% i,]
            }
            
            sel <- unique(dd$tailindex) #if only one arm , dont show the empty arm
            d <- d[d$tailindex %in% sel,]
            
            dx <- unique(dd[,c("rep","tailindex")])
            
            nn <- as.vector(table(dx$tailindex))
            
            levels(d$tailindex)  <- levels(dd$tailindex) <- 
                c(paste0("Active N=",n[1], " with " ,nn[1]," patient profile(s) shown"), 
                  paste0("Placebo N=",n[2], " with " ,nn[2]," patient profile(s) shown"))
            
            
            
            pd <- position_dodge(.4)
            pr1=NULL
            pr1<-ggplot(d,aes(x=memorypar ,y=hillest,color=tailindex, fill=tailindex )) + 
                stat_boxplot(geom = "errorbar", width = 0.3) +
                geom_boxplot( outlier.colour = NA) +#,alpha=0.1, color="lightblue",)  +  
                geom_line(data = dd,
                          aes(group=rep,x = memorypar, y = hillest),  size = .6, linetype="dashed") +
                scale_size_manual( values = c( 1) ) +
                geom_point(data=dd, aes(fill=tailindex, group=rep), 
                           pch=19, size=3, colour='black',alpha=0.7, position = pd ) +
                geom_point(aes(fill=tailindex, group=rep), 
                           pch=1, size=2, alpha=0.2, position = pd ) +
                stat_summary(fun.y=mean, geom="point", shape=3, size=2, colour="black", stroke=1.5,
                             position=pd, show.legend=FALSE) +
                scale_color_manual(name = "Treatment", values = c("blue", "darkgreen") ) +
                scale_fill_manual(name = "Treatment", values = c("lightblue", "green") ) +
                facet_wrap(~tailindex , ncol=2)    +
                labs(caption = "- The upper whisker is located at the smaller of the maximum y value and Q3 + 1.5xIQR, whereas the lower whisker is located at the larger of the smallest y value and Q1 – 1.5xIQR\n- The median is the horizontal line inside each box and the mean denoted by the cross\n -Individual patient profiles are denoted by dotted lines\n- A small amount of jitter is added to the data to aid visualisation.")+
                
                geom_text(data = dd %>% group_by( memorypar, tailindex) %>%
                              summarise(Count = n()) %>%
                              ungroup %>%
                              mutate(hillest=min((d$hillest)) - 0.05 * diff(range((d$hillest)))),
                          aes(label = paste0("n = ", Count)),
                          position = pd, size=5, show.legend = FALSE) 
            
            print(pr1 + labs(y=target, x = 'Visit (0 denotes the baseline visit)') + 
                      
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
            
        }   else  if (input$Plot1 ==  "Individual all tests") {
            
            
            i <- as.numeric(unlist(strsplit(input$vec1,",")))
            
            
            if(!isTruthy(input$vec1)){
                
                plot(0, xaxt = 'n', yaxt = 'n', bty = 'n', pch = '', ylab = '', xlab = '')
                title(toupper("The option '3. Select patient' is empty. Please enter a patient id in option 3 on the left, thank you!"), col.main = "red")
                
            } else  {  
                
                dd <- d[d$rep %in% i[1],]
                
                sel <- unique(dd$tailindex)  
                
                d <- dd[dd$tailindex %in% sel,]
                
                library(lattice)
                
                lattice.options(panel.error=NULL)
                
                colnames(d)[colnames(d) %in% "hillest"] <- "value"
                
                colnames(d)[colnames(d) %in% "memorypar"] <- "Visit"
                
                xy <<- xyplot(value ~ Visit | test,
                              main=paste0( input$Plot ,"; all observed results for patient ", i[1]," allocated to ",sel,""), 
                              par.settings=list(par.main.text=list(cex=2)),
                              par.strip.text=list(cex=.7),
                              group = test, data = d,
                              xlab="Visit (0 denotes the baseline visit)",
                              type = c("p" ,"l"),  scales = "free") 
                
                print(xy)
                
                
            }
            
            
        }
        
        
        
        
    })
    
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # diagnostics
    
    output$res.plot  <- renderPlot({       
        
        f <- fit.regression()
        fit <- f$fit.res
        
        d <- f$z
        
        target <- input$Plot
        d <- d[d$test %in% target,]
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
        
        foo<- make.data()$d1
        
        target <- input$Plot
        
        foo <- foo[foo$test %in% target,]
        
        foo$eij <- NULL 
        
        names(foo) <- c("Biochemistry test", "ID", "Visit", "Treatment","Response")
        
        rownames(foo) <- NULL
        library(DT)
        
        datatable(foo,   
                  
                  rownames = TRUE,
                  
                  options = list(
                      searching = TRUE,
                      pageLength = input$V-1,
                      paging=FALSE,
                      lengthMenu = FALSE ,
                      lengthChange = FALSE,
                      autoWidth = FALSE
                      # colReorder = TRUE,
                      # deferRender = TRUE,
                      # scrollY = 200,
                      # scroller = T
                  ))  %>%
            formatRound(
                columns= c("Biochemistry test", "ID", "Visit", "Treatment","Response"), digits=c(0,0,0,0,4)  )
    })
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
    # summary stats
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
    
    output$table2 = DT::renderDataTable({
        
        foo<- make.data()$d1
        
        target <- input$Plot
        
        foo <- foo[foo$test %in% target,]
        
        f<-plyr::ddply(foo, c("test", "memorypar","tailindex"), summarise,
                       min=min(hillest),mean = mean(hillest), sd = sd(hillest, na.rm=TRUE),
                       sem = sd(hillest)/sqrt(length(hillest)),  Q1=quantile(hillest, 0.25)    , 
                       median=median(hillest),   Q3=quantile(hillest, 0.75)  , max=max(hillest)  )
        
        names(f) <- c("Biochemistry test",  "Visit", "Treatment","Minimum", "Mean" , "SD", "SE", "Q1","Median","Q3", "Maximum")
        
        rownames(f) <- NULL
        
        
        library(DT)
        datatable(f,   
                  rownames = TRUE,
                  options = list(
                      searching = TRUE,
                      pageLength = input$V-1,
                      paging=FALSE,
                      lengthMenu = FALSE ,
                      lengthChange = FALSE,
                      autoWidth = FALSE
                      # colReorder = TRUE,
                      # deferRender = TRUE,
                      # scrollY = 200,
                      # scroller = T
                  ))  %>%
            formatRound(
                columns= c("Minimum", "Mean" , "SD", "SE", "Q1","Median","Q3", "Maximum"), digits=c(2)  )
        
        
    })
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
    # model output
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
    output$reg.summary2 <- renderPrint({
        
        summary <- fit.regression()$fit.res
        
        return(list(summary))
        
    })  
    
    output$reg.summaryx <- renderPrint({
        
        summary <- input$Plot
        
        return(list(summary))
        
    })  
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
})

# Run the application 
shinyApp(ui = ui, server = server)