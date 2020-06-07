 
  rm(list = ls())
  set.seed(2345)
  library(mvtnorm) 
  library(rms)

  # total patients
  N <- 600
  J <- 8     # maximum visit of each patient
  
  intercept = 10
  slope= 0
  trt= -1.0 
  interaction = - 0.2
  error <- 0.1
  
  # random effects parametere
  q = 0.5  # standard deviations for the intercept 
  r = 0.9  # random effects correlation of slope internet
  s = .01  # standard deviations for slope

  time.ref = 4  # so we can read this timepoint effect straigh of model outputs
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# set up patients and random treatment assignment
  
  unit.df <- data.frame(unit = c(1:N), treat = sample(0:1,   N, replace=TRUE) )  
  
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  unit.df <-  within(unit.df, {
    E.alpha.given.treat <-  intercept + trt  * treat      # trt is the true treatment effect
    E.beta.given.treat  <-  slope + interaction * treat   # interaction effect
  })
  #
  
  (unit.df)[1:40,]
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # covariance matrix for random effects
  
  cov.matrix <- matrix(c(q^2, r * q * s, r * q * s, s^2), nrow = 2, byrow = TRUE)
  
  random.effects <- rmvnorm(N, mean = c(0, 0), sigma = cov.matrix)
  
  # add random effects
  unit.df$alpha <- unit.df$E.alpha.given.treat + random.effects[, 1]
  unit.df$beta <-  unit.df$E.beta.given.treat  + random.effects[, 2]
  (unit.df)[1:40,]
  
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # structure of experimental design
  # time points

 # M = J * N  # Total number of observations
  #M= sum(p)
  x.grid = seq(0, 8, by = 8/J)[0:8]

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  # within.unit.df <-  data.frame(unit = sort(rep(c(1:N), J)), 
  #                                  j = rep(c(1:J), N), 
  #                               time = rep(x.grid, N))

 # set up unbalanced visits everyone at first visit but randomly end up to max after that
  
   
 
   p <- round(runif(N,2,J))            # last visit for each person
   
   M= sum(p)
  
   unit <- sort(rep(c(1:N), times=p))  # id for each person
  
   j <- as.vector(unlist(tapply(X=unit, INDEX=list( unit), FUN=seq_along))) # count with each person
 
   time <- j-1  # first visit , baseline becomes 0
  
   within.unit.df <- data.frame(cbind(unit, j, time))  # create a data frame
  
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~end unbalanced
 
 # merge design and dataand create response
   
  flat.df = merge(unit.df, within.unit.df)
  
  flat.df <-  within(flat.df, y <-  alpha + time * beta + error * rnorm(n = M))
 
  flat.df$treat <- as.factor(flat.df$treat)
  flat.df$treat <- ifelse(flat.df$treat %in% 1, "Active","Placebo" )
  flat.df[1:60,]
  
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# plot the data
  
  ggplot(flat.df, aes(x=time,y=y,colour=treat)) +  
    geom_line(data =flat.df, aes(x=time, y=y, group=unit) ) +
    theme_bw() +
    theme(legend.position="none") 

  pd <- position_dodge(.4)

  plot1 <-  ggplot(flat.df,   aes (x = time, y = y, group = unit, color = treat)) +
  geom_line() + geom_point() + ylab("response") + xlab("visit") +
    stat_summary(fun=mean,geom="line", colour="black",lwd=1,aes(group=treat ) ) +
    # geom_smooth(method=lm, se=FALSE, fullrange=TRUE )+
    # scale_shape_manual(values=c(3, 16))+ 
    scale_color_manual(values=c('#999999','#E69F00'))+
    theme(legend.position="top") +
    xlim(0, J) +
    scale_x_continuous(breaks=c(0:J)) 
  
  plot1
  
 # plot2 <-  plot1 + geom_text(data = flat.df %>% group_by(treat) %>%
 #              dplyr::summarise(Count = n()) %>%
 #              ungroup %>%
 #              #mutate(y=min((flat.df$y)) - 0.05 * diff(range((flat.df$y)))),
 #              mutate(y=6),
 #            aes(label = paste0("n = ", Count)),
 #            position = pd, size=3, show.legend = FALSE) 
 # 
 #  plot2
 #  
  
  
  
  
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(lme4)
# my.lmer <-  lmer(y ~ time + (1 + time | unit), data = simple.df)
# cat("AIC =", AIC(my.lmer))
 
my.lmer <- NULL
my.lmer <-  lmer(y ~    time * treat + (1 + time | unit), data = flat.df)
summary(my.lmer)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## new is this what we expect
require(rms)

tmp <-flat.df
table(tmp$j, tmp$time)
tmp$j <- as.factor(tmp$j )

ddz <<- datadist(tmp)  # need the double in rshiny environ <<
options(datadist='ddz')

#j works but not time consecutive integer error?
table(tmp$j, tmp$time)
tmp$j <- as.factor(tmp$j )

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

time. <- rep(1:(J))

k1a <- rms::contrast(fit, list(j=time.,  treat = "Active"  ),
                         list(j=time.,  treat = "Placebo" ))

k1a



#---------------------------------------------------------------------------
#---------------------------------------------------------------------------
#---------------------------------------------------------------------------
# Plot the estimated trt effect  including baseline





 

    k1a <- contrast(fit, list(j=time.,  treat = 'Placebo'),
                   list(j=time.,  treat = 'Active'))
    
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
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# work in incorporating baseline.............................................................

nbaseline <- flat.df[(flat.df$time !=0),]  
  
ggplot(nbaseline, aes(x=time,y=y,colour=treat)) +  
  geom_line(data =nbaseline, aes(x=time, y=y, group=unit) ) +
  theme_bw() +
  theme(legend.position="none") 


my.lmer <- NULL
my.lmer <-  lmer(y ~    time * treat + (1 + time | unit), data = nbaseline)
summary(my.lmer)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

baseline <- flat.df[(flat.df$time ==0),]  
baseline$alpha <- intercept + random.effects[, 1]
baseline$beta <-  slope     + random.effects[, 2]
# use tmp for a plot
tmp <- baseline <- within(baseline, y <-  alpha + 0 * beta + error * rnorm(n = N) )

baseline <- baseline[,c("unit","y"   )] 
names(baseline) <- c("unit","baseline"  )  # rename y to baseline

both <- merge (baseline , nbaseline  , all=TRUE)

d <-  both[, c("unit", "baseline", "treat", "time", "y")]
d$time<-factor(d$time)
d$treat<-factor(d$treat)
#d$time<-factor(d$time)

d$time <- relevel(d$time, ref=time.ref)

# just put in random countries so no association
#d$country <-  factor(sort(rep(sample(1:8 ),   N, times=J-1)))   # balanced
d$country <- factor(rep(sample(1:8 , N, replace=T), times=p-1))  # unbalanced

head(d, 3)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 
my.lmer <- NULL
my.lmer <-  lmer(y ~   time * treat + (1 + as.numeric(time) | unit), data = d)
summary(my.lmer)
my.lmer <- NULL
my.lmer <-  lmer(y ~  baseline * time + time * treat + (1 + as.numeric(time) | unit), data = d)
summary(my.lmer)


my.lmer <-  lmer(y ~  country + baseline * time + time * treat + (1 + as.numeric(time) | unit), data = d)
summary(my.lmer)

require(rms)

ddz <<- datadist(d)  # need the double in this environ <<
options(datadist='ddz')


(fit.res <-  
    tryCatch(Gls(y  ~ country + baseline * time + time * treat ,
                 correlation=corSymm(form = ~as.numeric(time) | unit) ,
                 weights=varIdent(form=~1|time),
                 d, x=TRUE,
                 na.action=na.exclude ),  
             error=function(e) e)
) 




fit <-  fit.res

time. <- rep(1:(J-1))

(k1 <- contrast(fit, list(time=time.,  treat = "Placebo" ),
               list(time=time.,  treat =  "Active" )))

(k1 <- contrast(fit, list(time=time.,  treat = "Placebo" ),
               list(time=time.,  treat =  "Active" ),type='average' ))

(k1 <- contrast(fit, list(time=4.,  treat = "Placebo" ),
               list(time=4,  treat =  "Active" ),type='average' ))


# match model output
(k1 <- contrast(fit, list(time=time.,  treat = "Placebo", baseline=0, country=1),
                    list(time=time.,  treat =  "Active", baseline=0, country=1)))

 

(k1 <- contrast(fit, list(time=time.,  treat = "Placebo", baseline=median(d$baseline), country=1),
               list(time=time.,  treat =  "Active",      baseline=median(d$baseline), country=1)))


x <- as.data.frame(k1[c('time', 'Contrast', 'Lower', 'Upper')]) 

namez <- c("Follow-up Visit", "Placebo - Active estimate", "Lower 95%CI","Upper 95%CI")

names(x) <- namez

x

k1a

#---------------------------------------------------------------------------
#---------------------------------------------------------------------------
# Plot the estimated trt effect  
 


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


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#############END#############################################################
d$time <- as.numeric(as.character(d$time ))
ggplot(d, aes(x=time,y=y,colour=treat)) +  
  geom_line(data =d, aes(x=time, y=y, group=unit) ) +
  theme_bw() +
  theme(legend.position="none") 


# showwing no difference at baseline
all <- rbind(tmp, nbaseline)

all$time <- as.numeric(as.character(all$time ))

ggplot(all, aes(x=time,y=y,colour=treat)) +  
  geom_line(data =all, aes(x=time, y=y, group=unit) ) +
  theme_bw() +
  theme(legend.position="none") 

# improve the above plot

ggplot(all,   aes (x = time, y = y, group = unit, color = treat)) +
  geom_line() + geom_point() + ylab("response") + xlab("visit") +
  stat_summary(fun=mean,geom="line", colour="black",lwd=1,aes(group=treat ) ) +
  # geom_smooth(method=lm, se=FALSE, fullrange=TRUE )+
  # scale_shape_manual(values=c(3, 16))+ 
  scale_color_manual(values=c('#999999','#E69F00'))+
  theme(legend.position="top") +
  xlim(0, J) +
  scale_x_continuous(breaks=c(0:J)) 

















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
#   input$Plot
#     
#  d1 <- d[d$test %in% ta




