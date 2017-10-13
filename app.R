library(shiny)
library(boot)
library(circular)

# USER INTERFACE INSTRUCTIONS
ui <- fluidPage(
   
   titlePanel("fullcircle: Online Circumplex Calculation Tools"),
   
   sidebarLayout(
      sidebarPanel(
        fileInput(inputId="file",label="Upload a data file",
                  multiple=FALSE,accept=c("text/csv","text/comma-separated-values,text/plain",".csv")),
        checkboxInput(inputId="header",label="Data file contains header row",value=TRUE),
        radioButtons(inputId="type",label="Select an analysis type",
                     choices=c("Compute the mean profile in a single sample"="profile-single",
                               "Compare the mean profiles of two samples/groups"="profile-groups",
                               "Compute a target measure's profile in a single sample"="target-single",
                               "Compare a target measure's profiles of two samples/groups"="target-groups",
                               "Compare two target measures' profiles in a single sample"="target-measures")),
        radioButtons(inputId="scales",label="Select number and type of circumplex scales",
                     choices=c("Octants (PA, BC, DE, FG, HI, JK, LM, NO)"="octants","Poles (PA, DE, HI, LM)"="poles","Quadrants (BC, FG, JK, NO)"="quadrants")),
        numericInput(inputId="nboot",label="Number of bootstrap replicates",
                     value=2000,min=1,max=NA,step=100),
        tags$hr(),
        conditionalPanel(condition="input.type=='target-single'||input.type=='target-groups'",uiOutput("stv")),
        conditionalPanel(condition="input.type=='profile-groups'||input.type=='target-groups'",uiOutput("gmv")),
        conditionalPanel(condition="input.type=='target-measures'",uiOutput("t1v"),uiOutput("t2v")),
        conditionalPanel(condition="input.scales=='octants'||input.scales=='poles'",uiOutput("pav")),
        conditionalPanel(condition="input.scales=='octants'||input.scales=='quadrants'",uiOutput("bcv")),
        conditionalPanel(condition="input.scales=='octants'||input.scales=='poles'",uiOutput("dev")),
        conditionalPanel(condition="input.scales=='octants'||input.scales=='quadrants'",uiOutput("fgv")),
        conditionalPanel(condition="input.scales=='octants'||input.scales=='poles'",uiOutput("hiv")),
        conditionalPanel(condition="input.scales=='octants'||input.scales=='quadrants'",uiOutput("jkv")),
        conditionalPanel(condition="input.scales=='octants'||input.scales=='poles'",uiOutput("lmv")),
        conditionalPanel(condition="input.scales=='octants'||input.scales=='quadrants'",uiOutput("nov"))
      ),
      
      mainPanel(
         h4("Descriptive Statistics"),
         tableOutput(outputId="descTable"),
         p("Only cases with complete data on all of the above variables are included."),
         tags$hr(),
         h4("Circular Statistics Table"),
         tableOutput(outputId="csTable"),
         p("Circular statistics with parametric 95% confidence interval for Theta_m."),
         tags$hr(),
         h4("Circular Statistics Plot"),
         #plotOutput(outputId="csPlot"),
         p("Circular statistics plots coming soon."),
         tags$hr(),
         h4("Structural Summary Method (SSM) Table"),
         tableOutput(outputId="ssmTable"),
         p("Structural summary parameters with bootstrapped 95% confidence intervals."),
         tags$hr(),
         h4("Structural Summary Method (SSM) Plot"),
         #plotOutput(outputId="ssmPlot"),
         p("Structural summary method plots coming soon.")
      )
   )
)

# SERVER INSTRUCTIONS
server <- function(input, output) {
   
   get.data.all <- reactive({
      inFile <- input$file
      if (is.null(inFile))
         return(NULL)
      data.all <- read.csv(inFile$datapath,header=input$header)
      return(data.all)
   })
   
   get.data.use <- reactive({
      # Return if required information is not selected
      if (is.null(input$file))
         return(NULL)
      if (input$scales=="octants" && (input$pav==""||input$bcv==""||input$dev==""||input$fgv==""||input$hiv==""||input$jkv==""||input$lmv==""||input$nov==""))
         return(NULL)
      if (input$scales=="poles" && (input$pav==""||input$dev==""||input$hiv==""||input$lmv==""))
         return(NULL)
      if (input$scales=="quadrants" && (input$bcv==""||input$fgv==""||input$jkv==""||input$nov==""))
         return(NULL)
      if (input$type=="profile-groups" && input$gmv=="")
         return(NULL)
      if (input$type=="target-single" && input$stv=="")
         return(NULL)
      if (input$type=="target-groups" && (input$stv==""||input$gmv==""))
         return(NULL)
      if (input$type=="target-measures" && (input$t1v==""||input$t2v==""))
         return(NULL)
      # Prepare the necessary variables for each type of analysis
      data.use <- get.data.all()
      if (input$type=="profile-single") {
         if (input$scales=="octants")
            data.use <- data.use[,c(input$pav,input$bcv,input$dev,input$fgv,input$hiv,input$jkv,input$lmv,input$nov)]
         if (input$scales=="poles")
            data.use <- data.use[,c(input$pav,input$dev,input$hiv,input$lmv)]
         if (input$scales=="quadrants")
            data.use <- data.use[,c(input$bcv,input$fgv,input$jkv,input$nov)]
      }
      if (input$type=="profile-groups") {
         if (input$scales=="octants")
            data.use <- data.use[,c(input$gmv,input$pav,input$bcv,input$dev,input$fgv,input$hiv,input$jkv,input$lmv,input$nov)]
         if (input$scales=="poles")
            data.use <- data.use[,c(input$gmv,input$pav,input$dev,input$hiv,input$lmv)]
         if (input$scales=="quadrants")
            data.use <- data.use[,c(input$gmv,input$bcv,input$fgv,input$jkv,input$nov)]
      }
      if (input$type=="target-single") {
         if (input$scales=="octants")
            data.use <- data.use[,c(input$stv,input$pav,input$bcv,input$dev,input$fgv,input$hiv,input$jkv,input$lmv,input$nov)]
         if (input$scales=="poles")
            data.use <- data.use[,c(input$stv,input$pav,input$dev,input$hiv,input$lmv)]
         if (input$scales=="quadrants")
            data.use <- data.use[,c(input$stv,input$bcv,input$fgv,input$jkv,input$nov)]
      }
      if (input$type=="target-groups") {
         if (input$scales=="octants")
            data.use <- data.use[,c(input$gmv,input$stv,input$pav,input$bcv,input$dev,input$fgv,input$hiv,input$jkv,input$lmv,input$nov)]
         if (input$scales=="poles")
            data.use <- data.use[,c(input$gmv,input$stv,input$pav,input$dev,input$hiv,input$lmv)]
         if (input$scales=="quadrants")
            data.use <- data.use[,c(input$gmv,input$stv,input$bcv,input$fgv,input$jkv,input$nov)]
      }
      if (input$type=="target-measures") {
         if (input$scales=="octants")
            data.use <- data.use[,c(input$t1v,input$t2v,input$pav,input$bcv,input$dev,input$fgv,input$hiv,input$jkv,input$lmv,input$nov)]
         if (input$scales=="poles")
            data.use <- data.use[,c(input$t1v,input$t2v,input$pav,input$dev,input$hiv,input$lmv)]
         if (input$scales=="quadrants")
            data.use <- data.use[,c(input$t1v,input$t2v,input$bcv,input$fgv,input$jkv,input$nov)]
      }
      # Remove all cases with missing data on any included variables
      data.use <- data.use[complete.cases(data.use),]
      return(data.use)
   })
   
   # Update variable drop-down lists based on loaded datafile
   output$stv <- renderUI({selectInput(inputId="stv","Select the target variable",choices=c("",names(get.data.all())))})
   output$gmv <- renderUI({selectInput(inputId="gmv","Select the grouping variable",choices=c("",names(get.data.all())))})
   output$t1v <- renderUI({selectInput(inputId="t1v","Select the 1st target variable",choices=c("",names(get.data.all())))})
   output$t2v <- renderUI({selectInput(inputId="t2v","Select the 2nd target variable",choices=c("",names(get.data.all())))})
   output$pav <- renderUI({selectInput(inputId="pav","Select the PA variable",choices=c("",names(get.data.all())))})
   output$bcv <- renderUI({selectInput(inputId="bcv","Select the BC variable",choices=c("",names(get.data.all())))})
   output$dev <- renderUI({selectInput(inputId="dev","Select the DE variable",choices=c("",names(get.data.all())))})
   output$fgv <- renderUI({selectInput(inputId="fgv","Select the FG variable",choices=c("",names(get.data.all())))})
   output$hiv <- renderUI({selectInput(inputId="hiv","Select the HI variable",choices=c("",names(get.data.all())))})
   output$jkv <- renderUI({selectInput(inputId="jkv","Select the JK variable",choices=c("",names(get.data.all())))})
   output$lmv <- renderUI({selectInput(inputId="lmv","Select the LM variable",choices=c("",names(get.data.all())))})
   output$nov <- renderUI({selectInput(inputId="nov","Select the NO variable",choices=c("",names(get.data.all())))})
   
   output$descTable <- renderTable({
      data.use <- get.data.use()
      if (input$type=="profile-single") {
         if (input$scales=="octants") {
            desc <- matrix(NA,5,8)
            colnames(desc) <- c("PA","BC","DE","FG","HI","JK","LM","NO")
         }
         if (input$scales=="poles") {
            desc <- matrix(NA,5,4)
            colnames(desc) <- c("PA","DE","HI","LM")
         }
         if (input$scales=="quadrants") {
            desc <- matrix(NA,5,4)
            colnames(desc) <- c("BC","FG","JK","NO")
         }
      }
      if (input$type=="profile-groups") {
         if (input$scales=="octants") {
            desc <- matrix(NA,5,9)
            colnames(desc) <- c("Group","PA","BC","DE","FG","HI","JK","LM","NO")
         }
         if (input$scales=="poles") {
            desc <- matrix(NA,5,5)
            colnames(desc) <- c("Group","PA","DE","HI","LM")
         }
         if (input$scales=="quadrants") {
            desc <- matrix(NA,5,5)
            colnames(desc) <- c("Group","BC","FG","JK","NO")
         }
      }
      if (input$type=="target-single") {
         if (input$scales=="octants") {
            desc <- matrix(NA,5,9)
            colnames(desc) <- c("Target","PA","BC","DE","FG","HI","JK","LM","NO")
         }
         if (input$scales=="poles") {
            desc <- matrix(NA,5,5)
            colnames(desc) <- c("Target","PA","DE","HI","LM")
         }
         if (input$scales=="quadrants") {
            desc <- matrix(NA,5,5)
            colnames(desc) <- c("Target","BC","FG","JK","NO")
         }
      }
      if (input$type=="target-groups") {
         if (input$scales=="octants") {
            desc <- matrix(NA,5,10)
            colnames(desc) <- c("Group","Target","PA","BC","DE","FG","HI","JK","LM","NO")
         }
         if (input$scales=="poles") {
            desc <- matrix(NA,5,6)
            colnames(desc) <- c("Group","Target","PA","DE","HI","LM")
         }
         if (input$scales=="quadrants") {
            desc <- matrix(NA,5,6)
            colnames(desc) <- c("Group","Target","BC","FG","JK","NO")
         }
      }
      if (input$type=="target-measures") {
         if (input$scales=="octants") {
            desc <- matrix(NA,5,10)
            colnames(desc) <- c("Target 1","Target 2","PA","BC","DE","FG","HI","JK","LM","NO")
         }
         if (input$scales=="poles") {
            desc <- matrix(NA,5,6)
            colnames(desc) <- c("Target 1","Target 2","PA","DE","HI","LM")
         }
         if (input$scales=="quadrants") {
            desc <- matrix(NA,5,6)
            colnames(desc) <- c("Target 1","Target 2","BC","FG","JK","NO")
         }
      }
      if (!is.null(data.use)) {
         desc <- rbind(
           apply(data.use,2,length),
           apply(data.use,2,function(x) round(min(x),2)),
           apply(data.use,2,function(x) round(max(x),2)),
           apply(data.use,2,function(x) round(mean(x),2)),
           apply(data.use,2,function(x) round(sd(x),2))
         )
      }
      rownames(desc) <- c("N","Min","Max","Mean","SD")
      return(desc)
   },rownames=TRUE)
   
   output$csTable <- renderTable({
      data.use <- get.data.use()
      if (input$scales=="octants") {
         theta <- c(90,135,180,225,270,315,360,45)
      }
      if (input$scales=="poles") {
         theta <- c(90,180,270,360)
      }
      if (input$scales=="quadrants") {
         theta <- c(135,225,315,45)
      }
      get.cs <- function(scores,angles) {
         cs <- list()
         k <- ncol(scores)
         n <- nrow(scores)
         x.i <- 2/k * (scores %*% cos(angles * pi/180))
         y.i <- 2/k * (scores %*% sin(angles * pi/180))
         d.i <- (atan2(y.i,x.i) * 180/pi) %% 360
         cs$x <- sum(cos(d.i * pi/180))
         cs$y <- sum(sin(d.i * pi/180))
         cs$theta.m <- (atan2(cs$y,cs$x) * 180/pi) %% 360
         cs$v.theta <- (acos(sum(cos((cs$theta.m - d.i) * pi/180)) / n) * 180/pi) %% 360
         cs$ci.lb <- cs$theta.m - 1.96 * (cs$v.theta / sqrt(n))
         cs$ci.ub <- cs$theta.m + 1.96 * (cs$v.theta / sqrt(n))
         return(cs)
      }
      if (input$type=="profile-single") {
         results <- matrix(NA,1,4)
         rownames(results) <- "Mean"
         colnames(results) <- c("X-Axis","Y-Axis","Theta_m","V_theta")
         if (!is.null(data.use)) {
            cs <- get.cs(as.matrix(data.use),theta)
            results[1] <- round(cs$x,2)
            results[2] <- round(cs$y,2)
            results[3] <- sprintf("%.1f [%.1f, %.1f]",cs$theta.m,cs$ci.lb,cs$ci.ub)
            results[4] <- round(cs$v.theta,2)
            rownames(results) <- "Mean"
         }
      }
      if (input$type=="profile-groups") {
         #TODO
         if (!is.null(data.use)) {
            #TODO
         }
      }
      if (input$type=="target-single") {
         #TODO
         if (!is.null(data.use)) {
            #TODO
         }
      }
      if (input$type=="target-groups") {
         #TODO
         if (!is.null(data.use)) {
            #TODO
         }
      }
      if (input$type=="target-measures") {
         #TODO
         if (!is.null(data.use)) {
            #TODO
         }
      }
      return(results)
   },rownames=TRUE)
   
   output$csPlot <- renderPlot({
      #TODO
   })
   
   output$ssmTable <- renderTable({
      data.use <- get.data.use()
      if (input$scales=="octants") {
         theta <<- c(90,135,180,225,270,315,360,45)
      }
      if (input$scales=="poles") {
         theta <<- c(90,180,270,360)
      }
      if (input$scales=="quadrants") {
         theta <<- c(135,225,315,45)
      }
      wd <- function(w.1, w.2) {return(((w.2 - w.1 + 180) %% 360) - 180)}
      get.ssm <- function(scores,angles) {
         ssm <- list()
         k <- length(scores)
         ssm$e <- mean(scores)
         ssm$x <- 2/k * (scores %*% cos(angles * pi/180))
         ssm$y <- 2/k * (scores %*% sin(angles * pi/180))
         ssm$a <- sqrt(ssm$x^2 + ssm$y^2)
         ssm$d <- atan2(ssm$y,ssm$x) * 180/pi
         sDsq <- sum((as.vector(ssm$e) + as.vector(ssm$a) * cos((angles - as.vector(ssm$d)) * pi/180) - scores)^2)
         sstot <- var(scores) * (k - 1)
         ssm$fit <- 1 - sDsq / sstot
         return(ssm)
      }
      if (input$type=="profile-single") {
         results <- matrix(NA,1,6)
         rownames(results) <- "Mean"
         colnames(results) <- c("Elevation","X-Axis","Y-Axis","Amplitude","Displacement","Fit")
         if (!is.null(data.use)) {
            mScore <- colMeans(data.use)
            ssm <- get.ssm(mScore,theta)
            # Bootstrapping circumplex parameters
            boot.circum <- function(b,d){
               resampl <- b[d,]
               mScore.r <- colMeans(resampl)
               ssm.r <- get.ssm(mScore.r,theta)
               return(c(ssm.r$e,ssm.r$x,ssm.r$y,ssm.r$a,ssm.r$d,ssm.r$fit))
            }
            boot.results <- boot(data.use,boot.circum,input$nboot)
            # Saving bootstrap results
            winkel <- circular(boot.results$t[,5], units = c("degrees"), rotation = c("counter"))
            results[1] <- sprintf("%.2f [%.2f, %.2f]",ssm$e,quantile(boot.results$t[,1],probs=.025),quantile(boot.results$t[,1],probs=.975))
            results[2] <- sprintf("%.2f [%.2f, %.2f]",ssm$x,quantile(boot.results$t[,2],probs=.025),quantile(boot.results$t[,2],probs=.975))
            results[3] <- sprintf("%.2f [%.2f, %.2f]",ssm$y,quantile(boot.results$t[,3],probs=.025),quantile(boot.results$t[,3],probs=.975))
            results[4] <- sprintf("%.2f [%.2f, %.2f]",ssm$a,quantile(boot.results$t[,4],probs=.025),quantile(boot.results$t[,4],probs=.975))
            results[5] <- sprintf("%.1f [%.1f, %.1f]",ssm$d%%360,quantile.circular(winkel,probs=.025)%%360,quantile.circular(winkel,probs=.975)%%360)
            results[6] <- round(ssm$fit,3)
            rownames(results) <- "Mean"
         }
      }
      if (input$type=="profile-groups") {
         #TODO
      }
      if (input$type=="target-single") {
         results <- matrix(NA,1,6)
         rownames(results) <- "Target"
         colnames(results) <- c("Elevation","X-Axis","Y-Axis","Amplitude","Displacement","Fit")
         if (!is.null(data.use)) {
            rmat <- cor(data.use)
            rvec <- rmat[1,2:ncol(rmat)]
            ssm <- get.ssm(rvec,theta)
            # Bootstrapping circumplex parameters
            boot.circum <- function(b,d){
               resampl <- b[d,]
               rmat.r <- cor(resampl)
               rvec.r <- rmat.r[1,2:ncol(rmat.r)]
               ssm.r <- get.ssm(rvec.r,theta)
               return(c(ssm.r$e,ssm.r$x,ssm.r$y,ssm.r$a,ssm.r$d))
            }
            boot.results <- boot(data.use,boot.circum,input$nboot)
            # Saving bootstrap results
            results[1] <- sprintf("%.2f [%.2f, %.2f]",ssm$e,quantile(boot.results$t[,1],probs=.025),quantile(boot.results$t[,1],probs=.975))
            results[2] <- sprintf("%.2f [%.2f, %.2f]",ssm$x,quantile(boot.results$t[,2],probs=.025),quantile(boot.results$t[,2],probs=.975))
            results[3] <- sprintf("%.2f [%.2f, %.2f]",ssm$y,quantile(boot.results$t[,3],probs=.025),quantile(boot.results$t[,3],probs=.975))
            results[4] <- sprintf("%.2f [%.2f, %.2f]",ssm$a,quantile(boot.results$t[,4],probs=.025),quantile(boot.results$t[,4],probs=.975))
            winkel <- circular(boot.results$t[,5], units = c("degrees"), rotation = c("counter"))
            results[5] <- sprintf("%.1f [%.1f, %.1f]",ssm$d%%360,quantile.circular(winkel,probs=.025)%%360,quantile.circular(winkel,probs=.975)%%360)
            results[6] <- round(ssm$fit,3)
            rownames(results) <- input$stv
         }
      }
      if (input$type=="target-groups") {
         results <- matrix(NA,3,6)
         rownames(results) <- c("Group 1","Group 2","Difference")
         colnames(results) <- c("Elevation", "X-Axis", "Y-Axis", "Amplitude", "Displacement","Fit")
         if (!is.null(data.use)) {
            data.use[,1] <- as.factor(data.use[,1])
            if (nlevels(data.use[,1]) != 2)
               return("Error: The number of unique groups must be equal to 2.")
            #Create separate dataframes for each sample
            data.g1 <- subset(data.use[,2:10],data.use[,1]==levels(data.use[,1])[1])
            data.g2 <- subset(data.use[,2:10],data.use[,1]==levels(data.use[,1])[2])
            rmat.g1 <- cor(data.g1)
            rmat.g2 <- cor(data.g2)
            rvec.g1 <- rmat.g1[1,2:ncol(rmat.g1)]
            rvec.g2 <- rmat.g2[1,2:ncol(rmat.g2)]
            #Calculate parameters for sample 1
            ssm.g1 <- get.ssm(rvec.g1,theta)
            #Calculate parameters for sample 2
            ssm.g2 <- get.ssm(rvec.g2,theta)
            #Calculate differences between samples' parameters
            ssm.gd <- list()
            ssm.gd$e <- ssm.g1$e - ssm.g2$e
            ssm.gd$x <- ssm.g1$x - ssm.g2$x
            ssm.gd$y <- ssm.g1$y - ssm.g2$y
            ssm.gd$a <- ssm.g1$a - ssm.g2$a
            ssm.gd$d <- wd(ssm.g1$d, ssm.g2$d)
            ssm.gd$fit <- ssm.g1$fit - ssm.g2$fit
            # Bootstrapping differences in circumplex parameters
            boot.circum <- function(b,d){
               resampl <- b[d,]
               resampl <- split(resampl,resampl[,1])
               rmat.r1 <- cor(resampl[[levels(b[,1])[1]]][,2:ncol(b)])
               rmat.r2 <- cor(resampl[[levels(b[,1])[2]]][,2:ncol(b)])
               rvec.r1 <- rmat.r1[1,2:ncol(rmat.r1)]
               rvec.r2 <- rmat.r2[1,2:ncol(rmat.r2)]
               ssm.r1 <- get.ssm(rvec.r1,theta)
               ssm.r2 <- get.ssm(rvec.r2,theta)
               ssm.rd <- list()
               ssm.rd$e <- ssm.r1$e - ssm.r2$e
               ssm.rd$x <- ssm.r1$x - ssm.r2$x
               ssm.rd$y <- ssm.r1$y - ssm.r2$y
               ssm.rd$a <- ssm.r1$a - ssm.r2$a
               ssm.rd$d <- wd(ssm.r1$d, ssm.r2$d)
               return(c(ssm.r1$e,ssm.r1$x,ssm.r1$y,ssm.r1$a,ssm.r1$d,ssm.r2$e,ssm.r2$x,ssm.r2$y,ssm.r2$a,ssm.r2$d,ssm.rd$e,ssm.rd$x,ssm.rd$y,ssm.rd$a,ssm.rd$d))
            }
            boot.results <- boot(data.use,boot.circum,input$nboot,strata=data.use[,1])
            #Saving bootstrap results
            results[1,1] <- sprintf("%.2f [%.2f, %.2f]",ssm.g1$e,quantile(boot.results$t[,1],probs=.025),quantile(boot.results$t[,1],probs=.975))
            results[1,2] <- sprintf("%.2f [%.2f, %.2f]",ssm.g1$x,quantile(boot.results$t[,2],probs=.025),quantile(boot.results$t[,2],probs=.975))
            results[1,3] <- sprintf("%.2f [%.2f, %.2f]",ssm.g1$y,quantile(boot.results$t[,3],probs=.025),quantile(boot.results$t[,3],probs=.975))
            results[1,4] <- sprintf("%.2f [%.2f, %.2f]",ssm.g1$a,quantile(boot.results$t[,4],probs=.025),quantile(boot.results$t[,4],probs=.975))
            winkel <- circular(boot.results$t[,5], units = c("degrees"), rotation = c("counter"))
            results[1,5] <- sprintf("%.1f [%.1f, %.1f]",ssm.g1$d%%360,quantile.circular(winkel,probs=.025)%%360,quantile.circular(winkel,probs=.975)%%360)
            results[1,6] <- round(ssm.g1$fit,3)
            results[2,1] <- sprintf("%.2f [%.2f, %.2f]",ssm.g2$e,quantile(boot.results$t[,6],probs=.025),quantile(boot.results$t[,6],probs=.975))
            results[2,2] <- sprintf("%.2f [%.2f, %.2f]",ssm.g2$x,quantile(boot.results$t[,7],probs=.025),quantile(boot.results$t[,7],probs=.975))
            results[2,3] <- sprintf("%.2f [%.2f, %.2f]",ssm.g2$y,quantile(boot.results$t[,8],probs=.025),quantile(boot.results$t[,8],probs=.975))
            results[2,4] <- sprintf("%.2f [%.2f, %.2f]",ssm.g2$a,quantile(boot.results$t[,9],probs=.025),quantile(boot.results$t[,9],probs=.975))
            winkel <- circular(boot.results$t[,10], units = c("degrees"), rotation = c("counter"))
            results[2,5] <- sprintf("%.1f [%.1f, %.1f]",ssm.g2$d%%360,quantile.circular(winkel,probs=.025)%%360,quantile.circular(winkel,probs=.975)%%360)
            results[2,6] <- round(ssm.g2$fit,3)
            results[3,1] <- sprintf("%.2f [%.2f, %.2f]",ssm.gd$e,quantile(boot.results$t[,11],probs=.025),quantile(boot.results$t[,11],probs=.975))
            results[3,2] <- sprintf("%.2f [%.2f, %.2f]",ssm.gd$x,quantile(boot.results$t[,12],probs=.025),quantile(boot.results$t[,12],probs=.975))
            results[3,3] <- sprintf("%.2f [%.2f, %.2f]",ssm.gd$y,quantile(boot.results$t[,13],probs=.025),quantile(boot.results$t[,13],probs=.975))
            results[3,4] <- sprintf("%.2f [%.2f, %.2f]",ssm.gd$a,quantile(boot.results$t[,14],probs=.025),quantile(boot.results$t[,14],probs=.975))
            winkel <- circular(boot.results$t[,15], units = c("degrees"), rotation = c("counter"))
            results[3,5] <- sprintf("%.1f [%.1f, %.1f]",ssm.gd$d%%360,quantile.circular(winkel,probs=.025)%%360,quantile.circular(winkel,probs=.975)%%360)
            results[3,6] <- round(ssm.gd$fit,3)
            rownames(results) <- c(sprintf("%s=%s",input$gmv,levels(data.use[,1])[1]),sprintf("%s=%s",input$gmv,levels(data.use[,1])[2]),"Difference")
         }
      }
      if (input$type=="target-measures") {
         results <- matrix(NA,3,6)
         rownames(results) <- c("Target 1","Target 2","Difference")
         colnames(results) <- c("Elevation","X-Axis","Y-Axis","Amplitude","Displacement","Fit")
         if (!is.null(data.use)) {
            rmat <- cor(data.use)
            rvec.m1 <- rmat[1,3:ncol(rmat)]
            rvec.m2 <- rmat[2,3:ncol(rmat)]
            #Calculate parameters for measure 1
            ssm.m1 <- get.ssm(rvec.m1,theta)
            #Calculate parameters for measure 2
            ssm.m2 <- get.ssm(rvec.m2,theta)
            #Calculate differences between samples' parameters
            ssm.md <- list()
            ssm.md$e <- ssm.m1$e - ssm.m2$e
            ssm.md$x <- ssm.m1$x - ssm.m2$x
            ssm.md$y <- ssm.m1$y - ssm.m2$y
            ssm.md$a <- ssm.m1$a - ssm.m2$a
            ssm.md$d <- wd(ssm.m1$d, ssm.m2$d)
            ssm.md$fit <- ssm.m1$fit - ssm.m2$fit
            # Bootstrapping circumplex parameters
            boot.circum <- function(b,d){
               resampl <- b[d,]
               rmat <- cor(resampl)
               rvec.r1 <- rmat[1,3:ncol(rmat)]
               rvec.r2 <- rmat[2,3:ncol(rmat)]
               ssm.r1 <- get.ssm(rvec.r1,theta)
               ssm.r2 <- get.ssm(rvec.r2,theta)
               ssm.rd <- list()
               ssm.rd$e <- ssm.r1$e - ssm.r2$e
               ssm.rd$x <- ssm.r1$x - ssm.r2$x
               ssm.rd$y <- ssm.r1$y - ssm.r2$y
               ssm.rd$a <- ssm.r1$a - ssm.r2$a
               ssm.rd$d <- wd(ssm.r1$d, ssm.r2$d)
               return(c(ssm.r1$e,ssm.r1$x,ssm.r1$y,ssm.r1$a,ssm.r1$d,ssm.r2$e,ssm.r2$x,ssm.r2$y,ssm.r2$a,ssm.r2$d,ssm.rd$e,ssm.rd$x,ssm.rd$y,ssm.rd$a,ssm.rd$d))
            }
            boot.results <- boot(data.use,boot.circum,input$nboot)
            # Saving bootstrap results
            results <- matrix(NA,3,6)
            results[1,1] <- sprintf("%.2f [%.2f, %.2f]",ssm.m1$e,quantile(boot.results$t[,1],probs=.025),quantile(boot.results$t[,1],probs=.975))
            results[1,2] <- sprintf("%.2f [%.2f, %.2f]",ssm.m1$x,quantile(boot.results$t[,2],probs=.025),quantile(boot.results$t[,2],probs=.975))
            results[1,3] <- sprintf("%.2f [%.2f, %.2f]",ssm.m1$y,quantile(boot.results$t[,3],probs=.025),quantile(boot.results$t[,3],probs=.975))
            results[1,4] <- sprintf("%.2f [%.2f, %.2f]",ssm.m1$a,quantile(boot.results$t[,4],probs=.025),quantile(boot.results$t[,4],probs=.975))
            winkel <- circular(boot.results$t[,5], units = c("degrees"), rotation = c("counter"))
            results[1,5] <- sprintf("%.1f [%.1f, %.1f]",ssm.m1$d%%360,quantile.circular(winkel,probs=.025)%%360,quantile.circular(winkel,probs=.975)%%360)
            results[1,6] <- round(ssm.m1$fit,3)
            results[2,1] <- sprintf("%.2f [%.2f, %.2f]",ssm.m2$e,quantile(boot.results$t[,6],probs=.025),quantile(boot.results$t[,6],probs=.975))
            results[2,2] <- sprintf("%.2f [%.2f, %.2f]",ssm.m2$x,quantile(boot.results$t[,7],probs=.025),quantile(boot.results$t[,7],probs=.975))
            results[2,3] <- sprintf("%.2f [%.2f, %.2f]",ssm.m2$y,quantile(boot.results$t[,8],probs=.025),quantile(boot.results$t[,8],probs=.975))
            results[2,4] <- sprintf("%.2f [%.2f, %.2f]",ssm.m2$a,quantile(boot.results$t[,9],probs=.025),quantile(boot.results$t[,9],probs=.975))
            winkel <- circular(boot.results$t[,10], units = c("degrees"), rotation = c("counter"))
            results[2,5] <- sprintf("%.1f [%.1f, %.1f]",ssm.m2$d%%360,quantile.circular(winkel,probs=.025)%%360,quantile.circular(winkel,probs=.975)%%360)
            results[2,6] <- round(ssm.m2$fit,3)
            results[3,1] <- sprintf("%.2f [%.2f, %.2f]",ssm.md$e,quantile(boot.results$t[,11],probs=.025),quantile(boot.results$t[,11],probs=.975))
            results[3,2] <- sprintf("%.2f [%.2f, %.2f]",ssm.md$x,quantile(boot.results$t[,12],probs=.025),quantile(boot.results$t[,12],probs=.975))
            results[3,3] <- sprintf("%.2f [%.2f, %.2f]",ssm.md$y,quantile(boot.results$t[,13],probs=.025),quantile(boot.results$t[,13],probs=.975))
            results[3,4] <- sprintf("%.2f [%.2f, %.2f]",ssm.md$a,quantile(boot.results$t[,14],probs=.025),quantile(boot.results$t[,14],probs=.975))
            winkel <- circular(boot.results$t[,15], units = c("degrees"), rotation = c("counter"))
            results[3,5] <- sprintf("%.1f [%.1f, %.1f]",ssm.md$d%%360,quantile.circular(winkel,probs=.025)%%360,quantile.circular(winkel,probs=.975)%%360)
            results[3,6] <- round(ssm.md$fit,3)
            rownames(results) <- c(input$t1v,input$t2v,"Difference")
            colnames(results) <- c("Elevation","X-Axis","Y-Axis","Amplitude","Displacement","Fit")
         }
      }
      return(results)
   },rownames=TRUE)
   
   output$ssmPlot <- renderPlot({
      #TODO
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

