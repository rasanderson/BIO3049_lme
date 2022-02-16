## a shiny app for loading data for a SEM
library(shiny)
library(ggplot2)
library(piecewiseSEM)
library(nlme)

sweden<-read.csv("sweden.csv", header=TRUE, sep=",")



##### the server bit
ui <- fluidPage(
    
    # Application title
    titlePanel("Exploring top predator hunting statistics"),
    # choice of output
    p("This creates Linear Mixed Effect Models for the Swedish hunting data ~ pic your variables to analyse and then investigate the trends indifferent variables"),
    radioButtons("bins1"," Analysis to undertake ",choices=c("Time_series", "Mixed Effect Models"), selected=c("Time_series")),
    
    
    # select the variable to plot
    radioButtons("bins2","Pick one variable to plot/model ",choices=c("Seed_sown","Sheep","Human_population", "Top_predators_killed","Red_fox_killed"), selected=c("Seed_sown")),
    
    # Show a plot of the data and fitted model by county
    mainPanel(plotOutput("distPlot"))
    #  , dataTableOutput('table')
    #, tableOutput("table")      
    , verbatimTextOutput("table")
    
) # end of fluidpage



#############  this is for running the model

server <- function(input, output) {
    
    plot_info <- reactive({
        choice1<-input$bins1
        choice2<-input$bins2
        # choice1<-c("Time_series")
        # choice2<-c("Mixed Effect Models")
        cat("choice 1 =",choice1, " choice 2 ", choice2,"\n")
        
        if(choice1=="Time_series")
        {
            df.dat<-cbind(sweden$county, sweden$time, sweden[,choice2])
            df.dat<-data.frame(df.dat)
            
            colnames(df.dat)<-c("County","time","Counts")
            
            test.gls<-gls(log(Counts+1)~time, data=df.dat)
            fitted<-exp(fitted(test.gls))-1
            myplot <- ggplot(df.dat, mapping=aes(x=time))+
                geom_line(aes(y=`Counts`),color="blue")+
                geom_line(aes(y=`fitted`, color="red"))+
                labs(y="Numbers", title=`choice2`)+
                theme(legend.position="none")+
                facet_wrap(~County)
            return(myplot)
        }
        else
        {
            df.dat<-cbind(sweden$county, sweden$time, sweden[,choice2])
            df.dat<-data.frame(df.dat)
            
            colnames(df.dat)<-c("County","time","Counts")
            df.grp <- groupedData(Counts ~ time | County, data=df.dat)
            test.gls<-lme(log(Counts+1)~time, random=~time|County,df.grp)
            print(test.gls$call)
            print(summary(test.gls))
            print(summary(df.grp))
            #fitted2 <- exp(augPred(test.gls)+1)
            fitted<-exp(fitted(test.gls))-1
            print(ls())
            #  get the statistics for the model
            # test.sum<-summary(test.gls)
            # test.t<-test.sum$tTable
            # poo<-nrow(df.dat)
            # Variable=rep(c("fitted"), poo)
            # test_df2<-cbind(df.dat$County, df.dat$time,df.dat$Counts,fitted)
            # test_df2<-data.frame(test_df2)
            # colnames(test_df2)<-c("County","time","Counts","fitted")
            # 
            # test_df2<-data.frame(test_df2)
            #colnames(fitted) <- c("time", "county", "values", "type")
            # myplot <- ggplot(fitted, aes(x=time, y=values, fill = "type")) +
            #     geom_line() +
            #     facet_wrap(~county)
            res.dat <- data.frame(cbind(df.dat, fitted))
            myplot <- ggplot(res.dat,mapping= aes(x=time)) +
                geom_line(aes(y=`Counts`),color="blue")+
                geom_line(aes(y=`fitted`),color="red")+
                labs(y="Numbers", title=choice2)+
                facet_wrap(~County)
            return(myplot)
        }
    })
    
    table_info <- reactive({
        choice1<-input$bins1
        choice2<-input$bins2
        if(input$bins1=="Time_series")
        {
            df.dat<-cbind(sweden$county,sweden$time,sweden[,choice2])
            df.dat<-data.frame(df.dat)
            
            colnames(df.dat)<-c("County","time","Counts")
            
            test.gls<-gls(log(Counts+1)~time, df.dat)
            fitted<-exp(fitted(test.gls))-1
            #  get the statistics for the model
            test.sum<-summary(test.gls)
            test.t<-test.sum$tTable

            return(test.sum)      
        }
        else
        {
            # choice1<-c("Time_series")
            # choice2<-c("Seed_sown")
            #cat("choice 1 ",choice1, " choice 2 ", choice2,"\n")
            df.dat<-cbind(sweden$county,sweden$time,sweden[,choice2])
            df.dat<-data.frame(df.dat)

            colnames(df.dat)<-c("County","time","Counts")

            test.gls<-lme(log(Counts+1)~time, random=~1|County,df.dat)
            fitted<-exp(fitted(test.gls))-1
            #  get the statistics for the model
            test.sum<-summary(test.gls)
            test.t<-test.sum$tTable

            return(test.sum)
        }

    })
    
    output$distPlot <- renderPlot({
        plot_info()
    }) # this expression above generates a plot
    
    output$table <- renderPrint({
        table_info()
    })
    
}




######## run it
shinyApp(ui = ui, server = server)
