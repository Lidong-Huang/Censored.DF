#  ex_dt <- read.csv("Example data.csv",as.is=TRUE,
#                   strip.white=TRUE, colClasses="character")
library(shiny)
library(DT)
library(fitdistrplus)
library(EnvStats)
library(spsComps)
myApp <- function(...){
ui <- navbarPage("Distribution fitting\n
           for left censored data",
                 tabPanel("Instruction",
                          sidebarLayout(
                            sidebarPanel(
                              textOutput("instruction1"),
                              textOutput("instruction2"),
                              textOutput("instruction3"),
                              textOutput("instruction4"),
                              textOutput("instruction5"),
                              textOutput("instruction6"),
                              textOutput("instruction7")
                            ),
                            mainPanel(
                              DTOutput("example_dt")
                            )
                          )
                 ),
                 tabPanel("Upload_file",
                          sidebarLayout(
                            sidebarPanel(
                              fileInput("file1", "Choose CSV File", accept = ".csv"),
                              actionButton("goButton", "Start Fitting!", class = "btn-success")
                            ),
                            mainPanel(
                              tableOutput("user_dt")
                            )
                          )
                 ),
                 tabPanel("Summary of Fitting",
                          DTOutput('tb_summary')
                 ),
                 tabPanel("CDF & Q-Q plot",
                          plotOutput('plot_submitted')
                 )
)
server <- function(input, output) {
  output$instruction1 <- renderText(
    "> This app is designed for testing the
    statistical distribution (Lognormal or Gamma) of left
    censored data."
  )
  output$instruction2 <- renderText(
    "> This app only accepts csv format file."
  )
  output$instruction3 <- renderText(
    "> The dataset should only contain two columns.The first column is numeric value in which censored
    value is replaced by detection limit. The second column is
    logical value (TRUE for the censored, FALSE for the detected)"
  )
  output$instruction4 <- renderText(
    "> You can download the exmple Lognormal data (right side) as template.
    In this example the detection limit is 0.828"
  )
  output$instruction5 <- renderText(
    "> Upload you data in the 'Upload_file' menu"
  )
  output$instruction6 <- renderText(
    "> After uploading your data, if everything is right,
    click 'Start Fitting!' in 'Upload_file' menu and the fitting is showed in
    'Summary of Fitting' and 'CDF & Q-Q plot'"
  )
  output$instruction7 <- renderText(
    "> The core funcions are from package {EnvStats}. Function 'gofTestCensored () is used to do goodness-of-fit.
    'cdfCompareCensored ()' and 'qqPlotCensored ()'are applied to plot the CDF and Q-Q graphs.
    The app is developed by Lidong Huang,
    copyright (c) reserved 2023"
  )
  output$example_dt <- renderDT(
    ex_dt,
    rownames=F,
    extensions = 'Buttons',
    options = list(
      autoWidth = F,
      dom = 'Bfrtip',
      buttons = c('copy', 'csv',  'pdf')
    )
  )
  storedData <- reactiveValues()
  output$user_dt <- renderTable(
    {infile <- input$file1
    ext <- tools::file_ext(infile$datapath)
    req(infile)
    if (is.null(infile))
      return(NULL)
    validate(need(ext == "csv", "Please upload a csv file!!"))
    storedData$data1 <-read.csv(infile$datapath, header = TRUE)
    })


  s_table <- eventReactive(input$goButton, {
  req(input$file1)
    data1 <- storedData$data1
    data1 <- data.frame(data1)
    data1[is.na(data1)] <- ""
    data1[!apply(data1 == "", 1, all), ]# remove empty row
    data1 <-
      data1[,colSums(is.na(data1)) != nrow(data1)]
    data1 <-
      data1[,!apply(data1 == "", 2, all)]
    data1 <- data1[,1:2]
    names(data1) <- c("Observ_data","Censored")
    lnorm_fit <- shinyCatch(
      gofTestCensored(data1$Observ_data, data1$Censored, test = "sw",
                      distribution = "lnormAlt")
          )
    gamma_fit <- shinyCatch(
      gofTestCensored(data1$Observ_data, data1$Censored, test = "sw",
                                            distribution = "gammaAlt")
          )
    final_summ <- data.frame(Statistics=
                               c("W_Statistic","P_value"),
                             Lognormal=c(lnorm_fit$statistic,lnorm_fit$p.value),
                             Gamma=c(gamma_fit$statistic,gamma_fit$p.value))
    # final_summ <- round(final_summ,3)

    fig_4plot <- function(){
      par(mfrow=c(2,2))
      if(class(lnorm_fit)!="NULL")
      cdfCompareCensored(data1$Observ_data,
                         data1$Censored,
                         distribution = "lnorm",
                         main="ECDF(dotted) vs Lognormal(blue)",
                         xlab="Order statistics")
      else
        plot(NULL, xlab="", ylab="", xaxt="n", yaxt="n",
             xlim=c(0, 10), ylim=c(0, 10))
      if(class(gamma_fit)!="NULL")
      cdfCompareCensored(data1$Observ_data,
                         data1$Censored,
                         distribution = "gamma",
                         main="ECDF(dotted) vs Gamma(red)",
                         xlab="Order statistics",
                         x.col = "red")
      else
        plot(NULL, xlab="", ylab="", xaxt="n", yaxt="n",
             xlim=c(0, 10), ylim=c(0, 10))
      if(class(lnorm_fit)!="NULL")
      qqPlotCensored(data1$Observ_data,
                     data1$Censored,
                     main="Q-Q Plot & Lognormal fit",
                     distribution = "lnorm",estimate.params=TRUE,
                     add.line = TRUE,
                     xlab="Lognormal Quantiles",
                     ylab="Quantiles of data")
      else
        plot(NULL, xlab="", ylab="", xaxt="n", yaxt="n",
             xlim=c(0, 10), ylim=c(0, 10))
      if(class(gamma_fit)!="NULL")
      qqPlotCensored(data1$Observ_data,
                     data1$Censored,
                     main="Q-Q Plot & Gamma fit",
                     distribution = "gamma",estimate.params=TRUE,
                     add.line = TRUE,
                     equal.axes=FALSE,
                     xlab="Gamma Quantiles",
                     ylab="Quantiles of data")
      else
        plot(NULL, xlab="", ylab="", xaxt="n", yaxt="n",
             xlim=c(0, 10), ylim=c(0, 10))

    }
    r_result <- list(ss_table=final_summ,
                     ss_plot=fig_4plot)
    return(r_result)

  })

output$tb_summary <- renderDT(
          s_table()$ss_table)
output$plot_submitted <- renderPlot(

      s_table()$ss_plot()
  )



}

# Run the application
shinyApp(ui = ui, server = server)
}


