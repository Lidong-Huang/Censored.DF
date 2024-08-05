#  ex_dt <- read.csv("Example data.csv",as.is=TRUE,
#                   strip.white=TRUE, colClasses="character")
# 加载所需的包
library(shiny)
library(shinydashboard)
library(DT)
library(fitdistrplus)
library(EnvStats)
library(spsComps)

# 自定义CSS
custom_css <- "
#header-title {
  white-space: pre-wrap; /* 保证换行显示 */
  text-align: center;
  line-height: 1.2; /* 调整行高以适应多行标题 */
  font-size: 14px; /* 调整字体大小以适应标题 */
}
footer {
  position: absolute;
  bottom: 0;
  width: 100%;
  text-align: center;
  padding: 10px 0;
  background-color: #f1f1f1;
}
"
myApp <- function(...){
  # UI部分
  ui <- dashboardPage(
    dashboardHeader(
      title = tags$div(id = "header-title",
                       "Distribution Fitting\nfor Left Censored Data\nand Weighting LOD/2 Substitution Method")
    ),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Instruction", tabName = "instruction", icon = icon("info-circle")),
        menuItem("Upload File", tabName = "upload_file", icon = icon("upload")),
        menuItem("Summary of Fitting", tabName = "summary", icon = icon("table")),
        menuItem("CDF & Q-Q Plot", tabName = "plots", icon = icon("chart-line")),
        menuItem("Weighting Substitution", tabName = "weighting", icon = icon("balance-scale"))
      )
    ),
    dashboardBody(
      tags$head(tags$style(HTML(custom_css))),
      tabItems(
        tabItem(tabName = "instruction",
                fluidRow(
                  box(title = "Instructions", width = 12,
                      htmlOutput("instructions")
                  ),
                  box(title = "Example Data", width = 12,
                      DTOutput("example_dt")
                  )
                )
        ),
        tabItem(tabName = "upload_file",
                fluidRow(
                  box(title = "Upload Your Data", width = 12,
                      fileInput("file1", "Choose CSV File", accept = ".csv"),
                      actionButton("goButton", "Start Fitting!", class = "btn-success")
                  ),
                  box(title = "User Data", width = 12,
                      tableOutput("user_dt")
                  )
                )
        ),
        tabItem(tabName = "summary",
                fluidRow(
                  box(title = "Summary of Fitting", width = 12,
                      DTOutput('tb_summary')
                  )
                )
        ),
        tabItem(tabName = "plots",
                fluidRow(
                  box(title = "CDF & Q-Q Plot", width = 12,
                      plotOutput('plot_submitted')
                  )
                )
        ),
        tabItem(tabName = "weighting",
                fluidRow(
                  box(title = "Weighting Substitution Method", width = 12,
                      selectInput("dist_type", "Select Distribution Type", choices = c("Lognormal", "Gamma")),
                      actionButton("calcButton", "Calculate", class = "btn-primary"),
                      verbatimTextOutput("weighting_result")
                  )
                )
        )
      ),
      tags$footer("The app is developed by Lidong Huang, copyright (c) reserved 2024")
    )
  )

  # 算术平均值权重预测函数
  predic_w_lg_am <- function(sd_l, mean_l, lod.p) {
    t <- sd_l / mean_l
    a <- 1E-3 * (1482 - 9.166 * lod.p)
    b <- -1E-3 * (166 + 2.569 * lod.p)
    a * t^b
  }

  # am 计算时权重预测函数
  predic_w_gamma_am <- function(sd_l,mean_l,lod.p)
  {
    t <- sd_l/mean_l
    a <- 1.215E-4*lod.p^2-1.315E-2*lod.p+2.339
    b <- -1.652E-4*lod.p^2-5.432E-4*lod.p-0.8939
    a * exp(b*t)
  }

  predic_w_gamma_gm <- function(sd_l,mean_l,lod.p)
  {
    t <- sd_l/mean_l
    a <- 1.403E-4*lod.p^2-1.236E-2*lod.p+2.924
    b <- -2.253E-4*lod.p^2-1.111E-2*lod.p-1.421
    a * exp(b*t)
  }
  # 几何平均值权重预测函数
  predic_w_lg_gm <- function(sd_l, mean_l, lod.p) {
    t <- sd_l / mean_l
    a <- 1E-2 * (142.327 - 1.098 * lod.p)
    b <- -1E-3 * (202.6 + 3.906 * lod.p)
    a * t^b
  }

  # Server部分
  server <- function(input, output) {
    instructions_text <- "
    > This app is designed for testing the statistical distribution (Lognormal or Gamma) of left censored data.<br>
    > This app only accepts csv format file.<br>
    > The dataset should only contain two columns. The first column is numeric value in which censored value is replaced by detection limit. The second column is logical value (TRUE for the censored, FALSE for the detected).<br>
    > You can download the example Lognormal data (right side) as a template. In this example, the detection limit is 0.828.<br>
    > Upload your data in the 'Upload File' menu.<br>
    > After uploading your data, if everything is right, click 'Start Fitting!' in the 'Upload File' menu and the fitting results will be shown in 'Summary of Fitting' and 'CDF & Q-Q Plot'.<br>
    > The core functions are from package {EnvStats}. Function 'gofTestCensored' is used to do goodness-of-fit. 'cdfCompareCensored' and 'qqPlotCensored' are applied to plot the CDF and Q-Q graphs.<br>
    > The 'Weighting Substitution' module allows you to choose the distribution type and calculate the arithmetic mean and geometric mean of the data with weighting substituted methods (w*LOD/2).
  "

    output$instructions <- renderUI(HTML(instructions_text))

    output$example_dt <- renderDT(
      ex_dt,
      rownames=F,
      extensions = 'Buttons',
      options = list(
        autoWidth = F,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'pdf')
      )
    )

    storedData <- reactiveValues()

    output$user_dt <- renderTable({
      infile <- input$file1
      ext <- tools::file_ext(infile$datapath)
      req(infile)
      if (is.null(infile))
        return(NULL)
      validate(need(ext == "csv", "Please upload a csv file!"))
      storedData$data1 <- read.csv(infile$datapath, header = TRUE)
    })

    s_table <- eventReactive(input$goButton, {
      req(input$file1)
      data1 <- storedData$data1
      data1 <- data.frame(data1)
      data1[is.na(data1)] <- ""
      data1[!apply(data1 == "", 1, all), ]
      data1 <- data1[, colSums(is.na(data1)) != nrow(data1)]
      data1 <- data1[, !apply(data1 == "", 2, all)]
      data1 <- data1[, 1:2]
      names(data1) <- c("Observ_data", "Censored")

      lnorm_fit <- shinyCatch(
        gofTestCensored(data1$Observ_data, data1$Censored, test = "sw",
                        distribution = "lnormAlt")
      )
      gamma_fit <- shinyCatch(
        gofTestCensored(data1$Observ_data, data1$Censored, test = "sw",
                        distribution = "gammaAlt")
      )

      final_summ <- data.frame(
        Statistics = c("W_Statistic", "P_value"),
        Lognormal = formatC(c(lnorm_fit$statistic, lnorm_fit$p.value), digits = 3, format = "f"),
        Gamma = formatC(c(gamma_fit$statistic, gamma_fit$p.value), digits = 3, format = "f")
      )
      final_summ$Lognormal[final_summ$Lognormal == "<0.001"] <- formatC(lnorm_fit$p.value, digits = 3, format = "e")
      final_summ$Gamma[final_summ$Gamma == "<0.001"] <- formatC(gamma_fit$p.value, digits = 3, format = "e")

      fig_4plot <- function() {
        par(mfrow = c(2, 2))
        if (class(lnorm_fit) != "NULL")
          cdfCompareCensored(data1$Observ_data,
                             data1$Censored,
                             distribution = "lnorm",
                             main = "ECDF(dotted) vs Lognormal(blue)",
                             xlab = "Order statistics")
        else
          plot(NULL, xlab = "", ylab = "", xaxt = "n", yaxt = "n",
               xlim = c(0, 10), ylim = c(0, 10))
        if (class(gamma_fit) != "NULL")
          cdfCompareCensored(data1$Observ_data,
                             data1$Censored,
                             distribution = "gamma",
                             main = "ECDF(dotted) vs Gamma(red)",
                             xlab = "Order statistics",
                             x.col = "red")
        else
          plot(NULL, xlab = "", ylab = "", xaxt = "n", yaxt = "n",
               xlim = c(0, 10), ylim = c(0, 10))
        if (class(lnorm_fit) != "NULL")
          qqPlotCensored(data1$Observ_data,
                         data1$Censored,
                         main = "Q-Q Plot & Lognormal fit",
                         distribution = "lnorm", estimate.params = TRUE,
                         add.line = TRUE,
                         xlab = "Lognormal Quantiles",
                         ylab = "Quantiles of data")
        else
          plot(NULL, xlab = "", ylab = "", xaxt = "n", yaxt = "n",
               xlim = c(0, 10), ylim = c(0, 10))
        if (class(gamma_fit) != "NULL")
          qqPlotCensored(data1$Observ_data,
                         data1$Censored,
                         main = "Q-Q Plot & Gamma fit",
                         distribution = "gamma", estimate.params = TRUE,
                         add.line = TRUE,
                         equal.axes = FALSE,
                         xlab = "Gamma Quantiles",
                         ylab = "Quantiles of data")
        else
          plot(NULL, xlab = "", ylab = "", xaxt = "n", yaxt = "n",
               xlim = c(0, 10), ylim = c(0, 10))
      }

      r_result <- list(ss_table = final_summ,
                       ss_plot = fig_4plot)
      return(r_result)
    })

    output$tb_summary <- renderDT({
      s_table()$ss_table
    })

    output$plot_submitted <- renderPlot({
      s_table()$ss_plot()
    })

    observeEvent(input$calcButton, {
      req(storedData$data1)
      data1 <- storedData$data1
      data1 <- data.frame(data1)
      data1 <- data1[!apply(data1 == "", 1, all), ]
      data1 <- data1[, colSums(is.na(data1)) != nrow(data1)]
      data1 <- data1[, !apply(data1 == "", 2, all)]
      data1 <- data1[, 1:2]
      names(data1) <- c("Data", "Censored")
      data2 <- data1
      sd_l <- sd(data1$Data[!data1$Censored])
      mean_l <- mean(data1$Data[!data1$Censored])
      lod.p <- sum(data1$Censored) / nrow(data1) * 100
      lod <- data1$Data[data1$Censored][1]
      if (input$dist_type == "Lognormal") {
        am_weight <- predic_w_lg_am(sd_l, mean_l, lod.p)
        gm_weight <- predic_w_lg_gm(sd_l, mean_l, lod.p)
      }
      if (input$dist_type == "Gamma") {
        am_weight <- predic_w_gamma_am(sd_l, mean_l, lod.p)
        gm_weight <- predic_w_gamma_gm(sd_l, mean_l, lod.p)
      }
      data1$Data[data1$Censored] <- am_weight * (lod / 2)
      am <- mean(data1$Data)
      sd_am <- sd(data1$Data)

      data2$Data[data2$Censored] <- gm_weight * (lod / 2)
      gm <- exp(mean(log(data2$Data)))

      output$weighting_result <- renderPrint({
        cat("Arithmetic Mean:", round(am, 3), "\n")
        cat("Geometric Mean:", round(gm, 3), "\n")
        cat("Standard Deviation:", round(sd_am, 3), "\n")
      })

    })
  }

  # 运行应用程序
  shinyApp(ui = ui, server = server)
}


