library(shiny)

if (!requireNamespace("irr", quietly = TRUE)) {
  stop("Package 'irr' is required. Please install it.")
}
if (!requireNamespace("kappaSize", quietly = TRUE)) {
  stop("Package 'kappaSize' is required. Please install it.")
}

# ========== UI ==========
ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "flatly"),

  titlePanel(
    div(
      h2("CATEKAPPA", style = "color: #2c3e50; font-weight: bold;"),
      h4("基于 Kappa 统计量的一致性检验设计与分析", style = "color: #7f8c8d;"),
      style = "border-bottom: 3px solid #3498db; padding-bottom: 10px; margin-bottom: 20px;"
    )
  ),

  sidebarLayout(
    sidebarPanel(
      width = 4,

      div(
        style = "background-color: #ecf0f1; padding: 15px; border-radius: 5px; margin-bottom: 15px;",
        h4("选择模块", style = "color: #2c3e50; margin-top: 0;"),
        selectInput("module", NULL,
                    choices = c(
                      "样本量设计" = "design",
                      "一致性分析" = "analysis"
                    ),
                    width = "100%")
      ),

      hr(),

      conditionalPanel(
        condition = "input.module == 'design'",
        h4("设计参数", style = "color: #2980b9;"),

        numericInput("kappa0", "零假设 Kappa (H0):",
                     value = 0.4, min = 0, max = 1, step = 0.05),

        numericInput("kappa1", "备择假设 Kappa (H1):",
                     value = 0.6, min = 0, max = 1, step = 0.05),

        textInput("props", "类别比例（逗号分隔）:",
                  value = "0.5, 0.5"),

        numericInput("alpha", "显著性水平:",
                     value = 0.05, min = 0.01, max = 0.2, step = 0.01),

        numericInput("power", "检验功效 (1-β):",
                     value = 0.8, min = 0.5, max = 0.99, step = 0.05),

        numericInput("raters", "评价者数量:",
                     value = 2, min = 2, max = 10, step = 1),

        actionButton("calc_n", "计算样本量",
                     class = "btn-primary btn-block",
                     style = "margin-top: 10px; width: 100%;")
      ),

      conditionalPanel(
        condition = "input.module == 'analysis'",
        h4("分析参数", style = "color: #27ae60;"),

        fileInput("datafile", "上传数据 (CSV/TXT):",
                  accept = c("text/csv", ".csv", ".txt"),
                  placeholder = "选择文件..."),

        helpText("数据格式：行为受试者，列为评价者"),

        selectInput("kappa_type", "Kappa 类型:",
                    choices = c(
                      "Cohen's Kappa (2 位评价者)" = "cohen",
                      "Fleiss' Kappa (3+ 位评价者)" = "fleiss",
                      "Light's Kappa (3+ 位评价者, 两两配对)" = "light"
                    )),

        checkboxInput("detail", "显示详细输出", value = FALSE),

        actionButton("analyze", "分析一致性",
                     class = "btn-success btn-block",
                     style = "margin-top: 10px; width: 100%;")
      ),

      hr(),

      conditionalPanel(
        condition = "input.module == 'analysis'",
        downloadButton("download_example", "下载示例数据",
                       class = "btn-info btn-block",
                       style = "width: 100%;")
      )
    ),

    mainPanel(
      width = 8,

      conditionalPanel(
        condition = "input.module == 'design'",
        div(
          style = "background-color: #f8f9fa; padding: 20px; border-radius: 5px; border-left: 5px solid #2980b9;",
          h3("样本量计算结果", style = "color: #2c3e50; margin-top: 0;"),

          uiOutput("design_summary"),

          hr(),

          h4("详细输出"),
          verbatimTextOutput("design_result")
        )
      ),

      conditionalPanel(
        condition = "input.module == 'analysis'",
        div(
          style = "background-color: #f8f9fa; padding: 20px; border-radius: 5px; border-left: 5px solid #27ae60;",
          h3("一致性分析结果", style = "color: #2c3e50; margin-top: 0;"),

          uiOutput("analysis_summary"),

          hr(),

          h4("结果解释"),
          htmlOutput("interpretation_box"),

          hr(),

          h4("详细统计量"),
          verbatimTextOutput("analysis_result")
        )
      )
    )
  ),

  div(
    style = "margin-top: 30px; padding: 15px; text-align: center; color: #7f8c8d; font-size: 12px; border-top: 1px solid #ecf0f1;",
    "CATE 包 | 基于 irr 与 kappaSize | 用于分类响应的一致性分析"
  )
)

# ========== Server ==========
server <- function(input, output, session) {

  design_result <- eventReactive(input$calc_n, {
    props <- tryCatch({
      p <- as.numeric(unlist(strsplit(input$props, ",")))
      if (any(is.na(p))) stop("Invalid proportions")
      p
    }, error = function(e) {
      showNotification("Invalid category proportions. Using equal proportions.", type = "error")
      rep(1/2, 2)
    })

    res <- calc_sample_size_kappa(
      kappa0 = input$kappa0,
      kappa1 = input$kappa1,
      props = props,
      alpha = input$alpha,
      power = input$power,
      raters = input$raters
    )
    return(res)
  })

  output$design_summary <- renderUI({
    req(design_result())
    res <- design_result()

    div(
      style = "margin: 15px 0;",
      div(
        style = paste0("background-color: ", ifelse(res$n > 200, "#f39c12", "#27ae60"),
                       "; color: white; padding: 20px; border-radius: 8px; text-align: center;"),
        h2(style = "margin: 0; font-size: 48px;", ceiling(res$n)),
        p(style = "margin: 5px 0 0 0; font-size: 16px;", "Required Sample Size")
      ),
      div(
        style = "margin-top: 15px;",
        HTML(paste0(
          "<table style='width: 100%;'>",
          "<tr><td style='padding: 5px;'><strong>Null Kappa (H0):</strong></td><td>", res$kappa0, "</td></tr>",
          "<tr><td style='padding: 5px;'><strong>Alternative Kappa (H1):</strong></td><td>", res$kappa1, "</td></tr>",
          "<tr><td style='padding: 5px;'><strong>Categories:</strong></td><td>", res$categories, "</td></tr>",
          "<tr><td style='padding: 5px;'><strong>Significance Level:</strong></td><td>", res$alpha, "</td></tr>",
          "<tr><td style='padding: 5px;'><strong>Power:</strong></td><td>", res$power, "</td></tr>",
          "<tr><td style='padding: 5px;'><strong>Raters:</strong></td><td>", res$raters, "</td></tr>",
          "<tr><td style='padding: 5px;'><strong>Method:</strong></td><td>", res$kappaSize_function, "</td></tr>",
          "</table>"
        ))
      )
    )
  })

  output$design_result <- renderPrint({
    req(design_result())
    print(design_result())
  })

  uploaded_data <- reactive({
    req(input$datafile)
    tryCatch({
      ext <- tools::file_ext(input$datafile$name)
      if (ext == "csv") {
        read.csv(input$datafile$datapath, row.names = NULL)
      } else {
        read.table(input$datafile$datapath, header = TRUE, row.names = NULL)
      }
    }, error = function(e) {
      showNotification(paste("Error reading file:", e$message), type = "error")
      NULL
    })
  })

  analysis_result <- eventReactive(input$analyze, {
    req(uploaded_data())
    data <- uploaded_data()
    type <- input$kappa_type

    if (type == "cohen" && ncol(data) != 2) {
      showNotification(paste0("Cohen's kappa requires exactly 2 raters. Your data has ", ncol(data), " raters."), type = "warning")
    }

    res <- analyze_kappa(data, type = type, detail = input$detail)
    return(res)
  })

  output$analysis_summary <- renderUI({
    req(analysis_result())
    res <- analysis_result()
    interp <- res$interpretation

    div(
      style = "margin: 15px 0;",
      div(
        style = paste0("background-color: ", interp$color,
                       "; color: white; padding: 20px; border-radius: 8px; text-align: center;"),
        h2(style = "margin: 0; font-size: 36px;", round(res$kappa, 4)),
        p(style = "margin: 5px 0 0 0; font-size: 18px;", interp$level),
        p(style = "margin: 2px 0 0 0; font-size: 14px;", interp$description)
      ),
      div(
        style = "margin-top: 15px;",
        HTML(paste0(
          "<div style='display: flex; justify-content: space-around; text-align: center;'>",
          "<div><h4>", res$n_subjects, "</h4><p>Subjects</p></div>",
          "<div><h4>", res$n_raters, "</h4><p>Raters</p></div>",
          "<div><h4>", length(res$categories), "</h4><p>Categories</p></div>",
          "</div>"
        ))
      )
    )
  })

  output$interpretation_box <- renderUI({
    req(analysis_result())
    res <- analysis_result()
    interp <- res$interpretation

    scale_html <- "
    <div style='margin-top: 10px; font-size: 13px; color: #666;'>
      <strong>Landis & Koch Scale:</strong><br>
      <span style='color: #d9534f;'>■</span> &lt; 0.00 : Poor<br>
      <span style='color: #f0ad4e;'>■</span> 0.00 – 0.20 : Slight<br>
      <span style='color: #f0ad4e;'>■</span> 0.21 – 0.40 : Fair<br>
      <span style='color: #5bc0de;'>■</span> 0.41 – 0.60 : Moderate<br>
      <span style='color: #5cb85c;'>■</span> 0.61 – 0.80 : Substantial<br>
      <span style='color: #5cb85c;'>■</span> 0.81 – 1.00 : Almost Perfect
    </div>
    "

    HTML(paste0(
      "<div style='background-color: #ffffff; padding: 15px; border-radius: 5px; border: 1px solid #ddd;'>",
      "<h4 style='margin-top: 0; color: ", interp$color, ";'>", interp$level, " Agreement</h4>",
      "<p>", interp$description, "</p>",
      scale_html,
      "</div>"
    ))
  })

  output$analysis_result <- renderPrint({
    req(analysis_result())
    print(analysis_result()$statistic)
  })

  output$download_example <- downloadHandler(
    filename = function() {
      "example_kappa_data.csv"
    },
    content = function(file) {
      set.seed(123)
      categories <- c("Low", "Medium", "High")
      n <- 30
      rater1 <- sample(categories, n, replace = TRUE, prob = c(0.3, 0.4, 0.3))
      rater2 <- sapply(rater1, function(x) {
        if (runif(1) < 0.7) x else sample(categories, 1)
      })
      example_data <- data.frame(Rater1 = rater1, Rater2 = rater2, stringsAsFactors = FALSE)
      write.csv(example_data, file, row.names = FALSE)
    }
  )
}

shinyApp(ui = ui, server = server)
