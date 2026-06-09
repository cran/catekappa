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

  # 辅助函数：安全提取单值
  safe_val <- function(x, default = "") {
    if (is.null(x)) return(default)
    if (length(x) == 0) return(default)
    if (is.numeric(x) && length(x) > 1) {
      # 如果是向量，取最后一个（通常样本量是最后一个）
      return(tail(x, 1))
    }
    as.character(x[1])
  }

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

    n_val <- ceiling(as.numeric(safe_val(res$n, 0)))
    kappa0_val <- safe_val(res$kappa0)
    kappa1_val <- safe_val(res$kappa1)
    cat_val <- safe_val(res$categories)
    alpha_val <- safe_val(res$alpha)
    power_val <- safe_val(res$power)
    raters_val <- safe_val(res$raters)
    method_val <- safe_val(res$kappaSize_function)

    div(
      style = "margin: 15px 0;",
      div(
        style = paste0("background-color: ", ifelse(n_val > 200, "#f39c12", "#27ae60"),
                       "; color: white; padding: 20px; border-radius: 8px; text-align: center;"),
        h2(style = "margin: 0; font-size: 48px;", n_val),
        p(style = "margin: 5px 0 0 0; font-size: 16px;", "Required Sample Size")
      ),
      div(
        style = "margin-top: 15px;",
        HTML(paste0(
          "<table style='width: 100%;'>",
          "<tr><td style='padding: 5px;'><strong>Null Kappa (H0):</strong></td><td>", kappa0_val, "</td></tr>",
          "<tr><td style='padding: 5px;'><strong>Alternative Kappa (H1):</strong></td><td>", kappa1_val, "</td></tr>",
          "<tr><td style='padding: 5px;'><strong>Categories:</strong></td><td>", cat_val, "</td></tr>",
          "<tr><td style='padding: 5px;'><strong>Significance Level:</strong></td><td>", alpha_val, "</td></tr>",
          "<tr><td style='padding: 5px;'><strong>Power:</strong></td><td>", power_val, "</td></tr>",
          "<tr><td style='padding: 5px;'><strong>Raters:</strong></td><td>", raters_val, "</td></tr>",
          "<tr><td style='padding: 5px;'><strong>Method:</strong></td><td>", method_val, "</td></tr>",
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

    kappa_val <- ifelse(is.null(res$kappa), NA, round(as.numeric(res$kappa), 4))
    color_val <- ifelse(is.null(interp$color), "#7f8c8d", interp$color)
    level_val <- ifelse(is.null(interp$level), "Unknown", interp$level)
    desc_val <- ifelse(is.null(interp$description), "", interp$description)

    div(
      style = "margin: 15px 0;",
      div(
        style = paste0("background-color: ", color_val,
                       "; color: white; padding: 20px; border-radius: 8px; text-align: center;"),
        h2(style = "margin: 0; font-size: 36px;", kappa_val),
        p(style = "margin: 5px 0 0 0; font-size: 18px;", level_val),
        p(style = "margin: 2px 0 0 0; font-size: 14px;", desc_val)
      ),
      div(
        style = "margin-top: 15px;",
        HTML(paste0(
          "<div style='display: flex; justify-content: space-around; text-align: center;'>",
          "<div><h4>", safe_val(res$n_subjects, 0), "</h4><p>Subjects</p></div>",
          "<div><h4>", safe_val(res$n_raters, 0), "</h4><p>Raters</p></div>",
          "<div><h4>", ifelse(is.null(res$categories), 0, length(res$categories)), "</h4><p>Categories</p></div>",
          "</div>"
        ))
      )
    )
  })

  output$interpretation_box <- renderUI({
    req(analysis_result())
    res <- analysis_result()
    interp <- res$interpretation

    color_val <- ifelse(is.null(interp$color), "#7f8c8d", interp$color)
    level_val <- ifelse(is.null(interp$level), "Unknown", interp$level)
    desc_val <- ifelse(is.null(interp$description), "", interp$description)

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
      "<h4 style='margin-top: 0; color: ", color_val, ";'>", level_val, " Agreement</h4>",
      "<p>", desc_val, "</p>",
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
