library(shiny)

u <- shinyUI(pageWithSidebar(
  
  headerPanel("输入数据进行直线拟合"),
  sidebarPanel(
    textInput('vec1', '自变量x，用半角逗号或空格分隔', 
              "0, 12.5, 25, 50, 100"),
    textInput('vec2', '因变量y，用半角逗号或空格分隔',
              "0 0.2855 0.623 0.723 0.861"),
    checkboxInput("model", "是否过原点", value = FALSE , width = NULL),
    textInput('vec3', '横轴标签',
              "浓度 (pg/ml)"),
    textInput('vec4', '纵轴标签',
              "490nm OD 值"),
    textInput('vec5', '图形标题',
              "ELISA 标准拟合直线"),
    br(),br()
    # downloadLink("download_pdf", "下载图片的pdf进行后续修改")
  ),
  
  mainPanel(
    h4('拟合数据'),
    verbatimTextOutput("oid1"),
    h4('拟合结果'),
    verbatimTextOutput("lm_result"),
    h4('拟合图形'),
    plotOutput("plot_lm", width = "520px", height = "400px" )
  )
  
))

s <- shinyServer(function(input, output) {
  output$oid1<-renderPrint({
    x <- as.numeric(unlist(strsplit(input$vec1,",")))
    if (any(is.na(x))) { x <-as.numeric(unlist(strsplit(input$vec1," "))) }
    y <- as.numeric(unlist(strsplit(input$vec2,",")))
    if (any(is.na(y))) { y <-as.numeric(unlist(strsplit(input$vec2," "))) }
    cat("x:\n")
    cat(x)
    cat("\ny:\n")
    cat(y)
  }
  )
  
  output$lm_result<-renderPrint({
    x <- as.numeric(unlist(strsplit(input$vec1,",")))
    if (any(is.na(x))) { x <-as.numeric(unlist(strsplit(input$vec1," "))) }
    y <- as.numeric(unlist(strsplit(input$vec2,",")))
    if (any(is.na(y))) { y <-as.numeric(unlist(strsplit(input$vec2," "))) }
    if (input$model){
    line.model <- lm(y~x+0)
    cat("your line：\ny = ",
        line.model$coefficients[1],
        "x", sep="")
    }
    else {
      line.model <- lm(y~x)
      cat("your line：\ny = ",
          line.model$coefficients[1], 
          " + ", line.model$coefficients[2],
          "x", sep="")}
    f <- summary(line.model)$fstatistic
    p.value <- pf(f[1], f[2], f[3], lower.tail=F)
    

    cat("\n\nR squared：\n",
        summary(line.model)$r.squared, sep="")
    cat("\n\nAdjusted R squared：\n",
        summary(line.model)$adj.r.squared, sep="")
    cat("\n\np value：\n",
        p.value, sep="")
  }
  )
  
  output$plot_lm<-renderPlot(
    {
      x <- as.numeric(unlist(strsplit(input$vec1,",")))
      if (any(is.na(x))) { x <-as.numeric(unlist(strsplit(input$vec1," "))) }
      y <- as.numeric(unlist(strsplit(input$vec2,",")))
      if (any(is.na(y))) { y <-as.numeric(unlist(strsplit(input$vec2," "))) }
      if (input$model){
        line.model <- lm(y~x+0)
        text1 <- paste("y = ", round(line.model$coefficients, 5),"x",sep="")
      }
      else {
        line.model <- lm(y~x)
        text1 <- paste("y = ", round(line.model$coefficients[2], 4),"x + ",
                       round(line.model$coefficients[1], 4),sep="")
        }
    plot(x, y, pch=16,
         xlab=input$vec3,
         ylab=input$vec4, 
         bty="n",
         xlim=c(min(x), max(x))+c(0, 0.2)*(max(x)-min(x))/5,
         ylim=c(min(y), max(y))+c(0, 0.2)*(max(y)-min(y))/5,
         main=input$vec5, axes=FALSE,
         family = "Microsoft Yahei")
    axis(2, at=round(seq(min(y), max(y), length=7), 2), las=2, tck=0.01, pos=0)
    axis(1, at=round(seq(min(x), max(x), length=6), 2), las=1, tck=0.01, pos=0)


    clip(min(x)-(max(x)-min(x))/5, max(x)+(max(x)-min(x))/5,
         min(y)-(max(y)-min(y))/5, max(y)+(max(y)-min(y))/5)

    arrows(max(x), min(y), max(x)+(max(x)-min(x))/14, min(y), length=0.1)
    arrows(min(x), max(y), min(x), max(y)+(max(y)-min(y))/12, length=0.1)

    clip(min(x), max(x), min(y), max(y)+(max(y)-min(y))/6)
    abline(line.model, lwd=2, col="red")

    text(x=quantile(x, 0.80), y=quantile(y, .39),
         labels= substitute(paste(text1, r.squared, sep=''), list(
           text1=paste(text1, "\n"), r.squared="")),
         adj=c(0,0))
    text(x=quantile(x, 0.80), y=quantile(y, .39),
         labels= substitute(paste(text1, R^2, " = ", r.squared, sep=''), list(
           text1="\n", r.squared=round(summary(line.model)$r.squared, 4))),
         adj=c(0.08,0))
    })
    
    # output$download_pdf <- downloadHandler(
    #   contentType = "application/pdf",
    #   filename = paste(input$vec5, '.pdf', sep=""),
    #   content <- function(file){
    #   pdf(file, width=7, height=5)
    #   plot(1:10)
    #   dev.off()})
    #   {
    #       x <- as.numeric(unlist(strsplit(input$vec1,",")))
    #       if (any(is.na(x))) { x <-as.numeric(unlist(strsplit(input$vec1," "))) }
    #       y <- as.numeric(unlist(strsplit(input$vec2,",")))
    #       if (any(is.na(y))) { y <-as.numeric(unlist(strsplit(input$vec2," "))) }
    #       if (input$model){
    #         line.model <- lm(y~x+0)
    #         text1 <- paste("y = ", round(line.model$coefficients, 5),"x")
    #       }
    #       else {
    #         line.model <- lm(y~x)
    #         text1 <- paste("y = ", round(line.model$coefficients[2], 4),"x + ", round(line.model$coefficients[1], 4))
    #       }
    #       plot(x, y, pch=16,
    #            xlab=input$vec3,
    #            ylab=input$vec4, 
    #            bty="n",
    #            xlim=c(min(x), max(x))+c(0, 0.2)*(max(x)-min(x))/5,
    #            ylim=c(min(y), max(y))+c(0, 0.2)*(max(y)-min(y))/5,
    #            main=input$vec5, axes=FALSE
    #            )
    #       axis(2, at=round(seq(0, max(y), length=7), 2), las=2, tck=0.01, pos=0)
    #       axis(1, at=round(seq(0, max(x), length=6), 2), las=1, tck=0.01, pos=0)
    #       
    #       
    #       clip(-2, 120, -1, 1)
    #       
    #       arrows(100, 0, 105, 0, length=0.1)
    #       arrows(0, max(y), 0, max(y)+0.1, length=0.1)
    #       
    #       clip(min(x), max(x), min(y), max(y)+(max(y)-min(y))/6)
    #       abline(line.model, lwd=2, col="red")
    #       
    #       #
    #       text(x=quantile(x, 0.85), y=quantile(y, .45),
    #            labels=text1, adj=c(0,0))
    #       
    #       text(x=quantile(x, 0.85), y=quantile(y, .39),
    #            labels= latex2exp::TeX(paste("R^{2} =",round(summary(line.model)$r.squared, 4))),
    #            adj=c(0,0))
    #       dev.copy2pdf(file = "plot.pdf")
    #       
    #   },
    #   content <- function(file) {
    #       file.copy(file = "plot.pdf", file)
    #       dev.off()
    #   }
    #   )

}
)
shinyApp(ui = u, server = s)