#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
# packages <- c("ggplot2", "plyr")
# if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
#   install.packages(setdiff(packages, rownames(installed.packages())))  
# }
library(shiny)
require(ggplot2)
require(plyr)
require(gdata)
# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # App title ----
  titlePanel("浆核RNA qPCR结果处理"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Select a file ----
      fileInput("file1", "选择原始excel表格数据上传",
                multiple = FALSE,
                accept = c(".xls", ".xlsx")),
                # accept = c("text/csv",
                #            "text/comma-separated-values,text/plain",
                #            ".csv", ".xls", ".xlsx")),
      # h5("如果上传CSV文件，则格式如下（列名称可随意）：", style="color:balck;"),
      # tableOutput("sample_csv"),
      # h5("选择原始数据表格", style="color:red;"),
      # downloadButton('downloadxls', '点击下载示例excel表格', style="color:gray"),
      # Horizontal line ----
      tags$hr(),
      downloadButton('downloadcsv', '下载处理结果文件（csv格式）'),
      tags$hr(),
      radioButtons(inputId = "type", label = "选择图片格式下载", choices = list("png", "pdf")),
      downloadButton('downloadpng', '点击下载图片')
      
      # Input: Checkbox if file has header ----
      # checkboxInput("header", "Header", TRUE)
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Data file ----
      plotOutput("contents", width = "600px", height = "400px")
      
    )
    
  )
))
