library(shiny)
require(ggplot2)
require(plyr)

require(gdata)
read_qPCR_xls <- function(file) {
    data_test <- read.xls(file, encoding = "UTF-8", stringsAsFactors = FALSE)
    data_test_clean <- data_test[data_test[, 3] != "", ]
    
    sample_target_index <- which(apply(data_test_clean[1, ], 2, function(x) any(grepl(".[Nn]ame", 
        x))))
    Ct_index <- which(apply(data_test_clean[1, ], 2, function(x) any(grepl("^[Cc]", 
        x))))[1]
    
    data_extract <- data_test_clean[, c(sample_target_index, Ct_index)]
    return(data_extract[-1, ])
}

# Define server logic required to draw a histogram
shinyServer( function(input, output) {

  # output$sample_csv <- renderTable({
  #   head(read.csv("www/sample.csv", 
  #                 header = TRUE, 
  #                 stringsAsFactors=FALSE)[1:3], 2)
  # }
  # )
  
  raw_data_generate <- reactive({
    inFile <- input$file1
    if (is.null(inFile)) {
      raw_data <- read.csv("www/sample.csv", 
                  header = TRUE, 
                  stringsAsFactors=FALSE)[, c(1, 2, 3)]
    }
    else {
    	if (identical(strsplit(inFile$datapath, "[.]")[[1]][length(strsplit(inFile$datapath, "[.]")[[1]])], "csv")) {
    		raw_data <- read.csv(inFile$datapath,
                   header = FALSE, 
                  stringsAsFactors=FALSE)[, c(1, 2, 3)]
    	}
    	else {
    		raw_data <- read_qPCR_xls(inFile$datapath)
    	}
      
    }
    
    })


  output$contents <- renderPlot({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    # req(input$file1)
    
    raw_data <- raw_data_generate()
    colnames(raw_data) = c("samples", "gene", "CT")
    raw_data$CT = as.numeric(raw_data$CT)
    
    cells <- substr(raw_data[, 1], c(1), nchar(raw_data[, 1])-2)
    fractions <- substr(raw_data[, 1], nchar(raw_data[, 1]), 
                        nchar(raw_data[, 1]))
    seq_C <- which(fractions=="C")
    seq_N <- which(fractions=="N")
    N_ratio <- 2^(raw_data$CT[seq_C] - raw_data$CT[seq_N])/
      (2^(raw_data$CT[seq_C] - raw_data$CT[seq_N])+1)
    NC_ratio <- 2^(raw_data$CT[seq_C] - raw_data$CT[seq_N])
    cleaned_data <- data.frame(cells=cells[seq_C], gene=raw_data$gene[seq_C], N_ratio, NC_ratio)
    
    
    N_ratio_str <- ddply(cleaned_data, .(gene, cells), 
                         function(x) data.frame(mean=mean(x$N_ratio),
                                                min=min(x$N_ratio), max=max(x$N_ratio), se=sd(x$N_ratio), 
                                                p.value=t.test(log2(x$NC_ratio), mu=0)$p.value))
    N_ratio_str <- data.frame(N_ratio_str, 
                              p.symbol=sapply(N_ratio_str$p.value, 
                                              function(x) if (x<0.001) return("***") 
                                              else if (x<0.01) return("**") 
                                              else if(x<0.05) return("*") 
                                              else return("")))
    write.csv(N_ratio_str, 
              file=paste("www/qPCR_output.csv", sep=''),
              row.names=FALSE)
    p <- ggplot(cleaned_data, aes(cells, N_ratio, fill=cells, colour=cells))+
      geom_boxplot(alpha=.3, width=.5)+facet_wrap(~gene)+
      geom_dotplot(binaxis='y', stackdir='center', dotsize=.5)+
      theme_bw()+theme(legend.position="none")+
      xlab("")+ylab(expression(paste("NUC/", "(NUC+CYT)")))+
      theme(axis.text.x=element_text(face="bold", size=8), 
            axis.text.y=element_text(face="bold", size=12), 
            axis.title.y=element_text(size=13, face="bold"))+
      geom_hline(yintercept=0.5, lwd=1, lty=2, color="red")
    
    genes <- as.character(N_ratio_str$gene[N_ratio_str$cells==N_ratio_str$cells[1]])
    
    ann_text1 <- data.frame(cells=N_ratio_str$cells[(N_ratio_str$gene)==genes[1]], 
                            N_ratio=N_ratio_str$max[N_ratio_str$gene==genes[1]]+0.02, gene=genes[1], 
                            lab=N_ratio_str$p.symbol[N_ratio_str$gene==genes[1]])
    ann_text2 <- data.frame(cells=N_ratio_str$cells[(N_ratio_str$gene)==genes[2]], 
                            N_ratio=N_ratio_str$max[N_ratio_str$gene==genes[2]]+0.02, gene=genes[2], 
                            lab=N_ratio_str$p.symbol[N_ratio_str$gene==genes[2]])
    p+scale_y_continuous(breaks=seq(0, 1, by=0.1), limits=c(0, 1))+
      geom_text(data=ann_text1, aes(label=lab), color="gray30", size=6)+
      geom_text(data=ann_text2, aes(label=lab), color="gray30", size=6)+
      theme(strip.text.x = element_text(size = 12, colour = "red", face="bold", angle = 360), strip.background = element_blank())
  })

  output$downloadcsv <- downloadHandler(
    filename = function() { paste('result_', '.csv', sep='') },
    content = function(file) {
      file.copy("www/qPCR_output.csv", file)
      # write.csv(read.csv("www/qPCR_output.csv", header=TRUE), file, row.names=FALSE)
      }
  )
  
  # output$downloadxls <- downloadHandler(
  #   filename = function() { paste('LHY-171004_data', '.xls', sep='') },
  #   content = function(file) {
  #     file.copy("LHY-171004_data.xls", file)
  #     # write.csv(read.csv("www/qPCR_output.csv", header=TRUE), file, row.names=FALSE)
  #     }
  # )
  
  output$downloadpng <- downloadHandler(
    filename =  function() {
      paste("result", input$type, sep=".")
    },
    # content is a function with argument file. content writes the plot to the device
    content = function(file) {
      if(input$type == "png")
        png(file, type="cairo", width=1400, height=1000, res=200)
        # Cairo::CairoPNG(, width=1400, height=1000, res=200) # open the png device
      else
        pdf(file, width=7, height=5) # open the pdf device
      raw_data <- raw_data_generate()
    colnames(raw_data) = c("samples", "gene", "CT")
    raw_data$CT = as.numeric(raw_data$CT)
    
    cells <- substr(raw_data[, 1], c(1), nchar(raw_data[, 1])-2)
    fractions <- substr(raw_data[, 1], nchar(raw_data[, 1]), 
                        nchar(raw_data[, 1]))
    seq_C <- which(fractions=="C")
    seq_N <- which(fractions=="N")
    N_ratio <- 2^(raw_data$CT[seq_C] - raw_data$CT[seq_N])/
      (2^(raw_data$CT[seq_C] - raw_data$CT[seq_N])+1)
    NC_ratio <- 2^(raw_data$CT[seq_C] - raw_data$CT[seq_N])
    cleaned_data <- data.frame(cells=cells[seq_C], gene=raw_data$gene[seq_C], N_ratio, NC_ratio)
    
    
    N_ratio_str <- ddply(cleaned_data, .(gene, cells), 
                         function(x) data.frame(mean=mean(x$N_ratio),
                                                min=min(x$N_ratio), max=max(x$N_ratio), se=sd(x$N_ratio), 
                                                p.value=t.test(log2(x$NC_ratio), mu=0)$p.value))
    N_ratio_str <- data.frame(N_ratio_str, 
                              p.symbol=sapply(N_ratio_str$p.value, 
                                              function(x) if (x<0.001) return("***") 
                                              else if (x<0.01) return("**") 
                                              else if(x<0.05) return("*") 
                                              else return("")))
    p <- ggplot(cleaned_data, aes(cells, N_ratio, fill=cells, colour=cells))+
      geom_boxplot(alpha=.3, width=.5)+facet_wrap(~gene)+
      geom_dotplot(binaxis='y', stackdir='center', dotsize=.5)+
      theme_bw()+theme(legend.position="none")+
      xlab("")+ylab(expression(paste("NUC/", "(NUC+CYT)")))+
      theme(axis.text.x=element_text(face="bold", size=8), 
            axis.text.y=element_text(face="bold", size=12), 
            axis.title.y=element_text(size=13, face="bold"))+
      geom_hline(yintercept=0.5, lwd=1, lty=2, color="red")
    
    genes <- as.character(N_ratio_str$gene[N_ratio_str$cells==N_ratio_str$cells[1]])
    
    ann_text1 <- data.frame(cells=N_ratio_str$cells[(N_ratio_str$gene)==genes[1]], 
                            N_ratio=N_ratio_str$max[N_ratio_str$gene==genes[1]]+0.02, gene=genes[1], 
                            lab=N_ratio_str$p.symbol[N_ratio_str$gene==genes[1]])
    ann_text2 <- data.frame(cells=N_ratio_str$cells[(N_ratio_str$gene)==genes[2]], 
                            N_ratio=N_ratio_str$max[N_ratio_str$gene==genes[2]]+0.02, gene=genes[2], 
                            lab=N_ratio_str$p.symbol[N_ratio_str$gene==genes[2]])
    p <- p+scale_y_continuous(breaks=seq(0, 1, by=0.1), limits=c(0, 1))+
      geom_text(data=ann_text1, aes(label=lab), color="gray30", size=6)+
      geom_text(data=ann_text2, aes(label=lab), color="gray30", size=6)+
      theme(strip.text.x = element_text(size = 12, colour = "red", face="bold", angle = 360), strip.background = element_blank())

    print(p)
    dev.off()  # turn the device off
    
    }) 


})
