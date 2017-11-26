setwd("E:\\r\\qPCR")
require(ggplot2)
require(plyr)

# library("xlsx")
# read xlxs data
file <- "LHY-A549-1026_data.csv"
# file <- "LHY-171004_data.xls"
# raw_data <- read.xlsx(file=file, sheetIndex = 2,
#   header=TRUE, encoding="UTF-8", stringsAsFactors=FALSE)
raw_data <- read.csv(file, header=TRUE, stringsAsFactors=FALSE)
raw_data <- raw_data[, c(1, 2, 3)]
colnames(raw_data) = c("samples", "gene", "CT")
raw_data$CT = as.numeric(raw_data$CT)

# clean data
cells <- substr(raw_data[, 1], c(1), nchar(raw_data[, 1])-2)
fractions <- substr(raw_data[, 1], nchar(raw_data[, 1]), 
                    nchar(raw_data[, 1]))
seq_C <- which(fractions=="C")
seq_N <- which(fractions=="N")
N_ratio <- 2^(raw_data$CT[seq_C] - raw_data$CT[seq_N])/
  (2^(raw_data$CT[seq_C] - raw_data$CT[seq_N])+1)
NC_ratio <- 2^(raw_data$CT[seq_C] - raw_data$CT[seq_N])
cleaned_data <- data.frame(cells=cells[seq_C], gene=raw_data$gene[seq_C], N_ratio, NC_ratio)

# use t-test for qPCR
# reference: https://guangchuangyu.github.io/statistics_notes/section-12.html#rt-pcr

# construct plot data

N_ratio_str <- ddply(cleaned_data, .(gene, cells), 
                     function(x) data.frame(mean=mean(x$N_ratio),
                                            max=max(x$N_ratio), se=sd(x$N_ratio), 
                                            p.value=t.test(log2(x$NC_ratio), mu=0)$p.value))
N_ratio_str <- data.frame(N_ratio_str, 
                          p.symbol=sapply(N_ratio_str$p.value, 
                                          function(x) if (x<0.001) return("***") 
                                          else if (x<0.01) return("**") 
                                          else if(x<0.05) return("*") 
                                          else return("")))

# Output structured data
# write.xlsx(N_ratio_str, 
#            file=paste(unlist(strsplit(file, "[.]"))[1],
#                       "_qPCR_output.xlsx", sep=''), 
#            sheetName="data_output", row.names=FALSE)
write.csv(N_ratio_str, 
           file=paste(unlist(strsplit(file, "[.]"))[1],
                      "_qPCR_output.csv", sep=''), row.names=FALSE)
# plot
# png(paste(unlist(strsplit(file, "[.]"))[1], "_qPCR.png", sep=''), type="cairo", width=1400, height=1000, res=200)
Cairo::CairoPNG(paste(unlist(strsplit(file, "[.]"))[1], "_qPCR.png", sep=''), width=1400, height=1000, res=200)

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
  geom_text(data=ann_text2, aes(label=lab), color="gray30", size=6)

# dev.off()

