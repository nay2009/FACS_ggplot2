library(ggplot2)
library(gridExtra)
library(reshape2)

CD45data <- read.delim("~/Desktop/CD45.txt", stringsAsFactors=FALSE)
Parentdata <- read.delim("~/Desktop/Parent.txt", stringsAsFactors=FALSE)

ColData <- read.delim("~/Desktop/ColData.txt")
ColData <- rbind(ColData,ColData)
ColData$Group <- c(rep("CD45+", 55), rep("Parent", 55))

ColNames <- read.delim("~/Desktop/ColNames.txt", header=FALSE)
colnames(CD45data) <- ColNames[[1]]
colnames(Parentdata) <- ColNames[[1]]

FACSdata <- rbind.data.frame(CD45data, Parentdata)
FACSdata <- cbind.data.frame(ColData, FACSdata)

FACSdata <- melt(FACSdata, 
                 id.vars = c("Time","Replicate","Experiment","Tissue","Diet", "Group" ),
                 variable.name = "Population", 
                 value.name = "Frequency")

FACSdata$Time <- as.factor(FACSdata$Time)
FACSdata$Replicate <- as.factor(FACSdata$Replicate)
FACSdata$Group <- as.factor(FACSdata$Group)
FACSdata$GroupTissuePopulation <- as.factor(paste(FACSdata$Group, FACSdata$Tissue, FACSdata$Population, sep=" "))

cairo_pdf(filename="3-1A Frequencies.pdf", w=20, h=10)
ggplot(data=FACSdata, aes(x=Time, y=Frequency, color=Diet, fill=Diet)) +
  facet_wrap(  ~ GroupTissuePopulation, scales="free_y", ncol=length(levels(FACSdata$Population))) +
  stat_summary (fun.y = "mean", geom = "bar", position = position_dodge()) +
  scale_fill_manual(values = c(Control= "#707070", Fat="#269040")) +
  scale_color_manual(values = c(Control= "#707070", Fat="#269040")) +
  geom_jitter(position=position_jitterdodge(0.9), shape=21, fill="white") +
  labs(title="3-1A Frequencies", x="Time (weeks)", y= "Percent") +
  theme(legend.position='none',
      strip.background = element_blank(),
      panel.background = element_rect(color="#E5E5E5", linetype="solid", fill=NA),
      panel.grid.major = element_line(color="#E5E5E5", linetype="dotted"),
      axis.title.y = element_blank(),
      plot.title = element_text(size = rel(2)))
dev.off()

