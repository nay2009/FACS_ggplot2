library(ggplot2)
library(gridExtra)
library(reshape2)

## Import frequency and MFI data from IBD Drive---------------------
setwd("~/Google Drive/Weill Cornell/Research/Analysis/FACS")

frequencydata <- read.delim("~/Google Drive/Weill Cornell/Research/Analysis/FACS/Combined Frequencies.txt", na.strings="")
mfidata <- read.delim("~/Google Drive/Weill Cornell/Research/Analysis/FACS/Combined MFI.txt", na.strings="")

## Format matrix column names and set variables as factors or numeric---------------------
ColNames <- read.csv("ColNames.csv", header=FALSE)
colnames(frequencydata) <- ColNames[[1]][1:14]
colnames(mfidata) <- ColNames[[2]][1:23]

frequencydata$Time <- as.factor(frequencydata$Time)
frequencydata$Replicate <- as.factor(frequencydata$Replicate)

mfidata$Time <- as.factor(mfidata$Time)
mfidata$Replicate <- as.factor(mfidata$Replicate)


## Reshape frequency and MFI data to long format (melt) and divide into individual experiments ---------------------
frequencydata <- melt(frequencydata, 
                 id.vars = c("Experiment","Sample", "Time", "Tissue","Diet","Replicate"),
                 variable.name = "Population", 
                 value.name = "Percent")
frequencydata$CombinedGroup <- paste0("%",frequencydata$Frequency," ",frequencydata$Population)
frequencydata$Group <- paste0(frequencydata$Tissue," ",frequencydata$Diet)

mfidata <- melt(mfidata, 
                      id.vars = c("Experiment","Sample", "Time", "Tissue","Diet","Replicate","Population"),
                      variable.name = "MFI", 
                      value.name = "Intensity")
mfidata$CombinedGroup <- paste0(mfidata$MFI," MFI ",mfidata$Population)
mfidata$Group <- paste0(mfidata$MFI," MFI ",mfidata$Tissue," ",mfidata$Population)
mfidata1 <- mfidata[-grep("Ob",mfidata$Diet),]
mfidata1 <- mfidata1[-grep("MWAT",mfidata$Tissue),]
## Graph combined experiment frequencies and MFI  ---------------------
cairo_pdf(filename="Total Frequencies.pdf", w=10, h=20)
ggplot(data=frequencydata, aes(x=Tissue, y=Percent, fill=Diet, color=Diet)) +
  facet_wrap(  ~ CombinedGroup, scales="free_y", ncol=2) +
  stat_summary (fun.y = "mean", geom = "bar", position = position_dodge(0.9), width=0.8) +
  scale_color_manual(values = c(Control= "#212121", Fat="#0F3919", Ob="#061f81")) +
  scale_fill_manual(values = c(Control= "#d4d4d4", Fat="#269040", Ob="#4266F6")) +
  geom_jitter(position=position_dodge(0.9), shape=21, fill="white", size=2) +
  labs(title="3-1 Total Frequencies") +
  theme(legend.position ='none',
        strip.background = element_blank(),
        panel.background = element_rect(color="#E5E5E5", linetype="solid", fill=NA),
        axis.title.y = element_blank(),
        plot.title = element_text(size = rel(2)))
dev.off()

cairo_pdf(filename="Combined MFI.pdf", w=10, h=15)
ggplot(data=mfidata1[-grep("CD4T", mfidata1$Population),], aes(x=Population, y=Intensity, fill=Diet, color=Diet)) +
  facet_wrap(  ~ MFI, scales="free_y", ncol=4) +
  stat_summary (fun.y = "mean", geom = "bar", position = position_dodge(0.9), width=0.8) +
  scale_color_manual(values = c(Control= "#212121", Con= "#212121", Fat="#0F3919", Ob="#061f81")) +
  scale_fill_manual(values = c(Control= "#d4d4d4", Con= "#d4d4d4", Fat="#269040",Ob="#4266F6")) +
  geom_jitter(position=position_dodge(0.9), shape=21, fill="white", size=2) +
  labs(title="3-1 Total MFI") +
  theme(legend.position='none',
        strip.background = element_blank(),
        panel.background = element_rect(color="#E5E5E5", linetype="solid", fill=NA),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        plot.title = element_text(size = rel(2)))
dev.off()

## Reshape frequency and MFI data to short format (cast)
castfrequency <- dcast(frequencydata, Experiment+Tissue+Population ~ Diet+Time+Replicate, value.var = "Percent")
castmfi <- dcast(mfidata, Experiment+Diet+Time+Tissue+Population+MFI ~ Replicate, value.var = "Intensity")

write.csv(castfrequency, file="3-1B Cast Combined Frequency.csv")
write.csv(castmfi, file="Cast Combined MFI.csv")

castfrequency1 <- dcast(frequencydata1, Frequency+Tissue+Population ~ Diet+Time+Replicate, value.var = "Percent")
castmfi1 <- dcast(mfidata1, MFI+Tissue+Population ~ Diet+Time+Replicate, value.var = "Intensity")

write.csv(castfrequency1, file="3-1B-1 Cast Combined Frequency.csv")
write.csv(castmfi1, file="3-1B-1 Cast Combined MFI.csv")

