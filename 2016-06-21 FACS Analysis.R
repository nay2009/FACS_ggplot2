library(ggplot2)
library(gridExtra)
library(reshape2)

## Import frequency and MFI data from IBD Drive---------------------
setwd("/Volumes/IBD/Yudanin/FACS/FACS Analysis/3-1B")

frequencydata <- read.delim("/Volumes/IBD/Yudanin/FACS/FACS Analysis/3-1B/3-1B Combined Frequencies.txt", na.strings="")
mfidata <- read.delim("/Volumes/IBD/Yudanin/FACS/FACS Analysis/3-1B/3-1B Combined MFI.txt", na.strings="")

## Format matrix column names and set variables as factors or numeric---------------------
ColNames <- read.csv("3-1B ColNames.csv", header=FALSE)
colnames(frequencydata) <- ColNames[[1]]
colnames(mfidata) <- ColNames[[2]][1:14]

frequencydata$Time <- as.factor(frequencydata$Time)
frequencydata$Replicate <- as.factor(frequencydata$Replicate)

mfidata$Time <- as.factor(mfidata$Time)
mfidata$Replicate <- as.factor(mfidata$Replicate)
mfidata$ILC2 <- as.numeric(mfidata$ILC2)
mfidata$Treg <- as.numeric(mfidata$Treg)
mfidata[,14] <- as.numeric(mfidata[,14])


## Reshape frequency and MFI data to long format (melt) and divide into individual experiments ---------------------
frequencydata <- melt(frequencydata, 
                 id.vars = c("Experiment","Sample", "Time", "Tissue","Diet","Replicate", "Frequency" ),
                 variable.name = "Population", 
                 value.name = "Percent")
frequencydata$CombinedGroup <- paste0("%",frequencydata$Frequency," ",frequencydata$Population)
frequencydata$Group <- paste0(frequencydata$Tissue," ",frequencydata$Diet)
frequencydata1 <- frequencydata[grep("3-1B-1", frequencydata$Experiment),]
frequencydata2 <- frequencydata[grep("3-1B-2", frequencydata$Experiment),]
frequencydata2 <- frequencydata2[-grep("Lung", frequencydata2$Tissue),]
frequencydata <- frequencydata[-grep("Lung", frequencydata$Tissue),]

mfidata <- melt(mfidata, 
                      id.vars = c("Experiment","Sample", "Time", "Tissue","Diet","Replicate", "MFI" ),
                      variable.name = "Population", 
                      value.name = "Intensity")
mfidata$Group <- paste0(mfidata$MFI," MFI ",mfidata$Tissue," ",mfidata$Population)
mfidata$CombinedGroup <- paste0(mfidata$MFI," MFI ",mfidata$Population)
mfidata1 <- mfidata[grep("3-1B-1", mfidata$Experiment),]
mfidata2 <- mfidata[grep("3-1B-2", mfidata$Experiment),]
mfidata2 <- mfidata2[-grep("Lung", mfidata2$Tissue),]
mfidata <- mfidata[-grep("Lung", mfidata$Tissue),]

## Graph individual experiment frequencies  ---------------------
cairo_pdf(filename="3-1B-1 Frequencies Over Time.pdf", w=15, h=10)
ggplot(data=frequencydata1, aes(x=Time, y=Percent, fill=Diet, color=Diet)) +
  facet_wrap( Tissue ~ CombinedGroup, scales="free_y", ncol=9) +
  stat_summary (fun.y = "mean", geom = "bar", position = position_dodge(0.9), width=0.8) +
  scale_color_manual(values = c(Control= "#212121", Fat="#0F3919")) +
  scale_fill_manual(values = c(Control= "#d4d4d4", Fat="#269040")) +
  geom_jitter(position=position_dodge(0.9), shape=21, fill="white", size=2) +
  labs(title="3-1B-1 Frequencies Over Time") +
  theme(legend.position ='none',
      strip.background = element_blank(),
      panel.background = element_rect(color="#E5E5E5", linetype="solid", fill=NA),
      axis.title.y = element_blank(),
      plot.title = element_text(size = rel(2)))
dev.off()

cairo_pdf(filename="3-1B-1 Combined Frequencies.pdf", w=20, h=10)
ggplot(data=frequencydata1, aes(x=Tissue, y=Percent, fill=Diet, color=Diet)) +
  facet_wrap(  ~ CombinedGroup, scales="free_y", ncol=9) +
  stat_summary (fun.y = "mean", geom = "bar", position = position_dodge(0.9), width=0.8) +
  scale_color_manual(values = c(Control= "#212121", Fat="#0F3919")) +
  scale_fill_manual(values = c(Control= "#d4d4d4", Fat="#269040")) +
  geom_jitter(position=position_dodge(0.9), shape=21, fill="white", size=2) +
  labs(title="3-1B-1 Combined Frequencies") +
  theme(legend.position ='none',
        strip.background = element_blank(),
        panel.background = element_rect(color="#E5E5E5", linetype="solid", fill=NA),
        axis.title.y = element_blank(),
        plot.title = element_text(size = rel(2)))
dev.off()


cairo_pdf(filename="3-1B-2 Frequencies Over Time.pdf", w=20, h=10)
ggplot(data=frequencydata2, aes(x=Time, y=Percent, fill=Diet, color=Diet)) +
  facet_wrap(Tissue  ~ CombinedGroup, scales="free_y", ncol=9) +
  stat_summary (fun.y = "mean", geom = "bar", position = position_dodge(0.9), width=0.8) +
  scale_color_manual(values = c(Control= "#212121", Fat="#0F3919")) +
  scale_fill_manual(values = c(Control= "#d4d4d4", Fat="#269040")) +
  geom_jitter(position=position_dodge(0.9), shape=21, fill="white", size=2) +
  labs(title="3-1B-2 Frequencies Over Time") +
  theme(legend.position ='none',
        strip.background = element_blank(),
        panel.background = element_rect(color="#E5E5E5", linetype="solid", fill=NA),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        plot.title = element_text(size = rel(2)))
dev.off()

cairo_pdf(filename="3-1B-2 Combined Frequencies.pdf", w=15, h=5)
ggplot(data=frequencydata2, aes(x=Tissue, y=Percent, fill=Diet, color=Diet)) +
  facet_wrap(  ~ CombinedGroup, scales="free_y", ncol=9) +
  stat_summary (fun.y = "mean", geom = "bar", position = position_dodge(0.9), width=0.8) +
  scale_color_manual(values = c(Control= "#212121", Fat="#0F3919")) +
  scale_fill_manual(values = c(Control= "#d4d4d4", Fat="#269040")) +
  geom_jitter(position=position_dodge(0.9), shape=21, fill="white", size=2) +
  labs(title="3-1B-2 Combined Frequencies") +
  theme(legend.position ='none',
        strip.background = element_blank(),
        panel.background = element_rect(color="#E5E5E5", linetype="solid", fill=NA),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        plot.title = element_text(size = rel(2)))
dev.off()

## Graph individual experiment MFI  ---------------------
cairo_pdf(filename="3-1B-1 KLRG1 MFI Over Time.pdf", w=15, h=5)
ggplot(data=mfidata1[mfidata1$MFI=="KLRG1",], aes(x=Time, y=Intensity, fill=Diet, color=Diet)) +
  facet_wrap(   ~ Group, scales="free_y", ncol=7) +
  stat_summary (fun.y = "mean", geom = "bar", position = position_dodge(0.9), width=0.8) +
  scale_color_manual(values = c(Control= "#212121", Fat="#0F3919")) +
  scale_fill_manual(values = c(Control= "#d4d4d4", Fat="#269040")) +
  geom_jitter(position=position_dodge(0.9), shape=21, fill="white", size=2) +
  labs(title="3-1B-1 MFI Over Time") +
  theme(legend.position='none',
        strip.background = element_blank(),
        panel.background = element_rect(color="#E5E5E5", linetype="solid", fill=NA),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        plot.title = element_text(size = rel(2)))
dev.off()

cairo_pdf(filename="3-1B-1 Combined MFI.pdf", w=20, h=8)
ggplot(data=mfidata1, aes(x=Tissue, y=Intensity, fill=Diet, color=Diet)) +
  facet_wrap(   ~ CombinedGroup, scales="free_y", ncol=7) +
  stat_summary (fun.y = "mean", geom = "bar", position = position_dodge(0.9), width=0.8) +
  scale_color_manual(values = c(Control= "#212121", Fat="#0F3919")) +
  scale_fill_manual(values = c(Control= "#d4d4d4", Fat="#269040")) +
  geom_jitter(position=position_dodge(0.9), shape=21, fill="white", size=2) +
  labs(title="3-1B-1 Combined MFI") +
  theme(legend.position='none',
        strip.background = element_blank(),
        panel.background = element_rect(color="#E5E5E5", linetype="solid", fill=NA),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        plot.title = element_text(size = rel(2)))
dev.off()

cairo_pdf(filename="3-1B-2 MFI Over Time.pdf", w=20, h=15)
ggplot(data=mfidata2, aes(x=Time, y=Intensity, fill=Diet, color=Diet)) +
  facet_wrap(  ~ Group, scales="free_y", ncol=7) +
  stat_summary (fun.y = "mean", geom = "bar", position = position_dodge(0.9), width=0.8) +
  scale_color_manual(values = c(Control= "#212121", Fat="#0F3919")) +
  scale_fill_manual(values = c(Control= "#d4d4d4", Fat="#269040")) +
  geom_jitter(position=position_dodge(0.9), shape=21, fill="white", size=2) +
  labs(title="3-1B-2 MFI Over Time") +
  theme(legend.position ='none',
        strip.background = element_blank(),
        panel.background = element_rect(color="#E5E5E5", linetype="solid", fill=NA),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        plot.title = element_text(size = rel(2)))
dev.off()

cairo_pdf(filename="3-1B-2 Combined MFI.pdf", w=20, h=15)
ggplot(data=mfidata2, aes(x=Tissue, y=Intensity, fill=Diet, color=Diet)) +
  facet_wrap(  ~ CombinedGroup, scales="free_y", ncol=7) +
  stat_summary (fun.y = "mean", geom = "bar", position = position_dodge(0.9), width=0.8) +
  scale_color_manual(values = c(Control= "#212121", Fat="#0F3919")) +
  scale_fill_manual(values = c(Control= "#d4d4d4", Fat="#269040")) +
  geom_jitter(position=position_dodge(0.9), shape=21, fill="white", size=2) +
  labs(title="3-1B-2 Combined MFI") +
  theme(legend.position ='none',
        strip.background = element_blank(),
        panel.background = element_rect(color="#E5E5E5", linetype="solid", fill=NA),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        plot.title = element_text(size = rel(2)))
dev.off()

## Graph combined experiment frequencies and MFI  ---------------------
cairo_pdf(filename="3-1B Total Frequencies.pdf", w=20, h=7)
ggplot(data=frequencydata, aes(x=Tissue, y=Percent, fill=Diet, color=Diet)) +
  facet_wrap(  ~ CombinedGroup, scales="free_y", ncol=9) +
  stat_summary (fun.y = "mean", geom = "bar", position = position_dodge(0.9), width=0.8) +
  scale_color_manual(values = c(Control= "#212121", Fat="#0F3919")) +
  scale_fill_manual(values = c(Control= "#d4d4d4", Fat="#269040")) +
  geom_jitter(position=position_dodge(0.9), shape=21, fill="white", size=2) +
  labs(title="3-1B Total Frequencies") +
  theme(legend.position ='none',
        strip.background = element_blank(),
        panel.background = element_rect(color="#E5E5E5", linetype="solid", fill=NA),
        axis.title.y = element_blank(),
        plot.title = element_text(size = rel(2)))
dev.off()

cairo_pdf(filename="3-1B Total MFI.pdf", w=20, h=10)
ggplot(data=mfidata, aes(x=Tissue, y=Intensity, fill=Diet, color=Diet)) +
  facet_wrap(   ~ CombinedGroup, scales="free_y", ncol=7) +
  stat_summary (fun.y = "mean", geom = "bar", position = position_dodge(0.9), width=0.8) +
  scale_color_manual(values = c(Control= "#212121", Fat="#0F3919")) +
  scale_fill_manual(values = c(Control= "#d4d4d4", Fat="#269040")) +
  geom_jitter(position=position_dodge(0.9), shape=21, fill="white", size=2) +
  labs(title="3-1B Total MFI") +
  theme(legend.position='none',
        strip.background = element_blank(),
        panel.background = element_rect(color="#E5E5E5", linetype="solid", fill=NA),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        plot.title = element_text(size = rel(2)))
dev.off()

## Reshape frequency and MFI data to short format (cast)
castfrequency <- dcast(frequencydata, Experiment+Frequency+Tissue+Population ~ Diet+Time+Replicate, value.var = "Percent")
castmfi <- dcast(mfidata, Experiment+MFI+Tissue+Population ~ Diet+Time+Replicate, value.var = "Intensity")

write.csv(castfrequency, file="3-1B Cast Combined Frequency.csv")
write.csv(castmfi, file="3-1B Cast Combined MFI.csv")

castfrequency1 <- dcast(frequencydata1, Frequency+Tissue+Population ~ Diet+Time+Replicate, value.var = "Percent")
castmfi1 <- dcast(mfidata1, MFI+Tissue+Population ~ Diet+Time+Replicate, value.var = "Intensity")

write.csv(castfrequency1, file="3-1B-1 Cast Combined Frequency.csv")
write.csv(castmfi1, file="3-1B-1 Cast Combined MFI.csv")

