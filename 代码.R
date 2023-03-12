#导入安装包
if(!require(Rmisc))install.packages('Rmisc')
if(!require(PMCMRplus))install.packages('PMCMRplus')
if(!require(tidyverse))install.packages('tidyverse')
if(!require(readxl))install.packages('readxl')
if(!require(rstatix))install.packages('rstatix')
if(!require(rstatix))install.packages('openxlsx')
library(openxlsx)
#设置根目录
setwd("D:\\分析相关\\3月\\3.10SPSS R学习作图")
#读取数据
data <- read.xlsx('data.xlsx', sheet = 1)
head(data)
class(data)
dput(names(data))
data2 <- 
  data %>% 
  pivot_longer(cols = 3:9,names_to = "time",values_to = "G") %>% 
  mutate_if(is.character, as.factor)


# 双因素重复测量方差分析计算：球形检验和校正会自动进行
res.aov <- anova_test(data = data2, dv = G, wid = No, 
                      within = time, between = Group)
get_anova_table(res.aov) 

library(PMCMRplus)
summary(lsdTest(G ~ Group, data = data2))



# rgb(236,195,143,maxColorValue = 255)
mycol <- c("#0000FF", "#FF0000","#00FF00", "purple","orange","black","#A65628")
myshape <- c(21,22,24,25,23,19,15)
library(tidyverse)
library(ggpubr)
library(rstatix)
library(Rmisc)
res <- summarySE(data2, measurevar="G", groupvars=c("Group","time"))
res

ggplot(res, aes(x=time, y=G, group=Group, colour=Group)) +
  geom_errorbar(aes(ymin=G, 
                    ymax=G+se), size=1,width=.1)+
  geom_line(aes(),size=1)+ geom_point(aes(fill=Group,shape=Group),size=4) +
  xlab("") +
  ylab("") +
  scale_colour_manual(values = mycol) +
  scale_fill_manual(values = mycol)+
  scale_shape_manual(values =myshape)+
  # guides(shape = guide_legend(override.aes = list(size = 5)),
  #        linetype=guide_legend(override.aes = list(size = 5)))+
  #  
  
  theme_classic() +
  theme(legend.text = element_text(size = 15, face = "bold"),
        legend.title =element_blank()
        ,legend.position ="right"
        ) 
