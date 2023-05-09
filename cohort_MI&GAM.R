setwd("C:/Users/54437/Desktop/paper")
library(tidyverse)
library(readxl)
library(data.table)
library(RColorBrewer)
library(wesanderson)
library(mgcv)
library(ggpmisc)
library(ggpubr)

##读取数据
ct_throat <- read_xlsx("raw_data.xlsx",sheet = 1)
ct_nose <- read_xlsx("raw_data.xlsx",sheet = 2)
compare <- read_xlsx("raw_data.xlsx",sheet = 3)

##查看数据基本情况
summary(ct_throat)
summary(ct_nose)

##鼻拭子、咽拭子箱式图比较
ggplot(compare,aes(x=group,y=Ct,fill=group,color=group))+
  geom_boxplot(width=0.8,color="black")+
  scale_y_reverse(limits=c(40,15),breaks=c(40,35,30,25,20,15), labels=c("(-)","35","30","25","20","15")) +
  scale_x_discrete(breaks=c("b","y"),labels = c("Nasal Swabs", "Throat Swabs"))

##咽拭子查看每个人的情况
fig_ct_person <- ct_throat %>% 
  ggplot(aes(x=DAY, y=Ct)) + 
  geom_line(aes(color=ID),size=1.2, alpha=0.35) + 
  scale_y_reverse(limits=c(40,15),breaks=c(40,35,30,25,20,15), labels=c("(-)","35","30","25","20","15")) +
  scale_x_continuous(breaks=seq(from=-3,to=9,by=1)) + 
  scale_color_manual(values = wes_palette("Darjeeling1",45,type="continuous"))+
  theme_minimal() + 
  theme(legend.position="none", text=element_text(size=14)) + 
  labs(x="Days since Onset of Symptoms(Throat Swabs) for each person", y="Ct")+
  facet_wrap(~ID)

fig_ct_person


##鼻拭子查看每个人的情况（以症状发生当天为0）
fig_ct_nose_person <- ct_nose %>% 
  ggplot(aes(x=DAY, y=Ct)) + 
  geom_line(aes(color=ID),size=1.2, alpha=0.35) + 
  scale_y_reverse(limits=c(40,15),breaks=c(40,35,30,25,20,15), labels=c("(-)","35","30","25","20","15")) +
  scale_x_continuous(breaks=seq(from=-3,to=9,by=1)) + 
  scale_color_manual(values = wes_palette("Darjeeling1",45,type="continuous"))+
  theme_minimal() + 
  theme(legend.position="none", text=element_text(size=14)) + 
  labs(x="Days since Onset of Symptoms(Nasal Swabs) for each person", y="Ct")+
  facet_wrap(~ID)

fig_ct_nose_person


##compare图,配对连线（文章中的figA）
compare_ct<- compare %>% 
  ggplot(aes(x=group,y=Ct,color=group)) + 
  labs(x=" ", y="Ct Value")+
  theme_classic()+
  geom_line(aes(group=pair),color="gray",position = position_dodge(0.2)) + 
  geom_point(aes(fill=group,group=pair),
             size=2,
             position=position_dodge(0.2))+
  scale_y_reverse(limits=c(40,15),
                  breaks=c(40,35,30,25,20,15),
                  labels=c("(-)","35","30","25","20","15"),
                  sec.axis = sec_axis(~.*-0.2976+14.631,
                                      name = 'Log10 viral loads(copies/ml)'))+
  theme_minimal() + 
  theme(legend.position="none", text=element_text(size=14))+
  scale_x_discrete(breaks=c("b","y"),labels = c("Nasal Swabs", "Throat Swabs"))+
  stat_compare_means(comparisons=list(c("b","y")),paired=TRUE,method="wilcox.test")

compare_ct

ggsave(
  filename = "鼻、咽拭子结果对比.jpg",
  width = 7,             # 宽
  height = 7,            # 高
  units = "in",          # 单位
  dpi = 300              # 分辨率DPI
)


##缺失值插补（采用多重插补）
library(mice)
library(magrittr)

ic(ct_throat)                                #查看含有缺失值的行
ic(ct_nose)

md.pattern(ct_throat,rotate.names = T)       #可视化缺失情况
md.pattern(ct_nose,rotate.names = T)

bu_ct_throat <- mice(ct_throat,m=20,maxit = 5,method="rf",seed=12345) 
bu_ct_nose <- mice(ct_nose,m=20,maxit = 5,method="rf",seed=12345) 

summary(bu_ct_throat)
summary(bu_ct_nose)

stripplot(bu_ct_throat,Ct~.imp,pch=20,cex=2) #查看插补情况
stripplot(bu_ct_nose,Ct~.imp,pch=20,cex=2)
                 
bu_ct_throat$imp$Ct                          #审查插补情况
bu_ct_nose$imp$Ct

complete_ct_throat <- complete(bu_ct_throat,4) #选择合适插补数据
complete_ct_nose <- complete(bu_ct_nose,4)

##完整数据拟合gam
complete_ct_throat$group <- c("y")
complete_ct_nose$group <- c("b")

all <- rbind(complete_ct_throat,complete_ct_nose)
all_1 <-as.data.table(filter(all,all$DAY>=0))   #筛选天数大于等于0

all_1 <- read_xlsx("all_1.xlsx",sheet=1)        #读取研究所用的插补后的完整数据

##绘图前确认gam模型参数
M1 <- gam(all_1 $Ct~s(all_1 $DAY,fx=F,bs="cr"))  #分别构建模型1/2/3
M2 <- gam(all_1 $Ct~s(all_1 $DAY,fx=F,bs="cs"))
M3 <- gam(all_1 $Ct~s(all_1 $DAY,fx=F,bs="tp"))

show<- par(mfrow=c(2,2))  #拼图
plot(M1,se=T)
plot(M2,se=T)
plot(M3,se=T)

summary(M1)
summary(M2)
summary(M3)

#选择M2
anova(M1,M2,M3,test="F")
AIC(M1)
AIC(M2)
AIC(M3)

##gam绘图（文章中的figB）
fig_ct_all <- all_1 %>% 
  ggplot(aes(x=DAY, y=Ct)) + 
  geom_line(aes(color=group,group=ID), alpha=0.2) + 
  geom_smooth(aes(color=group),alpha=.2,
              method="gam",formula = y~s(x,bs="cs"),
              size=1.1,se=F,fullrange=TRUE)+
  scale_y_reverse(limits=c(40,15),
                  breaks=c(40,35,30,25,20,15),
                  labels=c("(-)","35","30","25","20","15"),
                  sec.axis = sec_axis(~.*-0.2976+14.631,
                                      name = 'Log10 viral loads(copies/ml)'))+
  scale_x_continuous(breaks=seq(from=0,to=12,by=1)) + 
  scale_color_manual(values = c("#F8776D","#25C1C5"))+
  theme_minimal() + 
  theme(legend.position="none", text=element_text(size=14))+
  labs(x="Days since Onset of Symptoms", y="Ct Value")+
  theme(legend.position = 'right',legend.direction = 'vertical')+
  scale_colour_discrete(breaks=c("y","b"),labels=c("Throat Swabs","Nasal Swabs"))+
  theme(legend.position = c(1,1),legend.justification = c(1,1),legend.title = element_blank(),legend.text=element_text(size=12))

fig_ct_all

ggsave(
  filename = "鼻咽拭子gam图.jpg",
  width = 7,             # 宽
  height = 7,            # 高
  units = "in",          # 单位
  dpi = 300              # 分辨率DPI
)


##提取完整数据拟合gam模型构建参数
shuzhi <-ggplot_build(fig_ct_all)$data[[2]]
shuzhi$y <- -shuzhi$y

shuzhi_nose <- as.data.table(filter(shuzhi,shuzhi$group==1))
shuzhi_throat <- as.data.table(filter(shuzhi,shuzhi$group==2))

##未插补数据的gam模型拟合
compare_filter <- filter(compare,compare$DAY>=0)

fig_ct_all_2 <- compare_filter %>% 
  ggplot(aes(x=DAY, y=Ct)) + 
  geom_line(aes(color=group,group=class), alpha=0.2) + 
  geom_smooth(aes(color=group),alpha=.2,
              method="gam",formula = y~s(x,bs="cs"),
              size=1.1,se=F,fullrange=TRUE)+
  scale_y_reverse(limits=c(40,15),
                  breaks=c(40,35,30,25,20,15),
                  labels=c("(-)","35","30","25","20","15"),
                  sec.axis = sec_axis(~.*-0.2976+14.631,
                                      name = 'Log10 viral loads(copies/ml)'))+
  scale_x_continuous(breaks=seq(from=0,to=12,by=1)) + 
  scale_color_manual(values = c("#F8776D","#25C1C5"))+
  theme_minimal() + 
  theme(legend.position="none", text=element_text(size=14))+
  labs(x="Days since Onset of Symptoms", y="Ct Value")+
  theme(legend.position = 'right',legend.direction = 'vertical')+
  scale_colour_discrete(breaks=c("y","b"),labels=c("Throat Swabs","Nasal Swabs"))+
  theme(legend.position = c(1,1),legend.justification = c(1,1),legend.title = element_blank(),legend.text=element_text(size=12))

fig_ct_all_2

ggsave(
  filename = "鼻、咽拭子未插补gam图.jpg",
  width = 7,             # 宽
  height = 7,            # 高
  units = "in",          # 单位
  dpi = 300              # 分辨率DPI
)


