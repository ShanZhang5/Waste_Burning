install.packages(
  'pacman'
)
library(pacman)
library(dummies)
library(plm)
library(ggplot2)
library(scales)
library(here)
library(ggalt)
p_load(data.table, fst, here, plyr, tidyverse)


#upload the data
ipumsi_00001 <- fread(here::here("ipumsi_00009.csv"), nThread=4)
Brazil_city_data_update <- read.csv(here::here("data/Brazil_city_data_update.csv"))


#################################################################################################################

#===================================================
#data cleaning
#===================================================

#count a particular city
ipumsi_00001 %>% 
  dplyr::filter(METROBR==030 & TRASH==22) %>%
  tally()

rm(list=ls())

#Count obervations for each metropolitan area
city_counts<-ipumsi_00001 %>% 
  dplyr::group_by(METROBR) %>% 
  tally() %>% 
  dplyr::rename(count = n)

#Count observations who use burning for trash disposal in metropolitan areas
tash_counts<-ipumsi_00001 %>% 
  dplyr::group_by(METROBR)  %>% 
  dplyr::filter(TRASH==22)%>% 
  tally() %>% 
  dplyr::rename(count = n)

#Count obervations who have sewage access
sewage_counts <-ipumsi_00001%>% 
  dplyr::group_by(METROBR)  %>% 
  dplyr::filter(SEWAGE==11 | SEWAGE==12)%>% 
  tally() %>% 
  dplyr::rename(count = n)

#Include NA info out of the income data
income_000001 <- subset(ipumsi_00001,INCEARN != 99999999 & INCEARN !=99999998 )

#Devide data into metropolitan level and take average for each metropolitan
avg_income <-income_000001 %>% 
  dplyr::group_by(METROBR) %>%
  summarize(count=n(),INCEARN=mean(INCREARN))
#avg_income_city <- summarize(avg_income, count = n(), INCEARN = mean(INCEARN))

#Average children survival rate in metropolitan areas
avg_chsurv_city <-ipumsi_00001 %>% 
  filter(CHSURV != 99 & CHSURV !=98 ) %>% 
  dplyr::group_by(METROBR)%>%
  summarize(count = n(), CHSURV = mean(CHSURV))

#Average years of school in metropolitan areas
avg_years_school <-ipumsi_00001 %>% 
  filter( YRSCHOOL < 90 ) %>% 
  dplyr::group_by(METROBR)%>%
  summarize( count = n(), YRSCHOOL = mean(YRSCHOOL))


#Created burning as a dummy variable 
ipumsi_00001 = ipumsi_00001 %>%
  mutate( BURN = ifelse(TRASH == 22 , 
                        1 , 
                        0))

# Average education attainment in metropolitan areas
avg_education <-ipumsi_00001 %>% 
  filter(EDATTAIN != 0 & EDATTAIN !=9) %>%
  dplyr::group_by(METROBR)%>%
  summarize( count = n(), EDATTAIN = mean(EDATTAIN))

# Count disabilities 
blind_counts<-ipumsi_00001 %>% 
  dplyr::group_by(METROBR)  %>%
  dplyr::filter(DISBLND==1)%>% 
  tally() %>% 
  dplyr::rename(count = n)

deaf_counts<- ipumsi_00001 %>% 
  dplyr::group_by(METROBR)  %>% 
  dplyr::filter(DISDEAF==1)%>% 
  tally() %>% 
  dplyr::rename(count = n)

mentaldis_countplms<- ipumsi_00001 %>% 
  dplyr::group_by(METROBR)  %>% 
  dplyr::filter(DISMNTL==1)%>% 
  tally() %>% 
  dplyr::rename(count = n)

birthdeath_counts<- ipumsi_00001 %>% 
  dplyr::group_by(METROBR)  %>% 
  dplyr::filter(LASTBMORT==2)%>% 
  tally() %>% 
  dplyr::rename(count = n)

#Organize clean data into one folder
fwrite(avg_education,here::here("clean_data","avg_ed.csv"))

#After getting clean data, remove the raw data
#rm(impum_0001)
#===========================================================================
### Metropolitan level
#===========================================================================
#Create plot of average income and burning percentage in metropolitan areas
plot(Brazil_city_data_update$AVG_INCOME, Brazil_city_data_update$BURN_PERC,
     main="Average Income and Trash Burning Percentage within cities", 
     xlab="Average income per month", ylab="Trash burning percentage",
     col= "black", pch = 19, cex = 1, lty = "solid", lwd = 2)
text(Brazil_city_data_update$AVG_INCOME,Brazil_city_data_update$BURN_PERC , labels=Brazil_city_data_update$CITY, cex= 0.7)


#Regression inference
a<-lm(Brazil_city_data_update$BURN_PERC~Brazil_city_data_update$AVG_INCOME)
summary(a)
b<-lm(Brazil_city_data_update$BURN_PERC~Brazil_city_data_update$AVG_INCOME+Brazil_city_data_update$AVG_YEARSCHOOL)
summary(b)
c<-lm(Brazil_city_data_update$BURN_PERC~Brazil_city_data_update$AVG_INCOME+Brazil_city_data_update$AVG_EDUCATION)
summary(c)



#Look at the relationship between average income and children surviving within cities
#children_000001 <- subset(ipumsi_00001,CHSURV != 99 & CHSURV !=98 )



# Plots of open burning and disability
plot(Brazil_city_data_update$AVG_INCOME, Brazil_city_data_update$VISION_PERC,
     col= "black", pch = 19, cex = 1, lty = "solid", lwd = 2)
plot(Brazil_city_data_update$AVG_INCOME, Brazil_city_data_update$HEARING_PERC,
     col= "black", pch = 19, cex = 1, lty = "solid", lwd = 2)
plot(Brazil_city_data_update$AVG_INCOME, Brazil_city_data_update$MENTAL_PERC,
     col= "black", pch = 19, cex = 1, lty = "solid", lwd = 2)
plot(Brazil_city_data_update$BURN_PERC, Brazil_city_data_update$MENTAL_PERC,
     col= "black", pch = 19, cex = 1, lty = "solid", lwd = 2)
plot(Brazil_city_data_update$BURN_PERC, Brazil_city_data_update$VISION_PERC,
     col= "black", pch = 19, cex = 1, lty = "solid", lwd = 2)
plot(Brazil_city_data_update$BURN_PERC, Brazil_city_data_update$HEARING_PERC,
     col= "black", pch = 19, cex = 1, lty = "solid", lwd = 2)

#Plot of open burning and last birth mortality
plot(Brazil_city_data_update$BIRTHMORT_PERC,Brazil_city_data_update$BURN_PERC, 
     main="Last Birth Mortality Rate and Trash Burning Percentage within cities", 
     ylab="Last birth mortality", xlab="Trash burning percentage",
     col= "black", pch = 19, cex = 1, lty = "solid", lwd = 2)
text(Brazil_city_data_update$BIRTHMORT_PERC,Brazil_city_data_update$BURN_PERC,
     labels=Brazil_city_data_update$CITY, cex= 0.7)

#Plot of open burning and children survivial rate 
plot(Brazil_city_data_update$AVG_CHSURV,Brazil_city_data_update$BURN_PERC, 
     main="Average Children Survival per household and Trash Burning Percentage within cities", 
     ylab="Average children survival", xlab="Trash burning percentage",
     col= "black", pch = 19, cex = 1, lty = "solid", lwd = 2)
text(Brazil_city_data_update$AVG_CHSURV,Brazil_city_data_update$BURN_PERC,
     labels=Brazil_city_data_update$CITY, cex= 0.7)

#Plot of open burning and average years of school
plot(Brazil_city_data_update$BURN_PERC, Brazil_city_data_update$AVG_YEARSCHOOL,
     col= "black", pch = 19, cex = 1, lty = "solid", lwd = 2)

#Plot of average years of school and average income 
plot(Brazil_city_data_update$AVG_YEARSCHOOL, Brazil_city_data_update$AVG_INCOME,
     col= "black", pch = 19, cex = 1, lty = "solid", lwd = 2)

#Plot of average income and average children that survived
plot(Brazil_city_data_update$AVG_INCOME, Brazil_city_data_update$AVG_CHSURV,
     col= "black", pch = 19, cex = 1, lty = "solid", lwd = 2)

#Plot of average income and average education attainment
plot(Brazil_city_data_update$AVG_INCOME, Brazil_city_data_update$AVG_EDUCATION,
     col= "black", pch = 19, cex = 1, lty = "solid", lwd = 2)\

#Plot of burning and average education attainment
plot(Brazil_city_data_update$BURN_PERC, Brazil_city_data_update$AVG_EDUCATION,
     col= "black", pch = 19, cex = 1, lty = "solid", lwd = 2)

#Plot of average education attainment and burning percentage
plot(Brazil_city_data_update$AVG_EDUCATION, Brazil_city_data_update$BURN_PERC,
     main="Average Education and Trash Burning Percentage within cities", 
     xlab="Average education", ylab="Trash burning percentage",
     col= "black", pch = 19, cex = 1, lty = "solid", lwd = 2)
text(Brazil_city_data_update$AVG_EDUCATION ,Brazil_city_data_update$BURN_PERC ,
     labels=Brazil_city_data_update$CITY, cex= 0.7)

#Regression inference
d<-lm(Brazil_city_data_update$BURN_PERC~Brazil_city_data_update$AVG_EDUCATION)
summary(d)
e<-lm(Brazil_city_data_update$BURN_PERC~Brazil_city_data_update$AVG_EDUCATION+Brazil_city_data_update$AVG_INCOME)
summary(e)
f<-lm(Brazil_city_data_update$BIRTHMORT_PERC~Brazil_city_data_update$BURN_PERC)
summary(f)

#Regression of last birth mortality rate and burning controlling for income and education level
g<-lm(Brazil_city_data_update$BIRTHMORT_PERC~Brazil_city_data_update$BURN_PERC
      +Brazil_city_data_update$AVG_INCOME
      +Brazil_city_data_update$AVG_EDUCATION)
summary(g)

#Regression of children survival and buring
h<-lm(Brazil_city_data_update$AVG_CHSURV~Brazil_city_data_update$BURN_PERC)
summary(h)

#Regression of children survival and burning controlling income and education
i<-lm(Brazil_city_data_update$AVG_CHSURV~Brazil_city_data_update$BURN_PERC
      +Brazil_city_data_update$AVG_INCOME
      +Brazil_city_data_update$AVG_EDUCATION
      +Brazil_city_data_update$OBSERVATIONS)
summary(i)
#############################################################################################################
#===========================================================================================================
##Individual household level
#===========================================================================================================
#Regression of burning and income adding control variables and metropolitan areas as fixed effect
j<-lm(ipumsi_00001$BURN~ipumsi_00001$INCEARN+ipumsi_00001$EDATTAIN+
        factor(ipumsi_00001$METROBR)+ipumsi_00001$URBAN)
summary(j)

k<-plm(BURN~INCEARN+EDATTAIN,data=ipumsi_00001,index=c('METROBR'),model="within",effect = 'twoways')
summary(k)

blind_counts<-ipumsi_00001 %>% dplyr::group_by(METROBR)  %>% dplyr::filter(DISBLND==1)%>% tally() %>% dplyr::rename(count = n)
deaf_counts<- ipumsi_00001 %>% dplyr::group_by(METROBR)  %>% dplyr::filter(DISDEAF==1)%>% tally() %>% dplyr::rename(count = n)
mentaldis_countplms<- ipumsi_00001 %>% dplyr::group_by(METROBR)  %>% dplyr::filter(DISMNTL==1)%>% tally() %>% dplyr::rename(count = n)
birthdeath_counts<- ipumsi_00001 %>% dplyr::group_by(METROBR)  %>% dplyr::filter(LASTBMORT==2)%>% tally() %>% dplyr::rename(count = n)
#===================================================
#data cleaning/stuff goes here
#===================================================




#time to save my clean data
fst::write_fst(my_clean_data, here("clean_data","clean_data.fst"))
read_fst(here("clean_data","clean_data.fst"))

##Remove the useless data
rm(income_000001)
rm(avg_income_city)
rm(avg_chsurv)
rm(macro_2010)
rm(avg_years_school)
rm(blind_counts)
rm(deaf_counts)


#=======================================================
#Some results from ggplot
#=======================================================
plot_1 <- ggplot(data=Brazil_city_data_update,aes(x=AVG_INCOME, y=BURN_PERC))+
  geom_smooth(method="lm", col="darkseagreen")+
  geom_point(col="steelblue",alpha=0.8)+
  geom_text(data=Brazil_city_data_update,aes(label=CITY),check_overlap = TRUE)
print(plot_1+ggtitle("Open Trash Buring in Brazil Metropolitan Areas")+
        labs(y="Percentage of Open Burning",x="Average Income of the City(USD)"))

plot_2 <- ggplot(data=Brazil_city_data_update,aes(x=SEW_PERC, y=BURN_PERC))+
  geom_point(aes(size=OBSERVATIONS,col=REGION),alpha=0.8)+
  geom_text(aes(label=CITY),check_overlap = TRUE)+
  geom_smooth(method="loess",col="steelblue")
print(plot_2+ggtitle("Open Trash Buring in Brazil Metropolitan Areas")+
        labs(y="Percentage of Open Burning",x="Percentage of Sewage Access"))

theme_set(theme_classic())
theme_set(theme_classic())

# prep data
df <- read.csv("~/Desktop/Assignment_1/data/Burning_2010_vs_2000.csv")
dfs<-subset(df,X<=200)
left_label <- paste(dfs$CITY)
right_label <- paste(dfs$CITY)
dfs$class <- ifelse((dfs$BURNING_PERC_2010 - dfs$BURNING_PERC_2000) < 0, "red", "green")

# Plot
p <- ggplot(dfs) + geom_segment(aes(x=1, xend=2, y=BURNING_PERC_2000, yend=BURNING_PERC_2010, col=class), size=.75, show.legend=F) + 
  geom_vline(xintercept=1, linetype="dashed", size=.1) + 
  geom_vline(xintercept=2, linetype="dashed", size=.1) +
  scale_color_manual(labels = c("Up", "Down"), 
                     values = c("green"="#00ba38", "red"="#f8766d")) +  # color of lines
  labs(x="", y="open Burning Percentage") +  # Axis labels
  xlim(.5, 2.5) + ylim(0,(1.1*(max(dfs$BURNING_PERC_2000, dfs$BURNING_PERC_2010))))  # X and Y axis limits

# Add texts
p <- p + geom_text(label=left_label, y=dfs$BURNING_PERC_2000, x=rep(1, NROW(dfs)), hjust=1.1, size=2.5)
p <- p + geom_text(label=right_label, y=dfs$BURNING_PERC_2010, x=rep(2, NROW(dfs)), hjust=-0.1, size=2.5)
p <- p + geom_text(label="2000", x=1, y=1.1*(max(dfs$BURNING_PERC_2000, dfs$BURNING_PERC_2010)), hjust=1.2, size=5)  # title
p <- p + geom_text(label="2010", x=2, y=1.1*(max(dfs$BURNING_PERC_2000, dfs$BURNING_PERC_2010)), hjust=-0.1, size=5)  # title

p + theme(panel.background = element_blank(), 
          panel.grid = element_blank(),
          axis.ticks = element_blank(),
          axis.text.x = element_blank(),
          panel.border = element_blank(),
          plot.margin = unit(c(1,2,1,2), "cm"))


theme_set(theme_classic())

df$CITY <- factor(df$CITY, levels=as.character(df$CITY))  # for right ordering of the dumbells

gg <- ggplot(df, aes(x=BURNING_PERC_2000, xend=BURNING_PERC_2010, y=CITY, group=CITY)) + 
  geom_dumbbell(color="#a3c4dc", 
                size=0.75, 
                point.colour.l="#0e668b") + 
  scale_x_continuous(label=percent) + 
  labs(x=NULL, 
       y=NULL, 
       title="Open Burning Percentage in Metropolitan Areas", 
       subtitle="Pct Change: 2000 vs 2010", 
       caption="Source: https://github.com/hrbrmstr/ggalt") +
  theme(plot.title = element_text(hjust=0.5, face="bold"),
        plot.background=element_rect(fill="#f7f7f7"),
        panel.background=element_rect(fill="#f7f7f7"),
        panel.grid.minor=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.major.x=element_line(),
        axis.ticks=element_blank(),
        legend.position="top",
        panel.border=element_blank())
