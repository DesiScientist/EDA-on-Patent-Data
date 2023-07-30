## Group 04

#Loading txt files  into R
library(readr)
data_2000 <- read_delim("D:/LUMS/5 - Junior Fall/Data Science/Project/patentdata2000.txt", delim = "|",col_names = FALSE,guess_max = 3)
data_2001 <- read_delim("D:/LUMS/5 - Junior Fall/Data Science/Project/patentdata2001.txt", delim = "|",col_names = FALSE,guess_max = 3)
data_2002 <- read_delim("D:/LUMS/5 - Junior Fall/Data Science/Project/patentdata2002.txt", delim = "|",col_names = FALSE,guess_max = 3)
data_2003 <- read_delim("D:/LUMS/5 - Junior Fall/Data Science/Project/patentdata2003.txt", delim = "|",col_names = FALSE,guess_max = 3)
data_2004 <- read_delim("D:/LUMS/5 - Junior Fall/Data Science/Project/patentdata2004.txt", delim = "|",col_names = FALSE,guess_max = 3)
data_2005 <- read_delim("D:/LUMS/5 - Junior Fall/Data Science/Project/patentdata2005.txt", delim = "|",col_names = FALSE,guess_max = 3)
data_2006 <- read_delim("D:/LUMS/5 - Junior Fall/Data Science/Project/patentdata2006.txt", delim = "|",col_names = FALSE,guess_max = 3)
data_2007 <- read_delim("D:/LUMS/5 - Junior Fall/Data Science/Project/patentdata2007.txt", delim = "|",col_names = FALSE,guess_max = 3)
data_2008 <- read_delim("D:/LUMS/5 - Junior Fall/Data Science/Project/patentdata2008.txt", delim = "|",col_names = FALSE,guess_max = 3)
data_2009 <- read_delim("D:/LUMS/5 - Junior Fall/Data Science/Project/patentdata2009.txt", delim = "|",col_names = FALSE,guess_max = 3)
data_2010 <- read_delim("D:/LUMS/5 - Junior Fall/Data Science/Project/patentdata2010.txt", delim = "|",col_names = FALSE,guess_max = 3)
data_2011 <- read_delim("D:/LUMS/5 - Junior Fall/Data Science/Project/patentdata2011.txt", delim = "|",col_names = FALSE,guess_max = 3)
data_2012 <- read_delim("D:/LUMS/5 - Junior Fall/Data Science/Project/patentdata2012.txt", delim = "|",col_names = FALSE,guess_max = 3)
data_2013 <- read_delim("D:/LUMS/5 - Junior Fall/Data Science/Project/patentdata2013.txt", delim = "|",col_names = FALSE,guess_max = 3)
data_2014 <- read_delim("D:/LUMS/5 - Junior Fall/Data Science/Project/patentdata2014.txt", delim = "|",col_names = FALSE,guess_max = 3)

#Renaming Columns
colnames(data_2000)<-c("Patent_Year","Patent_Number","Assignee_Name","City_of_First_Inventor","State_Zip_code","Country","Class","Subclass")
colnames(data_2001)<-c("Patent_Year","Patent_Number","Assignee_Name","City_of_First_Inventor","State_Zip_code","Country","Class","Subclass")
colnames(data_2002)<-c("Patent_Year","Patent_Number","Assignee_Name","City_of_First_Inventor","State_Zip_code","Country","Class","Subclass")
colnames(data_2003)<-c("Patent_Year","Patent_Number","Assignee_Name","City_of_First_Inventor","State_Zip_code","Country","Class","Subclass")
colnames(data_2004)<-c("Patent_Year","Patent_Number","Assignee_Name","City_of_First_Inventor","State_Zip_code","Country","Class","Subclass")
colnames(data_2005)<-c("Patent_Year","Patent_Number","Assignee_Name","City_of_First_Inventor","State_Zip_code","Country","Class","Subclass")
colnames(data_2006)<-c("Patent_Year","Patent_Number","Assignee_Name","City_of_First_Inventor","State_Zip_code","Country","Class","Subclass")
colnames(data_2007)<-c("Patent_Year","Patent_Number","Assignee_Name","City_of_First_Inventor","State_Zip_code","Country","Class","Subclass")
colnames(data_2008)<-c("Patent_Year","Patent_Number","Assignee_Name","City_of_First_Inventor","State_Zip_code","Country","Class","Subclass")
colnames(data_2009)<-c("Patent_Year","Patent_Number","Assignee_Name","City_of_First_Inventor","State_Zip_code","Country","Class","Subclass")
colnames(data_2010)<-c("Patent_Year","Patent_Number","Assignee_Name","City_of_First_Inventor","State_Zip_code","Country","Class","Subclass")
colnames(data_2011)<-c("Patent_Year","Patent_Number","Assignee_Name","City_of_First_Inventor","State_Zip_code","Country","Class","Subclass")
colnames(data_2012)<-c("Patent_Year","Patent_Number","Assignee_Name","City_of_First_Inventor","State_Zip_code","Country","Class","Subclass")
colnames(data_2013)<-c("Patent_Year","Patent_Number","Assignee_Name","City_of_First_Inventor","State_Zip_code","Country","Class","Subclass")
colnames(data_2014)<-c("Patent_Year","Patent_Number","Assignee_Name","City_of_First_Inventor","State_Zip_code","Country","Class","Subclass")

#Checking variables
str(data_2000)
summary(data_2000)

#Merging data frames in R
master_data<-rbind(data_2000,data_2001,data_2002,data_2003,data_2004,data_2005,data_2006,data_2007,
                   data_2008,data_2009,data_2010,data_2011,data_2012,data_2013,data_2014)
summary(master_data)
str(master_data)
table(master_data$Country)
table(master_data$State_Zip_code)
master_data<-master_data[!(master_data$State_Zip_code=="000" & master_data$Country=="00"),]
master_data$Country<-gsub('00','US',master_data$Country)

#Adding new column with value "1" for summing purposes
master_data$patents_num<-c(rep(1,2922086))
aggregated_data<-aggregate(patents_num~Patent_Year+Country,master_data,sum)
aggregated_class<-aggregate(patents_num~Class,master_data,sum)
aggregated_country_class<-aggregate(patents_num~Country+Class,master_data,sum)
names(aggregated_data)[names(aggregated_data) == "Country"] <- "Country_Code"

#Importing RnD Data
library(readxl)
RnD_per_GDP<-read_excel("D:/LUMS/5 - Junior Fall/Data Science/Project/Research.xlsx")
RnD_per_GDP[is.na(RnD_per_GDP)]<-0
library(data.table)
long <- melt(setDT(RnD_per_GDP), id.vars = c("Country_Code"), variable.name = "year")
names(long)[2] <-"Patent_Year"
z<-merge(x = aggregated_data, y = long, by = c("Country_Code","Patent_Year"), all.x = TRUE)
Y<-merge(x = aggregated_data, y = long, by = c("Country_Code","Patent_Year"), all.y = TRUE)
Y[is.na(Y)]<-0

#Importing all other datasets:
#country names for country codes
country_names <- read.csv("D:/LUMS/5 - Junior Fall/Data Science/Project/countries.csv")
#class names for country codes
class_names <- read.csv("D:/LUMS/5 - Junior Fall/Data Science/Project/class_names.csv")


#Following variables for top 10 countries:
#RnD expenditure
RnD_per_GDP_long <- subset(long, Country_Code == "CA"| Country_Code =="CN" | Country_Code =="DE" | Country_Code =="FR"| Country_Code =="GB" |Country_Code =="IT"|Country_Code =="JP"|Country_Code =="KR"|Country_Code =="SE"|Country_Code =="US", select = c(Country_Code, Patent_Year, value))
RnD_per_GDP_long[is.na(RnD_per_GDP_long)]<-0
RnD_per_GDP_long.df<-RnD_per_GDP_long

#education enrollment
edu_enroll <- read.csv("D:/LUMS/5 - Junior Fall/Data Science/Project/edu_enroll.csv")
edu_enroll[is.na(edu_enroll)]<-0
edu_enroll.df<-edu_enroll

#gdp per capita
gdp_per_capita <- read.csv("D:/LUMS/5 - Junior Fall/Data Science/Project/gdp_per_capita.csv")
gdp_per_capita[is.na(gdp_per_capita)]<-0
gdp_per_capita.df<-gdp_per_capita

#government education expenditure
govt_edu_exp <- read.csv("D:/LUMS/5 - Junior Fall/Data Science/Project/govt_edu_exp.csv")
govt_edu_exp[is.na(govt_edu_exp)]<-0
govt_edu_exp.df<-govt_edu_exp

#labor force participation rate of ages 15-64
labor_force_part_15_64 <- read.csv("D:/LUMS/5 - Junior Fall/Data Science/Project/labor_force_part_15_64.csv")
labor_force_part_15_64[is.na(labor_force_part_15_64)]<-0
labor_force_part_15_64.df<-labor_force_part_15_64

#number of patent applications filed
patent_apps <- read.csv("D:/LUMS/5 - Junior Fall/Data Science/Project/patent_apps.csv")
patent_apps[is.na(patent_apps)]<-0
patent_apps.df<-patent_apps

#tax revenue generated
tax_revenue <- read.csv("D:/LUMS/5 - Junior Fall/Data Science/Project/tax_revenue.csv")
tax_revenue[is.na(tax_revenue)]<-0
tax_revenue.df<-tax_revenue


## EXPLORATORY DATA ANALYSIS
aggregated_sum_patents<-aggregate(patents_num~Country_Code,Y,sum)
df_final<- cbind(country_names, aggregated_sum_patents)
df_final<-df_final[-3]

#preliminary findings of number of patents data base-plotting system: to understand data trends, properties, patterns and set an approach
hist(df_final$patents_num, main ="Distribution of No. of Patents per country", col ="navy",xlab="Number of Patents",ylab="Density",cex.lab=1)
boxplot(df_final$patents_num, main ="Distribution of No. of Patents per country", varwidth = TRUE,xlab="Number of Patents",ylab="Density")

#too many countries with zero or minimal patents, so take top 10 to test relationship of RnD Spending on patents

#Finding the top 10 countries with highest number of patents from 2000-2014
library(dplyr)
top10patents <- top_n(df_final[,c(1:3)], 10)
#Canada, China, Germany, France, UK, Italy, Japan, South Korea, Sweden, USA

#preliminary findings for all other variables:
library(ggplot2)

#RnD Expenditure
ggplot(RnD_per_GDP_long.df, aes(x=Patent_Year, y=value)) + geom_point(aes(color=Country_Code), size= 3) + geom_smooth() +labs(title="RnD Expenditure", x="Years", y="RnD Expenditure")
hist(RnD_per_GDP_long.df$value, main ="Distribution of RnD Expenditure", xlab="RnD Expenditure", ylab="Density")

#Education Enrollment %
ggplot(edu_enroll.df, aes(x=Patent_Year, y=edu_enroll)) + geom_point(aes(color=country), size= 3) + geom_smooth() +labs(title="Trend of Education Enrollment", x="Years", y="Education Enrollment")
hist(edu_enroll.df$edu_enroll, main ="Distribution of Education Enrollment", xlab="Education Enrollment", ylab="Density")

#GDP per capita
ggplot(gdp_per_capita.df, aes(x=Patent_Year, y=gdp_per_capita)) + geom_point(aes(color=country), size= 3) + geom_smooth() +labs(title="Trend of GDP per capita", x="Years", y="GDP per Capita")
hist(gdp_per_capita.df$gdp_per_capita, main ="Distribution of GDP per capita", xlab="GDP Per Capita", ylab="Density")

#Govt Education Expenditure
ggplot(govt_edu_exp.df, aes(x=Patent_Year, y=govt_edu_exp)) + geom_point(aes(color=country), size= 3) + geom_smooth() +labs(title="Trend of Govt Education Expenditure", x="Years", y="Govt Education Expenditure")
hist(govt_edu_exp.df$govt_edu_exp, main ="Distribution of Govt Education Expenditure", xlab="Govt. Education Expenditure", ylab="Density")

#Labor force participation rate ages 15-64
ggplot(labor_force_part_15_64.df, aes(x=Patent_Year, y=labor_force_part_15_64)) + geom_point(aes(color=country), size= 3) + geom_smooth() +labs(title="Trend of Labor Force Participation Rate Ages 15-64", x="Years", y="Labor Force Participation Rate")
hist(labor_force_part_15_64.df$labor_force_part_15_64, main ="Distribution of Labor force participation rate ages 15-64", xlab="Labore Force Participation", ylab="Density")

#Patent Applications filed
ggplot(patent_apps.df, aes(x=Patent_Year, y=patent_apps)) + geom_point(aes(color=country), size= 3) + geom_smooth() +labs(title="Trend of Patent Applications", x="Years", y="Patent Applications")
hist(patent_apps.df$patent_apps, main ="Distribution of Patent Applications Filed", xlab="Patent Applications", ylab="Density")

#Tax revenue
ggplot(tax_revenue.df, aes(x=Patent_Year, y=tax_revenue)) + geom_point(aes(color=country), size= 3) + geom_smooth() +labs(title="Trend of Tax Revenue", x="Years", y="Tax Revenue")
hist(tax_revenue.df$tax_revenue, main ="Distribution of Tax Revenue", xlab="Tax Revenue", ylab="Density")

##Graphs for Top 10 countries with highest total number of patents from 2000-2014

#Lollipop chart
library(ggplot2)
theme_set(theme_bw())
ggplot(top10patents, aes(x=country, y=patents_num)) + 
  geom_segment(aes(x=country, 
                   xend=country, 
                   y=0, 
                   yend=patents_num), color="dark grey", size =2) + 
  geom_point(size=3, color="orange", fill=alpha("orange", 0.3), alpha=0.7, shape=21, stroke=3) +
  theme_light() +
  labs(title="Countries with Highest No. of Patents", 
       subtitle="from 2000-2014") + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6))+
  ylab("Total number of patents")

#Sum of Patents in each year of top 10 countries
master_data_top10 <- subset(master_data, Country == "CA"| Country =="CN" | Country =="DE" | Country =="FR"| Country =="GB" |Country =="IT"|Country =="JP"|Country =="KR"|Country =="SE"|Country =="US", select = c(Country, Patent_Year, patents_num))
aggregated_data_top10<-aggregate(patents_num ~ Patent_Year + Country , master_data_top10, sum)

#Number of patents of top 10 countries from 2000-2014
library(ggplot2)
ggplot(aggregated_data_top10, aes(x=Patent_Year, y=patents_num)) + geom_point(aes(color=Country), size= 3) + geom_smooth() +labs(title="Trend of Top 10 Countries", x="Years", y="Number of Patents")


#Relationship between number of patents and all other variables

#Dataset combining all variables and number of patents
data_1<-merge(aggregated_data, edu_enroll.df)
data_2<-merge(data_1, gdp_per_capita.df)
data_3<-merge(data_2, govt_edu_exp.df)
data_4<-merge(data_3, labor_force_part_15_64.df)
data_5<-merge(data_4, tax_revenue.df)
data_6<-merge(data_5, patent_apps.df)
data_7<-merge(data_6, RnD_per_GDP_long.df)
data_all_variables <- data_7 
names(data_all_variables)[11]<-"RnD_expenditure"
attach(data_all_variables)

#Scatterplots for each patents_num vs each variable, color coded by country
library(lattice)

#Number of patents w.r.t education enrollment   
ggplot(data=data_all_variables,aes(edu_enroll,patents_num))+geom_point(aes(color=country),size=3,position = "jitter")+
  geom_smooth() +
  labs(y = "Education Enrollment", x = "Number of Patents", title = "No. of Patents w.r.t Education Enrollment")

xyplot(patents_num~edu_enroll|country,data=data_all_variables,layout=c(2,5), xlab="Number of Patents", ylab="Education Enrollment", col="blue", main="No. of Patents w.r.t Education Enrollment")

#Number of patents w.r.t to GDP Per Capita
ggplot(data=data_all_variables,aes(gdp_per_capita,patents_num))+geom_point(aes(color=country),size=3,position = "jitter")+
  geom_smooth() +
  labs(y = "GDP Per Capita", x = "Number of Patents", title = "No. of Patents w.r.t GDP Per Capita")

xyplot(patents_num~gdp_per_capita|country,data=data_all_variables,layout=c(2,5), xlab="Number of Patents", ylab="GDP per Capita", main="No. of Patents w.r.t GDP Per Capita")

#Number of patents w.r.t to Government Education Expenditure
ggplot(data=data_all_variables,aes(govt_edu_exp,patents_num))+geom_point(aes(color=country),size=3,position = "jitter")+
  geom_smooth() +
  labs(y = "Govt. Education Expenditure", x = "Number of Patents", title = "No. of Patents w.r.t Govt. Education Expenditure")

xyplot(patents_num~govt_edu_exp|country,data=data_all_variables,layout=c(2,5), xlab="Number of Patents", ylab="Govt. Education Expenditure", main="No. of Patents w.r.t Govt. Education Expenditure")

#Number of patents w.r.t to Labor Force participation for Age Bracket 15-64
ggplot(data=data_all_variables,aes(labor_force_part_15_64,patents_num))+geom_point(aes(color=country),size=3,position = "jitter")+
  geom_smooth() +
  labs(y = "Labor Force Participation", x = "Number of Patents", title = "No. of Patents w.r.t Labor Force Participation")

xyplot(patents_num~labor_force_part_15_64|country,data=data_all_variables,layout=c(2,5), xlab="Number of Patents", ylab="Labor Force Participation (15-64 years)", main="No. of Patents w.r.t Labor Force Participation")

#Number of patents w.r.t to Tax Revenue
ggplot(data=data_all_variables,aes(tax_revenue,patents_num))+geom_point(aes(color=country),size=3,position = "jitter")+
  geom_smooth() +
  labs(y = "Tax Revenue", x = "Number of Patents", title = "No. of Patents w.r.t Tax Revenue")


xyplot(patents_num~tax_revenue|country,data=data_all_variables,layout=c(2,5), xlab="Number of Patents", ylab="Tax Revenue", main="No. of Patents w.r.t Tax Revenue")



#Number of patents w.r.t to RnD Expenditure 
ggplot(data=data_all_variables,aes(RnD_expenditure,patents_num))+geom_point(aes(color=country),size=3,position = "jitter")+
  geom_smooth()+
  labs(y = "RnD Expenditure", x = "Number of Patents", title = "No. of Patents w.r.t RnD Expenditure")

xyplot(patents_num~RnD_expenditure|country,data=data_all_variables,layout=c(2,5), xlab="Number of Patents", ylab="RnD Expenditure", main="No. of Patents w.r.t RnD Expenditure")

#Number of patents w.r.t to patents applications
ggplot(data=data_all_variables,aes(patent_apps,patents_num))+geom_point(aes(color=country),size=3,position = "jitter")+
  geom_smooth() +
  labs(y = "Patent Applications", x = "Number of Patents", title = "No. of Patents w.r.t Patent Applications")

xyplot(patents_num~patent_apps|country,data=data_all_variables,layout=c(2,5), xlab="Number of Patents", ylab="Patent Applications", main="No. of Patents w.r.t Patent Applications")





## STATISTICAL ANALYSIS

#panel data
library(panelr)
library(dplyr)
library(lmtest)
library(plm)

#Renaming variable
PanelData<-pdata.frame(data_all_variables,index=c("Country_Code","Patent_Year"))
summary(PanelData)
attach(PanelData)
hist(patents_num, main ="Distribution of Number of Patents", xlab ="Number of Patents", ylab="Density")

#Converting Number of Patent Applications to log 
data_all_variables$log_patent_apps <-log(data_all_variables$patent_apps+2)
summary(data_all_variables$log_patent_apps)

#Converting Number of Patents to log
data_all_variables$log_patents_nums<-log(data_all_variables$patents_num)
summary(data_all_variables$log_patents_nums)
hist(data_all_variables$log_patents_nums, main ="Distribution of Log - Number of Patents", xlab ="Number of Patents", ylab="Density")

attach(data_all_variables)

#Checking for correlation between control variables and Number of Patents
cor.test(data_all_variables$patent_apps,data_all_variables$patents_num)
cor.test(data_all_variables$edu_enroll,data_all_variables$patents_num)
cor.test(data_all_variables$gdp_per_capita,data_all_variables$patents_num)
cor.test(data_all_variables$labor_force_part_15_64,data_all_variables$patents_num)
cor.test(data_all_variables$govt_edu_exp,data_all_variables$patents_num)
cor.test(data_all_variables$tax_revenue,data_all_variables$patents_num)
cor.test(data_all_variables$RnD_expenditure,data_all_variables$patents_num)

#MLR using two ways fixed effects 
m1<-plm(log_patents_nums~RnD_expenditure+edu_enroll+gdp_per_capita+govt_edu_exp+labor_force_part_15_64+tax_revenue+patent_apps,data=data_all_variables, model = "within",effect = "twoways")
summary(m1)

#For robust standard errors
coeftest(m1,vcov=vcovDC(m1,type = "HC1"))

#MLR using Country as a factor
m2<-plm(log_patents_nums~RnD_expenditure+edu_enroll+gdp_per_capita+govt_edu_exp+labor_force_part_15_64+tax_revenue+patent_apps+factor(country),data=data_all_variables, model = "pooling")
summary(m2)

#MLR using Year as a factor
m3<-plm(log_patents_nums~RnD_expenditure+edu_enroll+gdp_per_capita+govt_edu_exp+labor_force_part_15_64+tax_revenue+patent_apps+factor(Patent_Year),data=data_all_variables, model = "pooling")
summary(m3)


#pFtest: Test two-way effects based on the comparison of the within and the pooling model

pFtest(m1,m2) 
#null hypothesis: m2 is better than m1
#since p value is smaller than 0.05, we can reject the null hypothesis and conclude that m1 is better than m2
pFtest(m1,m3) 
#null hypothesis: m3 is better than m1
#since p-value is smaller than 0.05, we can reject the null hypothesis and conclude that m1 is better than m3