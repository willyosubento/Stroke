#packages
library(readr)
library(Hmisc)
library(dplyr)
library(tidyr)
library(tidyverse)
stroke_data<-read.csv("C:\\Users\\user\\Downloads\\archive (8)\\healthcare-dataset-stroke-data.csv")
View(stroke_data)
#Rename data
df_stroke<-stroke_data %>%
  mutate(hypertension=recode(hypertension,`0`="No",`1`="Yes"),heart_disease=recode(heart_disease,`0`="No",`1`="Yes"),
         stroke=recode(stroke,`0`="No",`1`="Yes")) %>%
  view()
#Capitalize the first word of the values in the variable smoking status
df_stroke$smoking_status<-capitalize(df_stroke$smoking_status)
#Data cleaning and pre - processing

df_stroke_1<-df_stroke %>%
  select(gender,age,hypertension,heart_disease,ever_married,work_type,Residence_type,avg_glucose_level,bmi,smoking_status,stroke) %>%
  filter(complete.cases(.)) %>%
  view()
#Replace values in the data df_stroke_1
df_stroke_2<-df_stroke_1 %>%
  mutate(bmi=recode(bmi,`N/A`="30")) %>%
  view()


#values of the variable
unique(df_stroke_2$gender)
unique(df_stroke_2$hypertension)
unique(df_stroke_2$heart_disease)
unique(df_stroke_2$ever_married)
unique(df_stroke_2$work_type)
unique(df_stroke_2$Residence_type)
unique(df_stroke_2$smoking_status)
unique(df_stroke_2$stroke)

#Convert characters to factor

df_stroke_2$gender<-factor(df_stroke_2$gender)
df_stroke_2$hypertension<-factor(df_stroke_2$hypertension)
df_stroke_2$heart_disease<-factor(df_stroke_2$heart_disease)
df_stroke_2$ever_married<-factor(df_stroke_2$ever_married)
df_stroke_2$work_type<-factor(df_stroke_2$work_type)
df_stroke_2$Residence_type<-factor(df_stroke_2$Residence_type)
df_stroke_2$smoking_status<-factor(df_stroke_2$smoking_status)
df_stroke_2$stroke<-factor(df_stroke_2$stroke)
df_stroke_2$bmi<-as.numeric(df_stroke_2$bmi)
#Data types
glimpse(df_stroke_2)

#===========================================================================================================================================
#Data visualization
summary(df_stroke_2$age)
names(df_stroke_2)
#pie chart
library(ggplot2)
summary(df_stroke_2)
#========================================================================================================================
#Plot pie chat for gender
pie_gender<-df_stroke_2%>%
  count(gender) %>%
  arrange(desc(gender)) %>%
  mutate(prop =round(n*100/sum(n),2),
         lab.ypos=cumsum(prop)-0.5*(prop))
pie_gender$label <- paste0(pie_gender$gender, "\n",
                         round(pie_gender$prop,2), "%")
ggplot(pie_gender,aes(x="",y=prop,fill=gender))+
  geom_bar(width = 1,stat = "identity",col="black") +
  geom_text(aes(y = lab.ypos, label = label),
            color = "black")+
  coord_polar("y",start = 0,direction = -1)+
  theme_void()

#plot pie chart for hypertension

pie_hypertension<-df_stroke_2 %>%
  count(hypertension)%>%
  arrange(desc(hypertension))%>%
  mutate(prop=round(n*100/sum(n),2),
         lab.ypos=cumsum(prop)-0.5*(prop))

pie_hypertension$label<-paste(pie_hypertension$hypertension,"\n",
                              round(pie_hypertension$prop,2),"%")

plot1<-ggplot(pie_hypertension,aes(x="",y=prop,fill = hypertension)) +
  geom_bar(width = 1,stat = "identity",col ="black")+
  geom_text(aes(y=lab.ypos,label=label),color = "black") +
  coord_polar("y",start = 0,direction = -1) +
  theme_void() +
  labs(title = "pie chart for hypertension")
#plot pie chart for heart disease
pie_heart<-df_stroke_2 %>%
  count(heart_disease)%>%
  arrange(desc(heart_disease))%>%
  mutate(prop=round(n*100/sum(n),2),
         lab.ypos=cumsum(prop)-0.5*(prop))

pie_heart$label<-paste(pie_heart$heart,"\n",
                              round(pie_heart$prop,2),"%")

plot2<-ggplot(pie_heart,aes(x="",y=prop,fill = heart_disease)) +
  geom_bar(width = 1,stat = "identity",col ="black")+
  geom_text(aes(y=lab.ypos,label=label),color = "black") +
  coord_polar("y",start = 0,direction = -1) +
  theme_void() +
  labs(title = "pie chart for Heart Disease")
#plot pie chart for ever married
pie_married<-df_stroke_2 %>%
  count(ever_married)%>%
  arrange(desc(ever_married))%>%
  mutate(prop=round(n*100/sum(n),2),
         lab.ypos=cumsum(prop)-0.5*(prop))

pie_married$label<-paste(pie_married$ever_married,"\n",
                              round(pie_married$prop,2),"%")

plot3<-ggplot(pie_married,aes(x="",y=prop,fill = ever_married)) +
  geom_bar(width = 1,stat = "identity",col ="black")+
  geom_text(aes(y=lab.ypos,label=label),color = "black") +
  coord_polar("y",start = 0,direction = -1) +
  theme_void() +
  labs(title = "pie showing participants who were once married")

#plot pie chart for Residence
pie_residence<-df_stroke_2 %>%
  count(Residence_type)%>%
  arrange(desc(Residence_type))%>%
  mutate(prop=round(n*100/sum(n),2),
         lab.ypos=cumsum(prop)-0.5*(prop))

pie_residence$label<-paste(pie_residence$Residence_type,"\n",
                              round(pie_residence$prop,2),"%")

plot4<-ggplot(pie_residence,aes(x="",y=prop,fill = Residence_type)) +
  geom_bar(width = 1,stat = "identity",col ="black")+
  geom_text(aes(y=lab.ypos,label=label),color = "black") +
  coord_polar("y",start = 0,direction = -1) +
  theme_void() +
  labs(title = "pie chart showing Risidence")
#plot pie chart for smoking status
pie_smoking<-df_stroke_2 %>%
  count(smoking_status)%>%
  arrange(desc(smoking_status))%>%
  mutate(prop=round(n*100/sum(n),2),
         lab.ypos=cumsum(prop)-0.5*(prop))

pie_smoking$label<-paste(pie_smoking$smoking_status,"\n",
                              round(pie_smoking$prop,2),"%")

plot5<-ggplot(pie_smoking,aes(x="",y=prop,fill = smoking_status)) +
  geom_bar(width = 1,stat = "identity",col ="black")+
  geom_text(aes(y=lab.ypos,label=label),color = "black") +
  coord_polar("y",start = 0,direction = -1) +
  theme_void() +
  labs(title = "pie chart for smoking status")
#plot pie chart for stroke status
pie_stroke<-df_stroke_2 %>%
  count(stroke)%>%
  arrange(desc(stroke))%>%
  mutate(prop=round(n*100/sum(n),2),
         lab.ypos=cumsum(prop)-0.5*(prop))

pie_stroke$label<-paste(pie_stroke$stroke,"\n",
                              round(pie_stroke$prop,2),"%")

plot6<-ggplot(pie_stroke,aes(x="",y=prop,fill = stroke)) +
  geom_bar(width = 1,stat = "identity",col ="black")+
  geom_text(aes(y=lab.ypos,label=label),color = "black") +
  coord_polar("y",start = 0,direction = -1) +
  theme_void() +
  labs(title = "pie chart for stroke")
#================================================================================================================
#A plot for Bar Graph (work_type) with counts
plot_work_type <- df_stroke_2 %>%
  count(work_type)%>%
  mutate(prop=round(n*100/sum(n),2),lab.ypos=cumsum(prop)-0.5*(prop))

ggplot(plot_work_type,
       aes(x = reorder(work_type, n),
           y = n)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label=n,vjust = -0.5)) +
  labs(x = "work type",
       y = "Frequency",
       title = "Participants by work type")

#==========================================================================================================================
# plot the bars as percentages,
# in descending order with bar labels
plot_work_type <- df_stroke_2 %>%
  group_by(work_type) %>%
  summarize(n = n()) %>%
  mutate(pct = n/sum(n),
         lbl = scales::percent(pct))
plot_work_type
library(scales)
ggplot(plot_work_type,
       aes(x = reorder(work_type, -pct),
           y = pct)) +
  geom_bar(stat = "identity",
           fill = "blue",
           color = "black") +
  geom_text(aes(label = lbl),
            vjust = -0.25) +
  scale_y_continuous(labels = percent) +
  labs(x = "work_type",
       y = "Percent",
       title = "Participants by work_type")

#=============================================================================================================
#Histogram for age

ggplot(df_stroke_2,aes(x=age))+
  geom_histogram(fill="cornflowerblue",color="white",bins = 20) +
  labs(title = "Histogram of participants by age",x="Age",y="Number of participants")
#Histogram for average glucose level
ggplot(df_stroke_2,aes(x=avg_glucose_level))+
  geom_histogram(fill ="cornflowerblue",color = "white",bins = 5)+
  labs(title = "Histogram showing Average Glucose Level",x="Average Glucose Level",y="Number of participants")
#Histogram for BMI
ggplot(df_stroke_2,aes(bmi))+
  geom_histogram(fill="cornflowerblue",color="white",bins = 20)+
  labs(title = "Histogram showing Body mass index",x="Body mass index",y="Number of participants")

#==============================================================================================================
#stacked bar charts for smoking status
library(scales)
# create a summary dataset

stacked_graph <- df_stroke_2 %>%
  group_by(smoking_status, Residence_type) %>%
  summarize(n = n()) %>%
  mutate(pct = n/sum(n),
         lbl = scales::percent(pct))
stacked_graph
#create a segment for bar chat
#adding labels to each segment




#=========================================================================================================================
#model
mode1<-glm(stroke~gender+age + hypertension + heart_disease + ever_married + work_type + Residence_type + 
             avg_glucose_level + bmi + smoking_status,family = binomial(link = "logit"),data = df_stroke_2)


summary(mode1)
#Cross tabulation
library(gtsummary)
library(lme4)
library(sjPlot)
tbl_logistic_model<- mode1 %>%
  tbl_regression(
    exponentiate = TRUE,
    pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  add_glance_table(everything())%>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()
tbl_logistic_model

write.table("model.jpng")


























