#install and load necessary packages
install.packages("tidyverse")
install.packages("gtsummary")
install.packages("easystats")
install.packages("gt")
install.packages("ggplot2")
install.packages("naniar")
install.packages("ggthemes")
install.packages("RColorBrewer")
install.packages("rio")
install.packages("sjPlot")
library(tidyverse)
library(gtsummary)
library(easystats)
library(gt)
library(ggplot2)
library(naniar)
library(ggthemes)
library(RColorBrewer)
library(rio)
library(sjPlot)





#Import data
data2 <- readxl::read_xlsx("data/AMR_KAP_Data.xlsx",sheet = 1)
data <- readxl::read_xlsx("data/AMR_KAP_Data.xlsx",sheet = 2)


#Check missing values
gg_miss_var(data)


#Check Duplicated rows
sum(duplicated(data))



#Table 1. Demographic characteristics of study participants
data |>
  select(1:11)|>
  tbl_summary() |>
  as_gt()|>
  gtsave("Tables/Table1_Demographic_info.docx")





#Table 2. Major sources of information about antibiotic of parents
data |>
  select(41:49)|>
  tbl_summary() |>
  as_gt()|>
  gtsave("Tables/Table2_ Major_sources_of_information_about_antibiotic_of_parents.docx")







#Table 3.  Level of knowledge, attitudes, and practices
data <- data|>
  mutate(Knowledge_level = case_when(
    Knowledge_PCT < 25 ~ "Poor",
    Knowledge_PCT <= 50 ~ "Moderate",
    Knowledge_PCT >= 50 ~ "Good",
  )) |>
  mutate(Attitude_level = case_when(
    Attitude_PCT < 25 ~ "Poor",
    Attitude_PCT <= 50 ~ "Moderate",
    Attitude_PCT >= 50 ~ "Good"
  )) |>
  mutate(Practice_level = case_when(
    Practice_PCT < 25 ~ "Poor",
    Practice_PCT <=50 ~  "Moderate",
    Practice_PCT >= 50 ~  "Good"
  )) |>
  select(69:71)|>
  tbl_summary() |>
  as_gt()|>
  gtsave("Tables/Table3_Level_of_knowledge,_attitudes,_and_practices.docx")







#Table 4. Factors associated with the level of knowledge

      #Convert outcome into factor
data$Knowledge_level <- as.factor(data$Knowledge_level)

#Tbl_regression
data |> 
  select(1:9, Knowledge_level) |>
  tbl_uvregression(
    method = glm,
    y = Knowledge_level,
    method.args = list(family = binomial),
    exponentiate = T
  )|>
  bold_p(t=0.05)|>
  as_gt() |>
  gtsave("Tables/Table_4_UV_LogReg_Knowledge_level.docx")



#Table 5.  Factors associated with the level of attitudes towards antibiotic resistance 

#Convert outcome into factor
data$Attitude_level  <- as.factor(data$Attitude_level)

#Tbl_regression
temp<- data |> 
  select(1:9, Attitude_level) |>
  tbl_uvregression(
    method = glm,
    y = Attitude_level,
    method.args = list(family = binomial),
    exponentiate = T
  )|>
  bold_p(t=0.05)|>
  as_gt()|>
  gtsave("Tables/Table_5_UV_LogReg_Attitude_level.docx")







# Figure 1.  Distribution of knowledge of antibiotic resistance 
fig_data1 <- data2|>
  select(12:23)

# Reshape the data from wide to long format
long_fig_data <- fig_data1 |>
  pivot_longer(
    cols = 1:12, 
    names_to = "Question", # Name for the question column
    values_to = "Response" # Name for the response column
  )


# Count the responses for each question
summary_data <- long_fig_data |>
  group_by(Question, Response) |>
  summarise(Count = n(), .groups = 'drop') |>
  mutate(Percentage = Count / sum(Count) * 100)

# Create the plot
plot1<- ggplot(summary_data, aes(x = Question, y = Percentage, fill = Response))+
  geom_bar(stat = "identity", position = "fill") +
  coord_flip() +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values =  c("Yes" = "#1E7F7F",        
                                "No" = "#D3D3D3",        
                                "Don't Know" = "#D6C48A")) +
  labs( title="Figure 1.  Distribution of knowledge of antibiotic resistance
        among parents of school-going children (N = 704).",
        x = "",
       y = "Percentage",
       fill = "Response") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10),
        plot.title = element_text(size = 12, face = "bold",hjust = 0.5))

ggsave("Figures/Figure1.antibiotic_resistance_knowledge.png", 
       plot = plot1, 
       width = 10, 
       height = 6, 
       dpi = 600)







##Figure 2.   Attitude towards antibiotic resistance and the misuse of antibiotics
fig_data2 <- data2|>
  select(24:33)

# Reshape the data from wide to long format
long_fig_data2 <- fig_data2 |>
  pivot_longer(
    cols = 1:10, 
    names_to = "Question", # Name for the question column
    values_to = "Response" # Name for the response column
  )


# Count the responses for each question
summary_data2 <- long_fig_data2 |>
  group_by(Question, Response) |>
  summarise(Count = n(), .groups = 'drop') |>
  mutate(Percentage = Count / sum(Count) * 100)

# Create the plot
plot2<- ggplot(summary_data2, aes(x = Question, y = Percentage, fill = Response))+
  geom_bar(stat = "identity", position = "fill") +
  coord_flip() +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values =  c("Agree" = "#1E7F7F",        
                                "Disagree" = "#D3D3D3",        
                                "Neutral" = "#D6C48A")) +
  labs( title = " Figure 2. Attitude towards antibiotic resistance
  and the misuse of antibiotics among parents of school-going children (N = 704).",
  x = "",
        y = "Percentage",
        fill = "Response") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10),
        plot.title = element_text(size = 15, face = "bold",hjust = 0.5))

ggsave("Figures/Figure2.Attitude_towards_antibiotic_resistance.png", 
       plot = plot2, 
       width = 15, 
       height = 8, 
       dpi = 600)









## Figure 3.  Practices among parents
fig_data3 <- data2|>
  select(34:39)

# Reshape the data from wide to long format
long_fig_data3 <- fig_data3 |>
  pivot_longer(
    cols = 1:6, 
    names_to = "Question", # Name for the question column
    values_to = "Response" # Name for the response column
  )


# Count the responses for each question
summary_data3 <- long_fig_data3 |>
  group_by(Question, Response) |>
  summarise(Count = n(), .groups = 'drop') |>
  mutate(Percentage = Count / sum(Count) * 100)

# Create the plot
plot3<- ggplot(summary_data3, aes(x = Question, y = Percentage, fill = Response))+
  geom_bar(stat = "identity", position = "fill") +
  coord_flip() +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values =  c("Yes" = "#1E7F7F",        
                                "No" = "#D6C48A")) +
  labs( title = "Figure 3. Practices among parents of school-going children
        regarding antibiotic resistance (N = 704).",
        x = "",
        y = "Percentage",
        fill = "Response") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10),
        plot.title = element_text(size = 16, face = "bold",hjust = 0.5))

ggsave("Figures/Figure3.Practices_among_parents.png", 
       plot = plot3, 
       width = 15, 
       height = 8, 
       dpi = 600)







































































