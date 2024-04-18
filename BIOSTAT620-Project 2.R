setwd("C:/Users/kamis/Documents/Winter 2024/BIOSTAT620/Project 2")


# load the library
library(readxl)
library(dplyr)
library(nlme)
library(lubridate)
library(mice)

# load the data
df <- read_excel("Fulldata_620W24_Project2.xlsx",sheet = "screentime",
                 col_types = c("numeric","date","text","text","numeric",
                               "text","numeric","numeric","text",
                               "numeric","numeric","text"))
df2 <- read_xlsx("Fulldata_620W24_Project2.xlsx",sheet = "baseline")



# N = 34 participants
length(unique(df$pseudo_ID))




# Update for ID = 1019
date_sequence <- seq(as.Date("2024-01-03"), as.Date("2024-04-04"), by = "day")
df <- df %>%
  group_by(pseudo_ID) %>%
  mutate(Date = if_else(pseudo_ID == 1019, date_sequence[1:n()], Date)) %>%
  ungroup()

hm_to_min <- function(hm){ 
  unlist(lapply(hm,function(x){splt = strsplit(x,"h")[[1]] #extract hour
  hr=as.numeric(splt[1]) 
  mn=as.numeric(strsplit(splt[2],"m")[[1]][1])
  return (60*hr+mn) }) ) 
}
                                                                                                                                 

Total_not_match <- df[which(!hm_to_min(df$Total.ST) == df$Total.ST.min),]
Social_not_match <- df[which(!hm_to_min(df$Social.ST) == df$Social.ST.min),]

# 13 people has mismatch in Total Screen Time
length(unique(Total_not_match$pseudo_ID))

# 16 people has mismatch in Social Screen Time
length(unique(Social_not_match$pseudo_ID))

# 2059 has 1400 Total ST min, decision?
df[which(df$Total.ST.min== max(na.omit(df$Total.ST.min))),]


# Remove observation with Total Screen Time larger than 1000

hist(df$Total.ST.min)
df_clean <- subset(df, Total.ST.min <= 1000)


df_clean <- df_clean %>%
  mutate(Date = case_when(
    Day == "We" ~ as.Date("2024-03-26") + days(1),
    Day == "Th" ~ as.Date("2024-03-26") + days(2),
    Day == "Fi" ~ as.Date("2024-03-26") + days(3),
    Day == "Sa" ~ as.Date("2024-03-26") + days(4),
    Day == "Su" ~ as.Date("2024-03-26") + days(5),
    Day == "Mo" ~ as.Date("2024-03-26") + days(6),
    Day == "Tu" ~ as.Date("2024-03-26") + days(7),
    TRUE ~ Date
  ))



# 
# # Date had NA  2793 4278 1929 9680
# df_date_na <- df_clean[is.na(df_clean$Date),]
# # 2793 forgets to input date entirely
# View(df_clean %>% filter(pseudo_ID== 2793))
# # 4278 forgets to input date before 2024-01-21
# View(df_clean %>% filter(pseudo_ID== 4278))
# # 4278 forgets to input date entirely
# View(df_clean %>% filter(pseudo_ID== 1929))
# # 9680 has only 7 observations
# View(df_clean %>% filter(pseudo_ID== 9680))

# Assuming your dataframe is named 'data' and it has an 'ID' column




colnames(df2)[1] <- "pseudo_ID"
df_clean <- full_join(df_clean,df2,by="pseudo_ID")

df_clean <- df_clean %>%
  mutate(sex = recode(sex,
                      `0` = 0, 
                      `1` = 1, 
                      "male" = 1,
                      "Female" = 0))
# Verify the changes
table(df_clean$sex)


df_clean_select <- df_clean %>% select(pseudo_ID,Total.ST.min,
                                       Pickups,Treatment,age,sex,apps,
                                       devices,`procrastination score`)

# Rename the procrastination score
colnames(df_clean_select)[9]<- "P_Score"

# IMPUTATION
df_imputed_obj  <- mice(df_clean_select)
df_imputed <- complete(df_imputed_obj) %>% mutate(Date = df_clean$Date)


df_imputed <- na.omit(df_imputed)

# Remove the observation with Date is missing, since we can't not distinguish
# which period is the intervention period for them.
# We didn't remove too many observations


# 29 ID left




