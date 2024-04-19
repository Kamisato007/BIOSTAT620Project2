setwd("C:/Users/kamis/Documents/Winter 2024/BIOSTAT620/Project 2")


# load the library
library(readxl)
library(dplyr)
library(nlme)
library(lubridate)
library(mice)
library(texreg)
library(table1)

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





# 2024-4-19 Updated Below


# Create an indicator that represents before and after the intervention
df_imputed <- df_imputed %>% mutate(after_interv_ind= ifelse(Date>"2024-03-26",
                                                             1,0))


df_imputed <- df_imputed %>%
  mutate(sex = recode(sex,
                      `0` = "Female", 
                      `1` = "Male"))


df_A <- df_imputed %>% filter(Treatment == "A")
df_B <- df_imputed %>% filter(Treatment == "B")


model1 <- lme(fixed = Total.ST.min ~ after_interv_ind*sex + 
                after_interv_ind*devices + 
                after_interv_ind*P_Score, 
             random = ~ 1 | pseudo_ID,
             data = df_A,
             method = "REML")
summary(model1)


model2 <- lme(fixed = Pickups ~ after_interv_ind*sex + 
                after_interv_ind*devices + after_interv_ind*P_Score, 
              random = ~ 1 | pseudo_ID,
              data = df_B,
              method = "REML")
summary(model2)


# Generate Table 1
new_column_names <- c("pseudo_ID","Total Screen Time (mins)","Pickups",
                      "Treatment","Age","Sex","Apps","Devices",
                      "Procrastination Score","Date","after_interv_ind")
df_table1 <- df_imputed
colnames(df_table1) <- new_column_names

df_table1 <- df_table1 %>%
  mutate(Treatment = recode(Treatment,
                      "A" = "Treatment A", 
                      "B" = "Treatment B"))



table1(~ `Total Screen Time (mins)`+ Pickups+ Sex + 
         Devices + `Procrastination Score` | Treatment, data=df_table1)





#----------------------------------------------------------------------
# Objective 2: Which treatment is more effective? (Ordinal Logistic Regression)

df_clean_filtered <- df_imputed %>%
  filter(Date >= as.Date("2024-03-27") & Date <= as.Date("2024-04-02"))

# Input compliance
df_clean_filtered <- df_clean_filtered %>%
  mutate(compliance = case_when(
    Treatment == "A" & Total.ST.min <= 200 ~ 1,
    Treatment == "B" & Pickups <= 50 ~ 1,
    TRUE ~ 0  # failure
  ))


# Next...

# 1. Calculate the cumulative `compliance` for each ID
compliance_sums <- df_clean_filtered %>%
  group_by(pseudo_ID) %>%
  summarise(total_compliance = sum(compliance, na.rm = TRUE))  # Use na.rm = TRUE to ignore NA values

# 2. Add the cumulative values to the df_imputed dataset
df_imputed <- df_imputed %>%
  left_join(compliance_sums, by = "pseudo_ID") %>%
  mutate(total_compliance = if_else(is.na(total_compliance), 0, total_compliance))  # Ensure NAs are replaced with 0

# Check the updated df_imputed dataset
print(df_imputed)

# MASS for ordinal logistic
library(MASS)

# Calculate means and variances for each ID
df_stats <- df_imputed %>%
  group_by(pseudo_ID) %>%
  summarise(
    mean_total_st = mean(Total.ST.min, na.rm = TRUE),  # Calculate mean of Total.ST.min
    var_total_st = var(Total.ST.min, na.rm = TRUE),    # Calculate variance of Total.ST.min
    mean_pickups = mean(Pickups, na.rm = TRUE),        # Calculate mean of Pickups
    var_pickups = var(Pickups, na.rm = TRUE)           # Calculate variance of Pickups
  )

# Merge the statistics back into the main dataset
df_imputed <- df_imputed %>%
  left_join(df_stats, by = "pseudo_ID")

# Split data into two treatment groups
df_treatment_a <- df_imputed %>% filter(Treatment == "A")
df_treatment_b <- df_imputed %>% filter(Treatment == "B")

# Baseline data
df_base <- df_imputed %>%
  group_by(pseudo_ID) %>%
  slice(1) %>%
  ungroup()

hist(df_base$total_compliance)
# Since it is a RCT, we do not need to split groups
df_base$TreatmentA <- ifelse(df_base$Treatment == "A", 1, 0)
df_base$Treatment <- as.factor(df_base$Treatment)

m1 <- polr(as.factor(total_compliance) ~ Treatment + factor(sex) + devices + P_Score,
           data = df_base, Hess=TRUE)

summary(m1)

summary_m1 <- summary(m1)

## store table
(ctable <- coef(summary(m1)))

## calculate and store p values
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2

## combined table
(ctable <- cbind(ctable, "p value" = p))


# 95%CI

(ci <- confint(m1)) # default method gives profiled CIs

# Interpretation

## odds ratios and confidence intervals
exp(coef(m1))
exp(confint(m1))


#------------------------------------------------------------------------------