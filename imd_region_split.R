# load libraries and data -------------------------------------------------
# source libraries file
source("libraries.R")

# read lookups
lsoalookup <- read.csv("data files/Lower_Layer_Super_Output_Area_(2011)_to_Clinical_Commissioning_Group_to_Local_Authority_District_(April_2021)_Lookup_in_England.csv")
ccglookup <- read.csv("data files/Clinical_Commissioning_Group_to_STP_and_NHS_England_(Region)_(April_2021)_Lookup_in_England.csv")

# import adjusted IMD UK file
df_imd <- as_tibble(read.csv("data files/File_1_-_IMD2019_Index_of_Multiple_Deprivation.csv", sep = ','))
df_imdquintile <- df_imd %>% 
  mutate(IMDquintile = ntile(as.numeric(IMD_Rank), 5),
         lsoa11cd = as.character(LSOA_code_2011)) 

# aggregate percentages ---------------------------------------------------
# calculate regional totals and percentages of totals
reg_imd <- df_imdquintile %>% 
  left_join(lsoalookup, by = c("lsoa11cd" = "LSOA11CD")) %>% 
  left_join(ccglookup, by = "CCG21CD") %>% 
  group_by(NHSER21NM, IMDquintile) %>% 
  summarise(count = n()) %>% 
  ungroup() %>% 
  group_by(NHSER21NM) %>% 
  mutate(pct_reg = count / sum(count)) %>% 
  ungroup() %>% 
  group_by(IMDquintile) %>% 
  mutate(pct_nat = count / sum(count)) %>% 
  ungroup() %>% 
  mutate(IMDquintile = factor(as.character(IMDquintile), levels = c("5", "4", "3", "2", "1")))

# get key figures
se_core20 <- reg_imd %>% 
  filter(NHSER21NM == "South East" & IMDquintile == 1) %>% 
  select(pct_reg)

core20_in_se <- reg_imd %>% 
  filter(NHSER21NM == "South East" & IMDquintile == 1) %>% 
  select(pct_nat)

# now plot
reg_imd %>% 
  ggplot(aes(x = NHSER21NM, y = count, fill = IMDquintile)) +
  geom_bar(position = "fill", stat = "identity") +
  geom_text(aes(y = count, label = paste0(round(100*pct_reg, 1), "%")), 
            size = 4, position = position_fill(vjust = 0.8)) +
  scale_fill_manual(values = c("#E8EDEE", "#768692", "#00A9CE", "#41B6E6", "#005EB8")) +
  labs(title = paste0(round(100*se_core20, 1), "% of LSOAs in the South East are CORE20"),
       subtitle = "Percentage of LSOAs in each IMD quintile") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10))

reg_imd %>% 
  ggplot(aes(x = IMDquintile, y = count, fill = NHSER21NM)) +
  geom_bar(position = "fill", stat = "identity") +
  geom_text(aes(y = count, label = paste0(round(100*pct_nat, 1), "%")), 
            size = 4, position = position_fill(vjust = 0.8)) +
  scale_fill_manual(values = c("#E8EDEE", "#768692", "#78BE20", "#00A499", "#00A9CE", "#41B6E6", "#005EB8")) +
  labs(title = paste0(round(100*core20_in_se, 1), "% of CORE20 LSOAs are in the South East"),
       subtitle = "Percentage of LSOAs in each IMD quintile from each region") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.title = element_blank())
