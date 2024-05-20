# Cleaning school data
# 5/23/2023
# Alexa Freedman


# Script to work with School data - 2017-2018
library(readxl)
library(writexl)
library(dplyr)
library(tidyr)
library(stringr)


# Codebook for 2017-2018 school data:
## https://civilrightsdata.ed.gov/data

# Data download: 
## https://ffiec.cfpb.gov/data-publication/one-year-national-loan-level-dataset/2019


# Calculate school-level variables
# Set working directory to where school data files are saved
setwd("C:/School_data/2017-18 Public-Use Files/Data/SCH/CRDC/CSV")

# Read in school-level data and merge files
chars <-  read.csv("School Characteristics.csv")
lea_chars <- read.csv("LEA Characteristics.csv")
enroll <- read.csv("Enrollment.csv")
suspension <- read.csv("Suspensions.csv")
punishment <- read.csv("Corporal Punishment.csv")
expel <- read.csv("Expulsions.csv")
arrest <- read.csv("Referrals and Arrests.csv")
support <- read.csv("School Support.csv")


combine <- merge(chars, lea_chars, all=FALSE) %>% 
  merge(., enroll, all=FALSE) %>% 
  merge(., suspension, all=FALSE) %>% 
  merge(., punishment, all=FALSE) %>% 
  merge(., expel, all=FALSE) %>% 
  merge(., arrest, all=FALSE) %>% 
  merge(., support, all=FALSE)


# Calculate variables
schools <- combine %>%
  # Convert -9 and other values to NA (page 12 of data guide lists reason each value used)
  mutate(across(where(is.numeric), ~ na_if(.x, -3)),
         across(where(is.numeric), ~ na_if(.x, -5)),
         across(where(is.numeric), ~ na_if(.x, -6)),
         across(where(is.numeric), ~ na_if(.x, -8)),
         across(where(is.numeric), ~ na_if(.x, -9)),
         across(where(is.numeric), ~ na_if(.x, -11))) %>% 
  # Calculate variables of interest
  #Total white enrollment (white male + white female)
  mutate(total_white = SCH_ENR_WH_M + SCH_ENR_WH_F) %>%
  #Total Black enrollment (Black male + Black female)
  mutate(total_black = SCH_ENR_BL_M + SCH_ENR_BL_F) %>%
  # Total enrollment
  mutate(total_enroll = TOT_ENR_M + TOT_ENR_F) %>%
  # Proportion of white students
  mutate(prop_white = total_white / total_enroll) %>%
  #Proportion of Black students
  mutate(prop_black = total_black / total_enroll) %>%
  # Difference in proportion Black vs white
  mutate(diff_prop_bw = prop_black - prop_white) %>%
  # Total punishment 
  rowwise() %>% # This ensures sums are calculated for each row 
  mutate(punish_white = sum(
    # Corporal punishment
    SCH_DISCWODIS_CORP_WH_M, SCH_DISCWODIS_CORP_WH_F,
    SCH_DISCWDIS_CORP_IDEA_WH_M, SCH_DISCWDIS_CORP_IDEA_WH_F,
    # Expulsion
    SCH_DISCWODIS_EXPWE_WH_M, SCH_DISCWODIS_EXPWE_WH_F,
    SCH_DISCWODIS_EXPWOE_WH_M, SCH_DISCWODIS_EXPWOE_WH_F,
    SCH_DISCWODIS_EXPZT_WH_M, SCH_DISCWODIS_EXPZT_WH_F,
    SCH_DISCWDIS_EXPWE_IDEA_WH_M, SCH_DISCWDIS_EXPWE_IDEA_WH_F,
    SCH_DISCWDIS_EXPWOE_IDEA_WH_M, SCH_DISCWDIS_EXPWOE_IDEA_WH_F,
    SCH_DISCWDIS_EXPZT_IDEA_WH_M, SCH_DISCWDIS_EXPZT_IDEA_WH_F,
    # Referrals and arrests
    SCH_DISCWODIS_REF_WH_M, SCH_DISCWODIS_REF_WH_F,
    SCH_DISCWDIS_REF_IDEA_WH_M, SCH_DISCWDIS_REF_IDEA_WH_F,
    SCH_DISCWODIS_ARR_WH_M, SCH_DISCWODIS_ARR_WH_F,
    SCH_DISCWDIS_ARR_IDEA_WH_M, SCH_DISCWDIS_ARR_IDEA_WH_F,
    # Suspension
    SCH_DISCWODIS_ISS_WH_M, SCH_DISCWODIS_ISS_WH_F,
    SCH_DISCWODIS_SINGOOS_WH_M, SCH_DISCWODIS_SINGOOS_WH_F,
    SCH_DISCWODIS_MULTOOS_WH_M, SCH_DISCWODIS_MULTOOS_WH_F,
    SCH_DISCWDIS_ISS_IDEA_WH_M, SCH_DISCWDIS_ISS_IDEA_WH_F,
    SCH_DISCWDIS_SINGOOS_IDEA_WH_M, SCH_DISCWDIS_SINGOOS_IDEA_WH_F,
    SCH_DISCWDIS_MULTOOS_IDEA_WH_M, SCH_DISCWDIS_MULTOOS_IDEA_WH_F, 
    na.rm=TRUE)) %>%
  mutate(punish_black = sum(
    # Corporal punishment
    SCH_DISCWODIS_CORP_BL_M, SCH_DISCWODIS_CORP_BL_F,
    SCH_DISCWDIS_CORP_IDEA_BL_M, SCH_DISCWDIS_CORP_IDEA_BL_F,
    # Expulsion
    SCH_DISCWODIS_EXPWE_BL_M, SCH_DISCWODIS_EXPWE_BL_F,
    SCH_DISCWODIS_EXPWOE_BL_M, SCH_DISCWODIS_EXPWOE_BL_F,
    SCH_DISCWODIS_EXPZT_BL_M, SCH_DISCWODIS_EXPZT_BL_F,
    SCH_DISCWDIS_EXPWE_IDEA_BL_M, SCH_DISCWDIS_EXPWE_IDEA_BL_F,
    SCH_DISCWDIS_EXPWOE_IDEA_BL_M, SCH_DISCWDIS_EXPWOE_IDEA_BL_F,
    SCH_DISCWDIS_EXPZT_IDEA_BL_M, SCH_DISCWDIS_EXPZT_IDEA_BL_F,
    # Referrals and arrests
    SCH_DISCWODIS_REF_BL_M, SCH_DISCWODIS_REF_BL_F,
    SCH_DISCWDIS_REF_IDEA_BL_M, SCH_DISCWDIS_REF_IDEA_BL_F,
    SCH_DISCWODIS_ARR_BL_M, SCH_DISCWODIS_ARR_BL_F,
    SCH_DISCWDIS_ARR_IDEA_BL_M, SCH_DISCWDIS_ARR_IDEA_BL_F,
    # Suspension
    SCH_DISCWODIS_ISS_BL_M, SCH_DISCWODIS_ISS_BL_F, 
    SCH_DISCWODIS_SINGOOS_BL_M, SCH_DISCWODIS_SINGOOS_BL_F,
    SCH_DISCWODIS_MULTOOS_BL_M, SCH_DISCWODIS_MULTOOS_BL_F,
    SCH_DISCWDIS_ISS_IDEA_BL_M, SCH_DISCWDIS_ISS_IDEA_BL_F,
    SCH_DISCWDIS_SINGOOS_IDEA_BL_M, SCH_DISCWDIS_SINGOOS_IDEA_BL_F,
    SCH_DISCWDIS_MULTOOS_IDEA_BL_M, SCH_DISCWDIS_MULTOOS_IDEA_BL_F, 
    na.rm=TRUE)) %>%
  mutate(punish_hispanic = sum(
    # Corporal punishment
    SCH_DISCWODIS_CORP_HI_M, SCH_DISCWODIS_CORP_HI_F,
    SCH_DISCWDIS_CORP_IDEA_HI_M, SCH_DISCWDIS_CORP_IDEA_HI_F,
    # Expulsion
    SCH_DISCWODIS_EXPWE_BL_M, SCH_DISCWODIS_EXPWE_BL_F,
    SCH_DISCWODIS_EXPWOE_BL_M, SCH_DISCWODIS_EXPWOE_BL_F,
    SCH_DISCWODIS_EXPZT_BL_M, SCH_DISCWODIS_EXPZT_BL_F,
    SCH_DISCWDIS_EXPWE_IDEA_BL_M, SCH_DISCWDIS_EXPWE_IDEA_BL_F,
    SCH_DISCWDIS_EXPWOE_IDEA_BL_M, SCH_DISCWDIS_EXPWOE_IDEA_BL_F,
    SCH_DISCWDIS_EXPZT_IDEA_BL_M, SCH_DISCWDIS_EXPZT_IDEA_BL_F,
    # Referrals and arrests
    SCH_DISCWODIS_REF_BL_M, SCH_DISCWODIS_REF_BL_F,
    SCH_DISCWDIS_REF_IDEA_BL_M, SCH_DISCWDIS_REF_IDEA_BL_F,
    SCH_DISCWODIS_ARR_BL_M, SCH_DISCWODIS_ARR_BL_F,
    SCH_DISCWDIS_ARR_IDEA_BL_M, SCH_DISCWDIS_ARR_IDEA_BL_F,
    # Suspension
    SCH_DISCWODIS_ISS_BL_M, SCH_DISCWODIS_ISS_BL_F, 
    SCH_DISCWODIS_SINGOOS_BL_M, SCH_DISCWODIS_SINGOOS_BL_F,
    SCH_DISCWODIS_MULTOOS_BL_M, SCH_DISCWODIS_MULTOOS_BL_F,
    SCH_DISCWDIS_ISS_IDEA_BL_M, SCH_DISCWDIS_ISS_IDEA_BL_F,
    SCH_DISCWDIS_SINGOOS_IDEA_BL_M, SCH_DISCWDIS_SINGOOS_IDEA_BL_F,
    SCH_DISCWDIS_MULTOOS_IDEA_BL_M, SCH_DISCWDIS_MULTOOS_IDEA_BL_F, 
    na.rm=TRUE)) %>%
  mutate(punish_white_prop = ifelse(total_white == 0, NA, 
                                    punish_white / total_white)) %>%
  mutate(punish_black_prop = ifelse(total_black == 0, NA, 
                                    punish_black / total_black)) %>%
  # Cap proportion at 1 (a few schools have Black prop > 1 and/or white prop > 1)
  mutate(punish_white_prop = ifelse(punish_white_prop > 1, 1, punish_white_prop)) %>%
  mutate(punish_black_prop = ifelse(punish_black_prop > 1, 1, punish_black_prop)) %>%
  # Difference in proportions
  mutate(diff_punish_bw = punish_black_prop - punish_white_prop) %>%
  # Total school punishment
  mutate(punish_total = sum(
    # Corporal punishment
    SCH_CORPINSTANCES_WODIS, SCH_CORPINSTANCES_WODIS,
    # Expulsion
    TOT_DISCWODIS_EXPWE_M, TOT_DISCWODIS_EXPWE_F,
    TOT_DISCWODIS_EXPWOE_M, TOT_DISCWODIS_EXPWOE_F,
    TOT_DISCWODIS_EXPZT_M, TOT_DISCWODIS_EXPZT_F, 
    TOT_DISCWDIS_EXPWE_IDEA_M, TOT_DISCWDIS_EXPWE_IDEA_F,
    TOT_DISCWDIS_EXPWOE_IDEA_M, TOT_DISCWDIS_EXPWOE_IDEA_F,
    TOT_DISCWDIS_EXPZT_IDEA_M, TOT_DISCWDIS_EXPZT_IDEA_F,
    # Referrals and arrests
    TOT_DISCWODIS_REF_M, TOT_DISCWODIS_REF_F,
    TOT_DISCWDIS_REF_IDEA_M, TOT_DISCWDIS_REF_IDEA_F,
    TOT_DISCWODIS_ARR_M, TOT_DISCWODIS_ARR_F,
    TOT_DISCWDIS_ARR_IDEA_M, TOT_DISCWDIS_ARR_IDEA_F,
    # Suspensions
    TOT_DISCWODIS_ISS_M, TOT_DISCWODIS_ISS_F,
    TOT_DISCWODIS_SINGOOS_M, TOT_DISCWODIS_SINGOOS_F,
    TOT_DISCWODIS_MULTOOS_M, TOT_DISCWODIS_MULTOOS_F,
    TOT_DISCWDIS_ISS_IDEA_M, TOT_DISCWDIS_ISS_IDEA_F,
    TOT_DISCWDIS_SINGOOS_IDEA_M, TOT_DISCWDIS_SINGOOS_IDEA_F,
    TOT_DISCWDIS_MULTOOS_IDEA_M, TOT_DISCWDIS_MULTOOS_IDEA_F, 
    na.rm=TRUE)) %>%
  mutate(punish_total_prop = punish_total / total_enroll) %>% 
  # Cap at 1
  mutate(punish_total_prop = ifelse(punish_total_prop > 1, 1, punish_total_prop)) %>% 
  # Proportion of teachers with <3 years experience
  mutate(prop_teach_lt3yrs = ((SCH_FTETEACH_FY + SCH_FTETEACH_SY)/SCH_FTETEACH_TOT)) %>% 
  # Proportion of teachers with >10 days absent 
  mutate(prop_teach_highabs = (SCH_FTETEACH_ABSENT/SCH_FTETEACH_TOT)) %>% 
  # Keep only new variables and linking variables to make this easier to work with
  subset(select=c(COMBOKEY, total_white:prop_teach_highabs))



# Bring in file that links schools to census tracts and merge with school data
## File includes:
### Census tract ID (GEOID)
### School IDs for 3 closest schools (NCESSCH)
### Distance-based weight (weight)
### Rank for distance - closest school = 1 (rank)

link <- read_xlsx("C:/School_ct_merge.xlsx")

# Merge linking file with school data by school ID (COMBOKEY/NCESSCH) 
school_link <- merge(link, schools, by.x="NCESSCH", by.y="COMBOKEY")


# Summarize school measures for each census tract using weighted avg
school_link_sum <- school_link %>% 
  group_by(GEOID) %>% 
  summarise(
    # Weighted means
    n_white_wt = weighted.mean(total_white, weight, na.rm=TRUE),
    n_black_wt = weighted.mean(total_black, weight, na.rm=TRUE),
    n_total_wt = weighted.mean(total_enroll, weight, na.rm=TRUE),
    p_white_wt = weighted.mean(prop_white, weight, na.rm=TRUE),
    p_black_wt = weighted.mean(prop_black, weight, na.rm=TRUE),
    p_punish_white_wt = weighted.mean(punish_white_prop, weight, na.rm=TRUE),
    p_punish_black_wt = weighted.mean(punish_black_prop, weight, na.rm=TRUE),
    diff_punish_bw_wt = weighted.mean(diff_punish_bw, weight, na.rm=TRUE),
    p_punish_total_wt = weighted.mean(punish_total_prop, weight, na.rm=TRUE),
    prop_teach_lt3yrs_wt = weighted.mean(prop_teach_lt3yrs, weight, na.rm=TRUE),
    prop_teach_highabs_wt = weighted.mean(prop_teach_highabs, weight, na.rm=TRUE)
    )



# Create a dataset that contains the school IDs and weights used for each census tract calculation
school_link_tracking <- school_link %>% 
  subset(select=c(NCESSCH, GEOID, weight, rank)) %>% 
  # Sort by geoid and rank to keep 1,2,3 in order for collapsing
  arrange(GEOID, rank) %>% 
  group_by(GEOID) %>% 
  summarize(NCESSCH_ids = str_c(NCESSCH, collapse=", "),
            weights = str_c(weight, collapse=", ")) %>% 
  # Now separate into 1,2,3
  separate(weights, c("weight_1", "weight_2", "weight_3"), ", ") %>% 
  separate(NCESSCH_ids, c("NCESSCH_1", "NCESSCH_2", "NCESSCH_3"), ", ")
  # Error message because some CTs only have 1 or 2 schools so generating NA for weights 2/3, NCESSCH 2/3
    ## (linked schools are restricted to those in the same state as the CT, so 
    ##  it happens when there are no other nearby schools in the same state)

# Merge tracking data with  summary datasets
school_link_all <- merge(school_link_tracking, school_link_sum, by="GEOID") 
  ## This dataset contains summary school data for all census tracts US


write.csv(school_link_all, "C:/school_summary_data.csv")








#### DELETE BELOW




# Plot variables and look at correlations
heatmap_data <- school_link_all %>% 
  subset(select=c(n_total_1, n_total_uw, n_total_wt,
                  p_white_1, p_white_uw, p_white_wt,
                  p_black_1, p_black_uw, p_black_wt,
                  p_punish_white_1, p_punish_white_uw, p_punish_white_wt,
                  p_punish_black_1, p_punish_black_uw, p_punish_black_wt,
                  diff_punish_bw_1, diff_punish_bw_uw, diff_punish_bw_wt,
                  p_punish_total_1, p_punish_total_uw, p_punish_total_wt,
                  ap_offered_1, ap_offered_uw, ap_offered_wt,
                  prop_cert_teachers_1, prop_cert_teachers_uw, prop_cert_teachers_wt,
                  prop_teach_ge3yrs_1, prop_teach_ge3yrs_uw, prop_teach_ge3yrs_wt,
                  prop_teach_lowabs_1, prop_teach_lowabs_uw, prop_teach_lowabs_wt))


# Correlations across three measures
# Functions for restricting to lower/upper portion of matrix
# Get lower triangle of the correlation matrix
get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}
# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}
# Order plot based on correlations/distance to group like genes
reorder_cormat <- function(cormat){
  # Use correlation between variables as distance
  dd <- as.dist((1-cormat)/2)
  hc <- hclust(dd)
  cormat <-cormat[hc$order, hc$order]
}

# Overall

# Correlation matrix - use transposed (not long) dataset
corr_mat1 <- cor(na.omit(subset(heatmap_data))) # NA.omit removes any rows with NAs

# Reorder based on similarity (function above)
#corr_mat1 <- reorder_cormat(corr_mat1)

# Keep lower triangle since symmetric 
corr_mat_lower1 <- get_lower_tri(corr_mat1)

# Melt the correlation matrix
melted_corr_mat1 <- melt(corr_mat_lower1, na.rm=TRUE) # remove NAs

# Plot the correlation matrix heat map
heatmap_corrmat <- ggplot(melted_corr_mat1, aes(x=Var1, y=Var2, fill=value)) +
  geom_tile(color="white") +
  theme_minimal()+
  scale_fill_gradient2(low="blue", high="red", mid="white", midpoint=0, limit=c(-1,1), space="Lab", name="Pearson") +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, size = 8, hjust = 1), axis.text.y = element_text(size=8)) +
  xlab("") +
  ylab("") +
  coord_fixed() +
  theme(panel.background=element_blank(), 
        panel.grid.major = element_blank()) +
  geom_text(aes(Var1, Var2, label = round(value, 2)), color = "black", size = 4) 
heatmap_corrmat


# Mapping in R
library(RColorBrewer)
library(sf)
library(tmap)
library(tmaptools)
library(leaflet)
library(gridExtra)

#library(rgdal) # For readOGR (phased out - use sf now)
library(maps)



il_cts <- read_sf("C:/Users/afw5823/Documents/ArcGIS/Shape files/Illinois_ct/2017/tl_2017_17_tract.shp")
chicago_cts <- read_sf("C:/Users/afw5823/Documents/ArcGIS/Shape files/Chicago city census tracts/geo_export_5a65a581-0e09-4f4f-a3d2-4f7b9dddc8db.shp")




# Merge school data with CTs
school_map <- merge(il_cts, school_link_all, by="GEOID") %>% 
  # Indicator for census tract in chicago
  mutate(chicago = ifelse(GEOID %in% chicago_cts$geoid10, 1, 0)) %>% 
  # Create quartiles
  mutate(n_total_wt_q = ifelse(n_total_wt < 580, 1, 
                               ifelse(n_total_wt < 1086, 2, 
                                      ifelse(n_total_wt < 1829, 3, 4)))) %>% 
  # Proportion Black
  mutate(p_black_wt_q = ifelse(p_black_wt < 0.031, 1, 
                               ifelse(p_black_wt < 0.107, 2,
                                      ifelse(p_black_wt < 0.347, 3, 4)))) %>% 
  # Difference in school punishment
  mutate(diff_punish_bw_wt_q = ifelse(diff_punish_bw_wt < 0.093, 1, 
                                      ifelse(diff_punish_bw_wt < 0.195, 2,
                                             ifelse(diff_punish_bw_wt < 0.314, 3, 4))))

summary(school_link_all$diff_punish_bw_wt)
# Plot total enrollment
n_il <- ggplot(data=school_map) +
  geom_sf(aes(fill=as.factor(n_total_wt_q))) +
  scale_fill_brewer(palette = "RdYlBu", direction=-1, name="Total enrollment", labels=c("<580", "581-1,086", "1,087-1,829", ">1,829")) +
  labs(title="Total enrollment, weighted average") +
  theme_void()

n_chi <- ggplot(data=filter(school_map, chicago==1)) +
  geom_sf(aes(fill=as.factor(n_total_wt_q))) +
  scale_fill_brewer(palette = "RdYlBu", direction=-1, name="Total enrollment", labels=c("<580", "581-1,086", "1,087-1,829", ">1,829")) +
  labs(title="Total enrollment, weighted average") +
  theme_void()

nh_il <- ggplot(data=school_map, aes(x=n_total_wt)) +
  geom_histogram(binwidth = 50) +
  xlab("Total enrollment, weighted average") +
  labs(title="Illinois") +
  theme_bw()

nh_chi <- ggplot(data=filter(school_map, chicago==1), aes(x=n_total_wt)) +
  geom_histogram(binwidth = 50) +
  xlab("Total enrollment, weighted average") +
  labs(title="Chicago") +
  theme_bw()

grid.arrange(nh_il, nh_chi, n_il, n_chi, ncol=2)


# Plot proportion of black students
b_il <- ggplot(data=school_map) +
  geom_sf(aes(fill=as.factor(p_black_wt_q))) +
  scale_fill_brewer(palette = "RdYlBu", direction=-1, name="Prop. Black students", labels=c("<0.03", "0.03-0.10", "0.11-0.34", ">0.34")) +
  labs(title="Total enrollment, weighted average") +
  theme_void()

b_chi <- ggplot(data=filter(school_map, chicago==1)) +
  geom_sf(aes(fill=as.factor(p_black_wt_q))) +
  scale_fill_brewer(palette = "RdYlBu", direction=-1, name="Prop. Black students", labels=c("<0.03", "0.03-0.10", "0.11-0.34", ">0.34")) +
  labs(title="Total enrollment, weighted average") +
  theme_void()

bh_il <- ggplot(data=school_map, aes(x=p_black_wt)) +
  geom_histogram(binwidth = .05) +
  xlab("Prop. Black students, weighted average") +
  labs(title="Illinois") +
  theme_bw()

bh_chi <- ggplot(data=filter(school_map, chicago==1), aes(x=p_black_wt)) +
  geom_histogram(binwidth = .05) +
  xlab("Prop. Black students, weighted average") +
  labs(title="Chicago") +
  theme_bw()

grid.arrange(bh_il, bh_chi, b_il, b_chi, ncol=2)


# Plot difference in school punishment
p_il <- ggplot(data=school_map) +
  geom_sf(aes(fill=as.factor(diff_punish_bw_wt_q))) +
  scale_fill_brewer(palette = "RdYlBu", direction=-1, name="B-W diff", labels=c("<0.09", "0.09-0.19", "0.20-0.31", ">0.31")) +
  labs(title="B-W difference in school punishment, weighted average") +
  theme_void()

p_chi <- ggplot(data=filter(school_map, chicago==1)) +
  geom_sf(aes(fill=as.factor(diff_punish_bw_wt_q))) +
  scale_fill_brewer(palette = "RdYlBu", direction=-1, name="B-W diff", labels=c("<0.09", "0.09-0.19", "0.20-0.31", ">0.31")) +
  labs(title="B-W difference in school punishment, weighted average") +
  theme_void()

ph_il <- ggplot(data=school_map, aes(x=diff_punish_bw_wt)) +
  geom_histogram(binwidth = .05) +
  xlab("B-W difference in school punishment, weighted average") +
  labs(title="Illinois") +
  theme_bw()

ph_chi <- ggplot(data=filter(school_map, chicago==1), aes(x=diff_punish_bw_wt)) +
  geom_histogram(binwidth = .05) +
  xlab("B-W difference in school punishment, weighted average") +
  labs(title="Chicago") +
  theme_bw()

grid.arrange(ph_il, ph_chi, p_il, p_chi, ncol=2)



# Offers AP classes
a_il <- ggplot(data=school_map) +
  geom_sf(aes(fill=ap_offered_wt)) +
  #scale_fill_brewer(palette = "RdYlBu", direction=-1, name="B-W diff", labels=c("<0.09", "0.09-0.19", "0.20-0.31", ">0.31")) +
  labs(title="AP classes offered, weighted average") +  theme_void()

a_chi <- ggplot(data=filter(school_map, chicago==1)) +
  geom_sf(aes(fill=ap_offered_wt)) +
  #scale_fill_brewer(palette = "RdYlBu", direction=-1, name="B-W diff", labels=c("<0.09", "0.09-0.19", "0.20-0.31", ">0.31")) +
  labs(title="AP classes offered, weighted average") +
  theme_void()

ah_il <- ggplot(data=school_map, aes(x=ap_offered_wt)) +
  geom_histogram(binwidth = .05) +
  xlab("AP classes offered, weighted average") +
  labs(title="Illinois") +
  theme_bw()

ah_chi <- ggplot(data=filter(school_map, chicago==1), aes(x=ap_offered_wt)) +
  geom_histogram(binwidth = .05) +
  xlab("AP classes offered, weighted average") +
  labs(title="Chicago") +
  theme_bw()

grid.arrange(ah_il, ah_chi, a_il, a_chi, ncol=2)




# Link with MWMH data
# Using Eric's school data:
mwmh_sch <- openxlsx::read.xlsx("C:/Users/afw5823/OneDrive - Northwestern University/Documents - FHRC/Administrative/Personal Folders/Eric/MWMH projects/neighborhood murder CVD risk project/Mapping/School_mapping/MWMH School Addresses Matched 11.26.18_CorrectCopy.xlsx")

# Bring in Eric's home geo data
mwmh_ct <- read.csv("C:/Users/afw5823/OneDrive - Northwestern University/Documents - FHRC/Administrative/Personal Folders/Eric/MWMH projects/neighborhood murder CVD risk project/Mapping/MWMH_ALLhomesFIPS_intersect_trim.csv")

# Convert block group to census tract 
mwmh_ct2 <- mwmh_ct %>% 
  mutate(GEOID = substr(GEOID, 1, 11))

# Merge mwmh school and ct data
mwmh <- merge(mwmh_ct2, mwmh_sch, by.x="USER_subid", by.y="ChildID", all=TRUE)

# Merge mwmh with individual school data based on school ID
### First make restricted school data file
schools_res <- schools %>% 
  subset(select=c(COMBOKEY, total_white, total_black, total_enroll, prop_white, prop_black, punish_white_prop,
                  punish_black_prop, diff_punish_bw, ap_offered))

# Only keep those who link to a high school
## NOTE: Most MWMH are elementary/middle schools so won't link
mwmh2 <- merge(mwmh, schools_res, by="COMBOKEY")

# How many unique schools?
check <- as.data.frame(table(mwmh2$COMBOKEY))
## 8 schools (24 students)

check2 <- as.data.frame(table(mwmh2$SchoolNameGrades))

# Link with summary school data by CT
mwmh3 <- merge(mwmh2, school_link_all, by="GEOID") %>% 
# Any where the true school is the closest school? 
  mutate(correct_sch1 = ifelse(COMBOKEY == NCESSCH_1, 1, 0)) %>% 
  mutate(correct_sch2 = ifelse(COMBOKEY == NCESSCH_2, 1, 0)) %>% 
  mutate(correct_sch3 = ifelse(COMBOKEY == NCESSCH_3, 1, 0))

table(mwmh3$correct_sch1)
table(mwmh3$correct_sch2)
table(mwmh3$correct_sch3)

check3 <- subset(mwmh3, select=c(COMBOKEY, SchoolNameGrades, NCESSCH_1:NCESSCH_3, correct_sch1:correct_sch3))

### Create heatmaps for correlations across variables

# Demographic characteristics
heatmap_dem <- subset(mwmh3, select=c(total_enroll, n_total_1, n_total_uw, n_total_wt,
                                      total_black, n_black_1, n_black_uw, n_black_wt,
                                      total_white, n_white_1, n_white_uw, n_white_wt))
# Correlation matrix - use transposed (not long) dataset
corr_mat1 <- cor(na.omit(subset(heatmap_dem))) # NA.omit removes any rows with NAs

# Keep lower triangle since symmetric 
corr_mat_lower1 <- get_lower_tri(corr_mat1)

# Melt the correlation matrix
melted_corr_mat1 <- melt(corr_mat_lower1, na.rm=TRUE) # remove NAs

# Plot the correlation matrix heat map
heatmap_corrmat <- ggplot(melted_corr_mat1, aes(x=Var1, y=Var2, fill=value)) +
  geom_tile(color="white") +
  theme_minimal()+
  scale_fill_gradient2(low="blue", high="red", mid="white", midpoint=0, limit=c(-1,1), space="Lab", name="Pearson") +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, size = 8, hjust = 1), axis.text.y = element_text(size=8)) +
  xlab("") +
  ylab("") +
  coord_fixed() +
  theme(panel.background=element_blank(), 
        panel.grid.major = element_blank()) +
  geom_text(aes(Var1, Var2, label = round(value, 2)), color = "black", size = 4) 
heatmap_corrmat



# Demographic characteristics - proportions
heatmap_dem <- subset(mwmh3, select=c(prop_black, p_black_1, p_black_uw, p_black_wt,
                                      prop_white, p_white_1, p_white_uw, p_white_wt))
# Correlation matrix - use transposed (not long) dataset
corr_mat1 <- cor(na.omit(subset(heatmap_dem))) # NA.omit removes any rows with NAs

# Keep lower triangle since symmetric 
corr_mat_lower1 <- get_lower_tri(corr_mat1)

# Melt the correlation matrix
melted_corr_mat1 <- melt(corr_mat_lower1, na.rm=TRUE) # remove NAs

# Plot the correlation matrix heat map
heatmap_corrmat <- ggplot(melted_corr_mat1, aes(x=Var1, y=Var2, fill=value)) +
  geom_tile(color="white") +
  theme_minimal()+
  scale_fill_gradient2(low="blue", high="red", mid="white", midpoint=0, limit=c(-1,1), space="Lab", name="Pearson") +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, size = 8, hjust = 1), axis.text.y = element_text(size=8)) +
  xlab("") +
  ylab("") +
  coord_fixed() +
  theme(panel.background=element_blank(), 
        panel.grid.major = element_blank()) +
  geom_text(aes(Var1, Var2, label = round(value, 2)), color = "black", size = 4) 
heatmap_corrmat



# Demographic characteristics - proportions
heatmap_pun <- subset(mwmh3, select=c(punish_black_prop, p_punish_black_1, p_punish_black_uw, p_punish_black_wt,
                                      punish_white_prop, p_punish_white_1, p_punish_white_uw, p_punish_white_wt,
                                      diff_punish_bw, diff_punish_bw_1, diff_punish_bw_uw, diff_punish_bw_wt))
# Correlation matrix - use transposed (not long) dataset
corr_mat1 <- cor(na.omit(subset(heatmap_pun))) # NA.omit removes any rows with NAs

# Keep lower triangle since symmetric 
corr_mat_lower1 <- get_lower_tri(corr_mat1)

# Melt the correlation matrix
melted_corr_mat1 <- melt(corr_mat_lower1, na.rm=TRUE) # remove NAs

# Plot the correlation matrix heat map
heatmap_corrmat <- ggplot(melted_corr_mat1, aes(x=Var1, y=Var2, fill=value)) +
  geom_tile(color="white") +
  theme_minimal()+
  scale_fill_gradient2(low="blue", high="red", mid="white", midpoint=0, limit=c(-1,1), space="Lab", name="Pearson") +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, size = 8, hjust = 1), axis.text.y = element_text(size=8)) +
  xlab("") +
  ylab("") +
  coord_fixed() +
  theme(panel.background=element_blank(), 
        panel.grid.major = element_blank()) +
  geom_text(aes(Var1, Var2, label = round(value, 2)), color = "black", size = 4) 
heatmap_corrmat


#  AP characteristics 
## DOES NOT RUN -- ALL 1's - no variation (all true schools and summary schools offer AP classes)
heatmap_ap <- subset(mwmh3, select=c(ap_offered, ap_offered_1, ap_offered_uw, ap_offered_wt))
# Correlation matrix - use transposed (not long) dataset
corr_mat1 <- cor(na.omit(subset(heatmap_ap))) # NA.omit removes any rows with NAs

# Keep lower triangle since symmetric 
corr_mat_lower1 <- get_lower_tri(corr_mat1)

# Melt the correlation matrix
melted_corr_mat1 <- melt(corr_mat_lower1, na.rm=TRUE) # remove NAs

# Plot the correlation matrix heat map
heatmap_corrmat <- ggplot(melted_corr_mat1, aes(x=Var1, y=Var2, fill=value)) +
  geom_tile(color="white") +
  theme_minimal()+
  scale_fill_gradient2(low="blue", high="red", mid="white", midpoint=0, limit=c(-1,1), space="Lab", name="Pearson") +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, size = 8, hjust = 1), axis.text.y = element_text(size=8)) +
  xlab("") +
  ylab("") +
  coord_fixed() +
  theme(panel.background=element_blank(), 
        panel.grid.major = element_blank()) +
  geom_text(aes(Var1, Var2, label = round(value, 2)), color = "black", size = 4) 
heatmap_corrmat



# For reference - plot where 24 MWMH students live
school_map2 <- school_map %>% 
  mutate(mwmh = ifelse(GEOID %in% mwmh3$GEOID, 1, 0)) 

CrossTable(school_map2$mwmh, school_map2$chicago) # All chicago BGs - two live in same CT since 22 instead of 24


ggplot(data=filter(school_map2, chicago==1)) +
  geom_sf(aes(fill=as.factor(mwmh))) +
  scale_fill_manual(values=c("lightgray", "blue") ) +
  labs(title="MWMH census tracts") +
  theme_void()

# ggplot(data=filter(school_map2, chicago==1)) +
#   geom_sf(aes(fill=as.factor(NSC))) +
#   #scale_fill_manual(values=c("lightgray", "blue") ) +
#   labs(title="MWMH census tracts") +
#   theme_void()
# 






# LEA characteristics for district zip code
lea_chars <- read.csv("LEA Characteristics.csv")
chars_all <- merge(chars_res_hs, lea_chars, all=FALSE)


# Enrollment characteristics
enroll <- read.csv("Enrollment.csv")

# Merge with chars_res
school_desc <- merge(chars_all, enroll, all=FALSE)
  # No 'by' merges on all shared variables
  # All=FALSE only keeps those in both data frames (I think)
  # Alternatively, could restrict enroll file to those in chars_res if don't need chars_res variables



# Read in and merge school datasets of interest
suspension <- read.csv("Suspensions.csv")
  combine <- merge(school_desc, suspension, all=FALSE)
punishment <- read.csv("Corporal Punishment.csv")
  combine1 <- merge(combine, punishment, all=FALSE) 
expel <- read.csv("Expulsions.csv")
  combine2 <- merge(combine1, expel, all=FALSE)
arrest <- read.csv("Referrals and Arrests.csv")
  combine3 <- merge(combine2, arrest, all=FALSE)
support <- read.csv("School Support.csv")
  combine4 <- merge(combine3, support, all=FALSE)
ap_class <- read.csv("Advanced Placement.csv")
  combine5 <- merge(combine4, ap_class, all=FALSE)
adv_math <- read.csv("Advanced Mathematics.csv", all=FALSE)
  combine6 <- merge(combine5, adv_math, all=FALSE)
gifted <- read.csv("Gifted and Talented.csv")
  combine7 <- merge(combine6, gifted, all=FALSE)
satact <- read.csv("SAT and ACT.csv")
  combine8 <- merge(combine7, satact, all=FALSE)
sports <- read.csv("Single-sex Athletics.csv") 
  combine9 <- merge(combine8, sports, all=FALSE)
IB <- read.csv("International Baccalaureate.csv")
  combine10 <- merge(combine9, IB, all=FALSE)
algebra <- read.csv("Algebra I.csv") 
  combine11 <- merge(combine10, algebra, all=FALSE)
dualenroll <- read.csv("Dual Enrollment.csv")
  combine12 <- merge(combine11, dualenroll, all=FALSE)
  
  # Bring in IB to look at AP or IB as a variable

# Calculate variables
schools <- combine12 %>%
  # Convert -9 to NA (page 12 of data guide lists reason each value used)
  na_if(-3) %>%
  na_if(-5) %>%
  na_if(-6) %>%
  na_if(-8) %>%
  na_if(-9) %>% 
  na_if(-11) %>%
  # Calculate variables of interest
  #Total white enrollment (white male + white female)
  mutate(total_white = SCH_ENR_WH_M + SCH_ENR_WH_F) %>%
  #Total Black enrollment (Black male + Black female)
  mutate(total_black = SCH_ENR_BL_M + SCH_ENR_BL_F) %>%
  # Total enrollment
  mutate(total_enroll = TOT_ENR_M + TOT_ENR_F) %>%
  # Indicator for larger proportion of Black vs White students
  mutate(black_plurality = ifelse(total_black > total_white, 1, 0)) %>%
  # Indicator variable if no Black or no White students (no schools have 1)
  mutate(bw_indicator = ifelse(total_white > 0 & total_black > 0, 0,
                               ifelse(total_white == 0 & total_black == 0, 1, # 1 if no white and no Black students)
                                      ifelse(total_black == 0, 2, #2 if no Black students
                                             ifelse(total_white == 0, 3, NA))))) %>% # 3 if no white students
  # Proportion of white students
  mutate(prop_white = ifelse(total_white == 0, NA, 
                             total_white / total_enroll)) %>%
  #Proportion of Black students
  mutate(prop_black = ifelse(total_black == 0, NA,
                             total_black / total_enroll)) %>%
  # Difference in proportion Black vs white
  mutate(diff_prop_bw = prop_black - prop_white) %>%
  # Total punishment 
  rowwise() %>% # This ensures sums are calculated for each row (default is all rows at once, use ungroup() to remove)
  mutate(punish_white = sum(
                         # Corporal punishment
                         SCH_DISCWODIS_CORP_WH_M, SCH_DISCWODIS_CORP_WH_F,
                         SCH_DISCWDIS_CORP_IDEA_WH_M, SCH_DISCWDIS_CORP_IDEA_WH_F,
                         # Expulsion
                         SCH_DISCWODIS_EXPWE_WH_M, SCH_DISCWODIS_EXPWE_WH_F,
                         SCH_DISCWODIS_EXPWOE_WH_M, SCH_DISCWODIS_EXPWOE_WH_F,
                         SCH_DISCWODIS_EXPZT_WH_M, SCH_DISCWODIS_EXPZT_WH_F,
                         SCH_DISCWDIS_EXPWE_IDEA_WH_M, SCH_DISCWDIS_EXPWE_IDEA_WH_F,
                         SCH_DISCWDIS_EXPWOE_IDEA_WH_M, SCH_DISCWDIS_EXPWOE_IDEA_WH_F,
                         SCH_DISCWDIS_EXPZT_IDEA_WH_M, SCH_DISCWDIS_EXPZT_IDEA_WH_F,
                         # Referrals and arrests
                         SCH_DISCWODIS_REF_WH_M, SCH_DISCWODIS_REF_WH_F,
                         SCH_DISCWDIS_REF_IDEA_WH_M, SCH_DISCWDIS_REF_IDEA_WH_F,
                         SCH_DISCWODIS_ARR_WH_M, SCH_DISCWODIS_ARR_WH_F,
                         SCH_DISCWDIS_ARR_IDEA_WH_M, SCH_DISCWDIS_ARR_IDEA_WH_F,
                         # Suspension
                         SCH_DISCWODIS_ISS_WH_M, SCH_DISCWODIS_ISS_WH_F,
                         SCH_DISCWODIS_SINGOOS_WH_M, SCH_DISCWODIS_SINGOOS_WH_F,
                         SCH_DISCWODIS_MULTOOS_WH_M, SCH_DISCWODIS_MULTOOS_WH_F,
                         SCH_DISCWDIS_ISS_IDEA_WH_M, SCH_DISCWDIS_ISS_IDEA_WH_F,
                         SCH_DISCWDIS_SINGOOS_IDEA_WH_M, SCH_DISCWDIS_SINGOOS_IDEA_WH_F,
                         SCH_DISCWDIS_MULTOOS_IDEA_WH_M, SCH_DISCWDIS_MULTOOS_IDEA_WH_F, 
                         na.rm=TRUE)) %>%
  mutate(punish_black = sum(
                        # Corporal punishment
                         SCH_DISCWODIS_CORP_BL_M, SCH_DISCWODIS_CORP_BL_F,
                         SCH_DISCWDIS_CORP_IDEA_BL_M, SCH_DISCWDIS_CORP_IDEA_BL_F,
                         # Expulsion
                         SCH_DISCWODIS_EXPWE_BL_M, SCH_DISCWODIS_EXPWE_BL_F,
                         SCH_DISCWODIS_EXPWOE_BL_M, SCH_DISCWODIS_EXPWOE_BL_F,
                         SCH_DISCWODIS_EXPZT_BL_M, SCH_DISCWODIS_EXPZT_BL_F,
                         SCH_DISCWDIS_EXPWE_IDEA_BL_M, SCH_DISCWDIS_EXPWE_IDEA_BL_F,
                         SCH_DISCWDIS_EXPWOE_IDEA_BL_M, SCH_DISCWDIS_EXPWOE_IDEA_BL_F,
                         SCH_DISCWDIS_EXPZT_IDEA_BL_M, SCH_DISCWDIS_EXPZT_IDEA_BL_F,
                         # Referrals and arrests
                         SCH_DISCWODIS_REF_BL_M, SCH_DISCWODIS_REF_BL_F,
                         SCH_DISCWDIS_REF_IDEA_BL_M, SCH_DISCWDIS_REF_IDEA_BL_F,
                         SCH_DISCWODIS_ARR_BL_M, SCH_DISCWODIS_ARR_BL_F,
                         SCH_DISCWDIS_ARR_IDEA_BL_M, SCH_DISCWDIS_ARR_IDEA_BL_F,
                         # Suspension
                         SCH_DISCWODIS_ISS_BL_M, SCH_DISCWODIS_ISS_BL_F, 
                         SCH_DISCWODIS_SINGOOS_BL_M, SCH_DISCWODIS_SINGOOS_BL_F,
                         SCH_DISCWODIS_MULTOOS_BL_M, SCH_DISCWODIS_MULTOOS_BL_F,
                         SCH_DISCWDIS_ISS_IDEA_BL_M, SCH_DISCWDIS_ISS_IDEA_BL_F,
                         SCH_DISCWDIS_SINGOOS_IDEA_BL_M, SCH_DISCWDIS_SINGOOS_IDEA_BL_F,
                         SCH_DISCWDIS_MULTOOS_IDEA_BL_M, SCH_DISCWDIS_MULTOOS_IDEA_BL_F, 
                         na.rm=TRUE)) %>%
  mutate(punish_hispanic = sum(
    # Corporal punishment
    SCH_DISCWODIS_CORP_HI_M, SCH_DISCWODIS_CORP_HI_F,
    SCH_DISCWDIS_CORP_IDEA_HI_M, SCH_DISCWDIS_CORP_IDEA_HI_F,
    # Expulsion
    SCH_DISCWODIS_EXPWE_BL_M, SCH_DISCWODIS_EXPWE_BL_F,
    SCH_DISCWODIS_EXPWOE_BL_M, SCH_DISCWODIS_EXPWOE_BL_F,
    SCH_DISCWODIS_EXPZT_BL_M, SCH_DISCWODIS_EXPZT_BL_F,
    SCH_DISCWDIS_EXPWE_IDEA_BL_M, SCH_DISCWDIS_EXPWE_IDEA_BL_F,
    SCH_DISCWDIS_EXPWOE_IDEA_BL_M, SCH_DISCWDIS_EXPWOE_IDEA_BL_F,
    SCH_DISCWDIS_EXPZT_IDEA_BL_M, SCH_DISCWDIS_EXPZT_IDEA_BL_F,
    # Referrals and arrests
    SCH_DISCWODIS_REF_BL_M, SCH_DISCWODIS_REF_BL_F,
    SCH_DISCWDIS_REF_IDEA_BL_M, SCH_DISCWDIS_REF_IDEA_BL_F,
    SCH_DISCWODIS_ARR_BL_M, SCH_DISCWODIS_ARR_BL_F,
    SCH_DISCWDIS_ARR_IDEA_BL_M, SCH_DISCWDIS_ARR_IDEA_BL_F,
    # Suspension
    SCH_DISCWODIS_ISS_BL_M, SCH_DISCWODIS_ISS_BL_F, 
    SCH_DISCWODIS_SINGOOS_BL_M, SCH_DISCWODIS_SINGOOS_BL_F,
    SCH_DISCWODIS_MULTOOS_BL_M, SCH_DISCWODIS_MULTOOS_BL_F,
    SCH_DISCWDIS_ISS_IDEA_BL_M, SCH_DISCWDIS_ISS_IDEA_BL_F,
    SCH_DISCWDIS_SINGOOS_IDEA_BL_M, SCH_DISCWDIS_SINGOOS_IDEA_BL_F,
    SCH_DISCWDIS_MULTOOS_IDEA_BL_M, SCH_DISCWDIS_MULTOOS_IDEA_BL_F, 
    na.rm=TRUE)) %>%
  mutate(punish_white_prop = ifelse(total_white == 0, NA, 
                                    punish_white / total_white)) %>%
  mutate(punish_black_prop = ifelse(total_black == 0, NA, 
                                    punish_black / total_black)) %>%
  # Cap proportion at 1 (35 schools have Black prop > 1 (max=9) and 5 schools have white prop > 1 (max=1.9))
  mutate(punish_white_prop = ifelse(punish_white_prop > 1, 1, punish_white_prop)) %>%
  mutate(punish_black_prop = ifelse(punish_black_prop > 1, 1, punish_black_prop)) %>%
  # Difference in proportions
  mutate(diff_punish_bw = punish_black_prop - punish_white_prop) %>%
  # Suspension only
  mutate(suspend_white = sum(SCH_DISCWODIS_ISS_WH_M, SCH_DISCWODIS_ISS_WH_F,
                             SCH_DISCWODIS_SINGOOS_WH_M, SCH_DISCWODIS_SINGOOS_WH_F,
                             SCH_DISCWODIS_MULTOOS_WH_M, SCH_DISCWODIS_MULTOOS_WH_F,
                             SCH_DISCWDIS_ISS_IDEA_WH_M, SCH_DISCWDIS_ISS_IDEA_WH_F,
                             SCH_DISCWDIS_SINGOOS_IDEA_WH_M, SCH_DISCWDIS_SINGOOS_IDEA_WH_F,
                             SCH_DISCWDIS_MULTOOS_IDEA_WH_M, SCH_DISCWDIS_MULTOOS_IDEA_WH_F, 
                             na.rm=TRUE)) %>%
  mutate(suspend_black = sum(SCH_DISCWODIS_ISS_BL_M, SCH_DISCWODIS_ISS_BL_F,
                             SCH_DISCWODIS_SINGOOS_BL_M, SCH_DISCWODIS_SINGOOS_BL_F,
                             SCH_DISCWODIS_MULTOOS_BL_M, SCH_DISCWODIS_MULTOOS_BL_F,
                             SCH_DISCWDIS_ISS_IDEA_BL_M, SCH_DISCWDIS_ISS_IDEA_BL_F,
                             SCH_DISCWDIS_SINGOOS_IDEA_BL_M, SCH_DISCWDIS_SINGOOS_IDEA_BL_F,
                             SCH_DISCWDIS_MULTOOS_IDEA_BL_M, SCH_DISCWDIS_MULTOOS_IDEA_BL_F, 
                             na.rm=TRUE)) %>%
  mutate(suspend_white_prop = ifelse(total_white == 0, NA, 
                                    suspend_white / total_white)) %>%
  mutate(suspend_black_prop = ifelse(total_black == 0, NA, 
                                    suspend_black / total_black)) %>%
  # Cap proportion at 1 (22 schools have Black prop > 1 (max=9) and 4 schools have white prop > 1 (max=1.9))
  mutate(suspend_white_prop = ifelse(suspend_white_prop > 1, 1, suspend_white_prop)) %>%
  mutate(suspend_black_prop = ifelse(suspend_black_prop > 1, 1, suspend_black_prop)) %>%
  # Difference in proportions
  mutate(diff_suspend_bw = suspend_black_prop - suspend_white_prop) %>%
  # Avg number of school days missed # days of school / number of Black students with out of school suspension
  mutate(suspend_oos_black = sum(SCH_DISCWODIS_SINGOOS_BL_M, SCH_DISCWODIS_SINGOOS_BL_F,
                                 SCH_DISCWODIS_MULTOOS_BL_M, SCH_DISCWODIS_MULTOOS_BL_F,
                                 SCH_DISCWDIS_SINGOOS_IDEA_BL_M, SCH_DISCWDIS_SINGOOS_IDEA_BL_F,
                                 SCH_DISCWDIS_MULTOOS_IDEA_BL_M, SCH_DISCWDIS_MULTOOS_IDEA_BL_F, na.rm=TRUE)) %>% 
  mutate(suspend_days_black = SCH_DAYSMISSED_BL_M + SCH_DAYSMISSED_BL_F) %>% 
  mutate(suspend_avg_days_black = ifelse(suspend_oos_black == 0, 0, 
                                         suspend_days_black / suspend_oos_black)) %>% 
  # Avg number of school days missed # days of school / number of white students with out of school suspension
  mutate(suspend_oos_white = sum(SCH_DISCWODIS_SINGOOS_WH_M, SCH_DISCWODIS_SINGOOS_WH_F,
                                 SCH_DISCWODIS_MULTOOS_WH_M, SCH_DISCWODIS_MULTOOS_WH_F,
                                 SCH_DISCWDIS_SINGOOS_IDEA_WH_M, SCH_DISCWDIS_SINGOOS_IDEA_WH_F,
                                 SCH_DISCWDIS_MULTOOS_IDEA_WH_M, SCH_DISCWDIS_MULTOOS_IDEA_WH_F, na.rm=TRUE)) %>% 
  mutate(suspend_days_white = SCH_DAYSMISSED_WH_M + SCH_DAYSMISSED_WH_F) %>% 
  mutate(suspend_avg_days_white = ifelse(suspend_oos_white == 0, 0, 
                                         suspend_days_white / suspend_oos_white)) %>% 
  mutate(diff_suspenddays_bw = suspend_avg_days_black - suspend_avg_days_white) %>% 
  # Law enforcement and security
  mutate(tot_security = sum(SCH_FTESECURITY_LEO,  SCH_FTESECURITY_GUA, na.rm=TRUE)) %>%
  # Ratio of law enforcement per 100 students
  mutate(security_perstudent = (tot_security / total_enroll)*100) %>%
  # AP classes
  mutate(ap_white_prop = ifelse(SCH_APENR_IND == 0, NA, # If no AP program then don't calculate
                                (SCH_APENR_WH_M + SCH_APENR_WH_F)/ total_white)) %>%
  mutate(ap_black_prop = ifelse(SCH_APENR_IND == 0, NA, # If no AP program then don't calculate
                                (SCH_APENR_BL_M + SCH_APENR_BL_F)/ total_black)) %>%
  mutate(diff_ap_bw = ap_black_prop - ap_white_prop) %>%
  # Advanced mathematics
  #Indicator for offer advanced math
  mutate(advmath_ind = ifelse(SCH_MATHCLASSES_ADVM > 0, 1, 0)) %>%
  mutate(advmath_white_prop = ifelse(advmath_ind == 0, NA, 
                                     (SCH_MATHENR_ADVM_WH_M + SCH_MATHENR_ADVM_WH_F)/ total_white)) %>%
  mutate(advmath_black_prop = ifelse(advmath_ind == 0, NA,
                                     (SCH_MATHENR_ADVM_BL_M + SCH_MATHENR_ADVM_BL_F) / total_black)) %>%
  mutate(diff_advmath_bw = advmath_black_prop - advmath_white_prop) %>%
  # SAT/ACT participation
  mutate(satact_white_prop = (SCH_SATACT_WH_M + SCH_SATACT_WH_F)/total_white) %>%
  mutate(satact_black_prop = (SCH_SATACT_BL_M + SCH_SATACT_BL_F)/total_black) %>%
  # Correct proportions > 1
  mutate(satact_white_prop = ifelse(satact_white_prop > 1, 1, satact_white_prop)) %>%
  mutate(satact_black_prop = ifelse(satact_black_prop > 1, 1, satact_black_prop)) %>%
  mutate(diff_satact_bw = satact_black_prop - satact_white_prop) %>%
  # Counselors per 100 students
  mutate(counselor_perstudent = (SCH_FTECOUNSELORS/total_enroll)*100) %>%
  # Nurses per 100 students
  mutate(nurses_perstudent = (SCH_FTESERVICES_NUR/total_enroll)*100) %>%
  # Psychologists per student 
  mutate(psych_perstudent = (SCH_FTESERVICES_PSY/total_enroll)*100) %>%
  # Social workers per student
  mutate(social_perstudent = (SCH_FTESERVICES_SOC/total_enroll)*100) %>%
  # Total 'positive' support 
  mutate(support_perstudent = ((sum(SCH_FTECOUNSELORS,  SCH_FTESERVICES_NUR, SCH_FTESERVICES_PSY,
                                    SCH_FTESERVICES_SOC, na.rm=TRUE))/total_enroll)*100) %>%
  # Ratio of security to nurses/counselors 
  mutate(security_nursecounselor_ratio = tot_security/sum(SCH_FTESERVICES_NUR,SCH_FTECOUNSELORS, na.rm=TRUE)) %>%
  # Difference in security vs nurses/counselors per 100 students (ratio challenge with 0 security)
  mutate(security_nursecounselor_diff = security_perstudent - (sum(counselor_perstudent, nurses_perstudent, na.rm=TRUE))) %>%
  # Number of sports teams per 100 students
  mutate(sports_perstudent = (TOT_SSTEAMS / total_enroll)*100) %>%
  # What proportion of punishments are suspensions?
  mutate(sus_prop_white = (suspend_white / punish_white)*100) %>%
  mutate(sus_prop_black = (suspend_black / punish_black)*100) %>%
  # Passing algebra in 9/10th grade
  mutate(pass_algebra_w = (SCH_ALGPASS_GS0910_WH_M + SCH_ALGPASS_GS0910_WH_F) / (SCH_ALGENR_GS0910_WH_M + SCH_ALGENR_GS0910_WH_F)) %>%
  mutate(pass_algebra_b = (SCH_ALGPASS_GS0910_BL_M + SCH_ALGPASS_GS0910_BL_F) / (SCH_ALGENR_GS0910_BL_M + SCH_ALGENR_GS0910_BL_F)) %>%
  mutate(diff_algebra = pass_algebra_b - pass_algebra_w) %>%
  # # Enrollment in algebra 1 in 11/12 grade
  # mutate(enroll_algebra_w = (SCH_ALGENR_GS1112_WH_M + SCH_ALGENR_GS1112_WH_F) / total_white) %>%
  # mutate(enroll_algebra_b = (SCH_ALGENR_GS1112_BL_M + SCH_ALGENR_GS1112_BL_F) / total_black) %>%
  # mutate(diff_algebra_enroll = enroll_algebra_b - enroll_algebra_w) %>%
  # Enrollment in algebra in high school
  mutate(enroll_algebra_w = (SCH_ALGENR_GS1112_WH_M + SCH_ALGENR_GS1112_WH_F + SCH_ALGENR_GS0910_WH_M + SCH_ALGENR_GS0910_WH_F) / total_white) %>%
  mutate(enroll_algebra_b = (SCH_ALGENR_GS1112_BL_M + SCH_ALGENR_GS1112_BL_F + SCH_ALGENR_GS0910_BL_M + SCH_ALGENR_GS0910_BL_F) / total_black) %>%
  mutate(diff_algebra_enroll = enroll_algebra_b - enroll_algebra_w) %>% 
  # Racial disparity in dual enrollment
  mutate(dual_white_prop = ifelse(SCH_DUAL_IND == 'No', NA, 
                                     (SCH_DUALENR_WH_M + SCH_DUALENR_WH_F)/ total_white)) %>%
  mutate(dual_black_prop = ifelse(SCH_DUAL_IND == 'No', NA,
                                     (SCH_DUALENR_BL_M + SCH_DUALENR_BL_F) / total_black)) %>%
  mutate(diff_dual_bw = dual_black_prop - dual_white_prop)
  

  

### Merge with piinc data
# Bring in piinc sample
piinc_schools <- read_excel("C:/Users/afw5823/Documents/PIINC/Piinc_geo_0722/Piinc_Schools_072022.xlsx")

piinc_schools2 <- piinc_schools %>%
  select(c("Cradle_ID", "NCESSCH"))

count(piinc_schools2, is.na(NCESSCH))

# Merge piinc participants with school data
piinc_schools3 <- merge(piinc_schools2, schools, by.x="NCESSCH", by.y="COMBOKEY") %>%
  filter(LEA_STATE == "SC")


CrossTable(piinc_schools3$SCH_APENR_IND, piinc_schools3$advmath_ind)
# Numbers of schools excluded for no Black and/or White students
CrossTable(piinc_schools3$bw_indicator, piinc_schools3$LEA_STATE)

# How many schools 
school_count <- piinc_schools3 %>%
  group_by(NCESSCH) %>%
  summarise(count=n()) 

# How many schools by bw_indicator 
school_count2 <- piinc_schools3 %>%
  group_by(NCESSCH, black_plurality) %>%
  summarise(count=n()) 
table(school_count2$black_plurality)

# Distribution 

ggplot(data=piinc_schools3, aes(x=NCESSCH)) + 
  geom_bar(aes(y = (..count..))) +
  ylab("Number of piinc participants") +
  xlab("School ID") +
  theme_bw()

# Distribution of proportion black and white students
ggplot(piinc_schools3, aes(x=LEA_STATE, y=diff_prop_bw)) +
  geom_boxplot() +
  stat_summary(aes(label=round(..y..,2)), fun.y=median, geom="text", size=4, vjust=-0.5) +
  ylab("Diff. in proportion of Black vs. White students") +
  xlab("") +
  theme_bw()


ggplot(piinc_schools3, aes(x=prop_white)) +
  geom_histogram(binwidth=0.05) +
  xlab("Proportion of White students") +
  xlim(0,1) +
  theme_bw()

ggplot(piinc_schools3, aes(x=prop_black)) +
  geom_histogram(binwidth=0.05) +
  xlab("Proportion of Black students") +
  xlim(0,1) +
  theme_bw()

# All schools have both Black and White students
summary(piinc_schools3$prop_black)
summary(piinc_schools3$prop_white)


# School punishment
ggplot(piinc_schools3, aes(x=LEA_STATE, y=punish_white_prop)) +
  geom_boxplot() +
  # stat_summary(fun.y=median, shape=18, geom="point", size=4) +
  stat_summary(aes(label=round(..y..,2)), fun.y=median, geom="text", size=4, vjust=-0.5) +
  ylab("Proportion of White students punished") +
  xlab("") +
  ylim(0,1) +
  theme_bw()


ggplot(piinc_schools3, aes(x=LEA_STATE, y=punish_black_prop)) +
  geom_boxplot() +
  # stat_summary(fun.y=median, shape=18, geom="point", size=4) +
  stat_summary(aes(label=round(..y..,2)), fun.y=median, geom="text", size=4, vjust=-0.5) +
  ylab("Proportion of Black students punished") +
  xlab("") + 
  ylim(0,1) +
  theme_bw()

# All schools have both Black and White students
summary(piinc_schools3$punish_black_prop)
summary(piinc_schools3$punish_white_prop)


ggplot(piinc_schools3, aes(x=LEA_STATE, y=diff_punish_bw)) +
  geom_boxplot() +
  # stat_summary(fun.y=median, shape=18, geom="point", size=4) +
  stat_summary(aes(label=round(..y..,2)), fun.y=median, geom="text", size=4, vjust=-0.5) +
  ylab("Diff. in proportion of Black vs. White punishment") +
  xlab("") +
  ylim(-1,1) +
  theme_bw()

summary(piinc_schools3$diff_punish_bw)


# School Suspension (in and out of school)
ggplot(piinc_schools3, aes(x=LEA_STATE, y=suspend_white_prop)) +
  geom_boxplot() +
  # stat_summary(fun.y=median, shape=18, geom="point", size=4) +
  stat_summary(aes(label=round(..y..,2)), fun.y=median, geom="text", size=4, vjust=-0.5) +
  ylab("Proportion of White students suspended") +
  xlab("") +
  theme_bw()


ggplot(piinc_schools3, aes(x=LEA_STATE, y=suspend_black_prop)) +
  geom_boxplot() +
  # stat_summary(fun.y=median, shape=18, geom="point", size=4) +
  stat_summary(aes(label=round(..y..,2)), fun.y=median, geom="text", size=4, vjust=-0.5) +
  ylab("Proportion of Black students suspended") +
  xlab("") +
  theme_bw()


ggplot(piinc_schools3, aes(x=LEA_STATE, y=diff_suspend_bw)) +
  geom_boxplot() +
  # stat_summary(fun.y=median, shape=18, geom="point", size=4) +
  stat_summary(aes(label=round(..y..,2)), fun.y=median, geom="text", size=4, vjust=-0.5) +
  ylab("Diff. in proportion of Black vs. White suspension") +
  xlab("") +
  theme_bw()

summary(piinc_schools3$diff_suspend_bw)
sum(is.na(piinc_schools3$diff_suspend_bw))
sum(is.na(piinc_schools3$diff_suspend_bw)==FALSE)


# Days suspended
summary(piinc_schools3$suspend_oos_black)
summary(piinc_schools3$suspend_days_black)
summary(piinc_schools3$suspend_avg_days_black)
summary(piinc_schools3$suspend_days_white)
summary(piinc_schools3$suspend_days_black)
summary(piinc_schools3$diff_suspenddays_bw) 


ggplot(piinc_schools3, aes(x=LEA_STATE, y=diff_suspenddays_bw)) +
  geom_boxplot() +
  # stat_summary(fun.y=median, shape=18, geom="point", size=4) +
  stat_summary(aes(label=round(..y..,2)), fun.y=median, geom="text", size=4, vjust=-0.5) +
  ylab("Diff. in avg number of days missed for Black vs. White suspension") +
  xlab("") +
  theme_bw()


# Policing

ggplot(piinc_schools3, aes(x=LEA_STATE, y=security_perstudent)) +
  geom_boxplot() +
  # stat_summary(fun.y=median, shape=18, geom="point", size=4) +
  stat_summary(aes(label=round(..y..,2)), fun.y=median, geom="text", size=4, vjust=-0.5) +
  ylab("Security/police per 100 students") +
  xlab("") +
  theme_bw()

# Distribution of number of guards by state
ggplot(piinc_schools3, aes(x=tot_security)) +
  geom_histogram(binwidth = 1) +
  xlab("Number of security + law enforcement") +
  theme_bw()

# Checking ns of schools with no police/security
check <- subset(piinc_schools3, tot_security == 0)
check2 <- check %>%
  group_by(NCESSCH) %>%
  summarise(count=n())

ggplot(piinc_schools3, aes(x=as.factor(black_plurality), y=security_perstudent)) +
  geom_boxplot() +
  # stat_summary(fun.y=median, shape=18, geom="point", size=4) +
  stat_summary(aes(label=round(..y..,2)), fun.y=median, geom="text", size=4, vjust=-0.5) +
  ylab("Security/police per 100 students") +
  xlab("") +
  ggtitle("South Carolina") +
  theme_bw()



# AP classes
# N's and chisquare 
CrossTable(piinc_schools3$SCH_APENR_IND, piinc_schools3$black_plurality, chisq=TRUE)

# Restricted to schools that offer AP classes 
ggplot(piinc_schools3, aes(x=LEA_STATE, y=ap_white_prop)) +
  geom_boxplot() +
  # stat_summary(fun.y=median, shape=18, geom="point", size=4) +
  stat_summary(aes(label=round(..y..,2)), fun.y=median, geom="text", size=4, vjust=-0.5) +
  ylab("Proportion of White students enrolled in AP") +
  xlab("") +
  theme_bw()


ggplot(piinc_schools3, aes(x=LEA_STATE, y=ap_black_prop)) +
  geom_boxplot() +
  # stat_summary(fun.y=median, shape=18, geom="point", size=4) +
  stat_summary(aes(label=round(..y..,2)), fun.y=median, geom="text", size=4, vjust=-0.5) +
  ylab("Proportion of Black students enrolled in AP") +
  xlab("") +
  theme_bw()


ggplot(piinc_schools3, aes(x=LEA_STATE, y=diff_ap_bw)) +
  geom_boxplot() +
  # stat_summary(fun.y=median, shape=18, geom="point", size=4) +
  stat_summary(aes(label=round(..y..,2)), fun.y=median, geom="text", size=4, vjust=-0.5) +
  ylab("Diff. in proportion of Black vs. White enrolled in AP") +
  xlab("") +
  theme_bw()

sum(is.na(piinc_schools3$diff_ap_bw))



# DUAL enrollment
# N's and chisquare 
CrossTable(piinc_schools3$SCH_DUAL_IND, piinc_schools3$black_plurality, chisq=TRUE)

# Restricted to schools that offer AP classes 
ggplot(piinc_schools3, aes(x=LEA_STATE, y=dual_white_prop)) +
  geom_boxplot() +
  # stat_summary(fun.y=median, shape=18, geom="point", size=4) +
  stat_summary(aes(label=round(..y..,2)), fun.y=median, geom="text", size=4, vjust=-0.5) +
  ylab("Proportion of White students enrolled DE program") +
  xlab("") +
  theme_bw()


ggplot(piinc_schools3, aes(x=LEA_STATE, y=dual_black_prop)) +
  geom_boxplot() +
  # stat_summary(fun.y=median, shape=18, geom="point", size=4) +
  stat_summary(aes(label=round(..y..,2)), fun.y=median, geom="text", size=4, vjust=-0.5) +
  ylab("Proportion of Black students enrolled DE program") +
  xlab("") +
  theme_bw()


ggplot(piinc_schools3, aes(x=LEA_STATE, y=diff_dual_bw)) +
  geom_boxplot() +
  # stat_summary(fun.y=median, shape=18, geom="point", size=4) +
  stat_summary(aes(label=round(..y..,2)), fun.y=median, geom="text", size=4, vjust=-0.5) +
  ylab("Diff. in proportion of Black vs. White enrolled in DE program") +
  xlab("") +
  theme_bw()

sum(is.na(piinc_schools3$diff_dual_bw))

summary(piinc_schools3$diff_dual_bw)
summary(piinc_schools3$diff_ap_bw)

summary(piinc_schools3$SCH_ALGPASS_G08_HI_M)
# Advanced math
# N's and chisquare 
CrossTable(piinc_schools3$advmath_ind, piinc_schools3$black_plurality, chisq=TRUE)

ggplot(piinc_schools3, aes(x=LEA_STATE, y=advmath_white_prop)) +
  geom_boxplot() +
  # stat_summary(fun.y=median, shape=18, geom="point", size=4) +
  stat_summary(aes(label=round(..y..,2)), fun.y=median, geom="text", size=4, vjust=-0.5) +
  ylab("Proportion of White students enrolled in advanced math") +
  xlab("") +
  theme_bw()


ggplot(piinc_schools3, aes(x=LEA_STATE, y=advmath_black_prop)) +
  geom_boxplot() +
  # stat_summary(fun.y=median, shape=18, geom="point", size=4) +
  stat_summary(aes(label=round(..y..,2)), fun.y=median, geom="text", size=4, vjust=-0.5) +
  ylab("Proportion of Black students enrolled in advanced math") +
  xlab("") +
  theme_bw()


ggplot(piinc_schools3, aes(x=LEA_STATE, y=diff_advmath_bw)) +
  geom_boxplot() +
  # stat_summary(fun.y=median, shape=18, geom="point", size=4) +
  stat_summary(aes(label=round(..y..,2)), fun.y=median, geom="text", size=4, vjust=-0.5) +
  ylab("Diff. in proportion of Black vs. White enrolled in advanced math") +
  xlab("") +
  theme_bw()

sum(is.na(piinc_schools3$diff_advmath_bw))

# Comparing ap offering vs adv math
CrossTable(piinc_schools3$SCH_APENR_IND, piinc_schools3$advmath_ind, chisq=TRUE)
CrossTable(piinc_schools3$SCH_APENR_IND, piinc_schools3$SCH_DUAL_IND, chisq=TRUE)


# SAT/ACT participation
ggplot(piinc_schools3, aes(x=LEA_STATE, y=satact_white_prop)) +
  geom_boxplot() +
  # stat_summary(fun.y=median, shape=18, geom="point", size=4) +
  stat_summary(aes(label=round(..y..,2)), fun.y=median, geom="text", size=4, vjust=-0.5) +
  ylab("Proportion of White students who took SAT/ACT") +
  xlab("") +
  theme_bw()


ggplot(piinc_schools3, aes(x=LEA_STATE, y=satact_black_prop)) +
  geom_boxplot() +
  # stat_summary(fun.y=median, shape=18, geom="point", size=4) +
  stat_summary(aes(label=round(..y..,2)), fun.y=median, geom="text", size=4, vjust=-0.5) +
  ylab("Proportion of Black students who took SAT/ACT") +
  xlab("") +
  theme_bw()


ggplot(piinc_schools3, aes(x=LEA_STATE, y=diff_satact_bw)) +
  geom_boxplot() +
  # stat_summary(fun.y=median, shape=18, geom="point", size=4) +
  stat_summary(aes(label=round(..y..,2)), fun.y=median, geom="text", size=4, vjust=-0.5) +
  ylab("Diff. in proportion of Black vs. White who took SAT/ACT") +
  xlab("") +
  #ylim(-1,1) +
  theme_bw()





# algebra
# N's and chisquare 

ggplot(piinc_schools3, aes(x=LEA_STATE, y=pass_algebra_w)) +
  geom_boxplot() +
  # stat_summary(fun.y=median, shape=18, geom="point", size=4) +
  stat_summary(aes(label=round(..y..,2)), fun.y=median, geom="text", size=4, vjust=-0.5) +
  ylab("Proportion of White students who passed algebra in 9/10th grade") +
  xlab("") +
  theme_bw()


ggplot(piinc_schools3, aes(x=LEA_STATE, y=pass_algebra_b)) +
  geom_boxplot() +
  # stat_summary(fun.y=median, shape=18, geom="point", size=4) +
  stat_summary(aes(label=round(..y..,2)), fun.y=median, geom="text", size=4, vjust=-0.5) +
  ylab("Proportion of Black students who passed algebra in 9/10th grade") +
  xlab("") +
  theme_bw()


ggplot(piinc_schools3, aes(x=LEA_STATE, y=diff_algebra)) +
  geom_boxplot() +
  # stat_summary(fun.y=median, shape=18, geom="point", size=4) +
  stat_summary(aes(label=round(..y..,2)), fun.y=median, geom="text", size=4, vjust=-0.5) +
  ylab("Diff. in proportion of Black vs. White students passing algebra") +
  xlab("") +
  theme_bw()


sum(is.na(piinc_schools3$diff_algebra))

summary(piinc_schools3$diff_algebra)

# Enrolled in algebra in highschool (behind, typically 8th grade)
# N's and chisquare 

ggplot(piinc_schools3, aes(x=LEA_STATE, y=enroll_algebra_w)) +
  geom_boxplot() +
  # stat_summary(fun.y=median, shape=18, geom="point", size=4) +
  stat_summary(aes(label=round(..y..,2)), fun.y=median, geom="text", size=4, vjust=-0.5) +
  ylab("Proportion of White students enrolled in algebra in HS") +
  xlab("") +
  ylim(0,1) +
  theme_bw()


ggplot(piinc_schools3, aes(x=LEA_STATE, y=enroll_algebra_b)) +
  geom_boxplot() +
  # stat_summary(fun.y=median, shape=18, geom="point", size=4) +
  stat_summary(aes(label=round(..y..,2)), fun.y=median, geom="text", size=4, vjust=-0.5) +
  ylab("Proportion of Black students who enrolled in algebra in HS") +
  xlab("") +
  ylim(0,1) +
  theme_bw()


ggplot(piinc_schools3, aes(x=LEA_STATE, y=diff_algebra_enroll)) +
  geom_boxplot() +
  # stat_summary(fun.y=median, shape=18, geom="point", size=4) +
  stat_summary(aes(label=round(..y..,2)), fun.y=median, geom="text", size=4, vjust=-0.5) +
  ylab("Diff. in proportion of Black vs. White students taking algebra in HS") +
  xlab("") +
  theme_bw()


summary(piinc_schools3$enroll_algebra_b)


# Extra support - total support
ggplot(piinc_schools3, aes(x=LEA_STATE, y=support_perstudent)) +
  geom_boxplot() +
  # stat_summary(fun.y=median, shape=18, geom="point", size=4) +
  stat_summary(aes(label=round(..y..,2)), fun.y=median, geom="text", size=4, vjust=-0.5) +
  ylab("Extra support per 100 students") +
  xlab("") +
  theme_bw()


# Looking at distributions for schools with more Black vs. white students
# SC
ggplot(piinc_schools3, aes(x=as.factor(black_plurality), y=support_perstudent)) +
  geom_boxplot() +
  # stat_summary(fun.y=median, shape=18, geom="point", size=4) +
  stat_summary(aes(label=round(..y..,2)), fun.y=median, geom="text", size=4, vjust=-0.5) +
  ylab("Extra support per 100 students") +
  xlab("") +
  ggtitle("South Carolina") +
  theme_bw()

# Nurses
ggplot(piinc_schools3, aes(x=LEA_STATE, y=nurses_perstudent)) +
  geom_boxplot() +
  # stat_summary(fun.y=median, shape=18, geom="point", size=4) +
  stat_summary(aes(label=round(..y..,2)), fun.y=median, geom="text", size=4, vjust=-0.5) +
  ylab("Nurses per 100 students") +
  xlab("") +
  theme_bw()

ggplot(piinc_schools3, aes(x=as.factor(black_plurality), y=nurses_perstudent)) +
  geom_boxplot() +
  # stat_summary(fun.y=median, shape=18, geom="point", size=4) +
  stat_summary(aes(label=round(..y..,2)), fun.y=median, geom="text", size=4, vjust=-0.5) +
  ylab("Nurses per 100 students") +
  xlab("") +
  ggtitle("South Carolina") +
  theme_bw()


# Ratio of security to nurses/counselors

# Extra support - total support
ggplot(piinc_schools3, aes(x=LEA_STATE, y=security_nursecounselor_ratio)) +
  geom_boxplot() +
  # stat_summary(fun.y=median, shape=18, geom="point", size=4) +
  stat_summary(aes(label=round(..y..,2)), fun.y=median, geom="text", size=4, vjust=-0.5) +
  ylab("Ratio of security to nurses/counselors") +
  xlab("") +
  theme_bw()


# Looking at distributions for schools with more Black vs. white students
# SC
ggplot(piinc_schools3, aes(x=as.factor(black_plurality), y=security_nursecounselor_ratio)) +
  geom_boxplot() +
  # stat_summary(fun.y=median, shape=18, geom="point", size=4) +
  stat_summary(aes(label=round(..y..,2)), fun.y=median, geom="text", size=4, vjust=-0.5) +
  ylab("Ratio of security to nurses/counselors") +
  xlab("") +
  ggtitle("South Carolina") +
  theme_bw()



# Difference of security to nurses/counselors

ggplot(piinc_schools3, aes(x=LEA_STATE, y=security_nursecounselor_diff)) +
  geom_boxplot() +
  # stat_summary(fun.y=median, shape=18, geom="point", size=4) +
  stat_summary(aes(label=round(..y..,2)), fun.y=median, geom="text", size=4, vjust=-0.5) +
  ylab("Difference in security to nurses/counselors") +
  xlab("") +
  theme_bw()


# Looking at distributions for schools with more Black vs. white students
# SC
ggplot(filter(piinc_schools3, LEA_STATE=="SC"), aes(x=as.factor(black_plurality), y=security_nursecounselor_diff)) +
  geom_boxplot() +
  # stat_summary(fun.y=median, shape=18, geom="point", size=4) +
  stat_summary(aes(label=round(..y..,2)), fun.y=median, geom="text", size=4, vjust=-0.5) +
  ylab("Difference in security to nurses/counselors") +
  xlab("") +
  ggtitle("South Carolina") +
  theme_bw()





##### Export dataset #####
pinc_school_export<- subset(piinc_schools3, select=c("Cradle_ID", "NCESSCH", "prop_white", "prop_black",
                                                "diff_punish_bw", "diff_algebra_enroll",
                                                "SCH_APENR_IND", "diff_ap_bw")) %>%
  rename(record_id = Cradle_ID)
library(openxlsx)
write.xlsx(pinc_school_export, file="C:/Users/afw5823/Documents/School_data/piinc_School_export_110922.xlsx")



# Variable heatmap
# Based on: http://www.sthda.com/english/wiki/ggplot2-quick-correlation-matrix-heatmap-r-software-and-data-visualization

# heatmap_data <- subset(schools, select=c("LEA_STATE", "prop_white", "prop_black",
#                               "diff_suspend_bw", "diff_punish_bw", "security_perstudent",
#                               "diff_ap_bw", "diff_advmath_bw", "diff_satact_bw",
#                               "support_perstudent", "sports_perstudent"))

# heatmap_data <- subset(piinc_schools3, select=c("LEA_STATE", "prop_white", "prop_black",
#                                          "diff_punish_bw", "diff_suspenddays_bw", "diff_advmath_bw", "diff_algebra_enroll", 
#                                          "diff_satact_bw", "security_nursecounselor_diff"))

heatmap_data <- subset(piinc_schools3, select=c("LEA_STATE", "prop_white", 
                                                "diff_punish_bw", "diff_algebra_enroll",
                                                "diff_dual_bw", "diff_ap_bw"))


# Functions for restricting to lower/upper portion of matrix
# Get lower triangle of the correlation matrix
get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}
# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}
# Order plot based on correlations/distance to group like genes
reorder_cormat <- function(cormat){
  # Use correlation between variables as distance
  dd <- as.dist((1-cormat)/2)
  hc <- hclust(dd)
  cormat <-cormat[hc$order, hc$order]
}


# Overall
# Correlation matrix - use transposed (not long) dataset
corr_mat1 <- cor(na.omit(subset(heatmap_data, select=-c(LEA_STATE, prop_white)))) # NA.omit removes any rows with NAs

# Reorder based on similarity (function above)
#corr_mat1 <- reorder_cormat(corr_mat1)

# Keep lower triangle since symmetric 
corr_mat_lower1 <- get_lower_tri(corr_mat1)

# Melt the correlation matrix
melted_corr_mat1 <- melt(corr_mat_lower1, na.rm=TRUE) # remove NAs

# Plot the correlation matrix heat map
heatmap_corrmat <- ggplot(melted_corr_mat1, aes(x=Var1, y=Var2, fill=value)) +
  geom_tile(color="white") +
  theme_minimal()+
  scale_fill_gradient2(low="blue", high="red", mid="white", midpoint=0, limit=c(-1,1), space="Lab", name="Pearson") +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, size = 8, hjust = 1), axis.text.y = element_text(size=8)) +
  xlab("") +
  ylab("") +
  coord_fixed() +
  theme(panel.background=element_blank(), 
        panel.grid.major = element_blank()) +
  geom_text(aes(Var1, Var2, label = round(value, 2)), color = "black", size = 4) 
heatmap_corrmat


ggplot(data=piinc_schools3, aes(x=diff_ap_bw, y=diff_punish_bw)) +
  geom_point() +
  geom_smooth(method="lm", formula = y~x)




# Export shapefile of cleaned school data with XY coord shape file
# Load shapefile
schools_shape <- read_sf("C:/Users/afw5823/Documents/School_data/EDGE_GEOCODE_PUBLICSCH_1718/EDGE_GEOCODE_PUBLICSCH_1718.shp")

# Schools restricted to hs -- may want to restrict to dataset at end of cleaning
schools_shape_res_hs <- filter(schools_shape, schools_shape$NCESSCH %in% chars_res_hs$COMBOKEY)

# Export 
st_write(schools_shape_res_hs, "C:/Users/afw5823/Documents/School_data/HIGHSCHOOLS_cleanvars_1718.shp")
# Bring this file into arcgis to identify schools in each census tract


# Also want to look at residence-based school boundaries
# Load shapefile (from: https://data.cityofchicago.org/Education/Chicago-Public-Schools-High-School-Attendance-Boun/y9da-bb2y#revert)
chicago_shape <- read_sf("C:/Users/afw5823/Documents/School_data/Chicago school boundaries_1718/geo_export_943db742-3af6-4847-aa00-4652ce5fcd74.shp")

chicago_names <- as.data.frame(chicago_shape$school_nm) %>% 
  # Remove duplicates (dups because distinct shapes for attendance boundaries/non-contiguous)
  rename(school_nm = `chicago_shape$school_nm`) %>% 
  group_by(school_nm) %>% 
  slice(1) # Keep first row of duplicates

#48 location-based schools in CPS
  
