
  library(growthstandards)


# Customized centiles ---------------------------------------------------------

# Ethnicity score
data <- mutate(data, grow_eth_score = case_when(
  ethnicity == "Indian" ~ -149.4,
  ethnicity == "Nepali" ~ -149.4,
  ethnicity == "Pakistani" ~ -187.3,
  ethnicity == "Bangladeshi" ~ -79.3,
  ethnicity == "Jamaican" ~ -129.9,
  ethnicity == "Chinese" ~ -0.3,
  ethnicity %in% c("African", "Angolan", "Congalese", "Ethiopian", "Ghanaian",
                   "Malawian", "Nigerian", "Somali", "Sudanese", "Kenyan",
                   "Liberian", "South African") ~ -218.5,
  ethnicity %in% c("Filipino", "Phillipino", "Sri Lankan", "Thai",
                   "Fijian") ~  +56.4,
  ethnicity %in% c("Moroccan", "Saudi Arabian",  "Muslim", "Omani") ~ -89.9
)
)

data$grow_eth_score[is.na(data$grow_eth_score) & !is.na(data$ethnicity)] <- 0

# Parity
data <- mutate(data, grow_parity_score = case_when(
  parity == 0 ~ 0,
  parity == 1 ~ 101.9,
  parity == 2 ~ 133.7,
  parity == 3 ~ 140.2,
  parity >  3 ~ 162.7
)
)

# Sex
data <- mutate(data, grow_sex_score = case_when(
  baby_sex_1 == 0 ~ 0,
  baby_sex_1 == 1 ~ 48.9,
  baby_sex_1 == 2 ~ -48.9
)
)
data$grow_sex_score[is.na(data$baby_sex_1)] <- 0

data$baby_sex_1[data$baby_sex_1 == 0] <- NA
data$sex <- factor(data$baby_sex_1, labels = c("Male", "Female"))

# Weight (capped)

data$weight_grow <- data$weight
target <- data$bmi >= 30 & !is.na(data$weight_grow) & !is.na(data$height)
data$weight_grow[target] <-
  (30 * data$height[target]^2)

data$weight_grow <- data$weight_grow - 64

# Score
data$tow <-
  3455.6 +
  ((data$height*100 - 163) * 6.7) +
  data$weight_grow +
  data$grow_eth_score +
  data$grow_parity_score +
  data$grow_sex_score +
  (9.1733 * data$weight_grow) + (-0.151 * data$weight_grow^2) +
  (-0.001* data$weight_grow^3)

# Correct for GA
data$grow_pct_weight <- (298.8 +
                           (-31.85 * data$del_gstweeks) +
                           (1.094 * data$del_gstweeks^2) +
                           (-0.01055 * data$del_gstweeks^3)) / 100

# Calc corrected grow weight

data$grow <- data$tow * data$grow_pct_weight

data$grow_diff <- data$birth_weight_1 - data$grow

# Grow centile

data$tow_z <- (((data$birth_weight_1/data$tow) - 1) * 100) / 11 # z score from TOW
data$tow_centile <- pnorm(data$tow_z)


# z score from GROW - Get the % diff of observed weight from GROW. Divide by 11,
# the CV, to get the z-score. Then get the centile for that z-score.
data$grow_z <- (((data$birth_weight_1/data$grow) - 1) * 100) / 11 #
#  plot(data$grow_z,    pnorm(data$grow_z))
data$grow_centile <- pnorm(data$grow_z) * 100

data <- mutate(data, grow_size_ga = case_when(
  grow_centile <= 10 ~ "SGA",
  grow_centile > 10 & grow_centile < 90 ~ "Normal",
  grow_centile > 90 ~ "LGA"
)
)

data$grow_size_ga <- factor(data$grow_size_ga)



# WHO centiles

tar <- data$sex == "Male" & !is.na(data$sex)
data$igb_wt_centile[tar] <- igb_wtkg2centile(data$del_gstdays[tar],
                                             data$birth_weight_1[tar]/1000,
                                             sex = "Male")
tar <- data$sex == "Female" & !is.na(data$sex)
data$igb_wt_centile[tar] <- igb_wtkg2centile(data$del_gstdays[tar],
                                             data$birth_weight_1[tar]/1000,
                                             sex = "Female")

data <- mutate(data, igb_size_ga = case_when(
  igb_wt_centile <= 10 ~ "SGA",
  igb_wt_centile > 10 & igb_wt_centile < 90 ~ "Normal",
  igb_wt_centile > 90 ~ "LGA"
)
)

data$igb_size_ga <- factor(data$igb_size_ga)
