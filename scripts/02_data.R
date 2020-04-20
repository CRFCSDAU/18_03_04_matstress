
  library(tidyverse)
  library(summarytools)
  library(viridis)
  library(foreign)

# 1.0 Load data ----------------------------------------------------------------
# load("GUIdata.RData")

  raw_data <-  read.spss("data/GUI_3_infant_waves.sav",
                         to.data.frame = TRUE, stringsAsFactors = FALSE)
  data <- select(
    raw_data,
    Babymeaskgs, Babymeascms, b2kidkgs, b2kidcms, b3kidkgs, b3kidcms,
    b3_kidbmi, totstress, rewards,stressors, lackcontr, parsatis,
    SCGtotstress, SCGrewards, SCGstressors, SCGlackcontr, SCGparsatis,
    bpc2_stress, bpc3_stress, bsc2_stress,	bsc3_stress, MMagep1, MMagep2,
    MMG7kilos,MMH5a, MMH6gms, MMG12, MS12, FS12, MMD3b, MMD3a, QoAscore,
    QoAscg, dyadic_PCG, dyadic_SCG, Equivinc, EIncQuin, MML34, FF13,
    intPCGBMI, intPCGBMI_rec, intSCGBMI,intSCGBMI_rec, MMH13, MMH15b, MMH18,
    MMH19, p1yearsW2,	p1yearsw3, p2yearsW2, p3yearsw3, bpc2S12, bpc3S12,
    bsc2S12,	bsc3S12, bpc2b3, bpc3b2, b2_Equivinc, b2_EIncQuin, b3_EIncQuin,
    bpc2K1,	bpc3K1a_ed, bsc2E1,	bsc3K1a_ed,bpc2BMI, bpc2BMI_CAT,	bpc3bmi,
    bpc3bmi_cat, bsc2BMI, bsc2BMI_CAT,bsc3bmi, bsc3bmi_cat, bpc2C23, MME1,
    MME2a_e, MME2a_c, MME2b_c, MME2b_e, MME2c_c, MME2c_e, MME2d_c, MME2d_e,
    MME2e_c, MME2e_e, MME2f_c, MME2f_e, MME6, MMa5ap2, MMa5rcp1, MMa5ap1,
    MMa5rmp3, MMa5rcp3
  )

  names(data) <- tolower(names(data))

# Wave 1 - 9th
# Wave 2 - 3 years
# Wave 3 - 5 years

# 2.0 Look at data -------------------------------------------------------------
# view(dfSummary(data))

# 3.0 Clean variables ----------------------------------------------------------

# There are a lot of numeric variables that come in as factors due to text in
# the field (e.g "greater than 20"). This will tag the variables that are
# factors with more than 10 levels, and remove the alpha and convert to numeric.
# On the first run, this caused some problems for a few variables that were
# actually supposed to be factors, so tag2 corrects that.

# missing1 <- map(data, function(x) table(is.na(x))["TRUE"])

  tag <- sapply(data, function(x) is.factor(x) & nrow(table(x)) > 10)
  tag2 <- tolower(
    c("MML34", "FF13", "bpc2K1", "bpc3K1a_ed", "bsc2E1", "bsc3K1a_ed")
  )
  tag[tag2] <- FALSE
  data[tag] <- map(
    data[tag], function(x) as.numeric(gsub("[[:alpha:]]", "", x))
  )

# missing2 <- map(data, function(x) table(is.na(x))["TRUE"])

# View(data_frame(missing1 = missing1, missing2 = missing2))
# length(missing1 %in% missing2)

# 4.0 New variables ------------------------------------------------------------

# 4.1 BMI at 3 waves ----
# Calculating BMI for the 3 waves in 3 steps and adding new variables to the
# dataset @ each stage. First step is to square each of the length measurements
# at each wave Including wave 3 here as a type of check on the calculations as
# already have this variable
  data <- mutate(
    data,
    w1_cms_sq = babymeascms * babymeascms,
    w2_cms_sq = b2kidcms * b2kidcms,
    w3_cms_sq = b3kidcms * b3kidcms
  )

# Second step is to divide child weight at each wave by the squared length in
# cms
  data <- mutate(
    data,
    w1_kg_by_cmsq = babymeaskgs / w1_cms_sq,
    w2_kg_by_cmsq = b2kidkgs / w2_cms_sq,
    w3_kg_by_cmsq = b3kidkgs / w3_cms_sq
  )

# Third step is to multiply the weight divided by squared length by 10,000
# because length was measured in cms
  data <- mutate(
    data,
    w1_child_bmi = w1_kg_by_cmsq * 10000,
    w2_child_bmi = w2_kg_by_cmsq * 10000,
    w3_child_bmi = w2_kg_by_cmsq * 10000)

# One out of range bmi for wave 1.
# filter(data, w1_child_bmi > 50) %>%
#   select(babymeascms, babymeaskgs, w1_child_bmi) %>%
#   View()
# The weight and length were switched.
  tag <- data$w1_child_bmi > 5000 & !is.na(data$w1_child_bmi)
  x <- data$babymeascms[tag]
  data$babymeascms[tag] <- data$babymeaskgs[tag]
  data$babymeaskgs[tag] <- x; rm(x)
  data$w1_child_bmi[tag] <- data$babymeaskgs[tag] /
    (data$babymeascms[tag]^2) * 10000

# Look at the BMI values
# select(data, contains("child_bmi")) %>%
#   gather(wave, bmi) %>%
#   ggplot(aes(x = bmi, fill = wave, color = wave)) +
#     geom_density(alpha = 0.5) +
#     scale_fill_viridis(discrete = TRUE, end = 0.8) +
#     scale_color_viridis(discrete = TRUE, end = 0.8) +
#     geom_rug()
# There are lots of still out of range BMIs                                 FLAG

# 4.2 Low birth weight ----
  data$lbw <- factor(data$mmh6gms <= 2500, labels = c("No", "Yes"))
# ggplot(data, aes(x = mmh6gms)) +
#   geom_histogram() +
#   facet_wrap(~lbw, ncol = 1)

# 4.3 Sample ----
# Identify families with the female parent primary respondent and their partner
# as second respondent

  data <- mutate(
    data,
    sample_flag = case_when(
      mma5rcp1 == "Parent" &
        mma5ap1 == "female" &
        mma5rmp3 %in% c("Partner", "Husband/wife") &
        mma5rcp3 == "Parent" ~ "Yes"
    )
  )

# How many moms without parent partners?
# nrow(filter(data, mma5rcp1 == "Parent" & mma5ap1 == "female")) -
#   table(data$sample_flag)
#

# How can we 1) model parent partner stress without dropping single moms?   FLAG

# 4.4 Childcare ----
# Childcare type
# How many report different types of childcare?
  tar <- c("mme2a_e", "mme2b_e", "mme2c_e", "mme2d_e", "mme2f_e", "mme2e_e")
  data$mme2a_e[data$mme2a_e == "No"] <- NA
  data$mme2b_e[data$mme2b_e == "No"] <- NA
  data$mme2c_e[data$mme2c_e == "No"] <- NA
  data$mme2d_e[data$mme2d_e == "No"] <- NA
  data$mme2f_e[data$mme2e_e == "No"] <- NA
  data$mme2e_e[data$mme2f_e == "No"] <- NA
  data[tar] <- map(data[tar], function(x) as.numeric(factor(x)))
  data$all_types_childcare <- rowSums(data[tar], na.rm = TRUE)
# table(data$all_types_childcare)  There are a few (<200) with > 1 type

  data <- mutate(
    data,
    type_childcare = case_when(
      mme2a_e == 1 ~ "Relative in own home",
      mme2b_e == 1 ~ "Non-relative in own home",
      mme2c_e == 1 ~ "Relative in their home",
      mme2d_e == 1 ~ "Non-relative in their home",
      mme2f_e == 1 ~ "Centre based care",
      mme2e_e == 1 ~ "Other"
    )
  )
  data$type_childcare[is.na(data$type_childcare)] <- "No"
  data$type_childcare <- factor(data$type_childcare)
# table(data$mme1, data$type_childcare)
# Note: a few people with > 1 type of care will only have one reflected here.

# Total Childcare hours
  data <- mutate(data, mme2f_c2 = case_when(
    mme2f_c == levels(factor(data$mme2f_c))[1] ~ 5,
    mme2f_c == levels(factor(data$mme2f_c))[2] ~ 15,
    mme2f_c == levels(factor(data$mme2f_c))[3] ~ 25,
    mme2f_c == levels(factor(data$mme2f_c))[4] ~ 35,
    mme2f_c == levels(factor(data$mme2f_c))[5] ~ 45,
  ))

  tar <- c("mme2a_c", "mme2b_c", "mme2c_c", "mme2d_c", "mme2f_c2", "mme2e_c")
  data$total_hours_childcare <- rowSums(data[tar], na.rm = TRUE)
  hist(data$total_hours_childcare)

# ggplot(data, aes(x = total_hours_childcare, fill = type_childcare)) +
#   geom_histogram() +
#   facet_wrap(~type_childcare, ncol = 1, scales = "free_y")

# How to model hours but make zero distinct from no childcare               FLAG

# 4.5 Stress ----

# Primary
  data$totstress
  data$rewards
  data$stressors
  data$lackcontr
  data$parsatis
# Secondary
  data$scgtotstress
  data$scgrewards
  data$scgstressors
  data$scglackcontr
  data$scgparsatis

# 5.0 Progress to this point ---------------------------------------------------
# save(data, file = "data.RData")
  load("data.RData")

# 6.0 To do --------------------------------------------------------------------

# BMI centiles/scores
# Keep people not at both timepoints?

