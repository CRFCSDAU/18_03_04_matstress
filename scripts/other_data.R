#Script for analysis of parental stress effects on child BMI using the GUI data
#Includes setting up the data file for analysis

  install.packages("foreign")
  library("foreign")

  input_data <-  read.spss("~/Desktop/GUI weight/GUI_3_infant_waves.sav",
                           to.data.frame = TRUE, stringsAsFactors = FALSE)

  str(input_data)

  library("tidyverse")

  GUIdata <- select(input_data, "Babymeaskgs", "Babymeascms", "b2kidkgs", "b2kidcms", "b3kidkgs",
                    "b3kidcms", "b3_kidbmi", "totstress", "rewards","stressors", "lackcontr", "parsatis",
                    "SCGtotstress", "SCGrewards", "SCGstressors", "SCGlackcontr", "SCGparsatis", "bpc2_stress",
                    "bpc3_stress", "bsc2_stress",	"bsc3_stress", "MMagep1", "MMagep2", "MMG7kilos","MMH5a", "MMH6gms",
                    "MMG12", "MS12", "FS12", "MMD3b", "MMD3a", "QoAscore", "QoAscg", "dyadic_PCG", "dyadic_SCG", "Equivinc",
                    "EIncQuin", "MML34", "FF13", "intPCGBMI", "intPCGBMI_rec", "intSCGBMI","intSCGBMI_rec", "MMH13",
                    "MMH15b", "MMH18", "MMH19", "p1yearsW2",	"p1yearsw3", "p2yearsW2", "p3yearsw3", "bpc2S12",
                    "bpc3S12", "bsc2S12",	"bsc3S12", "bpc2b3", "bpc3b2", "b2_Equivinc", "b2_EIncQuin", "b3_EIncQuin",
                    "bpc2K1",	"bpc3K1a_ed", "bsc2E1",	"bsc3K1a_ed","bpc2BMI", "bpc2BMI_CAT",	"bpc3bmi",
                    "bpc3bmi_cat", "bsc2BMI", "bsc2BMI_CAT","bsc3bmi", "bsc3bmi_cat", "bpc2C23", "MME1",
                    "MME2a_e", "MME2a_c", "MME2b_c", "MME2b_e", "MME2c_c", "MME2c_e", "MME2d_c", "MME2d_e",
                    "MME2e_c", "MME2e_e", "MME2f_c", "MME2f_e", "MME6", "MMa5ap2")

  str(GUIdata)
#Renaming column names
  GUIdata <- as_tibble(GUIdata)
  GUIdata


# Need to examine individual variables because initial attempts to create new variables causing issues
# Appears that variables that should be numeric are categorical, most likely due to inclusion of
# entries e.g. '6 or more'
# Below is attempt to identify for which variables this is the case and what levels need to be
# amended for relevant variables


# Tables for child weight and length
  table(GUIdata$Babymeascms)
  table(GUIdata$b2kidcms)
  table(GUIdata$b3kidcms)
  table(GUIdata$Babymeaskgs)
  table(GUIdata$b2kidkgs)
  table(GUIdata$b3kidkgs)
  table(GUIdata$b3_kidbmi)


#Changing the '12 or less' and 22 or more' levels in the b3_kidbmi variable so can convert it to numeric
#Do lose some data in this way in the dataset but cannot run tests otherwise
#CHECK THIS BECAUSE CREATES 'ATOMIC VECTOR'- NOT FULLY SURE WHATS GOING ON

  levels(GUIdata$b3_kidbmi) <- c("12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22")
  table(GUIdata$b3_kidbmi)


#Converting b3_kidbmi to a numeric variable instead of a character variable.
#Including as.character to ensure I dont lose actual values in conversion
  as.numeric(as.character(GUIdata$b3_kidbmi))
  as.numeric(GUIdata$b3_kidbmi)
  class(GUIdata$b3_kidbmi)

  str(GUIdata)


#Tables for parental stress
  table(GUIdata$totstress)
  table(GUIdata$rewards)
  table(GUIdata$stressors)
  table(GUIdata$lackcontr)
  table(GUIdata$parsatis)
  table(GUIdata$SCGtotstress)
  table(GUIdata$SCGtotrewards)
  table(GUIdata$SCGstressors)
  table(GUIdata$SCGlackcontr)
  table(GUIdata$bpc2_stress)
  table(GUIdata$bpc3_stress)
  table(GUIdata$bsc2_stress)
  table(GUIdata$bsc3_stress)

  class(GUIdata$MMagep1)


#STILL NEED TO CHECK OTHER VARIABLES

#Calculating BMI for the 3 waves in 3 steps and adding new variables to the dataset @ each stage.
#First step is to square each of the length measurements at each wave
# Including wave 3 here as a type of check on the calculations as already have this variable
  GUIdata <- mutate(GUIdata,
                    W1cmsSq = Babymeascms * Babymeascms,
                    W2cmsSq = b2kidcms * b2kidcms,
                    W3cmsSq = b3kidcms * b3kidcms
  )

#Second step is to divide child weight at each wave by the squared length in Cms
  GUIdata <- mutate(GUIdata,
                    W1KGbyCMSq = Babymeaskgs / W1cmsSq,
                    W2KGbyCMSq = b2kidkgs / W2cmsSq,
                    W3KGbyCMSq = b3kidkgs / W3cmsSq)

#Third step is to multiply the weight divided by squared length by 10,000 because length was measured in cms
  GUIdata <- mutate(GUIdata,
                    W1ChildBMI = W1KGbyCMSq * 10000,
                    W2ChildBMI = W2KGbyCMSq * 10000,
                    W3ChildBMI = W3KGbyCMSq * 10000)

#Create variable to distinguish those infants born low birthweight (<2500g), and those born above this
#MMH6gms is birthweight variable in grams


#Create variable for person1 is childs mother, and person 3 is childs father and is partner/married to mother (to capture dyad)
# MMa5rcp1 is main caregivers relationship to study child
# MMa5ap1 is gender of main caregiver
# MMa5rmp3 is relationship of person 3 to childs mother
# MMa5rcp3 is relationship of person 3 to study child



# Create variable for Main_childcare_type
# MME2a_e is relative in own home as main childcare type
# MME2b_e is non-relative in own home as main childcare type
# MME2c_e is relative in their home as main childcare type
# MME2d_e is non-relative in their home as main childcare type
# MME2e_e is centre-based care as main childcare type
# MME2f_e is other as main childcare type


# Create variable for number of hours in childcare per week
# MME2a_c is hours in relative in own home as main childcare type
# MME2b_c is hours in non-relative in own home as main childcare type
# MME2c_c is hours in relative in their home as main childcare type
# MME2d_c is hours in non-relative in their home as main childcare type
# MME2e_c is hours in centre-based care as main childcare type
# MME2f_c is hours in other as main childcare type


