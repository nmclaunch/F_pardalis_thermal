# F_pardalis_thermal
Data and code for thermal limits, performance, preference and species distribution modeling of Furcifer pardalis. 

The files associated with this dataset include:

Assessing Thermal Traits
  Data files:
thermal_limit_summary_merged.csv
thermal_pref_summary_merged.csv
breadt_summary_merged.csv
##Colin add performance summary data file###

  Code files:
cham_thermal_limits_preference.R
##Add Colin's code to the thermal_limit_preference to keep all in one place

Species Distribution Modeling
  Data files:
  
  
  Code files:
  brick.R
  
  
Metadata
Thermal Traits
thermal_limit_summary_merged.csv
  row: unique row numbers
  sample: individual ID and season 
  date: date as mm/dd/yyyy
  cham_id: individual chameleon identification number
  cham_temp_end: body temperature of chameleon in degrees C at the completion of the respective thermal limit trial
  trial_type: the thermal limit assessed for the respective row. "cold" = critical thermal minimum trial; "hot" = critical thermal maximum trial
  which_first: thermal limit assessed first each season; order by random assignment; "cold" = critical thermal minimum trial; "hot" = critical thermal maximum trial
  season: the season experienced directly before thermal limits were assessed
  avg_cham_temp_rate_per_10s: the average rate of body temperature change per 10 seconds during the trial
  avg_cham_temp_rate_min: the average rate of body temperature change per minute during the trial
  notes: any notes about the animal that may have affected the trial/data
  sex: M= male, F= female
  mass_g: the mass of the animal in grams
  svl_mm: the snout-vent-length of the animal in mm
  
breadt_summary_merged.csv
  variables with same names as above
  tbreadth: the thermal breadth of the individual for the respective season as calculated by subtracting CTmin from CTgape
  
thermal_pref_summary_merged
  variables with same names as above
  avg_prefbody_temp: the average body temperature of the animal during the thermal preference trial
  mode_prefbody_temp: the mode body temperature of the animal during the thermal preference trial
  max_prefbody_temp: the maximum body temperature of the animal during the thermal preference trial
  min_prefbody_temp: the minimum body temperature of the animal during the thermal preference trial
  std_dev_prefbody_temp: the standard deviation of body temperatures of the animal during the thermal preference trial
  notes_pref: any notes that may have affected trial or caused duration of trial to be adjusted
  side_started: the end of the gradient the animal first walked to after starting the trial
  
  cham_thermal_limits_preference.R
    This contains code for analysis and plotting of thermal limits and preference data
    

Species Distribution Modeling
  brick.R
    This contains code for extracting data from PRISM, cropping to extent of FL, and extracting point data for min/max.
 
 
