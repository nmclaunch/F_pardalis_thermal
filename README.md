Data and code for thermal limits, performance, preference and species distribution modeling of Furcifer pardalis.

R scripts are contained in the Rscripts folder. Output from correlative species distribution modeling using Maxent is contained in the Maxent_output folder.

Data files (.csv) are in the main folder.

Metadata for .csv files :
thermal_limit_summary_merged.csv row: unique row numbers sample: individual ID and season date: date as mm/dd/yyyy cham_id: individual chameleon identification number cham_temp_end: body temperature of chameleon in degrees C at the completion of the respective thermal limit trial trial_type: the thermal limit assessed for the respective row. "cold" = critical thermal minimum trial; "hot" = critical thermal maximum trial which_first: thermal limit assessed first each season; order by random assignment; "cold" = critical thermal minimum trial; "hot" = critical thermal maximum trial season: the season experienced directly before thermal limits were assessed avg_cham_temp_rate_per_10s: the average rate of body temperature change per 10 seconds during the trial avg_cham_temp_rate_min: the average rate of body temperature change per minute during the trial notes: any notes about the animal that may have affected the trial/data sex: M= male, F= female mass_g: the mass of the animal in grams svl_mm: the snout-vent-length of the animal in mm

breadt_summary_merged.csv variables with same names as above tbreadth: the thermal breadth of the individual for the respective season as calculated by subtracting CTmin from CTgape

thermal_pref_summary_merged.csv variables with same names as above avg_prefbody_temp: the average body temperature of the animal during the thermal preference trial mode_prefbody_temp: the mode body temperature of the animal during the thermal preference trial max_prefbody_temp: the maximum body temperature of the animal during the thermal preference trial min_prefbody_temp: the minimum body temperature of the animal during the thermal preference trial std_dev_prefbody_temp: the standard deviation of body temperatures of the animal during the thermal preference trial notes_pref: any notes that may have affected trial or caused duration of trial to be adjusted side_started: the end of the gradient the animal first walked to after starting the trial

Sprint_df.csv. cham_id: individual chameleon identification number; date: date as mm/dd/yyyy; season: as above. trial: first or second time sprinting at the designated temperature; trial_temp: the body temperature at which sprint was tested; trial_time: time of day trial took place; cham_temp_i: initial body temperature of chameleon before sprinting; cham_temp_f: final body temperature of chameleon after sprinting; lap_x: the number of seconds to cross each 10 cm segment for each of 8 segments. Lap_50_x: the XXXX for each of 6 XXXX; Lap_100_x: the XXX for each of 2 XXX; Trial_successful: Y= trial completed without incident, N=trial failed; comment: notes associated with why trials failed or other relevant information.

q10_df.csv. Season, cham_id as above. trial_temp: the body temperature at which sprint speed was measured. Temp_range: the pair of temperatures for which q10 value was calculated. Q10: the q10 value resultant from the slop associated with performance of the values in the temp_range column.

morphometrics.csv  Cham_id and Season as above. sex: the sex of the individual; mass(g): mass of the individual in grams; svl(mm): snout vent length of the individual in mm; Tail_lgth(mm): length of the tail in mm. full_tail: Y= the individual has full intact tail, N= individual has partial tail; notes: relevant comments about the individual.


