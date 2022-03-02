;; WATERING_CROPGROWTH_RBB: Please refer the info tab for further information and suggestions for application. Please email Dr Kavin Narasimhan (k.narasimhan@surrey.ac.uk) for comments/queries

extensions [ rnd table csv ]

globals
[
  year_number                               ;; counter to keep track of model years
  month_number                              ;; 0 - 11
  month_num_name                            ;; a lookup table mapping month number and name for the purpose of printing outputs
  low_flow                                  ;; annual water flow in river is low
  medium_flow                               ;; annual water flow into the river is medium
  high_flow                                 ;; annual water flow into the river is high
  annual_flow                               ;; annual water flow in the simulated river
  water_source                              ;; amount of water at source, which provides to an irrigation scheme
  water_scheme                              ;; amount of water allocated to the scheme (might be stored in a reservoir)
  main_canal_level                          ;; pycor of the main canal
  sec_canal_level                           ;; pycor of the starting point of secondary canals (one patch below main canal)
  wat_allocated_irr                         ;; volume of water at source allocated for irrigation (equivalent of MCM units)
  scheme_peak                               ;; lowest depression among patches
  scheme_valley                             ;; highest depression among patches
  scheme_spread                             ;; difference between scheme_valley and scheme_peak
  scheme_middle                             ;; mean of the highest and lowest depression of patches
  scheme_depth                              ;; maximum depression or greatest depth of patches
  scheme_valley_middle                      ;; scheme valley middle line, used for colouring the landscape
  scheme_peak_middle                        ;; scheme peak middle line, used for colouring the landscape
  scheme_meander                            ;; water level of patches
  scheme_steepness                          ;; slope of patches
  hourly_water_supply                       ;; amount of water discharged from reservoir each irrigation hour
  monthly_water_supply                      ;; amount of water discharged from reservoir each month
  monthly_water_use                         ;; amount of water drawn from reservoir each month
  season_month                              ;; mapping between each calendar month and corresponding season (dry or wet)
  crop_calendar                             ;; a list indicating cultivation period for different crops (crop calendar)
  growing_period                            ;; total growing time of each crop
  growing_stages                            ;; duration of each growing stage (initial, development, mid and late seasons) of each crop
  Kc                                        ;; indicative crop factors for various crops and their growth stages
  water_land_prep                           ;; water needed for land preparation at different cropping stages  (mm/day)
  daytime_hours                             ;; mean daily percentage of annual daytime hours for different latitudes
]

breed [ farmers farm ]

patches-own [
  p_type                                   ;; reservoir/buffer/mcanal/scanal/valley
  surface_level                            ;; surface level of patch
  water_level                              ;; water level of patch; will be the same as surfaceLevel when patch is dry. can never be less than surfaceLevel
  water_vol                                ;; volume of water on patch (waterLevel - surfaceLevel)
]

farmers-own [
  f_size                 ;; farm size
  f_num_crops            ;; number of crops grown per year
  f_crop_name            ;; wet, dry or multi-season crop
  f_crop_stage           ;; crop growing stage; values: 0 (default), 1, 2, 3
  f_days_to_hvst         ;; no. of days until harvest
  f_days_at_stage        ;; no. of days in current crop growth stage
  f_ann_EtCrop           ;; crop evapotranspiration - list of 12 monthly values
  f_ann_net_crop         ;; net crop water need - list of 12 monthly values
  f_irr_demand           ;; annual irrigation demand - table of crop name and list of monthly values for each crop grown by the farmer
  f_tot_wat_demand       ;; list of monthly values for irrigation demand for all crops together (combined list of lists saved in f_irr_demand)
  f_tot_EtCrop           ;; list of monthly values for crop water demand for all crops together (combined list of lists saved in f_ann_EtCrop)
  f_crop_calendar        ;; could be different from gCropCalendar when staggered planting is enabled
  f_demand_spillover?    ;; indicates if f_crop_calendar of farmer spills into another calendar year
  f_one_julian_crop?     ;; one cultivation per year or two (with spillover from last year)
  f_crop_growth_chart    ;; crops assumed to grow 1 part for each month of the growing season (y/x), where x is length of growing season and y takes a value between 0 to 1 depending on how much water crops get (cf. nikolic2013)
]

to setup
  ca
  initialise-globals
  initialise-crops
  create-water
  create-irrigation-system
  create-farms
  create-farm-attributes
  calc-irr-demand
  reset-ticks
end

;; initialise global variables controlling simulation
to initialise-globals
  set year_number 1
  set month_number 0                                                             ;; month number starts at 0 to work easily with NetLogo lists
  set month_num_name [ [0 "january"] [1 "february"] [2 "march"] [3 "april"] [4 "may"] [5 "june"] [6 "july"] [7 "august"] [8 "september"] [9 "october"] [10 "november"] [11 "december"] ]
  set main_canal_level 20
  set sec_canal_level main_canal_level - 1
  set wat_allocated_irr 0
  set scheme_meander 10                                                         ;; controls the depression of patches
  set scheme_steepness 5                                                        ;; controls the slope of patches
end

;; initialise variables controlling crop growth - numerical values and/or value ranges used below are drawn from FAO manual (https://www.fao.org/3/s8376e/s8376e.pdf)
to initialise-crops
  ;; initialise what season each calendar month falls under
  ;; also check if the growing period of crops provided via interface can be covered within the respective seasons
  set season_month get-season-cal
  output-print (word "Seasons on calendar month basis: " season_month)

  ;; annual cropping calendar for differnt crops
  set crop_calendar table:make
  table:put crop_calendar dryCrop get-crop-cal onsetDrySeason dcGrowingPeriod
  table:put crop_calendar wetCrop get-crop-cal onsetWetSeason wcGrowingPeriod
  table:put crop_calendar multiCrop get-crop-cal onsetDrySeason mcGrowingPeriod
  table:put crop_calendar (word multiCrop "S2") get-crop-cal onsetWetSeason mcGrowingPeriod
  output-print (word "Global crop growing period in the study area: " crop_calendar)

  ;; total growth period
  set growing_period table:make
  table:put growing_period dryCrop dcGrowingPeriod
  table:put growing_period wetCrop wcGrowingPeriod
  table:put growing_period multiCrop mcGrowingPeriod
  table:put growing_period (word multiCrop "S2") mcGrowingPeriod

  ;; duration of each growth stage
  set growing_stages table:make
  table:put growing_stages dryCrop (list (item 0 growingStageSplit * dcGrowingPeriod)
  (item 1 growingStageSplit * dcGrowingPeriod)
  (item 2 growingStageSplit * dcGrowingPeriod)
  (item 3 growingStageSplit * dcGrowingPeriod))
  table:put growing_stages wetCrop (list (item 0 growingStageSplit * wcGrowingPeriod)
  (item 1 growingStageSplit * wcGrowingPeriod)
  (item 2 growingStageSplit * wcGrowingPeriod)
  (item 3 growingStageSplit * wcGrowingPeriod))
  table:put growing_stages multiCrop (list (item 0 growingStageSplit * mcGrowingPeriod)
  ((item 1 growingStageSplit + item 2 growingStageSplit) * mcGrowingPeriod)
  (item 3 growingStageSplit * mcGrowingPeriod))
   table:put growing_stages (word multiCrop "S2") (list (item 0 growingStageSplit * mcGrowingPeriod)
  ((item 1 growingStageSplit + item 2 growingStageSplit) * mcGrowingPeriod)
  (item 3 growingStageSplit * mcGrowingPeriod))

  ;; crop factor - FAO page 30 of 33 Chapter 3 Crop Water Needs
  set Kc table:make
  table:put Kc dryCrop (list 0.45 0.75 1.15 0.8)
  table:put Kc wetCrop (list 0.4 0.8 1.15 0.7)
  table:put Kc multiCrop (list 1.1 1.2 1)
  table:put Kc (word multiCrop "S2") (list 1.1 1.2 1)

  ;; indicative values below based on multiplying daily values from Tono dam (Ghana)
  set water_land_prep table:make
  table:put water_land_prep dryCrop (list 120 0 0 0 0 0 0 0 0 0 0 0)
  table:put water_land_prep wetCrop (list 0 0 0 0 0 0 0 0 0 0 0 0)
  table:put water_land_prep multiCrop (list 195 0 0 0 0 0 0 0 0 0 0 0)
  table:put water_land_prep (word multiCrop "S2") (list 195 0 0 0 0 0 0 0 0 0 0 0)

  ;; mean daily % of daytime hours (annual) for the latitude values of Navrongo (Ghana). Source: FAO Manual + FutureDAMS Hydroclimatology data (contact author for comments/questions)
  set daytime_hours [ 0.29 0.28 0.28 0.27 0.26 0.26 0.26 0.27 0.27 0.28 0.28 0.29 ]
end

;; create water in the scheme
to create-water
  set low_flow []
  set medium_flow []
  set high_flow []
  let low_elevation []
  let med_elevation []
  let high_elevation []
  let wl_counter 0

  ;; assumption: water level at source (which supplies to a reservoir in the irrigation scheme) follows a cosine function
  ;; assumption: water level would be average on most years, and really low or quite high in some years
  ;; implementation of assumptions
  ;; step #1: calculate values for simulated annual low, mean and high water levels (elevations)
  repeat 12 [
    set low_elevation lput precision (minRElevation + (random-float 10 * cos(wl_counter * 45))) 2 low_elevation
    set med_elevation lput precision (meanRElevation + (random-float 10 * cos(wl_counter * 45))) 2 med_elevation
    set high_elevation lput precision (maxRElevation + (random-float 10 * cos(wl_counter * 45))) 2 high_elevation
    set wl_counter wl_counter + 1 ]
  ;; step #2: find the (closest) volume corresponding to the simulated elevations
  foreach low_elevation [ x ->
    set low_flow lput item (position (closest-value x read-from-string rElevation) read-from-string rElevation) read-from-string rVolume low_flow ]
  foreach med_elevation [ x ->
    set medium_flow lput item (position (closest-value x read-from-string rElevation) read-from-string rElevation) read-from-string rVolume medium_flow ]
  foreach high_elevation [ x ->
    set high_flow lput item (position (closest-value x read-from-string rElevation) read-from-string rElevation) read-from-string rVolume high_flow ]
  ;; step #3: choose an elevation-volume combination (low,mean or high) to determine the water at source at the start of a year
  set annual_flow (list (list low_flow .1 ) (list medium_flow .7 ) (list high_flow .05 ) ) ;; choose water availability for the new year (low/mean/high) based on specified probabilities
  set water_source first rnd:weighted-one-of-list annual_flow [ [p] -> last p ]            ;; assign water level at source for the new year based on annual_flow
end

;; create the irrigation system
to create-irrigation-system
  ;; create reservoir
  ask patches with [pxcor < -18 and pycor > 20 ] [ set pcolor blue set p_type "reservoir" ask patch -20 24 [ set plabel "RESERVOIR" ] ]
  ;; create a buffer (dummy) zone
  ask patches with [pxcor >= -18 and pycor > 20] [ set pcolor grey set p_type "buffer" ]
  ;; create the main canal
  ask patches with [ pycor = main_canal_level ] [ set pcolor blue set p_type "mcanal" ask patch 0 main_canal_level [ set plabel "MAIN CANAL"] ]
  ;; create the secondary canals
  ask patches with [ pxcor mod 15 = 0 and pycor < main_canal_level ] [ set pcolor blue set p_type "scanal" ]
  ;; calculate water_level of patches that are not reservoir, buffer, mcanal and scanal based on the impress-variation method
  ask patches with [ p_type = 0 ][ impress-variation ]
  ;; assume that waterlevel of reservoir, buffer, mcanal and scanal patches is 1 higher than other patches
  ask patches with [ p_type != 0 ] [ set water_level (1 +  max [ water_level ] of patches with [ p_type = 0 ]) ]
  ;; calculate surface level of patches based on average elevation of the target area (surface_elevation in masl set via Interface) + water level of patches calculated above + related peak, valley and spread measures calculated below
  set scheme_peak max [ water_level ] of patches with [ p_type = 0 ]
  set scheme_valley min [ water_level ] of patches with [ p_type = 0 ]
  if scheme_peak = scheme_valley [
  set scheme_peak scheme_peak + 2
  set scheme_valley scheme_valley - 2 ]
  set scheme_spread scheme_peak - scheme_valley
  ask patches [ set surface_level (surface_elevation + (water_level - scheme_peak) / scheme_spread * max-pxcor) ]
  ;; tilt the landscape so everything runs downhill from North to South
  dry-setup
  ;; slope set by steepness
  ask patches [ set surface_level (surface_level + ( pycor / max-pycor ) * scheme_spread * scheme_steepness * .02) ]
  dry-setup
end

to impress-variation
  let px% 0
  let py% 0
  let sweep_width 0
  let meander_frequency 0
  let adj_py% 0
  let nearest_canal_point 0
  let xcor_nearest_canal_point 0
  set nearest_canal_point min-one-of patches with [ p_type = "scanal" ] [ distance myself ]
  set xcor_nearest_canal_point [ pxcor ] of nearest_canal_point
  ;; patches closest to the canal (scanals) have greater elevation
  set px% 100 - abs (pxcor - xcor_nearest_canal_point)
  set py% 1 - (min-pycor - pycor) / world-height     ;; results in values 0 ..1
  set sweep_width .01 * scheme_meander               ;; changing scheme_meander or the multiplication factor changes the smoothness of the distribution of elevation of patches up-down and left-right of the scanals
  set meander_frequency px% * 15                     ;; allows patches closest to scanals to have higher elevation and therefore darker patch colours
  set adj_py% (py% + (sweep_width * sin (meander_frequency)))
  set water_level abs adj_py%                        ;; set water level of patches
end

;; reset water level to surface level, i.e., dry the landscape
to dry-setup
  ask patches [ set water_level surface_level ]
  calc-variation
  update-setup
end

;; calculate peak, valley and spread values based on surface level measures: useful to model surface runoff later on if desired
to calc-variation
  set scheme_valley max [ surface_level ] of patches with [ p_type = 0 ]
  set scheme_peak min [ surface_level ] of patches with [ p_type = 0 ]
  if scheme_valley = scheme_peak [
    set scheme_valley scheme_valley + 2
    set scheme_peak scheme_peak - 2 ]
  set scheme_middle ( scheme_valley + scheme_peak ) * 0.5
  set scheme_spread abs ( scheme_valley - scheme_peak )
  set scheme_valley_middle (scheme_valley + scheme_middle) * 0.5
  set scheme_peak_middle (scheme_peak + scheme_middle) * 0.5
  set scheme_depth scheme_valley
end

;; apply colour to patches
to update-setup
  no-display         ;; freeze display when updating
  colour-setup       ;; apply colour
  display            ;; refresh-display
end

;; colour patches depending on water level: use level for display and show water colours
to colour-setup
  ask patches with [ p_type = 0 ] [ set water_vol water_level - surface_level ]
  let maxWL max-water-level
  let minWL min-water-level
  ask patches with [ p_type = 0 ] [ set pcolor set-colour maxWL minWL ]
end

;; create farmers on irrigable patches
to create-farms
  ;; create farms
  ;; farms will be located closer to canals to get water more easily (data + expert insight)
  ask n-of (0.3 * count patches) neighbors-canal with [ p_type = 0 ] [
    set p_type "farm"
    sprout-farmers 1 [
      set shape "plant"
      set size 1
      set color yellow
      set f_crop_calendar []
      set f_tot_wat_demand []
      set f_tot_EtCrop [] ]
  ]
end

;; initialise attributes for farmers
to create-farm-attributes
  ask farmers [ set f_size 0.5 + random-exponential 5 ]
  ;; more than one crop may be grown on the same plot each year, but max. only one crop per season
  ;; any of the crops indicated via the Interface may be grown
  ask farmers [
    ifelse (random 100 < 50) [ set f_num_crops 1 ] [ set f_num_crops 2 ]
    ifelse (f_num_crops = 1) [ set f_crop_name (list one-of (list dryCrop wetCrop multiCrop (word multiCrop "S2"))) ]
      [ ifelse (random 100 < 50) [ set f_crop_name (list multiCrop (word multiCrop "S2")) ] [ set f_crop_name (list dryCrop wetCrop)] ] ]
  set-crop-calendar
  output-print (word "Total no. of farmers: " count farmers " among which, the no. of farmers whose cropping schedule runs across calendar years is: " count farmers with [ f_demand_spillover? = true ] )
  set-crop-rota
end

;; create a crop calendar for each farm (farmer)
to set-crop-calendar
  ;; calculate f_crop_calendar based on f_crop_name and f_num_crops
  ask farmers [ set f_crop_calendar get-farmer-crop-cal ]
  ;; demand for water will spillover to a new year when f_crop_calendar runs across calendar years
  ask farmers [
    ;; below: combination 1: 1 crop only; dry or wet; combination 2: 1 crop only; multi crop; combination 3: 2 crops; dry and wet; combination 4: 2 crops; multi crop
    set f_demand_spillover? ifelse-value ((f_num_crops = 1 and member? "DC" f_crop_calendar and highest-consecutive-vals "DC" f_crop_calendar <  (table:get growing_period item 0 f_crop_name / 30)) or
      (f_num_crops = 1 and member? "WC" f_crop_calendar and highest-consecutive-vals "WC" f_crop_calendar <  (table:get growing_period item 0 f_crop_name / 30)) or
      (f_num_crops = 2 and (highest-consecutive-vals "DC" f_crop_calendar < (table:get growing_period item 0 f_crop_name / 30) or highest-consecutive-vals "WC" f_crop_calendar < (table:get growing_period item 1 f_crop_name / 30))))
    [ true ]
    [ false ] ]
end

;; create the annual cropping rota for each farm (farmer)
to set-crop-rota
  ask farmers [
    set f_crop_stage table:make
    set f_days_to_hvst table:make
    set f_days_at_stage table:make
    set f_ann_EtCrop table:make
    set f_ann_net_crop table:make
    set f_irr_demand table:make
    set f_crop_growth_chart n-values 12 [ 0 ]

    foreach f_crop_name [ x ->
      table:put f_crop_stage x 0
      table:put f_days_to_hvst x (table:get growing_period x)
      table:put f_days_at_stage x (item 0 table:get growing_stages x)
      table:put f_ann_EtCrop x []
      table:put f_ann_net_crop x []
      table:put f_irr_demand x [] ]

    ;; farmer's cropping calendar syncs with Julian calendar
    ifelse (f_demand_spillover? = false) [ set f_one_julian_crop? true ]

    [ ;; farmer's cropping calendar is out of sync with Julian calendar
      ;; calculate days harvested last year - based on which crop if growing more than one crop per year
      set f_one_julian_crop? false
      let days_completed 0

      if (f_num_crops = 1) [
        ;; can be dry crop only (DC); wet crop only (WC); or multi-crop grown in dry or wet months (still DC or WC)
        if (item 0 f_crop_calendar = "DC" or item 0 f_crop_calendar = "WC") [ ;; it might be a dry, wet or multi crop; starts with DC or WC; have only NCs other than DC or WC in f_crop_calendar
          set days_completed (table:get growing_period item 0 f_crop_name - ((length sublist f_crop_calendar 0 position "NC" f_crop_calendar) * 30)) ]
        ;; save days left for harvest
        table:put f_days_to_hvst (item 0 f_crop_name) (table:get growing_period item 0 f_crop_name -  days_completed)
        ;; calculate and save current crop stage of crop sowed last year
        let pos_counter -1
        foreach table:get growing_stages item 0 f_crop_name [ y ->
          if (days_completed > 0) [
            set pos_counter pos_counter + 1
            set days_completed days_completed - item pos_counter table:get growing_stages item 0 f_crop_name ] ]
        table:put f_crop_stage (item 0 f_crop_name) pos_counter
        ;; save days left in current crop stage of crop sowed last year
        table:put f_days_at_stage (item 0 f_crop_name) abs days_completed ]

      if (f_num_crops = 2) [
        let starting_value item 0 f_crop_calendar
        ;; starting value of the list will either be DC or WC only depending on which crop runs across years
        let starting_crop ifelse-value (starting_value = "DC") [ dryCrop ] [ wetCrop ]
        ;; whatever is not the starting crop will be the other crop
        let other_value ifelse-value (starting_value = "DC") [ "WC" ] [ "DC" ]
        let tmplst1 length sublist f_crop_calendar 0 position "NC" f_crop_calendar
        let tmplst2 length sublist f_crop_calendar 0 position other_value f_crop_calendar
        let ovlp min (list tmplst1 tmplst2) ;; NC or other crop whichever comes first, that position is taken to compute the remaining days of the starting crop

        if (length f_crop_name = 1) [
          ;; it's a multi-season crop in two growing seasons
          ;; NC, DC and WC are the values in the f_crop_calendar, which will start with DC or WC depending on whether its wet or dry season at the start of the year
          set days_completed (table:get growing_period item 0 f_crop_name - (ovlp * 30))
          ;; save days left for harvest of the rota pending from previous year
          table:put f_days_to_hvst (item 0 f_crop_name) (table:get growing_period item 0 f_crop_name -  days_completed)
          ;; calculate and save current crop stage of crop rota from last year
          let pos_counter -1
          foreach table:get growing_stages item 0 f_crop_name [ y ->
            if (days_completed > 0) [
              set pos_counter pos_counter + 1
              set days_completed days_completed - item pos_counter table:get growing_stages item 0 f_crop_name ] ]
          table:put f_crop_stage (item 0 f_crop_name) pos_counter
          ;; save days left in current crop stage of crop sowed last year
          table:put f_days_at_stage (item 0 f_crop_name) abs days_completed
        ]

        if (length f_crop_name = 2) [ ;; it's a combination of wet and dry season crops
          set days_completed (table:get growing_period starting_crop - (ovlp * 30))
          ;; save days left for harvest of the rota pending from previous year
          table:put f_days_to_hvst starting_crop (table:get growing_period starting_crop -  days_completed)
          ;; calculate and save current crop stage of crop rota from last year
          let pos_counter -1
          foreach table:get growing_stages starting_crop [ y ->
            if (days_completed > 0) [
              set pos_counter pos_counter + 1
              set days_completed days_completed - item pos_counter table:get growing_stages starting_crop ] ]
          table:put f_crop_stage starting_crop pos_counter
          ;; save days left in current crop stage of crop sowed last year
          table:put f_days_at_stage starting_crop abs days_completed
        ]
      ]
    ]
  ]
end

;; calculate the irrigation water demand for each farm (farmer)
to calc-irr-demand
  ;; simple calculation method - ref: FAO manual
  ;; step 1: collate yearly temperature and rainfall data
  let yearly_pre []      ;; annual precipitation
  let yearly_temp []     ;; annual temperature
  let yearly_pre_eff []  ;; annual effective precipitation
  let yearly_ETo []      ;; annual reference crop evapotranspiration
  let loc_counter 0      ;; local counter

  let prcp rainfall_pattern ;; rainfall_pattern for each month between jan-dec (set via the Interface) based on empirical data from our case study region; change as desired
  repeat 12 [
    set prcp replace-item loc_counter prcp abs (item loc_counter prcp * random-normal mean_prcp prcp_variation * sin (loc_counter * 15)) ;; mean and std. dev for precipitation (set via the Interface) based on empirical data from our case study region; change as desired
    set loc_counter loc_counter + 1
  ]
  set prcp map round prcp
  set loc_counter 0

  repeat 12 [
    let mR item loc_counter prcp ;; monthly rainfall
    let mRE ifelse-value ( mR >= 75 ) [ (0.8 * mR) - 25 ] [ max list 0 ((0.6 * mR) - 10) ] ; monthly rainfall effective
    set mRE round mRE
    let mTemp precision (random-normal mean_temp temp_variation) 1 ;; mean and std. dev for temperature (set via the Interface) based on empirical data from our case study region; change as desired
    set yearly_temp lput mTemp yearly_temp
    let mETo precision (item loc_counter daytime_hours * ((0.46 * mTemp) + 8)) 1 ;; monthly ref. crop evapotranspiration
    set yearly_pre lput mR yearly_pre
    set yearly_pre_eff lput mRE yearly_pre_eff
    set yearly_ETo lput mETo yearly_ETo
    set loc_counter loc_counter + 1 ]
    output-print (word "Effective Rainfall in Year " year_number " is: " yearly_pre_eff)

  ;; step 2: calculate the irrigation demand for the entire cropping season in a year for farmers based on the annual rainfall data from step 1
  ask farmers [
    foreach f_crop_name [ x -> ;; values can be wetCrop or dryCrop or multiCrop or multiCropS2
      let curr_pos 0
      foreach f_crop_calendar [ y -> ;; e.g., values ["DC" "DC" "NC" "NC" "WC" "WC" "WC" "WC" "WC" "DC" "DC" "DC" ]
        ;; reset demand if farmer's julian calendar crop rota has two rounds of cultivation: from previous + current years
        if (f_one_julian_crop? = false) [
          let unqVals length remove-duplicates f_crop_calendar ;; no. of unique values in the list - can be two (DC/WC and NC; DC and WC)  or three (DC WC and NC)
          if (curr_pos = (length f_crop_calendar - item unqVals count-vals f_crop_calendar)) [ ;; returns the first position of the last occurance of the crop rota whose cultivation is split across calendar years
            table:put f_crop_stage x 0
            table:put f_days_to_hvst x table:get growing_period x
            table:put f_days_at_stage x item 0 table:get growing_stages x
        ] ]

       ;; determine demand for calendar month currPos
        ifelse ((x = dryCrop and y ="DC") or (x = wetCrop and y = "WC") or (x = multiCrop and y = "DC") or (x = (word multiCrop "S2") and y = "WC")) [
          let newHvstCount (table:get f_days_to_hvst x - 30)
          table:put f_days_to_hvst x newHvstCount
          let mKc 0
          ifelse (table:get f_days_at_stage x >= 30) [ ;; 30 days or more in the current stage of crop growth
            let newStageCount (table:get f_days_at_stage x - 30)
            table:put f_days_at_stage x newStageCount
            set mKc item (table:get f_crop_stage x) table:get Kc x ] [ ;; less than 30 days in the current stage of crop growth
            let osDays table:get f_days_at_stage x
            let ocStage table:get f_crop_stage x
            ifelse (ocStage + 1 < length table:get growing_stages x) [
              table:put f_crop_stage x ocStage + 1
              table:put f_days_at_stage x (item (table:get f_crop_stage x) table:get growing_stages x - (30 - table:get f_days_at_stage x))
              set mKc precision (((osDays / 30) * item ocStage table:get Kc x) + (((30 - osDays) / 30) * item (table:get f_crop_stage x) table:get Kc x)) 2 ]
            [ set mKc precision (((osDays / 30) * item ocStage table:get Kc x)) 2  ] ]
             ;; crop evapotranspiration mm/month
             let etCrop (precision (mKc * item curr_pos yearly_ETo) 1) * 30
             ;; net irrigation demand mm/month
             let demandNET ifelse-value (item curr_pos yearly_pre_eff > etCrop) [ 0 ] [ (etCrop + item curr_pos table:get water_land_prep x) - item curr_pos yearly_pre_eff ]
             ;; net irrigation demand MCM/month
             set demandNET precision (0.00001 * f_size * demandNET) 4
             ;; gross irrigation demand MCM/month
             ;; gross irrigation requirement: net irrigation requirement / schemeIrrEfficiency
             ;; where irrigationEfficiency = Ec * Ea, Ec - conveyance efficiency, Ea - field application efficiency
             let demandGROSS demandNET / irrigationEfficiency
             table:put f_ann_EtCrop x lput etCrop table:get f_ann_EtCrop x
             table:put f_ann_net_crop x lput demandNET table:get f_ann_net_crop x
             table:put f_irr_demand x lput demandGROSS table:get f_irr_demand x
        ] [ ;; no demand for irrigation this month
          table:put f_ann_EtCrop x lput 0 table:get f_ann_EtCrop x
          table:put f_ann_net_crop x lput 0 table:get f_ann_net_crop x
          table:put f_irr_demand x lput 0 table:get f_irr_demand x
        ]
        ;; increment month counter
        set curr_pos curr_pos + 1
      ]
    ]
  ]
  ;; step 3: calculate cumulative results
  ask farmers [
    set f_tot_wat_demand n-values 12 [ 0 ]
    set f_tot_EtCrop n-values 12 [ 0 ]
    foreach f_crop_name [ x ->
      set f_tot_wat_demand (map + f_tot_wat_demand table:get f_irr_demand x)
      set f_tot_EtCrop (map + f_tot_EtCrop table:get f_ann_EtCrop x) ] ]
end


;; GO PROCEDURES
;; see comments on top of each go procedure for explanations of what they do

to go
  set-user-colour
  irrigate
  track-crop-growth
  print-outputs
  use-water
  update-time
  tick ;; each tick signifies one simulated month
end

to set-user-colour
  ;; farm's (plant's) colour is set to turquoise during cropping season, and yellow at other times
  ask farmers [ ifelse (item month_number f_crop_calendar != "NC") [ set color turquoise ] [ set color yellow ] ]
end

;; supply water to the scheme
to irrigate
  route-water
  water-losses
  color-patches
end

;; transfer water through the irrigation system (reservoir -> main canal -> secondary canals) based on irrigation scheme features (set via interface)
to route-water
  ;; assumption: totIrrAllocation of water available at source is allocated to the irrigation scheme
  set wat_allocated_irr ((totIrrAllocation / 100) * item month_number water_source)
  ask patches with [ p_type = "reservoir" ] [
    set water_vol wat_allocated_irr / count patches with [ p_type = "reservoir" ]
    set water_level water_vol + surface_level ]

  ;; wat_allocated_irr amount of water is routed to the main canal and secondary canals depending on the discharge rate, irrDaysPerDay and irrDaysPerMonth set via the interface
  ;; CHECKPOINT: Make sure we cannot discharge more water than is available for irrigation
  set hourly_water_supply dischargeRate * 60 * 60 * 0.000001 ;; in Million Cubic Metres (MCM)
  set monthly_water_supply hourly_water_supply * irrHoursPerDay * irrDaysPerMonth
  let temp dischargeRate
  while [monthly_water_supply > wat_allocated_irr] [
    set dischargeRate dischargeRate - 0.5
    set hourly_water_supply dischargeRate * 60 * 60 * 0.000001 ;; in MCM
    set monthly_water_supply hourly_water_supply * irrHoursPerDay * irrDaysPerMonth ]
  set dischargeRate temp
  ;; END OF CHECKPOINT

  ;; route water from reservoir to main canal
  ask patches with [ p_type = "mcanal" ] [
    set water_vol monthly_water_supply / count patches with [ p_type = "mcanal" ]
    set water_level water_vol + surface_level ]

  ;; route water from main canal to secondary canals
  ask patches with [ p_type = "scanal" ] [
    set water_vol monthly_water_supply / count patches with [ p_type = "scanal" ]
    set water_level surface_level + water_vol ]

  ;; route water from secondary canal to all patches in the irrigation scheme
  ;; create a patchset excluding reservoir, buffer and mcanal
  set monthly_water_use 0
  let water_recipients patches with [ p_type = 0 or p_type = "scanal" ]
  ask water_recipients [
    ;; randomly pick one neighbour with lowest water volume and move 10% of the difference in volume to that neighbour
    let local_min 0
    let min_vol 0
    let extra 0
    let portion 0

    ;; share if patch has water
    if water_vol > 0 and monthly_water_supply > 0 and monthly_water_use < monthly_water_supply [
      set min_vol min [ water_vol ] of neighbors
      if water_vol > min_vol [
        set local_min one-of neighbors with [ water_vol = min_vol ]
        set extra water_vol - min_vol
        ifelse extra < .001
        [ set portion extra ]
        [ set portion extra * benevolence ]
        ; if portion is more than is here, just take all of it
        if portion > water_vol [ set portion water_vol ]
        ;; adjust the levels
        set water_vol water_vol - portion
        set water_level surface_level + water_vol
        ask local_min [
          set water_vol water_vol + portion
          set water_level surface_level + water_vol
        ]
      set monthly_water_use monthly_water_use + portion
      ]
    ]
  ]
end

to water-losses
  ;; reduce water level by lossRate, which is linear and not proportional as it is due to surface area, not water volume
   if lossRate > 0 and losses?
   [ ask patches with [ water_level > surface_level ]
     [ set water_level water_level - random-float lossRate
       if water_level < surface_level
       [ ; don't allow level to be below elev!
         set water_level surface_level
       ]
     ]
   ]
end

to color-patches
  ask patches [ set water_vol water_level - surface_level ]
  ;; set the water colour (shades of blue) of each patch based on how much water they have received compared to other water user patches
  let maxWL max-water-level
  let minWL min-water-level
  ask patches with [p_type != "buffer" ] [
    ;; CHECKPOINT to avoid math exception error for scale-color function call if all water users get the same amount of water
    ifelse (maxWL = minWL)
    [ set pcolor set-colour (mean [ water_vol ] of patches with [p_type = "mcanal" ]) minWL ]
    [ set pcolor set-colour maxWL minWL ] ]
end

;; track crop growth each calendar month by comparing the amount of water needed to the amount of water on patch
to track-crop-growth
  ask farmers [
    let value 0
    ifelse (item month_number f_tot_wat_demand > 0) [
      ifelse (water_vol >= item month_number f_tot_wat_demand) [ set value 1 ] ;; water allocated exceeds demand for irrigation; full growth (1) at current crop stage
      [ set value (water_vol / item month_number f_tot_wat_demand) ] ] ;; water allocated falls short of demand for irrigation; crop growth proportional to water allocated
    [ ifelse (item month_number f_tot_EtCrop > 0) [ set value 1 ] ;; no need for irrigation if crop water demand is satisfied through rainfall - so full crop growth (1)
      [ set value 0 ] ] ;; no need for irrigation if no active crop
    set f_crop_growth_chart replace-item month_number f_crop_growth_chart value
    if (value = 1) [ set size size + 0.2 ] ]
end

;; print main summary statistics
to print-outputs
  clear-output
  output-print (word "*** SUMMARY STATS ***")
  output-print (word "Month: " item 1 item month_number month_num_name)
  output-print (word "Irrigation enabled for " irrHoursPerDay " hours per day for " irrDaysPerMonth " days a month")
  output-print (word " ")
  output-print (word "Water available (in reservoir): " monthly_water_supply " MCM")
  output-print (word "Water supplied (without targeted irrigation): " precision monthly_water_use 3 " MCM")
  output-print (word "Water demand (for growing crops): " precision sum [ item month_number f_tot_wat_demand ] of farmers 3 "MCM")
end

;; allocated water is used by water users
to use-water
  ask farmers [
    if ((water_vol >= item month_number f_tot_wat_demand) or (item month_number f_tot_wat_demand - water_vol <= 0.0001)) [
      set water_level water_level - item month_number f_tot_wat_demand
      set water_vol water_level - surface_level ] ]
end

;; update simulated time and time-dependent variables
to update-time
  set month_number month_number + 1                                                                ;; incrememt by one month
  if (month_number > 11) [                                                                         ;; tally outputs of the year completed
    clear-all-plots
    set year_number year_number + 1                                                                ;; increment year
    set month_number 0                                                                             ;; first month of the new year
    set annual_flow (list (list low_flow .1 ) (list medium_flow .7 ) (list high_flow .05 ) )       ;; choose water availability for the new year based on specified probabilities
    set water_source first rnd:weighted-one-of-list annual_flow [ [p] -> last p ]                  ;; assign water level at source for the new year based on annual_flow
    set-crop-calendar                                                                              ;; reset farmers' crop calendar
    set-crop-rota                                                                                  ;; reset farmers cropping plan as per crop calendar
    calc-irr-demand
    ask farmers [ set size 1 ]
  ]
end

;; helper methods ;;
;; report the closest value to input from a sourcelist
to-report closest-value [ input sourcelist ]
  report first reduce [[?1 ?2] -> ifelse-value (item 1 ?1 < item 1 ?2) [?1] [?2]] (map list sourcelist map [i -> abs(input - i)] sourcelist)
end

;; set color of the patch to shades of blue or brown depending on the amount of water (water_vol) available on patch
to-report set-colour [ maxLevel minLevel ]
  ifelse water_vol <= 0
  [ report set-earth-colour ]
  [ report set-water-colour maxLevel minLevel ]
end

;; set colour of the patch to shades of brown to signify water shortage
to-report set-earth-colour
  ifelse surface_level <= scheme_valley_middle
  [ report green - 6 + .8 * scale-color grey surface_level scheme_valley scheme_middle ]
  [ ifelse surface_level <= scheme_peak_middle
    [ report green - 4 + .8 * scale-color grey surface_level scheme_valley scheme_peak ]
    [ report green - 2 + .8 * scale-color grey surface_level scheme_middle scheme_peak ]
  ]
end

;; find max water_vol of water on any one patch across the scheme
to-report max-water-level
  let active_scheme_area patches with [p_type != "buffer" ]
  let localRecord [ water_vol ] of active_scheme_area
  set localRecord filter [ i -> i > 0 ] localRecord
  report ifelse-value (empty? localRecord) [ 0 ] [ max localRecord ]
end

;; find min water_vol of water on any one patch
to-report min-water-level
  let active_scheme_area patches with [p_type != "buffer" ]
  let localRecord [ water_vol ] of active_scheme_area
  set localRecord filter [ i -> i > 0 ] localRecord
  report ifelse-value (empty? localRecord) [ 0 ] [ min localRecord ]
end

;; set colour of patch to shades of blue based on water volume on patch
to-report set-water-colour [ maxLevel minLevel ]
  let color_gradient scale-color grey water_vol minLevel maxLevel
  ifelse color_gradient < 1 [ set color_gradient 7 - (color_gradient * 100) ] [ set color_gradient 0 ]
  ifelse color_gradient <= 0 [ set color_gradient 0 ] [ set color_gradient int color_gradient ]
  ;; greate the water_vol, darker the shade of blue on patch
  report 102 + color_gradient
end

;; find patches closer to canals for creating farms
to-report neighbors-canal
  report patch-set [ patches in-radius 4 ] of patches with [ p_type = "scanal" ]
end

;; calculate if it's dry or wet season each calendar month depending on the growing period and season onset values input via the interface
;; check for errors, give warning messages and adjust input values if needed
to-report get-season-cal
  ;; round off wcGrowingPeriod and dcGrowingPeriod to then calculate the wet and dry seasons and cropping schedule rounded to nearest months
  set wcGrowingPeriod wcGrowingPeriod - (wcGrowingPeriod mod 30) ;; might have to change later if the exact number of days affects crop water requirement
  set dcGrowingPeriod dcGrowingPeriod - (dcGrowingPeriod mod 30) ;; same as above
  set mcGrowingPeriod mcGrowingPeriod - (mcGrowingPeriod mod 30) ;; same as above

  let seasonCal n-values 12 ["D"]
  let mCounter 0
  let endWSeason 0
  let startWSeason position onsetWetSeason map last month_num_name ;; beginning of the wet season
  let startDSeason position onsetDrySeason map last month_num_name ;; beginning of the dry season

  ;; checkpoint #1
  ;; if the gap between onset of wet season and dry season is less than wcGrowingPeriod then readjust the onset of dry season accordingly
  if ((abs (startDSeason - startWSeason) * 30) < wcGrowingPeriod) [
    set onsetDrySeason item 1 item (startWSeason + (wcGrowingPeriod / 30)) month_num_name ;; update onset of dry season
    set startDSeason position onsetDrySeason map last month_num_name ;; updated beginning of the dry season
    show (word "WARNING: Onset of wet and dry seasons overlap. Readjusting onset of dry season based on the wcGrowingPeriod value: " onsetDrySeason)
    show "Reset option #1: If you prefer to retain the onset of dry season as it is but reduce the duration of wcGrowingPeriod instead, please update the values accordingly and press 'setup' again"
    show "Reset option #2: If you think wet and dry season cropping schedules do overlap - please get in touch with developer to discuss options"
  ]
  ;; step 1: calculate the end of wet season
  set endWSeason ifelse-value (startWSeason + (wcGrowingPeriod / 30) <= length seasonCal) [
    ;; end of wet season if in same calendar year
    startWSeason + (wcGrowingPeriod / 30) ]
  [ ;; end of the wet season if in next calendar year
    (wcGrowingPeriod / 30) - (length seasonCal - startWSeason)
  ]
  ;; step 2: assign which season each calendar month falls under based on the values we have calculated for startWSeason and endWSeason
  while [mCounter < length seasonCal] [
    if ((mCounter >= startWSeason and mCounter < endWSeason) or (startWSeason > endWSeason and (mCounter < endWSeason or mCounter >= startWSeason))) [
      set seasonCal replace-item mCounter seasonCal "W"
    ]
    set mCounter mCounter + 1
  ]
  ;; checkpoint #2: if dcGrowingPeriod is greater than the number of months in seasonCal with "D"
  ;; then readjust dcGrowingPeriod accordingly to avoid overlap with wet season and notify of the change
  if (dcGrowingPeriod >= ((length filter [ i -> i = "D" ] seasonCal) * 30)) [
     set dcGrowingPeriod (length filter [ i -> i = "D" ] seasonCal) * 30
    show (word "ERROR: The growing period suggested for the dry crop creates an overlap with the suggested onset of the wet season. Readjusting dcGrowingPeriod to avoid overlap. The new dcGrowingPeriod value is: " dcGrowingPeriod)
    show "Reset option #1: If you prefer to retain the value of dcGrowingPeriod as it is but change the onset of wet season instead, please update the values accordingly and press 'setup' again"
    show "Reset option #2: If you think wet and dry season cropping schedules do overlap - please get in touch with developer to discuss options"
  ]
  report seasonCal
end

;; calculate the annual crop growing season based on inputs provided under
to-report get-crop-cal [ month period ]
  let cropCal n-values 12 ["NC"] ;; NC stands for no cultivation
  let month1 0
  let month2 0
  let cancelSecondHarvest? false

  if (member? " " month) [
    ;; assumption: multi season crop will be harvested twice in a 12 month period
    set month1 substring month 0 position " " month
    set month2 substring month (position " " month + 1) length month
  ]

  foreach month_num_name [ x ->
    if ((item 1 x) = month or (item 1 x) = month1) [ ;; cropping season for dry crops indicated by "DC" in cropCal starting at onsetDryCrop; for wet crops, it's "WC" starting at onsetWetCrop
      if (month = onsetDrySeason or month1 = onsetDrySeason) [ set cropCal replace-item (position x month_num_name) cropCal "DC" ]
      if (month = onsetWetSeason or month1 = onsetWetSeason) [ set cropCal replace-item (position x month_num_name) cropCal "WC" ]
    ]
    if ((item 1 x) = month2) [
      if (month2 = onsetDrySeason) [ set cropCal replace-item (position x month_num_name) cropCal "DC" ]
      if (month2 = onsetWetSeason) [ set cropCal replace-item (position x month_num_name) cropCal "WC" ]
    ]
  ]

  if (member? "DC" cropCal) [
    let startPos position "DC" cropCal
    let endPos startPos + int (period / 30)
    let index startPos
    while [index >= startPos and index < endPos] [
      if (index >= length cropCal) [ set index 0 set startPos 0 set endPos endPos - length cropCal ] ;; cropping runs to next calendar year
      if (item index cropCal = "WC") [ ;; checkpoint
        show "Warning: The input you have provided for mcGrowingPeriod wouldn't work as the two cropping periods would overlap. So reducing to **one crop of multicrop** per calendar year."
        show "Reset option #1: If you prefer to retain two harvests for the multi season crop, reduce the duration of mcGrowingPeriod accordingly and press 'setup' again"
        show "Reset option #2: If you are OK with only one crop of a multi crop, but want to change the onset of the crop, then change the line of code: \"table:put gCropCalendar multiCrop get-crop-cal (word onsetDrySeason \" \" onsetWetSeason) mcGrowingPeriod\" to \"table:put gCropCalendar multiCrop get-crop-cal desired-month mcGrowingPeriod\" and press 'setup' again" ]
      set cropCal replace-item index cropCal "DC"
      set index index + 1
    ]
  ]

  if (member? "WC" cropCal) [
    let startPos position "WC" cropCal
    let endPos startPos + int (period / 30)
    let index startPos
    while [index >= startPos and index < endPos] [
      if (index >= length cropCal) [ set index 0 set startPos 0 set endPos endPos - length cropCal  ] ;; cropping runs to next calendar year
      ifelse (item index cropCal = "DC") [ ;; checkpoint: if we realise an overlap of crops mid-season, e.g., when cultivation runs to another calendar year
        show "Warning: The input you have provided for mcGrowingPeriod wouldn't work as the two cropping periods would overlap. So reducing to **one crop of multicrop** per calendar year."
        show "Reset option #1: If you prefer to retain two harvests for the multi season crop, reduce the duration of mcGrowingPeriod accordingly and press 'setup' again"
        show (word "Reset option #2: If you are OK with only one crop of a multi crop, but want to change the onset of the crop, then change the line of code: table:put gCropCalendar multiCrop get-crop-cal (word onsetDrySeason" " \" \"onsetWetSeason) mcGrowingPeriod to table:put gCropCalendar multiCrop get-crop-cal desired-month mcGrowingPeriod and press 'setup' again")
        set cancelSecondHarvest? true
      ] [
        set cropCal replace-item index cropCal "WC"
      ]
      set index index + 1
    ]
  ]
  ;; checkpoint: cancel second harvest season if growing months overlap with earlier crop
  if (cancelSecondHarvest? = true) [
    let index 0
    foreach cropCal [ ? ->
      if (? = "WC") [
       set cropCal replace-item index cropCal "NC" ]
      set index index + 1 ]
  ]
  report cropCal
end

;; calculate the crop calendar of farmer based on season_month and crop_calendar
to-report get-farmer-crop-cal
  let crop_rota []
  ;; only one crop per year, so the assigned crop_rota can remain intact
  if (f_num_crops = 1) [ set crop_rota table:get crop_calendar item 0 f_crop_name ]
  ;; two crops per year
  if (f_num_crops = 2) [
    let crop_cal1 table:get crop_calendar item 0 f_crop_name
    let crop_cal2 table:get crop_calendar item 1 f_crop_name
    let index 0
    foreach crop_cal1 [ x ->
      if (x != "NC") [ set crop_rota lput x crop_rota]
      if (x = "NC") [ ifelse (item index crop_cal2 != "NC") [set crop_rota lput (item index crop_cal2 ) crop_rota ] [set crop_rota lput "NC" crop_rota ] ]
      set index index + 1
    ]
  ]
  ;; checkpoint
  if (crop_rota = 0) [ show (word "Runtime Error: In assigning a crop rota for farmer " who) ]
  report crop_rota
end

;; report the highest number of consecutive occurances of a value in a list
to-report highest-consecutive-vals [x the-list]
  let curr_highest 0
  let curr_count 0
  foreach the-list [ curr_val ->
    ifelse (curr_val = x)
    [ set curr_count curr_count + 1 ]
    [ if (curr_count > curr_highest) [ set curr_highest curr_count ]
      set curr_count 0 ] ]
  if (curr_count > curr_highest) [ set curr_highest curr_count ]
  report curr_highest
end

;; report the (non-zero) lowest number of consecutive occurances of a value in a list
to-report lowest-consecutive-vals [x the-list]
  let curr_lowest (reduce + filter [ i -> i = x ] the-list) / x
  let curr_count curr_lowest
  foreach the-list [ curr_val ->
    ifelse (curr_val = x)
    [ set curr_count curr_count - 1 ]
    [ if (curr_count < curr_lowest and curr_count != 0) [ set curr_lowest curr_count ]
      set curr_count (reduce + filter [ i -> i = x ] the-list) / x ] ]
  if (curr_count < curr_lowest and curr_count != 0) [ set curr_lowest curr_count ]
  report curr_lowest
end

;; report the consecutive number of occurances of a value in a list
to-report count-vals [lst]
  if 0 = length lst [report lst]
  let value first lst
  let cnt 1
  let cts []

  foreach butfirst lst [? ->
    ifelse ? = value [ set cnt (1 + cnt) ]
    [
      set cts lput cnt cts
      set value ?
      set cnt 1 ]
  ]
  report lput cnt cts
end
@#$#@#$#@
GRAPHICS-WINDOW
424
10
1069
656
-1
-1
13.0
1
10
1
1
1
0
0
0
1
-24
24
-24
24
0
0
1
ticks
30.0

BUTTON
22
41
88
74
setup
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
94
41
157
74
go
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

BUTTON
163
41
226
74
tick
go
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

TEXTBOX
24
12
296
42
Basic Controls for Simulation
18
105.0
1

TEXTBOX
23
107
280
151
Model Settings 
18
105.0
1

TEXTBOX
22
140
347
166
Change values and settings below as desired
12
35.0
1

TEXTBOX
16
517
371
551
Irrigation Scheme Features 
14
105.0
0

SLIDER
15
575
191
608
dischargeRate
dischargeRate
1
8
7.0
1
1
m3/s
HORIZONTAL

SLIDER
15
610
191
643
irrHoursPerDay
irrHoursPerDay
0
24
12.0
1
1
hours
HORIZONTAL

SLIDER
15
645
191
678
irrDaysPerMonth
irrDaysPerMonth
0
30
19.0
1
1
days
HORIZONTAL

TEXTBOX
21
166
331
196
Water Availability 
14
105.0
0

TEXTBOX
21
192
290
220
Reservoir Elevation and Volume
11
35.0
0

TEXTBOX
18
213
412
283
Data below is approximation from Tono Dam, Ghana\n[change values to suit your chosen case study] 
11
35.0
0

INPUTBOX
15
379
363
439
rElevation
[163 166 167 169 171 172 173 175 176 178 179]
1
0
String

INPUTBOX
14
445
362
505
rVolume
[0 0.239 1.42 3.5 7.9 13 22.5 37 54 77.5 92.6]
1
0
String

INPUTBOX
17
250
107
310
minRElevation
170.0
1
0
Number

INPUTBOX
110
250
202
310
meanRElevation
175.0
1
0
Number

INPUTBOX
206
250
297
310
maxRElevation
180.0
1
0
Number

INPUTBOX
16
314
109
374
surface_elevation
170.0
1
0
Number

SLIDER
15
540
191
573
totIrrAllocation
totIrrAllocation
0
100
60.0
10
1
%
HORIZONTAL

PLOT
1081
253
1412
488
water in the irrigation scheme
month
volume of water (MCM)
0.0
11.0
0.0
0.1
true
true
"" ""
PENS
"water_allocated_irr" 1.0 0 -7500403 true "" "plot monthly_water_supply"
"water_supplied" 1.0 0 -13345367 true "" "plot monthly_water_use"

PLOT
1080
10
1407
246
water available at source
month
volume (million cu. metres)
0.0
11.0
0.0
100.0
true
false
"" ""
PENS
"default" 1.0 1 -16777216 true "" "plot item month_number water_source"

SLIDER
202
540
374
573
benevolence
benevolence
0
1
0.5
0.1
1
NIL
HORIZONTAL

SWITCH
203
619
306
652
losses?
losses?
1
1
-1000

TEXTBOX
205
584
402
629
water losses from evaporation and absorption
12
105.0
1

SLIDER
204
660
337
693
lossRate
lossRate
0
0.005
5.0E-4
0.0005
1
NIL
HORIZONTAL

TEXTBOX
425
658
1081
823
Visualisation cues: PRESS SETUP\n(1) Notice the reservoir, main canal and secondary canals (parallel blue lines), and irrigable land (green patches, varying shades indicate varying heights)\n(2) The yellow plant icons indicate farm plots. They change colour (turquoise) during cultivation months.\n\nPRESS GO\n(1) Notice the water flowing from the reservoir -> main canal -> secondary canals -> irrigable patches. Darker the shade of blue, greater the amount of water on patch\n(2) Notice the plant icons changing color and size depending on actual crop growth proportional to water needed for crop growth compared to water available on patch
12
25.0
1

TEXTBOX
20
730
170
748
Crop Growth 
14
105.0
1

TEXTBOX
20
763
359
815
Provide details of one major dry, rainy and multi-season crops\n\nSee examples below. Please use LOWERCASE ONLY.
11
35.0
1

INPUTBOX
25
814
101
874
wetCrop
maize
1
0
String

INPUTBOX
260
814
359
874
onsetWetSeason
may
1
0
String

INPUTBOX
104
814
180
874
dryCrop
tomato
1
0
String

INPUTBOX
183
814
258
874
multiCrop
rice
1
0
String

INPUTBOX
362
814
460
874
onsetDrySeason
october
1
0
String

INPUTBOX
24
879
133
939
wcGrowingPeriod
150.0
1
0
Number

INPUTBOX
135
879
244
939
dcGrowingPeriod
150.0
1
0
Number

INPUTBOX
246
879
357
939
mcGrowingPeriod
120.0
1
0
Number

CHOOSER
25
946
172
991
growingStageSplit
growingStageSplit
[0.2 0.3 0.3 0.2]
0

TEXTBOX
364
887
493
998
Note: Please provide growing periods as multiples of 30 day periods. Any other values provided will be rounded off
11
35.0
1

OUTPUT
1081
495
1587
638
14

SLIDER
15
681
192
714
irrigationEfficiency
irrigationEfficiency
0
1
0.6
0.1
1
NIL
HORIZONTAL

SLIDER
500
915
735
948
mean_temp
mean_temp
15
35
29.0
1
1
degree Celsius
HORIZONTAL

TEXTBOX
500
860
791
894
Temperature and Rainfall
14
105.0
1

SLIDER
500
954
672
987
temp_variation
temp_variation
0
5
2.0
0.5
1
NIL
HORIZONTAL

CHOOSER
500
993
693
1038
rainfall_pattern
rainfall_pattern
[0 0 0 1 1 1 1 1 1 0 0 0]
0

TEXTBOX
705
1002
855
1030
signifies it rains between April and Sep each year
11
35.0
1

TEXTBOX
501
881
888
915
All temperature and rainfall values set via these controls have been calibrated based on data gathered from Tono Dam (Ghana)
11
35.0
1

SLIDER
740
915
912
948
mean_prcp
mean_prcp
100
200
170.0
10
1
NIL
HORIZONTAL

SLIDER
739
952
911
985
prcp_variation
prcp_variation
50
100
80.0
10
1
NIL
HORIZONTAL

@#$#@#$#@
## WHAT IS IT?

This NetLogo model is a reusable component (also referred to as a Reusable Building Block or RBB) called WATERING_CROPGROWTH_RBB.

It is a sub-model of the WATER user associations at the Interface of Nexus Governance (WATERING) model (see https://www.youtube.com/watch?v=ZfSs__Lu52o) and extends the WATERING_IRRIGATION_RBB NetLogo model (https://github.com/kavinpreethi/WATERING_IRRIGATION_RBB) 

## HOW IT WORKS

* The plot showing water available at source (water volume) which supplies to the reservoir in the irrigation scheme follows a cosine pattern with two peaks (one at the start of a calendar year, and another in Sep-Oct each year). This assumption reflects the water availability in reservoirs we studied in Ghana (Africa). You could alter this assumption if you are interested in simulating a different pattern for water available at source in your case study. Make changes to the create-water procedure for this.

* This RBB represents an irrigation system made up of different types of patches. On setup you can identify these as: reservoir (water store providing water to the irrigation scheme), buffer zone (dummy area), main canal (principal outlet from reservoir), secondary canal (multiple outlets from the main canal), and water recipients (all green patches on setup). 

* On setup, the varying shades of green indicate the height of patches. Darker the green, greater the elevation of patches. Water reaches patches with the highest elevation first (assuming water is pumped up for the purpose of irrigation) and then flows to patches with lower elevations

* Water allocated for irrigation flows from the reservoir -> main canal -> secondary canals and finally reaches the water recipient patches

* The route-water procedure controls how water flows through the irrigation system

* The track-crop-growth procedure calculates the growth of crops during the growing season divided into four distinct growth stages (initial, development, mid-season, and late season). Crop growth is affected by the quantity of water available for irrigation

* The calc-irr-demand procedure calculates the water demand for the crops grown on each farm plot. Water demand varies based on the crop growth stage (initial, development, mid-season, and late season)

* The RBB allows each farmer to grow one or two crops per year. Crops grown can be dry season crops, wet season crops, or multi-season crops. You can change which dry, wet and rainy season crops are grown, for how long, and during which months by changing relevant values in the 'Crop Growth' section of the Interface. Also refer to the set-crop-calendar and set-crop-rota procedures.


## HOW TO USE IT

#### Water Availability settings has 6 Input parameters:
_**minRElevation:**_ Minimum water level (elevation) in source 
_**meanRElevation:**_ Mean water level (elevation) in source
_**maxRElevation:**_ Max water level (elevation) in source
_**surface_elevation:**_ Mean surface elevation of patches in the simulated irrigation scheme
_**rElevation:**_ Shows the average water level in meters above sea level (MASL) in Tono dam (Ghana) for each month of the year
_**rVolume:**_ Shows the average volume of water in million cubic meters (MCM) in Tono dam (Ghana) corresponding to rElevation values each month. 

_**Note:**_ An Area-Volume-Elevation mapping for a reservoir plays a key role in planning reservoir operations. We use _rElevation_ and _rVolume_ as an empirical reference to create simulated low, medium and high flows (all following a cosine pattern) based on _minRElevation_, _meanRElevation_, _maxRElevation_ and _surfaceRElevation_ to act as the water source (e.g., a bigger dam) supplying to the reservoir in our simulation


#### The Irrigation Scheme Features input settings have 8 controls:
_**totIrrAllocation:**_ Controls what % of the total water available at source is supplied to the reservoir for irrigation use
_**dischargeRate:**_ Controls the water flow rate (cubic metre per second) from the reservoir
_**irrHoursPerDay:**_ Duration of each irrigation event (0-24 hours)
_**irrDaysPerMonth:**_ Number of irrigation events each month (0 - 30 days) 
_Note:_ As the model runs in monthly time steps, the water flow you see at the end of each tick (month) is an aggregate of water supplied over the course of a month
_**irrigationEfficiency:**_ A value between 0 and 1 set via the interface as an approximation of how much water routed to the fields is effectively utilised for crop growth after losses (e.g., through evaporation)
_**benevolence:**_ A value between 0 and 1 affecting how much water each patch shares with its neighbouring patches
_**losses?:**_ Do you wish to simulate/mimic surface water evaporation from patches?
_**lossRate:**_ simulates water evaporation from patches if losses? is set to true


#### There are 9 crop growth settings - 8 input boxes and one chooser:

_**wetCrop:**_ Name of ONE preferred rainy season crop
_**dryCrop:**_ Name of ONE preferred dry season crop
_**multiCrop:**_ Name of ONE preferred multi-season crop 
_**onsetWetSeason:**_ Onset of the rainy season (value is a calendar month indicated in small case)
_**onsetDrySeason:**_ Onset of the dry season (value is a calendar month indicated in small case)
_**wcGrowingPeriod:**_ Duration of the growing season of the wetCrop (indicated in multiples of 30)
_**dcGrowingPeriod:**_ Duration of the growing season of the dryCrop (indicated in multiples of 30)
_**mcGrowingPeriod:**_ Duration of the growing season of the multiCrop (indicated in multiples of 30)
_**growingStageSplit:**_ _Note:_ If you alter this data to suit your needs, please remember that the sum of the duration in each stage should equal to 1.0

#### There are 5 input controls under the Temperature and Rainfall settings:

_**mean_temp:**_ mean temperature in the simulated irrigation scheme (the values we have used are based on our case studies in Ghana)
_**mean_prcp:**_ mean precipitation in the simulated irrigation scheme area
_**temp_variation:**_ standard deviation temperature 
_**prcp_variation:**_ standard deviation precipitation
_**rainfall_pattern:**_ months when rainfall is expected in the simulated irrigation scheme area (indicated by 1) or not (indicated by 0) within a list of 12 values


## THINGS TO NOTICE
Try changing the values of controls in the Interface and see how that affects water supply and use within the scheme. To keep track of the changes, make changes under one category at a time (e.g., Temperature and Rainfall) and see how that changes the outcomes. See if you can break the model. If you can, try fixing it and write to us (we will be happy to see your pull requests on GitHub) or write to us with issues and we will try getting it fixed to improve the model - and thereby all contributing to open and reproducible research. 

## THINGS TO TRY
Try changing the values of controls in the Interface and see how that affects water supply and use within the scheme. Try changing the benevolence parameter to see how that affects water sharing between patches. Think how you might convert the benevolence parameter into a procedure that simulates different levels of willingness among water users (on water recipient patches) to share water with other water users. Try to think also how you might introduce new agent categories or procedures to model water management policies within this simulated irrigated scheme.

## EXTENDING THE MODEL

What we are trying to achieve here is to simulate the routing of water available at source through a canal network in an irrigation scheme by mimicking gravity-based flow, whereby water flows from a higher elevation to lower elevations. The plot called _water in the irrigation scheme_ shows how much of the water allocated for irrigation is being effectively supplied to water recipients. Try making changes to the route-water procedure to see if you can minimise the difference between water supply and water use. Try making changes to the set-crop-rota, calc-irr-demand and tarck-crop-growth procedures (and changes within the helper procedures they call) to model different ways of irrigation and crop growth.

## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

## RELATED MODELS

The WATERING_CROPGROWTH_RBB uses the Blaney Criddle Method to calculate crop water demand (https://www.fao.org/3/s2022e/s2022e00.htm#Contents) and extends the WATERING_IRRIGATION_RBB (https://github.com/kavinpreethi/WATERING_IRRIGATION_RBB).

## CREDITS AND REFERENCES

Please email Dr Kavin Narasimhan (k.narasimhan@surrey.ac.uk) for comments/queries. If you adapt/use the WATERING_CROPGROWTH_RBB model, we would appreciate if you cite our GitHub repo. Acknowledgment: This work was supported by UK Research and Innovation Economic and Social Research Council [ES/P011373/1] as part of the Global Challenges Research Fund.

_**About WATERING:** WATERING was developed as an exploratory tool to understand and explain how participatory irrigation management through Water User Associations (WUAs) work. The model allows exploring the impact of community-based water management (through WUAs) on water availability, water use and economic productivity within an irrigation scheme. Please see https://www.youtube.com/watch?v=U-nqs9ak2nY for more information._
_**Note:** This RBB is not WATERING, it is a sub-model of WATERING which mimics water flow and crop growth an irrigation scheme - you can change the values of the input controls via the Interface and see how that affects water use and crop growth within the scheme (through visualisation in the NetLogo world and output plots). Our complete WATERING model includes other components to simulate various aspects of community-based water management through WUAs_
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.1.1
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
0
@#$#@#$#@
