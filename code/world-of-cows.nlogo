;; ### World of Cows
;; ### M18 - Impact Assessment of Markets and Policies on Land Use and Ecosystem Services
;; Developed by Maria Hänsel, Thomas Schmitt, Georg Smolka & Jakob Bogenreuther
;; Last updates made by MH on 16.11.2020

extensions [gis]                 ;; The gis extension is needed to be able to load (and work with) spatial data sets in NetLogo
breed [dairy-farms dairy-farm]   ;; Dairy farms (even though they are immobile) are represented as turtles for visualization purposes
breed [fields field]             ;; Fields (even though they are immobile) are represented as turtles for technical reasons
                                    ;; With turtles it is possible to adequately represent the centroid location of shapes in the GIS version - since non-integer values are possible

globals [
  land-lease-ha-price-grass      ;; The price that needs to be payed annually for leasing one hectare of grassland [€ / ha]
  land-lease-ha-price-crop       ;; The price that needs to be payed annually for leasing one hectare of cropland [€ / ha]

  Norg-limit                     ;; Allowed level of average organic Nitrogen per hectare on the fields [kg / ha]

  subsidies-crop                 ;; Subsidy-level per ha cropland [€ / ha]
  subsidies-grass                ;; Subsidy-level per ha grassland if the level of Norg is above the defined threshold [€ / ha]
  Norg-ext-threshold             ;; Grassland counts as extensive if its level of organic Nitrogen is below this threshold [kg / ha]
  subsidies-grass-ext            ;; Subsidy-level per ha cropland if the level of Norg is below the defined threshold [€ / ha]

  market-value-milk-int          ;; Producer price for milk, from dairy farms with (very) intensive strategy [€ / kg]
  market-value-milk-ext          ;; Producer price for milk, from dairy farms with (very) extensive strategy [€ / kg]
  mineral-fertilizer-price       ;; Price of mineral fertilizer [€ / kg]

  profit-very-intensive          ;; Average profit of all very-intensive farms [€ / ha]
  profit-intensive               ;; Average profit of all intensive farms [€ / ha]
  profit-extensive               ;; Average profit of all extensive farms [€ / ha]
  profit-very-extensive          ;; Average profit of all very-extensive farms [€ / ha]

  profit-size-small              ;; Average profit of all small farms [€ / ha]
  profit-size-medium             ;; Average profit of all medium farms [€ / ha]
  profit-size-large              ;; Average profit of all large farms [€ / ha]
  profit-size-extralarge         ;; Average profit of all extralarge farms [€ / ha]

  field-data                     ;; Global variable for field GIS data
  farm-data                      ;; Global variable for farm GIS data

  crop-fields                    ;; Agentset containing all fields with the land use "cropland"
  grass-fields                   ;; Agentset containing all fields with the land use "grassland"

  list-of-tenants                ;; Sorted agentset of dairy-farms that are interested in renting new land because they want to expand
  list-of-manure-recipients      ;; Sorted agentset of dairy-farms that are interested in receiving manure because their average Norg level is below the Norg limit
  manure-pool                    ;; Annual sum of all surplus manure (dairy-farms that exceed the Norg-limit at farm level) that can be received by farms with Norg deficit

  grassland-conversion-ban       ;; Grassland to cropland conversion is banned if the policy option 2 is chosen and the share of grassland falls below the defined threshold
                                    ;; of minimum grassland share (controlled via interface - slider "minimum-grassland-share") -> Can take the values "true" and "false"
  GHG-area-output                ;; The entire greenhouse gas (GHG) emissions of the area. This includes the sum of all on-farm emissions as well as the emissions due to fertilizer production [CO2 equivalents]
  nitrate-area-output            ;; The entire nitrate emissions of the area (sum of all farm nitrate emissions)

  climate-regulation-index       ;; Index for the ecosystem service "climate regulation": Ranging from 0 (very bad) to 100 (very good)
  max-climate-regulation         ;; Define the best value possible (assumption for the best scenario - everything is extensive grassland)
  min-climate-regulation         ;; Define the worst value possible (assumption for the worst scenario - everything is intensive cropland)

  water-quality-index            ;; Index for the ecosystem service "water quality": Ranging from 0 (very bad) to 100 (very good)
  max-water-quality              ;; Define the best value possible (assumption for the best scenario - everything is extensive grassland)
  min-water-quality              ;; Define the worst value possible (assumption for the worst scenario - everything is intensive cropland)

  habitat-quality-index          ;; Index for the ecosystem service "habitat quality": Ranging from 0 (very bad) to 100 (very good)
  max-habitat-quality            ;; Define the best value possible (assumption for the best scenario - everything is extensive grassland)
  min-habitat-quality            ;; Define the worst value possible (assumption for the worst scenario - everything is intensive cropland)

  soil-fertility-index           ;; Index for the ecosystem service "soil fertility": Ranging from 0 (very bad) to 100 (very good)
  max-soil-fertility             ;; Define the best value possible (assumption for the best scenario - everything is extensive grassland)
  min-soil-fertility             ;; Define the worst value possible (assumption for the worst scenario - everything is intensive cropland)
]

fields-own [
  gis-id                         ;; In GIS version: Field ID linking to original GIS data ("TARGET_FID") for data analysis purposes
  field-id                       ;; In GIS version: Needed for the setup / display of the individual shapes (fields)
  distance-farmfield             ;; Distance between the field and its farm [km]

  tenant                         ;; The farm ID number of the dairy-farm who leases the field
  ha-size                        ;; Size of the field in ha (For imported polygons, value is taken from GIS attributes) [ha]
  for-lease?                     ;; Is the field currently offered on the land market? -> Can take the values "true" and "false"
  new-tenant?                    ;; Did the field change tenant in the current year? -> Can take the values "true" and "false"

  land-use                       ;; Current land use of the field (imported start value for gis version) -> Can take two values - grassland and cropland
  years-since-land-use-change    ;; Amount of years passed since the last conversion of the land use of the field -> necessary for soil-fertility calculation

  kg-Norg-field                  ;; Total amount of organic fertilizers spread onto the respective field [kg N / year]
  kgN-mineral-field              ;; Amount of mineral fertilizers spread onto each field [kg N / year]
  kg-N-field                     ;; Amount of fertilizers spread onto each respective field in total (organic and mineral) [kg N / year]

  no-of-cuts                     ;; The number of cuts (times of harvest) per year -> only applies to grassland!
  my-farm-intensity              ;; The value of the intensity scale of the farm leasing this field. -> Can take the following values: 1 (very extensive), 2 (extensive), 3 (intensive), 4 (very intensive)

  received-subsidies-field       ;; Currently received subsidies for the field [€ / year]
  leasing-costs-field            ;; The annual lease costs per field, depending on the land-use (grassland / cropland) and the size of the field [€ / year]
  labour-costs-field             ;; Currently needed working time (in a monetary unit) for the field - depends on the fields ha-size, nitrogen fertilization and distance between the farm and the field [€ / year]

  soil-fertility-factor           ;; A factor to asses the productivity of each field based on its soil fertility. Varies from 0.4 (minimum productivity) to 1 (maximum productivity)
  feed-value-quantity-field      ;; The yearly amount of fodder (according to its feed value = Net energy content for lactation) produced on the field in mega joule [MJ / year]
                                 ;; The value of this variable depends on the land-use type (cropland vs. grassland), the amount of applied fertilizer (Norg + mineral), and the inherent productivity of the field
]

dairy-farms-own [
  farm-id                        ;; In GIS version: Linking to original GIS data ("farmid") for data analysis purposes -> matches "who" to easily inspect farms

  farm-size                      ;; Total area of farmed property [ha]
  my-fields                      ;; All fields (agentset) that are owned by a certain dairy-farm
  my-grassland-share             ;; Grassland share per dairy farm
  no-of-cows                     ;; Number of cows owned by a dairy-farm

  produced-manure                ;; Total amount of manure produced on farm level [m³ / year]
  received-manure                ;; Amount of manure received from other farms [m³ / year]
  available-manure               ;; Amount of manure that will be applied on farm level [m³ / year]
  initial-avg-Norg-level         ;; Average amount of applied organic fertilizer based on produced manure (before manure trade) [kg Norg / ha]
  adjusted-avg-Norg-level        ;; Average amount of applied organic fertilizer based on available manure (after manure trade) [kg Norg / ha]
  N-level                        ;; Average amount of N spread onto my fields (mineral and organic) per hectare [kg N / ha / year]

  feed-value-quantity            ;; The yearly amount of fodder (according to its feed value = Net energy content for lactation) produced by the farm in mega joule [MJ / year]
  milk-volume                    ;; Total milk volume per farm and year based on the available feed value quantity per cow [kg / year]

  received-subsidies             ;; Total amount of received subsidies for the farm (sum of all fields) [€ / year]
  received-market-value          ;; Total amount of received market value (from selling milk) for the farm [€ / year]
  input-costs                    ;; Current annual monetary investment of the farm - costs apply per hectare of land but vary with the farm's intensity and mineral fertilizer input [€ / year]
  land-lease                     ;; Total yearly amount of lease that has to be paid per farm (sum of all fields) [€ / year]
  labour-costs                   ;; Currently needed working time (in a monetary unit) for all fields of a farm (sum of all fields) [€ / year]
  annual-interim-profit          ;; Total amount of profit for the farm for the current year - balance without any policy mechanism applying [€ / year]
  annual-profit                  ;; Total amount of profit for the farm for the current year - after deduction of taxes, fines and subsidy reductions [€ / year]
  low-profit-years               ;; Count of years with an annual profit below a certain threshold.

  intended-farm-size-change      ;; Strategy of dairy-farms regarding farm size changes for the current year. -> Can take three values: -10 (downsize) / 0 (no change) / +10 (expand)
  intensity-scale                ;; Intensity of management on a farm-level for the current year. -> Can take the following values: 1 (very extensive), 2 (extensive), 3 (intensive), 4 (very intensive)
  size-class                     ;; Size class of the farm (used to determine based-on-profit strategy) -> Four categories (small, medium, large, extralarge)
  years-since-intensity-change   ;; Count of years since the last change of the farm's intensity

  Norg-surplus-fine              ;; Fine paid by a dairy farm, for exceeding the allowed level of average organic Nitrogen per hectare on their fields - after manure trade (if respective policy is chosen) [€]
                                 ;; The height of the fine (in percentage of the subsidies received) is set via the slider "subsidy-reduction-Norg-surplus"
  GHG-farm-emission              ;; The amount of GHG emissions in CO2 equivalents emitted by a dairy farm
  GHG-tax-paid                   ;; The amount of taxes paid for the emitted greenhouse gases per dairy farm (if respective policy is chosen) [€]
  nitrate-farm-emission          ;; The amount of nitrate emissions emitted by a dairy farm
  nitrate-tax-paid               ;; The amount of taxes paid for nitrate emissions per dairy farm (if respective policy is chosen) [€]

]



to setup
  clear-all

  ;; To import new shape from library: Tools >> Turtle Shapes Editor >> Import from library
  set-default-shape dairy-farms "person farmer"

  if (world-setup = "random-world")   ;; Observer can decide at the interface to run the model in the random world or the gis-world
  [set-up-random-world]

  if (world-setup = "gis-world")      ;; Observer can decide at the interface to run the model in the random world or the gis-world
  [set-up-gis-world]

  initialize-agent-variables
  initialize-global-variables

  ;; These two steps are needed for the procedure - "interact-on-land-market", since the list is sorted by annual profit
  calculate-interim-profit
  ask dairy-farms [set annual-profit annual-interim-profit]

  update-world

  reset-ticks
end

to go
  ;; Stop after a certain amount of years (= ticks)
  ;; To observe the long-term model development, the next line should be commented out
  if ticks > 30 [stop]              ;; stops the procedure "go" (= stops the model run)

  update-global-variables-start
  update-agent-variables-start

  stop-farming?

  decide-farm-strategy
  interact-on-land-market
  update-farm-size
  if count dairy-farms <= 4 [stop]  ;; stops the procedure "go" (= stops the model run)

  specify-field-use
  specify-cow-stuff
  exchange-manure
  set-available-manure
  fertilize-fields
  harvest-fields
  feed-and-milk-cows
  sell-milk
  influence-ecosystem-services
  calculate-interim-profit
  implement-policies

  update-global-variables-end
  update-world

  tick
end

to set-up-random-world  ;; "Random" distribution of farms & allocation of fields

  ;; Setting up dairy farms ------------------------------------------------------------------------------------------
  ;; Step 1: Create dairy-farms with random positions
  create-dairy-farms 175 [            ;; 175 farms are created to match the farm per ha densitiy of the gis version
    setxy random-pxcor random-pycor   ;; Assign random position to dairy-farm
    ;; Have dairy-farms choose another position if there is already a dairy-farm present at this location
    while [any? other dairy-farms-here] [setxy random-pxcor random-pycor]]
  ;; Step 2: Assigning dairy farm properties
  ask dairy-farms [
    ;; Assigning number of cows per farm based on a probability distribution - the mean number of cows per farm approximately matches the gis world (= around 34 cows per farm)
    set no-of-cows 5 + round (random-poisson 2 + random-gamma 0.461 0.017)
  ]

  ;; Setting up fields -----------------------------------------------------------------------------------------------
  ;; Step 1: Since in the gis-world setup fields have to be represented by agents (not by patches) the same procedure is chosen in the random-world setup
  ;; Each patch creates (= "sprouts") one field on itself - they are set to be invisible, since this is only about the technical implementation
  ask patches [sprout-fields 1 [set hidden? true]]
  ;; Step 2: Assigning field properties
  ask fields [
    set tenant -1             ;; Set all tenant to "no value" (in this case -1 is chosen as a dummy)
    set ha-size 0.5           ;; The size of one patch is set to 0.5 ha to approximately match the scale of the gis study area
    set land-use "grassland"  ;; First: set land use of all fields to grassland

  ]

  ;; Allocate fields to dairy-farms ----------------------------------------------------------------------------------
  ask dairy-farms [
    set farm-id who    ;; The internal NetLogo turtle id "who" is set as farm-id
    let current-dairy-farm farm-id
    ;; First: Set fields at the position of dairy-farms as leased by this farm
    ask fields-here [set tenant current-dairy-farm]]

  ;; Second: Let leased fields "grow" around dairy-farms by asking unallocated fields (= -1) to choose the same tenant as a neighbour field
  while [any? fields with [tenant = -1]] [       ;; -1 is used as "no value" in this case
    ask fields with [tenant = -1] [
    set tenant [tenant] of one-of fields-on neighbors4]]

  ;; assign fields explicitly as my-fields to dairy-farms
  ask dairy-farms [
    set my-fields fields with [tenant = [farm-id] of myself]
  ]

  ;; ask 0-3 "large" (>75 ha and <100ha) dairy-farms to grow bigger than 100 ha (randomly between 100 and 125 ha) to become "extralarge".
  if (count dairy-farms with [count my-fields >= 150 AND count my-fields <= 200] >= 3) [ ;; precondition is that there are at least 3 "large" farms
  ask n-of random 4 dairy-farms with [count my-fields >= 150 AND count my-fields <= 200] [ ;; one field is 0.5 ha (and 150 fields are 75 ha)
    ;;The closest X number of fiels ( X = the difference to 200 fields + 0-50 other fields) that are not directly located on the patch of another dairy-farm
      ;;and not already belonging to the farm that wants to become extra large are assigned to this farm
    ask min-n-of (200 - count my-fields + random 51) fields with [any? other dairy-farms-here = false AND tenant != [farm-id] of myself] [distance myself]
    [set tenant [farm-id] of myself]]
  ]

  ask dairy-farms with [count my-fields = 0] [die]
end

to set-up-gis-world   ;; Setup of fields based on real-world GIS data (ATKIS) - attribution of ownership and farm location was specified in ArcGIS and R-Studio

  ;; Load GIS data - draw map with polygons (colored according to land use) ------------------------------------------
  set field-data gis:load-dataset "/Users/zhaofujian/Documents/学习/TJU/基础课程/Semester2/可持续发展辅修/可持续发展环境法/world-of-cows-exploring-land-use-policies-for-a-dairy-farm-world-teaching-modeling-complex-human-environment-systems_v1/data/WP_fields.shp" ;; add here the path to the shapefile and the shapefile name "WP_fields.shp"
  set farm-data gis:load-dataset "/Users/zhaofujian/Documents/学习/TJU/基础课程/Semester2/可持续发展辅修/可持续发展环境法/world-of-cows-exploring-land-use-policies-for-a-dairy-farm-world-teaching-modeling-complex-human-environment-systems_v1/data/WP_farms.shp"

  ;; Make sure that all data is within the world, but doesn't resize the world -> this is done by the basic primitive RESIZE-WORLD
  gis:set-world-envelope gis:envelope-of field-data

  ;; Setting up dairy-farms --------------------------------------------------------------------------------------------
  foreach (gis:feature-list-of farm-data)
  [ the-point -> create-dairy-farms 1
    [let cell-centroid gis:centroid-of the-point
     let centroid-location gis:location-of cell-centroid
     setxy item 0 centroid-location item 1 centroid-location
     set farm-id gis:property-value the-point "farmid"
      ifelse (gis:property-value the-point "cows" < 5) [set no-of-cows round (gis:property-value the-point "cows") + 5] [set no-of-cows round (gis:property-value the-point "cows")]
      ]
  ]

  ;; Setting up fields -----------------------------------------------------------------------------------------------
  ;; Defining (hidden) agents to represent fields (centroid of polygons) and assigning layer properties
  let n 1
  foreach (gis:feature-list-of field-data)
    [?1 -> create-fields 1 [

      set field-id n
      set gis-id gis:property-value ?1 "TARGET_FID"
           let field-centroid gis:centroid-of ?1
           let centroid-location gis:location-of field-centroid
           setxy item 0 centroid-location item 1 centroid-location
           set hidden? true
           set land-use gis:property-value ?1 "type"
           set ha-size gis:property-value ?1 "area_field"
           set tenant gis:property-value ?1 "farmid"
      ]
      set n n +  1 ]
end

to stop-farming?    ;; Determines when farms are decide to no longer be in business because of low profitability (they "die")

  ;ask dairy-farms with [farm-size = 0] [die]                           ;; This has been moved to the procedure "update-farm-size"
  ask dairy-farms with [low-profit-years >= low-profit-years-needed]     ;; Amount of low-profit-years-needed can be adjusted by the observer at the interface
  [
    ask my-fields [set tenant -1]            ;; Before the farm dies it asks all its fields to set the tenant to "no value" (with dummy variable -1) -> used for interact-on-land-market
    die
  ]
end

to decide-farm-strategy
  ;; The farm strategy decision is oriented at the profit of the best performing farms
  if (ticks > 1) [            ;; in the first year, ther is no real income, so the procedure starts from the 2nd year on

    ;; STEP 1: Based on the four categories of both size and intensity, determine how much profit per hectare can be made in the "best" category
    let best-size max (list profit-size-small profit-size-medium profit-size-large profit-size-extralarge)
    let best-intensity max (list profit-very-intensive profit-intensive profit-extensive profit-very-extensive)

    ;; Farm size decision  ---------------------------------------------------------------------------------------------
    ;; STEP 2: Ask eligible farms to evaluate and potentially change their farm size (based on profit comparison)
    ask dairy-farms [
      ; An case the annual profit of the farm is lower (or within the accepted range of a reduced share => X% is set via interface - "accepted-share-of-best-strategy") of the mean annual profit of the best category, they change the farm size into the direction of the best category.
      ifelse (annual-profit > best-size * accepted-share-of-best-strategy / 100)
      ;ifelse (annual-profit / farm-size > best-size * ((100 - perc-best-strategy) / 100))
      [set intended-farm-size-change 0 ]
      [
      if (best-size = profit-size-small) [ifelse (size-class = "small") [set intended-farm-size-change 0 ] [set intended-farm-size-change -10]]
      if (best-size = profit-size-medium)
        [ifelse (size-class = "small") [set intended-farm-size-change 10]
          [ifelse (any? dairy-farms with [size-class = "large"] or any? dairy-farms with [size-class = "extralarge"])
            [ifelse (size-class = "medium") [set intended-farm-size-change 0] [set intended-farm-size-change -10]]
              [set intended-farm-size-change 10]]]
      if (best-size = profit-size-large)
        [ifelse (size-class = "extralarge") [set intended-farm-size-change -10]
          [ifelse (any? dairy-farms with [size-class = "extralarge"])
            [ifelse (size-class = "large") [set intended-farm-size-change 0] [set intended-farm-size-change 10]]
              [set intended-farm-size-change 10]]]
      if (best-size = profit-size-extralarge) [ifelse (size-class = "extralarge") [set intended-farm-size-change 0 ] [set intended-farm-size-change 10]]
      ]
    ]

    ;; Farm intensity decision  ----------------------------------------------------------------------------------------
    ;; STEP 3: Ask limited number of farms to evaluate and potentially change their intensity scale
    ;; 3.1 farms with less than five cows are only allowed to intensify:
      ask dairy-farms with [years-since-intensity-change > waiting-time-for-intensity-change][   ;; You must wait a minimum number of years until you can change your intensity again
        let my-previous-intensity intensity-scale
        ifelse ((no-of-cows <  5) AND intensity-scale != 4 )
      []
      ; [set intensity-scale intensity-scale + 1]    ;; If you only have 5 cows left and your intensity scale is not yet very intensive, then you can only intensify

        ;; 3.2 in case the annual profit of the farm is lower (or within the accepted range of a reduced share => X% is set via interface - "accepted-share-of-best-strategy") of the mean annual profit of the best category, farms adjust the intensity
        [ifelse (annual-profit / farm-size > best-intensity * accepted-share-of-best-strategy / 100)
        [set intensity-scale intensity-scale ]
        [if (best-intensity = profit-very-extensive ) [ifelse (intensity-scale = 1) [set intensity-scale intensity-scale] [set intensity-scale (intensity-scale - 1) ]]
             if (best-intensity = profit-extensive) [ifelse (intensity-scale = 1) [set intensity-scale (intensity-scale + 1) ] [ifelse (intensity-scale = 2) [set intensity-scale intensity-scale ] [set intensity-scale (intensity-scale - 1) ]]]
               if (best-intensity = profit-intensive) [ifelse (intensity-scale = 4) [set intensity-scale (intensity-scale - 1) ] [ifelse (intensity-scale = 3) [set intensity-scale intensity-scale ] [set intensity-scale (intensity-scale + 1) ]]]
                 if (best-intensity = profit-very-intensive ) [ifelse (intensity-scale = 4 )[set intensity-scale intensity-scale] [set intensity-scale (intensity-scale + 1 )]]
        ]
      ]
      if (my-previous-intensity != intensity-scale) [set years-since-intensity-change 0]
    ]
  ]
end

to interact-on-land-market
  ;; Assumption: Ludwig II owns all farmland - dairy-farms can only lease land
  ;; Real situation - in Bavaria: ~ 50% is leased / in study area ~ 30-40% is leased)

  ;; STEP 1: Label fields that are intended not to be leased anymore by those dairy-farms that want to downsize
  ;; (= 10% of leased fields, irrespective of field size)
  ask dairy-farms with [intended-farm-size-change < 0]
  [ ifelse count my-fields > 5
    [ifelse world-setup = "random-world"
    ;; Selection of most distant fields (10%) - these are then labeled as being "for-lease" - distance differentiation between random world (edges of world open) and gis world (edges closed)
      [ask max-n-of (round (0.1 * count my-fields)) my-fields [distance myself] [set for-lease? true]]
      [ask max-n-of (round (0.1 * count my-fields)) my-fields [distance-nowrap myself] [set for-lease? true]]
    ]
    ;; If 5 or less fields are currently leased, the most distant one is chosen to be labeled "for-lease" - distance differentiation between random world (edges of world open) and gis world (edges closed)
    [ifelse world-setup = "random-world"
      [ask max-one-of my-fields [distance myself] [set for-lease? true]]
      [ask max-one-of my-fields [distance-nowrap myself] [set for-lease? true]]
    ]
  ]
  ;; Fields of farms that stopped farming are also labeled "for-lease"
  ask fields with [tenant = -1] [set for-lease? true]

  ;; STEP 2: Create a list of interested tenants (= dairy-farms that want to expand), sorted by annual income (descending)
  set list-of-tenants sort-on [(- annual-profit)] dairy-farms with [intended-farm-size-change > 0]
  ;; Minus in front of annual-profit needed for descending order

  ;; STEP 3: Distribute fields to interested tenants (dairy-farms with higher annual income are served first) until
  ;;         a) all interested tenants are satisfied or b) all fields in the list are taken
  ;;         Those fields taken on lease are always the closest available to the tenant
  foreach list-of-tenants [  ;; This is an anonymos procedure for a list (sorted agentset) with the structure: foreach "list" [x -> ask x [...]]
      x -> ask x [
        ;; Check if there are any fields that are still for lease, else stop
        ifelse any? fields with [for-lease? = true]
      [
        ;; Check if there are fields to choose from, else take the remaining fields that are for lease
        ifelse round (0.1 * count my-fields) < count (fields with [for-lease? = true])
        [;; Select the closest fields to the interested new tenant and assign new tenant (= own farm-id) - distance differentiation between random world (edges of world open) and gis world (edges closed)
        ifelse world-setup = "random-world"
          [ask min-n-of (round (0.1 * count my-fields)) fields with [for-lease? = true] [distance myself]
            [set tenant [farm-id] of myself    ;; Change tenant
             set new-tenant? true              ;; Needed for analysis
              set for-lease? false]]           ;; Take field from land market
          [ask min-n-of (round (0.1 * count my-fields)) fields with [for-lease? = true] [distance-nowrap myself]
            [set tenant [farm-id] of myself    ;; Change tenant
             set new-tenant? true              ;; Needed for analysis
              set for-lease? false]]           ;; Take field from land market
        ]
        [;; Take the remaining fields and assign new tenant (= own farm-id)
          ask fields with [for-lease? = true]
            [set tenant [farm-id] of myself    ;; Change tenant
             set new-tenant? true              ;; Needed for analysis
             set for-lease? false]             ;; Take field from land market
      ]
      ]
      [stop]                                   ;; Stops the procedure "interact-on-land-market"
    ]
  ]
end

to update-farm-size ;; Update changed agent variables from previous procedure

  ask dairy-farms [
    set my-fields fields with [tenant = [farm-id] of myself]
    set farm-size sum [ha-size] of my-fields

    ;; All farms that don't have any fields after the land-exchange are removed
    if (farm-size = 0) [die]

    ;; calculate distance from field to its respective farm
    ask my-fields with [new-tenant? = true]
    [ifelse world-setup = "random-world"
      [set distance-farmfield (0.071 * distance one-of dairy-farms with [farm-id = [tenant] of myself])] ;; Factor 0.071 = conversion factor = patch side lenght [km] (distance variable is a multiple of patch side lenght, patch size = 0.5 ha, side length = 70.71 m = 0.071 km)
      [set distance-farmfield (0.13 * distance-nowrap one-of dairy-farms with [farm-id = [tenant] of myself])]]  ;; Factor 0.13 = conversion factor for the real distances in gis world [km]

    ;; Updates the difference size classes of farms (by amount of hectare)
    if (farm-size < 20 ) [set size-class "small"]
    if (farm-size >= 20 AND farm-size < 75) [set size-class "medium"]
    if (farm-size >= 75 AND farm-size < 100) [set size-class "large"]
    if (farm-size >= 100) [set size-class "extralarge"]

    ;; Assign the value of the farm's intensitiy to all of their fields
    ask my-fields [set my-farm-intensity [intensity-scale] of myself]
  ]
end

to specify-field-use
;; Policy option 2: Ban grassland conversion for the following year, if the total share of grasslands falls below the threshold (51-100%) defined by the observer via the interface.

  ;; OPTION 1 = policy 2 is switched on (only conversion from cropland to grassland possible if overall grassland-share falls under defined threshold (Switch - "minimum-grassland-share")
  ;;------------------------------------------------------------------------------------------------------------------
  if grassland-conversion-ban = true
  ;;------------------------------------------------------------------------------------------------------------------
    [
    ;; very-extensive farms only cultivate grasslands
    ask dairy-farms with [intensity-scale = 1]
      [ask my-fields with [land-use = "cropland"] [set land-use "grassland" set years-since-land-use-change 0]]

    ;;extensive farms make sure that at least 85% of their fields are grasslands
    ask dairy-farms with [intensity-scale = 2]
    [if my-grassland-share < 0.85
        [ifelse round ((0.85 - my-grassland-share) * count my-fields) < count my-fields with [land-use = "cropland"]
          [ask n-of (round ((0.85 - my-grassland-share) * count my-fields)) my-fields with [land-use = "cropland"]
            [set land-use "grassland" set years-since-land-use-change 0]]
          [ask my-fields with [land-use = "cropland"]
            [set land-use "grassland" set years-since-land-use-change 0]]
        ]
    ]

    ;; intensive farms make sure that at least 70% of their fields are grasslands
    ask dairy-farms with [intensity-scale = 3]
      [if my-grassland-share < 0.70
        [ifelse round ((0.70 - my-grassland-share) * count my-fields) < count my-fields with [land-use = "cropland"]
          [ask n-of (round ((0.70 - my-grassland-share) * count my-fields)) my-fields with [land-use = "cropland"]
            [set land-use "grassland" set years-since-land-use-change 0]]
          [ask my-fields with [land-use = "cropland"]
            [set land-use "grassland" set years-since-land-use-change 0]]
        ]
    ]

    ;; very intensive farms make sure that at least 50% of their fields are grasslands
    ask dairy-farms with [intensity-scale = 4]
      [if my-grassland-share < 0.50
        [ifelse round ((0.50 - my-grassland-share) * count my-fields) < count my-fields with [land-use = "cropland"]
          [ask n-of (round ((0.50 - my-grassland-share) * count my-fields)) my-fields with [land-use = "cropland"]
            [set land-use "grassland" set years-since-land-use-change 0]]
          [ask my-fields with [land-use = "cropland"]
            [set land-use "grassland" set years-since-land-use-change 0]]
        ]
    ]
  ]

  ;; OPTION 2 = policy 2 is switched off (conversion from cropland to grassland as well as from grassland to cropland possible)
  ;;------------------------------------------------------------------------------------------------------------------
  if grassland-conversion-ban = false
  ;;------------------------------------------------------------------------------------------------------------------

    [
    ;; very extensive farms only cultivate grasslands
    ask dairy-farms with [intensity-scale = 1]
      [ask my-fields with [land-use = "cropland"][set land-use "grassland" set years-since-land-use-change 0]]

    ;; extensive farms make sure that grasslands is equivalent to ~ 85% of their fields
    ask dairy-farms with [intensity-scale = 2]
      [ifelse my-grassland-share < 0.85
        [ifelse round ((0.85 - my-grassland-share) * count my-fields) < count my-fields with [land-use = "cropland"]
          [ask n-of (round ((0.85 - my-grassland-share) * count my-fields)) my-fields with [land-use = "cropland"]
            [set land-use "grassland" set years-since-land-use-change 0]]
          [ask my-fields with [land-use = "cropland"]
            [set land-use "grassland" set years-since-land-use-change 0]]
        ]
        [ifelse round ((my-grassland-share - 0.85) * count my-fields) < count my-fields with [land-use = "grassland"]
          [ask n-of (round ((my-grassland-share - 0.85) * count my-fields)) my-fields with [land-use = "grassland"]
            [set land-use "cropland" set years-since-land-use-change 0]]
          [ask my-fields with [land-use = "grassland"]
            [set land-use "cropland" set years-since-land-use-change 0]]
        ]
    ]

    ;; intensive farms make sure that grasslands is equivalent to ~ 70% of their fields
    ask dairy-farms with [intensity-scale = 3]
      [ifelse my-grassland-share < 0.70
        [ifelse round ((0.70 - my-grassland-share) * count my-fields) < count my-fields with [land-use = "cropland"]
          [ask n-of (round ((0.70 - my-grassland-share) * count my-fields)) my-fields with [land-use = "cropland"]
            [set land-use "grassland" set years-since-land-use-change 0]]
          [ask my-fields with [land-use = "cropland"]
            [set land-use "grassland" set years-since-land-use-change 0]]
        ]
        [ifelse round ((my-grassland-share - 0.70) * count my-fields) < count my-fields with [land-use = "grassland"]
          [ask n-of (round ((my-grassland-share - 0.70) * count my-fields)) my-fields with [land-use = "grassland"]
            [set land-use "cropland" set years-since-land-use-change 0]]
          [ask my-fields with [land-use = "grassland"]
            [set land-use "cropland" set years-since-land-use-change 0]]
        ]
    ]

    ;; very intensive farms make sure that grasslands is equivalent to ~ 50% of their fields
    ask dairy-farms with [intensity-scale = 4]
      [ifelse my-grassland-share < 0.50
        [ifelse round ((0.50 - my-grassland-share) * count my-fields) < count my-fields with [land-use = "cropland"]
          [ask n-of (round ((0.50 - my-grassland-share) * count my-fields)) my-fields with [land-use = "cropland"]
            [set land-use "grassland" set years-since-land-use-change 0]]
          [ask my-fields with [land-use = "cropland"]
            [set land-use "grassland" set years-since-land-use-change 0]]
        ]
        [ifelse round ((my-grassland-share - 0.50) * count my-fields) < count my-fields with [land-use = "grassland"]
          [ask n-of (round ((my-grassland-share - 0.50) * count my-fields)) my-fields with [land-use = "grassland"]
            [set land-use "cropland" set years-since-land-use-change 0]]
          [ask my-fields with [land-use = "grassland"]
            [set land-use "cropland" set years-since-land-use-change 0]]
        ]
    ]
  ]

  ;; Update agentsets of crop-fields /grass-fields after changes have been made --------------------------------------
  set crop-fields fields with [land-use = "cropland"]
  set grass-fields fields with [land-use = "grassland"]

  ;; Update grassland shares of dairy-farms according to new land use ------------------------------------------------
  ask dairy-farms [
   set my-grassland-share ((sum [ha-size] of my-fields with [land-use = "grassland"]) / sum [ha-size] of my-fields)
  ]
end

to specify-cow-stuff

  ;; Step 1: A certain number of cows per dairy farm is assigned, based on the intensity scale (cows have to be within the predefined range of cows per hectare)
  ask dairy-farms with [intensity-scale = 1]    ;; according to defined intensity scale, farms have between 0.2 and 0.79 cows per hectare
    [set no-of-cows round (farm-size * 0.2 + farm-size * random-float 0.59)]  ;; round is used to avoid to have fractions of cows per farm

  ask dairy-farms with [intensity-scale = 2]         ;; according to defined intensity scale, farms have between 0.8 and 1.39 cows per hectare
    [set no-of-cows round (farm-size * 0.8 + farm-size * random-float 0.59)]  ;; round is used to avoid to have fractions of cows per farm

  ask dairy-farms with [intensity-scale = 3]         ;; according to defined intensity scale, farms have between 1.4 and 1.69 cows per hectare
    [set no-of-cows round (farm-size * 1.4 + farm-size * random-float 0.29)]  ;; round is used to avoid to have fractions of cows per farm

  ask dairy-farms with [intensity-scale = 4]    ;; according to defined intensity scale, farms have between 1.7 and 2.5 cows per hectare
    [set no-of-cows round (farm-size * 1.7 + farm-size * random-float 0.8)]  ;; round is used to avoid to have fractions of cows per farm

  ;; Step 2: Define produced amount of manure and the initial average Norg level per farm (before manure is traded)
  ask dairy-farms [
    set produced-manure no-of-cows * 25.4  ;; every cow produces 25.4 m^3 manure per year
    ;; Source of value "25.4" -> LfL Leitfaden für die Düngung von Acker- und Grünland 2018 / Anhang 4b / Milchkuh 8000 kg Milch, 0.9 Kalb, TM 7.5%

    set initial-avg-Norg-level (produced-manure * 4.2 / farm-size) ;; each m^3 of manure contains 4.2 kg of N
    ;; Source of factor 4.2 -> LfL Leitfaden für die Düngung von Acker- und Grünland 2018 / Anhang 5 / Milchviehgülle (Grünland, TM 7.5%)
  ]
end

to exchange-manure  ;; dairy-farms try to give away or receive manure based on their Norg farm balance

  ;; STEP 1: Create pool of manure from dairy-farms with Norg surplus
  set manure-pool
;;    (sum [initial-avg-Norg-level] of dairy-farms with [initial-avg-Norg-level > Norg-limit]) - count dairy-farms with [initial-avg-Norg-level > Norg-limit] * Norg-limit
    (sum [(initial-avg-Norg-level - Norg-limit) / 4.2 * farm-size] of dairy-farms with [initial-avg-Norg-level > Norg-limit])

  ;; STEP 2: Create a list of interested manure receivers. Very intensive farms are served first, sorted by their manure deficit and followed by intensive farms, also sorted by their manure deficit (both descending).
  let manure-recipients-intensity-4 sort-on [initial-avg-Norg-level] dairy-farms with [initial-avg-Norg-level < Norg-limit and intensity-scale = 4]
  let manure-recipients-intensity-3 sort-on [initial-avg-Norg-level] dairy-farms with [initial-avg-Norg-level < Norg-limit and intensity-scale = 3]
  set list-of-manure-recipients sentence manure-recipients-intensity-4 manure-recipients-intensity-3

  ;; STEP 3: Distribute manure to interested receivers (dairy-farms with higher Norg deficit are served first) until
  ;;         a) all interested manure receivers are satisfied or b) there is no manure left in the manure-pool
  foreach list-of-manure-recipients [
      x -> ask x [
        ;; Check if there is still manure to be distributed, else stop
       ifelse manure-pool > 0
           [
        ;; Check if there is still enough manure to satisfy decifit, else take the remaining manure
        ifelse ((Norg-limit - initial-avg-Norg-level) / 4.2 * farm-size) < manure-pool
        ;; Factor 4.2 (see source above)
        [
         ;; Farm receives manure - based on deficit
         set received-manure (Norg-limit - initial-avg-Norg-level) / 4.2 * farm-size
         ;; Amount is deducted from manure pool
         set manure-pool (manure-pool - received-manure)
        ]
        [
         set received-manure manure-pool ;; Farm receives the left-over manure
         set manure-pool 0
      ]]
      [stop]                             ;; Stops the procedure "exchange-manure"
  ]]

end

to set-available-manure  ;; Set available manure & distribute left-over manure back to donor farms
  let manure-surplus sum [(initial-avg-Norg-level - Norg-limit) * farm-size] of dairy-farms with [initial-avg-Norg-level > Norg-limit] ;; Total manure-surplus before manure exchange

  ask dairy-farms with [initial-avg-Norg-level > Norg-limit] [
    let share-of-manure-surplus ((initial-avg-Norg-level - Norg-limit) * farm-size / manure-surplus) ;; Share of total manure suplus of each dairy farm that contributed to the surplus
    set available-manure (Norg-limit / 4.2 * farm-size + (manure-pool * share-of-manure-surplus))    ;; In case the manure-pool is not empty after trading, the manure goes back the farm (according to its share of contribution to the manure surplus)
]

  ask dairy-farms with [initial-avg-Norg-level <= Norg-limit] [
    set available-manure (produced-manure + received-manure)
    set received-manure 0]

  ask dairy-farms [set adjusted-avg-Norg-level round (available-manure * 4.2 / farm-size)]
end

to fertilize-fields

  ask dairy-farms [
    ask my-fields [
      ;; The available organic Nitrogen is distributed evenly to all fields
      set kg-Norg-field [adjusted-avg-Norg-level] of myself * ha-size

      ifelse ([intensity-scale] of myself = 4 ) [  ;; If the intensity scale is 4, mineral fertilizers are used to reach an overall N level of 300kg per hectare
        ifelse (([adjusted-avg-Norg-level] of myself ) < 300)
          [set kgN-mineral-field  ha-size * (300 - [adjusted-avg-Norg-level] of myself)]
          [set kgN-mineral-field 0 ]
      ]

      [ifelse ([intensity-scale ] of myself = 3) [ ;; If the intensity scale is 3, mineral fertilizers are used to reach an overall N level of 200kg per hectare
        ifelse (([adjusted-avg-Norg-level] of myself ) < 200)
          [set kgN-mineral-field ha-size * (200 - [adjusted-avg-Norg-level] of myself )]
          [set kgN-mineral-field 0 ]
          ]
        [set kgN-mineral-field 0]   ;; If the intensity scale is 2 or 1 - no mineral fertilizer is used
      ]
      ;; The total nitrogen input per field is the sum of mineral N and organic N
      set kg-N-field (kgN-mineral-field + kg-Norg-field)
    ]
    ;; Defines the average per farm level of applied nitrogen (organic + mineral) per hectare
    set N-level (sum [kg-N-field] of my-fields / farm-size)
  ]
end

to harvest-fields

  ;; Step 1: Depending on the intensity strategy, the number of cuts per year for grassland is defined
  ask grass-fields with [my-farm-intensity = 1] [set no-of-cuts 1]
  ask grass-fields with [my-farm-intensity = 2] [set no-of-cuts 2]
  ask grass-fields with [my-farm-intensity = 3] [set no-of-cuts one-of [3 4]]
  ask grass-fields with [my-farm-intensity = 4] [set no-of-cuts one-of [5 6]]

  ;; Step 2: Calculate the quantitiy of feed-value per field
  ;; For grassland fields: Considering ha-size, soil-fertility-factor, number of cuts, the net energy content for lactation (NEL) per kg dry mass of grass depending on nitrogen input, as well as the average grass dry mass harvest per cut
  ask grass-fields [set feed-value-quantity-field (ha-size * soil-fertility-factor * no-of-cuts * (5.63 + (kg-N-field / (ha-size * no-of-cuts)) * 0.004) * 3900)]
  ;; Source of value "5.63" (basic NEL of grass without N fertilization [MJ/kg]) -> LfL  Gruber Tabelle zur Fütterung der Milchkühe, Zuchtrinder, Schafe, Ziegen 2017 / p. 64 / NEL Wiesengras 6.28-7.06
  ;; Source of value "0.004" (increase factor of NEL depending on N fertilization [MJ/kg N]) -> Angeringer et al. 2018: Wirkung verschiedener Nutzungsintensitäten auf montane Goldhaferwiesen im Biolandbau
  ;; Source of value "3900" (Amount of grass harvested per year and cut [kg/ha])  -> LfL Aufwuchsverlauf von Grünlandbeständen in Bayern 2018 / Voralpines Hügelland / Ertrag 1700-5600 / https://www.lfl.bayern.de/mam/cms07/ite/dateien/voralpineshügelland.pdf

   ;; For cropland fields: Considering ha-size, soil-fertility-factor, the net energy content for lactation (NEL) per kg dry mass of crop depending on nitrogen input, as well as the average crop dry mass harvest per year
    ask crop-fields [set feed-value-quantity-field (ha-size * soil-fertility-factor * (5.91 + (kg-N-field / ha-size) * 0.004) * 18414)]
  ;; Source of value "5.91" (basic NEL of crop without N fertilization [MJ/kg]) -> LfL  Gruber Tabelle zur Fütterung der Milchkühe, Zuchtrinder, Schafe, Ziegen 2017 / p. 70 / NEL Maissilage Wachsreife 7
  ;; Source of value "18414" (Amount of crop harvested per year and cut [kg/ha]) -> Bayerisches Landesamt für Statistik 2015: Ernte der Feldfrüchte und des Grünlandes in Bayern 2014 / p. 6 / Grünmais, Silomais (35% Trockenmasse) / https://www.statistik.bayern.de/veroeffentlichungen/epaper.php?pid=42311&t=1 /

  ;; Step 3: Calculate quantity of feed-value per farm (= sum of feed-value per field)
  ask dairy-farms [set feed-value-quantity sum [feed-value-quantity-field] of my-fields]
end

to feed-and-milk-cows

  ;; calculate the amount of milk a farm produces
  ask dairy-farms [
    ifelse no-of-cows > 0 AND (feed-value-quantity / no-of-cows) > 14564 [ ;; Cows need a minimum amount of fodder to give milk
      ;; Source of value "14564" -> LfL  Gruber Tabelle zur Fütterung der Milchkühe, Zuchtrinder, Schafe, Ziegen 2017 / p. 20 / Versorgungsempfehlung für Kühe nach täglicher Milchmenge
      ifelse (feed-value-quantity / no-of-cows) <= 74789 ;; above a certain threshold of fodder, cows can't produce more milk (here 50 kg/day)
      ;; Source of value "74789" -> LfL  Gruber Tabelle zur Fütterung der Milchkühe, Zuchtrinder, Schafe, Ziegen 2017 / p. 20 / Versorgungsempfehlung für Kühe nach täglicher Milchmenge
        ;; milk production per cow in kg based on an average value for the region. The actual productivity depends on the feed value quantity each cow receives.
        [set milk-volume no-of-cows * 6748 * ((feed-value-quantity / no-of-cows) / 36831)]
        [set milk-volume no-of-cows * 6748 * (74789 / 36831)]
        ;; Source of value "6747.6" -> STMELF Buchführungsergebnisse landwirtschaftlicher Betriebe in Bayern / Wirtschaftsjahr 2016/2017 / Milchviehbetrieb / alle Betriebsgrößen / Region 1 / Verkaufte Milch pro Kuh /3700.1-7662.1
        ;; Source of value "36831.8" -> LfL  Gruber Tabelle zur Fütterung der Milchkühe, Zuchtrinder, Schafe, Ziegen 2017 / p. 20 / Versorgungsempfehlung für Kühe nach täglicher Milchmenge
    ]
    [set milk-volume 0]
  ]
end

to sell-milk

  ;; calculating the received market value for all the milk produced by a farm (market value depending on intensity)
  ask dairy-farms with [intensity-scale = 2 OR intensity-scale = 1]
    [set received-market-value (milk-volume * market-value-milk-ext)]

  ask dairy-farms with [intensity-scale = 3 OR intensity-scale = 4]
    [set received-market-value (milk-volume * market-value-milk-int)]
end

to influence-ecosystem-services

  ask dairy-farms [
    ;; Calculate greenhouse gas (GHG) emissions per dairy farm

    let CH4 (no-of-cows * 93.7) ;; CH4 output per cow per year
    ;; Source of value "93.7" -> Ellis et al. 2007:  Prediction of methane production from dairy and beef cattle
    let N2O sum [ha-size] of my-fields * N-level * (my-grassland-share * 0.0204 + (1 - my-grassland-share) * 0.0117)  ;; Grassland and cropland emit N2O depending on the amount of applied N
    ;; Source of values "0.0204" and "0.0117" (emission factors of N2O from N application) -> Dobbie et al. 1999: Nitrous oxide emissions from intensive agricultural systems: variations between crops and seasons, key driving variables, and mean emission factors
    let CO2plus sum [ha-size] of my-fields * (1 - my-grassland-share) * 840  ;; Cropland is on average a net emitter of CO2
    ;; Source of value "840" (kg CO2 emitted by 1 ha of cropland)-> Vleeshouwers & Verhagen 2002: Carbon emission and sequestration by agricultural land use: a model study for Europe
    let CO2minus sum [ha-size] of my-fields * my-grassland-share * 520     ;; Grassland is on average a net sink of CO2
    ;; Source of value "520" (kg CO2 sequestered by 1 ha of grassland) -> Vleeshouwers & Verhagen 2002: Carbon emission and sequestration by agricultural land use: a model study for Europe
    ;; Since the different greenhouse gas emissions have different climate impacts they are all converted to CO2 equivalents (hence the factors 25 and 298)
    set GHG-farm-emission (CH4 * 25 + N2O * 298 + CO2plus - CO2minus)
    ;; Source of values "25" and "298" (conversion factors) -> IPCC 2007

    ;; Calculate nitrate emissions per dairy farm (differing emission factor depending on land use and fertilizer input)
    let nitrate-cropland (sum [ha-size] of my-fields with [land-use = "cropland"] * N-level * 0.225)
    ;; Source of value "0.225" (fraction of N leaching from cropland into groundwater) -> Di & Cameron: Nitrate leaching in temperate agroecosystems: sources, factors and mitigating strategies
    let nitrate-grassland (sum [ha-size] of my-fields with [land-use = "grassland"] * N-level * 0.09)
    ;; Source of value "0.09" (fraction of N leaching from grassland into groundwater) -> Di & Cameron: Nitrate leaching in temperate agroecosystems: sources, factors and mitigating strategies
    set nitrate-farm-emission (nitrate-cropland + nitrate-grassland)
  ]

  ;; calculate the change in soil fertility - croplands loose fertility (organic carbon loss), grasslands gain fertility (organic carbon gain)
  ask fields [
    if land-use = "cropland" [
      ifelse years-since-land-use-change <= 17 ;; 17 years after converion from grassland into cropland an equilibirum is reached and soil fertility stay stable
        [set soil-fertility-factor soil-fertility-factor * 0.974] ;0.974
        [set soil-fertility-factor soil-fertility-factor]]
    ;; Source of values "17" and "0.974" (soil fertility reduction factor): Poeplau et al. 2002: Temporal dynamics of soil organic carbon after land‐use change in the temperate zone–carbon response functions as a model approach.
    if land-use = "grassland" [ ;; the fertility gain in grassalnds depends on the intensity (and respective cow density per ha)
      if my-farm-intensity = 1  [set soil-fertility-factor soil-fertility-factor * 1.015]
      if my-farm-intensity = 2  [set soil-fertility-factor soil-fertility-factor * 1.005]
      if my-farm-intensity = 3  [set soil-fertility-factor soil-fertility-factor * 1.000]
      if my-farm-intensity = 4  [set soil-fertility-factor soil-fertility-factor * 0.995]]
    ;; Source of values "1.015", "1.013", "1.010", "1.007" (soil fertility gain factors): Poeplau et al. 2002: Temporal dynamics of soil organic carbon after land‐use change in the temperate zone–carbon response functions as a model approach. AND Khalil et al. 2019
  ]


  ;; Define the area output of greenhouse gases and nitrate (needed for policy option 5)
  set GHG-area-output sum [GHG-farm-emission] of dairy-farms
  set nitrate-area-output sum [nitrate-farm-emission] of dairy-farms

  ;; Define the different indices for ecosystem services
  ;; To calculate an index between 0 (very bad) and 100 (very good) the following formulas are used
  ;; Case 1: The lower the value, the better (e.g. greenhouse gas emissions): Index-value = 100 * ((min-value - value) / (min-value - max-value))
  ;; Case 2: The higher the value the better (e.g. soil fertility): Index-value = 100 * ((value - min-value) / (max-value - min-value))
  set climate-regulation-index (100 * (min-climate-regulation - GHG-area-output) / (min-climate-regulation - max-climate-regulation))
  set water-quality-index (100 * (min-water-quality - nitrate-area-output) / (min-water-quality - max-water-quality))

  ;; habitat quality is basically only provided by grasslands, and there it depends on the intensity of use
  let area-with-high-habitat-quality (sum [ha-size] of grass-fields with [kg-N-field / ha-size <= 50 and no-of-cuts <= 1])
  ;; inspired by -> Boch et al. 2016: Extensive und jährlich wechselnde Nutzungsintensität fördert den Artenreichtum im Grünland
  let area-with-medium-habitat-quality (sum [ha-size] of grass-fields with [(kg-N-field / ha-size > 50 and kg-N-field / ha-size <= 100 and no-of-cuts <= 2) or (kg-N-field / ha-size <= 50 and no-of-cuts = 2)])
  ;; inspired by  -> Boch et al. 2016: Extensive und jährlich wechselnde Nutzungsintensität fördert den Artenreichtum im Grünland
  let area-with-low-habitat-quality (sum [ha-size] of grass-fields with [kg-N-field / ha-size > 100 or no-of-cuts > 2])
  ;; inspired by  -> Boch et al. 2016: Extensive und jährlich wechselnde Nutzungsintensität fördert den Artenreichtum im Grünland
  ;; different habitat-quality areas are weighted differently to calculate the overall habitat quality index
  set habitat-quality-index (100 * ((area-with-high-habitat-quality + 0.7 * area-with-medium-habitat-quality + 0.3 * area-with-low-habitat-quality) - min-habitat-quality) / (max-habitat-quality - min-habitat-quality))
  ;; inspired by -> Boch et al. 2016: Extensive und jährlich wechselnde Nutzungsintensität fördert den Artenreichtum im Grünland
  set soil-fertility-index (100 * (sum [soil-fertility-factor * ha-size] of fields - min-soil-fertility) / (max-soil-fertility - min-soil-fertility))
end

to calculate-interim-profit

  ;; Step 1: Calculate subsidies, leasing costs and labour input per field
  ask crop-fields [
    set received-subsidies-field (ha-size * subsidies-crop)
    set leasing-costs-field (ha-size * land-lease-ha-price-crop)
    set labour-costs-field (ha-size * 633 + kg-N-field * 0.43 + distance-farmfield * 23.1) ;; labour costs depend on ha-size, nitrogen fertilization and distance of the field from the farm
    ;; Source of values "633" (costs per ha of land)/"0.43" (labour costs per kg N applied on the field)/"23.1" (labour costs per km of distance from the farm)
       ;; -> Maschinenring Aibling-Miesbach-München e.V. 2018: Verrechnungssätze 2018, Maschinenring Aibling
  ]

  ask grass-fields [
    ifelse (kg-Norg-field / ha-size) < Norg-ext-threshold AND kgN-mineral-field = 0
      [set received-subsidies-field (ha-size * (subsidies-grass-ext + subsidies-grass))]
      [set received-subsidies-field (ha-size * subsidies-grass)]
    set leasing-costs-field (ha-size * land-lease-ha-price-grass)
      set labour-costs-field (ha-size * no-of-cuts * 125.7 + kg-N-field * 0.43 + distance-farmfield * 5.1 * no-of-cuts) ;; labour costs depend on ha-size, number of cuts, nitrogen fertilization and distance of the field from the farm
    ;; Source of values "125.7" (costs per ha of land and per cut)/"0.43" (labour costs per kg N applied on the field)/"5.1" (labour costs per km of distance from the farm per cut)
       ;; -> Maschinenring Aibling-Miesbach-München e.V. 2018: Verrechnungssätze 2018, Maschinenring Aibling
  ]

  ;; Step 2: Cacluate the balance (profit) of revenues and expenses per farm
  ask dairy-farms [
    set received-subsidies (sum [received-subsidies-field] of my-fields)
    ;; --------------------------------------------------------------------------------------------------------------------------------------------
    ;; Policy option 4: Deduct a certain amount of subsidies, if farm size exceeds the chosen threshold.
    ;; The reduction amount (also no payment at all above a certain amount of hectare can be chosen) is set via the slider "subsidy-reduction-per-ha"
      if policy-option-4-redistributive-subsidies = true
        [if sum [ha-size] of my-fields > farm-size-threshold
          [let subsidy-reduction ((subsidy-reduction-per-ha / 100) * (received-subsidies / farm-size) * (farm-size - farm-size-threshold))
           set received-subsidies received-subsidies - subsidy-reduction]
    ;; --------------------------------------------------------------------------------------------------------------------------------------------
    ]

    if received-market-value > 0 ;; Only set costs for land-lease, labour and input if there was any market-value received
    [
    set land-lease (sum [leasing-costs-field] of my-fields)
    set labour-costs (sum [labour-costs-field] of my-fields)

    ;; Current monetary investment of the farm - costs apply per hectare of land but vary with the milk output and mineral fertilizer input [€ / year]
    set input-costs farm-size * 1025 + sum [kgN-mineral-field] of my-fields * mineral-fertilizer-price +  abs (initial-avg-Norg-level - adjusted-avg-Norg-level) * farm-size * 2.4
    ;; Source of value "1025" (input costs per ha for milk volume < 100,000) and "709", "843", "1138", "910", "1709" (additional costs for more milk volume): STMELF Buchführungsergebnisse landwirtschaftlicher Betriebe in Bayern / Wirtschaftsjahr 2016/2017 / Milchviehbetrieb / alle Betriebsgrößen / Region 1
    ;; "Materialaufwand Saat- und Pflanzgut", "Materialaufwand Pflanzenschutz", "Materialaufwand Tierproduktion", "Aufwand Strom, Heizstoffe, Wasser", "Sonstige betriebliche Aufwendungen"
    ;; Source of value "2.4" (costs per kg N in traded manure) -> https://www.lksh.de/fileadmin/dokumente/Bauernblatt/PDF_Toepper_2014/BB_11_15.03/35-38_Reckleben.pdf
    if milk-volume >= 100000 and milk-volume < 150000 [set input-costs input-costs + farm-size * 709]
    if milk-volume >= 150000 and milk-volume < 200000 [set input-costs input-costs + farm-size * 843]
    if milk-volume >= 200000 and milk-volume < 250000 [set input-costs input-costs + farm-size * 1138]
    if milk-volume >= 250000 and milk-volume < 300000 [set input-costs input-costs + farm-size * 910]
    if milk-volume > 300000 [set input-costs input-costs + farm-size * 1709]
    ]
    ;; Calculation of interim profit (before policy intervention regarding fines and taxes)
    set annual-interim-profit (received-subsidies + received-market-value) - (land-lease + labour-costs + input-costs)
   ]
end

to implement-policies
  ;; --------------------------------------------------------------------------------------------------------------------------------------------
  ;; Policy option 1: Fine farms that exceed the allowed level of average organic Nitrogen per hectare on their fields (after manure trade)
  ;; The height of the fine (in percentage of the subsidies received) is set via the slider "subsidy-reduction-N-surplus"
  ;; How many farms are actually checked can be set with the slider "percentage-checked"
  ifelse policy-option-1-Norg-surplus-fine = true [
    ask n-of (percentage-checked / 100 * count dairy-farms) dairy-farms [
      ifelse adjusted-avg-Norg-level > Norg-limit
        [set Norg-surplus-fine (subsidy-reduction-Norg-surplus / 100 * received-subsidies)]
        [set Norg-surplus-fine 0]
  ]]
    [ask dairy-farms [set Norg-surplus-fine 0]]
  ;; --------------------------------------------------------------------------------------------------------------------------------------------

  ;; --------------------------------------------------------------------------------------------------------------------------------------------
  ;; Policy option 2: Ban grassland conversion for the following year (no land use change from grassland to cropland is allowed)
  ;; The ban is set "true" if the share of grassland falls below the defined threshold in "minimum-grassland-share"
  ;; more details see procedure "specify-field-use"
  ifelse policy-option-2-ban-grassland-conversion = true [
    ifelse (sum [ha-size] of grass-fields / sum [ha-size] of fields * 100) < minimum-grassland-share
      [set grassland-conversion-ban true] [set grassland-conversion-ban false]]
  [set grassland-conversion-ban false]
  ;; --------------------------------------------------------------------------------------------------------------------------------------------

  ;; Policy option 3: -> integrated in setup and "update-global-variables-start" procedure

  ;; Policy option 4:  > integrated in step 2 of the "calculate-interim-profit" procedure

  ;; --------------------------------------------------------------------------------------------------------------------------------------------
  ;; Policy option 5: Setting taxes for greenhouse gas (GHG) and nitrate emissions per dairy farm, if the area-wide threshold is exceeded ("GHG-threshold" / "Nitrate-threshold")
  ;; The height of the taxes is set via the sliders "GHG-tax" and "nitrate-tax"
  ifelse policy-option-5-emission-taxes = true [
    ask dairy-farms [
      ifelse GHG-area-output > GHG-threshold * 1000  ;; GHG-threshold is in tonnes, GHG-area-output in kg -> conversion
        [set GHG-tax-paid ((GHG-farm-emission / 1000) * GHG-tax)] ;; GHG-tax is per tonne, GHG-farm-emission in kg -> conversion
        [set GHG-tax-paid 0]

      ifelse nitrate-area-output > nitrate-threshold
        [set nitrate-tax-paid (nitrate-farm-emission * nitrate-tax)]
        [set nitrate-tax-paid 0]
  ]]
    [ask dairy-farms [
       set GHG-tax-paid 0
       set nitrate-tax-paid 0
  ]]
  ;; --------------------------------------------------------------------------------------------------------------------------------------------

  ;; Set the final annual profit after all policy measures have been implemented
  ask dairy-farms [
    set annual-profit annual-interim-profit - Norg-surplus-fine - nitrate-tax-paid - GHG-tax-paid
  ]
end

to initialize-agent-variables
  ;; initializing agent variables in the stup process

  ;; Assign a random soil-fertility-factor (from a normal distribution) between 0.4 and 1
  ask fields [
    set soil-fertility-factor 0.8 - random-normal 0 0.1
    if soil-fertility-factor > 1 [set soil-fertility-factor 1]
    if soil-fertility-factor < 0.4 [set soil-fertility-factor 0.4]

    ;; Define a random years-since-land-use-change between 0 and 30
    set years-since-land-use-change random 31

  ]

  ;; Define the farm properties
  ask dairy-farms [
    ;; Define the fields that belong to a farm (= "my-fields") and the farm size
    set my-fields fields with [tenant = [farm-id] of myself]
    set farm-size sum [ha-size] of my-fields

    ;; Define intensity of use from 1 (very-extensive) to 4 (very-intensive) (by number of cows/ha)
    if (no-of-cows / farm-size < 0.8 ) [set intensity-scale 1]
    if (no-of-cows / farm-size >= 0.8 AND no-of-cows / farm-size < 1.4) [set intensity-scale 2]
    if (no-of-cows / farm-size >= 1.4 AND no-of-cows / farm-size < 1.7) [set intensity-scale 3]
    if (no-of-cows / farm-size >= 1.7) [set intensity-scale 4]
    ask my-fields [set my-farm-intensity [intensity-scale] of myself]

    ;; Define a random years-since-intensity-change between 0 and 5 years
    set years-since-intensity-change random 6

    ;; Define the difference size classes of farms (by amount of hectare)
    if (farm-size < 20 ) [set size-class "small"]
    if (farm-size >= 20 AND farm-size < 75) [set size-class "medium"]
    if (farm-size >= 75 AND farm-size < 100) [set size-class "large"]
    if (farm-size >= 100) [set size-class "extralarge"]

    ;; for random-world: Select XX% of fields and assign cropland as land use (according to intensity scale, very extensive = 0%, extensive = 15%, intensive = 30%, very intensive = 50%)
    if (world-setup = "random-world") [
    if (intensity-scale = 2) [ask n-of (random-normal 0.15 0.025 * count my-fields) my-fields [set land-use "cropland"]]
    if (intensity-scale = 3) [ask n-of (random-normal 0.3 0.025 * count my-fields) my-fields [set land-use "cropland"]]
    if (intensity-scale = 4) [ask n-of (random-normal 0.5 0.025 * count my-fields) my-fields [set land-use "cropland"]]
    ]
    set my-grassland-share ((sum [ha-size] of my-fields with [land-use = "grassland"]) / sum [ha-size] of my-fields)

    ;; Define the distance between a farm and its fields (= "distance-farmfield")
    ask my-fields [
      ifelse world-setup = "random-world"
      [set distance-farmfield (0.071 * distance one-of dairy-farms with [farm-id = [tenant] of myself])] ;; GHG-threshold is in tonnes, GHG-area-output in kg -> conversion
      [set distance-farmfield (0.13 * distance-nowrap one-of dairy-farms with [farm-id = [tenant] of myself])]] ;; Factor 0.13 = conversion factor for the real distances in gis world [km]
    ]

  ;; Define the two different land use types (cropland / grassland) as agentsets
  set crop-fields fields with [land-use = "cropland"]
  set grass-fields fields with [land-use = "grassland"]

end

to initialize-global-variables

  ;; Set the initial market value for the two milk types
  set market-value-milk-int 0.31
  set market-value-milk-ext 0.48
  ;; Average values for conventional and organic milk for 2015 in Bavaria - those are set as initial values for milk from intensive and extensive dairy farms respectively
  ;; https://www.lfl.bayern.de/mam/cms07/publikationen/daten/informationen/statistik-bayerische-milchwirtschaft-2015_lfl-information.pdf (Table 16)

  ;; Set the inital market price for mineral fertilizer
  set mineral-fertilizer-price 0.234
  ;; Source of value "0.234" (costs in € per kg of mineral fertilizer) -> https://markt.agrarheute.com/duengemittel-4/stickstoffduenger-20

  ;; Set the default Norg limit if policy option 1 is not applied
  set Norg-limit 170

  ;; Set the initial value for the subsidies
  set subsidies-crop 300
  set subsidies-grass 300
  set Norg-ext-threshold 110
  set subsidies-grass-ext 0  ; if policy option 3 is turned off, there are no extra payments for extensive grasslands

    ;; For Policy 3: Set the current subsidy values according to the value of the sliders in the interface
  if policy-option-3-choose-subsidy-heights = true
    [
    set subsidies-crop subsidies-crop-FIT  ; standard value 300
    set subsidies-grass subsidies-grass-FIT  ; standard value 300
    set Norg-ext-threshold Norg-ext-threshold-FIT ; standard value 110
    set subsidies-grass-ext subsidies-grass-extra-FIT   ; standard value 350
  ]
  ;; Source of values "300" and "300" -> http://www.stmelf.bayern.de/agrarpolitik/foerderung/000958/index.php
  ;; Source of value "350" -> http://www.stmelf.bayern.de/agrarpolitik/foerderung/000958/index.php
     ;; and: https://www.stmelf.bayern.de/mam/cms01/agrarpolitik/dateien/massnahmenuebersicht_kulap.pdf

  ;; Set the values for the lease per hectare of cropland and grassland
  set land-lease-ha-price-crop 396
  set land-lease-ha-price-grass 221
  ;; Source of values "396" and "221" -> https://www.statistik.bayern.de/presse/archiv/142_2017.php

  ;; Set the different ecosystem services indices to 100 (maximum) as a start value
  set climate-regulation-index 100
  set water-quality-index 100
  set habitat-quality-index 100
  set soil-fertility-index 100

  ;; -------------------------------------------------------------------------------------------------------------------------------------------------
  ;; Define the best (max) values for the different ecosystem services (to be able to set the indices in the "influence-ecosystem-services")
  ;; The assumption for the calculation is, that in the best scenario, all land is used as extensive grassland, there is a livestock density of 0.4 cows per hectare,
  ;; there are no mineral fertilizer applications, grass is only cut once per year (important for max-habitat-quality) and stays extensive grassland for a period of 30 years (important for max-soil-fertility).
  ;; As the N2O emission factors are lower for cropland than for grassland, the lowest N2O emission possible is calculated with all land being used as cropland

  ;; Calculate best value for climate regulation
  let CH4_max (0.4 * sum [ha-size] of fields * 93.7)
  ;; Source of value "93.7" -> Ellis et al. 2007:  Prediction of methane production from dairy and beef cattle
  let average-Norg-level 0.4 * 25.4 * 4.2  ;; Livestock density per hectare * manure produced per cow * factor for N content of manure
  let N2O_lowest sum [ha-size] of fields * average-Norg-level * 0.0117  ;; Organic N leads to an emission of N2O from grassland
  ;; Source of value "0.0117" -> Dobbie et al. 1999: Nitrous oxide emissions from intensive agricultural systems: variations between crops and seasons, key driving variables, and mean emission factors
  let CO2minus sum [ha-size] of fields * 520     ;; Grassland is on average a net sink of CO2
  ;; Source of value "520" -> Vleeshouwers & Verhagen 2002: Carbon emission and sequestration by agricultural land use: a model study for Europe
  set max-climate-regulation (CH4_max * 25 + N2O_lowest * 298 - CO2minus)
  ;; Source of values "25" and "298" -> IPCC 2007

  ;; Calculate best value for water quality
  set max-water-quality (sum [ha-size] of fields * average-Norg-level * 0.09)
  ;; Source of value "0.09" -> Di & Cameron: Nitrate leaching in temperate agroecosystems: sources, factors and mitigating strategies

  ;; Calculate best value for habitat quality
  set max-habitat-quality sum [ha-size] of fields

  ;; Calculate best value for soil fertility
  set max-soil-fertility (sum [soil-fertility-factor * ha-size] of fields * 1.015 ^ 30) ;; For 30 years the soil fertility is increased by 1.5%
  ;; Source of value "1.015": Poeplau et al. 2002: Temporal dynamics of soil organic carbon after land‐use change in the temperate zone–carbon response functions as a model approach.

  ;; -------------------------------------------------------------------------------------------------------------------------------------------------

  ;; -------------------------------------------------------------------------------------------------------------------------------------------------
  ;; Define the worst (min) values for the different ecosystem services (to be able to set the indices in the "influence-ecosystem-services")
  ;; The assumption for the calculation is, that in the worst scenario, all land is used as intensive cropland, there is a livestock density of 2.5 cows per hectare,
  ;; an average of 300 kg / ha of mineral fertilizer is applied and stays cropland for a period of 30 years (important for min-soil-fertility).
  ;; As the N2O emission factors are higher for grassland than for cropland, the highest N2O emission possible is calculated with all land being used as grassland

  ;; Calculate worst value for climate regulation
  let CH4_min (2.5 * sum [ha-size] of fields * 93.7)
  ;; Source of value "93.7" -> Ellis et al. 2007:  Prediction of methane production from dairy and beef cattle
  let N2O_highest sum [ha-size] of fields * 300 * 0.0204  ;; Mineral fertilizers are used up to 300kg N per hectare. This leads to an emission of N2O from cropland
  ;; Source of value "0.0204" -> Dobbie et al. 1999: Nitrous oxide emissions from intensive agricultural systems: variations between crops and seasons, key driving variables, and mean emission factors
  let CO2plus sum [ha-size] of fields * 840  ;; Cropland is on average a net emitter of CO2
  ;; Source of value "840" -> Vleeshouwers & Verhagen 2002: Carbon emission and sequestration by agricultural land use: a model study for Europe
  set min-climate-regulation (CH4_min * 25 + N2O_highest * 298 + CO2plus)
  ;; Source of value "25" and "298" -> IPCC 2007

  ;; Calculate worst value for water quality
  let average-N-level 2.5 * 25.4 * 4.2 + 300 ;; Livestock density per hectare * manure produced per cow * factor for N content of manure + 300 kg of mineral N
  set min-water-quality (sum [ha-size] of fields * average-N-level * 0.225)
  ;; Source of value "0.225" -> Di & Cameron: Nitrate leaching in temperate agroecosystems: sources, factors and mitigating strategies

  ;; Calculate worst value for habitat quality
  set min-habitat-quality 0

  ;; Calculate worst value for soil fertility
  set min-soil-fertility (sum [soil-fertility-factor * ha-size] of fields * 0.974 ^ 17) ;; For 17 years the soil fertility is decreased by 1.5%
  ;; Source of value "0.974": Poeplau et al. 2002: Temporal dynamics of soil organic carbon after land‐use change in the temperate zone–carbon response functions as a model approach.
  ;; -------------------------------------------------------------------------------------------------------------------------------------------------
end

to update-global-variables-start

  set manure-pool 0

  ;; Set the new market value for the two milk types = old market value +/- market fluctuation (random process)
  let market-fluctuation-1 ((100 + random-normal 0 max-market-stochasticity) / 100)          ;; Define market fluctuation for milk from farms with intensive management
  let market-fluctuation-2 (market-fluctuation-1 - ((market-fluctuation-1 - 1) * 0.75))      ;; Define market fluctuation for milk from farms with extensive management (only 25% of fluctuation)
  set market-value-milk-int (market-value-milk-int * market-fluctuation-1)
  set market-value-milk-ext (market-value-milk-ext * market-fluctuation-2)
  ;; Because of the higher fluctuation of the market value for milk from farms with intensive management, it is possible that a higher price is set for "market-value-milk-int" than for "market-value-milk-ext".
  ;; To avoid this, the market value is set to the value of "market-value-milk-ext" in these cases.
  if market-value-milk-int > market-value-milk-ext
    [set market-value-milk-int market-value-milk-ext]
  ;; Set the new market price for mineral fertilizer = old price +/- market fluctuation (random process)
  set mineral-fertilizer-price (mineral-fertilizer-price * (100 + random-normal 1.1 max-market-stochasticity) / 100)

  ;; For Policy 1: Set the Norg-limit value from the default 170 to the value set by the observer (via the slider Norg-limit-FIT)
  if policy-option-1-Norg-surplus-fine = true [
    set Norg-limit Norg-limit-FIT
  ]

  ;; For Policy 3: Set the current subsidy values according to the value of the sliders in the interface
  if policy-option-3-choose-subsidy-heights = true
    [
    set subsidies-crop subsidies-crop-FIT
    set subsidies-grass subsidies-grass-FIT
    set Norg-ext-threshold Norg-ext-threshold-FIT
    set subsidies-grass-ext subsidies-grass-extra-FIT
  ]

end

to update-agent-variables-start

  ask fields [
    set for-lease? false    ;; Delete values from "previous year" (needed in "interact-on-land-market")
    set new-tenant? false
    set years-since-land-use-change years-since-land-use-change + 1
  ]

  ask dairy-farms [         ;; Advance one year for "years-since-x-change" counters (needed in "decide-farm-strategy")
    set years-since-intensity-change years-since-intensity-change + 1

    set Norg-surplus-fine 0

    if (ticks > 1) [ ;; increase the low-profit-years counter by +1 (starting from the 2nd tick on)
    ifelse annual-profit <= minimum-profit
      [set low-profit-years low-profit-years + 1 ]
      [set low-profit-years 0]
    ]
  ]

end

to update-global-variables-end

  ;; Calculate the mean annual profit per hectare for each farm intensity class
  if (count (dairy-farms with [intensity-scale = 4]) > 0 ) [
    set profit-very-intensive sum [annual-profit] of dairy-farms with [intensity-scale = 4] / sum [farm-size] of dairy-farms with [intensity-scale = 4]]
  if (count (dairy-farms with [intensity-scale = 3]) > 0 ) [
    set profit-intensive sum [annual-profit] of dairy-farms with [intensity-scale = 3] / sum [farm-size] of dairy-farms with [intensity-scale = 3]]
  if (count (dairy-farms with [intensity-scale = 2]) > 0 ) [
    set profit-extensive sum [annual-profit] of dairy-farms with [intensity-scale = 2] / sum [farm-size] of dairy-farms with [intensity-scale = 2]]
  if (count (dairy-farms with [intensity-scale = 1]) > 0 ) [
    set profit-very-extensive sum [annual-profit] of dairy-farms with [intensity-scale = 1] / sum [farm-size] of dairy-farms with [intensity-scale = 1]]

  ;; Calculate the mean annual profit per farm for each size class
  if (count (dairy-farms with [size-class ="small"]) > 0 ) [
    set profit-size-small sum [annual-profit] of dairy-farms with [size-class ="small"] / count dairy-farms with [size-class ="small"]]
  if (count (dairy-farms with [size-class ="medium"]) > 0 ) [
    set profit-size-medium sum [annual-profit] of dairy-farms with [size-class ="medium"] / count dairy-farms with [size-class ="medium"]]
  if (count (dairy-farms with [size-class ="large"]) > 0 ) [
    set profit-size-large sum [annual-profit] of dairy-farms with [size-class ="large"] / count dairy-farms with [size-class ="large"]]
  if (count (dairy-farms with [size-class ="extralarge"]) > 0 ) [
    set profit-size-extralarge sum [annual-profit] of dairy-farms with [size-class ="extralarge"] / count dairy-farms with [size-class ="extralarge"]]
end

to update-world

  if (world-setup = "random-world") [
    ;; Give each patch with a grass-field (turtle) on top a shade of green depending on the intensity of its farm
    ask grass-fields [set pcolor scale-color green (my-farm-intensity * -1) -10 0]
    ;; Give each patch with a crop-field (turtle) on top a shade of brown depending on the intensity of its farm
    ask crop-fields [set pcolor scale-color brown (my-farm-intensity * -1) -8 0]
    ask fields with [tenant = -1] [set pcolor black]
  ]

  if (world-setup = "gis-world") [
    ask fields[
         gis:set-drawing-color black  gis:fill  item (field-id - 1) gis:feature-list-of field-data 2.0
         ;; Give each field (gis feature) a shade of green depending on the intensity of its farm
         if land-use = "grassland" [ gis:set-drawing-color scale-color green (my-farm-intensity * -1) -10 2  gis:fill  item (field-id - 1) gis:feature-list-of field-data 2.0]
         ;; Give each field (gis feature) a shade of brown depending on the intensity of its farm
         if land-use = "cropland" [ gis:set-drawing-color scale-color brown (my-farm-intensity * -1) -8 0 gis:fill  item (field-id - 1) gis:feature-list-of field-data 2.0]
         if tenant = -1 [ gis:set-drawing-color black gis:fill item (field-id - 1) gis:feature-list-of field-data 2.0]
    ]
  ]

  ask dairy-farms [
    ;; Set the size of dairy-farm agents into proportion of its farm-size
    set size 3 + farm-size * 0.04

    ;; Set the color of dairy-farm agents differently per intensity scale of this farm (matching the chosen color in plots for this scale)
    if intensity-scale = 1 [set color 66]
    if intensity-scale = 2 [set color 86]
    if intensity-scale = 3 [set color 106]
    if intensity-scale = 4 [set color 126]
  ]

end
@#$#@#$#@
GRAPHICS-WINDOW
49
10
932
894
-1
-1
8.6634
1
10
1
1
1
0
1
1
1
-50
50
-50
50
1
1
1
ticks
30.0

BUTTON
122
988
177
1021
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
1

BUTTON
54
988
115
1021
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

PLOT
954
765
1464
947
Intended size change
Years
Farms [%]
0.0
30.0
0.0
100.0
true
true
"" ""
PENS
"expand" 1.0 0 -16777216 true "" "plot count dairy-farms with [intended-farm-size-change = 10] / count dairy-farms * 100"
"downsize" 1.0 0 -7500403 true "" "plot count dairy-farms with [intended-farm-size-change = -10] / count dairy-farms * 100"

BUTTON
54
1032
176
1065
1 year step
go
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

PLOT
954
955
1464
1135
Land exchange market
Years
No. of fields
0.0
30.0
0.0
10.0
true
true
"" ""
PENS
"exchanged" 1.0 0 -16777216 true "" "plot count fields with [new-tenant? = true]"
"offered" 1.0 0 -7500403 true "" "plot count fields with [for-lease? = true] + count fields with [new-tenant? = true]"

PLOT
1480
957
2015
1132
Farm size distribution
Farm size [ha]
No. of farms
0.0
150.0
0.0
10.0
true
false
"" ""
PENS
"default" 10.0 1 -16777216 true "" "histogram [farm-size] of dairy-farms"

PLOT
1477
584
2015
755
Farm profit distribution [€/ year]
Annual profit
No. of farms
-100000.0
500000.0
0.0
10.0
true
false
"" ""
PENS
"default" 50000.0 1 -16777216 true "" "histogram [annual-profit] of dairy-farms"

SLIDER
354
966
592
999
Norg-limit-FIT
Norg-limit-FIT
100
220
100.0
10
1
kg / ha
HORIZONTAL

PLOT
2034
767
2550
942
Initial Norg level (produced manure)
N-org [kg / ha]
No. of farms
0.0
300.0
0.0
25.0
true
true
"" ""
PENS
"N-org produced" 15.0 1 -16777216 true "" "histogram [initial-avg-Norg-level] of dairy-farms"
"N-org limit" 2.0 1 -2674135 true "" "histogram (list Norg-limit Norg-limit Norg-limit Norg-limit Norg-limit Norg-limit Norg-limit Norg-limit Norg-limit Norg-limit Norg-limit Norg-limit Norg-limit Norg-limit Norg-limit Norg-limit Norg-limit Norg-limit Norg-limit Norg-limit Norg-limit Norg-limit Norg-limit Norg-limit Norg-limit Norg-limit Norg-limit)"

PLOT
2032
957
2574
1149
Adjusted Norg level (after exchange)
N-org [kg / ha]
No. of farms
0.0
200.0
0.0
10.0
true
true
"" ""
PENS
"N-org applied" 15.0 1 -16777216 true "" "histogram [adjusted-avg-Norg-level] of dairy-farms"
"N-org limit" 2.0 1 -2674135 true "" "histogram (list Norg-limit Norg-limit Norg-limit Norg-limit Norg-limit Norg-limit Norg-limit Norg-limit Norg-limit Norg-limit Norg-limit Norg-limit Norg-limit Norg-limit Norg-limit Norg-limit Norg-limit Norg-limit Norg-limit Norg-limit Norg-limit Norg-limit Norg-limit Norg-limit Norg-limit Norg-limit Norg-limit)"

MONITOR
943
16
1075
61
No. of farms
count dairy-farms
17
1
11

MONITOR
1085
79
1187
124
No. of cows
sum [no-of-cows] of dairy-farms
17
1
11

PLOT
1477
394
2013
573
Producer prices for milk
Years
Price [€/kg]
0.0
30.0
0.0
0.7
true
true
"" ""
PENS
"milk int" 1.0 0 -12345184 true "" "plot market-value-milk-int"
"milk ext" 1.0 0 -6759204 true "" "plot market-value-milk-ext"

PLOT
1479
12
2012
188
Subsidy level
Years
[€/ year]
0.0
30.0
0.0
10.0
true
true
"" ""
PENS
"crop" 1.0 0 -5207188 true "" "plot subsidies-crop"
"grass-ext" 1.0 0 -4399183 true "" "plot subsidies-grass + subsidies-grass-ext"
"grass-int" 1.0 0 -10899396 true "" "plot subsidies-grass"

SLIDER
48
1098
290
1131
max-market-stochasticity
max-market-stochasticity
0
15
1.0
1
1
SD
HORIZONTAL

CHOOSER
54
930
293
975
world-setup
world-setup
"random-world" "gis-world"
1

MONITOR
1084
15
1189
60
avg ha per farm
sum [farm-size] of dairy-farms / count dairy-farms
1
1
11

MONITOR
1217
79
1349
124
No. of grass fields
count grass-fields
17
1
11

MONITOR
1218
12
1349
57
No. of crop fields
count crop-fields
17
1
11

SLIDER
356
1002
593
1035
subsidy-reduction-Norg-surplus
subsidy-reduction-Norg-surplus
1
30
19.0
1
1
%
HORIZONTAL

MONITOR
943
78
1075
123
cow density per farm
sum [no-of-cows] of dairy-farms / count dairy-farms
1
1
11

MONITOR
945
144
1189
189
cow density per ha
sum [no-of-cows] of dairy-farms / sum [farm-size] of dairy-farms
1
1
11

PLOT
1477
204
2014
385
Fertilizer prices
Years
Price [€/kg]
0.0
30.0
0.0
1.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot mineral-fertilizer-price"

SLIDER
622
1158
890
1191
subsidies-crop-FIT
subsidies-crop-FIT
0
600
95.0
5
1
€
HORIZONTAL

SLIDER
622
1196
890
1229
subsidies-grass-FIT
subsidies-grass-FIT
0
600
95.0
5
1
€
HORIZONTAL

SLIDER
624
1272
891
1305
subsidies-grass-extra-FIT
subsidies-grass-extra-FIT
0
1000
0.0
5
1
€
HORIZONTAL

SLIDER
624
1232
891
1265
Norg-ext-threshold-FIT
Norg-ext-threshold-FIT
0
160
110.0
10
1
kg Norg / ha
HORIZONTAL

PLOT
950
204
1461
385
Annual profit per ha
Years
Profit [€/ ha]
0.0
30.0
0.0
10.0
true
true
"" ""
PENS
"very intensive" 1.0 0 -4699768 true "" "if (ticks > 0) [plot profit-very-intensive]"
"intensive" 1.0 0 -10649926 true "" "if (ticks > 0) [plot profit-intensive]"
"extensive" 1.0 0 -8990512 true "" "if (ticks > 0) [plot profit-extensive]"
"very extensive" 1.0 0 -11085214 true "" "if (ticks > 0) [plot profit-very-extensive]"

PLOT
952
585
1466
755
Farm intensity distribution
Years
Farms [%]
0.0
30.0
0.0
10.0
true
true
"" ""
PENS
"very intensive" 1.0 0 -4699768 true "" "plot count dairy-farms with [intensity-scale = 4] / count dairy-farms * 100"
"intensive" 1.0 0 -10649926 true "" "plot count dairy-farms with [intensity-scale = 3] / count dairy-farms * 100"
"extensive" 1.0 0 -8990512 true "" "plot count dairy-farms with [intensity-scale = 2] / count dairy-farms * 100"
"very extensive" 1.0 0 -11085214 true "" "plot count dairy-farms with [intensity-scale = 1] / count dairy-farms * 100"

PLOT
949
394
1463
575
Annual profit
Years
Profit [€]
0.0
30.0
0.0
10.0
true
true
"" ""
PENS
"small" 1.0 0 -1604481 true "" "if (ticks > 0) [plot profit-size-small]"
"medium" 1.0 0 -2674135 true "" "if (ticks > 0) [plot profit-size-medium]"
"large" 1.0 0 -8053223 true "" "if (ticks > 0) [plot profit-size-large]"
"extra large" 1.0 0 -13628663 true "" "if (ticks > 0) [plot profit-size-extralarge]"

SLIDER
46
1234
289
1267
low-profit-years-needed
low-profit-years-needed
1
10
5.0
1
1
years
HORIZONTAL

PLOT
1477
767
2013
945
Farm size class distribution
Years
Farms [%]
0.0
30.0
0.0
10.0
true
true
"" ""
PENS
"small" 1.0 0 -1604481 true "" "plot count dairy-farms with [size-class = \"small\"] / count dairy-farms * 100"
"medium" 1.0 0 -2674135 true "" "plot count dairy-farms with [size-class = \"medium\"] / count dairy-farms * 100"
"large" 1.0 0 -8053223 true "" "plot count dairy-farms with [size-class = \"large\"] / count dairy-farms * 100"
"extra large" 1.0 0 -13628663 true "" "plot count dairy-farms with [size-class = \"extralarge\"] / count dairy-farms * 100"

PLOT
2035
395
2267
572
ES water quality
Years
Index
0.0
30.0
0.0
100.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "if (ticks > 0) [plot water-quality-index]"

PLOT
2035
205
2555
379
ES climate regulation
Years
Index
0.0
30.0
0.0
100.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "if (ticks > 0) [plot climate-regulation-index]"

SLIDER
356
1042
593
1075
percentage-checked
percentage-checked
1
100
1.0
1
1
%
HORIZONTAL

PLOT
2277
13
2550
187
Government spendings
Years
[€/ year]
0.0
30.0
0.0
10.0
true
true
"" ""
PENS
"Net balance" 1.0 0 -12895429 true "" "plot sum [received-subsidies] of dairy-farms - sum [nitrate-tax-paid] of dairy-farms - sum [GHG-tax-paid] of dairy-farms - sum [Norg-surplus-fine] of dairy-farms"
"Subsidies" 1.0 0 -2139308 true "" "plot sum [received-subsidies] of dairy-farms"

SWITCH
306
928
593
961
policy-option-1-Norg-surplus-fine
policy-option-1-Norg-surplus-fine
0
1
-1000

SWITCH
304
1232
591
1265
policy-option-2-ban-grassland-conversion
policy-option-2-ban-grassland-conversion
1
1
-1000

SLIDER
352
1272
591
1305
minimum-grassland-share
minimum-grassland-share
51
100
80.0
1
1
%
HORIZONTAL

SWITCH
604
1122
891
1155
policy-option-3-choose-subsidy-heights
policy-option-3-choose-subsidy-heights
1
1
-1000

SWITCH
300
1120
587
1153
policy-option-4-redistributive-subsidies
policy-option-4-redistributive-subsidies
1
1
-1000

SLIDER
348
1156
589
1189
farm-size-threshold
farm-size-threshold
20
100
45.0
5
1
ha
HORIZONTAL

SLIDER
348
1192
589
1225
subsidy-reduction-per-ha
subsidy-reduction-per-ha
5
100
30.0
5
1
%
HORIZONTAL

SWITCH
602
928
889
961
policy-option-5-emission-taxes
policy-option-5-emission-taxes
0
1
-1000

SLIDER
618
966
888
999
GHG-threshold
GHG-threshold
0
35000
17000.0
1000
1
t (entire area)
HORIZONTAL

SLIDER
620
1002
889
1035
GHG-tax
GHG-tax
1
500
42.0
1
1
€ / t
HORIZONTAL

SLIDER
618
1042
888
1075
Nitrate-threshold
Nitrate-threshold
0
650000
27491.0
1
1
kg (entire area)
HORIZONTAL

SLIDER
618
1080
889
1113
Nitrate-tax
Nitrate-tax
0
5
2.0
0.5
1
€ / kg
HORIZONTAL

PLOT
2285
585
2550
753
ES habitat provisioning
Years
Index
0.0
30.0
0.0
100.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "if (ticks > 0) [plot habitat-quality-index]"

PLOT
2284
395
2556
567
ES food provisioning
Years
[kg/ year]
0.0
30.0
0.0
10.0
true
true
"" ""
PENS
"milk int" 1.0 0 -12345184 true "" "if (ticks > 0) [plot sum [milk-volume] of dairy-farms with [intensity-scale = 3 OR intensity-scale = 4]] "
"milk ext" 1.0 0 -6759204 true "" "if (ticks > 0) [plot sum [milk-volume] of dairy-farms with [intensity-scale = 1 OR intensity-scale = 2]] "
"all milk" 1.0 0 -7500403 true "" "if (ticks > 0) [plot sum [milk-volume] of dairy-farms]"

PLOT
2035
585
2270
753
ES soil fertility
Years
Index
0.0
30.0
0.0
100.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "if (ticks > 0) [plot soil-fertility-index]"

MONITOR
1359
77
1461
122
% grassland
round (count grass-fields / count fields * 100)
17
1
11

MONITOR
1359
10
1463
55
% cropland
round (count crop-fields / count fields * 100)
17
1
11

PLOT
2033
12
2265
190
Government earnings
Years
[€/ year]
0.0
30.0
0.0
10.0
true
true
"" ""
PENS
"Norg fine" 1.0 0 -12440034 true "" "plot sum [Norg-surplus-fine] of dairy-farms"
"GHG tax" 1.0 0 -4539718 true "" "plot sum [GHG-tax-paid] of dairy-farms"
"Nitrate tax" 1.0 0 -13345367 true "" "plot sum [nitrate-tax-paid] of dairy-farms"

MONITOR
1217
144
1459
189
% extensive grassland
round (count grass-fields with [(kg-Norg-field / ha-size) < Norg-ext-threshold ]/ count fields * 100)
17
1
11

SLIDER
46
1272
289
1305
minimum-profit
minimum-profit
5000
20000
8000.0
100
1
€
HORIZONTAL

SLIDER
48
1136
291
1169
accepted-share-of-best-strategy
accepted-share-of-best-strategy
50
100
75.0
5
1
%
HORIZONTAL

MONITOR
1929
605
2006
650
mean profit
mean [annual-profit] of dairy-farms
0
1
11

MONITOR
1770
605
1922
650
subsidies' share [%]
mean [received-subsidies] of dairy-farms / mean [annual-profit] of dairy-farms * 100
0
1
11

CHOOSER
48
1178
288
1223
waiting-time-for-intensity-change
waiting-time-for-intensity-change
0 3 5
1

@#$#@#$#@
## WHAT IS IT?

This agent-based model simulates farmers’ decision-making in subalpine Bavaria regarding land use and associated effects on ecosystem services. The model represents a simplified world of cows in which farms only aim to produce milk. It is a land-use (change) model in which users can asses the potential effects of policies and markets on management practices and associated impacts on ecosystem services. 

The model exists in different versions. Before setting up the world, the user can decide which world setup should be investigated. The world setup is either based on a random allocation of fields, farms, and numbers of cows per farm (“random-world”) or based on a “gis-world”. The “gis-world” includes fields as polygon shapes typical for a region in Upper Bavaria. The associated data on ownership and cow farms are randomly assigned based on a typical distribution for the area.

The model was developed for a course on Land Use Policies, Markets & Ecosystems. It is designed for Master students to learn about agent-based modeling to assess the impacts of markets and policies on land use and ecosystem services. 

## HOW IT WORKS

The model runs for 30 years. Each year, dairy farms make a set of decisions influencing land use and ecosystem services.Decisions about farm management are taken in two different, independent steps. One decision is to change the farm’s size, and another is to change the farm’s intensity. The farm’s decisions are based on the profit distribution of farms in the previous year. In case a decision to change is taken, the farm will imitate the behavior of its most successful colleagues by implementing a strategy closer to the most profitable strategy of the past year (“best-size” or “best-intensity”) 
Based on the strategy (size or intensity) decision, the farms try to sell/lease land on the land market, buy/sell manure at the manure market, and add/reduce the number of cows. With the intensity assigned to a farm, the share of grassland and cropland, respectively, are also defined (and possibly changed).

A further set of variables included in the model are five policy options that the user can set and adjust via the interface. The user chooses to implement (one, several, or all) of the following: 

(i)   set financial punishment for exceeding the allowed limit of organic nitrogen application per hectare, 
(ii)  restrict the farms in actions (ban grassland to cropland conversion), 
(iii) control the amount of paid subsidies per hectare based on land use and applied organic nitrogen,
(iv)  set redistributive subsidies by reducing subsidies by a certain percentage for large farm sizes, and 
(iv)  set taxes for greenhouse gas and nitrate emissions. 

Farms stop operating when they run out of fields or have an insufficient income for several consecutive years. The number of farms, the number of cattle per farm, the size of the farm, the profit of each farm, and the most profitable strategies are all subject to change throughout a model run and can be observed in plots and monitors.

## HOW TO USE IT

1.	Decide on the world setup (“gis-world” or “random-world”). 
2.	Press the „setup“ button.
3.	Press the “go” button to run the model OR press the “1 year step” button to run the model for one year.
4.	You can make the following adjustments:
	a) Decide to turn on/ turn off the five different policy options and adjust thresholds and parameters in the sliders
  	b) Change the market stochasticity
  	c) Set the criteria for when farms give up because of too little income (“low-profit-years-needed” & “minimum-profit”)
  	d) Regarding the farm’s strategic land use intensity decision, there are two options (“waiting-time-for-intensity-change” ):
    		- Intensity change is allowed every year (waiting time is 0)
    		- Farms have to wait five years (since the last intensity change) until they can change their intensity (stressing that this is a longer-term strategic decision) 
  	e) Setting the percentage of the annual profit of the best strategy a farm has to fall below to change its strategy: “perc-best-strategy”)

View the plots and the graphical user interface to observe the emerging patterns.

## THINGS TO NOTICE

What happens to the distribution of farm types in the combination of different policy options?
What happens to the number of farms, the government expenditures, and the ecosystem services depending on the chosen combination of policy options?

## THINGS TO TRY

Try different combinations of policy options. Firstly, try only to change single policy options and their respective settings. Then try to change multiple ones, one by one. What patterns emerge in the graphical user interface? What patterns emerge in the plots?
Which parameter combination helps to improve ecosystem services?

## EXTENDING THE MODEL

We are waiting for your suggestions ...

## NETLOGO FEATURES

This model uses the NetLogo ‘GIS’ extension 

## RELATED MODELS



## CREDITS AND REFERENCES

The model was developed by Maria Haensel, Thomas Schmitt, and Jakob Bogenreuther

For further details, see:
Haensel M., Schmitt T. M. & Bogenreuther J (2023) Teaching the Modeling of Human–Environment Systems: Acknowledging Complexity with an Agent-Based Model. Journal of Science Education and Technology
(Supplementary information contains a full ODD+D protocol)
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

person farmer
false
0
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Polygon -1 true false 60 195 90 210 114 154 120 195 180 195 187 157 210 210 240 195 195 90 165 90 150 105 150 150 135 90 105 90
Circle -7500403 true true 110 5 80
Rectangle -7500403 true true 127 79 172 94
Polygon -13345367 true false 120 90 120 180 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 180 90 172 89 165 135 135 135 127 90
Polygon -6459832 true false 116 4 113 21 71 33 71 40 109 48 117 34 144 27 180 26 188 36 224 23 222 14 178 16 167 0
Line -16777216 false 225 90 270 90
Line -16777216 false 225 15 225 90
Line -16777216 false 270 15 270 90
Line -16777216 false 247 15 247 90
Rectangle -6459832 true false 240 90 255 300

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
NetLogo 6.3.0
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="experiment" repetitions="100" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>count dairy-farms</metric>
    <metric>count dairy-farms with [size-class = "large" OR size-class = "extralarge"]</metric>
    <enumeratedValueSet variable="policy-option-1-Norg-surplus-fine">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="perc-best-strategy">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidy-reduction-Norg-surplus">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="farmers-decision-style">
      <value value="&quot;random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Nitrate-threshold">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="farm-size-threshold">
      <value value="25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Norg-ext-threshold">
      <value value="140"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy-option-5-emission-taxes">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="low-profit-years-needed">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidies-crop-FIT">
      <value value="230"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-years-since-intensity-change">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="GHG-tax">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="minimum-profit">
      <value value="12000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Norg-limit">
      <value value="120"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy-option-3-choose-subsidy-heights">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="world-setup">
      <value value="&quot;gis-world&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidies-grass-ext-FIT">
      <value value="325"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="GHG-threshold">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Nitrate-tax">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy-option-4-redistributive-subsidies">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percentage-checked">
      <value value="100"/>
    </enumeratedValueSet>
    <steppedValueSet variable="min-years-since-size-change" first="0" step="1" last="5"/>
    <enumeratedValueSet variable="max-market-stochasticity">
      <value value="8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="minimum-grassland-share">
      <value value="88"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reduced-per-ha-payments">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy-option-2-ban-grassland-conversion">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidies-grass-int-FIT">
      <value value="295"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Calibration min-years-since-size-change" repetitions="100" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>count dairy-farms</metric>
    <metric>count dairy-farms with [size-class = "large" OR size-class = "extralarge"]</metric>
    <enumeratedValueSet variable="policy-option-1-Norg-surplus-fine">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="perc-best-strategy">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidy-reduction-Norg-surplus">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="farmers-decision-style">
      <value value="&quot;random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Nitrate-threshold">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="farm-size-threshold">
      <value value="25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Norg-ext-threshold">
      <value value="140"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy-option-5-emission-taxes">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="low-profit-years-needed">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidies-crop-FIT">
      <value value="230"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-years-since-intensity-change">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="GHG-tax">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="minimum-profit">
      <value value="12000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Norg-limit">
      <value value="170"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy-option-3-choose-subsidy-heights">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="world-setup">
      <value value="&quot;gis-world&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidies-grass-ext-FIT">
      <value value="325"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="GHG-threshold">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Nitrate-tax">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy-option-4-redistributive-subsidies">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percentage-checked">
      <value value="100"/>
    </enumeratedValueSet>
    <steppedValueSet variable="min-years-since-size-change" first="0" step="1" last="10"/>
    <enumeratedValueSet variable="max-market-stochasticity">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="minimum-grassland-share">
      <value value="88"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reduced-per-ha-payments">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy-option-2-ban-grassland-conversion">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidies-grass-int-FIT">
      <value value="295"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Calibration minimum-profit" repetitions="10" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>count dairy-farms</metric>
    <metric>count dairy-farms with [size-class = "large" OR size-class = "extralarge"]</metric>
    <enumeratedValueSet variable="policy-option-1-Norg-surplus-fine">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="perc-best-strategy">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidy-reduction-Norg-surplus">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="farmers-decision-style">
      <value value="&quot;random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Nitrate-threshold">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="farm-size-threshold">
      <value value="25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Norg-ext-threshold">
      <value value="140"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy-option-5-emission-taxes">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="low-profit-years-needed">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidies-crop-FIT">
      <value value="230"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-years-since-intensity-change">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="GHG-tax">
      <value value="50"/>
    </enumeratedValueSet>
    <steppedValueSet variable="minimum-profit" first="10000" step="1000" last="20000"/>
    <enumeratedValueSet variable="Norg-limit">
      <value value="170"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy-option-3-choose-subsidy-heights">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="world-setup">
      <value value="&quot;gis-world&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidies-grass-ext-FIT">
      <value value="325"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="GHG-threshold">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Nitrate-tax">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy-option-4-redistributive-subsidies">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percentage-checked">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-years-since-size-change">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-market-stochasticity">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="minimum-grassland-share">
      <value value="88"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reduced-per-ha-payments">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy-option-2-ban-grassland-conversion">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidies-grass-int-FIT">
      <value value="295"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Calibration minimum-profit - 2" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>count dairy-farms</metric>
    <metric>count dairy-farms with [size-class = "large" OR size-class = "extralarge"]</metric>
    <enumeratedValueSet variable="policy-option-1-Norg-surplus-fine">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="perc-best-strategy">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidy-reduction-Norg-surplus">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="farmers-decision-style">
      <value value="&quot;based-on-profit&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Nitrate-threshold">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="farm-size-threshold">
      <value value="25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Norg-ext-threshold">
      <value value="140"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy-option-5-emission-taxes">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="low-profit-years-needed">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidies-crop-FIT">
      <value value="230"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-years-since-intensity-change">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="GHG-tax">
      <value value="50"/>
    </enumeratedValueSet>
    <steppedValueSet variable="minimum-profit" first="10000" step="1000" last="20000"/>
    <enumeratedValueSet variable="Norg-limit">
      <value value="170"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy-option-3-choose-subsidy-heights">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="world-setup">
      <value value="&quot;gis-world&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidies-grass-ext-FIT">
      <value value="325"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="GHG-threshold">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Nitrate-tax">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy-option-4-redistributive-subsidies">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percentage-checked">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-years-since-size-change">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-market-stochasticity">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="minimum-grassland-share">
      <value value="88"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reduced-per-ha-payments">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy-option-2-ban-grassland-conversion">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidies-grass-int-FIT">
      <value value="295"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="perc-best-strategy" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>count dairy-farms with [intensity-scale = 3 OR intensity-scale = 4]</metric>
    <metric>mean [annual-profit] of dairy-farms</metric>
    <enumeratedValueSet variable="policy-option-1-Norg-surplus-fine">
      <value value="true"/>
    </enumeratedValueSet>
    <steppedValueSet variable="perc-best-strategy" first="5" step="5" last="95"/>
    <enumeratedValueSet variable="subsidy-reduction-Norg-surplus">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="farmers-decision-style">
      <value value="&quot;based-on-profit&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Nitrate-threshold">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="farm-size-threshold">
      <value value="25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Norg-ext-threshold">
      <value value="140"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy-option-5-emission-taxes">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="low-profit-years-needed">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidies-crop-FIT">
      <value value="230"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-years-since-intensity-change">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="GHG-tax">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="minimum-profit">
      <value value="10000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Norg-limit">
      <value value="170"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy-option-3-choose-subsidy-heights">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="world-setup">
      <value value="&quot;gis-world&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidies-grass-ext-FIT">
      <value value="325"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="GHG-threshold">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Nitrate-tax">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy-option-4-redistributive-subsidies">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percentage-checked">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-years-since-size-change">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-market-stochasticity">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="minimum-grassland-share">
      <value value="88"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reduced-per-ha-payments">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy-option-2-ban-grassland-conversion">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidies-grass-int-FIT">
      <value value="295"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="perc-best-strategy_random" repetitions="10" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>count dairy-farms with [intensity-scale = 3 OR intensity-scale = 4]</metric>
    <metric>mean [annual-profit] of dairy-farms</metric>
    <enumeratedValueSet variable="policy-option-1-Norg-surplus-fine">
      <value value="true"/>
    </enumeratedValueSet>
    <steppedValueSet variable="perc-best-strategy" first="5" step="5" last="95"/>
    <enumeratedValueSet variable="subsidy-reduction-Norg-surplus">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="farmers-decision-style">
      <value value="&quot;based-on-profit&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Nitrate-threshold">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="farm-size-threshold">
      <value value="25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Norg-ext-threshold">
      <value value="140"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy-option-5-emission-taxes">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="low-profit-years-needed">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidies-crop-FIT">
      <value value="230"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-years-since-intensity-change">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="GHG-tax">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="minimum-profit">
      <value value="10000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Norg-limit">
      <value value="170"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy-option-3-choose-subsidy-heights">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="world-setup">
      <value value="&quot;random-world&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidies-grass-ext-FIT">
      <value value="325"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="GHG-threshold">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Nitrate-tax">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy-option-4-redistributive-subsidies">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percentage-checked">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-years-since-size-change">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-market-stochasticity">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="minimum-grassland-share">
      <value value="88"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reduced-per-ha-payments">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy-option-2-ban-grassland-conversion">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidies-grass-int-FIT">
      <value value="295"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="perc-best-strategy_random2" repetitions="10" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>count dairy-farms with [intensity-scale = 3 OR intensity-scale = 4]</metric>
    <metric>mean [annual-profit] of dairy-farms</metric>
    <enumeratedValueSet variable="policy-option-1-Norg-surplus-fine">
      <value value="true"/>
    </enumeratedValueSet>
    <steppedValueSet variable="perc-best-strategy" first="5" step="5" last="95"/>
    <enumeratedValueSet variable="subsidy-reduction-Norg-surplus">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="farmers-decision-style">
      <value value="&quot;random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Nitrate-threshold">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="farm-size-threshold">
      <value value="25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Norg-ext-threshold">
      <value value="140"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy-option-5-emission-taxes">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="low-profit-years-needed">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidies-crop-FIT">
      <value value="230"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-years-since-intensity-change">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="GHG-tax">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="minimum-profit">
      <value value="10000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Norg-limit">
      <value value="170"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy-option-3-choose-subsidy-heights">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="world-setup">
      <value value="&quot;random-world&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidies-grass-ext-FIT">
      <value value="325"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="GHG-threshold">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Nitrate-tax">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy-option-4-redistributive-subsidies">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percentage-checked">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-years-since-size-change">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-market-stochasticity">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="minimum-grassland-share">
      <value value="88"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reduced-per-ha-payments">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy-option-2-ban-grassland-conversion">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidies-grass-int-FIT">
      <value value="295"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <enumeratedValueSet variable="perc-best-strategy">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy-option-1-Norg-surplus-fine">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidy-reduction-Norg-surplus">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Nitrate-threshold">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="farm-size-threshold">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Norg-ext-threshold">
      <value value="110"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy-option-5-emission-taxes">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="low-profit-years-needed">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidies-crop-FIT">
      <value value="285"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-years-since-intensity-change">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="GHG-tax">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="minimum-profit">
      <value value="5000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Norg-limit">
      <value value="120"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy-option-3-choose-subsidy-heights">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="world-setup">
      <value value="&quot;gis-world&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidies-grass-ext-FIT">
      <value value="235"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="GHG-threshold">
      <value value="10000000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Nitrate-tax">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy-option-4-redistributive-subsidies">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percentage-checked">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-years-since-size-change">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-market-stochasticity">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="minimum-grassland-share">
      <value value="97"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reduced-per-ha-payments">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidies-grass-int-FIT">
      <value value="285"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy-option-2-ban-grassland-conversion">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>count turtles</metric>
    <enumeratedValueSet variable="policy-option-1-Norg-surplus-fine">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="perc-best-strategy">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidy-reduction-Norg-surplus">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Nitrate-threshold">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="farm-size-threshold">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Norg-ext-threshold">
      <value value="110"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy-option-5-emission-taxes">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="low-profit-years-needed">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidies-crop-FIT">
      <value value="285"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-years-since-intensity-change">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="GHG-tax">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="world-setup">
      <value value="&quot;gis-world&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Norg-limit">
      <value value="120"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy-option-3-choose-subsidy-heights">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="minimum-profit">
      <value value="5000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidies-grass-ext-FIT">
      <value value="235"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="GHG-threshold">
      <value value="10000000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Nitrate-tax">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy-option-4-redistributive-subsidies">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percentage-checked">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-years-since-size-change">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-market-stochasticity">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="minimum-grassland-share">
      <value value="97"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reduced-per-ha-payments">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy-option-2-ban-grassland-conversion">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidies-grass-int-FIT">
      <value value="285"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="GrasslandShare" repetitions="10" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="31"/>
    <metric>[intensity-scale] of dairy-farm 86</metric>
    <metric>[my-grassland-share] of dairy-farm 86</metric>
    <metric>count [my-fields] of dairy-farm 86</metric>
    <enumeratedValueSet variable="policy-option-1-Norg-surplus-fine">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="perc-best-strategy">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidy-reduction-Norg-surplus">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Nitrate-threshold">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="farm-size-threshold">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Norg-ext-threshold">
      <value value="110"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy-option-5-emission-taxes">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="low-profit-years-needed">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidies-crop-FIT">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-years-since-intensity-change">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="GHG-tax">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="world-setup">
      <value value="&quot;random-world&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Norg-limit">
      <value value="120"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy-option-3-choose-subsidy-heights">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="minimum-profit">
      <value value="5000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidies-grass-ext-FIT">
      <value value="350"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="GHG-threshold">
      <value value="10000000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Nitrate-tax">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy-option-4-redistributive-subsidies">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percentage-checked">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-years-since-size-change">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-market-stochasticity">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="minimum-grassland-share">
      <value value="88"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reduced-per-ha-payments">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy-option-2-ban-grassland-conversion">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidies-grass-int-FIT">
      <value value="180"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1"/>
    <metric>[(word who "," size) ] of turtles</metric>
    <enumeratedValueSet variable="policy-option-1-Norg-surplus-fine">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="perc-best-strategy">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidy-reduction-Norg-surplus">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Nitrate-threshold">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="farm-size-threshold">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Norg-ext-threshold">
      <value value="110"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy-option-5-emission-taxes">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="low-profit-years-needed">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidies-crop-FIT">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-years-since-intensity-change">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="GHG-tax">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="world-setup">
      <value value="&quot;gis-world&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Norg-limit">
      <value value="120"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy-option-3-choose-subsidy-heights">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="minimum-profit">
      <value value="5000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidies-grass-ext-FIT">
      <value value="350"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="GHG-threshold">
      <value value="10000000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Nitrate-tax">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy-option-4-redistributive-subsidies">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percentage-checked">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-years-since-size-change">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-market-stochasticity">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="minimum-grassland-share">
      <value value="97"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reduced-per-ha-payments">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy-option-2-ban-grassland-conversion">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidies-grass-int-FIT">
      <value value="180"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Policy1" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="4"/>
    <metric>mean [received-subsidies] of dairy-farms with [intensity-scale = 1]</metric>
    <metric>mean [received-market-value] of dairy-farms with [intensity-scale = 1]</metric>
    <metric>mean [feed-value-quantity] of dairy-farms with [intensity-scale = 1]</metric>
    <metric>mean [no-of-cows] of dairy-farms with [intensity-scale = 1]</metric>
    <metric>mean [milk-volume] of dairy-farms with [intensity-scale = 1]</metric>
    <metric>mean [land-lease] of dairy-farms with [intensity-scale = 1]</metric>
    <metric>mean [labour-costs] of dairy-farms with [intensity-scale = 1]</metric>
    <metric>mean [input-costs] of dairy-farms with [intensity-scale = 1]</metric>
    <metric>mean [annual-profit] of dairy-farms with [intensity-scale = 1]</metric>
    <metric>mean [farm-size] of dairy-farms with [intensity-scale = 1]</metric>
    <metric>profit-very-extensive</metric>
    <metric>mean [received-subsidies] of dairy-farms with [intensity-scale = 2]</metric>
    <metric>mean [received-market-value] of dairy-farms with [intensity-scale = 2]</metric>
    <metric>mean [feed-value-quantity] of dairy-farms with [intensity-scale = 2]</metric>
    <metric>mean [no-of-cows] of dairy-farms with [intensity-scale = 2]</metric>
    <metric>mean [milk-volume] of dairy-farms with [intensity-scale = 2]</metric>
    <metric>mean [land-lease] of dairy-farms with [intensity-scale = 2]</metric>
    <metric>mean [labour-costs] of dairy-farms with [intensity-scale = 2]</metric>
    <metric>mean [input-costs] of dairy-farms with [intensity-scale = 2]</metric>
    <metric>mean [annual-profit] of dairy-farms with [intensity-scale = 2]</metric>
    <metric>mean [farm-size] of dairy-farms with [intensity-scale = 2]</metric>
    <metric>profit-extensive</metric>
    <enumeratedValueSet variable="policy-option-1-Norg-surplus-fine">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="perc-best-strategy">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidy-reduction-Norg-surplus">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Nitrate-threshold">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="farm-size-threshold">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Norg-ext-threshold">
      <value value="110"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy-option-5-emission-taxes">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="low-profit-years-needed">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidies-crop-FIT">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-years-since-intensity-change">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="GHG-tax">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="minimum-profit">
      <value value="5000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="world-setup">
      <value value="&quot;gis-world&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy-option-3-choose-subsidy-heights">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Norg-limit">
      <value value="120"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidies-grass-ext-FIT">
      <value value="350"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="GHG-threshold">
      <value value="10000000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Nitrate-tax">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy-option-4-redistributive-subsidies">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percentage-checked">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-years-since-size-change">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-market-stochasticity">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="minimum-grassland-share">
      <value value="88"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reduced-per-ha-payments">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy-option-2-ban-grassland-conversion">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidies-grass-int-FIT">
      <value value="180"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Policy 4" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="31"/>
    <metric>[farm-size] of dairy-farm 103</metric>
    <metric>[received-subsidies] of dairy-farm 103</metric>
    <metric>[land-lease] of dairy-farm 103</metric>
    <metric>[labour-costs] of dairy-farm 103</metric>
    <metric>[input-costs] of dairy-farm 103</metric>
    <metric>[annual-interim-profit] of dairy-farm 103</metric>
    <enumeratedValueSet variable="policy-option-1-Norg-surplus-fine">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="perc-best-strategy">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidy-reduction-Norg-surplus">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Nitrate-threshold">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="farm-size-threshold">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Norg-ext-threshold">
      <value value="110"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy-option-5-emission-taxes">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="low-profit-years-needed">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidies-crop-FIT">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-years-since-intensity-change">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="GHG-tax">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="minimum-profit">
      <value value="5000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="world-setup">
      <value value="&quot;random-world&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy-option-3-choose-subsidy-heights">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Norg-limit">
      <value value="120"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidies-grass-ext-FIT">
      <value value="350"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="GHG-threshold">
      <value value="10000000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Nitrate-tax">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy-option-4-redistributive-subsidies">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percentage-checked">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-years-since-size-change">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-market-stochasticity">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="minimum-grassland-share">
      <value value="88"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reduced-per-ha-payments">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy-option-2-ban-grassland-conversion">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidies-grass-int-FIT">
      <value value="180"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="manure" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>mean [no-of-cows] of dairy-farms</metric>
    <metric>mean [produced-manure] of dairy-farms</metric>
    <metric>mean [received-manure] of dairy-farms</metric>
    <metric>mean [available-manure] of dairy-farms</metric>
    <metric>mean [initial-avg-Norg-level] of dairy-farms</metric>
    <metric>mean [adjusted-avg-Norg-level] of dairy-farms</metric>
    <metric>mean [Norg-surplus-fine] of dairy-farms</metric>
    <metric>mean [intensity-scale] of dairy-farms</metric>
    <enumeratedValueSet variable="perc-best-strategy">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy-option-1-Norg-surplus-fine">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidy-reduction-Norg-surplus">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Nitrate-threshold">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="farm-size-threshold">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Norg-ext-threshold">
      <value value="110"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy-option-5-emission-taxes">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="low-profit-years-needed">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidies-crop-FIT">
      <value value="350"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="GHG-tax">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="waiting-time-for-intensity-change">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="world-setup">
      <value value="&quot;random-world&quot;"/>
      <value value="&quot;gis-world&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="minimum-profit">
      <value value="5000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy-option-3-choose-subsidy-heights">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidies-grass-ext-FIT">
      <value value="710"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="GHG-threshold">
      <value value="5646900"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Nitrate-tax">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy-option-4-redistributive-subsidies">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percentage-checked">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="minimum-grassland-share">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-market-stochasticity">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidies-grass-int-FIT">
      <value value="350"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reduced-per-ha-payments">
      <value value="80"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy-option-2-ban-grassland-conversion">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Norg-limit-FIT">
      <value value="120"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="ghg emisssions" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>mean [farm-size] of dairy-farms with [intensity-scale = 1]</metric>
    <metric>mean [GHG-farm-emission] of dairy-farms with [intensity-scale = 1]</metric>
    <metric>mean [GHG-tax-paid] of dairy-farms with [intensity-scale = 1]</metric>
    <metric>mean [annual-interim-profit] of dairy-farms with [intensity-scale = 1]</metric>
    <metric>mean [farm-size] of dairy-farms with [intensity-scale = 2]</metric>
    <metric>mean [GHG-farm-emission] of dairy-farms with [intensity-scale = 2]</metric>
    <metric>mean [GHG-tax-paid] of dairy-farms with [intensity-scale = 2]</metric>
    <metric>mean [annual-interim-profit] of dairy-farms with [intensity-scale = 2]</metric>
    <metric>mean [farm-size] of dairy-farms with [intensity-scale = 3]</metric>
    <metric>mean [GHG-farm-emission] of dairy-farms with [intensity-scale = 3]</metric>
    <metric>mean [GHG-tax-paid] of dairy-farms with [intensity-scale = 3]</metric>
    <metric>mean [annual-interim-profit] of dairy-farms with [intensity-scale = 3]</metric>
    <metric>mean [farm-size] of dairy-farms with [intensity-scale = 4]</metric>
    <metric>mean [GHG-farm-emission] of dairy-farms with [intensity-scale = 4]</metric>
    <metric>mean [GHG-tax-paid] of dairy-farms with [intensity-scale = 4]</metric>
    <metric>mean [annual-interim-profit] of dairy-farms with [intensity-scale = 4]</metric>
    <enumeratedValueSet variable="perc-best-strategy">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy-option-1-Norg-surplus-fine">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidy-reduction-Norg-surplus">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Nitrate-threshold">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="farm-size-threshold">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Norg-ext-threshold">
      <value value="110"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy-option-5-emission-taxes">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="low-profit-years-needed">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidies-crop-FIT">
      <value value="350"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="GHG-tax">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="waiting-time-for-intensity-change">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="minimum-profit">
      <value value="5000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="world-setup">
      <value value="&quot;random-world&quot;"/>
      <value value="&quot;gis-world&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy-option-3-choose-subsidy-heights">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidies-grass-ext-FIT">
      <value value="710"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="GHG-threshold">
      <value value="5646900"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Nitrate-tax">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy-option-4-redistributive-subsidies">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percentage-checked">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-market-stochasticity">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="minimum-grassland-share">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reduced-per-ha-payments">
      <value value="80"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Norg-limit-FIT">
      <value value="120"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy-option-2-ban-grassland-conversion">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidies-grass-int-FIT">
      <value value="350"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="cowdensity" repetitions="100" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>mean</metric>
    <enumeratedValueSet variable="perc-best-strategy">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy-option-1-Norg-surplus-fine">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidy-reduction-Norg-surplus">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Nitrate-threshold">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="farm-size-threshold">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Norg-ext-threshold">
      <value value="110"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy-option-5-emission-taxes">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="low-profit-years-needed">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidies-crop-FIT">
      <value value="350"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="GHG-tax">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="waiting-time-for-intensity-change">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="minimum-profit">
      <value value="5000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="world-setup">
      <value value="&quot;random-world&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy-option-3-choose-subsidy-heights">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidies-grass-ext-FIT">
      <value value="710"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="GHG-threshold">
      <value value="5646900"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Nitrate-tax">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy-option-4-redistributive-subsidies">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percentage-checked">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-market-stochasticity">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="minimum-grassland-share">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reduced-per-ha-payments">
      <value value="80"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Norg-limit-FIT">
      <value value="120"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy-option-2-ban-grassland-conversion">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidies-grass-int-FIT">
      <value value="350"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>mean [my-grassland-share] of dairy-farms with [intensity-scale = 1]</metric>
    <metric>mean [my-grassland-share] of dairy-farms with [intensity-scale = 2]</metric>
    <metric>mean [my-grassland-share] of dairy-farms with [intensity-scale = 3]</metric>
    <metric>mean [my-grassland-share] of dairy-farms with [intensity-scale = 4]</metric>
    <enumeratedValueSet variable="perc-best-strategy">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy-option-1-Norg-surplus-fine">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidy-reduction-Norg-surplus">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Nitrate-threshold">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="farm-size-threshold">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Norg-ext-threshold">
      <value value="110"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy-option-5-emission-taxes">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="low-profit-years-needed">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidies-crop-FIT">
      <value value="350"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="GHG-tax">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="waiting-time-for-intensity-change">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="minimum-profit">
      <value value="5000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="world-setup">
      <value value="&quot;random-world&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy-option-3-choose-subsidy-heights">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidies-grass-ext-FIT">
      <value value="710"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="GHG-threshold">
      <value value="5646900"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Nitrate-tax">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy-option-4-redistributive-subsidies">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percentage-checked">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-market-stochasticity">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="minimum-grassland-share">
      <value value="80"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reduced-per-ha-payments">
      <value value="80"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Norg-limit-FIT">
      <value value="120"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy-option-2-ban-grassland-conversion">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidies-grass-int-FIT">
      <value value="350"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment" repetitions="30" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>mean [received-subsidies] of dairy-farms with [intensity-scale = 4] / mean [annual-profit] of dairy-farms with [intensity-scale = 4]</metric>
    <enumeratedValueSet variable="policy-option-1-Norg-surplus-fine">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="perc-best-strategy">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidy-reduction-Norg-surplus">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Nitrate-threshold">
      <value value="27500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="farm-size-threshold">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Norg-ext-threshold">
      <value value="110"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy-option-5-emission-taxes">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="low-profit-years-needed">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidies-crop-FIT">
      <value value="350"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="GHG-tax">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="waiting-time-for-intensity-change">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="world-setup">
      <value value="&quot;random-world&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="minimum-profit">
      <value value="5000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy-option-3-choose-subsidy-heights">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidy-reduction-per-ha">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidies-grass-ext-FIT">
      <value value="350"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="GHG-threshold">
      <value value="14000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Nitrate-tax">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy-option-4-redistributive-subsidies">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percentage-checked">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="minimum-grassland-share">
      <value value="80"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-market-stochasticity">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidies-grass-int-FIT">
      <value value="350"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy-option-2-ban-grassland-conversion">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Norg-limit-FIT">
      <value value="100"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Norg-limit-FIT" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>water-quality-index</metric>
    <metric>climate-regulation-index</metric>
    <metric>habitat-quality-index</metric>
    <metric>sum [milk-volume] of dairy-farms with [intensity-scale = 3 OR intensity-scale = 4]</metric>
    <metric>sum [milk-volume] of dairy-farms with [intensity-scale = 1 OR intensity-scale = 2]</metric>
    <metric>soil-fertility-index</metric>
    <enumeratedValueSet variable="policy-option-1-Norg-surplus-fine">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="perc-best-strategy">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidy-reduction-Norg-surplus">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Nitrate-threshold">
      <value value="27500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="farm-size-threshold">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy-option-5-emission-taxes">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="low-profit-years-needed">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidies-crop-FIT">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="GHG-tax">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="waiting-time-for-intensity-change">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="minimum-profit">
      <value value="5000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="world-setup">
      <value value="&quot;random-world&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy-option-3-choose-subsidy-heights">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidy-reduction-per-ha">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidies-grass-ext-FIT">
      <value value="700"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="GHG-threshold">
      <value value="14000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Nitrate-tax">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy-option-4-redistributive-subsidies">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percentage-checked">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Norg-ext-threshold-FIT">
      <value value="110"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="minimum-grassland-share">
      <value value="80"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-market-stochasticity">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidies-grass-int-FIT">
      <value value="180"/>
    </enumeratedValueSet>
    <steppedValueSet variable="Norg-limit-FIT" first="20" step="10" last="400"/>
    <enumeratedValueSet variable="policy-option-2-ban-grassland-conversion">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="random-seed">
      <value value="100"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="subsidy-reduction-Norg-surplus" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>water-quality-index</metric>
    <metric>climate-regulation-index</metric>
    <metric>habitat-quality-index</metric>
    <metric>sum [milk-volume] of dairy-farms with [intensity-scale = 3 OR intensity-scale = 4]</metric>
    <metric>sum [milk-volume] of dairy-farms with [intensity-scale = 1 OR intensity-scale = 2]</metric>
    <metric>soil-fertility-index</metric>
    <enumeratedValueSet variable="policy-option-1-Norg-surplus-fine">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="perc-best-strategy">
      <value value="30"/>
    </enumeratedValueSet>
    <steppedValueSet variable="subsidy-reduction-Norg-surplus" first="0" step="2" last="80"/>
    <enumeratedValueSet variable="Nitrate-threshold">
      <value value="27500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="farm-size-threshold">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy-option-5-emission-taxes">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="low-profit-years-needed">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidies-crop-FIT">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="GHG-tax">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="waiting-time-for-intensity-change">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="minimum-profit">
      <value value="5000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="world-setup">
      <value value="&quot;random-world&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy-option-3-choose-subsidy-heights">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidy-reduction-per-ha">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidies-grass-ext-FIT">
      <value value="350"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="GHG-threshold">
      <value value="14000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Nitrate-tax">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy-option-4-redistributive-subsidies">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percentage-checked">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Norg-ext-threshold-FIT">
      <value value="110"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="minimum-grassland-share">
      <value value="80"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-market-stochasticity">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidies-grass-int-FIT">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Norg-limit-FIT">
      <value value="170"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy-option-2-ban-grassland-conversion">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="random-seed">
      <value value="100"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="percentage-checked" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>water-quality-index</metric>
    <metric>climate-regulation-index</metric>
    <metric>habitat-quality-index</metric>
    <metric>sum [milk-volume] of dairy-farms with [intensity-scale = 3 OR intensity-scale = 4]</metric>
    <metric>sum [milk-volume] of dairy-farms with [intensity-scale = 1 OR intensity-scale = 2]</metric>
    <metric>sum [milk-volume] of dairy-farms</metric>
    <metric>soil-fertility-index</metric>
    <enumeratedValueSet variable="policy-option-1-Norg-surplus-fine">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="perc-best-strategy">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidy-reduction-Norg-surplus">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Nitrate-threshold">
      <value value="27500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="farm-size-threshold">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy-option-5-emission-taxes">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="low-profit-years-needed">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidies-crop-FIT">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="GHG-tax">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="waiting-time-for-intensity-change">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="minimum-profit">
      <value value="5000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="world-setup">
      <value value="&quot;random-world&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy-option-3-choose-subsidy-heights">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidy-reduction-per-ha">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidies-grass-ext-FIT">
      <value value="350"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="GHG-threshold">
      <value value="14000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Nitrate-tax">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy-option-4-redistributive-subsidies">
      <value value="false"/>
    </enumeratedValueSet>
    <steppedValueSet variable="percentage-checked" first="0" step="2" last="100"/>
    <enumeratedValueSet variable="Norg-ext-threshold-FIT">
      <value value="110"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="minimum-grassland-share">
      <value value="80"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-market-stochasticity">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidies-grass-int-FIT">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Norg-limit-FIT">
      <value value="170"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy-option-2-ban-grassland-conversion">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="random-seed">
      <value value="100"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="subsidy-reduction-Norg-surplus2" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>water-quality-index</metric>
    <metric>climate-regulation-index</metric>
    <metric>habitat-quality-index</metric>
    <metric>sum [milk-volume] of dairy-farms with [intensity-scale = 3 OR intensity-scale = 4]</metric>
    <metric>sum [milk-volume] of dairy-farms with [intensity-scale = 1 OR intensity-scale = 2]</metric>
    <metric>sum [milk-volume] of dairy-farms</metric>
    <metric>soil-fertility-index</metric>
    <enumeratedValueSet variable="perc-best-strategy">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy-option-1-Norg-surplus-fine">
      <value value="true"/>
    </enumeratedValueSet>
    <steppedValueSet variable="subsidy-reduction-Norg-surplus" first="0" step="2" last="100"/>
    <enumeratedValueSet variable="Nitrate-threshold">
      <value value="27500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="farm-size-threshold">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy-option-5-emission-taxes">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="low-profit-years-needed">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidies-crop-FIT">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="GHG-tax">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="waiting-time-for-intensity-change">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="minimum-profit">
      <value value="5000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="world-setup">
      <value value="&quot;random-world&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy-option-3-choose-subsidy-heights">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidy-reduction-per-ha">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidies-grass-ext-FIT">
      <value value="350"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="GHG-threshold">
      <value value="14000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Nitrate-tax">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy-option-4-redistributive-subsidies">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percentage-checked">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Norg-ext-threshold-FIT">
      <value value="110"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-market-stochasticity">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="minimum-grassland-share">
      <value value="80"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidies-grass-int-FIT">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy-option-2-ban-grassland-conversion">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Norg-limit-FIT">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="random-seed">
      <value value="100"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="minimum-grassland-share" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>water-quality-index</metric>
    <metric>climate-regulation-index</metric>
    <metric>habitat-quality-index</metric>
    <metric>sum [milk-volume] of dairy-farms with [intensity-scale = 3 OR intensity-scale = 4]</metric>
    <metric>sum [milk-volume] of dairy-farms with [intensity-scale = 1 OR intensity-scale = 2]</metric>
    <metric>sum [milk-volume] of dairy-farms</metric>
    <metric>soil-fertility-index</metric>
    <enumeratedValueSet variable="perc-best-strategy">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy-option-1-Norg-surplus-fine">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidy-reduction-Norg-surplus">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Nitrate-threshold">
      <value value="27500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="farm-size-threshold">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy-option-5-emission-taxes">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="low-profit-years-needed">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidies-crop-FIT">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="GHG-tax">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="waiting-time-for-intensity-change">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="minimum-profit">
      <value value="5000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="world-setup">
      <value value="&quot;random-world&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy-option-3-choose-subsidy-heights">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidy-reduction-per-ha">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidies-grass-ext-FIT">
      <value value="350"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="GHG-threshold">
      <value value="14000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Nitrate-tax">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy-option-4-redistributive-subsidies">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percentage-checked">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Norg-ext-threshold-FIT">
      <value value="110"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-market-stochasticity">
      <value value="0"/>
    </enumeratedValueSet>
    <steppedValueSet variable="minimum-grassland-share" first="0" step="2" last="100"/>
    <enumeratedValueSet variable="subsidies-grass-int-FIT">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy-option-2-ban-grassland-conversion">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Norg-limit-FIT">
      <value value="170"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="random-seed">
      <value value="100"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="subsidies-crop-FIT" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>water-quality-index</metric>
    <metric>climate-regulation-index</metric>
    <metric>habitat-quality-index</metric>
    <metric>sum [milk-volume] of dairy-farms with [intensity-scale = 3 OR intensity-scale = 4]</metric>
    <metric>sum [milk-volume] of dairy-farms with [intensity-scale = 1 OR intensity-scale = 2]</metric>
    <metric>sum [milk-volume] of dairy-farms</metric>
    <metric>soil-fertility-index</metric>
    <enumeratedValueSet variable="perc-best-strategy">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy-option-1-Norg-surplus-fine">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidy-reduction-Norg-surplus">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Nitrate-threshold">
      <value value="27500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="farm-size-threshold">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy-option-5-emission-taxes">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="low-profit-years-needed">
      <value value="4"/>
    </enumeratedValueSet>
    <steppedValueSet variable="subsidies-crop-FIT" first="0" step="20" last="800"/>
    <enumeratedValueSet variable="GHG-tax">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="waiting-time-for-intensity-change">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="minimum-profit">
      <value value="5000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="world-setup">
      <value value="&quot;random-world&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy-option-3-choose-subsidy-heights">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidy-reduction-per-ha">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidies-grass-ext-FIT">
      <value value="350"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="GHG-threshold">
      <value value="14000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Nitrate-tax">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy-option-4-redistributive-subsidies">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percentage-checked">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Norg-ext-threshold-FIT">
      <value value="110"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-market-stochasticity">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="minimum-grassland-share">
      <value value="80"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidies-grass-int-FIT">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy-option-2-ban-grassland-conversion">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Norg-limit-FIT">
      <value value="170"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="random-seed">
      <value value="100"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="subsidies-grass-int-FIT" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>water-quality-index</metric>
    <metric>climate-regulation-index</metric>
    <metric>habitat-quality-index</metric>
    <metric>sum [milk-volume] of dairy-farms with [intensity-scale = 3 OR intensity-scale = 4]</metric>
    <metric>sum [milk-volume] of dairy-farms with [intensity-scale = 1 OR intensity-scale = 2]</metric>
    <metric>sum [milk-volume] of dairy-farms</metric>
    <metric>soil-fertility-index</metric>
    <enumeratedValueSet variable="perc-best-strategy">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy-option-1-Norg-surplus-fine">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidy-reduction-Norg-surplus">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Nitrate-threshold">
      <value value="27500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="farm-size-threshold">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy-option-5-emission-taxes">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="low-profit-years-needed">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidies-crop-FIT">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="GHG-tax">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="waiting-time-for-intensity-change">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="minimum-profit">
      <value value="5000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="world-setup">
      <value value="&quot;random-world&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy-option-3-choose-subsidy-heights">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidy-reduction-per-ha">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidies-grass-ext-FIT">
      <value value="350"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="GHG-threshold">
      <value value="14000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Nitrate-tax">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy-option-4-redistributive-subsidies">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percentage-checked">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Norg-ext-threshold-FIT">
      <value value="110"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-market-stochasticity">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="minimum-grassland-share">
      <value value="80"/>
    </enumeratedValueSet>
    <steppedValueSet variable="subsidies-grass-int-FIT" first="0" step="20" last="800"/>
    <enumeratedValueSet variable="policy-option-2-ban-grassland-conversion">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Norg-limit-FIT">
      <value value="170"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="random-seed">
      <value value="100"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Norg-ext-threshold-FIT2" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>water-quality-index</metric>
    <metric>climate-regulation-index</metric>
    <metric>habitat-quality-index</metric>
    <metric>sum [milk-volume] of dairy-farms with [intensity-scale = 3 OR intensity-scale = 4]</metric>
    <metric>sum [milk-volume] of dairy-farms with [intensity-scale = 1 OR intensity-scale = 2]</metric>
    <metric>sum [milk-volume] of dairy-farms</metric>
    <metric>soil-fertility-index</metric>
    <enumeratedValueSet variable="perc-best-strategy">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy-option-1-Norg-surplus-fine">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidy-reduction-Norg-surplus">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Nitrate-threshold">
      <value value="27500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="farm-size-threshold">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy-option-5-emission-taxes">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="low-profit-years-needed">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidies-crop-FIT">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="GHG-tax">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="waiting-time-for-intensity-change">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="minimum-profit">
      <value value="5000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="world-setup">
      <value value="&quot;random-world&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy-option-3-choose-subsidy-heights">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidy-reduction-per-ha">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidies-grass-ext-FIT">
      <value value="700"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="GHG-threshold">
      <value value="14000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Nitrate-tax">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy-option-4-redistributive-subsidies">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percentage-checked">
      <value value="20"/>
    </enumeratedValueSet>
    <steppedValueSet variable="Norg-ext-threshold-FIT" first="0" step="5" last="200"/>
    <enumeratedValueSet variable="max-market-stochasticity">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="minimum-grassland-share">
      <value value="80"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidies-grass-int-FIT">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy-option-2-ban-grassland-conversion">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Norg-limit-FIT">
      <value value="170"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="random-seed">
      <value value="100"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="subsidies-grass-ext-FIT" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>water-quality-index</metric>
    <metric>climate-regulation-index</metric>
    <metric>habitat-quality-index</metric>
    <metric>sum [milk-volume] of dairy-farms with [intensity-scale = 3 OR intensity-scale = 4]</metric>
    <metric>sum [milk-volume] of dairy-farms with [intensity-scale = 1 OR intensity-scale = 2]</metric>
    <metric>sum [milk-volume] of dairy-farms</metric>
    <metric>soil-fertility-index</metric>
    <enumeratedValueSet variable="perc-best-strategy">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy-option-1-Norg-surplus-fine">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidy-reduction-Norg-surplus">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Nitrate-threshold">
      <value value="27500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="farm-size-threshold">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy-option-5-emission-taxes">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="low-profit-years-needed">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidies-crop-FIT">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="GHG-tax">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="waiting-time-for-intensity-change">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="minimum-profit">
      <value value="5000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="world-setup">
      <value value="&quot;random-world&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy-option-3-choose-subsidy-heights">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidy-reduction-per-ha">
      <value value="30"/>
    </enumeratedValueSet>
    <steppedValueSet variable="subsidies-grass-ext-FIT" first="0" step="40" last="1600"/>
    <enumeratedValueSet variable="GHG-threshold">
      <value value="14000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Nitrate-tax">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy-option-4-redistributive-subsidies">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percentage-checked">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Norg-ext-threshold-FIT">
      <value value="110"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-market-stochasticity">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="minimum-grassland-share">
      <value value="80"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidies-grass-int-FIT">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy-option-2-ban-grassland-conversion">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Norg-limit-FIT">
      <value value="170"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="random-seed">
      <value value="100"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="farm-size-threshold" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>water-quality-index</metric>
    <metric>climate-regulation-index</metric>
    <metric>habitat-quality-index</metric>
    <metric>sum [milk-volume] of dairy-farms with [intensity-scale = 3 OR intensity-scale = 4]</metric>
    <metric>sum [milk-volume] of dairy-farms with [intensity-scale = 1 OR intensity-scale = 2]</metric>
    <metric>sum [milk-volume] of dairy-farms</metric>
    <metric>soil-fertility-index</metric>
    <enumeratedValueSet variable="perc-best-strategy">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy-option-1-Norg-surplus-fine">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidy-reduction-Norg-surplus">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Nitrate-threshold">
      <value value="27500"/>
    </enumeratedValueSet>
    <steppedValueSet variable="farm-size-threshold" first="10" step="5" last="210"/>
    <enumeratedValueSet variable="policy-option-5-emission-taxes">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="low-profit-years-needed">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidies-crop-FIT">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="GHG-tax">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="waiting-time-for-intensity-change">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="minimum-profit">
      <value value="5000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="world-setup">
      <value value="&quot;random-world&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy-option-3-choose-subsidy-heights">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidy-reduction-per-ha">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidies-grass-ext-FIT">
      <value value="350"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="GHG-threshold">
      <value value="14000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Nitrate-tax">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy-option-4-redistributive-subsidies">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percentage-checked">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Norg-ext-threshold-FIT">
      <value value="110"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-market-stochasticity">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="minimum-grassland-share">
      <value value="80"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidies-grass-int-FIT">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy-option-2-ban-grassland-conversion">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Norg-limit-FIT">
      <value value="170"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="random-seed">
      <value value="100"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Norg-ext-threshold-FIT3" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>water-quality-index</metric>
    <metric>climate-regulation-index</metric>
    <metric>habitat-quality-index</metric>
    <metric>sum [milk-volume] of dairy-farms with [intensity-scale = 3 OR intensity-scale = 4]</metric>
    <metric>sum [milk-volume] of dairy-farms with [intensity-scale = 1 OR intensity-scale = 2]</metric>
    <metric>sum [milk-volume] of dairy-farms</metric>
    <metric>soil-fertility-index</metric>
    <enumeratedValueSet variable="perc-best-strategy">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy-option-1-Norg-surplus-fine">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidy-reduction-Norg-surplus">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Nitrate-threshold">
      <value value="27500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="farm-size-threshold">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy-option-5-emission-taxes">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="low-profit-years-needed">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidies-crop-FIT">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="GHG-tax">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="waiting-time-for-intensity-change">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="minimum-profit">
      <value value="5000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="world-setup">
      <value value="&quot;random-world&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy-option-3-choose-subsidy-heights">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidy-reduction-per-ha">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidies-grass-ext-FIT">
      <value value="1500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="GHG-threshold">
      <value value="14000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Nitrate-tax">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy-option-4-redistributive-subsidies">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percentage-checked">
      <value value="20"/>
    </enumeratedValueSet>
    <steppedValueSet variable="Norg-ext-threshold-FIT" first="0" step="5" last="200"/>
    <enumeratedValueSet variable="max-market-stochasticity">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="minimum-grassland-share">
      <value value="80"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidies-grass-int-FIT">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy-option-2-ban-grassland-conversion">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Norg-limit-FIT">
      <value value="170"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="random-seed">
      <value value="100"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="subsidy-reduction-per-ha" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>water-quality-index</metric>
    <metric>climate-regulation-index</metric>
    <metric>habitat-quality-index</metric>
    <metric>sum [milk-volume] of dairy-farms with [intensity-scale = 3 OR intensity-scale = 4]</metric>
    <metric>sum [milk-volume] of dairy-farms with [intensity-scale = 1 OR intensity-scale = 2]</metric>
    <metric>sum [milk-volume] of dairy-farms</metric>
    <metric>soil-fertility-index</metric>
    <enumeratedValueSet variable="policy-option-1-Norg-surplus-fine">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="perc-best-strategy">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidy-reduction-Norg-surplus">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Nitrate-threshold">
      <value value="27500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="farm-size-threshold">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy-option-5-emission-taxes">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="low-profit-years-needed">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidies-crop-FIT">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="GHG-tax">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="waiting-time-for-intensity-change">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="minimum-profit">
      <value value="5000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="world-setup">
      <value value="&quot;random-world&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="subsidy-reduction-per-ha" first="0" step="2" last="100"/>
    <enumeratedValueSet variable="policy-option-3-choose-subsidy-heights">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidies-grass-ext-FIT">
      <value value="350"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="GHG-threshold">
      <value value="14000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Nitrate-tax">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy-option-4-redistributive-subsidies">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percentage-checked">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Norg-ext-threshold-FIT">
      <value value="110"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="minimum-grassland-share">
      <value value="80"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-market-stochasticity">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidies-grass-int-FIT">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Norg-limit-FIT">
      <value value="170"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy-option-2-ban-grassland-conversion">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="random-seed">
      <value value="100"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="GHG-threshold" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>water-quality-index</metric>
    <metric>climate-regulation-index</metric>
    <metric>habitat-quality-index</metric>
    <metric>sum [milk-volume] of dairy-farms with [intensity-scale = 3 OR intensity-scale = 4]</metric>
    <metric>sum [milk-volume] of dairy-farms with [intensity-scale = 1 OR intensity-scale = 2]</metric>
    <metric>sum [milk-volume] of dairy-farms</metric>
    <metric>soil-fertility-index</metric>
    <enumeratedValueSet variable="policy-option-1-Norg-surplus-fine">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="perc-best-strategy">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidy-reduction-Norg-surplus">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Nitrate-threshold">
      <value value="27500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="farm-size-threshold">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy-option-5-emission-taxes">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="low-profit-years-needed">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidies-crop-FIT">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="GHG-tax">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="waiting-time-for-intensity-change">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="minimum-profit">
      <value value="5000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="world-setup">
      <value value="&quot;random-world&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidy-reduction-per-ha">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy-option-3-choose-subsidy-heights">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidies-grass-ext-FIT">
      <value value="350"/>
    </enumeratedValueSet>
    <steppedValueSet variable="GHG-threshold" first="0" step="2000" last="80000"/>
    <enumeratedValueSet variable="Nitrate-tax">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy-option-4-redistributive-subsidies">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percentage-checked">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Norg-ext-threshold-FIT">
      <value value="110"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="minimum-grassland-share">
      <value value="80"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-market-stochasticity">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidies-grass-int-FIT">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Norg-limit-FIT">
      <value value="170"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy-option-2-ban-grassland-conversion">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="random-seed">
      <value value="100"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="GHG-tax" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>water-quality-index</metric>
    <metric>climate-regulation-index</metric>
    <metric>habitat-quality-index</metric>
    <metric>sum [milk-volume] of dairy-farms with [intensity-scale = 3 OR intensity-scale = 4]</metric>
    <metric>sum [milk-volume] of dairy-farms with [intensity-scale = 1 OR intensity-scale = 2]</metric>
    <metric>sum [milk-volume] of dairy-farms</metric>
    <metric>soil-fertility-index</metric>
    <enumeratedValueSet variable="policy-option-1-Norg-surplus-fine">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="perc-best-strategy">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidy-reduction-Norg-surplus">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Nitrate-threshold">
      <value value="27500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="farm-size-threshold">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy-option-5-emission-taxes">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="low-profit-years-needed">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidies-crop-FIT">
      <value value="180"/>
    </enumeratedValueSet>
    <steppedValueSet variable="GHG-tax" first="0" step="20" last="1000"/>
    <enumeratedValueSet variable="waiting-time-for-intensity-change">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="minimum-profit">
      <value value="5000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="world-setup">
      <value value="&quot;random-world&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidy-reduction-per-ha">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy-option-3-choose-subsidy-heights">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidies-grass-ext-FIT">
      <value value="350"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="GHG-threshold">
      <value value="14000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Nitrate-tax">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy-option-4-redistributive-subsidies">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percentage-checked">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Norg-ext-threshold-FIT">
      <value value="110"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="minimum-grassland-share">
      <value value="80"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-market-stochasticity">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidies-grass-int-FIT">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Norg-limit-FIT">
      <value value="170"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy-option-2-ban-grassland-conversion">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="random-seed">
      <value value="100"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Nitrate-threshold" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>water-quality-index</metric>
    <metric>climate-regulation-index</metric>
    <metric>habitat-quality-index</metric>
    <metric>sum [milk-volume] of dairy-farms with [intensity-scale = 3 OR intensity-scale = 4]</metric>
    <metric>sum [milk-volume] of dairy-farms with [intensity-scale = 1 OR intensity-scale = 2]</metric>
    <metric>sum [milk-volume] of dairy-farms</metric>
    <metric>soil-fertility-index</metric>
    <enumeratedValueSet variable="policy-option-1-Norg-surplus-fine">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="perc-best-strategy">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidy-reduction-Norg-surplus">
      <value value="20"/>
    </enumeratedValueSet>
    <steppedValueSet variable="Nitrate-threshold" first="0" step="20000" last="1000000"/>
    <enumeratedValueSet variable="farm-size-threshold">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy-option-5-emission-taxes">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="low-profit-years-needed">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidies-crop-FIT">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="GHG-tax">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="waiting-time-for-intensity-change">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="minimum-profit">
      <value value="5000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="world-setup">
      <value value="&quot;random-world&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidy-reduction-per-ha">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy-option-3-choose-subsidy-heights">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidies-grass-ext-FIT">
      <value value="350"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="GHG-threshold">
      <value value="14000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Nitrate-tax">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy-option-4-redistributive-subsidies">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percentage-checked">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Norg-ext-threshold-FIT">
      <value value="110"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="minimum-grassland-share">
      <value value="80"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-market-stochasticity">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidies-grass-int-FIT">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Norg-limit-FIT">
      <value value="170"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy-option-2-ban-grassland-conversion">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="random-seed">
      <value value="100"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Nitrate-tax" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>water-quality-index</metric>
    <metric>climate-regulation-index</metric>
    <metric>habitat-quality-index</metric>
    <metric>sum [milk-volume] of dairy-farms with [intensity-scale = 3 OR intensity-scale = 4]</metric>
    <metric>sum [milk-volume] of dairy-farms with [intensity-scale = 1 OR intensity-scale = 2]</metric>
    <metric>sum [milk-volume] of dairy-farms</metric>
    <metric>soil-fertility-index</metric>
    <enumeratedValueSet variable="policy-option-1-Norg-surplus-fine">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="perc-best-strategy">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidy-reduction-Norg-surplus">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Nitrate-threshold">
      <value value="27500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="farm-size-threshold">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy-option-5-emission-taxes">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="low-profit-years-needed">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidies-crop-FIT">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="GHG-tax">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="waiting-time-for-intensity-change">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="minimum-profit">
      <value value="5000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="world-setup">
      <value value="&quot;random-world&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidy-reduction-per-ha">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="random-seed">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy-option-3-choose-subsidy-heights">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidies-grass-ext-FIT">
      <value value="350"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="GHG-threshold">
      <value value="14000"/>
    </enumeratedValueSet>
    <steppedValueSet variable="Nitrate-tax" first="0" step="1" last="40"/>
    <enumeratedValueSet variable="policy-option-4-redistributive-subsidies">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percentage-checked">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Norg-ext-threshold-FIT">
      <value value="110"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="minimum-grassland-share">
      <value value="80"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-market-stochasticity">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidies-grass-int-FIT">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Norg-limit-FIT">
      <value value="170"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy-option-2-ban-grassland-conversion">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="low-profit-years-needed" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>water-quality-index</metric>
    <metric>climate-regulation-index</metric>
    <metric>habitat-quality-index</metric>
    <metric>sum [milk-volume] of dairy-farms with [intensity-scale = 3 OR intensity-scale = 4]</metric>
    <metric>sum [milk-volume] of dairy-farms with [intensity-scale = 1 OR intensity-scale = 2]</metric>
    <metric>sum [milk-volume] of dairy-farms</metric>
    <metric>soil-fertility-index</metric>
    <enumeratedValueSet variable="policy-option-1-Norg-surplus-fine">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="perc-best-strategy">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidy-reduction-Norg-surplus">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Nitrate-threshold">
      <value value="27500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="farm-size-threshold">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy-option-5-emission-taxes">
      <value value="false"/>
    </enumeratedValueSet>
    <steppedValueSet variable="low-profit-years-needed" first="1" step="1" last="20"/>
    <enumeratedValueSet variable="subsidies-crop-FIT">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="GHG-tax">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="waiting-time-for-intensity-change">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="minimum-profit">
      <value value="5000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="world-setup">
      <value value="&quot;random-world&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidy-reduction-per-ha">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy-option-3-choose-subsidy-heights">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidies-grass-ext-FIT">
      <value value="350"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="GHG-threshold">
      <value value="14000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Nitrate-tax">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy-option-4-redistributive-subsidies">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percentage-checked">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Norg-ext-threshold-FIT">
      <value value="110"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="minimum-grassland-share">
      <value value="80"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-market-stochasticity">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidies-grass-int-FIT">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Norg-limit-FIT">
      <value value="170"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy-option-2-ban-grassland-conversion">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="random-seed">
      <value value="100"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="minimum-profit" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>water-quality-index</metric>
    <metric>climate-regulation-index</metric>
    <metric>habitat-quality-index</metric>
    <metric>sum [milk-volume] of dairy-farms with [intensity-scale = 3 OR intensity-scale = 4]</metric>
    <metric>sum [milk-volume] of dairy-farms with [intensity-scale = 1 OR intensity-scale = 2]</metric>
    <metric>sum [milk-volume] of dairy-farms</metric>
    <metric>soil-fertility-index</metric>
    <enumeratedValueSet variable="policy-option-1-Norg-surplus-fine">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="perc-best-strategy">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidy-reduction-Norg-surplus">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Nitrate-threshold">
      <value value="27500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="farm-size-threshold">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy-option-5-emission-taxes">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="low-profit-years-needed">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidies-crop-FIT">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="GHG-tax">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="waiting-time-for-intensity-change">
      <value value="0"/>
    </enumeratedValueSet>
    <steppedValueSet variable="minimum-profit" first="1000" step="1000" last="40000"/>
    <enumeratedValueSet variable="world-setup">
      <value value="&quot;random-world&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidy-reduction-per-ha">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy-option-3-choose-subsidy-heights">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidies-grass-ext-FIT">
      <value value="350"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="GHG-threshold">
      <value value="14000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Nitrate-tax">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy-option-4-redistributive-subsidies">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percentage-checked">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Norg-ext-threshold-FIT">
      <value value="110"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="minimum-grassland-share">
      <value value="80"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-market-stochasticity">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidies-grass-int-FIT">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Norg-limit-FIT">
      <value value="170"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy-option-2-ban-grassland-conversion">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="random-seed">
      <value value="100"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="perc-best-strategy" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>water-quality-index</metric>
    <metric>climate-regulation-index</metric>
    <metric>habitat-quality-index</metric>
    <metric>sum [milk-volume] of dairy-farms with [intensity-scale = 3 OR intensity-scale = 4]</metric>
    <metric>sum [milk-volume] of dairy-farms with [intensity-scale = 1 OR intensity-scale = 2]</metric>
    <metric>sum [milk-volume] of dairy-farms</metric>
    <metric>soil-fertility-index</metric>
    <enumeratedValueSet variable="policy-option-1-Norg-surplus-fine">
      <value value="false"/>
    </enumeratedValueSet>
    <steppedValueSet variable="perc-best-strategy" first="10" step="2" last="90"/>
    <enumeratedValueSet variable="subsidy-reduction-Norg-surplus">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Nitrate-threshold">
      <value value="27500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="farm-size-threshold">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy-option-5-emission-taxes">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="low-profit-years-needed">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidies-crop-FIT">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="GHG-tax">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="waiting-time-for-intensity-change">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="minimum-profit">
      <value value="5000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="world-setup">
      <value value="&quot;random-world&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidy-reduction-per-ha">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy-option-3-choose-subsidy-heights">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidies-grass-ext-FIT">
      <value value="350"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="GHG-threshold">
      <value value="14000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Nitrate-tax">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy-option-4-redistributive-subsidies">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percentage-checked">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Norg-ext-threshold-FIT">
      <value value="110"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="minimum-grassland-share">
      <value value="80"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-market-stochasticity">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidies-grass-int-FIT">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Norg-limit-FIT">
      <value value="170"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy-option-2-ban-grassland-conversion">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="random-seed">
      <value value="100"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="new-input-costs-new-values" repetitions="10" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>count dairy-farms with [milk-volume &lt; 100000] / count dairy-farms * 100</metric>
    <metric>count dairy-farms with [milk-volume &gt;= 100000 AND milk-volume &lt; 150000] / count dairy-farms * 100</metric>
    <metric>count dairy-farms with [milk-volume &gt;= 150000 AND milk-volume &lt; 200000] / count dairy-farms * 100</metric>
    <metric>count dairy-farms with [milk-volume &gt;= 200000 AND milk-volume &lt; 250000] / count dairy-farms * 100</metric>
    <metric>count dairy-farms with [milk-volume &gt;= 250000 AND milk-volume &lt; 300000] / count dairy-farms * 100</metric>
    <metric>count dairy-farms with [milk-volume &gt; 300000] / count dairy-farms * 100</metric>
    <metric>mean [farm-size] of dairy-farms with [milk-volume &lt; 100000]</metric>
    <metric>mean [farm-size] of dairy-farms with [milk-volume &gt;= 100000 AND milk-volume &lt; 150000]</metric>
    <metric>mean [farm-size] of dairy-farms with [milk-volume &gt;= 150000 AND milk-volume &lt; 200000]</metric>
    <metric>mean [farm-size] of dairy-farms with [milk-volume &gt;= 200000 AND milk-volume &lt; 250000]</metric>
    <metric>mean [farm-size] of dairy-farms with [milk-volume &gt;= 250000 AND milk-volume &lt; 300000]</metric>
    <metric>mean [farm-size] of dairy-farms with [milk-volume &gt; 300000]</metric>
    <metric>mean [annual-profit] of dairy-farms with [milk-volume &lt; 100000]</metric>
    <metric>mean [annual-profit] of dairy-farms with [milk-volume &gt;= 100000 AND milk-volume &lt; 150000]</metric>
    <metric>mean [annual-profit] of dairy-farms with [milk-volume &gt;= 150000 AND milk-volume &lt; 200000]</metric>
    <metric>mean [annual-profit] of dairy-farms with [milk-volume &gt;= 200000 AND milk-volume &lt; 250000]</metric>
    <metric>mean [annual-profit] of dairy-farms with [milk-volume &gt;= 250000 AND milk-volume &lt; 300000]</metric>
    <metric>mean [annual-profit] of dairy-farms with [milk-volume &gt; 300000]</metric>
    <metric>count dairy-farms with [intensity-scale = 1] / count dairy-farms * 100</metric>
    <metric>count dairy-farms with [intensity-scale = 2] / count dairy-farms * 100</metric>
    <metric>count dairy-farms with [intensity-scale = 3] / count dairy-farms * 100</metric>
    <metric>count dairy-farms with [intensity-scale = 4] / count dairy-farms * 100</metric>
    <metric>count dairy-farms with [size-class = "small"] / count dairy-farms * 100</metric>
    <metric>count dairy-farms with [size-class = "medium"] / count dairy-farms * 100</metric>
    <metric>count dairy-farms with [size-class = "large"] / count dairy-farms * 100</metric>
    <metric>count dairy-farms with [size-class = "extralarge"] / count dairy-farms * 100</metric>
    <metric>mean [received-subsidies] of dairy-farms</metric>
    <metric>mean [annual-profit] of dairy-farms</metric>
    <enumeratedValueSet variable="policy-option-1-Norg-surplus-fine">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="perc-best-strategy">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidy-reduction-Norg-surplus">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Nitrate-threshold">
      <value value="27500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="farm-size-threshold">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy-option-5-emission-taxes">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="low-profit-years-needed">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidies-crop-FIT">
      <value value="300"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="GHG-tax">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="waiting-time-for-intensity-change">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="world-setup">
      <value value="&quot;random-world&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="minimum-profit">
      <value value="5000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy-option-3-choose-subsidy-heights">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidy-reduction-per-ha">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidies-grass-ext-FIT">
      <value value="350"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="GHG-threshold">
      <value value="14000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Nitrate-tax">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy-option-4-redistributive-subsidies">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percentage-checked">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Norg-ext-threshold-FIT">
      <value value="110"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="minimum-grassland-share">
      <value value="95"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-market-stochasticity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidies-grass-int-FIT">
      <value value="300"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy-option-2-ban-grassland-conversion">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Norg-limit-FIT">
      <value value="170"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment" repetitions="10" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="30"/>
    <metric>water-quality-index</metric>
    <enumeratedValueSet variable="policy-option-1-Norg-surplus-fine">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidy-reduction-Norg-surplus">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidies-grass-extra-FIT">
      <value value="630"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="accepted-share-of-best-strategy">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Nitrate-threshold">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="farm-size-threshold">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy-option-5-emission-taxes">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="low-profit-years-needed">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidies-crop-FIT">
      <value value="300"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="GHG-tax">
      <value value="32"/>
    </enumeratedValueSet>
    <steppedValueSet variable="waiting-time-for-intensity-change" first="3" step="1" last="10"/>
    <enumeratedValueSet variable="world-setup">
      <value value="&quot;gis-world&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="minimum-profit">
      <value value="10000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy-option-3-choose-subsidy-heights">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidy-reduction-per-ha">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="GHG-threshold">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Nitrate-tax">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy-option-4-redistributive-subsidies">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percentage-checked">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidies-grass-FIT">
      <value value="300"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Norg-ext-threshold-FIT">
      <value value="110"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="minimum-grassland-share">
      <value value="95"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-market-stochasticity">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Norg-limit-FIT">
      <value value="170"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy-option-2-ban-grassland-conversion">
      <value value="true"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment" repetitions="100" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>soil-fertility-index</metric>
    <enumeratedValueSet variable="policy-option-1-Norg-surplus-fine">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidy-reduction-Norg-surplus">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidies-grass-extra-FIT">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="accepted-share-of-best-strategy">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Nitrate-threshold">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="farm-size-threshold">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy-option-5-emission-taxes">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="low-profit-years-needed">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidies-crop-FIT">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="GHG-tax">
      <value value="402"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="waiting-time-for-intensity-change">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="world-setup">
      <value value="&quot;random-world&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="minimum-profit">
      <value value="5000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidy-reduction-per-ha">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy-option-3-choose-subsidy-heights">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="GHG-threshold">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Nitrate-tax">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy-option-4-redistributive-subsidies">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percentage-checked">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidies-grass-FIT">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Norg-ext-threshold-FIT">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="minimum-grassland-share">
      <value value="51"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-market-stochasticity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Norg-limit-FIT">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy-option-2-ban-grassland-conversion">
      <value value="true"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="division-by-zero" repetitions="100" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>soil-fertility-index</metric>
    <enumeratedValueSet variable="policy-option-1-Norg-surplus-fine">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidy-reduction-Norg-surplus">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidies-grass-extra-FIT">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="accepted-share-of-best-strategy">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Nitrate-threshold">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="farm-size-threshold">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy-option-5-emission-taxes">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="low-profit-years-needed">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidies-crop-FIT">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="GHG-tax">
      <value value="402"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="waiting-time-for-intensity-change">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="world-setup">
      <value value="&quot;random-world&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="minimum-profit">
      <value value="5000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidy-reduction-per-ha">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy-option-3-choose-subsidy-heights">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="GHG-threshold">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Nitrate-tax">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy-option-4-redistributive-subsidies">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percentage-checked">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidies-grass-FIT">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Norg-ext-threshold-FIT">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="minimum-grassland-share">
      <value value="51"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-market-stochasticity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Norg-limit-FIT">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="policy-option-2-ban-grassland-conversion">
      <value value="true"/>
    </enumeratedValueSet>
  </experiment>
</experiments>
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
