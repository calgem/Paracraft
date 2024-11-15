#########################################################################################
# Installing required packages                                                          #
# (Put "#" in front of "install.packages()" after first run to make later runs quicker) #
#########################################################################################
install.packages("readxl")
install.packages("writexl")
install.packages("sf")
install.packages("sfheaders")
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("ggpubr")
install.packages("tidyr")
install.packages("scatterpie")
install.packages("ggspatial")
install.packages("plotly")
install.packages("gridExtra")

################################
# Unpacking required libraries #
################################
library(readxl)
library(writexl)
library(sf)
library(sfheaders)
library(dplyr)
library(data.table)
library(ggplot2)
library(ggpubr) 
library(tidyr)
library(scatterpie)
library(ggspatial)
library(plotly)
library(gridExtra)

########################
# Setting up directory #
########################
setwd("/Users/tamimzaki/Desktop/H2Hub_example")                                 # Copy-paste the path for the chosen directory

# Directory creation for the results
dir.create(file.path(getwd(), "Results"))
dir.create(file.path(getwd(), "Results/Producer"))
dir.create(file.path(getwd(), "Results/Producer/Emissions_CO2"))
dir.create(file.path(getwd(), "Results/Producer/Emissions_other"))
dir.create(file.path(getwd(), "Results/Producer/Costs"))
dir.create(file.path(getwd(), "Results"))
dir.create(file.path(getwd(), "Results/Enduser"))
dir.create(file.path(getwd(), "Results/Enduser/Emissions_CO2"))
dir.create(file.path(getwd(), "Results/Enduser/Emissions_other"))

######################
# Reading input data #
######################
LCA_parameter <- read_excel("H2hub_input.xlsx", sheet = "LCA parameters")       # required LCA parameters
TEA_parameter <- read_excel("H2hub_input.xlsx", sheet = "TEA parameters")       # required TEA parameters
Producer <- read_excel("H2hub_input.xlsx", sheet = "Producer")                  # user-defined producer data
Supply <- read_excel("H2hub_input.xlsx", sheet = "Supply")                      # user-defined supply data
Enduser <- read_excel("H2hub_input.xlsx", sheet = "Enduser")                    # user-defined enduser data
Demand <- read_excel("H2hub_input.xlsx", sheet = "Demand")                      # user-defined demand data
producer_data <- read_excel("H2hub_input.xlsx", sheet = "Producer")[,c(1:3)]    # user-defined producer location data
enduser_data <- read_excel("H2hub_input.xlsx", sheet = "Enduser")[,c(1:4)]      # user-defined enduser location data

####################
# Getting GIS data #
####################
ca_bound <- st_read(paste0(getwd(), "/Geodata/California_State_Boundary-shp/f067d2f7-7950-4e16-beba-8972d811599c2020329-1-18infjv.25og.shp"))         # exporting California state polygon
utility <- st_read(paste0(getwd(), "/Geodata//Electric_Utilities/Electric_Utilities.shp")) %>%                                                        # exporting utility polygons
            st_zm() %>% 
            filter(!Acronym == "PWRPA")                                                                                                               # skipping this utility because it overlaps with PG&E
region <- st_read(paste0(getwd(), "/Geodata/H2hub_Regions/H2hub_regions.shp"))                                                                        # exporting H2hub region polygons
senate <- st_read(paste0(getwd(), "/Geodata/Senate_Districts/Senate_Districts.shp"))                                                                  # exporting senate districts polygons
congress <- st_read(paste0(getwd(), "/Geodata/US_Congressional_Districts/US_Congressional_Districts.shp"))                                            # exporting congressional districts polygons
assembly <- st_read(paste0(getwd(), "/Geodata/CA_Assembly_District_Boundaries_Outline_2022/CA_Assembly_District_Boundaries_Outline_2022.shp"))        # exporting assembly districts polygons

##########################################################
# Creating blank space for populating with final outputs #
##########################################################
# Supply
H2prod_all <- Supply[,-1]                                                             # H2 production by all producers
H2prod_NorCal <- data.frame(matrix(0, ncol = ncol(Supply)-1, nrow = nrow(Supply)))    # H2 production by NorCal producers
H2prod_NCV <- data.frame(matrix(0, ncol = ncol(Supply)-1, nrow = nrow(Supply)))       # H2 production by NCV producers
H2prod_SCV <- data.frame(matrix(0, ncol = ncol(Supply)-1, nrow = nrow(Supply)))       # H2 production by SCV producers
H2prod_SoCal <- data.frame(matrix(0, ncol = ncol(Supply)-1, nrow = nrow(Supply)))     # H2 production by SoCal producers

CO2_maint_all <- data.frame(matrix(0, 1, nrow = nrow(Supply)))                  # CO2 emissions from O&M other than electricity use for all producers  
CO2_maint_NorCal <- data.frame(matrix(0, 1, nrow = nrow(Supply)))               # CO2 emissions from O&M other than electricity use for NorCal producers
CO2_maint_NCV <- data.frame(matrix(0, 1, nrow = nrow(Supply)))                  # CO2 emissions from O&M other than electricity use for NCV producers
CO2_maint_SCV <- data.frame(matrix(0, 1, nrow = nrow(Supply)))                  # CO2 emissions from O&M other than electricity use for SCV producers
CO2_maint_SoCal <- data.frame(matrix(0, 1, nrow = nrow(Supply)))                # CO2 emissions from O&M other than electricity use for SoCal producers

CO2_feedstock_all <- data.frame(matrix(0, 1, nrow = nrow(Supply)))              # CO2 emissions from feedstock supply and preparation for all producers
CO2_feedstock_NorCal <- data.frame(matrix(0, 1, nrow = nrow(Supply)))           # CO2 emissions from feedstock supply and preparation for NorCal producers
CO2_feedstock_NCV <- data.frame(matrix(0, 1, nrow = nrow(Supply)))              # CO2 emissions from feedstock supply and preparation for NCV producers
CO2_feedstock_SCV <- data.frame(matrix(0, 1, nrow = nrow(Supply)))              # CO2 emissions from feedstock supply and preparation for SCV producers
CO2_feedstock_SoCal <- data.frame(matrix(0, 1, nrow = nrow(Supply)))            # CO2 emissions from feedstock supply and preparation for SoCal producers

CO2_storage_all <- data.frame(matrix(0, 1, nrow = nrow(Supply)))                # CO2 storage from biomass for all producers
CO2_storage_NorCal <- data.frame(matrix(0, 1, nrow = nrow(Supply)))             # CO2 storage from biomass for NorCal producers
CO2_storage_NCV <- data.frame(matrix(0, 1, nrow = nrow(Supply)))                # CO2 storage from biomass for NCV producers
CO2_storage_SCV <- data.frame(matrix(0, 1, nrow = nrow(Supply)))                # CO2 storage from biomass for SCV producers
CO2_storage_SoCal <- data.frame(matrix(0, 1, nrow = nrow(Supply)))              # CO2 storage from biomass for SoCal producers

CO2_elec_all <- data.frame(matrix(0, 1, nrow = nrow(Supply)))                   # CO2 emissions from electricity use for all producers
CO2_elec_NorCal <- data.frame(matrix(0, 1, nrow = nrow(Supply)))                # CO2 emissions from electricity use for NorCal producers
CO2_elec_NCV <- data.frame(matrix(0, 1, nrow = nrow(Supply)))                   # CO2 emissions from electricity use for NCV producers
CO2_elec_SCV <- data.frame(matrix(0, 1, nrow = nrow(Supply)))                   # CO2 emissions from electricity use for SCV producers
CO2_elec_SoCal <- data.frame(matrix(0, 1, nrow = nrow(Supply)))                 # CO2 emissions from electricity use for SoCal producers

SO2_elec_all <- data.frame(matrix(0, 1, nrow = nrow(Supply)))                   # SO2 emissions from electricity use for all producers
NOx_elec_all <- data.frame(matrix(0, 1, nrow = nrow(Supply)))                   # NOx emissions from electricity use for all producers
PM10_elec_all <- data.frame(matrix(0, 1, nrow = nrow(Supply)))                  # PM10 emissions from electricity use for all producers

CO2_H2liqcomp_all <- data.frame(matrix(0, 1, nrow = nrow(Supply)))              # CO2 emissions from H2 liquefaction and compression for all producers
CO2_H2liqcomp_NorCal <- data.frame(matrix(0, 1, nrow = nrow(Supply)))           # CO2 emissions from H2 liquefaction and compression for NorCal producers
CO2_H2liqcomp_NCV <- data.frame(matrix(0, 1, nrow = nrow(Supply)))              # CO2 emissions from H2 liquefaction and compression for NCV producers
CO2_H2liqcomp_SCV <- data.frame(matrix(0, 1, nrow = nrow(Supply)))              # CO2 emissions from H2 liquefaction and compression for SCV producers
CO2_H2liqcomp_SoCal <- data.frame(matrix(0, 1, nrow = nrow(Supply)))            # CO2 emissions from H2 liquefaction and compression for SoCal producers

CO2_H2trans_all <- data.frame(matrix(0, 1, nrow = nrow(Supply)))                # CO2 emissions from H2 transportation for all producers
CO2_H2trans_NorCal <- data.frame(matrix(0, 1, nrow = nrow(Supply)))             # CO2 emissions from H2 transportation for NorCal producers
CO2_H2trans_NCV <- data.frame(matrix(0, 1, nrow = nrow(Supply)))                # CO2 emissions from H2 transportation for NCV producers
CO2_H2trans_SCV <- data.frame(matrix(0, 1, nrow = nrow(Supply)))                # CO2 emissions from H2 transportation for SCV producers
CO2_H2trans_SoCal <- data.frame(matrix(0, 1, nrow = nrow(Supply)))              # CO2 emissions from H2 transportation for SoCal producers

capex_install_all <- data.frame(matrix(0, 1, nrow = nrow(Supply)))              # capital cost of installed equipment for all producers
capex_NorCal <- data.frame(matrix(0, 1, nrow = nrow(Supply)))                   # capital cost of installed equipment for NorCal producers
capex_NCV <- data.frame(matrix(0, 1, nrow = nrow(Supply)))                      # capital cost of installed equipment for NCV producers
capex_SCV <- data.frame(matrix(0, 1, nrow = nrow(Supply)))                      # capital cost of installed equipment for SCV producers
capex_SoCal <- data.frame(matrix(0, 1, nrow = nrow(Supply)))                    # capital cost of installed equipment for SoCal producers

cost_OM_insure_all <- data.frame(matrix(0, 1, nrow = nrow(Supply)))             # cost of O&M and insurance for all producers
cost_OM_NorCal <- data.frame(matrix(0, 1, nrow = nrow(Supply)))                 # cost of O&M and insurance for NorCal producers
cost_OM_NCV <- data.frame(matrix(0, 1, nrow = nrow(Supply)))                    # cost of O&M and insurance for NCV producers
cost_OM_SCV <- data.frame(matrix(0, 1, nrow = nrow(Supply)))                    # cost of O&M and insurance for SCV producers
cost_OM_SoCal <- data.frame(matrix(0, 1, nrow = nrow(Supply)))                  # cost of O&M and insurance for SoCal producers

cost_other_vars_all <- data.frame(matrix(0, 1, nrow = nrow(Supply)))            # cost of other O&M such as feedstock, labor, and liquefier maintenance for all producers
cost_other_NorCal <- data.frame(matrix(0, 1, nrow = nrow(Supply)))              # cost of other O&M such as feedstock, labor, and liquefier maintenance NorCal all producers
cost_other_NCV <- data.frame(matrix(0, 1, nrow = nrow(Supply)))                 # cost of other O&M such as feedstock, labor, and liquefier maintenance NCV all producers
cost_other_SCV <- data.frame(matrix(0, 1, nrow = nrow(Supply)))                 # cost of other O&M such as feedstock, labor, and liquefier maintenance SCV all producers
cost_other_SoCal <- data.frame(matrix(0, 1, nrow = nrow(Supply)))               # cost of other O&M such as feedstock, labor, and liquefier maintenance SoCal all producers

cost_elec_all <- data.frame(matrix(0, 1, nrow = nrow(Supply)))                  # cost of electricity use for all producers
cost_elec_NorCal <- data.frame(matrix(0, 1, nrow = nrow(Supply)))               # cost of electricity use for NorCal producers
cost_elec_NCV <- data.frame(matrix(0, 1, nrow = nrow(Supply)))                  # cost of electricity use for NCV producers
cost_elec_SCV <- data.frame(matrix(0, 1, nrow = nrow(Supply)))                  # cost of electricity use for SCV producers
cost_elec_SoCal <- data.frame(matrix(0, 1, nrow = nrow(Supply)))                # cost of electricity use for SoCal producers

cost_H2trans_all <- data.frame(matrix(0, 1, nrow = nrow(Supply)))               # cost of H2 transportation for all producers
cost_H2trans_NorCal <- data.frame(matrix(0, 1, nrow = nrow(Supply)))            # cost of H2 transportation for NorCal producers
cost_H2trans_NCV <- data.frame(matrix(0, 1, nrow = nrow(Supply)))               # cost of H2 transportation for NCV producers
cost_H2trans_SCV <- data.frame(matrix(0, 1, nrow = nrow(Supply)))               # cost of H2 transportation for SCV producers
cost_H2trans_SoCal <- data.frame(matrix(0, 1, nrow = nrow(Supply)))             # cost of H2 transportation for SoCal producers

cost_decomm_all <- data.frame(matrix(0, 1, nrow = nrow(Supply)))                # cost of equipment decommissioning for all producers
cost_decomm_NorCal <- data.frame(matrix(0, 1, nrow = nrow(Supply)))             # cost of equipment decommissioning for NorCal producers
cost_decomm_NCV <- data.frame(matrix(0, 1, nrow = nrow(Supply)))                # cost of equipment decommissioning for NCV producers
cost_decomm_SCV <- data.frame(matrix(0, 1, nrow = nrow(Supply)))                # cost of equipment decommissioning for SCV producers
cost_decomm_SoCal <- data.frame(matrix(0, 1, nrow = nrow(Supply)))              # cost of equipment decommissioning for SoCal producers

CO2_net_all <- data.frame(matrix(0, 1, nrow = nrow(Supply)))                    # net CO2 emissions for all producers
CO2_net_NorCal <- data.frame(matrix(0, 1, nrow = nrow(Supply)))                 # net CO2 emissions for NorCal producers
CO2_net_NCV <- data.frame(matrix(0, 1, nrow = nrow(Supply)))                    # net CO2 emissions for NCV producers
CO2_net_SCV <- data.frame(matrix(0, 1, nrow = nrow(Supply)))                    # net CO2 emissions for SCV producers
CO2_net_SoCal <- data.frame(matrix(0, 1, nrow = nrow(Supply)))                  # net CO2 emissions for SoCal producers

SO2_net_NorCal <- data.frame(matrix(0, 1, nrow = nrow(Supply)))                 # net SO2 emissions for NorCal producers
SO2_net_NCV <- data.frame(matrix(0, 1, nrow = nrow(Supply)))                    # net SO2 emissions for NCV producers
SO2_net_SCV <- data.frame(matrix(0, 1, nrow = nrow(Supply)))                    # net SO2 emissions for SCV producers
SO2_net_SoCal <- data.frame(matrix(0, 1, nrow = nrow(Supply)))                  # net SO2 emissions for SoCal producers

NOx_net_NorCal <- data.frame(matrix(0, 1, nrow = nrow(Supply)))                 # net NOx emissions for NorCal producers
NOx_net_NCV <- data.frame(matrix(0, 1, nrow = nrow(Supply)))                    # net NOx emissions for NCV producers
NOx_net_SCV <- data.frame(matrix(0, 1, nrow = nrow(Supply)))                    # net NOx emissions for SCV producers
NOx_net_SoCal <- data.frame(matrix(0, 1, nrow = nrow(Supply)))                  # net NOx emissions for SoCal producers

PM10_net_NorCal <- data.frame(matrix(0, 1, nrow = nrow(Supply)))                # net PM10 emissions for NorCal producers
PM10_net_NCV <- data.frame(matrix(0, 1, nrow = nrow(Supply)))                   # net PM10 emissions for NCV producers
PM10_net_SCV <- data.frame(matrix(0, 1, nrow = nrow(Supply)))                   # net PM10 emissions for SCV producers
PM10_net_SoCal <- data.frame(matrix(0, 1, nrow = nrow(Supply)))                 # net PM10 emissions for SoCal producers

cost_total_all <- data.frame(matrix(0, 1, nrow = nrow(Supply)))                 # Total costs for all producers
cost_total_NorCal <- data.frame(matrix(0, 1, nrow = nrow(Supply)))              # Total costs for NorCal producers
cost_total_NCV <- data.frame(matrix(0, 1, nrow = nrow(Supply)))                 # Total costs for NCV producers
cost_total_SCV <- data.frame(matrix(0, 1, nrow = nrow(Supply)))                 # Total costs for SCV producers
cost_total_SoCal <- data.frame(matrix(0, 1, nrow = nrow(Supply)))               # Total costs for SoCal producers

CO2_time_NorCal <- data.frame(matrix(0, ncol = ncol(Supply)-1, 1))              # Temporal CO2 emissions for NorCal producers
CO2_time_NCV <- data.frame(matrix(0, ncol = ncol(Supply)-1, 1))                 # Temporal CO2 emissions for NCV producers
CO2_time_SCV <- data.frame(matrix(0, ncol = ncol(Supply)-1, 1))                 # Temporal CO2 emissions for SCV producers
CO2_time_SoCal <- data.frame(matrix(0, ncol = ncol(Supply)-1, 1))               # Temporal CO2 emissions for SoCal producers

cost_time_NorCal <- data.frame(matrix(0, ncol = ncol(Supply)-1, 1))             # Temporal costs for NorCal producers
cost_time_NCV <- data.frame(matrix(0, ncol = ncol(Supply)-1, 1))                # Temporal costs for NCV producers
cost_time_SCV <- data.frame(matrix(0, ncol = ncol(Supply)-1, 1))                # Temporal costs for SCV producers
cost_time_SoCal <- data.frame(matrix(0, ncol = ncol(Supply)-1, 1))              # Temporal costs for SoCal producers

#demand
H2dem_all <- Demand[,-1]                                                            # H2 demand by all endusers
H2dem_NorCal <- data.frame(matrix(0, ncol = ncol(Demand)-1, nrow = nrow(Demand)))   # H2 demand by NorCal endusers
H2dem_NCV <- data.frame(matrix(0, ncol = ncol(Demand)-1, nrow = nrow(Demand)))      # H2 demand by NCV endusers
H2dem_SCV <- data.frame(matrix(0, ncol = ncol(Demand)-1, nrow = nrow(Demand)))      # H2 demand by SCV endusers
H2dem_SoCal <- data.frame(matrix(0, ncol = ncol(Demand)-1, nrow = nrow(Demand)))    # H2 demand by SoCal endusers

CO2_avoid_all <- data.frame(matrix(0, 1, nrow = nrow(Demand)))                  # CO2 emissions avoided by all endusers
CO2_avoid_NorCal <- data.frame(matrix(0, 1, nrow = nrow(Demand)))               # CO2 emissions avoided by NorCal endusers
CO2_avoid_NCV <- data.frame(matrix(0, 1, nrow = nrow(Demand)))                  # CO2 emissions avoided by NCV endusers
CO2_avoid_SCV <- data.frame(matrix(0, 1, nrow = nrow(Demand)))                  # CO2 emissions avoided by SCV endusers
CO2_avoid_SoCal <- data.frame(matrix(0, 1, nrow = nrow(Demand)))                # CO2 emissions avoided by SoCal endusers

SO2_avoid_all <- data.frame(matrix(0, 1, nrow = nrow(Demand)))                  # SO2 emissions avoided by all endusers
SO2_avoid_NorCal <- data.frame(matrix(0, 1, nrow = nrow(Demand)))               # SO2 emissions avoided by NorCal endusers
SO2_avoid_NCV <- data.frame(matrix(0, 1, nrow = nrow(Demand)))                  # SO2 emissions avoided by NCV endusers
SO2_avoid_SCV <- data.frame(matrix(0, 1, nrow = nrow(Demand)))                  # SO2 emissions avoided by SCV endusers
SO2_avoid_SoCal <- data.frame(matrix(0, 1, nrow = nrow(Demand)))                # SO2 emissions avoided by SoCal endusers

NOx_avoid_all <- data.frame(matrix(0, 1, nrow = nrow(Demand)))                  # NOx emissions avoided by SoCal endusers
NOx_avoid_NorCal <- data.frame(matrix(0, 1, nrow = nrow(Demand)))               # NOx emissions avoided by NorCal endusers
NOx_avoid_NCV <- data.frame(matrix(0, 1, nrow = nrow(Demand)))                  # NOx emissions avoided by NCV endusers
NOx_avoid_SCV <- data.frame(matrix(0, 1, nrow = nrow(Demand)))                  # NOx emissions avoided by SCV endusers
NOx_avoid_SoCal <- data.frame(matrix(0, 1, nrow = nrow(Demand)))                # NOx emissions avoided by SoCal endusers

PM10_avoid_all <- data.frame(matrix(0, 1, nrow = nrow(Demand)))                 # PM10 emissions avoided by SoCal endusers
PM10_avoid_NorCal <- data.frame(matrix(0, 1, nrow = nrow(Demand)))              # PM10 emissions avoided by NorCal endusers
PM10_avoid_NCV <- data.frame(matrix(0, 1, nrow = nrow(Demand)))                 # PM10 emissions avoided by NCV endusers
PM10_avoid_SCV <- data.frame(matrix(0, 1, nrow = nrow(Demand)))                 # PM10 emissions avoided by SCV endusers
PM10_avoid_SoCal <- data.frame(matrix(0, 1, nrow = nrow(Demand)))               # PM10 emissions avoided by SoCal endusers

PM25_avoid_all <- data.frame(matrix(0, 1, nrow = nrow(Demand)))                 # PM2.5 emissions avoided by SoCal endusers
PM25_avoid_NorCal <- data.frame(matrix(0, 1, nrow = nrow(Demand)))              # PM2.5 emissions avoided by NorCal endusers
PM25_avoid_NCV <- data.frame(matrix(0, 1, nrow = nrow(Demand)))                 # PM2.5 emissions avoided by NCV endusers
PM25_avoid_SCV <- data.frame(matrix(0, 1, nrow = nrow(Demand)))                 # PM2.5 emissions avoided by SCV endusers
PM25_avoid_SoCal <- data.frame(matrix(0, 1, nrow = nrow(Demand)))               # PM2.5 emissions avoided by SoCal endusers

CO2_power_NorCal <- data.frame(matrix(0, 1, nrow = nrow(Demand)))               # CO2 emissions avoided by NorCal power endusers
CO2_power_NCV <- data.frame(matrix(0, 1, nrow = nrow(Demand)))                  # CO2 emissions avoided by NCV power endusers
CO2_power_SCV <- data.frame(matrix(0, 1, nrow = nrow(Demand)))                  # CO2 emissions avoided by SCV power endusers
CO2_power_SoCal <- data.frame(matrix(0, 1, nrow = nrow(Demand)))                # CO2 emissions avoided by SoCal power endusers

CO2_transit_NorCal <- data.frame(matrix(0, 1, nrow = nrow(Demand)))             # CO2 emissions avoided by NorCal transit endusers
CO2_transit_NCV <- data.frame(matrix(0, 1, nrow = nrow(Demand)))                # CO2 emissions avoided by NCV transit endusers
CO2_transit_SCV <- data.frame(matrix(0, 1, nrow = nrow(Demand)))                # CO2 emissions avoided by SCV transit endusers
CO2_transit_SoCal <- data.frame(matrix(0, 1, nrow = nrow(Demand)))              # CO2 emissions avoided by SoCal transit endusers

CO2_truck_NorCal <- data.frame(matrix(0, 1, nrow = nrow(Demand)))               # CO2 emissions avoided by NorCal truck endusers
CO2_truck_NCV <- data.frame(matrix(0, 1, nrow = nrow(Demand)))                  # CO2 emissions avoided by NCV truck endusers
CO2_truck_SCV <- data.frame(matrix(0, 1, nrow = nrow(Demand)))                  # CO2 emissions avoided by SCV truck endusers
CO2_truck_SoCal <- data.frame(matrix(0, 1, nrow = nrow(Demand)))                # CO2 emissions avoided by SoCal truck endusers

CO2_port_NorCal <- data.frame(matrix(0, 1, nrow = nrow(Demand)))                # CO2 emissions avoided by NorCal port endusers
CO2_port_NCV <- data.frame(matrix(0, 1, nrow = nrow(Demand)))                   # CO2 emissions avoided by NCV port endusers
CO2_port_SCV <- data.frame(matrix(0, 1, nrow = nrow(Demand)))                   # CO2 emissions avoided by SCV port endusers
CO2_port_SoCal <- data.frame(matrix(0, 1, nrow = nrow(Demand)))                 # CO2 emissions avoided by SoCal port endusers

CO2_aviation_NorCal <- data.frame(matrix(0, 1, nrow = nrow(Demand)))            # CO2 emissions avoided by NorCal aviation endusers
CO2_aviation_NCV <- data.frame(matrix(0, 1, nrow = nrow(Demand)))               # CO2 emissions avoided by NCV aviation endusers
CO2_aviation_SCV <- data.frame(matrix(0, 1, nrow = nrow(Demand)))               # CO2 emissions avoided by SCV aviation endusers
CO2_aviation_SoCal <- data.frame(matrix(0, 1, nrow = nrow(Demand)))             # CO2 emissions avoided by SoCal aviation endusers

CO2_time_NorCal_dem <- data.frame(matrix(0, ncol = ncol(Demand)-1, 1))          # temporal CO2 emissions avoided by NorCal endusers
CO2_time_NCV_dem <- data.frame(matrix(0, ncol = ncol(Demand)-1, 1))             # temporal CO2 emissions avoided by NCV endusers
CO2_time_SCV_dem <- data.frame(matrix(0, ncol = ncol(Demand)-1, 1))             # temporal CO2 emissions avoided by SCV endusers
CO2_time_SoCal_dem <- data.frame(matrix(0, ncol = ncol(Demand)-1, 1))           # temporal CO2 emissions avoided by SoCal endusers

###########################################
# Finding electric utilities for producer #
###########################################
producer_cord <- data.frame(producer_data) %>% 
                  st_as_sf(coords = c("Longitude", "Lattitude"), crs = 4326) %>% 
                  st_transform(st_crs(utility))                                           # Aligning coordinate systems

producer_with_utility <- st_join(producer_cord, utility, left = F) %>% # not left = inner join
                          select(Name, Utility = Acronym) %>%
                          st_drop_geometry()

###########################################
# Finding electric utilities for end-user #
###########################################
enduser_cord <- data.frame(enduser_data) %>% 
  st_as_sf(coords = c("Longitude", "Lattitude"), crs = 4326) %>% 
  st_transform(st_crs(utility))                                           # Aligning coordinate systems

enduser_with_utility <- st_join(enduser_cord, utility, left = F) %>% # not left = inner join
  select(Name, Utility = Acronym) %>%
  st_drop_geometry()

################################
# Finding regions of producers #
################################
producer_with_region <- st_join(producer_cord, region) %>% # not left = inner join
  select(Name, Region = H2_Region) %>%
  st_drop_geometry()

producer_NorCal_cord <- st_join(producer_cord, region) %>% # not left = inner join
  select(Name, Region = H2_Region) %>%
  filter(Region == "NorCal")

producer_NCV_cord <- st_join(producer_cord, region) %>% # not left = inner join
  select(Name, Region = H2_Region) %>%
  filter(Region == "NCV")

producer_SoCal_cord <- st_join(producer_cord, region) %>% # not left = inner join
  select(Name, Region = H2_Region) %>%
  filter(Region == "SoCal")

producer_SCV_cord <- st_join(producer_cord, region) %>% # not left = inner join
  select(Name, Region = H2_Region) %>%
  filter(Region == "SCV")

# producers with senate district
senate_name <- senate[,1] %>% sf::st_drop_geometry(senate_name)                 # list of senate districts
producer_senate_nolist <- list()                                                # blank list for counting districts with no producers
j <- 0                                                                          # districts with no producer counter
producer_senate_count <- data.frame(matrix(0, 1, nrow = nrow(senate)))          # districts with no producer
names(producer_senate_count)[1] <- "Producers"                                  # specifying header name

for(i in 1:nrow(senate)) {
  senate_ID <- senate_name[i,1]                                                 # senate district selection
  producer_senate_cord <- st_join(producer_cord, senate) %>%                    # not left = inner join
    select(Name, Region = GEOID) %>%
    filter(Region == senate_ID)
  producer_senate_count[i,1] <- nrow(producer_senate_cord)                      # allocating producer with senate district
  if (nrow(producer_senate_cord) > 0) {
    assign(paste0("producer_S",i,"_cord"), producer_senate_cord)                # creating variables with changing names based on selected senate districts
  } else {
    j <- j + 1
    producer_senate_nolist[j] <- i                                              # counting districts with no producers
  }
}

senate_producer <- cbind(senate, producer_senate_count)                         # combining senate district list with corresponding number of producers
producer_senate_cord_all <- senate_producer %>%  filter(!row_number() %in% c(producer_senate_nolist))              # removing senate districts with no producers from the main list
producer_senate_all <- producer_senate_cord_all[,c(1,5)] %>% sf::st_drop_geometry(producer_senate_cord_all)        # removing geographic identity
producer_senate_out <- data.frame(matrix(NA, 6, nrow = nrow(producer_senate_all)))                                 # creating blank data space for storing district weighted means of final outputs
producer_senate_all <- cbind(producer_senate_all, producer_senate_out)          # combining senate districts with producers with blank results list 
names(producer_senate_all)[1] <- "Senate District"                              # renaming header (same as follows)
names(producer_senate_all)[3] <- "Production (kg/day)"
names(producer_senate_all)[4] <- "Net CO2 emissions\n(kg/kg)"
names(producer_senate_all)[5] <- "Net SO2 emissions\n(g/kg)"
names(producer_senate_all)[6] <- "Net NOx emissions\n(g/kg)"
names(producer_senate_all)[7] <- "Net PM10 emissions\n(g/kg)"
names(producer_senate_all)[8] <- "Total cost ($/kg)"

# producers with congressional district 
# (please see comments on the "producers with senate district" section for explanation on the algorithm)
congress_name <- congress[,1] %>% sf::st_drop_geometry(congress_name)
producer_congress_nolist <- list()
j <- 0
producer_congress_count <- data.frame(matrix(0, 1, nrow = nrow(congress)))
names(producer_congress_count)[1] <- "Producers"

for(i in 1:nrow(congress)) {
  congress_ID <- congress_name[i,1]
  producer_congress_cord <- st_join(producer_cord, congress) %>% # not left = inner join
                            select(Name, Region = GEOID) %>%
                            filter(Region == congress_ID)
  producer_congress_count[i,1] <- nrow(producer_congress_cord)
  if (nrow(producer_congress_cord) > 0) {
    assign(paste0("producer_C",i,"_cord"), producer_congress_cord)
  } else {
    j <- j + 1
    producer_congress_nolist[j] <- i
  }
}

congress_producer <- cbind(congress, producer_congress_count)
producer_congress_cord_all <- congress_producer %>%  filter(!row_number() %in% c(producer_congress_nolist))
producer_congress_all <- producer_congress_cord_all[,c(1,5)] %>% sf::st_drop_geometry(producer_congress_cord_all)
producer_congress_out <- data.frame(matrix(NA, 6, nrow = nrow(producer_congress_all)))
producer_congress_all <- cbind(producer_congress_all, producer_congress_out)
names(producer_congress_all)[1] <- "Congressional District"
names(producer_congress_all)[3] <- "Production (kg/day)"
names(producer_congress_all)[4] <- "Net CO2 emissions\n(kg/kg)"
names(producer_congress_all)[5] <- "Net SO2 emissions\n(g/kg)"
names(producer_congress_all)[6] <- "Net NOx emissions\n(g/kg)"
names(producer_congress_all)[7] <- "Net PM10 emissions\n(g/kg)"
names(producer_congress_all)[8] <- "Total cost ($/kg)"

# producers with assembly district
# (please see comments on the "producers with senate district" section for explanation on the algorithm)
assembly_name <- assembly[,22] %>% sf::st_drop_geometry(assembly_name)
producer_assembly_nolist <- list()
j <- 0
producer_assembly_count <- data.frame(matrix(0, 1, nrow = nrow(assembly)))
names(producer_assembly_count)[1] <- "Producers"

for(i in 1:nrow(assembly)) {
  assembly_ID <- assembly_name[i,1]
  producer_assembly_cord <- st_join(producer_cord, assembly) %>% # not left = inner join
    select(Name, Region = district) %>%
    filter(Region == assembly_ID)
  producer_assembly_count[i,1] <- nrow(producer_assembly_cord)
  if (nrow(producer_assembly_cord) > 0) {
    assign(paste0("producer_A",assembly_ID,"_cord"), producer_assembly_cord)
  } else {
    j <- j + 1
    producer_assembly_nolist[j] <- i
  }
}

assembly_producer <- cbind(assembly, producer_assembly_count)
producer_assembly_cord_all <- assembly_producer %>%  filter(!row_number() %in% c(producer_assembly_nolist))
producer_assembly_all <- producer_assembly_cord_all[,c(22,26)] %>% sf::st_drop_geometry(producer_assembly_cord_all)
producer_assembly_out <- data.frame(matrix(NA, 6, nrow = nrow(producer_assembly_all)))
producer_assembly_all <- cbind(producer_assembly_all, producer_assembly_out)
names(producer_assembly_all)[1] <- "Assembly District"
names(producer_assembly_all)[3] <- "Production (kg/day)"
names(producer_assembly_all)[4] <- "Net CO2 emissions\n(kg/kg)"
names(producer_assembly_all)[5] <- "Net SO2 emissions\n(g/kg)"
names(producer_assembly_all)[6] <- "Net NOx emissions\n(g/kg)"
names(producer_assembly_all)[7] <- "Net PM10 emissions\n(g/kg)"
names(producer_assembly_all)[8] <- "Total cost ($/kg)"

###############################
# Finding regions of endusers #
###############################
enduser_cord <- data.frame(enduser_data) %>% 
  st_as_sf(coords = c("Longitude", "Lattitude"), crs = 4326) %>% 
  st_transform(st_crs(region))                                           # Aligning coordinate systems

enduser_with_region <- st_join(enduser_cord, region) %>% # not left = inner join
  select(Name, Region = H2_Region) %>%
  st_drop_geometry()

enduser_NorCal_cord <- st_join(enduser_cord, region) %>% # not left = inner join
  select(Name, Region = H2_Region) %>%
  filter(Region == "NorCal" | is.na(Region) == 1)

enduser_NCV_cord <- st_join(enduser_cord, region) %>% # not left = inner join
  select(Name, Region = H2_Region) %>%
  filter(Region == "NCV")

enduser_SoCal_cord <- st_join(enduser_cord, region) %>% # not left = inner join
  select(Name, Region = H2_Region) %>%
  filter(Region == "SoCal")

enduser_SCV_cord <- st_join(enduser_cord, region) %>% # not left = inner join
  select(Name, Region = H2_Region) %>%
  filter(Region == "SCV")

# endusers with senate district
# (please see comments on the "producers with senate district" section for explanation on the algorithm)
enduser_senate_nolist <- list()
j <- 0
enduser_senate_count <- data.frame(matrix(0, 1, nrow = nrow(senate)))
names(enduser_senate_count)[1] <- "Endusers"

for(i in 1:nrow(senate)) {
  senate_ID <- senate_name[i,1]
  enduser_senate_cord <- st_join(enduser_cord, senate) %>% # not left = inner join
    select(Name, Region = GEOID) %>%
    filter(Region == senate_ID)
  enduser_senate_count[i,1] <- nrow(enduser_senate_cord)
  if (nrow(enduser_senate_cord) > 0) {
    assign(paste0("enduser_S",i,"_cord"), enduser_senate_cord)
  } else {
    j <- j + 1
    enduser_senate_nolist[j] <- i
  }
}

senate_enduser <- cbind(senate, enduser_senate_count)
enduser_senate_cord_all <- senate_enduser %>%  filter(!row_number() %in% c(enduser_senate_nolist))
enduser_senate_all <- enduser_senate_cord_all[,c(1,5)] %>% sf::st_drop_geometry(enduser_senate_cord_all)
enduser_senate_out <- data.frame(matrix(NA, 6, nrow = nrow(enduser_senate_all)))
enduser_senate_all <- cbind(enduser_senate_all, enduser_senate_out)
names(enduser_senate_all)[1] <- "Senate District"
names(enduser_senate_all)[3] <- "Demand (kg/day)"
names(enduser_senate_all)[4] <- "CO2 emissions avoided (kg/kg)"
names(enduser_senate_all)[5] <- "SO2 emissions avoided (g/kg)"
names(enduser_senate_all)[6] <- "NOx emissions avoided (g/kg)"
names(enduser_senate_all)[7] <- "PM10 emissions avoided (g/kg)"
names(enduser_senate_all)[8] <- "PM2.5 emissions avoided (g/kg)"

# endusers with congressional district
# (please see comments on the "producers with senate district" section for explanation on the algorithm)
enduser_congress_nolist <- list()
j <- 0
enduser_congress_count <- data.frame(matrix(0, 1, nrow = nrow(congress)))
names(enduser_congress_count)[1] <- "Endusers"

for(i in 1:nrow(congress)) {
  congress_ID <- congress_name[i,1]
  enduser_congress_cord <- st_join(enduser_cord, congress) %>% # not left = inner join
    select(Name, Region = GEOID) %>%
    filter(Region == congress_ID)
  enduser_congress_count[i,1] <- nrow(enduser_congress_cord)
  if (nrow(enduser_congress_cord) > 0) {
    assign(paste0("enduser_C",i,"_cord"), enduser_congress_cord)
  } else {
    j <- j + 1
    enduser_congress_nolist[j] <- i
  }
}

congress_enduser <- cbind(congress, enduser_congress_count)
enduser_congress_cord_all <- congress_enduser %>%  filter(!row_number() %in% c(enduser_congress_nolist))
enduser_congress_all <- enduser_congress_cord_all[,c(1,5)] %>% sf::st_drop_geometry(enduser_congress_cord_all)
enduser_congress_out <- data.frame(matrix(NA, 6, nrow = nrow(enduser_congress_all)))
enduser_congress_all <- cbind(enduser_congress_all, enduser_congress_out)
names(enduser_congress_all)[1] <- "Congressional District"
names(enduser_congress_all)[3] <- "Demand (kg/day)"
names(enduser_congress_all)[4] <- "CO2 emissions avoided (kg/kg)"
names(enduser_congress_all)[5] <- "SO2 emissions avoided (g/kg)"
names(enduser_congress_all)[6] <- "NOx emissions avoided (g/kg)"
names(enduser_congress_all)[7] <- "PM10 emissions avoided (g/kg)"
names(enduser_congress_all)[8] <- "PM2.5 emissions avoided (g/kg)"

# producers with assembly district
# (please see comments on the "producers with senate district" section for explanation on the algorithm)
enduser_assembly_nolist <- list()
j <- 0
enduser_assembly_count <- data.frame(matrix(0, 1, nrow = nrow(assembly)))
names(enduser_assembly_count)[1] <- "Endusers"

for(i in 1:nrow(assembly)) {
  assembly_ID <- assembly_name[i,1]
  enduser_assembly_cord <- st_join(enduser_cord, assembly) %>% # not left = inner join
    select(Name, Region = district) %>%
    filter(Region == assembly_ID)
  enduser_assembly_count[i,1] <- nrow(enduser_assembly_cord)
  if (nrow(enduser_assembly_cord) > 0) {
    assign(paste0("enduser_A",assembly_ID,"_cord"), enduser_assembly_cord)
  } else {
    j <- j + 1
    enduser_assembly_nolist[j] <- i
  }
}

assembly_enduser <- cbind(assembly, enduser_assembly_count)
enduser_assembly_cord_all <- assembly_enduser %>%  filter(!row_number() %in% c(enduser_assembly_nolist))
enduser_assembly_all <- enduser_assembly_cord_all[,c(22,26)] %>% sf::st_drop_geometry(enduser_assembly_cord_all)
enduser_assembly_out <- data.frame(matrix(NA, 6, nrow = nrow(enduser_assembly_all)))
enduser_assembly_all <- cbind(enduser_assembly_all, enduser_assembly_out)
names(enduser_assembly_all)[1] <- "Assembly District"
names(enduser_assembly_all)[3] <- "Demand (kg/day)"
names(enduser_assembly_all)[4] <- "CO2 emissions avoided (kg/kg)"
names(enduser_assembly_all)[5] <- "SO2 emissions avoided (g/kg)"
names(enduser_assembly_all)[6] <- "NOx emissions avoided (g/kg)"
names(enduser_assembly_all)[7] <- "PM10 emissions avoided (g/kg)"
names(enduser_assembly_all)[8] <- "PM2.5 emissions avoided (g/kg)"

#######################################################################################
# Capital - General construction cost and system data from H2A, HDSAM, and literature #
#######################################################################################

# Production
capex_alkstack_small <- TEA_parameter[1,2]        # alkaline electrolysis stack (<1MTD): $/kW
capex_alkbop_small <- TEA_parameter[2,2]          # alkaline electrolysis BoP (<1MTD): $/kW
capex_alkstack_large <- TEA_parameter[3,2]        # alkaline electrolysis stack (>1MTD): $/kW
capex_alkbop_large <- TEA_parameter[4,2]          # alkaline electrolysis BoP (>1MTD): $/kW
capex_pemstack <- TEA_parameter[5,2]              # PEM electrolysis stack: $/kW
capex_pembop <- TEA_parameter[6,2]                # PEM electrolysis BoP: $/kW
capex_biogas <- TEA_parameter[7,2]                # biomass gasification: $/kg H2
capex_biothermo <- TEA_parameter[8,2]             # biomass thermochemical treatment: $/kg H2
capex_compressor_all <- TEA_parameter[9,2]        # compressor: $/kW
capex_liquefier_all <- TEA_parameter[10,2]        # liquefier: $/kW
capex_storage_gas <- TEA_parameter[11,2]          # gas storage: $/kg
capex_storage_liq <- TEA_parameter[12,2]          # liquid storage: $/kg
storage_time <- TEA_parameter[13,2]               # storage duration: day
capex_install_ratio <- TEA_parameter[14,2]        # installation and contingency cost: fraction of uninstalled CAPEX
plant_life <- TEA_parameter[15,2]                 # plant lifetime: year
discount_rate <- TEA_parameter[16,2]              # discount rate: fraction
life_alkstack <- TEA_parameter[17,2]              # alkaline electrolysis lifetime: hour
life_pemstack <- TEA_parameter[18,2]              # PEM electrolysis lifetime: hour

# End-use
refuel_utilization <- TEA_parameter[33,2]                # HD refuel annual utilization of capacity (of max capacity): fraction
refuel_H2lost_comp <- TEA_parameter[34,2]                # HD refuel H2 lost during compression: fraction
refuel_refrig_life <- TEA_parameter[35,2]                # HD refuel refrigeration unit lifetime: years
refuel_comp_pump_life <- TEA_parameter[36,2]             # HD refuel compressor or cryogenic pump lifetime: years
refuel_store_life <- TEA_parameter[37,2]                 # HD refuel storage lifetime: years
refuel_evap_life <- TEA_parameter[38,2]                  # HD refuel evaporator lifetime: years
refuel_elec_life <- TEA_parameter[39,2]                  # HD refuel electrical equipment lifetime: years
refuel_disp_life <- TEA_parameter[40,2]                  # HD refuel dispenser and cascade lifetime: years
refuel_remain_life <- TEA_parameter[41,2]                # HD refuel remainder station lifetime: years
fcapex_refuel_decomm <- TEA_parameter[42,2]              # HD refuel decommissioning cost (of equipment CAPEX): fraction
capex_refuel_remain <- TEA_parameter[43,2]               # HD refuel remainder station installed CAPEX: $
fcapex_refuel_site <- TEA_parameter[44,2]                # HD refuel site preparation (of installed total CAPEX): fraction
fcapex_refuel_eng <- TEA_parameter[45,2]                 # HD refuel engineering & design (of installed total CAPEX): fraction
fcapex_refuel_contingency <- TEA_parameter[46,2]         # HD refuel project contingency (of installed total CAPEX): fraction
fcapex_refuel_permit <- TEA_parameter[47,2]              # HD refuel permitting (of installed total CAPEX): fraction
capex_refuel_refrig_GH2 <- TEA_parameter[48,2]           # GH2 HD refuel refrigeration unit installed CAPEX: $
capex_refuel_store_GH2 <- TEA_parameter[49,2]            # GH2 HD refuel storage unit installed CAPEX: $
capex_refuel_comp_GH2_pipe350 <- TEA_parameter[50,2]     # GH2 HD refuel gas compressor installed CAPEX (pipeline - 350 bar): $
capex_refuel_comp_GH2_pipe700 <- TEA_parameter[51,2]     # GH2 HD refuel gas compressor installed CAPEX (pipeline - 700 bar): $
capex_refuel_comp_GH2_truck350 <- TEA_parameter[52,2]    # GH2 HD refuel gas compressor installed CAPEX (truck - 350 bar): $
capex_refuel_comp_GH2_truck700 <- TEA_parameter[53,2]    # GH2 HD refuel gas compressor installed CAPEX (truck - 700 bar): $
capex_refuel_elec_GH2_pipe350 <- TEA_parameter[54,2]     # GH2 HD refuel electrical installed CAPEX (pipeline - 350 bar): $
capex_refuel_elec_GH2_pipe700 <- TEA_parameter[55,2]     # GH2 HD refuel electrical installed CAPEX (pipeline - 700 bar): $
capex_refuel_elec_GH2_truck350 <- TEA_parameter[56,2]    # GH2 HD refuel electrical installed CAPEX (truck - 350 bar): $
capex_refuel_elec_GH2_truck700 <- TEA_parameter[57,2]    # GH2 HD refuel electrical installed CAPEX (truck - 700 bar): $
capex_refuel_disp_GH2_350 <- TEA_parameter[58,2]         # GH2 HD refuel dispenser and cascade installed CAPEX (350 bar): $
capex_refuel_disp_GH2_700 <- TEA_parameter[59,2]         # GH2 HD refuel dispenser and cascade installed CAPEX (700 bar): $
capex_refuel_refrig_LH2 <- TEA_parameter[60,2]           # LH2 HD refuel refrigeration unit installed CAPEX: $
capex_refuel_store_LH2 <- TEA_parameter[61,2]            # LH2 HD refuel storage unit installed CAPEX: $
capex_refuel_evap_LH2 <- TEA_parameter[62,2]             # LH2 HD refuel evaporator installed CAPEX: $
capex_refuel_comp_LH2_350 <- TEA_parameter[63,2]         # LH2 HD refuel gas compressor installed CAPEX (350 bar): $
capex_refuel_comp_LH2_700 <- TEA_parameter[64,2]         # LH2 HD refuel gas compressor installed CAPEX (700 bar): $
capex_refuel_pump_LH2_350 <- TEA_parameter[65,2]         # LH2 HD refuel cryogenic pump installed CAPEX (350 bar): $
capex_refuel_pump_LH2_700 <- TEA_parameter[66,2]         # LH2 HD refuel cryogenic pump installed CAPEX (700 bar): $
capex_refuel_elec_LH2_pump350 <- TEA_parameter[67,2]     # LH2 HD refuel electrical installed CAPEX (pump - 350 bar): $
capex_refuel_elec_LH2_pump700 <- TEA_parameter[68,2]     # LH2 HD refuel electrical installed CAPEX (pump - 700 bar): $
capex_refuel_elec_LH2_comp350 <- TEA_parameter[69,2]     # LH2 HD refuel electrical installed CAPEX (comp - 350 bar): $
capex_refuel_elec_LH2_comp700 <- TEA_parameter[70,2]     # LH2 HD refuel electrical installed CAPEX (comp - 700 bar): $
capex_refuel_disp_LH2_350 <- TEA_parameter[71,2]         # LH2 HD refuel dispenser and cascade installed CAPEX (350 bar): $
capex_refuel_disp_LH2_700 <- TEA_parameter[72,2]         # LH2 HD refuel dispenser and cascade installed CAPEX (700 bar): $

total_DC <- 0
for (i in 1:unlist(plant_life))
{
  total_DC <- total_DC + (1/(1 + discount_rate)^i)    # determining the total discounted cash flow
}
DCF <- total_DC^-1                                # determining the discounted cash flow factor

############################################################################################
# O&M - General electricity and other resources usage data from H2A, HDSAM, and literature #
############################################################################################

# Production
elec_alkstack_small <- LCA_parameter[1,2]         # alkaline electrolysis stack (<1MTD): kWh/kg H2
elec_alkbop_small <- LCA_parameter[2,2]           # alkaline electrolysis BoP (<1MTD): kWh/kg H2
elec_alkstack_large <- LCA_parameter[3,2]         # alkaline electrolysis stack (>1MTD): kWh/kg H2
elec_alkbop_large <- LCA_parameter[4,2]           # alkaline electrolysis BoP (>1MTD): kWh/kg H2
elec_pemstack <- LCA_parameter[5,2]               # PEM electrolysis stack: kWh/kg H2
elec_pembop <- LCA_parameter[6,2]                 # PEM electrolysis BoP: kWh/kg H2
elec_biogas <- LCA_parameter[7,2]                 # biomass gasification: kWh/kg H2
elec_biothermo <- LCA_parameter[8,2]              # biomass thermochemical treatment: kWh/kg H2
elec_H2liq_prod <- LCA_parameter[9,2]             # H2 liquefaction by producer: kWh/kg H2
elec_H2liq_reg <- LCA_parameter[10,2]             # H2 liquefaction by regional facility: kWh/kg H2
elec_H2comp_prod <- LCA_parameter[11,2]           # H2 compression by producer: kWh/kg H2
CF_alkpem_PV <- LCA_parameter[12,2]               # capacity factor for electrolyzers using PV electricity: fraction
CF_alkpem_grid <- LCA_parameter[13,2]             # capacity factor for electrolyzers using grid electricity: fraction
CF_bio_PPA <- LCA_parameter[14,2]                 # capacity factor for biomass processes using PPA electricity: fraction
water_alkpem <- LCA_parameter[15,2]               # electrolysis water consumption: gal/kg
water_gas <- LCA_parameter[16,2]                  # biomass gasification water consumption: gal/kg
water_thermo <- LCA_parameter[17,2]               # biomass thermochemical treatment water consumption: gal/kg
dens_combustNG <- LCA_parameter[51,2]             # combustion natural gas density: kg/cf
VEq_combustNG <- LCA_parameter[52,2]              # combustion natural gas equivalency: cf/gal
dens_diesel <- LCA_parameter[65,2]                # CA ultra low sulfur diesel density: kg/gal

# End-use
refuel_land_GH2_pipe <- LCA_parameter[74,2]                # GH2 HD refuel land required (pipeline): m2
refuel_land_GH2_truck <- LCA_parameter[75,2]               # GH2 HD refuel land required (truck): m2
refuel_elec_refrigGH2 <- LCA_parameter[76,2]               # GH2 HD refuel refrigeration unit electricity use: kWh/yr
refuel_elec_comp_pump_GH2_pipe350 <- LCA_parameter[77,2]   # GH2 HD refuel compressor or pump electricity use (pipeline - 350 bar): kWh/yr
refuel_elec_comp_pump_GH2_pipe700 <- LCA_parameter[78,2]   # GH2 HD refuel compressor or pump electricity use (pipeline - 700 bar): kWh/yr
refuel_elec_comp_pump_GH2_truck350 <- LCA_parameter[79,2]  # GH2 HD refuel compressor or pump electricity use (pipeline - 350 bar): kWh/yr
refuel_elec_comp_pump_GH2_truck700 <- LCA_parameter[80,2]  # GH2 HD refuel compressor or pump electricity use (pipeline - 700 bar): kWh/yr
refuel_land_LH2 <- LCA_parameter[81,2]                     # LH2 HD refuel land required: m2
refuel_elec_comp_pump_LH2_comp350 <- LCA_parameter[82,2]   # LH2 HD refuel compressor or pump electricity use (pipeline - 350 bar): kWh/yr
refuel_elec_comp_pump_LH2_comp700 <- LCA_parameter[83,2]   # LH2 HD refuel compressor or pump electricity use (pipeline - 700 bar): kWh/yr
refuel_elec_comp_pump_LH2_pump350 <- LCA_parameter[84,2]   # LH2 HD refuel compressor or pump electricity use (pipeline - 350 bar): kWh/yr
refuel_elec_comp_pump_LH2_pump700 <- LCA_parameter[85,2]   # LH2 HD refuel compressor or pump electricity use (pipeline - 700 bar): kWh/yr
refuel_boiloff_LH2_pump <- LCA_parameter[86,2]             # LH2 HD refuel boil-off amount (pump): kg/day
refuel_boiloff_LH2_comp <- LCA_parameter[87,2]             # LH2 HD refuel boil-off amount (comp): kg/day

#################################################################################
# O&M - General emission factors and unit costs from H2A, HDSAM, and literature #
#################################################################################

# Production
EF_landfill_CO2 <- LCA_parameter[38,2]            # emission avoidance from biomass landfill: kg/ton
CO2_water_alkpem <- LCA_parameter[39,2]           # CO2eq emissions from electrolyzer water delivery and treatment: kg/kg H2
CO2_maint_alk <- LCA_parameter[40,2]              # CO2eq emissions from alkaline electrolyzer maintenance (KOH maintenance and N2 purge): kg/kg H2
CO2_maint_pem <- LCA_parameter[41,2]              # CO2eq emissions from PEM electrolyzer maintenance (N2 purge): kg/kg H2
EF_truck_H2liq <- LCA_parameter[42,2]             # EF of CO2eq from transporting liquefied H2: kg/kg H2-mile
EF_truck_H2comp <- LCA_parameter[43,2]            # EF of CO2eq from transporting compressed H2: kg/kg H2-mile

ucost_decomm_all <- TEA_parameter[24,2]           # unit cost of decommissioning electrolyzers and biomass treatment systems: $/kg H2
fcost_restack_alkpem <- TEA_parameter[25,2]       # fractional cost of electrolyzer stack replacement: fraction of uninstalled CAPEX
fcost_insure_alkpem <- TEA_parameter[26,2]        # fractional cost of electrolyzer insurance: fraction of installed CAPEX
ucost_OM_insure_gasthermo <- TEA_parameter[27,2]  # unit cost of gasification and thermochemical treatment fixed O&M and insurance: $/kg H2

# End-use
ucost_land <- TEA_parameter[32,2]                 # unit cost of land: $/m2-month
refuel_labor <- TEA_parameter[73,2]               # HD refuel labor required: hrs/yr
ucost_refuel_labor <- TEA_parameter[74,2]         # HD refuel operating labor cost: $/man-hr
fcost_refuel_overhead <- TEA_parameter[75,2]      # HD refuel overhead and G&A (of total Labor Cost): fraction
fcost_refuel_refrig_OM <- TEA_parameter[76,2]     # HD refuel O&M and repairs for refrigeration (of refrigeration installed CAPEX): fraction
fcost_refuel_comp_pump_OM <- TEA_parameter[77,2]  # HD refuel O&M and repairs for compressor or pump (of compressor or pump installed CAPEX): fraction
fcost_refuel_other_OM <- TEA_parameter[78,2]      # HD refuel O&M and repairs for other equipment (of other equipment installed CAPEX): fraction
ucost_refuel_H2deliver <- TEA_parameter[79,2]     # HD refuel cost of liquid H2 delivered to station: $/kg H2
fcost_refuel_insurance <- TEA_parameter[80,2]     # HD refuel O&M insurance (of total CAPEX): fraction
fcost_refuel_property <- TEA_parameter[81,2]      # HD refuel O&M property tax (of total CAPEX): fraction
fcost_refuel_permit <- TEA_parameter[82,2]        # HD refuel O&M licensing and permitting (of total CAPEX): fraction

#############################################################################################
# O&M - California-specific emission factors and unit costs from H2A, HDSAM, and literature #
#############################################################################################

# Production
EF_renew_CO2 <- LCA_parameter[18,2]               # EF of CO2eq for PV, PPA, geothermal, and hydro electricity: kg/kWh
EF_renew_SO2 <- LCA_parameter[19,2]               # EF of SO2 for PV, PPA, geothermal, and hydro electricity: kg/kWh
EF_renew_NOx <- LCA_parameter[20,2]               # EF of NOx for PV, PPA, geothermal, and hydro electricity: kg/kWh
EF_renew_PM10 <- LCA_parameter[21,2]              # EF of PM10 for PV, PPA, geothermal, and hydro electricity: kg/kWh
EF_PHS_CO2 <- LCA_parameter[22,2]                 # EF of CO2eq for PHS electricity: kg/kWh
EF_PHS_SO2 <- LCA_parameter[23,2]                 # EF of SO2 for PHS electricity: kg/kWh
EF_PHS_NOx <- LCA_parameter[24,2]                 # EF of NOx for PHS electricity: kg/kWh
EF_PHS_PM10 <- LCA_parameter[25,2]                # EF of PM10 for PHS electricity: kg/kWh

ucost_PV <- TEA_parameter[19,2]                   # unit cost of PV electricity: $/kWh
ucost_PPA <- TEA_parameter[20,2]                  # unit cost of PPA electricity: $/kWh
ucost_PHS <- TEA_parameter[21,2]                  # unit cost of PHS electricity: $/kWh
ucost_geohyd <- TEA_parameter[22,2]               # unit cost of geothermal and hydro electricity: $/kWh
ucost_grid <- TEA_parameter[23,2]                 # unit cost of grid (PG&E, SDG&E, and SCE) electricity: $/kWh
ucost_water <- TEA_parameter[28,2]                # unit cost of water: $/kg
ucost_NG <- TEA_parameter[29,2]                   # unit cost of natural gas: $/MMBTU
ucost_MSW <- TEA_parameter[30,2]                  # unit cost of municipal solid waste: $/ton
ucost_Wood <- TEA_parameter[31,2]                 # unit cost of woody waste: $/ton

# End-use
EF_combustNG_CO2 <- LCA_parameter[44,2]           # EF of CO2eq for natural gas combustion: kg/MJ
EF_combustNG_SO2 <- LCA_parameter[45,2]           # EF of SO2 for natural gas combustion: kg/MJ
EF_combustNG_NOx <- LCA_parameter[46,2]           # EF of NOx for natural gas combustion: kg/MJ
EF_combustNG_PM25 <- LCA_parameter[47,2]          # EF of PM2.5 for natural gas combustion: kg/MJ
EF_combustNG_PM10 <- LCA_parameter[48,2]          # EF of PM10 for natural gas combustion: kg/MJ
Heat_combustNG <- LCA_parameter[49,2]             # Heat content for natural gas: MJ/MMBTU
Ener_combustNG <- LCA_parameter[50,2]             # Heat density for natural gas: cf/MMBTU

EF_ULSdiesel_CO2 <- LCA_parameter[53,2]           # EF of CO2eq for ultra low sulfur diesel combustion: kg/MJ
EF_ULSdiesel_SO2 <- LCA_parameter[54,2]           # EF of SO2 for ultra low sulfur diesel combustion: kg/MJ
EF_ULSdiesel_NOx_Port <- LCA_parameter[55,2]      # EF of NOx for ultra low sulfur diesel combustion (port and power): kg/MJ
EF_ULSdiesel_NOx_Trans <- LCA_parameter[56,2]     # EF of NOx for ultra low sulfur diesel combustion (transit): kg/MJ
EF_ULSdiesel_NOx_Truck <- LCA_parameter[57,2]     # EF of NOx for ultra low sulfur diesel combustion (truck): kg/MJ
EF_ULSdiesel_PM25_Port <- LCA_parameter[58,2]     # EF of PM2.5 for ultra low sulfur diesel combustion (port and power): kg/MJ
EF_ULSdiesel_PM25_Trans <- LCA_parameter[59,2]    # EF of PM2.5 for ultra low sulfur diesel combustion (transit): kg/MJ
EF_ULSdiesel_PM25_Truck <- LCA_parameter[60,2]    # EF of PM2.5 for ultra low sulfur diesel combustion (truck): kg/MJ
EF_ULSdiesel_PM10_Port <- LCA_parameter[61,2]     # EF of PM10 for ultra low sulfur diesel combustion (port and power): kg/MJ
EF_ULSdiesel_PM10_Trans <- LCA_parameter[62,2]    # EF of PM10 for ultra low sulfur diesel combustion (transit): kg/MJ
EF_ULSdiesel_PM10_Truck <- LCA_parameter[63,2]    # EF of PM10 for ultra low sulfur diesel combustion (truck): kg/MJ
Ener_ULSdiesel <- LCA_parameter[64,2]             # Energy density for ultra low sulfur diesel: MJ/gal

EF_jetfuel_CO2 <- LCA_parameter[66,2]             # EF of CO2eq for aviation jet fuel combustion: kg/MJ
EF_jetfuel_SO2 <- LCA_parameter[67,2]             # EF of SO2 for aviation jet fuel combustion: kg/MJ
EF_jetfuel_NOx <- LCA_parameter[68,2]             # EF of NOx for aviation jet fuel combustion: kg/MJ
EF_jetfuel_PM25 <- LCA_parameter[69,2]            # EF of PM2.5 for aviation jet fuel combustion: kg/MJ
EF_jetfuel_PM10 <- LCA_parameter[70,2]            # EF of PM10 for aviation jet fuel combustion: kg/MJ
Ener_jetfuel <- LCA_parameter[71,2]               # Energy density for aviation jet fuel: MJ/gal

H2energy <- LCA_parameter[72,2]                   # H2 energy content: MJ/kg
eff_dieseltruck <- LCA_parameter[73,2]            # Efficiency of diesel trucks: Fraction

#################################################################################
# Calculating LCA supply emissions and costs one producer at a time via looping #
#################################################################################
for (i in 1:nrow(Producer))
{
  #######################
  # General Information #
  #######################
  producer_process <- Producer[i,4]       # H2 production processes (alkaline and PEM electrolysis, and biomass gasification and thermochemical treatment)
  H2prod <- Supply[i,-1]                  # H2 production in the final year
  
  ################################################################
  # Capital - Company-specific construction cost and system data #
  ################################################################
  elec_size_prod <- Producer[i,5]                   # electrolyzer size: MW
  capex_uninstall_prod <- Producer[i,6]             # uninstalled total: $
  capex_stack_prod <- Producer[i,7]                 # electrolysis stack total: $
  capex_bop_prod <- Producer[i,8]                   # electrolysis BoP total: $
  H2liq <- Producer[i,9]                           # ratio of liquefied H2 product
  H2comp <- Producer[i,10]                          # ratio of compressed gas H2 product
  capex_liquefier_prod <- Producer[i,11]            # liquefier total: $
  capex_storage_prod <- Producer[i,12]              # liquefier total: $
  
  #####################################################################
  # O&M - Company-specific electricity and other resources usage data #
  #####################################################################
  PV_elec_ratio <- Producer[i,13]                   # ratio of PV electricity usage
  PPA_elec_ratio <- Producer[i,14]                  # ratio of PPA electricity usage
  geohyd_elec_ratio <- Producer[i,15]               # ratio of geothermal and hydro electricity usage
  PHS_elec_ratio <- Producer[i,16]                  # ratio of PHS electricity usage
  grid_elec <- producer_with_utility[i,2]           # grid electricity provider
  grid_elec_ratio <- Producer[i,17]                 # ratio of grid electricity usage
  elec_consump_misc <- Producer[i,18]               # misc. electricity consumption: kWh/kg
  water_consump <- Producer[i,19]                   # water consumption: gal/yr 
  NG_consump <- Producer[i,25]                      # natural gas consumption: kg/yr
  die_consump <- Producer[i,26]                     # diesel consumption: kg/yr
  bio_type <- Producer[i,27]                        # biomass type: MSW or Woody
  bio_consump <- Producer[i,28]                     # biomass consumption: ton/yr
  bio_avoid <- Producer[i,29]                       # biomass emissions avoidance: Landfill or empty
  H2distribution <- Producer[i,36]                  # H2 distribution medium: Truck or Pipeline
  H2distance <- Producer[i,37]                      # H2 distribution distance: miles
  
  #####################################################################################
  # O&M - California- and company-specific emission factors and costs from literature #
  #####################################################################################
  if (grepl("PG&E",grid_elec) == "TRUE"){
    EF_grid_CO2 <- LCA_parameter[26,2]              # EF of CO2eq for PG&E grid electricity: kg/kWh
    EF_grid_SO2 <- LCA_parameter[27,2]              # EF of SO2 for PG&E grid electricity: kg/kWh
    EF_grid_NOx <- LCA_parameter[28,2]              # EF of NOx for PG&E grid electricity: kg/kWh
    EF_grid_PM10 <- LCA_parameter[29,2]             # EF of PM10 for PG&E grid electricity: kg/kWh
  }else if (grepl("SDG&E",grid_elec) == "TRUE"){
    EF_grid_CO2 <- LCA_parameter[30,2]              # EF of CO2eq for SDG&E grid electricity: kg/kWh
    EF_grid_SO2 <- LCA_parameter[31,2]              # EF of SO2 for SDG&E grid electricity: kg/kWh
    EF_grid_NOx <- LCA_parameter[32,2]              # EF of NOx for SDG&E grid electricity: kg/kWh
    EF_grid_PM10 <- LCA_parameter[33,2]             # EF of PM10 for SDG&E grid electricity: kg/kWh
  }else if (grepl("SCE",grid_elec) == "TRUE"){
    EF_grid_CO2 <- LCA_parameter[34,2]              # EF of CO2eq for SCE grid electricity: kg/kWh
    EF_grid_SO2 <- LCA_parameter[35,2]              # EF of SO2 for SCE grid electricity: kg/kWh
    EF_grid_NOx <- LCA_parameter[36,2]              # EF of NOx for SCE grid electricity: kg/kWh
    EF_grid_PM10 <- LCA_parameter[37,2]             # EF of PM10 for SCE grid electricity: kg/kWh
  }
  
  #########################################################
  # O&M - Company-specific emission factors and cost data #
  #########################################################
  CO2_bio_gasthermo <- Producer[i,30]               # CO2eq emissions from biomass transport and preparation for gasification and thermochemical treatment: kg/kg H2
  CO2_maint_gasthermo <- Producer[i,31]             # CO2eq emissions from maintenance for gasification and thermochemical treatment: kg/kg H2
  CO2_soilstorage <- Producer[i,32]                 # CO2eq emissions avoided from soil carbon storage for gasification and thermochemical treatment: kg/kg H2
  SO2_gasthermo <- Producer[i,33]                   # SO2 emissions from gasification and thermochemical treatment: kg/yr
  NOx_gasthermo <- Producer[i,34]                   # NOx emissions from gasification and thermochemical treatment: kg/yr
  PM10_gasthermo <- Producer[i,35]                  # PM10 emissions from gasification and thermochemical treatment: kg/yr
  cost_water <- Producer[i,20]                      # cost of water: $/year
  cost_labor <- Producer[i,21]                      # cost of labor: $/year
  cost_maint_alkpem <- Producer[i,22]               # cost of electrolyzer maintenance: $/year
  cost_chemicals <- Producer[i,23]                  # cost of KOH and/or N2 for maintenance: $/year
  cost_maint_liquefier <- Producer[i,24]            # cost of liquefier maintenance: $/year
  ucost_H2trans <- Producer[i,38]                   # unit cost of H2 transport: $/kg H2
  
  ##############################
  # Capital - Cost calculation #
  ##############################

  # stack electricity consumption (needed to determine electrolyzer capacity and capacity factor)
  # (not needed for biomass gasification and thermochemical treatment)
  if (grepl("Alkaline electrolysis", producer_process) == "TRUE"){
    if (H2prod[,length(H2prod)] < 1000){
      elec_consump_stack <- elec_alkstack_small                                           # alkaline electrolyzer stack <1MTD: kWh/kg H2
    }else{
      elec_consump_stack <- elec_alkstack_large                                           # alkaline electrolyzer stack >1MTD: kWh/kg H2
    }
  }else if (grepl("PEM electrolysis", producer_process) == "TRUE"){
    elec_consump_stack <- elec_pemstack                                                   # PEM electrolyzer stack: kWh/kg H2
  }
      
  # electrolyzer size and capacity factor 
  # (if company gives electrolyzer size then use that to determine capacity factor; PV/grid mix or geothermal and hydro)
  if (grepl("N/A", elec_size_prod) == "FALSE"){
    elec_size <- as.double(elec_size_prod)                                                # company-given electrolyzer size: MW  
    CF <- H2prod[,length(H2prod)]/((elec_size * 1000 * 24)/elec_consump_stack)                             # determined capacity factor: fraction
      
  # (if not, determine electrolyzer size using generalized capacity factors; only PV or only grid or only PPA)
  }else if (grepl("N/A", elec_size_prod) == "TRUE"){
    if (grepl("Alkaline electrolysis", producer_process) == "TRUE" | grepl("PEM electrolysis", producer_process) == "TRUE"){
      if (PV_elec_ratio == 1){
        CF <- CF_alkpem_PV                                                                # generalized PV capacity factor: fraction
      }else if (grid_elec_ratio == 1 | PHS_elec_ratio == 1 | PPA_elec_ratio == 1 | geohyd_elec_ratio == 1){
        CF <- CF_alkpem_grid                                                              # generalized grid, PHS, and PPA capacity factor: fraction
      }else if (PV_elec_ratio < 1 & PV_elec_ratio > 0 & grid_elec_ratio < 1 & grid_elec_ratio > 0){
        CF <- PV_elec_ratio * CF_alkpem_PV + grid_elec_ratio * CF_alkpem_grid             # generalized mixed PV and grid capacity factor: fraction
      }else if (PV_elec_ratio < 1 & PV_elec_ratio > 0 & PHS_elec_ratio < 1 & PHS_elec_ratio > 0){
        CF <- PV_elec_ratio * CF_alkpem_PV + grid_elec_ratio * CF_alkpem_grid             # generalized mixed PV and PHS capacity factor: fraction
      }
      elec_size <- ((H2prod[,length(H2prod)] * 365 * elec_consump_stack)/(365 * 24 * CF))/1000           # determined electrolyzer size: fraction
    }else if (grepl("Biomass gasification", producer_process) == "TRUE" | grepl("Biomass thermochemical", producer_process) == "TRUE"){
      if (PPA_elec_ratio == 1){
        CF <- CF_bio_PPA                                                                  # generalized PPA capacity factor: fraction (no electrolyzer size)
      }
    }
  }
      
  # installed CAPEX (both electrolyzers and biomass treatments)
  # (if company gives total uninstalled CAPEX then normalize that with electrolyzer size)
  if (grepl("N/A", capex_uninstall_prod) == "FALSE"){
    capex_uninstall <- as.double(capex_uninstall_prod)/(elec_size * 1000)                 # company-given uninstalled (stack, BoP, liquefier, and storage tank for electrolyzers) CAPEX: $/kW
    capex_install <- (DCF * capex_uninstall * elec_size * 1000 * (1 + capex_install_ratio))/(H2prod[,length(H2prod)] * CF * 365 * plant_life)
  }else if (grepl("N/A", capex_uninstall_prod) == "TRUE"){
    # (if alkaline or PEM electrolyzer)
    if (grepl("Alkaline electrolysis", producer_process) == "TRUE" | grepl("PEM electrolysis", producer_process) == "TRUE"){
      # (if company gives stack and BoP CAPEX then normalize that with electrolyzer size)
      if (grepl("N/A", capex_stack_prod) == "FALSE" & grepl("N/A", capex_bop_prod) == "FALSE"){
        capex_stack <- as.double(capex_stack_prod)/(elec_size * 1000)                     # company-given electrolyzer stack CAPEX: $/kW
        capex_bop <- as.double(capex_bop_prod)/(elec_size * 1000)                         # company-given electrolyzer BoP CAPEX: $/kW
      # (if not, use generalized stack and BoP CAPEX already normalized by electrolyzer size)
      }else{
        if (grepl("Alkaline electrolysis", producer_process) == "TRUE"){
          if (H2prod[,length(H2prod)] < 1000){
            capex_stack <- capex_alkstack_small                                           # generalized alkaline electrolyzer (<1MTD) stack CAPEX: $/kW
            capex_bop <- capex_alkbop_small                                               # generalized alkaline electrolyzer (<1MTD) BoP CAPEX: $/kW
          }else{
            capex_stack <- capex_alkstack_large                                           # generalized alkaline electrolyzer (>1MTD) stack CAPEX: $/kW
            capex_bop <- capex_alkbop_large                                               # generalized alkaline electrolyzer (>1MTD) BoP CAPEX: $/kW
          }
        }else if (grepl("PEM electrolysis", producer_process) == "TRUE"){
          capex_stack <- capex_pemstack                                                   # generalized PEM electrolyzer stack CAPEX: $/kW
          capex_bop <- capex_pembop                                                       # generalized PEM electrolyzer BoP CAPEX: $/kW
        }
      }
          
      # (if final product is liquefied H2)
      if (H2liq > 0){
        # (if company gives liquefier CAPEX then normalize that with electrolyzer size)
        if (grepl("N/A", capex_liquefier_prod) == "FALSE"){
          capex_liquefier <- as.double(capex_liquefier_prod)/(elec_size * 1000)           # company-given H2 liquefier CAPEX: $/kW
        # (if not, use generalized liquefier CAPEX already normalized by electrolyzer size) 
        }else{
          capex_liquefier <- capex_liquefier_all                                          # generalized H2 liquefier CAPEX: $/kW
        }
      }else{
        capex_liquefier <- 0
      }
        
      # (if company gives storage tank CAPEX then normalized that with electrolyzer size)
      if (grepl("N/A", capex_storage_prod) == "FALSE"){
        capex_storage <- as.double(capex_storage_prod)/(elec_size * 1000)                 # company-given H2 storage tank CAPEX: $/kW
      # (if not, use generalized storage tank CAPEX already normalized by electrolyzer size)
      }else{
        capex_storage <- ((capex_storage_liq * H2liq * H2prod[,length(H2prod)] * storage_time)/(elec_size * 1000)) +
                          ((capex_storage_gas * H2comp * H2prod[,length(H2prod)] * storage_time)/(elec_size * 1000))    # generalized H2 storage tank CAPEX: $/kW
      }
      capex_uninstall <- capex_stack + capex_bop + capex_liquefier + capex_storage
      capex_install <- (DCF * capex_uninstall * elec_size * 1000 * (1 + capex_install_ratio))/(H2prod[,length(H2prod)] * CF * 365 * plant_life)
    }else if (grepl("Biomass gasification", producer_process) == "TRUE"){
      capex_install <- (DCF * capex_biogas)                                                # generalized installed biomass gasification CAPEX: $/kg H2
    }else if (grepl("Biomass thermochemical", producer_process) == "TRUE"){
      capex_install <- (DCF * capex_biothermo)                                                    # generalized installed biomass thermochemical CAPEX: $/kg H2
    }
    capex_install_all[i,1] <- capex_install                                               # total installed CAPEX of H2 production system: $/kg H2
  }
  
  ##########################
  # O&M - Cost calculation #
  ##########################
  
  # stack replacement timeline
  if (grepl("Alkaline electrolysis", producer_process) == "TRUE"){
    restack_time <- round(life_alkstack/(CF * 365 * 24),0)                                # generalized alkaline electrolyzer stack replacement interval: yrs
  }else if (grepl("PEM electrolysis", producer_process) == "TRUE"){
    restack_time <- round(life_pemstack/(CF * 365 * 24),0)                                # generalized PEM electrolyzer stack replacement interval: yrs
  }
  
  # CO2 emissions from maintenance, feedstock (water or biomass) delivery and preparation, and soil carbon storage of H2 production
  if (grepl("Alkaline electrolysis",producer_process) == "TRUE"){
    CO2_maint <- CO2_maint_alk                                                            # generalized alkaline electrolyzer KOH and H2 maintenance CO2 emission: kg/kg H2
    CO2_feedstock <- CO2_water_alkpem                                                     # generalized alkaline electrolyzer water delivery and preparation CO2 emission: kg/kg H2
    CO2_storage <- 0                                                                      # generalized alkaline electrolyzer soil C storage: kg/kg H2
 
  }else if (grepl("PEM electrolysis",producer_process) == "TRUE"){
    CO2_maint <- CO2_maint_pem                                                            # generalized PEM electrolyzer KOH and H2 maintenance CO2 emission: kg/kg H2
    CO2_feedstock <- CO2_water_alkpem                                                     # generalized PEM electrolyzer water delivery and treatment CO2 emission: kg/kg H2
    CO2_storage <- 0                                                                      # generalized PEM electrolyzer soil C storage: kg/kg H2
     
  }else if (grepl("Biomass gasification", producer_process) == "TRUE" | grepl("Biomass thermochemical", producer_process) == "TRUE"){
    CO2_maint <- as.double(CO2_maint_gasthermo)                                           # company-given biomass gasification and thermochemical CO2 emission: kg/kg H2
    CO2_feedstock <- as.double(CO2_bio_gasthermo)                                         # company-given biomass gasification and thermochemical biomass delivery and preparation CO2 emission: kg/kg H2
    if (grepl("Landfill", bio_avoid) == "TRUE"){
      CO2_storage <- -((bio_consump * EF_landfill_CO2)/(H2prod[,length(H2prod)] * 365))        # company-given CO2 emissions avoided from using biomass diverted from landfills: kg/kg H2
    } else{
      CO2_storage <- -as.double(CO2_soilstorage)                                             # company-given biomass gasification and thermochemical soil C storage: kg/kg H2
    }
  }
  CO2_maint_all[i,1] <- CO2_maint                                                         # net CO2 emissions from maintenance of H2 production process: kg/kg H2
  CO2_feedstock_all[i,1] <- CO2_feedstock                                                 # net CO2 emissions from feedstock delivery and preparation for H2 production: kg/kg H2
  CO2_storage_all[i,1] <- CO2_storage                                                     # net soil carbon storage during H2 production: kg/kg H2
  
  # cost of fixed O&M and insurance
  if (grepl("Alkaline electrolysis", producer_process) == "TRUE" | grepl("PEM electrolysis", producer_process) == "TRUE"){
    cost_restack <- ((DCF * floor(plant_life/restack_time) * fcost_restack_alkpem * capex_uninstall * elec_size * 1000)/(H2prod[,length(H2prod)] * CF * 365 * plant_life))
    cost_OM_insure <- cost_restack +                                                      # generalized electrolyzer stack replacement cost: $/kg H2
                      (fcost_insure_alkpem * capex_install) +                             # generalized electrolyzer insurance cost: $/kg H2
                      (as.double(cost_maint_alkpem)/(H2prod[,length(H2prod)] * 365))                 # company-given electrolyzer maintenance cost: $/kg H2
        
  }else if (grepl("Biomass gasification", producer_process) == "TRUE" | grepl("Biomass thermochemical", producer_process) == "TRUE"){
    cost_OM_insure <- ucost_OM_insure_gasthermo                                           # total cost for fixed O&M and insurance for biomass treatment: $/kg H2
  }
  cost_OM_insure_all[i,1] <- cost_OM_insure                                               # total cost for fixed O&M and insurance for H2 production: $/kg H2
  
  # costs of feedstock, labor, and liquefier maintenance (if needed)
  if (grepl("N/A", cost_water) == "TRUE" ){
    if (grepl("Alkaline electrolysis", producer_process) == "TRUE" | grepl("PEM electrolysis", producer_process) == "TRUE"){
      cost_water <- ucost_water * 3.79 * water_alkpem * (H2prod[,length(H2prod)] * 365)
    } else if(grepl("Biomass gasification", producer_process) == "TRUE"){
      cost_water <- ucost_water * 3.79 * water_gas * (H2prod[,length(H2prod)] * 365)
    } else if(grepl("Biomass thermochemical", producer_process) == "TRUE"){
      cost_water <- ucost_water * 3.79 * water_thermo * (H2prod[,length(H2prod)] * 365)
    }
  }
  
  # (if for alkaline or PEM electrolyzers)
  if (grepl("Alkaline electrolysis", producer_process) == "TRUE" | grepl("PEM electrolysis", producer_process) == "TRUE"){
    # (if final product is liquefied H2)
    if (H2liq > 0){
      cost_other_vars <- as.double(cost_chemicals)/(H2prod[,length(H2prod)] * 365) +                       # company-given electrolyzer KOH and/or N2 cost: $/kg H2
                         as.double(cost_water)/(H2prod[,length(H2prod)] * 365) +                           # company-given electrolyzer water cost: $/kg H2
                         as.double(cost_labor)/(H2prod[,length(H2prod)] * 365) +                           # company-given electrolyzer labor cost: $/kg H2
                         as.double(cost_maint_liquefier)/(H2prod[,length(H2prod)] * 365)                   # company-given electrolyzer liquefier maintenance cost: $/kg H2
    # (if final product is not liquefied H2)
    } else if (H2liq == 0){
      cost_other_vars <- as.double(cost_chemicals)/(H2prod[,length(H2prod)] * 365) +                       # company-given electrolyzer KOH and/or N2 cost: $/kg H2
                         as.double(cost_water)/(H2prod[,length(H2prod)] * 365) +                           # company-given electrolyzer water cost: $/kg H2
                         as.double(cost_labor)/(H2prod[,length(H2prod)] * 365)                             # company-given electrolyzer labor cost: $/kg H2
    }
      
  # (if for biomass gasification or thermochemical treatment)
  } else if(grepl("Biomass gasification", producer_process) == "TRUE" | grepl("Biomass thermochemical", producer_process) == "TRUE"){
    # (if final product is liquefied H2)
    if (H2liq > 0){
      # (if biomass feedstock is municipal solid waste)
      if (grepl("MSW", bio_type) == "TRUE"){
        cost_other_vars <- (bio_consump * ucost_MSW)/(H2prod[,length(H2prod)] * 365) +                      # generalized feedstock (municipal solid waste) cost: $/kg H2
                           as.double(cost_water)/(H2prod[,length(H2prod)] * 365) +                          # company-given electrolyzer water cost: $/kg H2
                           as.double(cost_labor)/(H2prod[,length(H2prod)] * 365) +                          # company-given electrolyzer labor cost: $/kg H2
                           as.double(cost_maint_liquefier)/(H2prod[,length(H2prod)] * 365)                  # company-given electrolyzer liquefier maintenance cost: $/kg H2
          
      # (if biomass feedstock is woody biomass)
      } else if (grepl("Woody", bio_type) == "TRUE"){
        cost_other_vars <- (bio_consump * ucost_Wood)/(H2prod[,length(H2prod)] * 365) +                     # generalized feedstock (municipal solid waste) cost: $/kg H2
                           as.double(cost_water)/(H2prod[,length(H2prod)] * 365) +                          # company-given electrolyzer water cost: $/kg H2
                           as.double(cost_labor)/(H2prod[,length(H2prod)] * 365) +                          # company-given electrolyzer labor cost: $/kg H2
                           as.double(cost_maint_liquefier)/(H2prod[,length(H2prod)] * 365)                  # company-given electrolyzer liquefier maintenance cost: $/kg H2
      }
    # (if final product is not liquefied H2)
    } else if (H2liq == 0){
      # (if biomass feedstock is municipal solid waste)
      if (grepl("MSW", bio_type) == "TRUE"){
        cost_other_vars <- (bio_consump * ucost_MSW)/(H2prod[,length(H2prod)] * 365) +                      # generalized feedstock (municipal solid waste) cost: $/kg H2
                           as.double(cost_water)/(H2prod[,length(H2prod)] * 365) +                          # company-given electrolyzer water cost: $/kg H2
                           as.double(cost_labor)/(H2prod[,length(H2prod)] * 365)                            # company-given electrolyzer labor cost: $/kg H2
          
      # (if biomass feedstock is woody biomass)
      } else if (grepl("Woody", bio_type) == "TRUE"){
        cost_other_vars <- (bio_consump * ucost_Wood)/(H2prod[,length(H2prod)] * 365) +                     # generalized feedstock (municipal solid waste) cost: $/kg H2
                           as.double(cost_water)/(H2prod[,length(H2prod)] * 365) +                          # company-given electrolyzer water cost: $/kg H2
                           as.double(cost_labor)/(H2prod[,length(H2prod)] * 365)                            # company-given electrolyzer labor cost: $/kg H2
      }
    }
  }
  cost_other_vars_all[i,1] <- cost_other_vars                                              # total cost for feedstock, labor, and liquefier maintenance (if needed) for H2 production: $/kg H2
    
  # EF and unit costs for electricity
  EF_CO2 <- 0
  EF_SO2 <- 0
  EF_NOx <- 0
  EF_PM10 <- 0
  ucost_elec <- 0
  if (PV_elec_ratio > 0){
    EF_CO2 <- EF_CO2 + PV_elec_ratio * EF_renew_CO2                         # CO2eq EF for PV electricity:kg/kWh
    EF_SO2 <- EF_SO2 + PV_elec_ratio * EF_renew_SO2                         # SO2 EF for PV electricity:kg/kWh
    EF_NOx <- EF_NOx + PV_elec_ratio * EF_renew_NOx                         # NOx EF for PV electricity:kg/kWh
    EF_PM10 <- EF_PM10 + PV_elec_ratio * EF_renew_PM10                      # PM10 EF for PV electricity:kg/kWh
    ucost_elec <- ucost_elec + PV_elec_ratio * ucost_PV                     # unit cost for PV electricity:$/kWh
  }
  if (PPA_elec_ratio > 0){
    EF_CO2 <- EF_CO2 + PPA_elec_ratio * EF_renew_CO2                        # CO2eq EF for PPA electricity:kg/kWh
    EF_SO2 <- EF_SO2 + PPA_elec_ratio * EF_renew_SO2                        # SO2 EF for PPA electricity:kg/kWh
    EF_NOx <- EF_NOx + PPA_elec_ratio * EF_renew_NOx                        # NOx EF for PPA electricity:kg/kWh
    EF_PM10 <- EF_PM10 + PPA_elec_ratio * EF_renew_PM10                     # PM10 EF for PPA electricity:kg/kWh
    ucost_elec <- ucost_elec + PPA_elec_ratio * ucost_PPA                   # unit cost for PPA electricity:$/kWh
  }
  if (geohyd_elec_ratio > 0){
    EF_CO2 <- EF_CO2 + geohyd_elec_ratio * EF_renew_CO2                     # CO2eq EF for geothermal and hydro electricity:kg/kWh
    EF_SO2 <- EF_SO2 + geohyd_elec_ratio * EF_renew_SO2                     # SO2 EF for geothermal and hydro electricity:kg/kWh
    EF_NOx <- EF_NOx + geohyd_elec_ratio * EF_renew_NOx                     # NOx EF for geothermal and hydro electricity:kg/kWh
    EF_PM10 <- EF_PM10 + geohyd_elec_ratio * EF_renew_PM10                  # PM10 EF for geothermal and hydro electricity:kg/kWh
    ucost_elec <- ucost_elec + geohyd_elec_ratio * ucost_geohyd             # unit cost for geothermal and hydro electricity:$/kWh
  }
  if (PHS_elec_ratio > 0){
    EF_CO2 <- EF_CO2 + PHS_elec_ratio * EF_PHS_CO2                          # CO2eq EF for PHS electricity:kg/kWh
    EF_SO2 <- EF_SO2 + PHS_elec_ratio * EF_PHS_SO2                          # SO2 EF for PHS electricity:kg/kWh
    EF_NOx <- EF_NOx + PHS_elec_ratio * EF_PHS_NOx                          # NOx EF for PHS electricity:kg/kWh
    EF_PM10 <- EF_PM10 + PHS_elec_ratio * EF_PHS_PM10                       # PM10 EF for PHS electricity:kg/kWh
    ucost_elec <- ucost_elec + PHS_elec_ratio * ucost_PHS                   # unit cost for PHS electricity:$/kWh
  }
  if (grid_elec_ratio > 0){
    EF_CO2 <- EF_CO2 + grid_elec_ratio * EF_grid_CO2                        # CO2eq EF for grid electricity:kg/kWh
    EF_SO2 <- EF_SO2 + grid_elec_ratio * EF_grid_SO2                        # SO2 EF for grid electricity:kg/kWh
    EF_NOx <- EF_NOx + grid_elec_ratio * EF_grid_NOx                        # NOx EF for grid electricity:kg/kWh
    EF_PM10 <- EF_PM10 + grid_elec_ratio * EF_grid_PM10                     # PM10 EF for grid electricity:kg/kWh
    ucost_elec <- ucost_elec + grid_elec_ratio * ucost_grid                 # unit cost for PHS electricity:$/kWh
  }
    
  # electricity consumption for H2 production
  if (grepl("Alkaline electrolysis", producer_process) == "TRUE"){
    if (H2prod[,length(H2prod)] < 1000){
      elec_consump_H2prod <- elec_alkstack_small + elec_alkbop_small            # generalized electricity consumption by <1 MTD alkaline electrolyzer: kWh/kg H2
    }else{
      elec_consump_H2prod <- elec_alkstack_large + elec_alkbop_large            # generalized electricity consumption by >1 MTD alkaline electrolyzer: kWh/kg H2
    }
  }else if (grepl("PEM electrolysis", producer_process) == "TRUE"){
    elec_consump_H2prod <- elec_pemstack + elec_pembop                          # generalized electricity consumption by PEM electrolyzer: kWh/kg H2
    
  }else if (grepl("Biomass gasification", producer_process) == "TRUE"){
    elec_consump_H2prod <- elec_biogas                                          # generalized electricity consumption by biomass gasification: kWh/kg H2
    
  }else if (grepl("Biomass thermochemical", producer_process) == "TRUE"){
    elec_consump_H2prod <- elec_biothermo                                       # generalized electricity consumption by biomass thermochemical conversion: kWh/kg H2
  
  }
    
  # CO2 emissions from electricity usage for H2 production
  CO2_elec <- elec_consump_H2prod * EF_CO2                                      # generalized CO2 emissions from electricity usage for H2 production: kg/kg H2
  
  CO2_elec_all[i,1] <- CO2_elec                                                 # net CO2 emissions for H2 production: kg/kg H2
  
  # SO2, NOx, and PM10 emissions for H2 production
  if (grepl("Alkaline electrolysis",producer_process) == "TRUE" | grepl("PEM electrolysis",producer_process) == "TRUE"){
    SO2_elec <- elec_consump_H2prod * EF_SO2                                    # generalized SO2 emissions from electrolyzer electricity usage for H2 production: kg/kg H2
    NOx_elec <- elec_consump_H2prod * EF_NOx                                    # generalized NOx emissions from electrolyzer electricity usage for H2 production: kg/kg H2
    PM10_elec <- elec_consump_H2prod * EF_PM10                                  # generalized PM10 emissions from electrolyzer electricity usage for H2 production: kg/kg H2

  }else if (grepl("Biomass gasification", producer_process) == "TRUE" | grepl("Biomass thermochemical", producer_process) == "TRUE"){
    SO2_elec <- as.double(SO2_gasthermo)/(H2prod[,length(H2prod)] * 365)                   # company-given SO2 emissions from electrolyzer electricity usage for H2 production: kg/kg H2
    NOx_elec <- as.double(NOx_gasthermo)/(H2prod[,length(H2prod)] * 365)                   # company-given NOx emissions from electrolyzer electricity usage for H2 production: kg/kg H2
    PM10_elec <- as.double(PM10_gasthermo)/(H2prod[,length(H2prod)] * 365)                 # company-given PM10 emissions from electrolyzer electricity usage for H2 production: kg/kg H2

  }
  
  SO2_elec_all[i,1] <- SO2_elec * 10^3                                          # net SO2 emissions for H2 production: g/kg H2
  NOx_elec_all[i,1] <- NOx_elec * 10^3                                          # net NOx emissions for H2 production: g/kg H2
  PM10_elec_all[i,1] <- PM10_elec * 10^3                                        # net PM10 emissions for H2 production: g/kg H2
  
  # CO2 emissions from H2 liquefaction and/or compression
  CO2_H2liqcomp <- ((H2comp * elec_H2comp_prod) + (H2liq * elec_H2liq_prod)) * EF_CO2               # generalized CO2 emissions from electricity usage H2 liquefaction and/or compression: kg/kg H2
  CO2_H2liqcomp_all[i,1] <- CO2_H2liqcomp                                                           # net CO2 emissions for liquefaction and/or compression of H2: kg/kg H2 

  # cost of electricity usage
  cost_elec <- (elec_consump_H2prod + (H2comp * elec_H2comp_prod) + (H2liq * elec_H2liq_prod) + elec_consump_misc) * ucost_elec       # generalized cost from electricity of H2 production, liquefaction and/or compression, and misc usage: $/kg H2
  cost_elec_all[i,1] <- cost_elec                                                                                                     # total cost for electricity of H2 production, liquefaction and/or compression, and misc usage: $/kg H2
  
  # CO2 emissions from H2 transportation
  if (grepl("Truck", H2distribution) == "TRUE"){
    CO2_H2trans <- (H2distance * EF_truck_H2comp * H2comp) * (H2distance * EF_truck_H2liq * H2liq)  # generalized CO2 emissions from H2 transport using trucks: kg/kg H2
  }else if (grepl("Pipeline",H2distribution) == "TRUE"){
    CO2_H2trans <- 0                                                                                # no CO2 emissions from H2 transport using pipelines
  }
  CO2_H2trans_all <- CO2_H2trans                                                # net CO2 emissions from H2 transportation: kg/kg H2
  
  # cost of H2 transportation
  cost_H2trans <- ucost_H2trans                                                 # derived cost of H2 transportation: $/kg H2
  cost_H2trans_all <- cost_H2trans                                              # total cost of H2 transportation: $/kg H2
  
  # decommissioning cost
  cost_decomm <- ucost_decomm_all
  cost_decomm_all[i,1] <- cost_decomm                                                # total decommissioning cost of H2 production system: $/kg  
  
  # net CO2 emissions and total production cost
  CO2_net_all[i,1] <- CO2_maint + CO2_feedstock + CO2_storage + CO2_elec + CO2_H2liqcomp + CO2_H2trans
  cost_total_all[i,1] <- capex_install + cost_OM_insure + cost_other_vars + cost_elec + cost_H2trans + cost_decomm

  if (grepl("NorCal",producer_with_region[i,2]) == "TRUE"){
    CO2_maint_NorCal[i,1] <- CO2_maint
    CO2_feedstock_NorCal[i,1] <- CO2_feedstock
    CO2_storage_NorCal[i,1] <- CO2_storage
    CO2_elec_NorCal[i,1] <- CO2_elec
    CO2_H2liqcomp_NorCal[i,1] <- CO2_H2liqcomp
    CO2_H2trans_NorCal[i,1] <- CO2_H2trans
    
    capex_NorCal[i,1] <- capex_install
    cost_elec_NorCal[i,1] <- cost_elec
    cost_OM_NorCal[i,1] <- cost_OM_insure
    cost_other_NorCal[i,1] <- cost_other_vars
    cost_H2trans_NorCal[i,1] <- cost_H2trans
    cost_decomm_NorCal[i,1] <- cost_decomm
    
    CO2_net_NorCal[i,1] <- CO2_net_all[i,1]
    SO2_net_NorCal[i,1] <- SO2_elec_all[i,1]
    NOx_net_NorCal[i,1] <- NOx_elec_all[i,1]
    PM10_net_NorCal[i,1] <- PM10_elec_all[i,1]
    
    cost_total_NorCal[i,1] <- cost_total_all[i,1]
    H2prod_NorCal[i,] <- H2prod
    
  } else if (grepl("NCV",producer_with_region[i,2]) == "TRUE"){
    CO2_maint_NCV[i,1] <- CO2_maint
    CO2_feedstock_NCV[i,1] <- CO2_feedstock
    CO2_storage_NCV[i,1] <- CO2_storage
    CO2_elec_NCV[i,1] <- CO2_elec
    CO2_H2liqcomp_NCV[i,1] <- CO2_H2liqcomp
    CO2_H2trans_NCV[i,1] <- CO2_H2trans
    
    capex_NCV[i,1] <- capex_install
    cost_elec_NCV[i,1] <- cost_elec
    cost_OM_NCV[i,1] <- cost_OM_insure
    cost_other_NCV[i,1] <- cost_other_vars
    cost_H2trans_NCV[i,1] <- cost_H2trans
    cost_decomm_NCV[i,1] <- cost_decomm
    
    CO2_net_NCV[i,1] <- CO2_net_all[i,1]
    SO2_net_NCV[i,1] <- SO2_elec_all[i,1]
    NOx_net_NCV[i,1] <- NOx_elec_all[i,1]
    PM10_net_NCV[i,1] <- PM10_elec_all[i,1]
    
    cost_total_NCV[i,1] <- cost_total_all[i,1]
    H2prod_NCV[i,] <- H2prod
    
  } else if (grepl("SCV",producer_with_region[i,2]) == "TRUE"){
    CO2_maint_SCV[i,1] <- CO2_maint
    CO2_feedstock_SCV[i,1] <- CO2_feedstock
    CO2_storage_SCV[i,1] <- CO2_storage
    CO2_elec_SCV[i,1] <- CO2_elec
    CO2_H2liqcomp_SCV[i,1] <- CO2_H2liqcomp
    CO2_H2trans_SCV[i,1] <- CO2_H2trans
    
    capex_SCV[i,1] <- capex_install
    cost_elec_SCV[i,1] <- cost_elec
    cost_OM_SCV[i,1] <- cost_OM_insure
    cost_other_SCV[i,1] <- cost_other_vars
    cost_H2trans_SCV[i,1] <- cost_H2trans
    cost_decomm_SCV[i,1] <- cost_decomm
    
    CO2_net_SCV[i,1] <- CO2_net_all[i,1]
    SO2_net_SCV[i,1] <- SO2_elec_all[i,1]
    NOx_net_SCV[i,1] <- NOx_elec_all[i,1]
    PM10_net_SCV[i,1] <- PM10_elec_all[i,1]
    
    cost_total_SCV[i,1] <- cost_total_all[i,1]
    H2prod_SCV[i,] <- H2prod
    
  } else if (grepl("SoCal",producer_with_region[i,2]) == "TRUE"){
    CO2_maint_SoCal[i,1] <- CO2_maint
    CO2_feedstock_SoCal[i,1] <- CO2_feedstock
    CO2_storage_SoCal[i,1] <- CO2_storage
    CO2_elec_SoCal[i,1] <- CO2_elec
    CO2_H2liqcomp_SoCal[i,1] <- CO2_H2liqcomp
    CO2_H2trans_SoCal[i,1] <- CO2_H2trans
    
    capex_SoCal[i,1] <- capex_install
    cost_elec_SoCal[i,1] <- cost_elec
    cost_OM_SoCal[i,1] <- cost_OM_insure
    cost_other_SoCal[i,1] <- cost_other_vars
    cost_H2trans_SoCal[i,1] <- cost_H2trans
    cost_decomm_SoCal[i,1] <- cost_decomm
    
    CO2_net_SoCal[i,1] <- CO2_net_all[i,1]
    SO2_net_SoCal[i,1] <- SO2_elec_all[i,1]
    NOx_net_SoCal[i,1] <- NOx_elec_all[i,1]
    PM10_net_SoCal[i,1] <- PM10_elec_all[i,1]
    
    cost_total_SoCal[i,1] <- cost_total_all[i,1]
    H2prod_SoCal[i,] <- H2prod
    
  }

}

producer_results_all <- cbind(Producer[,1],H2prod_all[,ncol(H2prod_all)],CO2_net_all,SO2_elec_all,NOx_elec_all,PM10_elec_all,cost_total_all)        # combining emissions and costs of all producers

# producers results summary for senate district
for(k in 1:nrow(producer_senate_all)) 
{
  producer_senate_select <- get(paste0("producer_S",as.double(producer_senate_all[k,1]),"_cord"))           # extracting senate district information of producers
  producer_senate_select <- producer_senate_select %>% sf::st_drop_geometry(producer_senate_select)         # removing geographic information
  producer_out <- data.frame(matrix(NA, 6, nrow = nrow(producer_senate_select)))                            # creating blank space for storing emissions and costs
  
  for(j in 1:nrow(producer_results_all)) {
    for(n in 1:nrow(producer_senate_select)) {
      if (grepl(producer_senate_select[n,1],producer_results_all[j,1]) == "TRUE"){
        producer_out[n,1:6] <- round(producer_results_all[j,2:7],2)                                         # storing emissions and costs
      }
    }
  }
  
  # Conducting weighted mean for senate districts with multiple producers
  if (producer_senate_all[k,2] > 1) {
    producer_out_senate <- data.frame(matrix(NA, 6, nrow = 1))
    producer_out_senate[,1] <- mean(producer_out[,1])
    producer_out_senate[,2] <- weighted.mean(producer_out[,2],producer_out[,1])
    producer_out_senate[,3] <- weighted.mean(producer_out[,3],producer_out[,1])
    producer_out_senate[,4] <- weighted.mean(producer_out[,4],producer_out[,1])
    producer_out_senate[,5] <- weighted.mean(producer_out[,5],producer_out[,1])
    producer_out_senate[,6] <- weighted.mean(producer_out[,6],producer_out[,1])
    producer_out <- producer_out_senate
  }
  
  producer_senate_all[k,3:8] <- producer_out
}  

# producers results summary for congressional district
# (please see comments on the "producers results summary for senate district" section for explanation on the algorithm)
for(k in 1:nrow(producer_congress_all)) 
  {
  producer_congress_select <- get(paste0("producer_C",as.double(producer_congress_all[k,1]),"_cord"))
  producer_congress_select <- producer_congress_select %>% sf::st_drop_geometry(producer_congress_select)
  producer_out <- data.frame(matrix(NA, 6, nrow = nrow(producer_congress_select)))
  
  for(j in 1:nrow(producer_results_all)) {
    for(n in 1:nrow(producer_congress_select)) {
      if (grepl(producer_congress_select[n,1],producer_results_all[j,1]) == "TRUE"){
        producer_out[n,1:6] <- round(producer_results_all[j,2:7],2)
      }
    }
  }
  
  if (producer_congress_all[k,2] > 1) {
    producer_out_congress <- data.frame(matrix(NA, 3, nrow = 1))
    producer_out_congress[,1] <- mean(producer_out[,1])
    producer_out_congress[,2] <- weighted.mean(producer_out[,2],producer_out[,1])
    producer_out_congress[,3] <- weighted.mean(producer_out[,3],producer_out[,1])
    producer_out_congress[,4] <- weighted.mean(producer_out[,4],producer_out[,1])
    producer_out_congress[,5] <- weighted.mean(producer_out[,5],producer_out[,1])
    producer_out_congress[,6] <- weighted.mean(producer_out[,6],producer_out[,1])
    producer_out <- producer_out_congress
  }

  producer_congress_all[k,3:8] <- producer_out
}  

# producers results summary for assembly district
# (please see comments on the "producers results summary for senate district" section for explanation on the algorithm)
for(k in 1:nrow(producer_assembly_all)) 
{
  producer_assembly_select <- get(paste0("producer_A",as.double(producer_assembly_all[k,1]),"_cord"))
  producer_assembly_select <- producer_assembly_select %>% sf::st_drop_geometry(producer_assembly_select)
  producer_out <- data.frame(matrix(NA, 6, nrow = nrow(producer_assembly_select)))
  
  for(j in 1:nrow(producer_results_all)) {
    for(n in 1:nrow(producer_assembly_select)) {
      if (grepl(producer_assembly_select[n,1],producer_results_all[j,1]) == "TRUE"){
        producer_out[n,1:6] <- round(producer_results_all[j,2:7],2)
      }
    }
  }
  
  if (producer_assembly_all[k,2] > 1) {
    producer_out_assembly <- data.frame(matrix(NA, 3, nrow = 1))
    producer_out_assembly[,1] <- mean(producer_out[,1])
    producer_out_assembly[,2] <- weighted.mean(producer_out[,2],producer_out[,1])
    producer_out_assembly[,3] <- weighted.mean(producer_out[,3],producer_out[,1])
    producer_out_assembly[,4] <- weighted.mean(producer_out[,4],producer_out[,1])
    producer_out_assembly[,5] <- weighted.mean(producer_out[,5],producer_out[,1])
    producer_out_assembly[,6] <- weighted.mean(producer_out[,6],producer_out[,1])
    producer_out <- producer_out_assembly
  }
  
  producer_assembly_all[k,3:8] <- producer_out
}  

######################
# emissions plotting #
######################

# Regional net CO2 emissions
CO2_region_NorCal <- weighted.mean(CO2_net_NorCal, data.frame(H2prod_NorCal[,ncol(H2prod_NorCal)]))
CO2_region_NCV <- weighted.mean(CO2_net_NCV, data.frame(H2prod_NCV[,ncol(H2prod_NCV)]))
CO2_region_SCV <- weighted.mean(CO2_net_SCV, data.frame(H2prod_SCV[,ncol(H2prod_SCV)]))
CO2_region_SoCal <- weighted.mean(CO2_net_SoCal, data.frame(H2prod_SoCal[,ncol(H2prod_SoCal)]))

region_CO2 <- data.frame(c("NorCal","NCV","SCV","SoCal"),
                          c(unlist(CO2_region_NorCal), unlist(CO2_region_NCV), unlist(CO2_region_SCV), unlist(CO2_region_SoCal)))
names(region_CO2) <- c("H2_Region","Net_CO2_emissions")

region_CO2cord <- region %>% 
  left_join(region_CO2, by = c("H2_Region" = "H2_Region")) 

CO2_supply_map <- ggplot() + 
              geom_sf(data = ca_bound) +
              geom_sf(data = region_CO2cord, aes(fill = Net_CO2_emissions)) + 
              geom_sf_text(data = region_CO2cord, aes(label = H2_Region)) +
              theme_bw() +
              theme(text=element_text(size = 11),
                legend.position = c(0.7, 0.75),
                axis.title = element_blank()) +
              scale_fill_gradient2(bquote("Net carbon\ndioxide\nequivalent\nemissions (kg/kg"~H[2]*")"),
                       low = "orchid4", mid = "white", high = "orange", midpoint = 0) +
  ggspatial::annotation_scale(data = region_CO2cord, location = "bl") +
  ggspatial::annotation_north_arrow(which_north = "true",location = "tr") 

ggsave(paste0(getwd(), "/Results/Producer/Emissions_CO2/Regional_CO2_emissions.jpg"), plot = CO2_supply_map)

# Regional net SO2 emissions
SO2_region_NorCal <- weighted.mean(SO2_net_NorCal, data.frame(H2prod_NorCal[,ncol(H2prod_NorCal)]))
SO2_region_NCV <- weighted.mean(SO2_net_NCV, data.frame(H2prod_NCV[,ncol(H2prod_NCV)]))
SO2_region_SCV <- weighted.mean(SO2_net_SCV, data.frame(H2prod_SCV[,ncol(H2prod_SCV)]))
SO2_region_SoCal <- weighted.mean(SO2_net_SoCal, data.frame(H2prod_SoCal[,ncol(H2prod_SoCal)]))

region_SO2 <- data.frame(c("NorCal","NCV","SCV","SoCal"),
                         c(unlist(SO2_region_NorCal), unlist(SO2_region_NCV), unlist(SO2_region_SCV), unlist(SO2_region_SoCal)))
names(region_SO2) <- c("H2_Region","Net_SO2_emissions")

region_SO2cord <- region %>% 
  left_join(region_SO2, by = c("H2_Region" = "H2_Region")) 

SO2_supply_map <- ggplot() + 
              geom_sf(data = ca_bound) +
              geom_sf(data = region_SO2cord, aes(fill = Net_SO2_emissions)) + 
              geom_sf_text(data = region_SO2cord, aes(label = H2_Region)) +
              theme_bw() +
              theme(text=element_text(size = 12),
                legend.position = c(0.7, 0.75),
                axis.title = element_blank()) +
              scale_fill_gradient2(bquote("Sulfur\ndioxide\nemissions (g/kg"~H[2]*")"),
                       low = "orchid4", mid = "white", high = "orange", midpoint = 0) +
  ggspatial::annotation_scale(data = region_SO2cord, location = "bl") +
  ggspatial::annotation_north_arrow(which_north = "true",location = "tr") 

ggsave(paste0(getwd(), "/Results/Producer/Emissions_other/Regional_SO2_emissions.jpg"), plot = SO2_supply_map)

# Regional net NOx emissions
NOx_region_NorCal <- weighted.mean(NOx_net_NorCal, data.frame(H2prod_NorCal[,ncol(H2prod_NorCal)]))
NOx_region_NCV <- weighted.mean(NOx_net_NCV, data.frame(H2prod_NCV[,ncol(H2prod_NCV)]))
NOx_region_SCV <- weighted.mean(NOx_net_SCV, data.frame(H2prod_SCV[,ncol(H2prod_SCV)]))
NOx_region_SoCal <- weighted.mean(NOx_net_SoCal, data.frame(H2prod_SoCal[,ncol(H2prod_SoCal)]))

region_NOx <- data.frame(c("NorCal","NCV","SCV","SoCal"),
                         c(unlist(NOx_region_NorCal), unlist(NOx_region_NCV), unlist(NOx_region_SCV), unlist(NOx_region_SoCal)))
names(region_NOx) <- c("H2_Region","Net_NOx_emissions")

region_NOxcord <- region %>% 
  left_join(region_NOx, by = c("H2_Region" = "H2_Region")) 

NOx_supply_map <- ggplot() + 
              geom_sf(data = ca_bound) +
              geom_sf(data = region_NOxcord, aes(fill = Net_NOx_emissions)) + 
              geom_sf_text(data = region_NOxcord, aes(label = H2_Region)) +
              theme_bw() +
              theme(text=element_text(size = 12),
                legend.position = c(0.7, 0.75),
                axis.title = element_blank()) +
              scale_fill_gradient2(bquote("Nitrogen\noxides\nemissions (g/kg"~H[2]*")"),
                       low = "orchid4", mid = "white", high = "orange", midpoint = 0) + 
  ggspatial::annotation_scale(data = region_NOxcord, location = "bl") +
  ggspatial::annotation_north_arrow(which_north = "true",location = "tr")

ggsave(paste0(getwd(), "/Results/Producer/Emissions_other/Regional_NOx_emissions.jpg"), plot = NOx_supply_map)

# Regional net PM10 emissions
PM10_region_NorCal <- weighted.mean(PM10_net_NorCal, data.frame(H2prod_NorCal[,ncol(H2prod_NorCal)]))
PM10_region_NCV <- weighted.mean(PM10_net_NCV, data.frame(H2prod_NCV[,ncol(H2prod_NCV)]))
PM10_region_SCV <- weighted.mean(PM10_net_SCV, data.frame(H2prod_SCV[,ncol(H2prod_SCV)]))
PM10_region_SoCal <- weighted.mean(PM10_net_SoCal, data.frame(H2prod_SoCal[,ncol(H2prod_SoCal)]))

region_PM10 <- data.frame(c("NorCal","NCV","SCV","SoCal"),
                         c(unlist(PM10_region_NorCal), unlist(PM10_region_NCV), unlist(PM10_region_SCV), unlist(PM10_region_SoCal)))
names(region_PM10) <- c("H2_Region","Net_PM10_emissions")

region_PM10cord <- region %>% 
  left_join(region_PM10, by = c("H2_Region" = "H2_Region")) 

PM10_supply_map <- ggplot() + 
              geom_sf(data = ca_bound) +
              geom_sf(data = region_PM10cord, aes(fill = Net_PM10_emissions)) + 
              geom_sf_text(data = region_PM10cord, aes(label = H2_Region)) +
              theme_bw() +
              theme(text=element_text(size = 12),
                legend.position = c(0.7, 0.75),
                axis.title = element_blank()) +
              scale_fill_gradient2(bquote("Particulate\nmatter 10\nemissions (g/kg"~H[2]*")"),
                       low = "orchid4", mid = "white", high = "orange", midpoint = 0) + 
  ggspatial::annotation_scale(data = region_NOxcord, location = "bl") +
  ggspatial::annotation_north_arrow(which_north = "true",location = "tr")

ggsave(paste0(getwd(), "/Results/Producer/Emissions_other/Regional_PM10_emissions.jpg"), plot = PM10_supply_map)

# Regional temporal net CO2 emissions
for (j in 1:ncol(H2prod_all))
{
  CO2_time_NorCal[,j] <- weighted.mean(CO2_net_NorCal, data.frame(H2prod_NorCal[,j]))
  CO2_time_NCV[,j] <- weighted.mean(CO2_net_NCV, data.frame(H2prod_NCV[,j]))
  CO2_time_SCV[,j] <- weighted.mean(CO2_net_SCV, data.frame(H2prod_SCV[,j]))
  CO2_time_SoCal[,j] <- weighted.mean(CO2_net_SoCal, data.frame(H2prod_SoCal[,j]))
}

Years_region <- as.numeric(rep(seq(2023,2031),times = 4))
Regions <- rep(c('NorCal', 'NCV', 'SCV', 'SoCal'),each = 9)
CO2_region1 <- bind_rows(transpose(CO2_time_NorCal), transpose(CO2_time_NCV), 
                          transpose(CO2_time_SCV), transpose(CO2_time_SoCal))

CO2_supply_temporal <- ggplot(data.frame(Years_region, CO2_region1, Regions), aes(x = Years_region, y = V1, group = Regions)) + 
                        geom_line(aes(color = Regions),size = 1.5) + 
                        ylab(bquote("Net carbon dioxide equivalent emissions (kg/kg"~H[2]*")")) + 
                        xlab("Years") +
                        theme_bw() +
                        theme(text=element_text(size = 16),
                              legend.title = element_blank(),
                              legend.position = "top") +
                        scale_x_continuous(breaks=c(seq(2023, 2031, by = 1)))

ggsave(paste0(getwd(), "/Results/Producer/Emissions_CO2/Temporal_CO2_emissions.jpg"), plot = CO2_supply_temporal)

# Category-wise regional plot
CO2_maint_NorCal_all <- weighted.mean(CO2_maint_NorCal, data.frame(H2prod_NorCal[,ncol(H2prod_NorCal)]))
CO2_maint_NCV_all <- weighted.mean(CO2_maint_NCV, data.frame(H2prod_NCV[,ncol(H2prod_NCV)]))
CO2_maint_SCV_all <- weighted.mean(CO2_maint_SCV, data.frame(H2prod_SCV[,ncol(H2prod_SCV)]))
CO2_maint_SoCal_all <- weighted.mean(CO2_maint_SoCal, data.frame(H2prod_SoCal[,ncol(H2prod_SoCal)]))

CO2_feedstock_NorCal_all <- weighted.mean(CO2_feedstock_NorCal, data.frame(H2prod_NorCal[,ncol(H2prod_NorCal)]))
CO2_feedstock_NCV_all <- weighted.mean(CO2_feedstock_NCV, data.frame(H2prod_NCV[,ncol(H2prod_NCV)]))
CO2_feedstock_SCV_all <- weighted.mean(CO2_feedstock_SCV, data.frame(H2prod_SCV[,ncol(H2prod_SCV)]))
CO2_feedstock_SoCal_all <- weighted.mean(CO2_feedstock_SoCal, data.frame(H2prod_SoCal[,ncol(H2prod_SoCal)]))

CO2_storage_NorCal_all <- weighted.mean(CO2_storage_NorCal, data.frame(H2prod_NorCal[,ncol(H2prod_NorCal)]))
CO2_storage_NCV_all <- weighted.mean(CO2_storage_NCV, data.frame(H2prod_NCV[,ncol(H2prod_NCV)]))
CO2_storage_SCV_all <- weighted.mean(CO2_storage_SCV, data.frame(H2prod_SCV[,ncol(H2prod_SCV)]))
CO2_storage_SoCal_all <- weighted.mean(CO2_storage_SoCal, data.frame(H2prod_SoCal[,ncol(H2prod_SoCal)]))

CO2_elec_NorCal_all <- weighted.mean(CO2_elec_NorCal, data.frame(H2prod_NorCal[,ncol(H2prod_NorCal)]))
CO2_elec_NCV_all <- weighted.mean(CO2_elec_NCV, data.frame(H2prod_NCV[,ncol(H2prod_NCV)]))
CO2_elec_SCV_all <- weighted.mean(CO2_elec_SCV, data.frame(H2prod_SCV[,ncol(H2prod_SCV)]))
CO2_elec_SoCal_all <- weighted.mean(CO2_elec_SoCal, data.frame(H2prod_SoCal[,ncol(H2prod_SoCal)]))

CO2_H2liqcomp_NorCal_all <- weighted.mean(CO2_H2liqcomp_NorCal, data.frame(H2prod_NorCal[,ncol(H2prod_NorCal)]))
CO2_H2liqcomp_NCV_all <- weighted.mean(CO2_H2liqcomp_NCV, data.frame(H2prod_NCV[,ncol(H2prod_NCV)]))
CO2_H2liqcomp_SCV_all <- weighted.mean(CO2_H2liqcomp_SCV, data.frame(H2prod_SCV[,ncol(H2prod_SCV)]))
CO2_H2liqcomp_SoCal_all <- weighted.mean(CO2_H2liqcomp_SoCal, data.frame(H2prod_SoCal[,ncol(H2prod_SoCal)]))

CO2_H2trans_NorCal_all <- weighted.mean(CO2_H2trans_NorCal, data.frame(H2prod_NorCal[,ncol(H2prod_NorCal)]))
CO2_H2trans_NCV_all <- weighted.mean(CO2_H2trans_NCV, data.frame(H2prod_NCV[,ncol(H2prod_NCV)]))
CO2_H2trans_SCV_all <- weighted.mean(CO2_H2trans_SCV, data.frame(H2prod_SCV[,ncol(H2prod_SCV)]))
CO2_H2trans_SoCal_all <- weighted.mean(CO2_H2trans_SoCal, data.frame(H2prod_SoCal[,ncol(H2prod_SoCal)]))

CO2_region_NorCal <- c(CO2_maint_NorCal_all, CO2_feedstock_NorCal_all, CO2_storage_NorCal_all, CO2_elec_NorCal_all, CO2_H2liqcomp_NorCal_all, CO2_H2trans_NorCal_all)
CO2_region_NCV <- c(CO2_maint_NCV_all, CO2_feedstock_NCV_all, CO2_storage_NCV_all, CO2_elec_NCV_all, CO2_H2liqcomp_NCV_all, CO2_H2trans_NCV_all)
CO2_region_SCV <- c(CO2_maint_SCV_all, CO2_feedstock_SCV_all, CO2_storage_SCV_all, CO2_elec_SCV_all, CO2_H2liqcomp_SCV_all, CO2_H2trans_SCV_all)
CO2_region_SoCal <- c(CO2_maint_SoCal_all, CO2_feedstock_SoCal_all, CO2_storage_SoCal_all, CO2_elec_SoCal_all, CO2_H2liqcomp_SoCal_all, CO2_H2trans_SoCal_all)

CO2_region <- data.frame(CO2_region_NorCal, CO2_region_NCV, CO2_region_SCV, CO2_region_SoCal)
CO2_category <- data.frame(c('Maintenance', 'Feedstock', 'Carbon sequestration', 'Electricity', 'Hydrogen storage', 'Hydrogen transport'))
CO2_category_region <- cbind(CO2_category, CO2_region)
colnames(CO2_category_region) <- c("Category","NorCal","NCV","SCV","SoCal")  
dfm1 <- pivot_longer(CO2_category_region, -Category, names_to = "region", values_to = "cost")

CO2_supply_category <- ggplot(dfm1, aes(x = region, y = cost, fill = Category)) +
                        geom_bar(stat = "identity") + #, position = "dodge") +
                        ylab(bquote("Net carbon dioxide equivalent emissions (kg/kg"~H[2]*")")) + 
                        xlab("Regions") +
                        theme_bw() +
                        theme(text=element_text(size = 16),
                              legend.title = element_blank(),
                              legend.position = "top") +
                        guides(fill=guide_legend(ncol=2)) +
                        scale_fill_viridis_d(option  = "cividis")

ggsave(paste0(getwd(), "/Results/Producer/Emissions_CO2/Breakdown_CO2_emissions.jpg"), plot = CO2_supply_category)

#################
# Cost plotting #
#################

# Regional total production cost
cost_region_NorCal <- weighted.mean(cost_total_NorCal, data.frame(H2prod_NorCal[,ncol(H2prod_NorCal)]))
cost_region_NCV <- weighted.mean(cost_total_NCV, data.frame(H2prod_NCV[,ncol(H2prod_NCV)]))
cost_region_SCV <- weighted.mean(cost_total_SCV, data.frame(H2prod_SCV[,ncol(H2prod_SCV)]))
cost_region_SoCal <- weighted.mean(cost_total_SoCal, data.frame(H2prod_SoCal[,ncol(H2prod_SoCal)]))

region_cost <- data.frame(c("NorCal","NCV","SCV","SoCal"),
                          c(unlist(cost_region_NorCal), unlist(cost_region_NCV), unlist(cost_region_SCV), unlist(cost_region_SoCal)))
names(region_cost) <- c("H2_Region","Total_production_cost")

region_costcord <- region %>% 
  left_join(region_cost, by = c("H2_Region" = "H2_Region")) 

cost_supply_map <- ggplot() + 
                    geom_sf(data = ca_bound) +
                    geom_sf(data = region_costcord, aes(fill = Total_production_cost)) + 
                    geom_sf_text(data = region_costcord, aes(label = H2_Region)) +
                    theme_bw() +
                    theme(text=element_text(size = 12),
                          legend.position = c(0.7, 0.75),
                          axis.title = element_blank()) +
                    scale_fill_gradient(bquote("Total\nproduction\ncost ($/kg"~H[2]*")"),low = "white", high = "orange") +
                    ggspatial::annotation_scale(data = region_costcord, location = "bl") +
                    ggspatial::annotation_north_arrow(which_north = "true",location = "tr") 

ggsave(paste0(getwd(), "/Results/Producer/Costs/Regional_cost.jpg"), plot = cost_supply_map)

# Regional temporal total production cost
for (j in 1:ncol(H2prod_all))
{
  cost_time_NorCal[,j] <- weighted.mean(cost_total_NorCal, data.frame(H2prod_NorCal[,j]))
  cost_time_NCV[,j] <- weighted.mean(cost_total_NCV, data.frame(H2prod_NCV[,j]))
  cost_time_SCV[,j] <- weighted.mean(cost_total_SCV, data.frame(H2prod_SCV[,j]))
  cost_time_SoCal[,j] <- weighted.mean(cost_total_SoCal, data.frame(H2prod_SoCal[,j]))
}

Years_region <- as.numeric(rep(seq(2023,2031),times = 4))
Regions <- rep(c('NorCal', 'NCV', 'SCV', 'SoCal'),each = 9)
cost_region1 <- bind_rows(transpose(cost_time_NorCal), transpose(cost_time_NCV), 
                          transpose(cost_time_SCV), transpose(cost_time_SoCal))

cost_supply_temporal <- ggplot(data.frame(Years_region, cost_region1, Regions), aes(x = Years_region, y = V1, group = Regions)) + 
                          geom_line(aes(color = Regions),size = 1.5) + 
                          ylab(bquote("Total production cost ($/kg"~H[2]*")")) + 
                          xlab("Years") +
                          theme_bw() +
                          theme(text=element_text(size = 16),
                                legend.title = element_blank(),
                                legend.position = "top") +
                          scale_x_continuous(breaks=c(seq(2023, 2031, by = 1)))

ggsave(paste0(getwd(), "/Results/Producer/Costs/Temporal_cost.jpg"), plot = cost_supply_temporal)

# Category-wise regional plot
capex_NorCal_all <- weighted.mean(capex_NorCal, data.frame(H2prod_NorCal[,ncol(H2prod_NorCal)]))
capex_NCV_all <- weighted.mean(capex_NCV, data.frame(H2prod_NCV[,ncol(H2prod_NCV)]))
capex_SCV_all <- weighted.mean(capex_SCV, data.frame(H2prod_SCV[,ncol(H2prod_SCV)]))
capex_SoCal_all <- weighted.mean(capex_SoCal, data.frame(H2prod_SoCal[,ncol(H2prod_SoCal)]))

cost_elec_NorCal <- weighted.mean(cost_elec_NorCal, data.frame(H2prod_NorCal[,ncol(H2prod_NorCal)]))
cost_elec_NCV <- weighted.mean(cost_elec_NCV, data.frame(H2prod_NCV[,ncol(H2prod_NCV)]))
cost_elec_SCV <- weighted.mean(cost_elec_SCV, data.frame(H2prod_SCV[,ncol(H2prod_SCV)]))
cost_elec_SoCal <- weighted.mean(cost_elec_SoCal, data.frame(H2prod_SoCal[,ncol(H2prod_SoCal)]))

cost_OM_NorCal <- weighted.mean(cost_OM_NorCal, data.frame(H2prod_NorCal[,ncol(H2prod_NorCal)]))
cost_OM_NCV <- weighted.mean(cost_OM_NCV, data.frame(H2prod_NCV[,ncol(H2prod_NCV)]))
cost_OM_SCV <- weighted.mean(cost_OM_SCV, data.frame(H2prod_SCV[,ncol(H2prod_SCV)]))
cost_OM_SoCal <- weighted.mean(cost_OM_SoCal, data.frame(H2prod_SoCal[,ncol(H2prod_SoCal)]))

cost_other_NorCal <- weighted.mean(cost_other_NorCal, data.frame(H2prod_NorCal[,ncol(H2prod_NorCal)]))
cost_other_NCV <- weighted.mean(cost_other_NCV, data.frame(H2prod_NCV[,ncol(H2prod_NCV)]))
cost_other_SCV <- weighted.mean(cost_other_SCV, data.frame(H2prod_SCV[,ncol(H2prod_SCV)]))
cost_other_SoCal <- weighted.mean(cost_other_SoCal, data.frame(H2prod_SoCal[,ncol(H2prod_SoCal)]))

cost_H2trans_NorCal <- weighted.mean(cost_H2trans_NorCal, data.frame(H2prod_NorCal[,ncol(H2prod_NorCal)]))
cost_H2trans_NCV <- weighted.mean(cost_H2trans_NCV, data.frame(H2prod_NCV[,ncol(H2prod_NCV)]))
cost_H2trans_SCV <- weighted.mean(cost_H2trans_SCV, data.frame(H2prod_SCV[,ncol(H2prod_SCV)]))
cost_H2trans_SoCal <- weighted.mean(cost_H2trans_SoCal, data.frame(H2prod_SoCal[,ncol(H2prod_SoCal)]))

cost_decomm_NorCal <- weighted.mean(cost_decomm_NorCal, data.frame(H2prod_NorCal[,ncol(H2prod_NorCal)]))
cost_decomm_NCV <- weighted.mean(cost_decomm_NCV, data.frame(H2prod_NCV[,ncol(H2prod_NCV)]))
cost_decomm_SCV <- weighted.mean(cost_decomm_SCV, data.frame(H2prod_SCV[,ncol(H2prod_SCV)]))
cost_decomm_SoCal <- weighted.mean(cost_decomm_SoCal, data.frame(H2prod_SoCal[,ncol(H2prod_SoCal)]))

cost_region_NorCal <- c(capex_NorCal_all, cost_elec_NorCal, cost_OM_NorCal, cost_other_NorCal, cost_H2trans_NorCal, cost_decomm_NorCal)
cost_region_NCV <- c(capex_NCV_all, cost_elec_NCV, cost_OM_NCV, cost_other_NCV, cost_H2trans_NCV, cost_decomm_NCV)
cost_region_SCV <- c(capex_SCV_all, cost_elec_SCV, cost_OM_SCV, cost_other_SCV, cost_H2trans_SCV, cost_decomm_SCV)
cost_region_SoCal <- c(capex_SoCal_all, cost_elec_SoCal, cost_OM_SoCal, cost_other_SoCal, cost_H2trans_SoCal, cost_decomm_SoCal)

cost_region <- data.frame(cost_region_NorCal, cost_region_NCV, cost_region_SCV, cost_region_SoCal)
cost_category <- data.frame(c('Capital', 'Electricity', 'Fixed O&M and insurance', 'Feedstock and labor', 'Hydrogen transport', 'Decommision'))
cost_category_region <- cbind(cost_category, cost_region)
colnames(cost_category_region) <- c("Category","NorCal","NCV","SCV","SoCal")  
dfm2 <- pivot_longer(cost_category_region, -Category, names_to = "region", values_to = "cost")

cost_supply_category <- ggplot(dfm2, aes(x = region, y = cost, fill = Category)) +
                          geom_bar(stat = "identity") + #, position = "dodge") +
                          ylab(bquote("Total production cost ($/kg"~H[2]*")")) + 
                          xlab("Regions") +
                          theme_bw() +
                          theme(legend.title = element_blank(),
                                text=element_text(size = 16),
                                legend.position = "top") +
                          guides(fill=guide_legend(ncol=2)) +
                          scale_fill_viridis_d(option  = "magma")

ggsave(paste0(getwd(), "/Results/Producer/Costs/Breakdown_cost.jpg"), plot = cost_supply_category)


#######################################################################
# Calculating LCA demand emissions one end-user at a time via looping #
#######################################################################

# Special calculation for trucks using ultra low sulfur diesel
H2dem_truck_all <- filter(cbind(Enduser[,4],Demand[,ncol(Demand)]), Usetype == "TRUCK")

for (i in 1:nrow(Enduser))
{
  #######################
  # General Information #
  #######################
  enduser_type <- Enduser[i,4]                      # H2 end-users (Truck, Transit, Power, Port, and Aviation)
  enduser_fuel <- Enduser[i,5]                      # end-user conventional fuel type (Natural gas and Ultra low sulfur diesel)
  H2dem <- Demand[i,-1]                             # H2 demand in the last year
  
  ###########################################################
  # O&M - California- and company-specific emission factors #
  ###########################################################
  grid_elec <- enduser_with_utility[i,2]            # grid electricity provider
  if (grepl("PG&E",grid_elec) == "TRUE"){
    EF_grid_CO2 <- LCA_parameter[26,2]              # EF of CO2eq for PG&E grid electricity: kg/kWh
    EF_grid_SO2 <- LCA_parameter[27,2]              # EF of SO2 for PG&E grid electricity: kg/kWh
    EF_grid_NOx <- LCA_parameter[28,2]              # EF of NOx for PG&E grid electricity: kg/kWh
    EF_grid_PM25 <- 0                               # EF of PM2.5 for PG&E grid electricity: kg/kWh
    EF_grid_PM10 <- LCA_parameter[29,2]             # EF of PM10 for PG&E grid electricity: kg/kWh
  }else if (grepl("SDG&E",grid_elec) == "TRUE"){
    EF_grid_CO2 <- LCA_parameter[30,2]              # EF of CO2eq for SDG&E grid electricity: kg/kWh
    EF_grid_SO2 <- LCA_parameter[31,2]              # EF of SO2 for SDG&E grid electricity: kg/kWh
    EF_grid_NOx <- LCA_parameter[32,2]              # EF of NOx for SDG&E grid electricity: kg/kWh
    EF_grid_PM25 <- 0                               # EF of PM2.5 for SDG&E grid electricity: kg/kWh
    EF_grid_PM10 <- LCA_parameter[33,2]             # EF of PM10 for SDG&E grid electricity: kg/kWh
  }else if (grepl("SCE",grid_elec) == "TRUE"){
    EF_grid_CO2 <- LCA_parameter[34,2]              # EF of CO2eq for SCE grid electricity: kg/kWh
    EF_grid_SO2 <- LCA_parameter[35,2]              # EF of SO2 for SCE grid electricity: kg/kWh
    EF_grid_NOx <- LCA_parameter[36,2]              # EF of NOx for SCE grid electricity: kg/kWh
    EF_grid_PM25 <- 0                               # EF of PM2.5 for SCE grid electricity: kg/kWh
    EF_grid_PM10 <- LCA_parameter[37,2]             # EF of PM10 for SCE grid electricity: kg/kWh
  }

  # Avoided emissions and resource consumption based on fuel-type
  if (grepl("Natural gas", enduser_fuel) == "TRUE"){
    # Avoided emissions
    CO2_avoid <- -EF_combustNG_CO2 * H2energy
    SO2_avoid <- -EF_combustNG_SO2 * H2energy
    NOx_avoid <- -EF_combustNG_NOx * H2energy
    PM25_avoid <- -EF_combustNG_PM25 * H2energy
    PM10_avoid <- -EF_combustNG_PM10 * H2energy
    
    # Fuel consumption
    # consump_NG_end[i,j] <- -((H2energy * H2dem[j] * 365)/(Heat_combustNG/Ener_combustNG)) * VEq_combustNG         # in gal/year
    
  }else if (grepl("Ultra low sulfur diesel", enduser_fuel) == "TRUE"){
    # Avoided emissions
    CO2_avoid <- -EF_ULSdiesel_CO2 * H2energy
    SO2_avoid <- -EF_ULSdiesel_SO2 * H2energy
    
    if (grepl("PORT", enduser_type) == "TRUE" | grepl("POWER", enduser_type) == "TRUE"){
      NOx_avoid <- -EF_ULSdiesel_NOx_Port * H2energy
      PM25_avoid <- -EF_ULSdiesel_PM25_Port * H2energy
      PM10_avoid <- -EF_ULSdiesel_PM10_Port * H2energy
      
    }else if (grepl("TRANSIT", enduser_type) == "TRUE"){
      NOx_avoid <- -EF_ULSdiesel_NOx_Trans * H2energy
      PM25_avoid <- -EF_ULSdiesel_PM25_Trans * H2energy
      PM10_avoid <- -EF_ULSdiesel_PM10_Trans * H2energy
      
    }else if (grepl("TRUCK", enduser_type) == "TRUE"){
      NOx_avoid <- (-EF_ULSdiesel_NOx_Truck * (H2dem[,ncol(H2dem)]/sum(H2dem_truck_all[,-1])))/H2dem[,ncol(H2dem)]
      PM25_avoid <- (-EF_ULSdiesel_PM25_Truck * (H2dem[,ncol(H2dem)]/sum(H2dem_truck_all[,-1])))/H2dem[,ncol(H2dem)]
      PM10_avoid <- (-EF_ULSdiesel_PM10_Truck * (H2dem[,ncol(H2dem)]/sum(H2dem_truck_all[,-1])))/H2dem[,ncol(H2dem)]
      
    }
    
    # Fuel consumption
    #if (grepl("TRUCK", enduser_type) == "TRUE"){
    #  consump_die_end[i,j] <- -(eff_dieseltruck* H2energy * H2dem[j] * 365)/Ener_ULSdiesel         # in gal/year
    #}else{
    #  consump_die_end[i,j] <- -(H2energy * H2dem[j] * 365)/Ener_ULSdiesel         # in gal/year
    #}
    
  }else if (grepl("Jet fuel", enduser_fuel) == "TRUE"){
    # Avoided emissions
    CO2_avoid <- -EF_jetfuel_CO2 * H2energy
    SO2_avoid <- -EF_jetfuel_SO2 * H2energy
    NOx_avoid <- -EF_jetfuel_NOx * H2energy
    PM25_avoid <- -EF_jetfuel_PM25 * H2energy
    PM10_avoid <- -EF_jetfuel_PM10 * H2energy
    
    # Fuel consumption
    #consump_jet_end[i,j] <- -(H2energy * H2dem[j] * 365)/Ener_jetfuel         # in gal/year
    
  }else if (grepl("SCE grid", enduser_fuel) == "TRUE"){
    
    CO2_avoid <- -(EF_grid_CO2/3.6) * H2energy
    SO2_avoid <- -(EF_grid_SO2/3.6) * H2energy
    NOx_avoid <- -(EF_grid_NOx/3.6) * H2energy
    PM25_avoid <- -(EF_grid_PM25/3.6) * H2energy
    PM10_avoid <- -(EF_grid_PM10/3.6) * H2energy
  }
  
  CO2_avoid_all[i,] <- CO2_avoid
  SO2_avoid_all[i,] <- SO2_avoid * 1000
  NOx_avoid_all[i,] <- NOx_avoid * 1000
  PM25_avoid_all[i,] <- PM25_avoid * 1000
  PM10_avoid_all[i,] <- PM10_avoid * 1000
  
  # Region-specific classification
  if (grepl("NorCal",enduser_with_region[i,2]) == "TRUE"){
    CO2_avoid_NorCal[i,1] <- CO2_avoid
    SO2_avoid_NorCal[i,1] <- SO2_avoid * 1000
    NOx_avoid_NorCal[i,1] <- NOx_avoid * 1000
    PM10_avoid_NorCal[i,1] <- PM10_avoid * 1000
    PM25_avoid_NorCal[i,1] <- PM25_avoid * 1000
    H2dem_NorCal[i,] <- H2dem
    
  } else if (grepl("NCV",enduser_with_region[i,2]) == "TRUE"){
    CO2_avoid_NCV[i,1] <- CO2_avoid
    SO2_avoid_NCV[i,1] <- SO2_avoid * 1000
    NOx_avoid_NCV[i,1] <- NOx_avoid * 1000
    PM10_avoid_NCV[i,1] <- PM10_avoid * 1000
    PM25_avoid_NCV[i,1] <- PM25_avoid * 1000
    H2dem_NCV[i,] <- H2dem
    
  } else if (grepl("SCV",enduser_with_region[i,2]) == "TRUE"){
    CO2_avoid_SCV[i,1] <- CO2_avoid
    SO2_avoid_SCV[i,1] <- SO2_avoid * 1000
    NOx_avoid_SCV[i,1] <- NOx_avoid * 1000
    PM10_avoid_SCV[i,1] <- PM10_avoid * 1000
    PM25_avoid_SCV[i,1] <- PM25_avoid * 1000
    H2dem_SCV[i,] <- H2dem
    
  } else if (grepl("SoCal",enduser_with_region[i,2]) == "TRUE"){
    CO2_avoid_SoCal[i,1] <- CO2_avoid
    SO2_avoid_SoCal[i,1] <- SO2_avoid * 1000
    NOx_avoid_SoCal[i,1] <- NOx_avoid * 1000
    PM10_avoid_SoCal[i,1] <- PM10_avoid * 1000
    PM25_avoid_SoCal[i,1] <- PM25_avoid * 1000
    H2dem_SoCal[i,] <- H2dem
    
  }
  
  
  # User type and region-specific classification
  if (grepl("PORT", enduser_type) == "TRUE"){
    if (grepl("NorCal",enduser_with_region[i,2]) == "TRUE"){
      CO2_port_NorCal[i,1] <- CO2_avoid
    } else if (grepl("NCV",enduser_with_region[i,2]) == "TRUE"){
      CO2_port_NCV[i,1] <- CO2_avoid
    } else if (grepl("SCV",enduser_with_region[i,2]) == "TRUE"){
      CO2_port_SCV[i,1] <- CO2_avoid
    } else if (grepl("SoCal",enduser_with_region[i,2]) == "TRUE"){
      CO2_port_SoCal[i,1] <- CO2_avoid
    }
    
  } else if (grepl("POWER", enduser_type) == "TRUE"){
    if (grepl("NorCal",enduser_with_region[i,2]) == "TRUE"){
      CO2_power_NorCal[i,1] <- CO2_avoid
    } else if (grepl("NCV",enduser_with_region[i,2]) == "TRUE"){
      CO2_power_NCV[i,1] <- CO2_avoid
    } else if (grepl("SCV",enduser_with_region[i,2]) == "TRUE"){
      CO2_power_SCV[i,1] <- CO2_avoid
    } else if (grepl("SoCal",enduser_with_region[i,2]) == "TRUE"){
      CO2_power_SoCal[i,1] <- CO2_avoid
    }
    
  } else if (grepl("TRANSIT", enduser_type) == "TRUE"){
    if (grepl("NorCal",enduser_with_region[i,2]) == "TRUE"){
      CO2_transit_NorCal[i,1] <- CO2_avoid
    } else if (grepl("NCV",enduser_with_region[i,2]) == "TRUE"){
      CO2_transit_NCV[i,1] <- CO2_avoid
    } else if (grepl("SCV",enduser_with_region[i,2]) == "TRUE"){
      CO2_transit_SCV[i,1] <- CO2_avoid
    } else if (grepl("SoCal",enduser_with_region[i,2]) == "TRUE"){
      CO2_transit_SoCal[i,1] <- CO2_avoid
    }
    
  } else if (grepl("TRUCK", enduser_type) == "TRUE"){
    if (grepl("NorCal",enduser_with_region[i,2]) == "TRUE"){
      CO2_truck_NorCal[i,1] <- CO2_avoid
    } else if (grepl("NCV",enduser_with_region[i,2]) == "TRUE"){
      CO2_truck_NCV[i,1] <- CO2_avoid
    } else if (grepl("SCV",enduser_with_region[i,2]) == "TRUE"){
      CO2_truck_SCV[i,1] <- CO2_avoid
    } else if (grepl("SoCal",enduser_with_region[i,2]) == "TRUE"){
      CO2_truck_SoCal[i,1] <- CO2_avoid
    }
    
  } else if (grepl("AVIATION", enduser_type) == "TRUE"){
    if (grepl("NorCal",enduser_with_region[i,2]) == "TRUE"){
      CO2_aviation_NorCal[i,1] <- CO2_avoid
    } else if (grepl("NCV",enduser_with_region[i,2]) == "TRUE"){
      CO2_aviation_NCV[i,1] <- CO2_avoid
    } else if (grepl("SCV",enduser_with_region[i,2]) == "TRUE"){
      CO2_aviation_SCV[i,1] <- CO2_avoid
    } else if (grepl("SoCal",enduser_with_region[i,2]) == "TRUE"){
      CO2_aviation_SoCal[i,1] <- CO2_avoid
    }
  }
}

enduser_results_all <- cbind(Enduser[,1],H2dem_all[,ncol(H2dem_all)],CO2_avoid_all,SO2_avoid_all,NOx_avoid_all,PM10_avoid_all,PM25_avoid_all)

# enduser results summary for senate district
# (please see comments on the "producers results summary for senate district" section for explanation on the algorithm)
for(k in 1:nrow(enduser_senate_all)) 
{
  enduser_senate_select <- get(paste0("enduser_S",as.double(enduser_senate_all[k,1]),"_cord"))
  enduser_senate_select <- enduser_senate_select %>% sf::st_drop_geometry(enduser_senate_select)
  enduser_out <- data.frame(matrix(NA, 6, nrow = nrow(enduser_senate_select)))
  
  for(j in 1:nrow(enduser_results_all)) {
    for(n in 1:nrow(enduser_senate_select)) {
      if (grepl(enduser_senate_select[n,1],enduser_results_all[j,1]) == "TRUE"){
        enduser_out[n,1:6] <- round(enduser_results_all[j,2:7],2)
      }
    }
  }
  
  if (enduser_senate_all[k,2] > 1) {
    enduser_out_senate <- data.frame(matrix(NA, 6, nrow = 1))
    enduser_out_senate[,1] <- mean(enduser_out[,1])
    enduser_out_senate[,2] <- weighted.mean(enduser_out[,2],enduser_out[,1])
    enduser_out_senate[,3] <- weighted.mean(enduser_out[,3],enduser_out[,1])
    enduser_out_senate[,4] <- weighted.mean(enduser_out[,4],enduser_out[,1])
    enduser_out_senate[,5] <- weighted.mean(enduser_out[,5],enduser_out[,1])
    enduser_out_senate[,6] <- weighted.mean(enduser_out[,6],enduser_out[,1])
    enduser_out <- enduser_out_senate
  }
  
  enduser_senate_all[k,3:8] <- enduser_out
}  

# enduser results summary for congressional district
# (please see comments on the "producers results summary for senate district" section for explanation on the algorithm)
for(k in 1:nrow(enduser_congress_all)) 
{
  enduser_congress_select <- get(paste0("enduser_C",as.double(enduser_congress_all[k,1]),"_cord"))
  enduser_congress_select <- enduser_congress_select %>% sf::st_drop_geometry(enduser_congress_select)
  enduser_out <- data.frame(matrix(NA, 6, nrow = nrow(enduser_congress_select)))
  
  for(j in 1:nrow(enduser_results_all)) {
    for(n in 1:nrow(enduser_congress_select)) {
      if (grepl(enduser_congress_select[n,1],enduser_results_all[j,1]) == "TRUE"){
        enduser_out[n,1:6] <- round(enduser_results_all[j,2:7],2)
      }
    }
  }
  
  if (enduser_congress_all[k,2] > 1) {
    enduser_out_congress <- data.frame(matrix(NA, 3, nrow = 1))
    enduser_out_congress[,1] <- mean(enduser_out[,1])
    enduser_out_congress[,2] <- weighted.mean(enduser_out[,2],enduser_out[,1])
    enduser_out_congress[,3] <- weighted.mean(enduser_out[,3],enduser_out[,1])
    enduser_out_congress[,4] <- weighted.mean(enduser_out[,4],enduser_out[,1])
    enduser_out_congress[,5] <- weighted.mean(enduser_out[,5],enduser_out[,1])
    enduser_out_congress[,6] <- weighted.mean(enduser_out[,6],enduser_out[,1])
    enduser_out <- enduser_out_congress
  }
  
  enduser_congress_all[k,3:8] <- enduser_out
}  

# endusers results summary for assembly district
# (please see comments on the "producers results summary for senate district" section for explanation on the algorithm)
for(k in 1:nrow(enduser_assembly_all)) 
{
  enduser_assembly_select <- get(paste0("enduser_A",as.double(enduser_assembly_all[k,1]),"_cord"))
  enduser_assembly_select <- enduser_assembly_select %>% sf::st_drop_geometry(enduser_assembly_select)
  enduser_out <- data.frame(matrix(NA, 6, nrow = nrow(enduser_assembly_select)))
  
  for(j in 1:nrow(enduser_results_all)) {
    for(n in 1:nrow(enduser_assembly_select)) {
      if (grepl(enduser_assembly_select[n,1],enduser_results_all[j,1]) == "TRUE"){
        enduser_out[n,1:6] <- round(enduser_results_all[j,2:7],2)
      }
    }
  }
  
  if (enduser_assembly_all[k,2] > 1) {
    enduser_out_assembly <- data.frame(matrix(NA, 3, nrow = 1))
    enduser_out_assembly[,1] <- mean(producer_out[,1])
    enduser_out_assembly[,2] <- weighted.mean(enduser_out[,2],enduser_out[,1])
    enduser_out_assembly[,3] <- weighted.mean(enduser_out[,3],enduser_out[,1])
    enduser_out_assembly[,4] <- weighted.mean(enduser_out[,4],enduser_out[,1])
    enduser_out_assembly[,5] <- weighted.mean(enduser_out[,5],enduser_out[,1])
    enduser_out_assembly[,6] <- weighted.mean(enduser_out[,6],enduser_out[,1])
    enduser_out <- enduser_out_assembly
  }
  
  enduser_assembly_all[k,3:8] <- enduser_out
}  
write_xlsx(list(senate_production = producer_senate_all, congress_production = producer_congress_all, assembly_production = producer_assembly_all,
                senate_enduse = enduser_senate_all, congress_enduse = enduser_congress_all, assembly_enduse = enduser_assembly_all), paste0(getwd(), "/Results/District_results.xlsx"))

######################
# emissions plotting #
######################

# Regional net CO2 emissions
CO2dem_region_NorCal <- weighted.mean(CO2_avoid_NorCal, data.frame(H2dem_NorCal[,ncol(H2dem_NorCal)]))
CO2dem_region_NCV <- weighted.mean(CO2_avoid_NCV, data.frame(H2dem_NCV[,ncol(H2dem_NCV)]))
CO2dem_region_SCV <- weighted.mean(CO2_avoid_SCV, data.frame(H2dem_SCV[,ncol(H2dem_SCV)]))
CO2dem_region_SoCal <- weighted.mean(CO2_avoid_SoCal, data.frame(H2dem_SoCal[,ncol(H2dem_SoCal)]))

region_CO2dem <- data.frame(c("NorCal","NCV","SCV","SoCal"),
                            c(unlist(CO2dem_region_NorCal), unlist(CO2dem_region_NCV), unlist(CO2dem_region_SCV), unlist(CO2dem_region_SoCal)))
names(region_CO2dem) <- c("H2_Region","Net_CO2_avoided")

region_CO2cord_dem <- region %>% 
  left_join(region_CO2dem, by = c("H2_Region" = "H2_Region")) 

CO2_avoid_map <- ggplot() + 
                  geom_sf(data = ca_bound) +
                  geom_sf(data = region_CO2cord_dem, aes(fill = Net_CO2_avoided)) + 
                  geom_sf_text(data = region_CO2cord_dem, aes(label = H2_Region)) +
                  theme_bw() +
                  theme(text=element_text(size = 12),
                        legend.position = c(0.7, 0.75),
                        axis.title = element_blank()) +
                  scale_fill_gradient(bquote("Carbon dioxide\nequivalent\nemissions (kg/kg"~H[2]*")"),low = "orchid4", high = "white") +
                  ggspatial::annotation_scale(data = region_CO2cord, location = "bl") +
                  ggspatial::annotation_north_arrow(which_north = "true",location = "tr") 

ggsave(paste0(getwd(), "/Results/Enduser/Emissions_CO2/Regional_CO2_avoided.jpg"), plot = CO2_avoid_map)

# Regional SO2 emissions
SO2dem_region_NorCal <- weighted.mean(SO2_avoid_NorCal, data.frame(H2dem_NorCal[,ncol(H2dem_NorCal)]))
SO2dem_region_NCV <- weighted.mean(SO2_avoid_NCV, data.frame(H2dem_NCV[,ncol(H2dem_NCV)]))
SO2dem_region_SCV <- weighted.mean(SO2_avoid_SCV, data.frame(H2dem_SCV[,ncol(H2dem_SCV)]))
SO2dem_region_SoCal <- weighted.mean(SO2_avoid_SoCal, data.frame(H2dem_SoCal[,ncol(H2dem_SoCal)]))

region_SO2dem <- data.frame(c("NorCal","NCV","SCV","SoCal"),
                            c(unlist(SO2dem_region_NorCal), unlist(SO2dem_region_NCV), unlist(SO2dem_region_SCV), unlist(SO2dem_region_SoCal)))
names(region_SO2dem) <- c("H2_Region","SO2_avoided")

region_SO2cord_dem <- region %>% 
  left_join(region_SO2dem, by = c("H2_Region" = "H2_Region")) 

SO2_avoid_map <- ggplot() + 
                  geom_sf(data = ca_bound) +
                  geom_sf(data = region_SO2cord_dem, aes(fill = SO2_avoided)) + 
                  geom_sf_text(data = region_SO2cord_dem, aes(label = H2_Region)) +
                  theme_bw() +
                  theme(text=element_text(size = 12),
                        legend.position = c(0.7, 0.75),
                        axis.title = element_blank()) +
                  scale_fill_gradient(bquote("Sulfur dioxide\nemissions (g/kg"~H[2]*")"),low = "orchid4", high = "white") +
                  ggspatial::annotation_scale(data = region_SO2cord, location = "bl") +
                  ggspatial::annotation_north_arrow(which_north = "true",location = "tr") 

ggsave(paste0(getwd(), "/Results/Enduser/Emissions_other/Regional_SO2_avoided.jpg"), plot = SO2_avoid_map)

# Regional NOx emissions
NOxdem_region_NorCal <- weighted.mean(NOx_avoid_NorCal, data.frame(H2dem_NorCal[,ncol(H2dem_NorCal)]))
NOxdem_region_NCV <- weighted.mean(NOx_avoid_NCV, data.frame(H2dem_NCV[,ncol(H2dem_NCV)]))
NOxdem_region_SCV <- weighted.mean(NOx_avoid_SCV, data.frame(H2dem_SCV[,ncol(H2dem_SCV)]))
NOxdem_region_SoCal <- weighted.mean(NOx_avoid_SoCal, data.frame(H2dem_SoCal[,ncol(H2dem_SoCal)]))

region_NOxdem <- data.frame(c("NorCal","NCV","SCV","SoCal"),
                            c(unlist(NOxdem_region_NorCal), unlist(NOxdem_region_NCV), unlist(NOxdem_region_SCV), unlist(NOxdem_region_SoCal)))
names(region_NOxdem) <- c("H2_Region","NOx_avoided")

region_NOxcord_dem <- region %>% 
  left_join(region_NOxdem, by = c("H2_Region" = "H2_Region")) 

NOx_avoid_map <- ggplot() + 
                  geom_sf(data = ca_bound) +
                  geom_sf(data = region_NOxcord_dem, aes(fill = NOx_avoided)) + 
                  geom_sf_text(data = region_NOxcord_dem, aes(label = H2_Region)) +
                  theme_bw() +
                  theme(text=element_text(size = 12),
                        legend.position = c(0.7, 0.75),
                        axis.title = element_blank()) +
                  scale_fill_gradient(bquote("Nitrogen\noxides\nemissions (g/kg"~H[2]*")"),low = "orchid4", high = "white") +
                  ggspatial::annotation_scale(data = region_NOxcord, location = "bl") +
                  ggspatial::annotation_north_arrow(which_north = "true",location = "tr") 

ggsave(paste0(getwd(), "/Results/Enduser/Emissions_other/Regional_NOx_avoided.jpg"), plot = NOx_avoid_map)

# Regional PM10 emissions
PM10dem_region_NorCal <- weighted.mean(PM10_avoid_NorCal, data.frame(H2dem_NorCal[,ncol(H2dem_NorCal)]))
PM10dem_region_NCV <- weighted.mean(PM10_avoid_NCV, data.frame(H2dem_NCV[,ncol(H2dem_NCV)]))
PM10dem_region_SCV <- weighted.mean(PM10_avoid_SCV, data.frame(H2dem_SCV[,ncol(H2dem_SCV)]))
PM10dem_region_SoCal <- weighted.mean(PM10_avoid_SoCal, data.frame(H2dem_SoCal[,ncol(H2dem_SoCal)]))

region_PM10dem <- data.frame(c("NorCal","NCV","SCV","SoCal"),
                            c(unlist(PM10dem_region_NorCal), unlist(PM10dem_region_NCV), unlist(PM10dem_region_SCV), unlist(PM10dem_region_SoCal)))
names(region_PM10dem) <- c("H2_Region","PM10_avoided")

region_PM10cord_dem <- region %>% 
  left_join(region_PM10dem, by = c("H2_Region" = "H2_Region")) 

PM10_avoid_map <- ggplot() + 
                    geom_sf(data = ca_bound) +
                    geom_sf(data = region_PM10cord_dem, aes(fill = PM10_avoided)) + 
                    geom_sf_text(data = region_PM10cord_dem, aes(label = H2_Region)) +
                    theme_bw() +
                    theme(text=element_text(size = 12),
                          legend.position = c(0.7, 0.75),
                          axis.title = element_blank()) +
                    scale_fill_gradient(bquote("Particulate\nmatter 10\nemissions (g/kg"~H[2]*")"),low = "orchid4", high = "white") +
                    ggspatial::annotation_scale(data = region_NOxcord, location = "bl") +
                    ggspatial::annotation_north_arrow(which_north = "true",location = "tr") 

ggsave(paste0(getwd(), "/Results/Enduser/Emissions_other/Regional_PM10_avoided.jpg"), plot = PM10_avoid_map)

# Regional PM25 emissions
PM25dem_region_NorCal <- weighted.mean(PM25_avoid_NorCal, data.frame(H2dem_NorCal[,ncol(H2dem_NorCal)]))
PM25dem_region_NCV <- weighted.mean(PM25_avoid_NCV, data.frame(H2dem_NCV[,ncol(H2dem_NCV)]))
PM25dem_region_SCV <- weighted.mean(PM25_avoid_SCV, data.frame(H2dem_SCV[,ncol(H2dem_SCV)]))
PM25dem_region_SoCal <- weighted.mean(PM25_avoid_SoCal, data.frame(H2dem_SoCal[,ncol(H2dem_SoCal)]))

region_PM25dem <- data.frame(c("NorCal","NCV","SCV","SoCal"),
                             c(unlist(PM25dem_region_NorCal), unlist(PM25dem_region_NCV), unlist(PM25dem_region_SCV), unlist(PM25dem_region_SoCal)))
names(region_PM25dem) <- c("H2_Region","PM25_avoided")

region_PM25cord_dem <- region %>% 
  left_join(region_PM25dem, by = c("H2_Region" = "H2_Region")) 

PM25_avoid_map <- ggplot() + 
                    geom_sf(data = ca_bound) +
                    geom_sf(data = region_PM25cord_dem, aes(fill = PM25_avoided)) + 
                    geom_sf_text(data = region_PM25cord_dem, aes(label = H2_Region)) +
                    theme_bw() +
                    theme(text=element_text(size = 12),
                          legend.position = c(0.7, 0.75),
                          axis.title = element_blank()) +
                    scale_fill_gradient(bquote("Particulate\nmatter 2.5\nemissions (g/kg"~H[2]*")"),low = "orchid4", high = "white") +
                    ggspatial::annotation_scale(data = region_NOxcord, location = "bl") +
                    ggspatial::annotation_north_arrow(which_north = "true",location = "tr") 

ggsave(paste0(getwd(), "/Results/Enduser/Emissions_other/Regional_PM25_avoided.jpg"), plot = PM25_avoid_map)

# Regional temporal net CO2 emissions
for (j in 1:ncol(H2dem_all))
{
  CO2_time_NorCal_dem[,j] <- weighted.mean(CO2_avoid_NorCal, data.frame(H2dem_NorCal[,j]))
  CO2_time_NCV_dem[,j] <- weighted.mean(CO2_avoid_NCV, data.frame(H2dem_NCV[,j]))
  CO2_time_SCV_dem[,j] <- weighted.mean(CO2_avoid_SCV, data.frame(H2dem_SCV[,j]))
  CO2_time_SoCal_dem[,j] <- weighted.mean(CO2_avoid_SoCal, data.frame(H2dem_SoCal[,j]))
}

Years_region <- as.numeric(rep(seq(2023,2031),times = 4))
Regions <- rep(c('NorCal', 'NCV', 'SCV', 'SoCal'),each = 9)
CO2dem_region1 <- bind_rows(transpose(CO2_time_NorCal_dem), transpose(CO2_time_NCV_dem), 
                            transpose(CO2_time_SCV_dem), transpose(CO2_time_SoCal_dem))

avoided_CO2_demand_temporal <- ggplot(data.frame(Years_region, CO2dem_region1, Regions), aes(x = Years_region, y = V1, group = Regions)) + 
                                geom_line(aes(color = Regions),size = 1.5) + 
                                ylab(bquote("Net carbon dioxide equivalent emissions (kg/kg"~H[2]*")")) + 
                                xlab("Years") +
                                theme_bw() +
                                theme(text=element_text(size = 16),
                                      legend.title = element_blank(),
                                      legend.position = "top") +
                                scale_x_continuous(breaks=c(seq(2023, 2031, by = 1)))

ggsave(paste0(getwd(), "/Results/Enduser/Emissions_CO2/Temporal_CO2_avoided.jpg"), plot = avoided_CO2_demand_temporal)

# Category-wise regional plot
CO2_power_NorCal_all <- weighted.mean(CO2_power_NorCal, data.frame(H2dem_NorCal[,ncol(H2dem_NorCal)]))
CO2_power_NCV_all <- weighted.mean(CO2_power_NCV, data.frame(H2dem_NCV[,ncol(H2dem_NCV)]))
CO2_power_SCV_all <- weighted.mean(CO2_power_SCV, data.frame(H2dem_SCV[,ncol(H2dem_SCV)]))
CO2_power_SoCal_all <- weighted.mean(CO2_power_SoCal, data.frame(H2dem_SoCal[,ncol(H2dem_SoCal)]))

CO2_transit_NorCal_all <- weighted.mean(CO2_transit_NorCal, data.frame(H2dem_NorCal[,ncol(H2dem_NorCal)]))
CO2_transit_NCV_all <- weighted.mean(CO2_transit_NCV, data.frame(H2dem_NCV[,ncol(H2dem_NCV)]))
CO2_transit_SCV_all <- weighted.mean(CO2_transit_SCV, data.frame(H2dem_SCV[,ncol(H2dem_SCV)]))
CO2_transit_SoCal_all <- weighted.mean(CO2_transit_SoCal, data.frame(H2dem_SoCal[,ncol(H2dem_SoCal)]))

CO2_truck_NorCal_all <- weighted.mean(CO2_truck_NorCal, data.frame(H2dem_NorCal[,ncol(H2dem_NorCal)]))
CO2_truck_NCV_all <- weighted.mean(CO2_truck_NCV, data.frame(H2dem_NCV[,ncol(H2dem_NCV)]))
CO2_truck_SCV_all <- weighted.mean(CO2_truck_SCV, data.frame(H2dem_SCV[,ncol(H2dem_SCV)]))
CO2_truck_SoCal_all <- weighted.mean(CO2_truck_SoCal, data.frame(H2dem_SoCal[,ncol(H2dem_SoCal)]))

CO2_port_NorCal_all <- weighted.mean(CO2_port_NorCal, data.frame(H2dem_NorCal[,ncol(H2dem_NorCal)]))
CO2_port_NCV_all <- weighted.mean(CO2_port_NCV, data.frame(H2dem_NCV[,ncol(H2dem_NCV)]))
CO2_port_SCV_all <- weighted.mean(CO2_port_SCV, data.frame(H2dem_SCV[,ncol(H2dem_SCV)]))
CO2_port_SoCal_all <- weighted.mean(CO2_port_SoCal, data.frame(H2dem_SoCal[,ncol(H2dem_SoCal)]))

CO2_aviation_NorCal_all <- weighted.mean(CO2_aviation_NorCal, data.frame(H2dem_NorCal[,ncol(H2dem_NorCal)]))
CO2_aviation_NCV_all <- weighted.mean(CO2_aviation_NCV, data.frame(H2dem_NCV[,ncol(H2dem_NCV)]))
CO2_aviation_SCV_all <- weighted.mean(CO2_aviation_SCV, data.frame(H2dem_SCV[,ncol(H2dem_SCV)]))
CO2_aviation_SoCal_all <- weighted.mean(CO2_aviation_SoCal, data.frame(H2dem_SoCal[,ncol(H2dem_SoCal)]))

CO2_region_NorCal_dem <- c(CO2_power_NorCal_all, CO2_transit_NorCal_all, CO2_truck_NorCal_all, CO2_port_NorCal_all, CO2_aviation_NorCal_all)
CO2_region_NCV_dem <- c(CO2_power_NCV_all, CO2_transit_NCV_all, CO2_truck_NCV_all, CO2_port_NCV_all, CO2_aviation_NCV_all)
CO2_region_SCV_dem <- c(CO2_power_SCV_all, CO2_transit_SCV_all, CO2_truck_SCV_all, CO2_port_SCV_all, CO2_aviation_SCV_all)
CO2_region_SoCal_dem <- c(CO2_power_SoCal_all, CO2_transit_SoCal_all, CO2_truck_SoCal_all, CO2_port_SoCal_all, CO2_aviation_SoCal_all)

CO2_region_dem <- data.frame(CO2_region_NorCal_dem, CO2_region_NCV_dem, CO2_region_SCV_dem, CO2_region_SoCal_dem)
CO2_category_dem <- data.frame(c('Power', 'Transit', 'Truck', 'Port', 'Aviation'))
CO2_category_region_dem <- cbind(CO2_category_dem, CO2_region_dem)
colnames(CO2_category_region_dem) <- c("Category","NorCal","NCV","SCV","SoCal")  
dfm3 <- pivot_longer(CO2_category_region_dem, -Category, names_to = "region", values_to = "cost")

avoided_CO2_demand_category <- ggplot(dfm3, aes(x = region, y = cost, fill = Category)) +
                                geom_bar(stat = "identity") + #, position = "dodge") +
                                ylab(bquote("Net carbon dioxide equivalent emissions (kg/kg"~H[2]*")")) + 
                                xlab("Regions") +
                                theme_bw() +
                                theme(text=element_text(size = 16),
                                      legend.title = element_blank(),
                                      legend.position = "top") +
                                guides(fill=guide_legend(ncol=2)) +
                                scale_fill_viridis_d(option  = "rocket")

ggsave(paste0(getwd(), "/Results/Enduser/Emissions_CO2/Breakdown_CO2_avoided.jpg"), plot = avoided_CO2_demand_category)