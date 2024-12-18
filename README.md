**Hydrogen Hub Systems Analysis and Mapping Tool (Paracraft) v1**

Hanna M Breunig; Adam Z Weber; Seongeun Jeong; Mohammed Tamim Zaki

The hydrogen (H<sub>2</sub>) hub systems analysis and mapping tool "Paracraft"
developed using R version 4.3.2 is an integrated life cycle assessment
(LCA), techno-economic analysis (TEA), and geospatial analysis tool. The
tool quantifies the environmental and economic impacts of a H<sub>2</sub> hub
that includes feedstock, production, infrastructure/storage,
distribution, and end-use of H<sub>2</sub> within the system boundary. Additional
modules and impacts will be integrated into the software throughout the
Department of Energy (DOE) ARCHES hydrogen hub project through a
multi-institution collaboration involving the Lawrence Berkeley National
Laboratory, University of California Davis, University of California
Berkeley, University of California Irvine, Renewables100, National
Renewable Energy Laboratory, ARCHES LLC, and Jacobs.

Version 1 of the tool includes modules for the following components:

1.  <ins>Feedstock</ins>: Electricity (grid, power purchase
    agreement, and renewables), water, and biomass (municipal solid
    waste and woody waste)

2.  <ins>Production</ins>: Alkaline water electrolysis, Proton
    exchange membrane (PEM) water electrolysis, Biomass thermochemical
    conversion, and Biomass gasification.

3.  <ins>Infrastructure/storage</ins>: H<sub>2</sub> compression, H<sub>2</sub>
    liquefaction, and storage for compressed and liquefied H<sub>2</sub>.

4.  <ins>Distribution</ins>: Truck transport of compressed and
    liquefied H<sub>2</sub> and pipeline transport of compressed H<sub>2</sub>.

5.  <ins>End-use</ins>: Refueling stations and medium duty class 6
    trucks, transit buses, fuel cell and peaker plant for power
    generation, and aviation fuel.

The environmental impacts from LCA are quantified as net carbon dioxide
equivalent emissions (kg CO<sub>2</sub>e/kg H<sub>2</sub>), sulfur dioxide emissions (g
SO<sub>2</sub>/kg H<sub>2</sub>), nitrogen oxide emissions (g NO<sub>x</sub>/kg H<sub>2</sub>), and
particulate matter emissions (g PM<sub>10</sub>/kg H<sub>2</sub> and g PM<sub>2.5</sub>/kg H<sub>2</sub>).
Health impacts and benefits associated with net changes in air quality
will be provided in a subsequent version.

The economic impact from TEA is quantified as total levelized cost
(\$/kg H<sub>2</sub>). Net present value, total revenue, total policy incentives,
and other financial performance indicators will be provided in a
subsequent version following the approach used in the DOE H2FAST model.

***Capabilities of version 1***

-   <ins>Projects</ins>: Flexible inclusion, exclusion, and
    modification of projects in a hub over time.

-   <ins>LCA</ins>: Emissions from feedstock preparation and
    acquisition, H<sub>2</sub> production, H<sub>2</sub> postprocessing and compression
    and/or liquefaction, H<sub>2</sub> transport, and avoided emissions from H<sub>2</sub>
    end-uses that offset current fossil fuel consumption.

-   <ins>TEA</ins>: Total levelized cost (capital, operation and
    maintenance) of feedstock preparation, H<sub>2</sub> production, H<sub>2</sub>
    compression and/or liquefaction, and H<sub>2</sub> transport. End-user markup and total levelized cost to consumers will be available
in the next version.

-   <ins>Geospatial</ins>: Linking projects to specific regions in
    California such as Northern California (NorCal), North Central
    Valley (NCV), South Central Valley (SCV), and Southern California
    (SoCal). Linking projects to different legislative districts in
    California such as senate, congress, and assembly.

-   <ins>Visualization</ins>: Creating summary tables, plots, and
    maps for individual projects, market sectors, technology types, and
    geospatial boundaries.

***Getting Started***

In addition to the R code, which can be run in IDEs (Integrated
Development Environments) such as Rstudio and Visual Studio, the tool
requires a partially user-defined input Excel spreadsheet
("H2hub_input.xlsx") and GIS (Geographic Information System) shapefiles
located in the "Geodata" folder. The "Geodata" folder includes polygon
shapefiles for regions in California, utilities, senate, congressional, and assembly
districts obtained from the California State Geoportal Website.

To get started:

<ins>Step 1</ins>: Preparation of the input data. The input Excel
spreadsheet includes six tabs and should be prepared as follows:

1.  <ins>LCA parameters</ins>: Default life cycle inventory including
    electricity use, water use, and emission factors collected from
    literature, US DOE H2A model, GREET model, and eGRID. **No user
    input** is needed in this tab to run the code.

2.  <ins>TEA parameters</ins>: Default capital, electricity costs,
    water costs and other operation and maintenance costs collected from
    literature, US DOE H2A model, and HDSAM model. **No user input** is
    needed in this tab.

3.  <ins>Producer</ins>: Data for each projects including production,
    liquefaction and/or compression, storage, and transport. **Mandatory
    user inputs** in this tab are:

    -   *Name*: Type project name.

    -   *Latitude* and *Longitude*: Type project location in decimal
        degrees.

    -   *H<sub>2</sub> production process*: Type "Alkaline electrolysis", "PEM
        electrolysis", "Biomass gasification", or "Biomass
        thermochemical". These inputs are case-sensitive.

    -   *Ratio of liquified H<sub>2</sub>* and *Ratio of compressed H<sub>2</sub>*: Type
        ratios of produced H2 liquified and/or compressed ranging
        between 0 and 1.

    -   *PV electricity ratio for H<sub>2</sub> production, PPA electricity ratio
        for H<sub>2</sub> production, Other renewable electricity ratio for H<sub>2</sub>
        production, PHS electricity ratio for H<sub>2</sub> production,* and
        *Grid electricity ratio for H<sub>2</sub> production*: Type ratios of
        various electricity sources used for H<sub>2</sub> production ranging
        between 0 and 1. When using multiple electricity sources, user
        should make sure that the sum of the ratios is 1.

    -   *Biomass type*: Type "Woody" or "MSW". These inputs are case-sensitive.

    -   *Biomass consumption*: Type biomass used for H<sub>2</sub> production in specified units.

    -   *Biomass emissions avoidance source*: Type "Landfill" or "N/A".
        These inputs are case-sensitive.

    -   *H<sub>2</sub> Distribution*: Type "Truck" or "Pipeline". These inputs
        are case-sensitive.

    -   *H<sub>2</sub> travel distance*: Type distance liquefied and/compressed
        H<sub>2</sub> travels via truck and/or pipeline in specified units.

    -   *H<sub>2</sub> transport cost*: Type cost of transporting H<sub>2</sub> via truck
        and/or pipeline in specified units.

Among **optional user inputs** in this tab are: *Electrolyzer size,
Total uninstalled CAPEX, Stack CAPEX, BoP CAPEX, Liquefier CAPEX,
Storage tank CAPEX, Misc. electricity usage, Water consumption,* and
*Water cost*. "N/A" should be typed unless project-specific values are
known in the specified units.

Apart from the mandatory and optional user inputs, there are **default
inputs** (*Labor cost, Electrolyzer maintenance cost, KOH and/or N<sub>2</sub>
cost, Liquefier maintenance cost, Natural gas consumption by biomass
conversion, Diesel consumption by biomass conversion, Biomass transport
and preparation emissions, Biomass conversion maintenance emissions, Soil carbon storage, SO<sub>2</sub> emissions from biomass conversion, NO<sub>x</sub> emissions from biomass conversion,* and *PM10 emissions from biomass conversion*), which if known by user should be typed in the specified
units.

4.  <ins>Supply</ins>: Year-specific average daily H<sub>2</sub> production data in specified units for each of the producers entered in the "Producer" tab. **User input is mandatory** for this tab to run the code.

5.  <ins>End-user</ins>: Data for each end-user projects. **All
    inputs are mandatory** in this tab:

    -   *Name*: type project name.

    -   *Latitude* and *Longitude*: type project location in decimal
        degrees.

    -   *Use type*: type "TRANSIT", "TRUCK", "POWER", or "AVIATION".
        These inputs are case-sensitive.

    -   *Fuel type*: type "Natural gas" or "Ultra low sulfur diesel".
        These inputs represent the type of fuel avoided by H<sub>2</sub>. These
        inputs are case-sensitive.

6.  <ins>Demand</ins>: Year-specific average daily H<sub>2</sub> demand data in specified units for each of the endusers entered in the "Enduser" tab. **User input is mandatory** for this tab to run the code.

<ins>Step 2</ins>: Open the code in an IDE and set the directory for
the input Excel spreadsheet and shapefiles folder so that the code can
pull the data for analysis and modeling.

<ins>Step 3</ins>: Run the code. Note that after the first run,
installing the packages is not required and can be deactivated by
putting "#" in front of the "install.packages()" to make the later runs
quicker.

<ins>Step 4</ins>: Once the code run is complete, the results will be
saved in the same directory under the "Results" folder (note that the
existing files will be replaced by newer files with each run. Hence,
change the name of the main folder if needed for later comparison):

1.  <ins>District_results.xlsx</ins>: This spreadsheet includes a
    summary of the environmental and economic impacts, categorized into
    senate, congressional, and assembly districts and producers and
    end-users.

2.  <ins>Producer</ins>:

    a.  <ins>Emissions_CO2</ins>: Net CO<sub>2</sub> emissions from feedstock
        preparation, H<sub>2</sub> production, H<sub>2</sub> compression and/or
        liquefaction, and H<sub>2</sub> transport.

       &nbsp;&nbsp;&nbsp;&nbsp;i.  <ins>Regional_CO2_emissions.jpg </ins>: Map with regional
            CO<sub>2</sub> emissions.

       &nbsp;&nbsp;&nbsp;&nbsp;ii. <ins>Temporal_CO2_emissions.jpg</ins>: Plot with temporal
            CO<sub>2</sub> emissions.

       &nbsp;&nbsp;&nbsp;&nbsp;iii. <ins>Breakdown_CO2_emissions.jpg</ins>: Plot with
             breakdown of CO<sub>2</sub> emissions.
    <br></br>
    b.  <ins>Emissions_other</ins>: Other criteria emissions from
        feedstock preparation, H<sub>2</sub> production, H<sub>2</sub> compression and/or
        liquefaction, and H<sub>2</sub> transport.

       &nbsp;&nbsp;&nbsp;&nbsp;i.  <ins>Regional_SO2_emissions.jpg</ins>: Map with regional
            SO<sub>2</sub> emissions.

       &nbsp;&nbsp;&nbsp;&nbsp;ii. <ins>Regional_NOx_emissions.jpg</ins>: Map with regional
            NO<sub>x</sub> emissions.

       &nbsp;&nbsp;&nbsp;&nbsp;iii. <ins>Regional_PM10_emissions.jpg</ins>: Map with
             regional PM10 emissions.

    c.  <ins>Costs</ins>: Total costs from feedstock preparation,
        H<sub>2</sub> production, H<sub>2</sub> compression and/or liquefaction, and H<sub>2</sub>
        transport.

       &nbsp;&nbsp;&nbsp;&nbsp;i.  <ins>Regional_cost.jpg</ins>: Map with regional total
            costs.

       &nbsp;&nbsp;&nbsp;&nbsp;ii. <ins>Temporal_cost.jpg</ins>: Plot with temporal total
            costs.

       &nbsp;&nbsp;&nbsp;&nbsp;iii. <ins>Breakdown_cost.jpg</ins>: Plot with breakdown of
             total costs.

3.  <ins>Enduser</ins>:

    a.  <ins>Emissions_CO2</ins>: Net CO<sub>2</sub> emissions avoided by
        endusers.

       &nbsp;&nbsp;&nbsp;&nbsp;i.  <ins>Regional_CO2_avoided.jpg</ins>: Map with regional
            CO<sub>2</sub> emissions.

       &nbsp;&nbsp;&nbsp;&nbsp;ii. <ins>Temporal_CO2_avoided.jpg</ins>: Plot with temporal
            CO<sub>2</sub> emissions.

       &nbsp;&nbsp;&nbsp;&nbsp;iii. <ins>Breakdown_CO2_avoided.jpg</ins>: Plot with
             breakdown of CO<sub>2</sub> emissions.

    b.  <ins>Emissions_other</ins>: Other criteria emissions avoided
        by endusers.

       &nbsp;&nbsp;&nbsp;&nbsp;i.  <ins>Regional_SO2_emissions.jpg</ins>: Map with regional
            SO<sub>2</sub> emissions.

       &nbsp;&nbsp;&nbsp;&nbsp;ii. <ins>Regional_NOx_emissions.jpg</ins>: Map with regional
            NO<sub>x</sub> emissions.

       &nbsp;&nbsp;&nbsp;&nbsp;iii. <ins>Regional_PM10_emissions.jpg</ins>: Map with
             regional PM10 emissions.

Copyright Notice

Hydrogen Hub Systems Analysis and Mapping Tool (ParaCraft) Copyright (c)
2024, The Regents of the University of California, through Lawrence
Berkeley National Laboratory (subject to receipt of any required
approvals from the U.S. Dept. of Energy). All rights reserved.

If you have questions about your rights to use or distribute this
software, please contact Berkeley Lab\'s Intellectual Property Office at
<IPO@lbl.gov>.\
\
NOTICE.  This Software was developed under funding from the U.S.
Department of Energy and the U.S. Government consequently retains
certain rights.  As such, the U.S. Government has been granted for
itself and others acting on its behalf a paid-up, nonexclusive,
irrevocable, worldwide license in the Software to reproduce, distribute
copies to the public, prepare derivative  works, and perform publicly
and display publicly, and to permit others to do so.

---
**License Agreement**

Hydrogen Hub Systems Analysis and Mapping Tool (ParaCraft) Copyright (c) 2024, The Regents of the University of California, through Lawrence Berkeley National Laboratory (subject to receipt of any required approvals from the U.S. Dept. of Energy). All rights reserved. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

(1) Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.

(2) Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.

(3) Neither the name of the University of California, Lawrence Berkeley National Laboratory, U.S. Dept. of Energy nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

You are under no obligation whatsoever to provide any bug fixes, patches, or upgrades to the features, functionality or performance of the source code ("Enhancements") to anyone; however, if you choose to make your Enhancements available either publicly, or directly to Lawrence Berkeley National Laboratory, without imposing a separate written license agreement for such Enhancements, then you hereby grant the following license: a non-exclusive, royalty-free perpetual license to install, use, modify, prepare derivative works, incorporate into other computer software, distribute, and sublicense such enhancements or derivative works thereof, in binary and source code form.

