# hamiltonCovid19 0.0.0.9000

#### 2020-11-18
* Added irish_hosp_data and irish_icu_data (also scrape and update code).
* Changed values of Ireland Hospitalised and ICU cases at Summary tab to the current values (instead of the total number).

#### 2020-11-17
* Animation tab plot now starts always 30 days before the current date.
* Added back in grid lines on all plots.
* Rounded values in Map tab leaflet plots.
* Fixing problem with SQRT and Log variables at animation tab.

#### 2020-11-03
* Added tests for global_data, irish_data and irish_country_data update workflow.

#### 2020-10-30
* Transfered repository from curso-r to hamilton-institute organization.

#### 2020-10-26
* Fixed issue 31: selecting global in country selection is not working

#### 2020-10-23
* Fixed issue 19: change value boxes variable in summary tab
* Fixed issue 20: check 14-day cases per 100k residents table in summary tab
* Fixed issue 21: change variable in map tab
* Fixed issue 30: fix numbers in legend
* Minor layout fixes in Summary tab
* Removed spinners in Summary tab that was making the app shake

#### 2020-10-21
* Fixed issue 12: remove axis label from legend on "graphs" tab
* Fixed issue 22: add variable option in Graphs tab
* Now it is only possible to select multiple variables in the "graphs" tab if only one country is selected

#### 2020-10-19
* Created scrape scripts
* Added Github Actions to automatic deploy

#### 2020-10-15
* Minor changes to the CSS file

#### 2020-10-08
* Created distill theme
* Added variable selector for ranking table in summary tab
* Fixed the play bar to start on the 1st day of previous month in animations tab

#### 2020-09-30
* Replaced DT with reactable
* Created reactable theme

#### 2020-09-28
* Recreated Interventions tab
* Recreated Sources tab
* Recreated dark theme

#### 2020-09-24
* Recreated Animations tab

#### 2020-09-23
* Recreated Graphs tab

#### 2020-09-21
* Recreated ranking tables in Summary tab
* Recreated (county) Map tab

#### 2020-09-16
* Recreated Ireland leaflet map in Summary tab
* Changed sidebar width
* Recreated global valueboxes in Summary tab

#### 2020-09-14
* Created `bs4dash` layout
* Recreated valueboxes for Ireland data in Summary tab

#### 2020-08-13
* Added a `NEWS.md` file to track changes to the package.
* Added default MIT LICENSE.
* Added Maynooth University Hamilton Institute favicon.
