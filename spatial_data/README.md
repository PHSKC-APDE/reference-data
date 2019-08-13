# reference-data/spatial_data

## Notes
These files are used for cross-walk between different levels of geography. In August 2019 there was an attempt to reduce reduancy and combine files. However, some redudancy continues to exist, so the user will have to examine the files to ensure they are using the one best suited to their needs.  

The CSVs were originally created from tabs in '[composition of places-HRA.xlsx](https://kc1.sharepoint.com/:x:/r/teams/PHc/datareq/_layouts/15/Doc.aspx?sourcedoc=%7BB791BD4F-1554-49F9-8E12-29F132764949%7D&file=composition%20of%20places-HRA.xlsx)' on SharePoint.

## Table of Contents
* **[acs_council_districts.csv](https://github.com/PHSKC-APDE/reference-data/blob/master/spatial_data/acs_council_districts.csv)** = Cross-walk from 2010 census geoid / tract to King County and Seattle City Council districts
* **[acs_hra_region_place_etc.csv](https://github.com/PHSKC-APDE/reference-data/blob/master/spatial_data/acs_hra_region_place_etc.csv)** = Cross-walk between HRA names, KC regions, COO places, 2010 census geoids, places, tracts, and block groups
* **[blocks00_to_hra.csv](https://github.com/PHSKC-APDE/reference-data/blob/master/spatial_data/blocks00_to_hra.csv)** = Cross-walk between 2000 (?) census blocks and HRA names ... legacy, no longer used
* **[blocks10_to_city_council_dist.csv](https://github.com/PHSKC-APDE/reference-data/blob/master/spatial_data/blocks10_to_city_council_dist.csv)** = Cross-walk between 2010 census blocks and council districts; includes acreage of land and and water
* **[blocks10_to_hra.csv](https://github.com/PHSKC-APDE/reference-data/blob/master/spatial_data/blocks10_to_hra.csv)** = Cross-walk between 2010 census blocks and HRAs
* **[blocks10_to_region.csv](https://github.com/PHSKC-APDE/reference-data/blob/master/spatial_data/blocks10_to_region.csv)** = Crosswalk between 2010 census blocks, HRAs, and KC regions
* **[hra_vid_region.csv](https://github.com/PHSKC-APDE/reference-data/blob/master/spatial_data/hra_vid_region.csv)** = Crosswalk between HRAs, vids, and KC regions
* **[school_dist_to_region.csv](https://github.com/PHSKC-APDE/reference-data/blob/master/spatial_data/school_dist_to_region.csv)** = Crosswalk between King County school districts and KC regions
* **[tract_to_county_council_dist.csv](https://github.com/PHSKC-APDE/reference-data/blob/master/spatial_data/tract_to_county_council_dist.csv)** = Crosswalk between 2010 census tracts and King County Council Districts.
* **[tract10_to_puma.csv](https://github.com/PHSKC-APDE/reference-data/blob/master/spatial_data/tract10_to_puma.csv)** = Crosswalk between 2010 census tracts and PUMAs (Public Use Microdata Areas) 
* **[zip_admin.csv](https://github.com/PHSKC-APDE/reference-data/blob/master/spatial_data/zip_admin.csv)** = 122 zip codes with zip type and city, used to define King County in admnistative data (e.g., claims, housing, etc.)
* **[zip_city_region_vid.csv](https://github.com/PHSKC-APDE/reference-data/blob/master/spatial_data/zip_city_region_vid.csv)** = Crosswalk between 120 zip codes (with cities) to KC regions [**DATA IS NOT CLEANLY FORMATTED**]
* **[zip_hca.csv](https://github.com/PHSKC-APDE/reference-data/blob/master/spatial_data/zip_hca.csv)** = 133 zip codes, with zip type, city, primary_city, used to define King County in HCA Medicaid data extracts. Includes 98354 and 98422, 
which barely cross into south KC). ZIPs spanning county lines are noted in the two_counties field.
* **[zip_to_city_council_dist.csv](https://github.com/PHSKC-APDE/reference-data/blob/master/spatial_data/zip_to_city_council_dist.csv)** = Crosswalk between 25 zip codes and Seattle City Council Districts
* **[zip_to_region.csv](https://github.com/PHSKC-APDE/reference-data/blob/master/spatial_data/zip_to_region.csv)** = Crosswalk between 120 zip codes and Communities Count regions

