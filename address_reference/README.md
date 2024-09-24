# Address Reference Table

## Purpose

The address reference table synthesizes information about addresses using sources in the Integrated Data Hub. Users can take their cleaned and/or geocoded addresses and join them to the table to retrieve information on the type of address (e.g. outpatient health facility, housing, pharmacy, etc.).

Currently, this contains address information from the Medicaid and HMIS data sources.

More information and useful documentation, such as the crosswalk, can be found in the docs/ref_schema section of the HHSAW Users Sharepoint

## Usage

1.  If you haven't yet installed [`kcgeocode`](https://github.com/PHSKC-APDE/kcgeocode/), follow the directions on the Github page to do so.

2.  Become familiar with the kcgeocode package usage using the vignettes and READMEs.

3.  Follow the directions to fetch the geocoded versions of your addresses (either clean/fetch or clean/upload/fetch, depending on whether your addresses have been geocoded before). The primary output you need out of this step is the geo_hash_clean column to use for merging. Note that you COULD try just merging on the raw addresses without any of the geocoding, but this is more prone to errors and missed matches.

4.  Load the address_reference table and merge it onto your addresses using the geo_hash_clean column.

5.  Check to see which ones successfully merged and whether the results are what you'd expect.

6.  You're ready to roll!

## Important Notes

-   There are potentially multiple rows for each address! If an address contains multiple offices, for instance, then it may have a row for the pharmacy info, one for the outpatient health facility info, and more. Expect the merge to create more rows than you started with.

-   The levels of detail columns describing address type are works in progress. The most detailed level is currently based on the values that are in HMIS and Medicaid for facility type, while the next level up is a guess at which midlevel groupings would be relevant. If you see something wrong, let us know in the issues.

## Problems?

-   If you come across a bug or have specific suggestions for improvement, please click on ["Issues"](https://github.com/PHSKC-APDE/reference-data/issues) at the top of this page and then click ["New Issue"](https://github.com/PHSKC-APDE/reference-data/issues/new/choose) and provide the necessary details.
