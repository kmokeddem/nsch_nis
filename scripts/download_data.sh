#!/usr/bin/env bash
# Download raw inputs for scripts/build_joined_data.py into data/raw/.
#   NSCH  : Census Bureau public-use topical files (Stata), survey years 2016-2025
#   NIS   : CDC "Vaccination Coverage among Young Children (0-35 Months)" (data.cdc.gov fhky-rtsk)
#   Glyph : USGS EPest county-level agricultural pesticide use estimates, 2010-2019
#   Pop   : Census county population estimates 2010-2020 + 2019 county gazetteer (land area)
set -euo pipefail
cd "$(dirname "$0")/../data"
mkdir -p raw/nsch/extracted raw/nis raw/glyphosate raw/population

# ---- NSCH ----
for y in 2016 2017 2018 2019 2020 2021 2022 2023 2024 2025; do
  f=raw/nsch/nsch_${y}_topical_Stata.zip
  [ -s "$f" ] && continue
  name=nsch_${y}_topical_Stata.zip
  [ "$y" = 2023 ] && name=nsch_2023e_topical_Stata.zip
  curl -fL -o "$f" "https://www2.census.gov/programs-surveys/nsch/datasets/$y/$name"
done
for f in raw/nsch/*.zip; do unzip -o -q -d raw/nsch/extracted "$f" '*.dta'; done

# ---- NIS ----
curl -fL -o raw/nis/nis_fhky-rtsk.csv \
  "https://data.cdc.gov/api/views/fhky-rtsk/rows.csv?accessType=DOWNLOAD"

# ---- NIS-Child public-use files, survey years 2003-2024 (for scripts/reconstruct_nis.py) ----
mkdir -p raw/nis_puf
N=https://ftp.cdc.gov/pub/Health_Statistics/NCHS/Datasets/nis
V=https://ftp.cdc.gov/pub/Vaccines_NIS
M=https://www.cdc.gov/nis/media/files
get() { [ -s "$1" ] || curl -fL -o "$1" "$2"; }
get raw/nis_puf/nispuf03.zip $N/nispuf03dat.zip;  get raw/nis_puf/nispuf03.sas $N/nispuf03.sas
get raw/nis_puf/nispuf04.zip $N/nispuf04dat.zip;  get raw/nis_puf/nispuf04.sas $N/nispuf04.sas
get raw/nis_puf/nispuf05.zip $N/nispuf05_dat.zip; get raw/nis_puf/nispuf05.sas $N/nispuf05.sas
get raw/nis_puf/nispuf06.zip $N/nispuf06.dat.zip; get raw/nis_puf/nispuf06.sas $N/nispuf06.sas
get raw/nis_puf/nispuf07.zip $N/nispuf07.dat.zip; get raw/nis_puf/nispuf07.sas $N/nispuf07.sas
for z in raw/nis_puf/*.zip; do [ -e "$z" ] && unzip -o -q -d raw/nis_puf "$z" && rm "$z"; done
for y in 08 09 10 11 12 13 14; do get raw/nis_puf/nispuf$y.dat $N/nispuf$y.dat; get raw/nis_puf/nispuf$y.sas $N/nispuf$y.sas; done
for y in 15 16 17 18 20 21 22; do get raw/nis_puf/nispuf$y.dat $V/NISPUF$y.DAT; done
for y in 16 17 18 20 21 22; do get raw/nis_puf/nispuf$y.r $V/NISPUF$y.R; done   # 2015 layout: scripts/nis_layouts/
get raw/nis_puf/nispuf19.dat $M/2024/09/NISPUF19.DAT; get raw/nis_puf/nispuf19.sas $M/2024/09/NISPUF19.SAS
get raw/nis_puf/nispuf23.dat $M/2024/11/NISPUF23.DAT; get raw/nis_puf/nispuf23.sas $M/2024/11/NISPUF23.SAS
get raw/nis_puf/nispuf24.dat $M/2026/05/NISPUF24.DAT; get raw/nis_puf/nispuf24.sas $M/2026/05/NISPUF24.SAS

# ---- Glyphosate (USGS) ----
B=https://water.usgs.gov/nawqa/pnsp/usage/maps/county-level/PesticideUseEstimates
for y in 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012; do
  curl -fL -o raw/glyphosate/EPest.county.estimates.$y.txt $B/EPest.county.estimates.$y.txt
done
# 2013-2017 final (doi:10.5066/P9F2SRYH)
curl -fL -o raw/glyphosate/EPest_county_estimates_2013_2017_v2.txt \
  "https://www.sciencebase.gov/catalog/file/get/5e95c12282ce172707f2524e?f=__disk__62%2F83%2Fd3%2F6283d3501f1028b1ccc3976ea2e6de848bc2fef8"
# 2018 preliminary (doi:10.5066/P920L09S)
curl -fL -o raw/glyphosate/EPest_county_estimates_2018.txt \
  "https://www.sciencebase.gov/catalog/file/get/6081a706d34e8564d686618e?f=__disk__58%2F6a%2Fed%2F586aed9a844eac0174a0600c8a7293ec4cda0265"
# 2019 preliminary (doi:10.5066/P9EDTHQL)
curl -fL -o raw/glyphosate/EPest_county_estimates_2019.txt \
  "https://www.sciencebase.gov/catalog/file/get/6081a924d34e8564d68661a1?f=__disk__08%2F42%2Fcd%2F0842cdac3a7d8b5056645a4dc08d1da96ad4e0b7"

# ---- County population + land area ----
curl -fL -o raw/population/co-est2009-alldata.csv \
  https://www2.census.gov/programs-surveys/popest/datasets/2000-2009/counties/totals/co-est2009-alldata.csv
curl -fL -o raw/population/co-est2020-alldata.csv \
  https://www2.census.gov/programs-surveys/popest/datasets/2010-2020/counties/totals/co-est2020-alldata.csv
curl -fL -o raw/population/gaz_counties.zip \
  https://www2.census.gov/geo/docs/maps-data/data/gazetteer/2019_Gazetteer/2019_Gaz_counties_national.zip
unzip -o -q -d raw/population raw/population/gaz_counties.zip && rm raw/population/gaz_counties.zip

echo "done"
