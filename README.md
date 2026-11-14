# nsch_nis

<h3>First unzip data:</h3>
cd data<br>
unzip data.zip<br>
cd ..<br>
<p>
<h3>Standard rules for building:</h3>
mkdir build<br>
cd build<br>
cmake ..<br>
make<br>
<p>
<h3>Then to regenerate the data set if you want:</h3>
./src/nsch_nis ../data/2020-2021\ NSCH_Topical_CAHMI_DRC.csv ../data/Vaccination_Coverage_among_Young_Children__0-35_Months_.csv > ../data/joined_data.csv <br>
<p>
Then you can find R code in script.R

<h3>Extended dataset (NSCH 2019-2025, NIS birth years 2011-2022, glyphosate)</h3>
pip install -r scripts/requirements.txt<br>
./scripts/download_data.sh &nbsp;&nbsp;# raw files -> data/raw/ (~400 MB, not committed)<br>
python3 scripts/build_joined_data.py &nbsp;&nbsp;# writes data/joined_data2025.csv<br>
<p>
One row per birth year (<code>year</code>) x state FIPS (<code>region</code>), same layout as joined_data2023.csv.
<ul>
<li>NSCH: Census public-use topical files, surveys 2019-2025 pooled (2016-2018 lack BIRTH_YR). Unweighted prevalence of
autism, allergies, ADHD, asthma, epilepsy, diabetes, blood disorders, obesity (doctor-identified overweight), Tourette's.
<code>asd_prev3</code>/<code>asd_prev4</code> = autism first diagnosed before age 3/4. <code>mom_age</code> = mother's age at birth (2019-2022 surveys only).
<code>sc_age_mean</code> = mean child age at survey (later birth years are observed younger, so "ever diagnosed" prevalence is lower).</li>
<li>NIS: CDC data.cdc.gov fhky-rtsk; column naming identical to the C++ joiner. Missing estimates are NA (old file used 0).</li>
<li>Glyphosate: USGS EPest county estimates (2018-2019 preliminary, <code>glyph_prelim</code>=1), conterminous US only (AK/HI NA), none after 2019.
<code>glyph_kg_*</code> state total kg; <code>glyph_kg_km2_*</code> kg per km2 of state land; <code>glyph_popw_*</code> population-weighted mean county
kg/km2 (main exposure proxy); <code>glyph_birthw_*</code> same weighted by county births. <code>_low</code>/<code>_high</code> = EPest-low/high.</li>
</ul>

<h3>NIS reconstructed from the public-use files (birth years 2002-2021)</h3>
python3 scripts/reconstruct_nis.py &nbsp;&nbsp;# data/raw/nis_puf/ -> data/nis_reconstructed.csv (same format as the CDC CSV)<br>
python3 scripts/build_joined_data.py --nis reconstructed &nbsp;&nbsp;# -> data/joined_data2025_nis_reconstructed.csv<br>
Rscript script2025.R data/joined_data2025_nis_reconstructed.csv results_nis_reconstructed<br>
<p>
The NIS-Child public-use files (survey years 2003-2024) have no birth dates, only survey year and age group
(19-23/24-29/30-35 months), so each child is split across its two possible birth years by probability
(see the docstring in scripts/reconstruct_nis.py). Coverage by age is a weighted Kaplan-Meier estimate from
the provider-reported age in days at each dose. Against CDC's official birth-year estimates for 2011-2021:
r = 0.989, mean absolute error 1.6 percentage points, mean bias +0.14 (results/nis_reconstruction_validation.txt).
Only "k+ doses by age m" measures are rebuilt (DTaP, HepB, Hib, PCV, Polio, MMR, Varicella, HepA); Hib series,
rotavirus, influenza and the combined 7-vaccine series are not.
The 2015 file layout was transcribed from the codebook (scripts/nis_layouts/nispuf15.sas).

<h3>Vaccine aluminum in micrograms (from NIS dose type codes)</h3>
python3 scripts/reconstruct_aluminum.py &nbsp;&nbsp;# -> data/nis_aluminum.csv, data/nis_aluminum_product_mix.csv<br>
then rebuild the joined CSVs; their <code>al_*</code>/<code>alcum_*</code> columns feed Part 4 of script2025.R.
<p>
Each provider-reported dose has an age in days and a type code (e.g. 08 = Pediarix, D3 = Pentacel, HM = PedvaxHIB).
Doses are converted to micrograms of aluminum from the package inserts. Combination shots are counted once.
<code>al_{low,high}_m</code> is the mean ug received in the window ending at m months (the paper's windows: 3, 5, 7, 13, 19, 24, 35), and
<code>alcum_*</code> is the cumulative total. Where the code doesn't identify the product, the bounds use the lightest (<code>low</code>)
and heaviest (<code>high</code>) possible product. That applies to plain "DTaP" (Tripedia 170 / Daptacel 330 / Infanrix 625), Hib before
the 2009 surveys (0 or 225) and HepA (225/250). The full mapping is in the script docstring. From birth year 2011 the bounds agree closely
(r = 0.91-0.98 by window); for 2002-2010 the infant windows are poorly identified (r = 0.01-0.52).
