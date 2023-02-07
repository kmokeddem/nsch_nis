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
