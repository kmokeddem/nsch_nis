# nsch_nis

First unzip data:
cd data
unzip data.zip
cd ..

Standard rules for building:
mkdir build
cd build
cmake ..
make

Then to regenerate the data set if you want:
./src/nsch_nis ../data/2020-2021\ NSCH_Topical_CAHMI_DRC.csv ../data/Vaccination_Coverage_among_Young_Children__0-35_Months_.csv > ../data/joined_data.csv 

Then you can find R code in script.R
