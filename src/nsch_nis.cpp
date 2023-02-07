//============================================================================
// Name        : nsch_nis.cpp
// Author      : Kamal Mokeddem
// Version     :
// Copyright   : 2023
// Description : join datasets
//============================================================================

#include <iostream>
#include <stdlib.h>
#include <fstream>
#include <sstream>
#include <string>
#include <vector>
#include <map>
#include <algorithm>

#include "fips.h"

struct row
{
	int birth_yr;
	int state;
	int asd;
	int allergies;
	int asthma;
	int adhd;
	int epilepsy;
	int diabetes;
	int hospital;
	int hospital_er;
	int arthritis;
	int tourettes;
};

struct nsch_data
{
	double asd_prevalence;
	int asd_count;
	double allergies_prevalence;
	int allergies_count;
	double asthma_prevalence;
	int asthma_count;
	double adhd_prevalence;
	int adhd_count;
	double epilepsy_prevalence;
	int epilepsy_count;
	double diabetes_prevalence;
	int diabetes_count;
	double hospital_prevalence;
	int hospital_count;
	double hospital_er_prevalence;
	int hospital_er_count;
	double arthritis_prevalence;
	int arthritis_count;
	double tourettes_prevalence;
	int tourettes_count;
};

struct nis
{
	int year;
	int state;
	std::string shot;
	std::string dose;
	int months;
	double estimate;

	std::string getLabel()
	{
		char c[2048];
		sprintf(c, "%s.%s.%d", shot.c_str(), dose.c_str(), months);

		return std::string(c);
	}
};


std::string nschHeader;
std::vector<std::string> v_nsch;
std::vector<std::string> v_nis;
std::vector<row> v_rows;
std::vector<nis> v_nisData;
std::map<std::string, std::map<int, std::map<int, double> > > nis_map;
std::map<int, std::map<int, nsch_data> > nsch_map;


void parseNsch(std::string filename)
{
	std::ifstream infile1(filename);
	std::getline(infile1, nschHeader);
	std::string line;
	int i = 0;
	while (std::getline(infile1, line))
	{
	    v_nsch.push_back(line);
	    i++;
	}
	//printf("read %d lines\n", i);
}

void parseNis(std::string filename)
{
	std::ifstream infile2(filename);
	std::string line;
	int i = 0;
	std::getline(infile2, line); // get header
	while (std::getline(infile2, line))
	{
		v_nis.push_back(line);
		i++;
	}
	//printf("read %d lines\n", i);
}

void extractNsch()
{
	for(auto line : v_nsch)
	{
		int i = 1;
		row r;
		std::stringstream ss(line);
		std::string str;
		while (getline(ss, str, ','))
		{
			if(i == 1)
			{
				r.state = atoi(str.c_str());
			}
			if(i == 12)
			{
				r.birth_yr = atoi(str.c_str());
				//printf("birth year = %s\n", str.c_str());
			}
			if(i == 108)
			{
				r.asd = atoi(str.c_str());
				//printf("asd = %s\n", str.c_str());
			}
			if(i == 69)
			{
				r.asthma = atoi(str.c_str());
			}
			if(i == 65)
			{
				r.allergies = atoi(str.c_str());
			}
			if(i == 73)
			{
				r.diabetes = atoi(str.c_str());
			}
			if(i == 75)
			{
				r.epilepsy = atoi(str.c_str());
			}
			if(i == 112)
			{
				r.adhd = atoi(str.c_str());
			}
			if(i == 138)
			{
				r.hospital = atoi(str.c_str());
			}
			if(i == 343)
			{
				r.hospital_er = atoi(str.c_str());
			}
			if(i == 67)
			{
				r.arthritis = atoi(str.c_str());
			}
			if(i == 82)
			{
				r.tourettes = atoi(str.c_str());
			}
			i++;
			if(r.birth_yr < 2011 || r.birth_yr > 2017)
				continue;
		}
		v_rows.push_back(r);
	}
}

void computePrevalence(int year, int region)
{
	int asd_yes = 0;
	int asd_total = 0;
	int allergies_yes = 0;
	int allergies_total = 0;
	int asthma_yes = 0;
	int asthma_total = 0;
	int adhd_yes = 0;
	int adhd_total = 0;
	int epilepsy_yes = 0;
	int epilepsy_total = 0;
	int diabetes_yes = 0;
	int diabetes_total = 0;
	int hospital_yes = 0;
	int hospital_total = 0;
	int hospital_er_yes = 0;
	int hospital_er_total = 0;
	int arthritis_yes = 0;
	int arthritis_total = 0;
	int tourettes_yes = 0;
	int tourettes_total = 0;

	for(auto r : v_rows)
	{
		if(r.birth_yr == year && r.state == region)
		{
			if(r.asd == 1 || r.asd == 2)
			{
				asd_total++;
				if(r.asd == 1)
					asd_yes++;
			}
			if(r.allergies == 1 || r.allergies == 2)
			{
				allergies_total++;
				if(r.allergies == 1)
					allergies_yes++;
			}
			if(r.adhd == 1 || r.adhd == 2)
			{
				adhd_total++;
				if(r.adhd == 1)
					adhd_yes++;
			}
			if(r.asthma == 1 || r.asthma == 2)
			{
				asthma_total++;
				if(r.asthma == 1)
					asthma_yes++;
			}
			if(r.epilepsy == 1 || r.epilepsy == 2)
			{
				epilepsy_total++;
				if(r.epilepsy == 1)
					epilepsy_yes++;
			}
			if(r.diabetes == 1 || r.diabetes == 2)
			{
				diabetes_total++;
				if(r.diabetes == 1)
					diabetes_yes++;
			}
			if(r.hospital == 1 || r.hospital == 2)
			{
				hospital_total++;
				if(r.hospital == 1)
					hospital_yes++;
			}
			if(r.hospital_er == 1 || r.hospital_er == 2)
			{
				hospital_er_total++;
				if(r.hospital_er == 1)
					hospital_er_yes++;
			}
			if(r.arthritis == 1 || r.arthritis == 2)
			{
				arthritis_total++;
				if(r.arthritis == 1)
					arthritis_yes++;
			}
			if(r.tourettes == 1 || r.tourettes == 2)
			{
				tourettes_total++;
				if(r.tourettes == 1)
					tourettes_yes++;
			}
		}
	}
	nsch_data data;
	data.asd_count = asd_total;
	data.asd_prevalence = (double)asd_yes/(double)asd_total;
	data.allergies_count = allergies_total;
	data.allergies_prevalence = (double)allergies_yes/(double)allergies_total;
	data.adhd_count = adhd_total;
	data.adhd_prevalence = (double)adhd_yes/(double)adhd_total;
	data.asthma_count = asthma_total;
	data.asthma_prevalence = (double)asthma_yes/(double)asthma_total;
	data.epilepsy_count = epilepsy_total;
	data.epilepsy_prevalence = (double)epilepsy_yes/(double)epilepsy_total;
	data.diabetes_count = diabetes_total;
	data.diabetes_prevalence = (double)diabetes_yes/(double)diabetes_total;
	data.hospital_count = hospital_total;
	data.hospital_prevalence = (double)hospital_yes/(double)hospital_total;
	data.hospital_er_count = hospital_er_total;
	data.hospital_er_prevalence = (double)hospital_er_yes/(double)hospital_er_total;
	data.arthritis_count = arthritis_total;
	data.arthritis_prevalence = (double)arthritis_yes/(double)arthritis_total;
	data.tourettes_count = tourettes_total;
	data.tourettes_prevalence = (double)tourettes_yes/(double)tourettes_total;


	//printf("autism prevalence in %d for region %d = %lf, based on %d samples\n",
	//		year, region, data.prevalence, data.count);

	nsch_map[year][region] = data;
}

bool invalidChar(char c)
{
	return !(c>=0 && c < 128);
}

std::string stripString(std::string str)
{
	str.erase(std::remove(str.begin(), str.end(), ' '), str.end());
	str.erase(std::remove(str.begin(), str.end(), ','), str.end());
	str.erase(std::remove_if(str.begin(), str.end(), invalidChar), str.end());
	str.erase(std::remove(str.begin(), str.end(), '('), str.end());
	str.erase(std::remove(str.begin(), str.end(), ')'), str.end());
	return str;
}

void extractNIS()
{
	for(auto line : v_nis)
	{
		int i = 1;
		nis n;
		std::stringstream ss(line);
		std::string str;
		while (getline(ss, str, ','))
		{
			if(i == 1)
			{
				n.shot = stripString(str);
			}
			if(i == 2)
			{
				n.dose = stripString(str);
			}
			if(i == 3)
			{
				if(str != "States/Local Areas")
				{
					break;
				}
			}
			if(i == 4)
			{
				std::map<std::string, int>::iterator iter = state_code_map.find(str);
				if(iter != state_code_map.end())
				{
					n.state = iter->second;
				}
				else
					break;
			}
			if(i == 5)
			{
				if(str.size() > 5)
					break;
				n.year = atoi(str.c_str());
				if(n.year > 2017)
					break;
			}
			if(i == 6)
			{
				if(str != "Age")
					break;
			}
			if(i == 7)
			{
				if(str.find("Months") != str.npos)
				{
					n.months = atoi(str.c_str());
				}
				else
					break;
			}
			if(i == 8)
			{
				n.estimate = atof(str.c_str())/100.0;
				if(n.state > 0)
					v_nisData.push_back(n);
			}
			i++;
		}


	}

}

void constructLabels()
{
	for(auto x : v_nisData)
	{
		std::string label = x.getLabel();
		nis_map[label][x.year][x.state] = x.estimate;
	}
}

void outputData()
{
	//print header
	printf("year,region,asd_prev,asd_count,allergies_prev,allergies_count");
	printf(",adhd_prev,adhd_count,asthma_prev,asthma_count,epilepsy_prev,epilepsy_count");
	printf(",diabetes_prev,diabetes_count,hospital_prev,hopital_count");
	printf(",hospital_er_prev,hopital_er_count,arthritis_prev,arthritis_count,tourettes_prev,tourettes_count");
	for(auto p : nis_map)
	{
		printf(",%s", p.first.c_str());
	}
	printf("\n");

	for(int i = 2011; i < 2018; i++)
	{
		for(auto p : state_code_map)
		{
			printf("%d,%d,%lf,%d,%lf,%d,%lf,%d,%lf,%d,%lf,%d,%lf,%d,%lf,%d,%lf,%d,%lf,%d,%lf,%d",
					i, p.second,
					nsch_map[i][p.second].asd_prevalence, nsch_map[i][p.second].asd_count,
					nsch_map[i][p.second].allergies_prevalence, nsch_map[i][p.second].allergies_count,
					nsch_map[i][p.second].adhd_prevalence, nsch_map[i][p.second].adhd_count,
					nsch_map[i][p.second].asthma_prevalence, nsch_map[i][p.second].asthma_count,
					nsch_map[i][p.second].epilepsy_prevalence, nsch_map[i][p.second].epilepsy_count,
					nsch_map[i][p.second].diabetes_prevalence, nsch_map[i][p.second].diabetes_count,
					nsch_map[i][p.second].hospital_prevalence, nsch_map[i][p.second].hospital_count,
					nsch_map[i][p.second].hospital_er_prevalence, nsch_map[i][p.second].hospital_er_count,
					nsch_map[i][p.second].arthritis_prevalence, nsch_map[i][p.second].arthritis_count,
					nsch_map[i][p.second].tourettes_prevalence, nsch_map[i][p.second].tourettes_count
			);
			//print data
			for(auto pr : nis_map)
			{
				printf(",%lf", pr.second[i][p.second]);
			}
			printf("\n");
		}
	}
}

int main(int argc, char* argv[])
{
	if(argc < 2)
		printf("usage: nsch_nis <nsch_file> <nis_file>\n");

	parseNsch(argv[1]);
	parseNis(argv[2]);

	extractNsch();

	for(int i = 2011; i < 2018; i++)
	{
		for(auto p : state_code_map)
		{
			computePrevalence(i, p.second);
		}
	}

	extractNIS();
	constructLabels();
	outputData();


	return 0;
}

