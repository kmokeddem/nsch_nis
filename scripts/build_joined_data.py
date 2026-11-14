#!/usr/bin/env python3
"""Join NSCH outcomes, CDC NIS vaccination coverage, and a USGS glyphosate
exposure index by US state (FIPS) and birth year.

Output is shaped like data/joined_data2023.csv (one row per birth year x state,
prevalence/count columns, then one column per NIS vaccine.dose.age label), with
extra outcome columns (blood, obesity) from the 2025 preprint and glyphosate
columns appended. If data/nis_aluminum.csv exists (scripts/reconstruct_aluminum.py),
its per-window vaccine-aluminum columns (al_*, alcum_*, al_n) are appended last.

Run scripts/download_data.sh first, then:
    python3 scripts/build_joined_data.py                       # official CDC NIS CSV
    python3 scripts/build_joined_data.py --nis reconstructed   # NIS rebuilt from public-use
                                                               # files (scripts/reconstruct_nis.py)
"""
import argparse
import glob
import os
import re
import sys

import numpy as np
import pandas as pd
import pyreadstat

ROOT = os.path.abspath(os.path.join(os.path.dirname(__file__), ".."))
RAW = os.path.join(ROOT, "data", "raw")
ap = argparse.ArgumentParser()
ap.add_argument("--nis", choices=["official", "reconstructed"], default="official")
ap.add_argument("out", nargs="?")
ARGS = ap.parse_args()
NIS_CSV = {"official": os.path.join(RAW, "nis", "nis_fhky-rtsk.csv"),
           "reconstructed": os.path.join(ROOT, "data", "nis_reconstructed.csv")}[ARGS.nis]
OUT = ARGS.out or os.path.join(ROOT, "data", "joined_data2025.csv" if ARGS.nis == "official"
                               else "joined_data2025_nis_reconstructed.csv")

# NSCH survey years to pool. 2016-2018 public files have no BIRTH_YR, and
# (survey year - age) matches the true birth year for only ~58% of children,
# so those years are excluded.
NSCH_YEARS = range(2019, 2026)

# 50 states + DC (same set as src/fips.h)
STATES = {
    "Alabama": 1, "Alaska": 2, "Arizona": 4, "Arkansas": 5, "California": 6,
    "Colorado": 8, "Connecticut": 9, "Delaware": 10, "District of Columbia": 11,
    "Florida": 12, "Georgia": 13, "Hawaii": 15, "Idaho": 16, "Illinois": 17,
    "Indiana": 18, "Iowa": 19, "Kansas": 20, "Kentucky": 21, "Louisiana": 22,
    "Maine": 23, "Maryland": 24, "Massachusetts": 25, "Michigan": 26,
    "Minnesota": 27, "Mississippi": 28, "Missouri": 29, "Montana": 30,
    "Nebraska": 31, "Nevada": 32, "New Hampshire": 33, "New Jersey": 34,
    "New Mexico": 35, "New York": 36, "North Carolina": 37, "North Dakota": 38,
    "Ohio": 39, "Oklahoma": 40, "Oregon": 41, "Pennsylvania": 42,
    "Rhode Island": 44, "South Carolina": 45, "South Dakota": 46,
    "Tennessee": 47, "Texas": 48, "Utah": 49, "Vermont": 50, "Virginia": 51,
    "Washington": 53, "West Virginia": 54, "Wisconsin": 55, "Wyoming": 56,
}

# outcome name -> NSCH variable (1 = yes, 2 = no; Stata missing codes -> NaN).
# Paper outcomes: autism, allergies, ADHD, asthma, blood (negative control),
# epilepsy, obesity (doctor-identified overweight), Tourette's. Diabetes kept
# from joined_data2023.csv.
OUTCOMES = {
    "asd": "k2q35a",
    "allergies": "allergies",
    "adhd": "k2q31a",
    "asthma": "k2q40a",
    "epilepsy": "k2q42a",
    "diabetes": "diabetes",   # k2q41a before 2022
    "blood": "blood",
    "obesity": "overweight",
    "tourettes": "k2q38a",
}


# --------------------------------------------------------------------------- NSCH
def load_nsch():
    frames = []
    for y in NSCH_YEARS:
        path = glob.glob(os.path.join(RAW, "nsch", "extracted", f"nsch_{y}*_topical.dta"))[0]
        _, meta = pyreadstat.read_dta(path, metadataonly=True)
        cols = set(meta.column_names)
        diab = "diabetes" if "diabetes" in cols else "k2q41a"
        want = ["fipsst", "birth_yr", "sc_age_years", "k2q35a_1_years"] + \
               [v for v in OUTCOMES.values() if v != "diabetes"] + [diab]
        if "momage" in cols:
            want.append("momage")
        d, _ = pyreadstat.read_dta(path, usecols=want)
        d = d.rename(columns={diab: "diabetes"})
        if "momage" not in d:
            d["momage"] = np.nan
        d["survey_year"] = y
        frames.append(d)
        print(f"NSCH {y}: {len(d)} children", file=sys.stderr)
    d = pd.concat(frames, ignore_index=True)
    d["fipsst"] = pd.to_numeric(d["fipsst"]).astype(int)
    return d.dropna(subset=["birth_yr"])


def nsch_prevalence(d):
    d = d.copy()
    for name, var in OUTCOMES.items():
        v = d[var]
        d[f"{name}_valid"] = v.isin([1, 2])
        d[f"{name}_yes"] = v == 1
    # autism first diagnosed before age 3 / before age 4 (share of all valid asd responses)
    d["asd3_yes"] = d["asd_yes"] & (d["k2q35a_1_years"] < 3)
    d["asd4_yes"] = d["asd_yes"] & (d["k2q35a_1_years"] < 4)

    g = d.groupby(["birth_yr", "fipsst"])
    out = pd.DataFrame(index=g.size().index)
    for name in OUTCOMES:
        n = g[f"{name}_valid"].sum()
        out[f"{name}_prev"] = g[f"{name}_yes"].sum() / n
        out[f"{name}_count"] = n
        if name == "asd":
            out["asd_prev3"] = g["asd3_yes"].sum() / n
            out["asd_prev4"] = g["asd4_yes"].sum() / n
    # mother's age at child's birth (MOMAGE, only in 2019-2022 public files)
    out["mom_age"] = g["momage"].mean()
    out["mom_total"] = g["momage"].count()
    # mean child age at survey: "ever diagnosed" prevalence rises with age, and
    # pooling surveys means each birth cohort is observed at different ages.
    out["sc_age_mean"] = g["sc_age_years"].mean()
    out["nsch_n"] = g.size()
    out.index.names = ["year", "region"]
    return out


# --------------------------------------------------------------------------- NIS
def strip_label(s):
    """Same normalisation as stripString() in src/nsch_nis.cpp."""
    if pd.isna(s):
        return ""
    s = "".join(ch for ch in str(s) if ord(ch) < 128)
    return re.sub(r"[ ,()]", "", s)


def load_nis():
    d = pd.read_csv(NIS_CSV, dtype=str)
    d = d[(d["Geography Type"] == "States/Local Areas")
          & (d["Dimension Type"] == "Age")
          & d["Geography"].isin(STATES)
          & d["Birth Year/Birth Cohort"].str.fullmatch(r"\d{4}")
          & d["Dimension"].str.contains("Months")]
    d = d.assign(
        year=d["Birth Year/Birth Cohort"].astype(int),
        region=d["Geography"].map(STATES),
        months=d["Dimension"].str.extract(r"^(\d+)")[0].astype(int),
        estimate=pd.to_numeric(d["Estimate (%)"], errors="coerce") / 100.0,
    )
    d["label"] = (d["Vaccine"].map(strip_label) + "." + d["Dose"].map(strip_label)
                  + "." + d["months"].astype(str))
    w = d.pivot_table(index=["year", "region"], columns="label", values="estimate", aggfunc="first")
    return w[sorted(w.columns)]


AL_CSV = os.path.join(ROOT, "data", "nis_aluminum.csv")


def load_aluminum():
    """Mean vaccine aluminum (ug) by age window, from NIS public-use files."""
    if not os.path.exists(AL_CSV):
        return None
    a = pd.read_csv(AL_CSV)
    a = a[a["region"].isin(STATES)].assign(region=lambda x: x["region"].map(STATES))
    return a.set_index(["year", "region"])


# --------------------------------------------------------------------------- glyphosate
# County FIPS changes 2000-2019: Shannon Co. SD -> Oglala Lakota (2015);
# Bedford city VA merged into Bedford Co. (2013).
FIPS_REMAP = {"46113": "46102", "51515": "51019", "51560": "51005",  # + Clifton Forge -> Alleghany (2001)
              "12025": "12086"}                                     # Dade -> Miami-Dade (old code in USGS files)


def load_glyphosate():
    files = sorted(glob.glob(os.path.join(RAW, "glyphosate", "EPest*.txt")))
    frames = []
    for f in files:
        d = pd.read_csv(f, sep="\t", dtype={"STATE_FIPS_CODE": str, "COUNTY_FIPS_CODE": str})
        d = d[d["COMPOUND"].str.upper() == "GLYPHOSATE"].copy()
        d["prelim"] = int("2018" in f or "2019" in f)
        frames.append(d)
    g = pd.concat(frames, ignore_index=True)
    g["geoid"] = g["STATE_FIPS_CODE"].str.zfill(2) + g["COUNTY_FIPS_CODE"].str.zfill(3)
    g["geoid"] = g["geoid"].replace(FIPS_REMAP)
    # EPest-low leaves a county blank when no surveyed use was reported -> 0
    g["low"] = g["EPEST_LOW_KG"].fillna(0)
    g["high"] = g["EPEST_HIGH_KG"].fillna(0)
    return (g.groupby(["YEAR", "geoid"], as_index=False)
             .agg(low=("low", "sum"), high=("high", "sum"), prelim=("prelim", "max")))


def _county_file(fname, years):
    p = pd.read_csv(os.path.join(RAW, "population", fname),
                    encoding="latin-1", dtype={"STATE": str, "COUNTY": str})
    p = p[p["SUMLEV"] == 50].copy()
    p["geoid"] = (p["STATE"].str.zfill(2) + p["COUNTY"].str.zfill(3)).replace(FIPS_REMAP)
    pop = p.melt(id_vars="geoid", value_vars=[f"POPESTIMATE{y}" for y in years],
                 var_name="v", value_name="pop")
    pop["YEAR"] = pop["v"].str[-4:].astype(int)
    births = p.melt(id_vars="geoid", value_vars=[f"BIRTHS{y}" for y in years],
                    var_name="v", value_name="births")
    births["YEAR"] = births["v"].str[-4:].astype(int)
    c = pop.drop(columns="v").merge(births.drop(columns="v"), on=["geoid", "YEAR"])
    # merged counties (FIPS_REMAP) are summed into their successor
    return c.groupby(["geoid", "YEAR"], as_index=False)[["pop", "births"]].sum()


def load_counties():
    parts = [_county_file("co-est2020-alldata.csv", range(2010, 2020))]
    if os.path.exists(os.path.join(RAW, "population", "co-est2009-alldata.csv")):
        parts.append(_county_file("co-est2009-alldata.csv", range(2000, 2010)))
    c = pd.concat(parts, ignore_index=True)

    gaz = pd.read_csv(glob.glob(os.path.join(RAW, "population", "*Gaz_counties*.txt"))[0],
                      sep="\t", dtype={"GEOID": str})
    gaz.columns = gaz.columns.str.strip()
    gaz = gaz.rename(columns={"GEOID": "geoid"})
    gaz["land_km2"] = gaz["ALAND"] / 1e6
    c = c.merge(gaz[["geoid", "land_km2"]], on="geoid", how="left")
    # USGS EPest covers the conterminous US only: drop Alaska and Hawaii
    c = c[~c["geoid"].str[:2].isin(["02", "15"])]
    if c["land_km2"].isna().any():
        raise SystemExit(f"counties missing land area: {c.loc[c.land_km2.isna(), 'geoid'].unique()}")
    return c


def glyphosate_index():
    g = load_glyphosate()
    c = load_counties()
    unmatched = set(g["geoid"]) - set(c["geoid"])
    if unmatched:
        print(f"WARNING: glyphosate counties not in population file: {sorted(unmatched)}", file=sys.stderr)

    gyears = sorted(g["YEAR"].unique())
    c = c[c["YEAR"].isin(gyears)]
    # USGS covers the conterminous US only; counties absent from a year's file
    # in a covered state are treated as zero agricultural use.
    m = c.merge(g, on=["YEAR", "geoid"], how="left")
    m["state"] = m["geoid"].str[:2].astype(int)
    m = m[~m["state"].isin([2, 15])]
    m[["low", "high"]] = m[["low", "high"]].fillna(0)
    m["prelim"] = m.groupby("YEAR")["prelim"].transform("max")

    rows = []
    for (yr, st), s in m.groupby(["YEAR", "state"]):
        r = {"year": yr, "region": st, "glyph_prelim": int(s["prelim"].max())}
        for est in ("low", "high"):
            dens = s[est] / s["land_km2"]                     # kg per km2 in each county
            r[f"glyph_kg_{est}"] = s[est].sum()             # total state ag use (kg)
            r[f"glyph_kg_km2_{est}"] = s[est].sum() / s["land_km2"].sum()
            # population-weighted mean county application density (kg/km2):
            # the density of glyphosate use where the state's residents live
            r[f"glyph_popw_{est}"] = (dens * s["pop"]).sum() / s["pop"].sum()
            # same, weighted by county births that year (the birth cohort's exposure)
            r[f"glyph_birthw_{est}"] = (dens * s["births"]).sum() / s["births"].sum()
        r["glyph_kg_per_capita_high"] = s["high"].sum() / s["pop"].sum()
        r["state_pop"] = s["pop"].sum()
        rows.append(r)
    return pd.DataFrame(rows).set_index(["year", "region"])


# --------------------------------------------------------------------------- main
def main():
    nsch = nsch_prevalence(load_nsch())
    nis = load_nis()
    gly = glyphosate_index()

    years = sorted(set(nis.index.get_level_values("year")))
    idx = pd.MultiIndex.from_tuples(
        [(y, STATES[s]) for y in years for s in sorted(STATES)], names=["year", "region"])
    out = (pd.DataFrame(index=idx)
           .join(nsch, how="left")
           .join(nis, how="left")
           .join(gly, how="left"))
    al = load_aluminum()
    al_cols = []
    if al is not None:
        out = out.join(al, how="left")
        al_cols = list(al.columns)

    lead = ["asd_prev", "asd_prev3", "asd_prev4", "asd_count"]
    rest = [f"{n}_{s}" for n in OUTCOMES if n != "asd" for s in ("prev", "count")]
    nsch_cols = lead + rest + ["mom_age", "mom_total", "sc_age_mean", "nsch_n"]
    out = out[nsch_cols + list(nis.columns) + list(gly.columns) + al_cols]
    count_cols = [c for c in out.columns if c.endswith(("_count", "_total", "_n"))] + ["state_pop"]
    out[count_cols] = out[count_cols].astype("Int64")
    out.reset_index().to_csv(OUT, index=False, na_rep="NA", float_format="%.6f")
    print(f"wrote {OUT}: {out.shape[0]} rows x {out.shape[1] + 2} cols, birth years "
          f"{years[0]}-{years[-1]}", file=sys.stderr)


if __name__ == "__main__":
    main()
