#!/usr/bin/env python3
"""Reconstruct CDC NIS-Child state x birth-year vaccination coverage from the
annual NIS-Child public-use files (PUFs), in the same long format as the
data.cdc.gov "Vaccination Coverage among Young Children (0-35 Months)" CSV.

APPROXIMATION -- read before use
--------------------------------
CDC's official birth-cohort estimates use exact birth dates, which are only
available in the restricted files (Research Data Center). The PUFs give only
the survey year Y and an age group (19-23 / 24-29 / 30-35 months). Assuming
interviews are spread evenly over the year and ages evenly within each group,
a child's birth year is:
    19-23 mo: Y-2 with p=.792, Y-1 with p=.208
    24-29 mo: Y-2 with p=.75,  Y-3 with p=.25
    30-35 mo: Y-3 with p=.75,  Y-2 with p=.25
Each child contributes to both possible birth cohorts with weight
(provider-phase survey weight x probability). About a quarter of each cohort's
weight therefore comes from a neighbouring birth year.

Coverage "by m months" is a weighted Kaplan-Meier estimate of the age in days
at the k-th provider-reported dose. A child is observed up to the lower bound
of its age group (19 or 24 months); 30-35 month olds are treated as observed to
35 months (doses after the interview are not in the file, so 35-month coverage
is slightly understated).

Only simple "k+ doses by age m" measures are reconstructed (DTaP, HepB, Hib,
PCV, Polio, MMR, Varicella, HepA). Brand-dependent measures (Hib primary/full
series, rotavirus, combined 7-vaccine series) and influenza are not.

Usage: python3 scripts/reconstruct_nis.py [data/raw/nis_puf] [out.csv]
"""
import glob
import os
import re
import sys

import numpy as np
import pandas as pd

ROOT = os.path.abspath(os.path.join(os.path.dirname(__file__), ".."))
PUF = sys.argv[1] if len(sys.argv) > 1 else os.path.join(ROOT, "data", "raw", "nis_puf")
OUT = sys.argv[2] if len(sys.argv) > 2 else os.path.join(ROOT, "data", "nis_reconstructed.csv")

STATE_NAMES = {
    1: "Alabama", 2: "Alaska", 4: "Arizona", 5: "Arkansas", 6: "California", 8: "Colorado",
    9: "Connecticut", 10: "Delaware", 11: "District of Columbia", 12: "Florida", 13: "Georgia",
    15: "Hawaii", 16: "Idaho", 17: "Illinois", 18: "Indiana", 19: "Iowa", 20: "Kansas",
    21: "Kentucky", 22: "Louisiana", 23: "Maine", 24: "Maryland", 25: "Massachusetts",
    26: "Michigan", 27: "Minnesota", 28: "Mississippi", 29: "Missouri", 30: "Montana",
    31: "Nebraska", 32: "Nevada", 33: "New Hampshire", 34: "New Jersey", 35: "New Mexico",
    36: "New York", 37: "North Carolina", 38: "North Dakota", 39: "Ohio", 40: "Oklahoma",
    41: "Oregon", 42: "Pennsylvania", 44: "Rhode Island", 45: "South Carolina",
    46: "South Dakota", 47: "Tennessee", 48: "Texas", 49: "Utah", 50: "Vermont",
    51: "Virginia", 53: "Washington", 54: "West Virginia", 55: "Wisconsin", 56: "Wyoming",
}

# (CDC vaccine name, PUF dose-age prefix, CDC dose label template)
VACCINES = [
    ("DTaP", "DDTP"), ("Hep B", "DHEPB"), ("Hib", "DHIB"), ("PCV", "DPCV"),
    ("Polio", "DPOLIO"), ("MMR", "DMMR"), ("Varicella", "DVRC"), ("Hep A", "DHEPA"),
]
# measures published by CDC: vaccine -> {doses: [ages in months]}
MEASURES = {
    "DTaP":      {1: [3], 2: [5], 3: [7, 13, 19, 24, 35], 4: [19, 24, 35]},
    "Hep B":     {1: [3], 2: [5, 7, 13], 3: [7, 13, 19, 24, 35]},
    "Hib":       {1: [3], 2: [5, 7, 13], 3: [7, 13, 19, 24, 35]},
    "PCV":       {1: [3], 2: [5], 3: [7, 13, 19, 24, 35], 4: [19, 24, 35]},
    "Polio":     {1: [3], 2: [5, 7, 13], 3: [19, 24, 35]},
    "MMR":       {1: [13, 19, 24, 35]},
    "Varicella": {1: [13, 19, 24, 35]},
    "Hep A":     {1: [19, 24, 35], 2: [19, 24, 35]},
}
AGE_GROUP_PROBS = {1: {-2: .7917, -1: .2083}, 2: {-2: .75, -3: .25}, 3: {-3: .75, -2: .25}}
DAYS_PER_MONTH = 365.25 / 12
# observation (censoring) age in days by age group
CENSOR = {1: 19 * DAYS_PER_MONTH, 2: 24 * DAYS_PER_MONTH, 3: 35 * DAYS_PER_MONTH}


CUTOFF_SHIFT = float(os.environ.get("NIS_CUTOFF_SHIFT", 0))
# MMR / varicella doses only count if given on/after the 1st birthday
# (ACIP 4-day grace period -> 361 days)
MIN_VALID_AGE = {"MMR": 361, "Varicella": 361}


def by_age_days(m):
    """'by m months' == dose given before the child turns m months old."""
    return m * DAYS_PER_MONTH + CUTOFF_SHIFT


def dose_age(s, pre, k, vname):
    """Age in days at the k-th countable dose (NaN = never)."""
    floor = MIN_VALID_AGE.get(vname)
    if floor is None:
        return s[f"{pre}{k}"].to_numpy(dtype=float)
    a = np.column_stack([s[c].to_numpy(dtype=float) for c in s.columns if re.fullmatch(rf"{pre}\d", c)])
    a = np.where(a >= floor, a, np.nan)
    a = np.sort(a, axis=1)              # NaN sorts last
    return a[:, k - 1]


# --------------------------------------------------------------------------- layout parsing
def parse_sas_layout(path):
    """Column positions from a SAS INPUT statement. Handles '@pos NAME $w.d'
    and 'NAME $ start - end' styles. Returns {name: (start0, end0)}."""
    txt = open(path, encoding="latin-1").read()
    m = re.search(r"^\s*input\b(.*?);", txt, re.I | re.S | re.M)
    body = m.group(1)
    cols = {}
    for pos, name, width in re.findall(r"@\s*(\d+)\s+([A-Za-z_][A-Za-z0-9_]*)\s+\$?\s*(\d+)\.", body):
        cols[name.upper()] = (int(pos) - 1, int(pos) - 1 + int(width))
    for name, a, b in re.findall(r"([A-Za-z_][A-Za-z0-9_]*)\s+\$?\s*(\d+)\s*-\s*(\d+)", body):
        cols.setdefault(name.upper(), (int(a) - 1, int(b)))
    return cols


def parse_r_layout(path):
    """Column positions from CDC's NISPUFyy.R reader, which lists
    LIST.NAMEWIDTH <- c("NAME", width, "NAME", width, ...) for read.fwf."""
    txt = open(path, encoding="latin-1").read()
    m = re.search(r"LIST\.NAMEWIDTH\s*<-\s*c\((.*?)\)\s*\n", txt, re.S)
    if not m:
        raise ValueError(f"cannot parse layout {path}")
    cols, pos = {}, 0
    for name, w in re.findall(r'"([^"]+)"\s*,\s*(\d+)', m.group(1)):
        cols[name.upper()] = (pos, pos + int(w))
        pos += int(w)
    return cols


LAYOUT_DIR = os.path.join(os.path.dirname(os.path.abspath(__file__)), "nis_layouts")


def layout_for(year2):
    # 2015: CDC publishes no reader; layout transcribed from the codebook
    for p in (os.path.join(PUF, f"nispuf{year2}.sas"), os.path.join(PUF, f"nispuf{year2}.SAS"),
              os.path.join(LAYOUT_DIR, f"nispuf{year2}.sas")):
        if os.path.exists(p):
            return parse_sas_layout(p)
    return parse_r_layout(os.path.join(PUF, f"nispuf{year2}.r"))


def data_file(year2):
    c = [p for p in glob.glob(os.path.join(PUF, "*"))
         if re.search(rf"nispuf{year2}\.?(dat|txt)?$", os.path.basename(p), re.I)
         and not p.lower().endswith((".sas", ".r"))]
    if not c:
        raise FileNotFoundError(f"no data file for 20{year2}")
    return c[0]


# --------------------------------------------------------------------------- reading
def read_puf(survey_year):
    y2 = f"{survey_year % 100:02d}"
    lay = layout_for(y2)
    wvar = next(w for w in ("PROVWT_C", "PROVWT_D", "PROVWT", "WGT") if w in lay)
    want = {"WEIGHT": wvar, "AGEGRP": "AGEGRP", "STATE": "STATE", "PDAT": "PDAT"}
    for _, pre in VACCINES:
        for k in range(1, 5):
            if f"{pre}{k}" in lay:
                want[f"{pre}{k}"] = f"{pre}{k}"
    missing = [v for v in ("AGEGRP", "STATE", "PDAT") if v not in lay]
    if missing:
        raise KeyError(f"{survey_year}: layout lacks {missing}")
    spans = [lay[v] for v in want.values()]
    d = pd.read_fwf(data_file(y2), colspecs=spans, names=list(want), header=None,
                    dtype=str, encoding="latin-1")
    d = d.apply(lambda s: pd.to_numeric(s.str.strip(), errors="coerce"))
    d = d[(d["PDAT"] == 1) & (d["WEIGHT"] > 0)]
    d["SURVEY_YEAR"] = survey_year
    return d, wvar


# --------------------------------------------------------------------------- estimation
def km_coverage(t, cens, w, cutoff):
    """Weighted KM P(dose by `cutoff` days) with censoring at CENSOR[grp].
    Censoring only happens at three fixed ages, so KM reduces to a product of
    conditional survivals between those ages."""
    t = np.where(np.isnan(t), np.inf, t)   # no recorded dose = not vaccinated
    surv, lo = 1.0, 0.0
    for c_age in sorted(set(CENSOR.values())):
        hi = min(cutoff, c_age)
        risk = (cens >= hi) & (t >= lo)
        # at risk over (lo, hi]: not yet vaccinated at lo and observed through hi
        denom = w[risk].sum()
        if denom <= 0:
            return np.nan
        surv *= w[risk & (t >= hi)].sum() / denom
        if cutoff <= c_age:
            break
        lo = c_age
    return 1 - surv


def main():
    years = sorted(int("20" + m.group(1)) for f in os.listdir(PUF)
                   if (m := re.fullmatch(r"nispuf(\d\d)\.(dat|txt)", f, re.I)))
    years = sorted(set(years))
    frames = []
    for y in years:
        d, wvar = read_puf(y)
        frames.append(d)
        print(f"PUF {y}: {len(d)} children with provider data (weight {wvar})", file=sys.stderr)
    kids = pd.concat(frames, ignore_index=True)
    kids = kids[kids["STATE"].isin(STATE_NAMES) & kids["AGEGRP"].isin([1, 2, 3])]

    # expand each child to its possible birth years
    parts = []
    for g, probs in AGE_GROUP_PROBS.items():
        for off, p in probs.items():
            x = kids[kids["AGEGRP"] == g].copy()
            x["BIRTH_YEAR"] = x["SURVEY_YEAR"] + off
            x["W"] = x["WEIGHT"] * p
            parts.append(x)
    ex = pd.concat(parts, ignore_index=True)
    # a birth cohort needs every survey year that can contain it: B+1 .. B+3
    have = set(years)
    full = [b for b in sorted(ex["BIRTH_YEAR"].unique()) if all(b + k in have for k in (1, 2, 3))]
    ex = ex[ex["BIRTH_YEAR"].isin(full)]
    print(f"complete birth cohorts: {full[0]}-{full[-1]}", file=sys.stderr)

    rows = []
    for (b, st), s in ex.groupby(["BIRTH_YEAR", "STATE"]):
        cens = s["AGEGRP"].map(CENSOR).to_numpy(); w = s["W"].to_numpy()
        n = len(s)
        for vname, pre in VACCINES:
            for k, ages in MEASURES[vname].items():
                col = f"{pre}{k}"
                if col not in s.columns:
                    continue
                t = dose_age(s, pre, k, vname)
                for m in ages:
                    est = km_coverage(t, cens, w, by_age_days(m))
                    if vname in ("MMR", "Varicella"):
                        vac, dose = f"≥1 Dose {vname}", ""
                    else:
                        vac, dose = vname, f"≥{k} Dose" + ("s" if k > 1 else "")
                    rows.append({"Vaccine": vac, "Dose": dose,
                                 "Geography Type": "States/Local Areas",
                                 "Geography": STATE_NAMES[int(st)],
                                 "Birth Year/Birth Cohort": str(int(b)),
                                 "Dimension Type": "Age", "Dimension": f"{m} Months",
                                 "Estimate (%)": round(100 * est, 1) if not np.isnan(est) else np.nan,
                                 "95% CI (%)": "", "Sample Size": n})
    out = pd.DataFrame(rows)
    out.to_csv(OUT, index=False)
    print(f"wrote {OUT}: {len(out)} rows, birth years {full[0]}-{full[-1]}", file=sys.stderr)


if __name__ == "__main__":
    main()
