#!/usr/bin/env python3
"""Estimate mean vaccine-aluminum dose (micrograms) received per child by age
window, for each US state x birth year, from the NIS-Child public-use files.

Each provider-reported dose of a DTaP-, HepB-, Hib-, PCV- or HepA-containing
vaccine has an age in days (D*k) and, except HepA, a type code (X*TYk). The
type code names the antigen combination, which identifies the product for
combination vaccines; a few Hib codes name the manufacturer (2009+ surveys).
Aluminum per dose is the package-insert content of the product. Where the
code does not pin down the product, a LOW and a HIGH bound are carried:

  DTaP family (DDTP/XDTPTY)
    08 DTaP-HepB-IPV  Pediarix (GSK)                 850
    D3 DTaP-IPV/Hib   Pentacel (Sanofi)              330
    D4 DTaP-IPV-Hib-HepB  Vaxelis                    319
    07 DTaP-Hib       TriHIBit (Tripedia + ActHIB)   170
    01 DT pediatric                                  170
    04 DTaP, brand not coded: Infanrix 625 / Daptacel 330 / Tripedia 170
         low = 170 for birth years <= 2010 (Tripedia still distributed), else 330
         high = 625
    anything else (DTP-unknown, blank, DK, other ...): low = smallest product
         (170 / 319), high = 850
  HepB family (DHEPB/XHEPTY): Engerix-B and Recombivax HB pediatric are both
    250, so single-antigen or unknown = 250. Combination codes (08, D4, DH, 43)
    = 0 here because their aluminum is counted once, in the DTaP or Hib family.
  Hib family (DHIB/XHIBTY)
    HM PedvaxHIB (Merck) 225; 43 Comvax HepB-Hib (Merck) 225
    HS ActHIB, HG Hiberix, H2 Sanofi-or-GSK, HY MenHibrix: 0
    07, D3, D4, 05 (combinations): 0 here (counted in the DTaP family)
    44 "Hib only" (all pre-2009 coding), HI unknown, blank, other: 0 / 225
  PCV family (DPCV/XPCVTY): PCV7/13/15/20 all 125; 71 polysaccharide 0.
  HepA (DHEPA, no type code): Vaqta 225 / Havrix 250.
  Polio, MMR, varicella, rotavirus, influenza: no aluminum.

Figures are the per-dose amounts stated in US package inserts ("not more
than" values for Infanrix/Pediarix are used as given).

Age windows match the paper's All.m windows: doses given before 3 months
("0m-2m"), [3,5), [5,7), [7,13), [13,19), [19,24) and [24,35) months, using
the same "before m months" cutoff as scripts/reconstruct_nis.py. A child
contributes to a window only if observed through its end: 19-23 month olds to
19 months, 24-29 to 24, 30-35 to 35 (so the last window is slightly
understated). Birth year is assigned probabilistically exactly as in
reconstruct_nis.py.

Outputs
  data/nis_aluminum.csv              state x birth year: al_{low,high}_{m}
                                     (mean ug in the window ending at m months)
                                     and alcum_{low,high}_{m} (cumulative ug)
  data/nis_aluminum_product_mix.csv  state x birth year shares of DTaP doses by
                                     product and of Hib doses that are Merck

Usage: python3 scripts/reconstruct_aluminum.py [data/raw/nis_puf] [out.csv]
"""
import os
import re
import sys

import numpy as np
import pandas as pd

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
_argv, sys.argv = sys.argv, sys.argv[:1]          # reconstruct_nis reads argv at import
import reconstruct_nis as rn                      # noqa: E402
sys.argv = _argv

ROOT = rn.ROOT
PUF = sys.argv[1] if len(sys.argv) > 1 else os.path.join(ROOT, "data", "raw", "nis_puf")
OUT = sys.argv[2] if len(sys.argv) > 2 else os.path.join(ROOT, "data", "nis_aluminum.csv")
MIX_OUT = re.sub(r"\.csv$", "", OUT) + "_product_mix.csv"
rn.PUF = PUF

WINDOWS = [3, 5, 7, 13, 19, 24, 35]
FAMILIES = {"DTP": ("DDTP", "XDTPTY"), "HEPB": ("DHEPB", "XHEPTY"),
            "HIB": ("DHIB", "XHIBTY"), "PCV": ("DPCV", "XPCVTY"), "HEPA": ("DHEPA", None)}
TRIPEDIA_LAST_BIRTH_YEAR = 2010


def dose_al(fam, code, birth_year):
    """(low, high) micrograms of aluminum for one dose; code is a str array."""
    n = len(code)
    lo, hi = np.zeros(n), np.zeros(n)
    if fam == "DTP":
        early = birth_year <= TRIPEDIA_LAST_BIRTH_YEAR
        lo[:], hi[:] = np.where(early, 170, 319), 850             # unknown product
        fixed = {"08": 850, "D3": 330, "D4": 319, "07": 170, "01": 170}
        for c, v in fixed.items():
            lo[code == c], hi[code == c] = v, v
        plain = code == "04"
        lo[plain] = np.where(early[plain], 170, 330)
        hi[plain] = 625
    elif fam == "HEPB":
        lo[:], hi[:] = 250, 250
        combo = np.isin(code, ["08", "D4", "DH", "43"])
        lo[combo], hi[combo] = 0, 0
    elif fam == "HIB":
        lo[:], hi[:] = 0, 225                                      # unknown brand
        merck = np.isin(code, ["HM", "43"])
        lo[merck], hi[merck] = 225, 225
        none = np.isin(code, ["HS", "HG", "H2", "HY", "07", "D3", "D4", "05"])
        lo[none], hi[none] = 0, 0
    elif fam == "PCV":
        lo[:], hi[:] = 125, 125
        lo[code == "71"], hi[code == "71"] = 0, 0
    elif fam == "HEPA":
        lo[:], hi[:] = 225, 250
    return lo, hi


def norm_code(s):
    s = s.fillna("").astype(str).str.strip().str.upper()
    return s.where(~s.str.fullmatch(r"\d"), s.str.zfill(2)).to_numpy()   # 2003-04 "4" -> "04"


def read_year(y):
    y2 = f"{y % 100:02d}"
    lay = rn.layout_for(y2)
    wvar = next(w for w in ("PROVWT_C", "PROVWT_D", "PROVWT", "WGT") if w in lay)
    cols = {"WEIGHT": wvar, "AGEGRP": "AGEGRP", "STATE": "STATE", "PDAT": "PDAT"}
    for age_pre, ty_pre in FAMILIES.values():
        for k in range(1, 10):
            if f"{age_pre}{k}" in lay:
                cols[f"{age_pre}{k}"] = f"{age_pre}{k}"
            if ty_pre and f"{ty_pre}{k}" in lay:
                cols[f"{ty_pre}{k}"] = f"{ty_pre}{k}"
    if "XDTPTY1" not in cols:
        raise KeyError(f"{y}: layout has no vaccine type codes")
    d = pd.read_fwf(rn.data_file(y2), colspecs=[lay[v] for v in cols.values()],
                    names=list(cols), header=None, dtype=str, encoding="latin-1")
    num = [c for c in d.columns if not c.startswith("X")]
    d[num] = d[num].apply(lambda s: pd.to_numeric(s.str.strip(), errors="coerce"))
    d = d[(d["PDAT"] == 1) & (d["WEIGHT"] > 0)]
    d = d[d["STATE"].isin(rn.STATE_NAMES) & d["AGEGRP"].isin([1, 2, 3])].copy()
    d["SURVEY_YEAR"] = y
    return d


def child_aluminum(ex):
    """Per expanded child row: ug received in each window, low and high; plus
    product-mix counts for the diagnostics file."""
    cuts = [-np.inf] + [rn.by_age_days(m) for m in WINDOWS]
    n = len(ex)
    by = ex["BIRTH_YEAR"].to_numpy()
    al = {b: np.zeros((n, len(WINDOWS))) for b in ("low", "high")}
    mix = {k: np.zeros(n) for k in ("dtp_n", "dtp_pediarix", "dtp_pentacel", "dtp_vaxelis",
                                   "dtp_plain", "dtp_other", "hib_n", "hib_merck", "hib_nonmerck")}
    for fam, (age_pre, ty_pre) in FAMILIES.items():
        for k in range(1, 10):
            ac = f"{age_pre}{k}"
            if ac not in ex:
                continue
            t = ex[ac].to_numpy(dtype=float)
            code = norm_code(ex[f"{ty_pre}{k}"]) if ty_pre and f"{ty_pre}{k}" in ex else np.full(n, "")
            lo, hi = dose_al(fam, code, by)
            given = ~np.isnan(t)
            for j in range(len(WINDOWS)):
                inw = given & (t >= cuts[j]) & (t < cuts[j + 1])
                al["low"][inw, j] += lo[inw]
                al["high"][inw, j] += hi[inw]
            early = given & (t < rn.by_age_days(13))        # product mix of infant doses
            if fam == "DTP":
                mix["dtp_n"] += early
                mix["dtp_pediarix"] += early & (code == "08")
                mix["dtp_pentacel"] += early & (code == "D3")
                mix["dtp_vaxelis"] += early & (code == "D4")
                mix["dtp_plain"] += early & (code == "04")
                mix["dtp_other"] += early & ~np.isin(code, ["08", "D3", "D4", "04"])
            elif fam == "HIB":
                mix["hib_n"] += early
                mix["hib_merck"] += early & np.isin(code, ["HM", "43"])
                mix["hib_nonmerck"] += early & np.isin(code, ["HS", "HG", "H2", "HY", "07", "D3", "D4"])
    return al, mix


def main():
    years = sorted({int("20" + m.group(1)) for f in os.listdir(PUF)
                    if (m := re.fullmatch(r"nispuf(\d\d)\.(dat|txt)", f, re.I))})
    kids = pd.concat([read_year(y) for y in years], ignore_index=True)
    print(f"read {len(kids)} children, surveys {years[0]}-{years[-1]}", file=sys.stderr)

    parts = []
    for g, probs in rn.AGE_GROUP_PROBS.items():
        for off, p in probs.items():
            x = kids[kids["AGEGRP"] == g].copy()
            x["BIRTH_YEAR"] = x["SURVEY_YEAR"] + off
            x["W"] = x["WEIGHT"] * p
            parts.append(x)
    ex = pd.concat(parts, ignore_index=True)
    full = [b for b in sorted(ex["BIRTH_YEAR"].unique()) if all(b + k in years for k in (1, 2, 3))]
    ex = ex[ex["BIRTH_YEAR"].isin(full)].reset_index(drop=True)

    al, mix = child_aluminum(ex)
    cens = ex["AGEGRP"].map(rn.CENSOR).to_numpy()
    w = ex["W"].to_numpy()
    obs = np.column_stack([cens >= rn.by_age_days(m) for m in WINDOWS])   # observed through window end

    key = ex[["STATE", "BIRTH_YEAR"]]
    cols = {}
    for b in ("low", "high"):
        cum = np.cumsum(al[b], axis=1)
        for j, m in enumerate(WINDOWS):
            ww = w * obs[:, j]
            cols[f"al_{b}_{m}"] = (al[b][:, j] * ww, ww)
            cols[f"alcum_{b}_{m}"] = (cum[:, j] * ww, ww)
    num = pd.DataFrame({k: v[0] for k, v in cols.items()})
    den = pd.DataFrame({k: v[1] for k, v in cols.items()})
    g_num = num.groupby([key["STATE"], key["BIRTH_YEAR"]]).sum()
    g_den = den.groupby([key["STATE"], key["BIRTH_YEAR"]]).sum()
    out = g_num / g_den.replace(0, np.nan)
    out["al_n"] = key.groupby(["STATE", "BIRTH_YEAR"]).size()
    out = out.reset_index()
    out.insert(0, "region", out.pop("STATE").map(lambda s: rn.STATE_NAMES[int(s)]))
    out = out.rename(columns={"BIRTH_YEAR": "year"})
    out = out[["year", "region", "al_n"] + list(cols)].sort_values(["year", "region"])
    out.to_csv(OUT, index=False, float_format="%.2f")

    m = pd.DataFrame({k: v * w for k, v in mix.items()})
    gm = m.groupby([key["STATE"], key["BIRTH_YEAR"]]).sum()
    mx = pd.DataFrame({
        "dtp_share_pediarix": gm["dtp_pediarix"] / gm["dtp_n"],
        "dtp_share_pentacel": gm["dtp_pentacel"] / gm["dtp_n"],
        "dtp_share_vaxelis": gm["dtp_vaxelis"] / gm["dtp_n"],
        "dtp_share_plain_unbranded": gm["dtp_plain"] / gm["dtp_n"],
        "dtp_share_other_unknown": gm["dtp_other"] / gm["dtp_n"],
        "hib_share_merck": gm["hib_merck"] / gm["hib_n"],
        "hib_share_nonmerck": gm["hib_nonmerck"] / gm["hib_n"],
    }).reset_index()
    mx.insert(0, "region", mx.pop("STATE").map(lambda s: rn.STATE_NAMES[int(s)]))
    mx = mx.rename(columns={"BIRTH_YEAR": "year"}).sort_values(["year", "region"])
    mx.to_csv(MIX_OUT, index=False, float_format="%.4f")
    print(f"wrote {OUT} ({len(out)} rows, birth years {full[0]}-{full[-1]}) and {MIX_OUT}",
          file=sys.stderr)


if __name__ == "__main__":
    main()
