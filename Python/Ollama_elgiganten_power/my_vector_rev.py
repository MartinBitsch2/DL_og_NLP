# -*- coding: utf-8 -*-
"""
Created on Thu May 21 12:58:19 2026

@author: mikk1
"""

# -*- coding: utf-8 -*-

import re
import os
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import gender_guesser.detector as gender
import geonamescache
from sentida import Sentida


# --------------------------------------------------
# Indstillinger
# --------------------------------------------------

BASE_PATH = "C:/Users/mikk1/Documents/Deep Learning/Python/Spyder"

ELGIGANTEN_PATH = os.path.join(BASE_PATH, "elgiganten.parquet")
POWER_PATH = os.path.join(BASE_PATH, "power.parquet")

OUTPUT_PATH = os.path.join(BASE_PATH, "electronics_reviews_prepared.parquet")
PLOT_FOLDER = os.path.join(BASE_PATH, "plots_electronics")

os.makedirs(PLOT_FOLDER, exist_ok=True)


# --------------------------------------------------
# Indlæs data
# --------------------------------------------------

df_elgiganten = pd.read_parquet(ELGIGANTEN_PATH)
df_power = pd.read_parquet(POWER_PATH)

df_elgiganten["company_source"] = "Elgiganten"
df_power["company_source"] = "Power"

df = pd.concat(
    [df_elgiganten, df_power],
    ignore_index=True
)

print("Antal reviews i alt:", len(df))
print("Antal Elgiganten-reviews:", len(df_elgiganten))
print("Antal Power-reviews:", len(df_power))

print("\nKolonner i original df:")
print(df.columns.tolist())


# --------------------------------------------------
# Tjek nødvendige kolonner
# --------------------------------------------------

required_columns = ["name", "content"]

missing_columns = [col for col in required_columns if col not in df.columns]

if missing_columns:
    raise ValueError(f"Mangler nødvendige kolonner: {missing_columns}")


# --------------------------------------------------
# Rens tekst
# --------------------------------------------------

def clean_text(text):
    if pd.isna(text):
        return ""

    text = str(text)
    text = re.sub(r"\s+", " ", text)
    return text.strip()


df["content_clean"] = df["content"].apply(clean_text)


# --------------------------------------------------
# Sentida sentiment
# --------------------------------------------------

sentida = Sentida()


def get_sentida_score(text):
    if pd.isna(text):
        return np.nan

    text = str(text).strip()

    if text == "":
        return np.nan

    try:
        score = sentida.sentida(
            text,
            output="mean",
            normal=False
        )
        return float(score)

    except Exception:
        return np.nan


def get_sentida_label(score):
    if pd.isna(score):
        return "Ved ikke"

    elif score < -0.05:
        return "Negativ"

    elif score > 0.05:
        return "Positiv"

    else:
        return "Neutral"


print("\nBeregner Sentida-score...")

df["sentida_score"] = df["content_clean"].apply(get_sentida_score)
df["sentida_label"] = df["sentida_score"].apply(get_sentida_label)

print("\nFordeling af Sentida-labels:")
print(df["sentida_label"].value_counts(dropna=False))


# --------------------------------------------------
# Infereret køn
# --------------------------------------------------

gender_detector = gender.Detector(case_sensitive=False)


def infer_gender(name):
    if pd.isna(name):
        return "Ved ikke"

    name = str(name).strip()

    if name == "":
        return "Ved ikke"

    if " og " in name.lower():
        return "Flere navne"

    first_name = name.split()[0]
    result = gender_detector.get_gender(first_name)

    if result in ["male", "mostly_male"]:
        return "Mand"
    elif result in ["female", "mostly_female"]:
        return "Kvinde"
    else:
        return "Ved ikke"


df["inferred_gender"] = df["name"].apply(infer_gender)


# --------------------------------------------------
# Infereret by
# --------------------------------------------------

gc = geonamescache.GeonamesCache()
cities = gc.get_cities()

danish_cities = set(
    city_info["name"].lower()
    for city_info in cities.values()
    if city_info.get("countrycode") == "DK"
)

city_aliases = {
    "nørrebro": "København",
    "noerrebro": "København",
    "norrebro": "København",
    "indre by": "København",
    "vesterbro": "København",
    "østerbro": "København",
    "oesterbro": "København",
    "osterbro": "København",
    "amager": "København",
    "amagerbro": "København",
    "valby": "København",
    "vanløse": "København",
    "vanloese": "København",
    "brønshøj": "København",
    "bronshoj": "København",
    "kbh": "København",
    "kbh k": "København",
    "kbh n": "København",
    "kbh ø": "København",
    "kbh o": "København",
    "kbh s": "København",
    "københavn k": "København",
    "københavn n": "København",
    "københavn ø": "København",
    "københavn o": "København",
    "københavn s": "København",

    "aarhus c": "Aarhus",
    "århus c": "Aarhus",
    "århus": "Aarhus",

    "odense c": "Odense",
    "odense m": "Odense",

    "aalborg c": "Aalborg",
    "ålborg": "Aalborg",
    "ålborg c": "Aalborg",
}

all_city_terms = sorted(
    set(danish_cities) | set(city_aliases.keys()),
    key=len,
    reverse=True
)


def normalize_text_for_city(text):
    text = str(text).lower()
    text = text.replace("ø", "oe")
    text = text.replace("å", "aa")
    text = text.replace("æ", "ae")
    return text


def infer_city_from_row(row):
    search_parts = []

    for col in [
        "content_clean",
        "title",
        "company",
        "store",
        "location",
        "department",
        "branch",
        "company_source"
    ]:
        if col in row.index:
            search_parts.append(row.get(col, ""))

    text_original = " ".join([str(x) for x in search_parts if pd.notna(x)])
    text_normalized = normalize_text_for_city(text_original)

    for city_term in all_city_terms:
        city_term_normalized = normalize_text_for_city(city_term)

        pattern = (
            r"(?<![a-zA-ZæøåÆØÅ])"
            + re.escape(city_term_normalized)
            + r"(?![a-zA-ZæøåÆØÅ])"
        )

        if re.search(pattern, text_normalized):
            if city_term in city_aliases:
                return city_aliases[city_term]

            return city_term.title()

    return "Ved ikke"


df["inferred_city"] = df.apply(infer_city_from_row, axis=1)


# --------------------------------------------------
# Markér usikre features
# --------------------------------------------------

df["has_inferred_gender"] = df["inferred_gender"] != "Ved ikke"
df["has_inferred_city"] = df["inferred_city"] != "Ved ikke"
df["has_sentida_score"] = df["sentida_label"] != "Ved ikke"


# --------------------------------------------------
# Find rating-kolonne automatisk
# --------------------------------------------------

possible_rating_columns = [
    "rating",
    "score",
    "stars",
    "vurdering",
    "bedømmelse",
    "review_rating"
]

rating_col = None

for col in possible_rating_columns:
    if col in df.columns:
        rating_col = col
        break

print("\nFundet rating-kolonne:", rating_col)


if rating_col is not None:
    df[rating_col] = pd.to_numeric(df[rating_col], errors="coerce")


# --------------------------------------------------
# Beskrivende statistik
# --------------------------------------------------

print("\n--- Beskrivende statistik ---")

total_reviews = len(df)

print("\nAntal observationer:")
print(total_reviews)

print("\nReviews pr. virksomhed:")
print(df["company_source"].value_counts(dropna=False))

print("\nAntal unikke navne:")
print(df["name"].nunique(dropna=True))

df["review_length_chars"] = df["content_clean"].str.len()
df["review_length_words"] = df["content_clean"].apply(lambda x: len(x.split()))

print("\nGennemsnitlig review-længde i tegn:")
print(round(df["review_length_chars"].mean(), 2))

print("\nGennemsnitlig review-længde i ord:")
print(round(df["review_length_words"].mean(), 2))

print("\nGennemsnitlig Sentida-score:")
print(round(df["sentida_score"].mean(), 4))

print("\nSentida-labels pr. virksomhed:")
print(
    pd.crosstab(
        df["company_source"],
        df["sentida_label"],
        normalize="index"
    ).mul(100).round(2)
)

if rating_col is not None:
    print("\nGennemsnitlig rating pr. virksomhed:")
    print(
        df.groupby("company_source")[rating_col]
        .mean()
        .round(2)
    )


# --------------------------------------------------
# Plots
# --------------------------------------------------

sentida_pct_plot = (
    df["sentida_label"]
    .value_counts(normalize=True, dropna=False)
    .mul(100)
)

plt.figure(figsize=(7, 4))
sentida_pct_plot.plot(kind="bar")
plt.title("Fordeling af Sentida-labels (%)")
plt.xlabel("Sentida-label")
plt.ylabel("Andel af reviews (%)")
plt.xticks(rotation=0)
plt.ylim(0, 100)
plt.tight_layout()
plt.savefig(os.path.join(PLOT_FOLDER, "sentida_label_distribution_pct.png"), dpi=300)
plt.show()


company_sentida = (
    pd.crosstab(
        df["company_source"],
        df["sentida_label"],
        normalize="index"
    )
    .mul(100)
)

company_sentida.plot(kind="bar", figsize=(8, 4))
plt.title("Sentida-labels pr. virksomhed (%)")
plt.xlabel("Virksomhed")
plt.ylabel("Andel af reviews (%)")
plt.xticks(rotation=0)
plt.ylim(0, 100)
plt.tight_layout()
plt.savefig(os.path.join(PLOT_FOLDER, "sentida_by_company_pct.png"), dpi=300)
plt.show()


if rating_col is not None:
    rating_by_company = (
        df.groupby("company_source")[rating_col]
        .mean()
        .sort_values(ascending=False)
    )

    plt.figure(figsize=(7, 4))
    rating_by_company.plot(kind="bar")
    plt.title("Gennemsnitlig rating pr. virksomhed")
    plt.xlabel("Virksomhed")
    plt.ylabel("Gennemsnitlig rating")
    plt.xticks(rotation=0)
    plt.tight_layout()
    plt.savefig(os.path.join(PLOT_FOLDER, "avg_rating_by_company.png"), dpi=300)
    plt.show()


# --------------------------------------------------
# Gem som Parquet
# --------------------------------------------------

df.to_parquet(
    OUTPUT_PATH,
    index=False
)

print("\nKlargjort Parquet gemt her:")
print(OUTPUT_PATH)

print("\nPlots gemt i mappen:")
print(PLOT_FOLDER)

print("\nScriptet er færdigt.")