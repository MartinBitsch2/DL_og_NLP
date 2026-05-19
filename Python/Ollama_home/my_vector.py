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

INPUT_PATH = "C:/Users/mikk1/Documents/Deep Learning/Python/Spyder/home.csv"
OUTPUT_PATH = "C:/Users/mikk1/Documents/Deep Learning/Python/Spyder/home_prepared.csv"
PLOT_FOLDER = "C:/Users/mikk1/Documents/Deep Learning/Python/Spyder/plots"

os.makedirs(PLOT_FOLDER, exist_ok=True)


# --------------------------------------------------
# Indlæs data
# --------------------------------------------------

df = pd.read_csv(
    INPUT_PATH,
    encoding="latin1",
    delimiter=","
)

print("Antal reviews i CSV:", len(df))


# --------------------------------------------------
# Tjek nødvendige kolonner
# --------------------------------------------------

required_columns = ["name", "content"]

missing_columns = [col for col in required_columns if col not in df.columns]

if missing_columns:
    raise ValueError(f"Mangler nødvendige kolonner i CSV: {missing_columns}")

print("\nKolonner i original df:")
print(df.columns.tolist())


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
# Feature engineering: Sentida sentiment
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

print("\nEksempel på Sentida-scores:")
print(
    df[
        [
            "content_clean",
            "sentida_score",
            "sentida_label"
        ]
    ].head(10)
)


# --------------------------------------------------
# Feature engineering: infereret køn
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

print("\nFordeling af infereret køn:")
print(df["inferred_gender"].value_counts(dropna=False))


# --------------------------------------------------
# Feature engineering: infereret by
# --------------------------------------------------

gc = geonamescache.GeonamesCache()
cities = gc.get_cities()

# --------------------------------------------------
# Standard danske byer fra geonamescache
# --------------------------------------------------

danish_cities = set(
    city_info["name"].lower()
    for city_info in cities.values()
    if city_info.get("countrycode") == "DK"
)

# --------------------------------------------------
# Manuelle aliases / bydele / forkortelser
# --------------------------------------------------

city_aliases = {
    # København bydele
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
    "bronshoj": "København",
    "brønshøj": "København",
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

    # Aarhus
    "aarhus c": "Aarhus",
    "århus c": "Aarhus",
    "århus": "Aarhus",

    # Odense
    "odense c": "Odense",
    "odense m": "Odense",

    # Aalborg
    "aalborg c": "Aalborg",
    "ålborg": "Aalborg",
    "ålborg c": "Aalborg",
}

all_city_terms = set(danish_cities) | set(city_aliases.keys())

all_city_terms = sorted(
    all_city_terms,
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

    if "content_clean" in row.index:
        search_parts.append(row.get("content_clean", ""))

    if "title" in row.index:
        search_parts.append(row.get("title", ""))

    if "company" in row.index:
        search_parts.append(row.get("company", ""))

    if "broker" in row.index:
        search_parts.append(row.get("broker", ""))

    text_original = " ".join([str(x) for x in search_parts if pd.notna(x)])
    text_normalized = normalize_text_for_city(text_original)

    for city_term in all_city_terms:
        city_term_normalized = normalize_text_for_city(city_term)

        pattern = r"(?<![a-zA-ZæøåÆØÅ])" + re.escape(city_term_normalized) + r"(?![a-zA-ZæøåÆØÅ])"

        if re.search(pattern, text_normalized):
            if city_term in city_aliases:
                return city_aliases[city_term]

            return city_term.title()

    return "Ved ikke"

# --------------------------------------------------
# Kombinér alt til samlet søgeliste
# --------------------------------------------------

all_city_terms = set(danish_cities) | set(city_aliases.keys())

# Sortér længste først
all_city_terms = sorted(
    all_city_terms,
    key=len,
    reverse=True
)

print("\nAntal danske byord:", len(all_city_terms))

print("\nAntal danske byer i bylisten:", len(danish_cities))


def infer_city(text):

    if pd.isna(text):
        return "Ved ikke"

    text = str(text).lower()

    for city_term in all_city_terms:

        pattern = r"\b" + re.escape(city_term) + r"\b"

        if re.search(pattern, text):

            # Hvis alias → map til hovedby
            if city_term in city_aliases:
                return city_aliases[city_term]

            # Ellers brug originalt bynavn
            return city_term.title()

    return "Ved ikke"


df["inferred_city"] = df.apply(infer_city_from_row, axis=1)

print("\nTop 20 infererede byer:")
print(df["inferred_city"].value_counts(dropna=False).head(20))


# --------------------------------------------------
# Markér usikre features
# --------------------------------------------------

df["has_inferred_gender"] = df["inferred_gender"] != "Ved ikke"
df["has_inferred_city"] = df["inferred_city"] != "Ved ikke"
df["has_sentida_score"] = df["sentida_label"] != "Ved ikke"


# --------------------------------------------------
# Automatisk forsøg på at finde rating- og mæglerkolonne
# --------------------------------------------------

possible_rating_columns = ["rating", "score", "stars", "vurdering", "bedømmelse"]
possible_broker_columns = ["company", "broker", "mægler", "maegler", "agency", "estate_agent"]

rating_col = None
broker_col = None

for col in possible_rating_columns:
    if col in df.columns:
        rating_col = col
        break

for col in possible_broker_columns:
    if col in df.columns:
        broker_col = col
        break

print("\nFundet rating-kolonne:", rating_col)
print("Fundet mægler-kolonne:", broker_col)


# --------------------------------------------------
# Beskrivende statistik
# --------------------------------------------------

print("\n--- Beskrivende statistik ---")

total_reviews = len(df)

print("\nAntal observationer:")
print(total_reviews)

print("\nAntal unikke navne:")
print(df["name"].nunique(dropna=True))

empty_reviews_count = (df["content_clean"] == "").sum()
empty_reviews_pct = empty_reviews_count / total_reviews * 100

print("\nTomme reviews:")
print(f"{empty_reviews_count} reviews ({empty_reviews_pct:.2f}%)")

print("\nGennemsnitlig review-længde i tegn:")
df["review_length_chars"] = df["content_clean"].str.len()
print(round(df["review_length_chars"].mean(), 2))

print("\nGennemsnitlig review-længde i ord:")
df["review_length_words"] = df["content_clean"].apply(lambda x: len(x.split()))
print(round(df["review_length_words"].mean(), 2))


# --------------------------------------------------
# Sentida-statistik
# --------------------------------------------------

print("\nGennemsnitlig Sentida-score:")
print(round(df["sentida_score"].mean(), 4))

print("\nMedian Sentida-score:")
print(round(df["sentida_score"].median(), 4))

sentida_pct = (
    df["sentida_label"]
    .value_counts(normalize=True, dropna=False)
    .mul(100)
)

print("\nFordeling af Sentida-labels i %:")
print(sentida_pct.round(2))


# --------------------------------------------------
# Rating-statistik
# --------------------------------------------------

if rating_col is not None:
    df[rating_col] = pd.to_numeric(df[rating_col], errors="coerce")

    print("\nGennemsnitlig vurdering:")
    print(round(df[rating_col].mean(), 2))

    print("\nMedian vurdering:")
    print(round(df[rating_col].median(), 2))

    print("\nLaveste vurdering:")
    print(df[rating_col].min())

    print("\nHøjeste vurdering:")
    print(df[rating_col].max())

    rating_pct = (
        df[rating_col]
        .dropna()
        .value_counts(normalize=True)
        .sort_index()
        .mul(100)
    )

    print("\nFordeling af ratings i %:")
    print(rating_pct.round(2))


# --------------------------------------------------
# Fordeling af infereret køn i %
# --------------------------------------------------

gender_pct = (
    df["inferred_gender"]
    .value_counts(normalize=True, dropna=False)
    .mul(100)
)

print("\nFordeling af infereret køn i %:")
print(gender_pct.round(2))


# --------------------------------------------------
# Fordeling af infereret by i %
# --------------------------------------------------

city_pct = (
    df["inferred_city"]
    .value_counts(normalize=True, dropna=False)
    .mul(100)
)

print("\nTop 20 infererede byer i %:")
print(city_pct.head(20).round(2))


# --------------------------------------------------
# Plot: Fordeling af Sentida-labels i %
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


# --------------------------------------------------
# Plot: Fordeling af Sentida-score
# --------------------------------------------------

plt.figure(figsize=(7, 4))

df["sentida_score"].dropna().plot(
    kind="hist",
    bins=30
)

plt.title("Fordeling af Sentida-score")
plt.xlabel("Sentida-score")
plt.ylabel("Antal reviews")

plt.tight_layout()
plt.savefig(os.path.join(PLOT_FOLDER, "sentida_score_distribution.png"), dpi=300)
plt.show()


# --------------------------------------------------
# Plot: Fordeling af infereret køn i %
# --------------------------------------------------

gender_pct_plot = (
    df["inferred_gender"]
    .value_counts(normalize=True, dropna=False)
    .mul(100)
)

plt.figure(figsize=(7, 4))
gender_pct_plot.plot(kind="bar")

plt.title("Fordeling af infereret køn (%)")
plt.xlabel("Infereret køn")
plt.ylabel("Andel af reviews (%)")
plt.xticks(rotation=0)
plt.ylim(0, 100)

plt.tight_layout()
plt.savefig(os.path.join(PLOT_FOLDER, "gender_distribution_pct.png"), dpi=300)
plt.show()


# --------------------------------------------------
# Plot: Top 10 byer med flest anmeldelser i %
# --------------------------------------------------

city_pct_plot = (
    df[df["inferred_city"] != "Ved ikke"]["inferred_city"]
    .value_counts(normalize=True)
    .head(10)
    .mul(100)
)

if len(city_pct_plot) > 0:
    plt.figure(figsize=(10, 5))
    city_pct_plot.plot(kind="bar")

    plt.title("Top 10 byer med flest anmeldelser (%)")
    plt.xlabel("By")
    plt.ylabel("Andel af reviews med kendt by (%)")
    plt.xticks(rotation=45, ha="right")
    plt.ylim(0, 100)

    plt.tight_layout()
    plt.savefig(os.path.join(PLOT_FOLDER, "top_10_cities_pct.png"), dpi=300)
    plt.show()
else:
    print("\nIngen byer fundet til by-plot.")


# --------------------------------------------------
# Plot: Fordeling af ratings i %
# --------------------------------------------------

if rating_col is not None:
    rating_pct_plot = (
        df[rating_col]
        .dropna()
        .value_counts(normalize=True)
        .sort_index()
        .mul(100)
    )

    plt.figure(figsize=(7, 4))
    rating_pct_plot.plot(kind="bar")

    plt.title("Fordeling af vurderinger (%)")
    plt.xlabel("Vurdering")
    plt.ylabel("Andel af reviews med rating (%)")
    plt.ylim(0, 100)

    plt.tight_layout()
    plt.savefig(os.path.join(PLOT_FOLDER, "rating_distribution_pct.png"), dpi=300)
    plt.show()
else:
    print("\nIngen rating-kolonne fundet. Springer rating-plot over.")


# --------------------------------------------------
# Top 5 bedste og værste mæglere
# --------------------------------------------------

if broker_col is not None and rating_col is not None:
    broker_stats = (
        df.dropna(subset=[broker_col, rating_col])
        .groupby(broker_col)
        .agg(
            avg_rating=(rating_col, "mean"),
            median_rating=(rating_col, "median"),
            avg_sentida_score=("sentida_score", "mean"),
            review_count=(rating_col, "count")
        )
        .reset_index()
    )

    min_reviews = 5

    broker_stats_filtered = broker_stats[
        broker_stats["review_count"] >= min_reviews
    ]

    if len(broker_stats_filtered) == 0:
        print(
            f"\nIngen mæglere har mindst {min_reviews} anmeldelser. "
            "Bruger derfor alle mæglere i stedet."
        )
        broker_stats_filtered = broker_stats.copy()

    best_brokers = broker_stats_filtered.sort_values(
        by=["avg_rating", "review_count"],
        ascending=[False, False]
    ).head(5)

    worst_brokers = broker_stats_filtered.sort_values(
        by=["avg_rating", "review_count"],
        ascending=[True, False]
    ).head(5)

    print("\nTop 5 bedste mæglere:")
    print(best_brokers)

    print("\nTop 5 værste mæglere:")
    print(worst_brokers)

    plt.figure(figsize=(9, 4))
    plt.bar(
        best_brokers[broker_col].astype(str),
        best_brokers["avg_rating"]
    )

    plt.title("Top 5 bedste mæglere")
    plt.xlabel("Mægler")
    plt.ylabel("Gennemsnitlig vurdering")
    plt.xticks(rotation=45, ha="right")

    plt.tight_layout()
    plt.savefig(os.path.join(PLOT_FOLDER, "top_5_best_brokers.png"), dpi=300)
    plt.show()

    plt.figure(figsize=(9, 4))
    plt.bar(
        worst_brokers[broker_col].astype(str),
        worst_brokers["avg_rating"]
    )

    plt.title("Top 5 værste mæglere")
    plt.xlabel("Mægler")
    plt.ylabel("Gennemsnitlig vurdering")
    plt.xticks(rotation=45, ha="right")

    plt.tight_layout()
    plt.savefig(os.path.join(PLOT_FOLDER, "top_5_worst_brokers.png"), dpi=300)
    plt.show()

else:
    print("\nKan ikke lave top/værste mæglere, fordi rating- eller mæglerkolonne mangler.")


# --------------------------------------------------
# Gennemsnitlig rating og Sentida-score pr. by
# --------------------------------------------------

if rating_col is not None:
    city_rating_stats = (
        df[
            (df["inferred_city"] != "Ved ikke")
            & (df[rating_col].notna())
        ]
        .groupby("inferred_city")
        .agg(
            avg_rating=(rating_col, "mean"),
            avg_sentida_score=("sentida_score", "mean"),
            review_count=(rating_col, "count")
        )
        .reset_index()
    )

    min_city_reviews = 5

    city_rating_stats = city_rating_stats[
        city_rating_stats["review_count"] >= min_city_reviews
    ]

    best_cities = city_rating_stats.sort_values(
        by=["avg_rating", "review_count"],
        ascending=[False, False]
    ).head(10)

    worst_cities = city_rating_stats.sort_values(
        by=["avg_rating", "review_count"],
        ascending=[True, False]
    ).head(10)

    print("\nTop 10 bedst ratede byer:")
    print(best_cities)

    print("\nTop 10 dårligst ratede byer:")
    print(worst_cities)

    plt.figure(figsize=(10, 5))
    plt.bar(
        best_cities["inferred_city"],
        best_cities["avg_rating"]
    )

    plt.title("Top 10 bedst ratede byer")
    plt.xlabel("By")
    plt.ylabel("Gennemsnitlig rating")
    plt.xticks(rotation=45, ha="right")

    plt.tight_layout()
    plt.savefig(os.path.join(PLOT_FOLDER, "best_cities.png"), dpi=300)
    plt.show()

    plt.figure(figsize=(10, 5))
    plt.bar(
        worst_cities["inferred_city"],
        worst_cities["avg_rating"]
    )

    plt.title("Top 10 dårligst ratede byer")
    plt.xlabel("By")
    plt.ylabel("Gennemsnitlig rating")
    plt.xticks(rotation=45, ha="right")

    plt.tight_layout()
    plt.savefig(os.path.join(PLOT_FOLDER, "worst_cities.png"), dpi=300)
    plt.show()

else:
    print("\nIngen rating-kolonne fundet. Springer by-rating analyse over.")


# --------------------------------------------------
# Gem klargjort dataframe
# --------------------------------------------------

df.to_csv(
    OUTPUT_PATH,
    index=False,
    encoding="utf-8-sig"
)


# --------------------------------------------------
# Færdig
# --------------------------------------------------

print("\nKolonner i klargjort df:")
print(df.columns.tolist())

print("\nKlargjort CSV gemt her:")
print(OUTPUT_PATH)

print("\nPlots gemt i mappen:")
print(PLOT_FOLDER)

print("\nScriptet er færdigt.")