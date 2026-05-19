# -*- coding: utf-8 -*-

import importlib
import numpy as np
import pandas as pd
from sentida import Sentida
from langchain_ollama.llms import OllamaLLM
from langchain_core.prompts import ChatPromptTemplate
import my_vector


# --------------------------------------------------
# Reload my_vector
# --------------------------------------------------

importlib.reload(my_vector)


# --------------------------------------------------
# Hent dataframe
# --------------------------------------------------

df = my_vector.df.copy()

print("Kolonner i my_vector.df:")
print(df.columns.tolist())


# --------------------------------------------------
# Hjælpefunktion: find kolonne automatisk
# --------------------------------------------------

def find_column(df, possible_names):
    for col in possible_names:
        if col in df.columns:
            return col
    return None


rating_col = find_column(df, ["rating", "score", "stars", "vurdering", "bedømmelse"])
gender_col = find_column(df, ["inferred_gender", "gender_guess"])
city_col = find_column(df, ["inferred_city", "city_guess"])
content_col = find_column(df, ["content_clean", "content", "review"])
title_col = find_column(df, ["title", "headline", "overskrift"])
date_col = find_column(df, ["published", "date", "dato"])
name_col = find_column(df, ["name", "user", "bruger"])
broker_col = find_column(df, ["company", "broker", "mægler", "maegler", "agency", "estate_agent"])


# --------------------------------------------------
# By-aliases i spørgsmål
# --------------------------------------------------

city_question_aliases = {
    "nørrebro": "københavn",
    "noerrebro": "københavn",
    "norrebro": "københavn",
    "amager": "københavn",
    "amagerbro": "københavn",
    "vesterbro": "københavn",
    "østerbro": "københavn",
    "oesterbro": "københavn",
    "osterbro": "københavn",
    "valby": "københavn",
    "vanløse": "københavn",
    "vanloese": "københavn",
    "brønshøj": "københavn",
    "bronshoj": "københavn",
    "indre by": "københavn",
    "kbh": "københavn",
    "kbh k": "københavn",
    "kbh n": "københavn",
    "kbh ø": "københavn",
    "kbh o": "københavn",
    "kbh s": "københavn",
    "københavn k": "københavn",
    "københavn n": "københavn",
    "københavn ø": "københavn",
    "københavn o": "københavn",
    "københavn s": "københavn",

    "aarhus c": "aarhus",
    "århus c": "aarhus",
    "århus": "aarhus",

    "odense c": "odense",
    "odense m": "odense",

    "aalborg c": "aalborg",
    "ålborg": "aalborg",
    "ålborg c": "aalborg",
}


# --------------------------------------------------
# HURTIG FIX: lav Sentida-kolonner hvis de mangler
# --------------------------------------------------

if "sentida_score" not in df.columns or "sentida_label" not in df.columns:

    print("\nSentida-kolonner mangler - beregner dem nu...")

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


    if content_col is None:
        raise ValueError("Kan ikke beregne Sentida, fordi der ikke findes en tekstkolonne.")

    df["sentida_score"] = df[content_col].apply(get_sentida_score)
    df["sentida_label"] = df["sentida_score"].apply(get_sentida_label)

    print("\nSentida-kolonner oprettet.")
    print(df["sentida_label"].value_counts(dropna=False))


sentida_score_col = find_column(df, ["sentida_score", "sentidascore", "sscore"])
sentida_label_col = find_column(df, ["sentida_label", "sentidalabel", "slabel"])


print("\nFundne kolonner:")
print("Rating:", rating_col)
print("Køn:", gender_col)
print("By:", city_col)
print("Tekst:", content_col)
print("Titel:", title_col)
print("Dato:", date_col)
print("Navn:", name_col)
print("Mægler:", broker_col)
print("Sentida-score:", sentida_score_col)
print("Sentida-label:", sentida_label_col)


# --------------------------------------------------
# Klargør numeriske kolonner
# --------------------------------------------------

if rating_col is not None:
    df[rating_col] = pd.to_numeric(df[rating_col], errors="coerce")

if sentida_score_col is not None:
    df[sentida_score_col] = pd.to_numeric(df[sentida_score_col], errors="coerce")


# --------------------------------------------------
# Model
# --------------------------------------------------

model = OllamaLLM(
    model="llama3.1:8b",
    temperature=0,
    num_predict=200
)


# --------------------------------------------------
# Prompt
# --------------------------------------------------

template = """
Du er en præcis dansk analyseassistent, der hjælper med at analysere Home-anmeldelser.

Du må kun bruge information fra anmeldelserne nedenfor og metadataen nedenfor.
Du må ikke opfinde tal, navne, byer, mønstre eller årsager.

Vigtige regler:
- Du må ikke selv tælle anmeldelser, hvis et samlet antal allerede står i metadata.
- Brug altid "Antal filtrerede anmeldelser" som det korrekte antal.
- Anmeldelserne nedenfor er kun eksempler, ikke nødvendigvis hele datasættet.
- Hvis der kun vises 10 anmeldelser, betyder det ikke, at der kun findes 10 anmeldelser.
- Hvis datagrundlaget er lille, skal du sige det tydeligt.
- Hvis spørgsmålet ikke kan besvares ud fra anmeldelserne eller metadataen, skal du sige det.
- Brug konkrete eksempler fra anmeldelserne, men undgå lange citater.
- Skeln mellem fakta og fortolkning.
- Infereret køn er usikkert og baseret på fornavn.
- Infereret by er usikker og baseret på tekstindhold og eventuelle by-aliases.
- Sentida-score og Sentida-label bruges til at identificere positive, neutrale og negative anmeldelser.
- Rating er kundens vurdering, hvis den findes.
- Svar på dansk.
- Svar struktureret med korte afsnit.

Metadata:
{metadata}

Anmeldelser:
{reviews}

Brugerens spørgsmål:
{question}

Svar med denne struktur:

1. Kort svar
2. Hvad anmeldelserne viser
3. Eksempler fra anmeldelserne
4. Eventuelle forbehold
"""

prompt = ChatPromptTemplate.from_template(template)
chain = prompt | model


# --------------------------------------------------
# Hjælpefunktion: filtrér dataframe
# --------------------------------------------------

def filter_dataframe(question, df):
    subset = df.copy()
    question_lower = question.lower()

    active_filters = []

    # Tilføj hovedby til spørgsmålet, hvis brugeren nævner en bydel/alias
    for alias, main_city in city_question_aliases.items():
        if alias in question_lower:
            question_lower += f" {main_city}"
            active_filters.append(f"by-alias: {alias} -> {main_city.title()}")

    # Filtrer på køn
    if gender_col is not None:
        if "kvinde" in question_lower or "kvinder" in question_lower:
            subset = subset[subset[gender_col] == "Kvinde"]
            active_filters.append("køn = Kvinde")

        if "mand" in question_lower or "mænd" in question_lower:
            subset = subset[subset[gender_col] == "Mand"]
            active_filters.append("køn = Mand")

    # Filtrer på Sentida-label
    if sentida_label_col is not None:
        if "negativ" in question_lower or "negative" in question_lower:
            subset = subset[
                subset[sentida_label_col].astype(str).str.lower() == "negativ"
            ]
            active_filters.append("sentida_label = Negativ")

        elif "positiv" in question_lower or "positive" in question_lower:
            subset = subset[
                subset[sentida_label_col].astype(str).str.lower() == "positiv"
            ]
            active_filters.append("sentida_label = Positiv")

        elif "neutral" in question_lower or "neutrale" in question_lower:
            subset = subset[
                subset[sentida_label_col].astype(str).str.lower() == "neutral"
            ]
            active_filters.append("sentida_label = Neutral")

    # Filtrer på rating
    if rating_col is not None:
        if "lav rating" in question_lower or "lav vurdering" in question_lower:
            subset = subset[subset[rating_col] <= 2]
            active_filters.append("rating <= 2")

        if "høj rating" in question_lower or "høj vurdering" in question_lower:
            subset = subset[subset[rating_col] >= 4]
            active_filters.append("rating >= 4")

        # ----------------------------------------------
    # Filtrer på by/bydel
    # ----------------------------------------------

    if city_col is not None:

        # Først: direkte alias-søgning i tekstfelter
        matched_alias = None

        for alias, main_city in city_question_aliases.items():
            if alias in question_lower:
                matched_alias = alias
                break

        if matched_alias is not None:
            search_cols = []

            for col in [content_col, title_col, broker_col, city_col]:
                if col is not None and col in df.columns:
                    search_cols.append(col)

            alias_pattern = matched_alias.lower()

            mask = pd.Series(False, index=subset.index)

            for col in search_cols:
                mask = mask | subset[col].astype(str).str.lower().str.contains(
                    alias_pattern,
                    regex=False,
                    na=False
                )

            subset = subset[mask]
            active_filters.append(f"bydel/alias = {matched_alias}")

        else:
            # Almindelig by-søgning
            known_cities = (
                df[city_col]
                .dropna()
                .astype(str)
                .unique()
                .tolist()
            )

            for city in known_cities:
                city_lower = city.lower()

                if city_lower != "ved ikke" and city_lower in question_lower:
                    subset = subset[
                        subset[city_col].astype(str).str.lower() == city_lower
                    ]
                    active_filters.append(f"by = {city}")

    # Filtrer på mægler/firma
    if broker_col is not None:
        known_brokers = (
            df[broker_col]
            .dropna()
            .astype(str)
            .unique()
            .tolist()
        )

        for broker in known_brokers:
            broker_lower = broker.lower()

            if broker_lower in question_lower:
                subset = subset[
                    subset[broker_col].astype(str).str.lower() == broker_lower
                ]
                active_filters.append(f"mægler = {broker}")

    return subset, active_filters


# --------------------------------------------------
# Hjælpefunktion: lav metadata
# --------------------------------------------------

def build_metadata(subset, active_filters):
    metadata = f"""
Antal anmeldelser i hele datasættet: {len(df)}
Antal filtrerede anmeldelser: {len(subset)}
Aktive filtre: {active_filters if active_filters else "Ingen"}
"""

    if rating_col is not None and subset[rating_col].notna().sum() > 0:
        metadata += f"""
Gennemsnitlig rating i filtreret datasæt: {subset[rating_col].mean():.2f}
Median rating i filtreret datasæt: {subset[rating_col].median():.2f}
Laveste rating i filtreret datasæt: {subset[rating_col].min()}
Højeste rating i filtreret datasæt: {subset[rating_col].max()}
"""

    if sentida_label_col is not None:
        metadata += "\nSentida-label-fordeling i filtreret datasæt:\n"
        metadata += subset[sentida_label_col].value_counts(dropna=False).to_string()

    if sentida_score_col is not None and subset[sentida_score_col].notna().sum() > 0:
        metadata += f"""

Gennemsnitlig Sentida-score i filtreret datasæt: {subset[sentida_score_col].mean():.4f}
Laveste Sentida-score i filtreret datasæt: {subset[sentida_score_col].min():.4f}
Højeste Sentida-score i filtreret datasæt: {subset[sentida_score_col].max():.4f}
"""

    if city_col is not None:
        metadata += "\nTop byer i filtreret datasæt:\n"
        metadata += subset[city_col].value_counts(dropna=False).head(10).to_string()

    return metadata


# --------------------------------------------------
# Hjælpefunktion: lav tekst til modellen
# --------------------------------------------------

def build_reviews_text(subset, max_reviews=5, max_chars_per_review=700):
    subset = subset.head(max_reviews)

    reviews_text = ""

    for _, row in subset.iterrows():

        review_text = row.get(content_col, "Ingen anmeldelsestekst") if content_col else "Ingen anmeldelsestekst"
        title = row.get(title_col, "Ukendt titel") if title_col else "Ukendt titel"
        rating = row.get(rating_col, "Ukendt") if rating_col else "Ukendt"
        date = row.get(date_col, "Ukendt") if date_col else "Ukendt"
        name = row.get(name_col, "Ukendt") if name_col else "Ukendt"
        gender_value = row.get(gender_col, "Ved ikke") if gender_col else "Ved ikke"
        city_value = row.get(city_col, "Ved ikke") if city_col else "Ved ikke"
        broker_value = row.get(broker_col, "Ukendt") if broker_col else "Ukendt"
        sentida_score = row.get(sentida_score_col, "Ukendt") if sentida_score_col else "Ukendt"
        sentida_label = row.get(sentida_label_col, "Ukendt") if sentida_label_col else "Ukendt"

        reviews_text += f"""
Anmeldelse:
{str(review_text)[:max_chars_per_review]}

Titel:
{title}

Metadata:
- Rating: {rating}
- Dato: {date}
- Bruger: {name}
- Infereret køn: {gender_value}
- Infereret by: {city_value}
- Mægler/firma: {broker_value}
- Sentida-score: {sentida_score}
- Sentida-label: {sentida_label}

---
"""

    return reviews_text


# --------------------------------------------------
# Hjælpefunktion: identificér rene tællespørgsmål
# --------------------------------------------------

def is_count_question(question):
    question_lower = question.lower()

    count_terms = [
        "hvor mange",
        "antal",
        "tæl",
        "tælle",
        "count"
    ]

    return any(term in question_lower for term in count_terms)


# --------------------------------------------------
# Main loop
# --------------------------------------------------

while True:

    print("\n\n-------------------------------")

    question = input("Stil dit spørgsmål (q to quit): ")

    print("\n")

    if question.lower() == "q":
        break

    subset, active_filters = filter_dataframe(question, df)

    print("Aktive filtre:", active_filters if active_filters else "Ingen")
    print("Antal filtrerede anmeldelser:", len(subset))

    if len(subset) == 0:
        print("Ingen anmeldelser matcher filtrene.")
        continue

    if is_count_question(question):
        print("\nSvar:\n")
        print(f"Der er {len(subset)} anmeldelser, der matcher filtrene.")
        continue

    question_lower = question.lower()

    if rating_col is not None:
        if "lav rating" in question_lower or "lav vurdering" in question_lower:
            subset = subset.sort_values(by=rating_col, ascending=True)

        elif "høj rating" in question_lower or "høj vurdering" in question_lower:
            subset = subset.sort_values(by=rating_col, ascending=False)

    if sentida_score_col is not None:
        if "negativ" in question_lower or "negative" in question_lower:
            subset = subset.sort_values(by=sentida_score_col, ascending=True)

        elif "positiv" in question_lower or "positive" in question_lower:
            subset = subset.sort_values(by=sentida_score_col, ascending=False)

    metadata = build_metadata(subset, active_filters)

    reviews_text = build_reviews_text(
        subset,
        max_reviews=10,
        max_chars_per_review=700
    )

    print("\nMetadata sendt til modellen:\n")
    print(metadata)

    print("\nSender disse anmeldelseseksempler til modellen:\n")
    print(reviews_text[:3000])

    print("\nSender prompt til Ollama...\n")

    result = chain.invoke({
        "metadata": metadata,
        "reviews": reviews_text,
        "question": question
    })

    print("\nSvar:\n")
    print(result)
    
##### Fakta tjek
odense_positive = df[
    (df["inferred_city"].astype(str).str.lower() == "odense")
    & (df["sentida_label"].astype(str).str.lower() == "positiv")
]

print("Positive reviews i Odense:", len(odense_positive))