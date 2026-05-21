# -*- coding: utf-8 -*-

import importlib
import numpy as np
import pandas as pd
from sentida import Sentida
from langchain_ollama.llms import OllamaLLM
from langchain_core.prompts import ChatPromptTemplate
import my_vector_rev


# --------------------------------------------------
# Performance-indstillinger
# --------------------------------------------------

MAX_REVIEWS_TO_MODEL = 8
MAX_CHARS_PER_REVIEW = 600
MODEL_NAME = "llama3.1:8b"
NUM_PREDICT = 180


# --------------------------------------------------
# Reload vector-fil
# --------------------------------------------------

importlib.reload(my_vector_rev)


# --------------------------------------------------
# Hent dataframe
# --------------------------------------------------

df = my_vector_rev.df.copy()

print("Kolonner i my_vector_rev.df:")
print(df.columns.tolist())
print("\nAntal reviews:", len(df))


# --------------------------------------------------
# Hjælpefunktion: find kolonne automatisk
# --------------------------------------------------

def find_column(df, possible_names):
    for col in possible_names:
        if col in df.columns:
            return col
    return None


rating_col = find_column(df, ["rating", "score", "stars", "vurdering", "bedømmelse", "review_rating"])
gender_col = find_column(df, ["inferred_gender", "gender_guess"])
city_col = find_column(df, ["inferred_city", "city_guess"])
content_col = find_column(df, ["content_clean", "content", "review", "text"])
title_col = find_column(df, ["title", "headline", "overskrift"])
date_col = find_column(df, ["published", "date", "dato", "created_at"])
name_col = find_column(df, ["name", "user", "bruger", "author"])

company_col = find_column(
    df,
    [
        "company_source",
        "company",
        "store",
        "brand",
        "chain",
        "retailer"
    ]
)


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
# Sentida-kolonner hvis de mangler
# --------------------------------------------------

if "sentida_score" not in df.columns or "sentida_label" not in df.columns:

    print("\nSentida-kolonner mangler - beregner dem nu...")

    if content_col is None:
        raise ValueError("Kan ikke beregne Sentida, fordi der ikke findes en tekstkolonne.")

    sentida = Sentida()

    def get_sentida_score(text):
        if pd.isna(text):
            return np.nan

        text = str(text).strip()

        if text == "":
            return np.nan

        try:
            return float(
                sentida.sentida(
                    text,
                    output="mean",
                    normal=False
                )
            )
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


    df["sentida_score"] = df[content_col].apply(get_sentida_score)
    df["sentida_label"] = df["sentida_score"].apply(get_sentida_label)

    print("Sentida-kolonner oprettet.")

sentida_score_col = find_column(df, ["sentida_score", "sentidascore", "sscore"])
sentida_label_col = find_column(df, ["sentida_label", "sentidalabel", "slabel"])


# --------------------------------------------------
# Klargør numeriske kolonner
# --------------------------------------------------

if rating_col is not None:
    df[rating_col] = pd.to_numeric(df[rating_col], errors="coerce")

if sentida_score_col is not None:
    df[sentida_score_col] = pd.to_numeric(df[sentida_score_col], errors="coerce")


print("\nFundne kolonner:")
print("Rating:", rating_col)
print("Køn:", gender_col)
print("By:", city_col)
print("Tekst:", content_col)
print("Titel:", title_col)
print("Dato:", date_col)
print("Navn:", name_col)
print("Virksomhed:", company_col)
print("Sentida-score:", sentida_score_col)
print("Sentida-label:", sentida_label_col)


# --------------------------------------------------
# Forbered lowercase hjælpekolonner for hurtigere filtrering
# --------------------------------------------------

if company_col is not None:
    df["_company_lower"] = df[company_col].astype(str).str.lower()

if city_col is not None:
    df["_city_lower"] = df[city_col].astype(str).str.lower()

if gender_col is not None:
    df["_gender_lower"] = df[gender_col].astype(str).str.lower()

if sentida_label_col is not None:
    df["_sentida_label_lower"] = df[sentida_label_col].astype(str).str.lower()


# --------------------------------------------------
# Model
# --------------------------------------------------

model = OllamaLLM(
    model=MODEL_NAME,
    temperature=0,
    num_predict=NUM_PREDICT
)


# --------------------------------------------------
# Prompt
# --------------------------------------------------

template = """
Du er en præcis dansk analyseassistent, der hjælper med at analysere kundeanmeldelser af Power og Elgiganten.

Du må kun bruge information fra anmeldelserne nedenfor og metadataen nedenfor.
Du må ikke opfinde tal, navne, byer, mønstre eller årsager.

Vigtige regler:
- Brug altid "Antal filtrerede anmeldelser" som det korrekte antal.
- Anmeldelserne nedenfor er kun eksempler, ikke nødvendigvis hele datasættet.
- Hvis der kun vises få anmeldelser, betyder det ikke, at det er hele datasættet.
- Hvis datagrundlaget er lille, skal du sige det tydeligt.
- Hvis spørgsmålet ikke kan besvares ud fra anmeldelserne eller metadataen, skal du sige det.
- Brug konkrete eksempler fra anmeldelserne, men undgå lange citater.
- Skeln mellem fakta og fortolkning.
- Infereret køn er usikkert og baseret på fornavn.
- Infereret by er usikker og baseret på tekstindhold.
- Sentida-score og Sentida-label bruges til at identificere positive, neutrale og negative anmeldelser.
- Rating er kundens vurdering, hvis den findes.
- Hvis brugeren sammenligner Power og Elgiganten, skal du tydeligt beskrive forskelle og ligheder.
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
3. Forskelle/mønstre
4. Eksempler fra anmeldelserne
5. Eventuelle forbehold
"""

prompt = ChatPromptTemplate.from_template(template)
chain = prompt | model


# --------------------------------------------------
# Filtrér dataframe
# --------------------------------------------------

def filter_dataframe(question, df):
    subset = df
    question_lower = question.lower()

    active_filters = []

    for alias, main_city in city_question_aliases.items():
        if alias in question_lower:
            question_lower += f" {main_city}"
            active_filters.append(f"by-alias: {alias} -> {main_city.title()}")

    if company_col is not None:
        if "power" in question_lower:
            subset = subset[subset["_company_lower"].str.contains("power", na=False)]
            active_filters.append("virksomhed = Power")

        if "elgiganten" in question_lower:
            subset = subset[subset["_company_lower"].str.contains("elgiganten", na=False)]
            active_filters.append("virksomhed = Elgiganten")

    if gender_col is not None:
        if "kvinde" in question_lower or "kvinder" in question_lower:
            subset = subset[subset["_gender_lower"] == "kvinde"]
            active_filters.append("køn = Kvinde")

        if "mand" in question_lower or "mænd" in question_lower:
            subset = subset[subset["_gender_lower"] == "mand"]
            active_filters.append("køn = Mand")

    if sentida_label_col is not None:
        if "negativ" in question_lower or "negative" in question_lower:
            subset = subset[subset["_sentida_label_lower"] == "negativ"]
            active_filters.append("sentida_label = Negativ")

        elif "positiv" in question_lower or "positive" in question_lower:
            subset = subset[subset["_sentida_label_lower"] == "positiv"]
            active_filters.append("sentida_label = Positiv")

        elif "neutral" in question_lower or "neutrale" in question_lower:
            subset = subset[subset["_sentida_label_lower"] == "neutral"]
            active_filters.append("sentida_label = Neutral")

    if rating_col is not None:
        if "lav rating" in question_lower or "lav vurdering" in question_lower:
            subset = subset[subset[rating_col] <= 2]
            active_filters.append("rating <= 2")

        if "høj rating" in question_lower or "høj vurdering" in question_lower:
            subset = subset[subset[rating_col] >= 4]
            active_filters.append("rating >= 4")

    if city_col is not None:
        known_cities = df["_city_lower"].dropna().unique().tolist()

        for city in known_cities:
            if city != "ved ikke" and city in question_lower:
                subset = subset[subset["_city_lower"] == city]
                active_filters.append(f"by = {city.title()}")
                break

    return subset, active_filters


# --------------------------------------------------
# Metadata
# --------------------------------------------------

def build_metadata(subset, active_filters):
    metadata = f"""
Antal anmeldelser i hele datasættet: {len(df)}
Antal filtrerede anmeldelser: {len(subset)}
Aktive filtre: {active_filters if active_filters else "Ingen"}
"""

    if company_col is not None:
        metadata += "\nFordeling på virksomhed i filtreret datasæt:\n"
        metadata += subset[company_col].value_counts(dropna=False).to_string()

    if rating_col is not None and subset[rating_col].notna().sum() > 0:
        metadata += f"""

Gennemsnitlig rating i filtreret datasæt: {subset[rating_col].mean():.2f}
Median rating i filtreret datasæt: {subset[rating_col].median():.2f}
Laveste rating i filtreret datasæt: {subset[rating_col].min()}
Højeste rating i filtreret datasæt: {subset[rating_col].max()}
"""

        if company_col is not None:
            metadata += "\nGennemsnitlig rating pr. virksomhed:\n"
            metadata += subset.groupby(company_col)[rating_col].mean().round(2).to_string()

    if sentida_label_col is not None:
        metadata += "\n\nSentida-label-fordeling i filtreret datasæt:\n"
        metadata += subset[sentida_label_col].value_counts(dropna=False).to_string()

        if company_col is not None:
            metadata += "\n\nSentida-label-fordeling pr. virksomhed i procent:\n"
            metadata += (
                pd.crosstab(
                    subset[company_col],
                    subset[sentida_label_col],
                    normalize="index"
                )
                .mul(100)
                .round(2)
                .to_string()
            )

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
# Vælg reviews til modellen
# --------------------------------------------------

def select_reviews_for_model(subset, question):
    question_lower = question.lower()

    selected = subset

    if sentida_score_col is not None:
        if "negativ" in question_lower or "negative" in question_lower or "problem" in question_lower or "klage" in question_lower:
            selected = selected.sort_values(by=sentida_score_col, ascending=True)

        elif "positiv" in question_lower or "positive" in question_lower or "ros" in question_lower:
            selected = selected.sort_values(by=sentida_score_col, ascending=False)

    elif rating_col is not None:
        if "lav" in question_lower or "dårlig" in question_lower or "værst" in question_lower:
            selected = selected.sort_values(by=rating_col, ascending=True)

        elif "høj" in question_lower or "god" in question_lower or "bedst" in question_lower:
            selected = selected.sort_values(by=rating_col, ascending=False)

    return selected.head(MAX_REVIEWS_TO_MODEL)


# --------------------------------------------------
# Lav review-tekst
# --------------------------------------------------

def build_reviews_text(subset):
    reviews_text = ""

    for _, row in subset.iterrows():

        review_text = row.get(content_col, "Ingen anmeldelsestekst") if content_col else "Ingen anmeldelsestekst"
        title = row.get(title_col, "Ukendt titel") if title_col else "Ukendt titel"
        rating = row.get(rating_col, "Ukendt") if rating_col else "Ukendt"
        date = row.get(date_col, "Ukendt") if date_col else "Ukendt"
        name = row.get(name_col, "Ukendt") if name_col else "Ukendt"
        gender_value = row.get(gender_col, "Ved ikke") if gender_col else "Ved ikke"
        city_value = row.get(city_col, "Ved ikke") if city_col else "Ved ikke"
        company_value = row.get(company_col, "Ukendt") if company_col else "Ukendt"
        sentida_score = row.get(sentida_score_col, "Ukendt") if sentida_score_col else "Ukendt"
        sentida_label = row.get(sentida_label_col, "Ukendt") if sentida_label_col else "Ukendt"

        reviews_text += f"""
Anmeldelse:
{str(review_text)[:MAX_CHARS_PER_REVIEW]}

Titel:
{title}

Metadata:
- Virksomhed: {company_value}
- Rating: {rating}
- Dato: {date}
- Bruger: {name}
- Infereret køn: {gender_value}
- Infereret by: {city_value}
- Sentida-score: {sentida_score}
- Sentida-label: {sentida_label}

---
"""

    return reviews_text


# --------------------------------------------------
# Rene tællespørgsmål
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

    metadata = build_metadata(subset, active_filters)

    selected_reviews = select_reviews_for_model(subset, question)

    reviews_text = build_reviews_text(selected_reviews)

    print("\nMetadata sendt til modellen:\n")
    print(metadata)

    print("\nAntal anmeldelser sendt til modellen:", len(selected_reviews))

    print("\nSender prompt til Ollama...\n")

    result = chain.invoke({
        "metadata": metadata,
        "reviews": reviews_text,
        "question": question
    })

    print("\nSvar:\n")
    print(result)