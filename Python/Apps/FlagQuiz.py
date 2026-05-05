#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon May  4 10:43:48 2026

@author: nicolailove
"""
import streamlit as st
import random
import time
import json
import os

LEADERBOARD_FILE = "leaderboard.json"
TIME_LIMIT = 60

countries = {
    "dk": "Danmark", "se": "Sverige", "no": "Norge", "fi": "Finland",
    "is": "Island", "de": "Tyskland", "fr": "Frankrig", "es": "Spanien",
    "it": "Italien", "pt": "Portugal", "nl": "Holland", "be": "Belgien",
    "ch": "Schweiz", "at": "Østrig", "pl": "Polen", "cz": "Tjekkiet",
    "sk": "Slovakiet", "si": "Slovenien", "hr": "Kroatien", "rs": "Serbien",
    "ba": "Bosnien-Hercegovina", "me": "Montenegro", "al": "Albanien",
    "mk": "Nordmakedonien", "gr": "Grækenland", "tr": "Tyrkiet",
    "ro": "Rumænien", "bg": "Bulgarien", "hu": "Ungarn", "ua": "Ukraine",
    "lt": "Litauen", "lv": "Letland", "ee": "Estland", "ie": "Irland",
    "gb": "Storbritannien", "us": "USA", "ca": "Canada", "mx": "Mexico",
    "br": "Brasilien", "ar": "Argentina", "cl": "Chile", "uy": "Uruguay",
    "py": "Paraguay", "bo": "Bolivia", "pe": "Peru", "co": "Colombia",
    "ve": "Venezuela", "ec": "Ecuador", "jp": "Japan", "cn": "Kina",
    "kr": "Sydkorea", "in": "Indien", "id": "Indonesien", "th": "Thailand",
    "vn": "Vietnam", "ph": "Filippinerne", "my": "Malaysia", "sg": "Singapore",
    "au": "Australien", "nz": "New Zealand", "za": "Sydafrika", "eg": "Egypten",
    "ma": "Marokko", "tn": "Tunesien", "dz": "Algeriet", "ng": "Nigeria",
    "gh": "Ghana", "ke": "Kenya", "cm": "Cameroun", "sn": "Senegal",
    "sa": "Saudi-Arabien", "ae": "Forenede Arabiske Emirater", "qa": "Qatar",
    "il": "Israel", "ge": "Georgien", "am": "Armenien", "az": "Aserbajdsjan",
    "kz": "Kasakhstan", "uz": "Usbekistan", "li": "Liechtenstein",
    "lu": "Luxembourg", "mt": "Malta", "cy": "Cypern", "md": "Moldova"
}


def load_leaderboard():
    if os.path.exists(LEADERBOARD_FILE):
        with open(LEADERBOARD_FILE, "r") as file:
            return json.load(file)
    return []


def save_score(name, score):
    leaderboard = load_leaderboard()

    leaderboard.append({
        "name": name,
        "score": score
    })

    leaderboard = sorted(
        leaderboard,
        key=lambda player: player["score"],
        reverse=True
    )

    leaderboard = leaderboard[:10]

    with open(LEADERBOARD_FILE, "w") as file:
        json.dump(leaderboard, file)


def new_question():
    if len(st.session_state.remaining_codes) == 0:
        st.session_state.remaining_codes = list(countries.keys())
        random.shuffle(st.session_state.remaining_codes)

    correct_code = st.session_state.remaining_codes.pop()
    correct_country = countries[correct_code]

    wrong_answers = random.sample(
        [country for country in countries.values() if country != correct_country],
        3
    )

    options = wrong_answers + [correct_country]
    random.shuffle(options)

    st.session_state.correct_country = correct_country
    st.session_state.flag_url = f"https://flagcdn.com/w320/{correct_code}.png"
    st.session_state.options = options


def end_quiz():
    if not st.session_state.score_saved:
        save_score(st.session_state.name, st.session_state.score)
        st.session_state.score_saved = True


st.set_page_config(page_title="Speed Flag Quiz", page_icon="🌍")

st.title("🌍 Speed Flag Quiz")
st.write("Du har 60 sekunder. Hvor mange flag kan du gætte?")

st.sidebar.header("🏆 Leaderboard")
leaderboard = load_leaderboard()

if leaderboard:
    for position, player in enumerate(leaderboard, start=1):
        st.sidebar.write(f"{position}. **{player['name']}** — {player['score']} point")
else:
    st.sidebar.write("Ingen scores endnu")


if "started" not in st.session_state:
    st.session_state.started = False

if "score" not in st.session_state:
    st.session_state.score = 0

if "start_time" not in st.session_state:
    st.session_state.start_time = None

if "correct_country" not in st.session_state:
    st.session_state.correct_country = None

if "flag_url" not in st.session_state:
    st.session_state.flag_url = None

if "options" not in st.session_state:
    st.session_state.options = []

if "remaining_codes" not in st.session_state:
    st.session_state.remaining_codes = list(countries.keys())
    random.shuffle(st.session_state.remaining_codes)

if "score_saved" not in st.session_state:
    st.session_state.score_saved = False

if "name" not in st.session_state:
    st.session_state.name = ""


if not st.session_state.started:
    name = st.text_input("Skriv dit navn:")

    if st.button("🚀 Start quiz"):
        if name.strip() == "":
            st.warning("Du skal skrive dit navn først.")
        else:
            st.session_state.name = name
            st.session_state.started = True
            st.session_state.score = 0
            st.session_state.score_saved = False
            st.session_state.start_time = time.time()
            st.session_state.remaining_codes = list(countries.keys())
            random.shuffle(st.session_state.remaining_codes)
            new_question()
            st.rerun()


if st.session_state.started:
    elapsed = time.time() - st.session_state.start_time
    time_left = max(0, TIME_LIMIT - elapsed)

    st.subheader(f"Spiller: {st.session_state.name}")
    st.subheader(f"⏱️ Tid tilbage: {int(time_left)} sekunder")
    st.subheader(f"✅ Score: {st.session_state.score}")

    if time_left <= 0:
        end_quiz()

        st.error("Tiden er gået!")
        st.success("Din score er gemt på leaderboardet.")

        if st.button("🔁 Prøv igen"):
            st.session_state.started = False
            st.session_state.score = 0
            st.session_state.start_time = None
            st.session_state.correct_country = None
            st.session_state.flag_url = None
            st.session_state.options = []
            st.session_state.remaining_codes = list(countries.keys())
            random.shuffle(st.session_state.remaining_codes)
            st.session_state.score_saved = False
            st.rerun()

    else:
        st.image(st.session_state.flag_url, width=260)
        st.write("Hvilket land er dette?")

        col1, col2 = st.columns(2)

        for index, option in enumerate(st.session_state.options):
            column = col1 if index < 2 else col2

            with column:
                if st.button(option, key=f"{option}_{st.session_state.correct_country}"):
                    if option == st.session_state.correct_country:
                        st.session_state.score += 1

                    new_question()
                    st.rerun()
