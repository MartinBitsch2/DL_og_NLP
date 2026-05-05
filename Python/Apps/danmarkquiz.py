# app.py
import streamlit as st

# 1. Opsætning af data med point!
# Vi bruger en 'dictionary' for at knytte point sammen med bynavnet.
# Du kan selvfølgelig altid rette pointene til!
cities_data = {
    "Aarhus": 10, "Aalborg": 10, "Odense": 10, "Esbjerg": 10, 
    "Randers": 20, "Kolding": 20, "Horsens": 20, "Herning": 20, "Silkeborg": 20, "Fredericia": 20, "Roskilde": 20, "Amager": 20, "Lyngby": 20,
    "Holstebro": 30, "Sønderborg": 30, "Køge": 30, "Holbæk": 30, "Slagelse": 30, "Hillerød": 30, "Svendborg": 30, "Næstved": 30,
    "Hjørring": 40, "Frederikshavn": 40, "Haderslev": 40, "Skive": 40, "Viborg": 40, "Frederikssund": 40,
    "Thisted": 50, "Nyborg": 50, "Middelfart": 50, "Skanderborg": 50, "Vallensbæk": 50, "Vordingborg": 50, "Rønne": 50, "Faaborg": 50,
    "Nykøbing Falster": 60, "Billund": 60, "Tønder": 60, "Sorø": 60,
    "Padborg": 80, "Nykøbing Mors": 80, 
    "Sakskøbing": 100
}

# Vi trækker bare bynavnene ud til en liste, vi kan tjekke imod
cities = list(cities_data.keys())
cities_lower = [city.lower() for city in cities]

# Linket til dit Danmarkskort
kort_url = "https://cdn.gfforsikring.dk/gfforsikring/image/upload/c_limit,w_1840/f_webp/q_80/v1/cms/media/xoui2x31/danmark_tegningsomraade_nyt-website_valnoed.png?_a=BAVAZGDY0"


# 2. Vores funktion (def)
def tjek_svar(gaet):
    if gaet in cities_lower:
        rigtig_by = cities[cities_lower.index(gaet)]
        
        if rigtig_by in st.session_state.guessed_cities:
            return "allerede_gættet", rigtig_by, 0
        else:
            # Vi slår op i vores ordbog (cities_data) for at finde ud af, hvor mange point byen er værd
            point_vundet = cities_data[rigtig_by]
            
            # Tilføj til hukommelsen
            st.session_state.guessed_cities.append(rigtig_by)
            st.session_state.score += point_vundet # Læg point til den samlede score
            
            return "korrekt", rigtig_by, point_vundet
    else:
        return "forkert", gaet, 0


# 3. Hukommelse (session_state)
if 'guessed_cities' not in st.session_state:
    st.session_state.guessed_cities = []
# NYT: Vi gemmer også scoren i hukommelsen!
if 'score' not in st.session_state:
    st.session_state.score = 0


# 4. Brugerflade og design
st.title("Gæt byerne på kortet! 🗺️")
st.write(f"Kig på kortet nedenfor og se, om du kan navngive de {len(cities)} markerede byer. Små byer giver flere point!")

# Vis kortet direkte i Streamlit!
st.image(kort_url, caption="Danmarkskort", use_container_width=True)

# Input felt
user_input = st.text_input("Indtast navnet på en by:")

# 5. Kør logikken når brugeren trykker Enter
if user_input:
    rent_gaet = user_input.strip().lower()
    
    # Funktionen returnerer nu tre ting: status, bynavn og point
    status, by_navn, point = tjek_svar(rent_gaet)
    
    if status == "korrekt":
        st.success(f"Korrekt! **{by_navn}** gav dig **{point} point**!")
    elif status == "allerede_gættet":
        st.warning(f"Hov! Du har allerede gættet {by_navn}.")
    elif status == "forkert":
        st.error(f"Desværre, prøv igen!")

# 6. Scorebord
st.divider()
# Vi opdaterer overskriften til at vise både point og antal byer fundet
st.subheader(f"🏆 Din score: {st.session_state.score} point")
st.write(f"Du har fundet {len(st.session_state.guessed_cities)} ud af {len(cities)} byer.")

# Vi viser byerne og deres pointværdi i listen over fundne byer
for city in st.session_state.guessed_cities:
    st.write(f"✅ {city} ({cities_data[city]} pt)")

# Sejrs-fejring
if len(st.session_state.guessed_cities) == len(cities):
    st.balloons()
    st.success(f"Vanvittigt! Du har fundet dem alle og endte med en topscore på {st.session_state.score} point!")
