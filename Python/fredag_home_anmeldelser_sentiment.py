from sentida import Sentida

mysentida = Sentida()

teststr = "Her bor min ven Otto"
addMom = "Lone"

# trin 2
totalstr = f"{teststr} og min mor hedder {addMom}"

# trin 3
mynames = list()
mynames.append("Lone")
mynames.append("Lotte")
mynames.append("Mona")
mynames.append("Ib")
mynames.append("Bo")
mynames.append("Viggo")

sentencList = list()
for name in mynames:
    totalstr = f"{teststr} og min mor hedder {name}"
    sentencList.append(totalstr)

parentDict = {}
parentDict['moms'] = sentencList
parentDict['dads'] = sentencList


def myStarter(name, age=21):
    retval = f"Mit navn er {name} og jeg er {age} gammel"
    return retval

svar_liste = []
svar_dict = {}

rundialog = True   
while rundialog:
    
    navn = input("Hvad er dit navn? (Skriv 'quit' for at stoppe): ")
    if navn.lower() == "quit":
        rundialog = False
        continue
        
    svar = input(f"Hej {navn}, hvad synes du om Home? (Skriv 'quit' for at stoppe): ")
    if svar.lower() == "quit":
        rundialog = False
        continue

    sentidascore = mysentida.sentida(svar, output="mean", normal=False)
    
    if sentidascore > 0:
        print("-> Du er glad!")
    elif sentidascore < 0:
        print("-> Du er sur!")
    else:
        print("-> Du er neutral.")
        
    svar_liste.append(svar)
    svar_dict[navn] = svar

print("\n--- Programmet er afsluttet ---")
print("Alle svar i listen:", svar_liste)
print("Svar gemt i dictionary:", svar_dict)