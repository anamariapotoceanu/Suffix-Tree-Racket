# Suffix-Tree-Racket

# Funcții Principale

# ETAPA 1
## `first-branch st`
Primește un arbore de sufixe (ST) și întoarce prima ramură a acestuia (o pereche etichetă-subarbore).

## `other-branches st`
Primește un ST și întoarce ST-ul fără prima sa ramură (o listă de ramuri, așa cum era și ST-ul original).

## `get-branch-label branch`
Primește o ramură a unui ST și întoarce eticheta acesteia.

## `get-branch-subtree branch`
Primește o ramură a unui ST și întoarce subarborele de sub eticheta acesteia.

## `get-ch-branch st ch`
Primește un ST și un caracter `ch` și întoarce ramura a ST-ului a cărei etichetă începe cu caracterul `ch`, sau `false` dacă nu există o asemenea ramură.

## `longest-common-prefix w1 w2`
Primește două cuvinte (liste de caractere) `w1` și `w2` și întoarce lista formată din cel mai lung prefix comun, restul lui `w1` și restul lui `w2`.

## `longest-common-prefix-of-list words`
Primește o listă nevidă de cuvinte care încep cu același caracter și întoarce cel mai lung prefix comun al tuturor cuvintelor din listă.

## `match-pattern-with-label st pattern`
Caută un șablon (subșir) într-un text folosind ST-ul asociat și întoarce rezultatul în funcție de potrivirea găsită.

## `st-has-pattern? st pattern`
Primește un ST și un șablon și întoarce `true` dacă șablonul apare în ST, respectiv `false` dacă nu apare.

# ETAPA 2

## `get-suffixes text`
Primește un text (o listă de caractere cu caracterul special `$` la final) și întoarce toate sufixele textului în ordine descrescătoare a lungimii.

## `get-ch-words words ch`
Primește o listă de cuvinte `words` și un caracter `ch`, întorcând cuvintele care încep cu caracterul `ch`.

## `ast-func suffixes`
Funcție de etichetare care primește o listă de sufixe care încep cu același caracter și calculează o pereche între eticheta ramurii și noile sufixe după eliminarea prefixului.

## `cst-func suffixes`
Funcție de etichetare similară cu `ast-func`, dar eticheta este cel mai lung prefix comun al sufixelor din lista `suffixes`.

## `suffixes->st labeling-func suffixes alphabet`
Primește o funcție de etichetare, o listă de sufixe și un alfabet, întorcând AST-ul sau CST-ul asociat textului în funcție de funcția de etichetare utilizată.

## `text->ast text`
Primește un text și calculează arborele de sufixe atomic asociat acestuia.

## `text->cst text`
Primește un text și calculează arborele de sufixe compact asociat acestuia.

# ETAPA 3

## `substring?`
Primește un text și un șablon și întoarce `true` dacă șablonul apare în text, altfel întoarce `false`.

## `longest-common-substring`
Calculează cel mai lung subșir comun între două texte, construind arborele de sufixe pentru primul text și căutând potriviri în al doilea text.

## `repeated-substring-of-given-length`
Caută un subșir de lungime specificată care se repetă în text și întoarce primul subșir găsit sau `false` dacă nu există o astfel de potrivire.

# ETAPA 4 


Se modifică implementarea arborelui de sufixe pentru a-l construi „leneș”, adică pe măsură ce devine necesar, utilizând fluxuri de ramuri în loc de liste.
