# Decision Trees
Aquest document descriu Decision Trees, la pràctica de Haskell de Llenguatges de programació (edició tardor 2020). 
## Descripció
Aquesta pràctica tenia l'objectiu didàctic de practicar diverses estructures de dades i funcions d'ordre superior.
En concret: construcció d'arbres, navegació d'arbres, monades IO i de control, heurístiques per a arbres decisionals.
I en general practicar projectes en haskell.
## Taula de continguts
- **dts.hs**: Codi del projecte.
- **agaricus-lepiota.data**: Dades emprades per la pràctica.
- ## Resum d'algunes funcions importants dintre de _dts.hs_:
- **buildTree** : Espera la matriu de dades i una llista de ints buits. El paràmetre banned impedeix que es faci "split" a un atribut ja visitat.
- **printPreorder** : Print del arbre en preordre, espera una llista buida (que es la tabulacio inicial dels string a imprimir) i l'arbre a imprimir.
- **navigate** : Donat un arbre (o subarbre) i una "elecció" d'on es vol anar, retorna erro si no es pot anar on es desitja, edible, poisonus, o no_entry. El retorn inclou una tupla amb l'arbre resultant de la navegació.
- **bestScore** : Donada una matriu i la llista d'atributs ja visitats, troba un atribut vàlid que maximitza l'heurística emprada. Se suporta de scoreAtribute, que al mateix temps se suporta de scoreLabel, per anar un per un fent la puntuació de cada atribut vàlid (candidatos).
- **fragment** : Donada una matriu i un atribut per el qual fragmentar, reparteix les entrades (tuples de dades) de la matriu en llistes d'entrades. Cada llista és un fragment de la matriu, que correspon a un possible valor que pot agafar l'atribut per el qual partim. Pot ser que certs valors no tinguin cap dada (en aquest cas donarà no_entries la constructora de l'arbre). Finalment, aclarir que aquesta llista de llistes té els fragments ordenats alfabèticament, per concordar amb la resta d'estructures de dades.
- **selectBranch** : Donat un arbre i el Maybe Int, retorna si pot el resultat de navegar al i-essim label d'aquell atribut.

Ex: donat l'atribut "odor" que pot ser ["none","foul","musty"]
Si volem navegar a foul, selectBranch del node odor amb Just 1.
- **modaAbsoluta** : Si totes les entrades són edible o poisonus, retorna (True, moda), amb un fold lazy que crec que és més eficient que la moda (següent funció).
- **moda** : Calcula el valor real en % d'entrades edible i poisonus, retorna la string corresponent.
- **decodeAtr** : Donat l'índex d'un atribut, retorna el string que l'identifica.
- **llistaLabelsAbreviats** : És una matriu de strings amb els valors que pot adquirir el i-essim atribut (tots, fins i tot els que no tenen entrades a la matriu de dades).
- **listLabel** : Donat un atribut, retorna una llista amb els strings dels possibles valors que pot adquirir.
## Usage
Des de la terminal
```bash
ghci dts.hs
```
Des de el GHCI
```haskell
:load dts.hs
main
```
## Millores
Queden per millorar molts d'aspectes d'eficiència, en particular tinc llistades:
- Millorar el "filtrat" de la matriu, substituint els filter per un classificador, que en una sola passada segregui les entrades de la matriu.
- Millorar el procés de puntuar els atributs creant tuples per tots els atributs d'una entrada de la matriu (i generar tuples d'entrada en entrada), recorrent la matriu horitzontalment i no verticalment.
- Fer més clar i net el codi :(
## Contribucions
No espero cap contribució, pero son totes benvingudes
## Links
- Gerard Escudero, 2020.  [Machine Learning](https://gebakx.github.io/ml/)
- Jeff Schlimmer, 1981. [Mushroom Data Set](https://archive.ics.uci.edu/ml/datasets/Mushroom)
