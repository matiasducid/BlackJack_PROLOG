/*cartas posibles*/
card(a,c).
card(2,c).
card(3,c).
card(4,c).
card(5,c).
card(6,c).
card(7,c).
card(8,c).
card(9,c).
card(10,c).
card(j,c).
card(q,c).
card(k,c).

card(a,p).
card(2,p).
card(3,p).
card(4,p).
card(5,p).
card(6,p).
card(7,p).
card(8,p).
card(9,p).
card(10,p).
card(j,p).
card(q,p).
card(k,p).

card(a,t).
card(2,t).
card(3,t).
card(4,t).
card(5,t).
card(6,t).
card(7,t).
card(8,t).
card(9,t).
card(10,t).
card(j,t).
card(q,t).
card(k,t).

card(a,d).
card(2,d).
card(3,d).
card(4,d).
card(5,d).
card(6,d).
card(7,d).
card(8,d).
card(9,d).
card(10,d).
card(j,d).
card(q,d).
card(k,d).


/*Valores posibles de las cartas*/
valorCarta(2,2).
valorCarta(3,3).
valorCarta(4,4).
valorCarta(5,5).
valorCarta(6,6).
valorCarta(7,7).
valorCarta(8,8).
valorCarta(9,9).
valorCarta(10,10).
valorCarta(j,10).
valorCarta(q,10).
valorCarta(k,10).
valorCarta(a,1).
valorCarta(a,11).

/*value(carta(Numero,Palo),ValorRetorno)*/
value(c(N,_),Valor):-
  valorCarta(N,Valor).

/*Suma*/
suma([], 0).
suma([N| Resto], Suma):-
    suma(Resto, SumaAux),
    Suma is N + SumaAux.

/*enlistar(Elemento,Lista del elemento) sirve para luego hacer la concatenacion y la suma(ARREGLARLO MEJOR SI SE PUEDE).*/
enlistar(VC,[VC]).

/*concatenar(elemento,lista,lista con el elemento)*/
concatenar([E], L, [E|L]):-!.
concatenar([E|Resto], L, [E|ResultadoAux]):-
    concatenar(Resto, L, ResultadoAux).

/*hand(Lista de Cartas, Valor de la mano)*/
hand([],0).
hand([Hand|RestoHand],ValorManoTotal):-
  hand(RestoHand,ValorManoAux),
  value(Hand,ValorCarta),
  enlistar(ValorCarta,Lista),
  concatenar(Lista,[],ListaTotal),
  suma(ListaTotal,ValorMano),
  ValorManoTotal is ValorMano + ValorManoAux.

/*Verdadero si la mano da 21 exacto.*/
twentyone(Hand):-
  hand(Hand,X),
  21 is X.

/*Verdadero si la mano se pasa de 21.*/
over(Hand):-
  hand(Hand,X),
  21 < X.

/*Da la longitud de una lista*/
longitud([], 0).
longitud([_|Resto], L):- longitud(Resto, LResto), L is LResto + 1.

/*blackjack indica si la mano da 21 exacto solo con 2 cartas.*/
blackjack(Hand):-
    longitud(Hand,CantidadCartas),
    CantidadCartas is 2,
    twentyone(Hand).
/*menorA17(Lista de cartas)devuelve verdadero si la mano es menor a 17*/
menorA17(Hand):-
  hand(Hand,X),
  17 > X.
/*mayora17(Lista de Cartas)devuelve verdadero si la mano es mayor a 17*/
mayorA17(Hand):-
  hand(Hand,X),
  17 =< X.

/*SECCION DEL CRUPIER*/

/*soft_dealer(Lista de cartas) se queda si la mano es mayor a 17.*/
soft_dealer(Hand):-
  menorA17(Hand).
/*hard_dealer(Lista de cartas) sigue pidiendo cartas si la mano es mayor o igual a 17*/
hard_dealer(Hand):-
  mayorA17(Hand).

/*pedirOtraCrupier(Mano del crupier)
define si el crupier pide otra carta o se queda con lo que tiene*/
pedirOtraCrupier(Hand):-
  hard_dealer(Hand).

mostrarDesicionCrupier(Hand):-
  pedirOtraCrupier(Hand),!,
  writef('Me planto soy el CRUPIER').
mostrarDesicionCrupier(_):- writef('Pido otra carta soy el CRUPIER').


/*SECCION DEL JUGADOR*/
/*agregoACards(Lista de la mano, Lista de cartas que salieron)*/
agregoACards([],Cards,Cards).
agregoACards(Hand,Cards,Aux):-
  concatenar(Hand,Cards,Aux).
/*Me dice CUANTO me falta para llegar a 21.*/
calcularCuantoPara21([],21).
calcularCuantoPara21(Hand,ValorFaltante):-
  hand(Hand,ValorMano),
  ValorFaltante is 21 - ValorMano.

/*contar cuenta las ocurrencias de la carta que necesito o menores.*/
contar(X,[],0).
contar(X,[X|L],C):- !,contar(X,L,C1), C is C1+1.
contar(X,[Y|L],C):- Y < X, contar(X,L,C1),C is C1+1.
contar(X,[Y|L],C):- contar(X,L,C).

/* diferencia (L,K,M), el cual es válido si M es la diferencia de L y K*/
diferencia([],_,[]).
diferencia([A|B],K,M):- member(A,K), diferencia(B,K,M).
diferencia([A|B],K,[A|M]):- not(member(A,K)), diferencia(B,K,M).
/*Genero el Mazo de las cartas que quedan*/
generarMazo(Cards,Mazo):-
  diferencia([c(a,p),c(2,p),c(3,p),c(4,p),c(5,p),c(6,p),c(7,p),c(8,p),c(9,p),c(10,p),c(j,p),c(q,p),c(k,p),c(a,c),c(2,c),c(3,c),c(4,c),c(5,c),c(6,c),c(7,c),c(8,c),c(9,c),c(10,c),c(j,c),c(q,c),c(k,c),c(a,d),c(2,d),c(3,d),c(4,d),c(5,d),c(6,d),c(7,d),c(8,d),c(9,d),c(10,d),c(j,d),c(q,d),c(k,d),c(a,t),c(2,t),c(3,t),c(4,t),c(5,t),c(6,t),c(7,t),c(8,t),c(9,t),c(10,t),c(j,t),c(q,t),c(k,t)],Cards,Mazo).
/*Cuantas probabilidades tengo*/

probabilidad(CantCartaNecesito,CantCartasMazo,Respuesta):-
  Respuesta is CantCartaNecesito/CantCartasMazo.
/*Obtengo una lista de numeros a partir de una lista de functores.
Aprovecho el backtracking al no usar cut para obtener todos los valores de as.
ObtenerValores(ListaFuntores,ListaValores)*/
obtenerValores([],[]).
obtenerValores([CMazo|RMazo],ListaTotal):-
  value(CMazo,V),
  enlistar(V,ListaAux),
  concatenar(ListaAux,[],ListaPreFinal),
  obtenerValoresAux(RMazo,ListaPreFinal,ListaTotal).

obtenerValoresAux([],L,L):-!.
obtenerValoresAux([CMazo|RMazo],ListaTotal,Z):-
  value(CMazo,X),
  enlistar(X,ListaSegunda),
  concatenar(ListaSegunda,ListaTotal,ListaAux),
  obtenerValoresAux(RMazo,ListaAux,Z).
/*Decide el JUGADOR si pedir otra o no.*/
pedirOtraJugador(Hand,Crupier,Cards):-
  agregoACards(Hand,Cards,CardsAumentada),
  agregoACards(Crupier,CardsAumentada,CardsFinal),
  generarMazo(CardsFinal,RestoDeMazo),
  obtenerValores(RestoDeMazo,ValoresEnMazo),
  calcularCuantoPara21(Hand,Faltante),
  longitud(ValoresEnMazo,CantidadCartasEnMazo),
  contar(Faltante,ValoresEnMazo,CantCartaQueNecesito),
  probabilidad(CantCartaQueNecesito,CantidadCartasEnMazo,Probabilidad),
  Probabilidad > 0.7.



mostrarDesicion(Hand,Crupier, Cards):-
  pedirOtraJugador(Hand,Crupier,Cards), !,
  writef('PIDO OTRA SOY EL JUGADOR\n',).
mostrarDesicion(_,_,_):- writef("Jugador Se planto").
/*PONER EL PRINT NECESARIO*/
