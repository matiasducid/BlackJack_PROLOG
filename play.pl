
:- initialization main.
main :-
  current_prolog_flag(argv,Argv),
  verificarJugador(Argv),
  halt.


borro_elem_lista(_, [], []).
borro_elem_lista(Y, [Y|Xs], Zs):-
  borro_elem_lista(Y, Xs, Zs), !.
borro_elem_lista(X, [Y|Xs], [Y|Zs]):-
  borro_elem_lista(X, Xs, Zs).

/*Parseo la cadena de entrada en 3 subcadenas separadas por espacio*/
parsearArgs(Resto,X,Y,Z):-
  nth0(0,Resto,X),
  nth0(1,Resto,Y),
  nth0(2,Resto,Z).
/*Convierte una cadena en una lista de sus valores ASCII*//*
pasarAASCII([],[]).
pasarAASCII([CCadena|RCadena],[CCadenaListada|L]):-
  string_to_list(CCadena,CCadenaListada),
  pasarAASCII(RCadena,L).*/
/*Verifica si la secuencia de ASCII hacen referencia a un diez*/
esUnDiez(49,48,10).
/*caso recursivo de generar cartas*/
generarCartasAux([],[]).
generarCartasAux([N,P|Resto],[c(Numero,Palo)|RestoCartas]):-
  obtengoChar(N,Numero),
  obtengoChar(P,Palo),
  generarCartasAux(Resto,RestoCartas).
generarCartasAux([N1,N2,P|Resto],[c(10,Palo)|RestoCartas]):-
  esUnDiez(N1,N2,10),
  obtengoChar(P,Palo),
  generarCartasAux(Resto,RestoCartas).
/*dada una cadena genero las cartas que representa*/
generarCartas([],[]).
generarCartas([CArg],[CCartas]):-
  string_to_card(CArg,CCartas).
generarCartas([CCadenaEntrada],ListaCartas):-
  string_to_list(CCadenaEntrada,ListaASCII),
  borro_elem_lista(44,ListaASCII,ListaSinComas),
  generarCartasAux(ListaSinComas,ListaCartas).
/*formarListas recibe una serie de argumentos y te devuelve la mano del jugador,
 del crupier y las cartas vistas en mesa*/
formarListas(Argumentos,ManoJugador,ManoCrupier,CartasVistas):-
  parsearArgs(Argumentos,MJ,MC,CV),
  generarCartas([MJ],ManoJugador),
  generarCartas([MC],ManoCrupier),
  generarCartas([CV],CartasVistas).

/*Elije si es soft o hard el crupier*//*
elejirModoJuego(hard|Resto):-
  formarListas(Resto,_,MC,_),
  mostrarDesicionCrupierHard(MC).
elejirModoJuego(soft|Resto):-
  formarListas(Resto,_,MC,_),
  mostrarDesicionCrupierSoft(MC).
*/
/*verificarJugador verifica si el comando pedido es del crupier o del jugador, luego corre la regla correspondiente*/
verificarJugador([crupier, soft | Resto]):-
  formarListas(Resto,_,MC,_),
  mostrarDesicionCrupierSoft(MC).

verificarJugador([crupier, hard | Resto]):-
  formarListas(Resto,_,MC,_),
  mostrarDesicionCrupierHard(MC).

verificarJugador([jugador | Resto]):-
  formarListas(Resto,ManoJugador,ManoCrupier,CartasVistas),
  playJugador(ManoJugador,ManoCrupier,CartasVistas).

playJugador(ManoJugador,ManoCrupier,CartasVistas):-
  mostrarDesicionJugador(ManoJugador,ManoCrupier,CartasVistas).
  /*Convierte una cadena en número si es que la cadena contiene un nro */
get_number_atom(S, A) :- atom_number(S, A), !.
get_number_atom(S, S).
/*transforma una cadena en su functor de carta correspondiente*/
string_to_card(S, c(Numero,Palo)) :-
  string_to_list(S, [N, P]),
  obtengoChar(N,Numero),
  obtengoChar(P,Palo).
  /*caso especial del 10*/
string_to_card(S, c(Numero,Palo)):-
  string_to_list(S, [49,48,P]),
  obtengoChar(43,Numero),
  obtengoChar(P,Palo).

obtengoChar(43,'10').
obtengoChar(44,',').
obtengoChar(48,'0').
obtengoChar(49,'1').
obtengoChar(50,'2').
obtengoChar(51,'3').
obtengoChar(52,'4').
obtengoChar(53,'5').
obtengoChar(54,'6').
obtengoChar(55,'7').
obtengoChar(56,'8').
obtengoChar(57,'9').
/*Con haber definido solo los valores de los palos y los valores validos teniendo en
cuenta letras y numeros alcanzaba suponiendo que no ingresen un dato invalido.*/
obtengoChar(97,'a').
obtengoChar(98,'b').
obtengoChar(99,'c').
obtengoChar(100,'d').
obtengoChar(101,'e').
obtengoChar(102,'f').
obtengoChar(103,'g').
obtengoChar(104,'h').
obtengoChar(105,'i').
obtengoChar(106,'j').
obtengoChar(107,'k').
obtengoChar(108,'l').
obtengoChar(109,'m').
obtengoChar(110,'n').
obtengoChar(111,'o').
obtengoChar(112,'p').
obtengoChar(113,'q').
obtengoChar(114,'r').
obtengoChar(115,'s').
obtengoChar(116,'t').
obtengoChar(117,'u').
obtengoChar(118,'v').
obtengoChar(119,'w').
obtengoChar(120,'x').
obtengoChar(121,'y').
obtengoChar(122,'z').

/*Valores posibles de las cartas(se puede borrar, no se usa)*/
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
valorCarta(a,11).
valorCarta(a,1).

valorCaracter('2',2).
valorCaracter('3',3).
valorCaracter('4',4).
valorCaracter('5',5).
valorCaracter('6',6).
valorCaracter('7',7).
valorCaracter('8',8).
valorCaracter('9',9).
/*value(carta(Numero,Palo),ValorRetorno)*/
value(c(N,_),Valor):-
  member(N,['2','3','4','5','6','7','8','9']),
  valorCaracter(N,R),
  Valor is R.
value(c(N,_),Valor):-
  member(N,[j,q,k,a]),
  valorCarta(N,Valor).
value(c('10',_),10).
value(c(10,_),10).
/*Suma*//*
suma([], 0).
suma([N| Resto], Suma):-
    suma(Resto, SumaAux),
    Suma is N + SumaAux. */
/*enlistar(Elemento,Lista del elemento) sirve para luego hacer la
 concatenacion y la suma(ARREGLARLO MEJOR SI SE PUEDE).*/
enlistar(VC,[VC]).
/*concatenar(elemento,lista,lista con el elemento)*/
concatenar([E], L, [E|L]):-!.
concatenar([E|Resto], L, [E|ResultadoAux]):-
    concatenar(Resto, L, ResultadoAux).
/*hand(Lista de Cartas, Valor de la mano)*/
hand([],0).
hand([CHand|Resto],TotalMano):-
  value(CHand,AuxTotal),
  hand(Resto,PreTotal),
  TotalMano is AuxTotal+PreTotal.
/*Verdadero si la mano da 21 exacto.*/
twentyone(Hand):-
  hand(Hand,X),
  21 is X,
  format('Tengo veintiuno.\n').
/*Da la longitud de una lista*/
longitud([], 0).
longitud([_|Resto], L):- longitud(Resto, LResto), L is LResto + 1.
		/*blackjack indica si la mano da 21 exacto solo con 2 cartas.*/
blackjack(Hand):-
  longitud(Hand,CantidadCartas),
  CantidadCartas is 2,
  twentyone(Hand),
  writef('Gané, tengo BLACKJACK\n').
/*mayora a 21, devuelve verdadero si te pasaste de 21*/
mayorA21(Hand):-
  hand(Hand,X),!,
  21 < X.
  /*mayorA18(Lista de Cartas)devuelve verdadero si la mano es mayor a 18*/
mayorA18(Hand):-
  hand(Hand,X),
	18 =< X.
/*SECCION DEL CRUPIER*/
/*soft_dealer(Lista de cartas) se queda si la mano es mayor a 17.*/
soft_dealer(Hand):-
  hand(Hand,X),!,
  X>=17 .
/*hard_dealer(Lista de cartas) sigue pidiendo cartas si la mano no es mayor a 17*/
hard_dealer(Hand):-
  hand(Hand,X),!,
  X>=18.

/*regla para correr al crupier*/
mostrarDesicionCrupierHard(Hand):-
  hard_dealer(Hand),!,
  writef('Me planto soy el CRUPIER\n').
mostrarDesicionCrupierHard(_):- writef('Pido otra carta soy el CRUPIER\n').
mostrarDesicionCrupierSoft(Hand):-
  soft_dealer(Hand),!,
  writef('Me planto soy el CRUPIER\n').
mostrarDesicionCrupierSoft(_):- writef('Pido otra carta soy el CRUPIER\n').


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
contar(_,[],0).
contar(X,[X|L],C):- !,contar(X,L,C1), C is C1+1.
contar(X,[Y|L],C):- Y < X, contar(X,L,C1),C is C1+1.
contar(X,[_|L],C):- contar(X,L,C).
/* diferencia (L,K,M), el cual es válido si M es la diferencia de L y K*/
diferencia([],_,[]).
diferencia([A|B],K,M):- member(A,K), diferencia(B,K,M).
diferencia([A|B],K,[A|M]):- not(member(A,K)), diferencia(B,K,M).
/*Genero el Mazo de las cartas que quedan*/
generarMazo(Cards,Mazo):-
	diferencia([c('a',p),c('2',p),c('3',p),c('4',p),c('5',p),c('6',p),
              c('7',p),c('8',p),c('9',p),c('10',p),c('j',p),c('q',p),c('k',p),
              c('a',c),c('2',c),c('3',c),c('4',c),c('5',c),c('6',c),
              c('7',c),c('8',c),c('9',c),c('10',c),c('j',c),c('q',c),c('k',c),
              c('a',d),c('2',d),c('3',d),c('4',d),c('5',d),c('6',d),
              c('7',d),c('8',d),c('9',d),c('10',d),c('j',d),c('q',d),c('k',d),
              c('a',t),c('2',t),c('3',t),c('4',t),c('5',t),c('6',t),
              c('7',t),c('8',t),c('9',t),c('10',t),c('j',t),c('q',t),c('k',t)],
              Cards,Mazo).
/*Cuantas probabilidades tengo*/
probabilidad(CantCartaNecesito,CantCartasMazo,Respuesta):-
  Respuesta is CantCartaNecesito/CantCartasMazo.
/*Obtengo una lista de numeros a partir de una lista de functores.*/
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
  not(mayorA21(Hand)),
  not(blackjack(Hand)),
  not(twentyone(Hand)),
	not(mayorA18(Hand)),
  agregoACards(Hand,Cards,CardsAumentada),
  agregoACards(Crupier,CardsAumentada,CardsFinal),
  generarMazo(CardsFinal,RestoDeMazo),
  calcularCuantoPara21(Hand,Faltante),
  obtenerValores(RestoDeMazo,ValoresEnMazo),
	longitud(ValoresEnMazo,CantidadCartasEnMazo),
	contar(Faltante,ValoresEnMazo,CantCartaQueNecesito),
	probabilidad(CantCartaQueNecesito,CantidadCartasEnMazo,Probabilidad),
  Probabilidad > 0.4 .
/*muestra la desicion tomada por el jugador(seguir pidiendo o plantarse)*/
mostrarDesicionJugador(Hand,Crupier, Cards):-
	pedirOtraJugador(Hand,Crupier,Cards), !,
	writef('PIDO OTRA CARTA SOY EL JUGADOR \n').
mostrarDesicionJugador(_,_,_):- writef("Jugador Se planto\n").
