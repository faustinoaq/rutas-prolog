% Proyecto Semestral de Programación 4
% Faustino Aguilar
% Kevin Vázques
% CRUV FIEC 2016
% % % % % % % % %
% Ejemplos
% ?- mejor_ruta(pa1, pb3, R, T).
% ?- mejor_ruta(pb6, pa2, R, T).
% Listado de las paradas y sus datos

% Ruta A de Canto del Llano-Mercado
parada(pa1, "Canto del Llano-Mercado", "Centro Salud").
parada(pa2, "Canto del Llano-Mercado", "CRUV Puente").
parada(pa3, "Canto del Llano-Mercado", "Universidad Oteima").
parada(pc1, "Canto del Llano-Mercado", "IPT Veraguas").
parada(pc2, "Canto del Llano-Mercado", "Terminal").
parada(pc3, "Canto del Llano-Mercado", "Policlínica").
parada(pc4, "Canto del Llano-Mercado", "Escuela Normal").
parada(pa4, "Canto del Llano-Mercado", "Mercado").
parada(pa5, "Canto del Llano-Mercado", "Escuela Normal").

% Ruta B de Forestal-Mercado
parada(pb1, "Forestal-Mercado", "INADEH").
parada(pb2, "Forestal-Mercado", "Calle 18 Norte").
parada(pb3, "Forestal-Mercado", "CRUV Forestal").
parada(pb4, "Forestal-Mercado", "Escuela San Martin").
parada(pb5, "Forestal-Mercado", "Cancha San Martin").
parada(pb6, "Forestal-Mercado", "Parada Delicias").
parada(pc1, "Forestal-Mercado", "IPT Veraguas").
parada(pc2, "Forestal-Mercado", "Terminal").
parada(pc3, "Forestal-Mercado", "Policlinica").
parada(pc4, "Forestal-Mercado", "Escuela Normal").
parada(pb7, "Forestal-Mercado", "Mercado").
parada(pb8, "Forestal-Mercado", "Escuela Normal").

% Interconexiones de Rutas A y B
parada(pi1, "Interconexión A-B", "CRUV").
parada(pi2, "Interconexión A-B", "Mercado").
parada(pi3, "Interconexión A-B", "Escuela Normal").

% Ruta A
conexion(pa1, pa2, 3).
conexion(pa2, pa3, 0.5).
conexion(pa3, pc1, 0.67).
conexion(pc1, pc2, 3).
conexion(pc2, pc3, 4).
conexion(pc3, pc4, 0.8).
conexion(pc4, pa4, 1.5).
conexion(pa4, pa5, 2).
conexion(pa5, pc2, 2).

% Ruta B
conexion(pb1, pb2, 0.5).
conexion(pb2, pb3, 1).
conexion(pb3, pb4, 2).
conexion(pb4, pb5, 0.5).
conexion(pb5, pb6, 0.67).
conexion(pb6, pc1, 0.5).
conexion(pc1, pc2, 3).
conexion(pc2, pc3, 4).
conexion(pc3, pc4, 0.8).
conexion(pc4, pb7, 1).
conexion(pb7, pb8, 2).
conexion(pb8, pc4, 0.5).

% Interconexiones
conexion(pa1, pi1, 3).
conexion(pb2, pi1, 1).
conexion(pi1, pb4, 2).
conexion(pc4, pi2, 1.25).
conexion(pi3, pi2, 2).
conexion(pi3, pc4, 1).
conexion(pi3, pc2, 2.5).

% Permite evaluar las paradas en dos sentidos
conectados(X, Y, T) :-
  conexion(X, Y, T).
conectados(X, Y, T) :-
  conexion(Y, X, T).

% Menú de la Aplicación
inicio :-
  writeln("\n\tRutas de Santiago\n"),
  writeln("\n\tPara saber el menor tiempo escriba:"),
  writeln("\tmejor_ruta(pxx, pyy, R, T)."),
  writeln("\tpxx = Parada de inicio"),
  writeln("\tpyy = Parada de fin"),
  writeln("\tPara ver las paradas escriba: ayuda.").

% Comando informativo
ayuda :-
  writeln("Parada\tRuta\t\t\tInformación"),
  parada(X, Y, Z),
  write(X),
  write("\t"),
  write(Y),
  write("\t"),
  writeln(Z),
  fail,
  nl.

% Comprueba si las paradas existen
existen(X, Y) :-
  parada(X,_,_),
  parada(Y,_,_).

% Imprime los datos de la mejor ruta
imprimir_datos([]).
imprimir_datos([A|B]) :-
  parada(A, R, D),
  write(A),
  write("\t"),
  write(R),
  write("\t"),
  writeln(D),
  imprimir_datos(B).

% Imprime los tiempos de la mejor Ruta
imprimir_tiempos([_]).
imprimir_tiempos([A,B|C]) :-
  conectados(A, B, T),
  write("("),
  write(A),
  write(", "),
  write(B),
  write(") = "),
  write(T),
  writeln(" s"),
  append([B], C, L),
  % write(L),
  imprimir_tiempos(L).

% Función principal para calcular la mejor ruta
mejor_ruta(X, Y, R, T) :-
  X \== Y,
  existen(X, Y),
  mejor(X, Y, R, T),
  writeln("\n\t=========== Mejor Ruta ==========="),
  writeln("\nParada\tRuta\t\t\tInformación"),
  imprimir_datos(R),
  writeln("\n\tTiempos:"),
  imprimir_tiempos(R).

% Evalua el recorrido de las paradas
recorrido(A, B, Ruta, Tiempo) :-
  opciones(A, B, [A], Q, Tiempo),
  reverse(Q, Ruta).

% Obtiene todas las rutas posibles
opciones(A, B, P, [B|P], T) :-
  conectados(A, B, T).
opciones(A, B, Marcados, Ruta, T) :-
  conectados(A, C, D),
  C \== B,
  not(member(C, Marcados)),
  opciones(C, B, [C|Marcados], Ruta, T1),
  T is D + T1.

%  Imprime las todas las rutas
imprimir_all_rutas([]).
imprimir_all_rutas([[A,T]|B]) :-
  writeln("\nParada\tRuta\t\t\tInformación"),
  imprimir_datos(A),
  imprimir_tiempos(A),
  write("(pxx, pyy) = "),
  write(T),
  writeln("s <- Total"),
  imprimir_all_rutas(B).

% Sub regla de mejor_ruta se utiliza para calcular la ruta con menor tiempo
mejor(A, B, Ruta, Tiempo) :-
   setof([P, T], recorrido(A, B, P, T), Conjunto),
   Conjunto = [_|_], % Falla si no hay ruta
   imprimir_all_rutas(Conjunto),
   minimo(Conjunto, [Ruta,Tiempo]).

% Calcula el minimo tiempo dada las rutas y sus respectivos tiempos
minimo([F|R], M) :-
  min(R, F, M).
% minimo recorrido
min([], M, M).
min([[P,L]|R], [_,M], Min) :-
  L < M,
  !,
  min(R, [P,L], Min).
min([_|R], M, Min) :-
  min(R, M, Min).
