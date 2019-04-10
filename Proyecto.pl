
:-dynamic materiasCursadas/1.
:-dynamic materiasPorCursar/1.
:-dynamic materiasReprobadas/1.
:-dynamic contador/1.
:-dynamic contador2/1.
:-dynamic materia/4.
:-dynamic s/12.
:-dynamic s1/3.
:-dynamic s2/3.
:-dynamic s3/3.
:-dynamic s4/3.
:-dynamic s5/3.
:-dynamic s6/3.
:-dynamic s7/3.
:-dynamic s8/3.
:-dynamic s9/3.
:-dynamic s10/3.
:-dynamic s11/3.
:-dynamic s12/3.


%materia(nombre,semestreMinimo,creditos,seriada,aprobacion)
materia(diferencial,        1,5,no).
materia(fundProg,   1,5,no).
materia(etica,1,4,no).
materia(mateDiscretas,      1,5,no).
materia(tallerAdm,          1,4,no).

materia(integral,           2,5,diferencial).
materia(poo,                2,5,fundamentrosProg).
materia(contabilidad,       2,4,no).
materia(quimica,            2,4,no).
materia(algebra,            2,5,no).

materia(vectorial,          3,5,integral).
materia(estructuraDeDatos,  3,5,poo).
materia(culturaEmp,         3,4,no).
materia(io,                 3,4,no).
materia(desarrolloSustentable,3,5,no).

materia(ecuaciones,         4,5,vectorial).
materia(metodosNumericos,   4,4,estructuraDeDatos).
materia(topicos,            4,5,estructuraDeDatos).
materia(fundBD,             4,5,no).
materia(simulacion,         4,5,no).

materia(graficacion,        5,4,no).
materia(telecomunicaciones, 5,4,no).
materia(sistemasOp,         5,4,no).
materia(tallerBD,           5,4,fundBD).
materia(fundIngSoft,        5,4,no).

materia(automatas,          6,5,no). 
materia(redes,              6,5,telecomunicaciones).
materia(tallerSistemasOp,   6,4,sistemasOp).
materia(admBD,              6,5,tallerBD).
materia(ingSoft,            6,5,fundIngSoft).

materia(automatas2,         7,5,automatas).
materia(conmutacion,        7,5,redes).
materia(tallerInv,          7,4,no).
materia(gestionProy,        7,6,ingSoft).


materiasPorCursar([fundProg,etica,mateDiscretas,tallerAdm,integral,poo,contabilidad,quimica,algebra,vectorial,estructuraDeDatos,culturaEmp
,io,desarrolloSustentable,ecuaciones,metodosNumericos,topicos,fundBD,simulacion,graficacion,telecomunicaciones,sistemasOp,tallerBD,fundIngSoft,automatas
,redes,tallerSistemasOp,admBD,ingSoft,automatas2,conmutacion,tallerInv,gestionProy,diferencial]).

materiasCursadas([]).

s([1,[]], [2,[]], [3,[]], [4,[]], [5,[]], [6,[]], [7,[]], [8,[]], [9,[]], [10,[]], [11,[]], [12,[]]).
s1(S,C,[]).
s2(S,C,[]).
s3(S,C,[]).
s4(S,C,[]).
s5(S,C,[]).
s6(S,C,[]).
s7(S,C,[]).
s8(S,C,[]).
s9(S,C,[]).
s10(S,C,[]).
s11(S,C,[]).
s12(S,C,[]).

contador(0).            % este es el contador de creditos totales
contador2(0).           % este es el contador de creditos por semestre


creditosMin(20).
creditosMax(36).

%Regla que recibee como parametro el semestre
init(Semestre):-

    contador(X),                                % X sera un contador que declare arriba
    materiasPorCursar([Mat|Col]),               % Mat sera el primer elemento de la lista de materiasPorCursar y Col sera el resto
    materiasCursadas(M),                        % M seran los elementos que esten guardados en materiasCursadas
    materia(Mat,Semestre,Cred,_),               % Busco Mat dentro de la base de conocimiento , si existe y si es del semestre que le pase de parametro, guarda sus creditos
                                                        % en la variable Cred
    Creditos is X + Cred,                       % Aqui Voy sumando el total de creditos que se van acumulando por cada materia
    elimina(Mat,[Mat|Col],R),                   % luego elimino Mat de la lista materiasPorCursar y guardo el resultado en R
    agrega(Mat,M,R1),                           % Agrego la Mat a la lista materiasCursadas y guardo el resultado en R1
        
        %Elimina materia de materias por cursar
        retract(materiasPorCursar(_)), asserta(materiasPorCursar(R)),           % aqui simplemente borro la lista materiasPorCursar y le inserto la nueva lista(R) ya sin el elemento
        %inserta materia a materias cursadas
        retract(materiasCursadas(_)), asserta(materiasCursadas(R1)),            % aqui lo mismo pero con R1

        retract(contador(_)),asserta(contador(Creditos)),                       % actualizo el contador de los creditos
        init(Semestre).                                                       % vuelve a repeterce hasta que ya no encuentre materias con el semestre dado

%Correr esto para probarlo
%captura(1,[diferencial,fundProg,etica,mateDiscretas,tallerAdm]).
%captura(2,[diferencial,fundProg,contabilidad,quimica,algebra,desarrolloSustentable]).
%obtenerSem(1,Creditos,Materias).


captura(Semestre,[]):-              % Con esta regla se insertan los datos
    retract(contador2(_)),asserta(contador2(0)).        % Cuando la lista quede vacia se reinicia el contador
captura(Semestre, [Mat|Col]):-
    contador(X),contador2(Y),                   
    materiasCursadas(M),        
    obtenerSem(Semestre,C,Materia),
    verifica(Mat,Semestre,Creditos),                    % Verifica que la materia exista en la base de conocimiento y sea del semestre correspondiente, y obtengo sus creditos
    Cred is Creditos + X, Cred1 is Creditos + Y,        % Cred son globales y Cred1 son por semestre
    sumaCred(Cred),sumaCred1(Cred1),
    agrega(Mat,M,R1),                                   % Se agrega la cabeza(materia) a materiasCursadas
    retract(materiasCursadas(_)),assert(materiasCursadas(R1)),      % Se remplaza por la nueva lista con la materia agregada
    agrega(Mat,Materia,R),                                  
    remplazar(Semestre,Cred1,R),                        % Se agregan las materias a su respectivo semestre

    captura(Semestre,Col).

verifica(Mat,Semestre,Creditos):-
    materia(Mat,Semestre,Creditos,_);           
    (retract(materia(Mat,_,Creditos,Seriada)),              % Si la materia no corresponde a su semestre, se ajusta  (no optimo)
    asserta(materia(Mat,Semestre,Creditos,Seriada))).       % !! Falta agregar un contador para contarla como reprobada o algo parecido

sumaCred(Cred):-
   retract(contador(_)),asserta(contador(Cred)).
sumaCred1(Cred1):-
   retract(contador2(_)),asserta(contador2(Cred1)).

obtenerSem(Sem,Creditos,Materia):-
    ((Sem = 1,s1(Sem,Cred,Mat),Creditos = Cred, Materia = Mat);
    (Sem = 2,s2(Sem,Cred,Mat),Creditos = Cred, Materia = Mat);
    (Sem = 3,s3(Sem,Cred,Mat),Creditos = Cred, Materia = Mat);
    (Sem = 4,s4(Sem,Cred,Mat),Creditos = Cred, Materia = Mat);
    (Sem = 5,s5(Sem,Cred,Mat),Creditos = Cred, Materia = Mat);
    (Sem = 6,s6(Sem,Cred,Mat),Creditos = Cred, Materia = Mat);
    (Sem = 7,s7(Sem,Cred,Mat),Creditos = Cred, Materia = Mat);
    (Sem = 8,s8(Sem,Cred,Mat),Creditos = Cred, Materia = Mat);
    (Sem = 9,s9(Sem,Cred,Mat),Creditos = Cred, Materia = Mat);
    (Sem = 10,s10(Sem,Cred,Mat),Creditos = Cred, Materia = Mat);
    (Sem = 11,s11(Sem,Cred,Mat),Creditos = Cred, Materia = Mat);
    (Sem = 12,s12(Sem,Cred,Mat),Creditos = Cred, Materia = Mat)).

remplazar(Semestre, Creditos, R):-
    (
        (Semestre = 1,retract(s1(_,_,_)), asserta(s1(Semestre,Creditos,R)) );
        (Semestre = 2,retract(s2(_,_,_)), asserta(s2(Semestre,Creditos,R)) );
        (Semestre = 3,retract(s3(_,_,_)), asserta(s3(Semestre,Creditos,R)) );
        (Semestre = 4,retract(s4(_,_,_)),asserta(s4(Semestre,Creditos,R)) );
        (Semestre = 5,retract(s5(_,_,_)),asserta(s5(Semestre,Creditos,R)) );
        (Semestre = 6,retract(s6(_,_,_)),asserta(s6(Semestre,Creditos,R)) );
        (Semestre = 7,retract(s7(_,_,_)),asserta(s7(Semestre,Creditos,R)) );
        (Semestre = 8,retract(s8(_,_,_)),asserta(s8(Semestre,Creditos,R)) );
        (Semestre = 9,retract(s9(_,_,_)),asserta(s9(Semestre,Creditos,R)) );
        (Semestre = 10,retract(s10(_,_,_)),asserta(s10(Semestre,Creditos,R)) );
        (Semestre = 11,retract(s11(_,_,_)),asserta(s11(Semestre,Creditos,R)) );
        (Semestre = 12,retract(s12(_,_,_)),asserta(s12(Semestre,Creditos,R)) )
).

init3(Semestre, Materias):- 

    random(1,7,Rand),
    contador(X),                                % X sera un contador que declare arriba         % M seran los elementos que esten guardados en materiasCursadas
    materia(Mat,Semestre,Cred,_),               % Busco Mat dentro de la base de conocimiento , si existe y si es del semestre que le pase de parametro, guarda sus creditos
    materiasCursadas(M),                                                     
    Creditos is X + Cred,                       % Aqui Voy sumando el total de creditos que se van acumulando por cada materia
    agrega(Mat,M,R1),                           % Agrego la Mat a la lista materiasCursadas y guardo el resultado en R1
        
        %inserta materia a materias cursadas
        retract(materiasCursadas(_)), asserta(materiasCursadas(R1)),            % aqui lo mismo pero con R1

        retract(contador(_)),asserta(contador(Creditos)) ,                     % actualizo el contador de los creditos
        init(Semestre). 

r1(Semestre):-          % solo para prueba
    materiasPorCursar(Y),
    recursivo(Semestre,Y).

recursivo(Semestre,[Mat|Col]):-  % solo para prueba
    (
        (materia(Mat,Semestre,Cred,_),write(Semestre+Mat));
        (materia(Mat,_,Cred,_), recursivo(Semestre, Col))
    ).
     
resultados(X):- % ignorar esto
    s(C,M),
    write(C+M).

   

elimina( Var, [Var|Cuerpo], Cuerpo).                                        % estos son reglas que hicimos en clase para eliminar, agregar una variable de una lista
elimina( Var, [Var2|Cuerpo], [Var2,Resp]):- elimina(Var,Cuerpo,Resp).

agrega(Var,[],[Var]).
agrega(Var, Lista,[Var|Lista]).

size([],0).                                         % te regresa el tamaño de una lista
size([X|Y], N):-size(Y, N1), N is N1+1.

pertenece(C, [C|_]).                                % te dice si pertenece un elemento a una lista
pertenece(C, [_|R]) :- pertenece(C,R).

concatenar([],L,L).
concatenar([X|M],L,[X|Z]):-concatenar(M,L,Z).
