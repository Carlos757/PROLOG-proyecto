
:-dynamic materiasCursadas/1.
:-dynamic materiasPorCursar/1.
:-dynamic materiasReprobadas/1.
:-dynamic contador/1.
:-dynamic contador2/1.
:-dynamic materia/4.
:-dynamic s/2.


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


materiasPorCursar([diferencial,fundProg,etica,mateDiscretas,tallerAdm,integral,poo,contabilidad,quimica,algebra,vectorial,estructuraDeDatos,culturaEmp
,io,desarrolloSustentable,ecuaciones,metodosNumericos,topicos,fundBD,simulacion,graficacion,telecomunicaciones,sistemasOp,tallerBD,fundIngSoft,automatas
,redes,tallerSistemasOp,admBD,ingSoft,automatas2,conmutacion,tallerInv,gestionProy]).

materiasCursadas([]).

s(0,[]).

contador(0).            % este es el contador de creditos
contador2(0).


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
        init(Semestre).                                                         % vuelve a repeterce hasta que ya no encuentre materias con el semestre dado

init2(Semestre,[Cab|Cuer]):-    %este es un experimento pero hace lo mismo asi que no lo comentare.
    contador(X),
    materia(Cab,_,Cred,_),
    Creditos is X + Cred,  
    s(C,M),
    materiasPorCursar(Mat),
    elimina(Cab,Mat,R),
    agrega(Cab,M,R1),

    %Elimina materia de materias por cursar
    retract(materiasPorCursar(_)), asserta(materiasPorCursar(R)),
    %inserta materia a materias cursadas
    retract(materiasCursadas(_)), asserta(materiasCursadas(R1)),

    retract(contador(_)),asserta(contador(Creditos)),

    retract(s(_,_)), asserta(s(Creditos,R1)),
    init2(Semestre,Cuer).

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

     