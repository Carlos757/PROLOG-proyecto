
:-dynamic materiasCursadas/1.
:-dynamic materiasPorCursar/1.
:-dynamic materiasReprobadas/1.
:-dynamic contador/1.
:-dynamic contador2/1.
:-dynamic materia/5.
:-dynamic s/1.
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
materia(diferencial,        1,5,no,0).
materia(fundProg,   1,5,no,0).
materia(etica,1,4,no,0).
materia(mateDiscretas,      1,5,no,0).
materia(tallerAdm,          1,4,no,0).

materia(integral,           2,5,diferencial,0).
materia(poo,                2,5,fundProg,0).
materia(contabilidad,       2,4,no,0).
materia(quimica,            2,4,no,0).
materia(algebra,            2,5,no,0).

materia(vectorial,          3,5,integral,0).
materia(estructuraDeDatos,  3,5,poo,0).
materia(culturaEmp,         3,4,no,0).
materia(io,                 3,4,no,0).
materia(desarrolloSustentable,3,5,no,0).

materia(ecuaciones,         4,5,vectorial,0).
materia(metodosNumericos,   4,4,estructuraDeDatos,0).
materia(topicos,            4,5,estructuraDeDatos,0).
materia(fundBD,             4,5,no,0).
materia(simulacion,         4,5,no,0).

materia(graficacion,        5,4,no,0).
materia(telecomunicaciones, 5,4,no,0).
materia(sistemasOp,         5,4,no,0).
materia(tallerBD,           5,4,fundBD,0).
materia(fundIngSoft,        5,4,no,0).

materia(automatas,          6,5,no,0). 
materia(redes,              6,5,telecomunicaciones,0).
materia(tallerSistemasOp,   6,4,sistemasOp,0).
materia(admBD,              6,5,tallerBD,0).
materia(ingSoft,            6,5,fundIngSoft,0).

materia(automatas2,         7,5,automatas,0).
materia(conmutacion,        7,5,redes,0).
materia(tallerInv,          7,4,no,0).
materia(gestionProy,        7,6,ingSoft,0).


materiasPorCursar([diferencial,fundProg,etica,mateDiscretas,tallerAdm,integral,poo,contabilidad,quimica,algebra,vectorial,estructuraDeDatos,culturaEmp
,io,desarrolloSustentable,ecuaciones,metodosNumericos,topicos,fundBD,simulacion,graficacion,telecomunicaciones,sistemasOp,tallerBD,fundIngSoft,automatas
,redes,tallerSistemasOp,admBD,ingSoft,automatas2,conmutacion,tallerInv,gestionProy]).

materiasCursadas([]).

s([]).
s1(1,0,[]).
s2(2,0,[]).
s3(3,0,[]).
s4(4,0,[]).
s5(5,0,[]).
s6(6,0,[]).
s7(7,0,[]).
s8(8,0,[]).
s9(9,0,[]).
s10(10,0,[]).
s11(11,0,[]).
s12(12,0,[]).

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
        init(Semestre).                                                       % vuelve a repeterce hasta que ya no,0 encuentre materias con el semestre dado

%Correr esto para probarlo
%captura(1,[diferencial,fundProg,etica,mateDiscretas,tallerAdm]).
%captura(2,[diferencial,fundProg,contabilidad,quimica,algebra,desarrolloSustentable]).
%obtenerSem(1,Creditos,Materias).


captura(Semestre,[]):-              % Con esta regla se insertan los datos
    retract(contador2(_)),asserta(contador2(0)),nl,        % Cuando la lista quede vacia se reinicia el contador
    Sem1 is Semestre+1,
    write('Continuar? '), read(Fin),
    ( 
        Fin='si',
        ( write('Ok') )
        ; 
        (   
            semestreAux([]),                %Se limpia el semestreAux
            write('Generando carga'),nl,
            ajustaMat(1),
            materiasPorCursar(List),
            consegirMat(Sem1,List,R),
            s(Res),
            captura(Sem1,Res)
            %llenar(Sem1,R)
        ) 
    ).

captura(Semestre, [Mat|Col]):-
    contador(X),contador2(Y),                   
    materiasCursadas(M),        
    obtenerSem(Semestre,C,Materia),
    verifica(Mat,Semestre,Creditos),                    % Verifica que la materia exista en la base de conocimiento y sea del semestre correspondiente, y obtengo sus creditos
    verifica1(Mat,M,V),                                 % Si la materia ya existe en cursadas V = 1 y si no V = 0
    Cred is Creditos + X, Cred1 is Creditos + Y,        % Cred son globales y Cred1 son por semestre
    sumaCred(Cred),sumaCred1(Cred1),
    
    (
        V = 1,( 
            agrega(Mat,Materia,R),                                  
            remplazar(Semestre,Cred1,R),
            captura(Semestre,Col) 
        );
        V = 0,(
            agrega(Mat,M,R1),                                   % Se agrega la cabeza(materia) a materiasCursadas
            retract(materiasCursadas(_)),assert(materiasCursadas(R1)),        % Se remplaza por la nueva lista con la materia agregada
            agrega(Mat,Materia,R),                                  
            remplazar(Semestre,Cred1,R),
            captura(Semestre,Col)   
        )
    ).
%
llenar(Semestre,[Mat|Col]):-
    contador(X),contador2(Y),                   
    materiasCursadas(M), materiasPorCursar([Mat1|Col1]),
    obtenerSem(Semestre,C,Materia).
    %verifica(Mat1,Semestre,Creditos).   

ajustaMat(X):-
    materiasCursadas(M),materiasPorCursar(N),  
    subtract(N,M,Res),
    retract(materiasPorCursar(_)),assert(materiasPorCursar(Res)).


consegirMat(Semestre,[Mat|Col],R):- 
    contador2(Y),
    (
        Y < 36,s(P),
        (
            materiasCursadas(M), materiasPorCursar(N),
            materia(Mat,Sem,Cred,Seriada,_),
            (
                %Semestre>Sem,( verifica(Mat,Semestre,Creditos) )
                Sem =< Semestre,
                ( 
                    agrega(Mat,R,R1), Y1 is Y + Cred , sumaCred1(Y1), consegirMat(Semestre,Col,R1),  write(aqui1),nl
                );
                member(Seriada,M),
                (
                    agrega(Mat,R,R1), Y1 is Y + Cred , sumaCred1(Y1), consegirMat(Semestre,Col,R1), write(aqui2),nl
                );write('No hay mas materias aptas para cargarse'),nl,semestreAux(R),retract(contador2(_)),asserta(contador2(0)) 
            )
        );write('Se alcanzo el limite de creditos'+Y),nl
        
    ).

semestreAux(R):-
    retract(s(_)),assert(s(R)).

verifica(Mat,Semestre,Creditos):-
    materia(Mat,Semestre,Creditos,Seriada,Rep);           
    (    
        retract(materia(Mat,_,Creditos,Seriada,Rep)),              % Si la materia no corresponde a su semestre, se ajusta  (no optimo)
        asserta(materia(Mat,Semestre,Creditos,Seriada,Rep))
    ).       % !! Falta agregar un contador para contarla como reprobada o algo parecido

verifica1(Mat,M,V):-
    materia(Mat,Semestre,Creditos,_,Rep),
    (
        member(Mat,M),  
        Rep1 is Rep +1, 
        retract(materia(Mat,Semestre,Creditos,Seriada,_)),       %Si la materia ya existe se aumenta las veces reprobadas
        asserta(materia(Mat,Semestre,Creditos,Seriada,Rep1)),
        V is 1,write(reprobo+Mat)
       
    );V is 0.
    
test(X):-
    materiasCursadas(M),
    member(diferencial,M).

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
ver(X):-
    s1(S1,C1,M1),s2(S2,C2,M2), s3(S3,C3,M3),s4(S4,C4,M4),s5(S5,C5,M5),
    s6(S6,C6,M6),s7(S7,C7,M7),s8(S8,C8,M8),s9(S9,C9,M9),s10(S10,C10,M10),
    s11(S11,C11,M11),s12(S12,C12,M12),contador(Y),nl,
    write('Semestre 1 = Creditos':C1:M1),nl,
    write('Semestre 2 = Creditos':C2:M2),nl,
    write('Semestre 3 = Creditos':C3:M3),nl,
    write('Semestre 4 = Creditos':C4:M4),nl,
    write('Semestre 5 = Creditos':C5:M5),nl,
    write('Semestre 6 = Creditos':C6:M6),nl,
    write('Semestre 7 = Creditos':C7:M7),nl,
    write('Semestre 8 = Creditos':C8:M8),nl,
    write('Semestre 9 = Creditos':C9:M9),nl,
    write('Semestre 10 = Creditos':C10:M10),nl,
    write('Semestre 11 = Creditos':C11:M11),nl,
    write('Semestre 12 = Creditos':C12:M12),nl,
    write('Creditos globales':Y).

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

tam(Y):- materiasCursadas(X),size(X,Y).
