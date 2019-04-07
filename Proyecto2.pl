:-dynamic materiasCursadas/1.
:-dynamic materiasPorCursar/1.
:-dynamic materiasReprobadas/1.
:-dynamic contador/1.
:-dynamic contador2/1.
:-dynamic materia/4.


%materia(nombre,semestreMinimo,creditos,seriada,aprobacion)
materia(diferencial,        1,5,no).
materia(fundamentrosProg,   1,5,no).
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

materiasPorCursar([diferencial,fundamentrosProg,etica,mateDiscretas,tallerAdm,integral,poo,contabilidad,quimica,algebra,vectorial,estructuraDeDatos,culturaEmp
,io,desarrolloSustentable,ecuaciones,metodosNumericos,topicos,fundBD,simulacion,graficacion,telecomunicaciones,sistemasOp,tallerBD,fundIngSoft,automatas
,redes,tallerSistemasOp,admBD,ingSoft,automatas2,conmutacion,tallerInv,gestionProy]).

materiasCursadas([]).
materiasReprobadas([]).

semestre1([]). semestre2([]). semestre3([]). semestre4([]). semestre5([]). semestre6([]).
semestre7([]). semestre8([]). semestre9([]). semestre10([]). semestre11([]). semestre12([]).


contador(0).
contador2(0).

creditosMin(20).
creditosMax(36).

%init(Semestre):-
%    materiasCursadas(M),
%    contador(Creditos),
%    write(Semestre+Creditos+M).
init(Semestre):-
    contador(X),
    materiasReprobadas(MatRep), size(MatRep,Rep),
    random(1,7,Rand),
    write(Rep),
    materiasPorCursar([Mat|Col]),
    materiasCursadas(M),
    materia(Mat,Semestre,Cred,_),
    write(Rand),
    ((
        Rand>1,
        Creditos is X + Cred,  
        elimina(Mat,[Mat|Col],R),
        agrega(Mat,M,R1),
         %Elimina materia de materias por cursar
        retract(materiasPorCursar(_)),
        asserta(materiasPorCursar(R)),
        %inserta materia a materias cursadas
        retract(materiasCursadas(_)),
        asserta(materiasCursadas(R1)),

        retract(contador(_)),
        asserta(contador(Creditos))
       
    );
    (
        Rand=1,
        Sem is Semestre+1,
        %write(Sem),
        write(Mat),
         %Elimina materia de materias por cursar
        retract(materia(Mat,_,C,D)),
        assert(materia(Mat,Sem,C,D)),

        agrega(Mat,MatRep,R3),
        retract(materiasReprobadas(_)),
        assert(materiasReprobadas(R3)),

        elimina(Mat,[Mat|Col],R),
        retract(materiasPorCursar(_)),
        asserta(materiasPorCursar(R))

    )), init(Semestre).

   
        
       

%Solo para test
corrida(X):-
    contador(Creditos),
    contador2(N),
    materiasCursadas(Mat),
    Semestre is N+1,
    (init(Semestre); (retract(contador2(_)), asserta(contador2(Semestre)))),
    write(Semestre+Creditos+Mat),nl,   
    (Creditos<157,Semestre<12),
    corrida(N).
    

elimina( Var, [Var|Cuerpo], Cuerpo).
elimina( Var, [Var2|Cuerpo], [Var2,Resp]):- elimina(Var,Cuerpo,Resp).

agrega(Var,[],[Var]).
agrega(Var, Lista,[Var|Lista]).

size([],0).
size([X|Y], N):-size(Y, N1), N is N1+1.

pertenece(C, [C|_]).
pertenece(C, [_|R]) :- pertenece(C,R).

