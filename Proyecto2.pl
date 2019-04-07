:-dynamic materiasCursadas/1.
:-dynamic materiasPorCursar/1.
:-dynamic contador/1.

%materia(nombre,semestreMinimo,creditos,seriada,aprobacion)
materia(diferencial,        1,5,no,R):-random(6,10,R).
materia(fundamentrosProg,   1,5,no,R):-random(6,10,R).
materia(etica,              1,4,no,R):-random(6,10,R).
materia(mateDiscretas,      1,5,no,R):-random(6,10,R).
materia(tallerAdm,          1,4,no,R):-random(6,10,R).

materia(integral,           2,5,diferencial,R):-random(6,10,R).
materia(poo,                2,5,fundamentrosProg,R):-random(6,10,R).
materia(contabilidad,       2,4,no,R):-random(6,10,R).
materia(quimica,            2,4,no,R):-random(6,10,R).
materia(algebra,            2,5,no,R):-random(6,10,R).

materia(vectorial,          3,5,integral,R):-random(6,10,R).
materia(estructuraDeDatos,  3,5,poo,R):-random(6,10,R).
materia(culturaEmp,         3,4,no,R):-random(6,10,R).
materia(io,                 3,4,no,R):-random(6,10,R).
materia(desarrolloSustentable,3,5,no,R):-random(6,10,R).

materia(ecuaciones,         4,5,vectorial,R):-random(6,10,R).
materia(metodosNumericos,   4,4,estructuraDeDatos,R):-random(6,10,R).
materia(topicos,            4,5,estructuraDeDatos,R):-random(6,10,R).
materia(fundBD,             4,5,no,R):-random(6,10,R).
materia(simulacion,         4,5,no,R):-random(6,10,R).

materia(graficacion,        5,4,no,R):-random(6,10,R).
materia(telecomunicaciones, 5,4,no,R):-random(6,10,R).
materia(sistemasOp,         5,4,no,R):-random(6,10,R).
materia(tallerBD,           5,4,fundBD,R):-random(6,10,R).
materia(fundIngSoft,        5,4,no,R):-random(6,10,R).

materia(automatas,          6,5,no,R):-random(6,10,R). 
materia(redes,              6,5,telecomunicaciones,R):-random(6,10,R).
materia(tallerSistemasOp,   6,4,sistemasOp,R):-random(6,10,R).
materia(admBD,              6,5,tallerBD,R):-random(6,10,R).
materia(ingSoft,            6,5,fundIngSoft,R):-random(6,10,R).

materia(automatas2,         7,5,automatas,R):-random(6,10,R).
materia(conmutacion,        7,5,redes,R):-random(6,10,R).
materia(tallerInv,          7,4,no,R):-random(6,10,R).
materia(gestionProy,        7,6,ingSoft,R):-random(6,10,R).

materiasPorCursar([diferencial,fundamentrosProg,etica,mateDiscretas,tallerAdm,integral,poo,contabilidad,quimica,algebra,vectorial,estructuraDeDatos,culturaEmp
,io,desarrolloSustentable,ecuaciones,metodosNumericos,topicos,fundBD,simulacion,graficacion,telecomunicaciones,sistemasOp,tallerBD,fundIngSoft,automatas
,redes,tallerSistemasOp,admBD,ingSoft,automatas2,conmutacion,tallerInv,gestionProy]).

materiasCursadas([]).
contador(0).

creditosMin(20).
creditosMax(36).


init(Semestre):-
    contador(X),
    materiasPorCursar([Mat|Col]),
    materiasCursadas(M),
    materia(Mat,Semestre,Cred,_,_),
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
        asserta(contador(Creditos)),
        write(Creditos).


   

elimina( Var, [Var|Cuerpo], Cuerpo).
elimina( Var, [Var2|Cuerpo], [Var2,Resp]):- elimina(Var,Cuerpo,Resp).

agrega(Var,[],[Var]).
agrega(Var, Lista,[Var|Lista]).

size([],0).
size([X|Y], N):-size(Y, N1), N is N1+1.

pertenece(C, [C|_]).
pertenece(C, [_|R]) :- pertenece(C,R).

     