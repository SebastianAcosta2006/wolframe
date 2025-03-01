# wolframe
(* algoritmo del factorial *)
Factorial[n_] := If[n == 0, 1, n * Factorial[n - 1]]

(* saber si un numero es primo *)
EsPrimoQ[n_] := PrimeQ[n]

(* Agoritmo de euclides para el MCD *)
MCD[a_, b_] := If[b == 0, a, MCD[b, Mod[a, b]]]

(* Algoritmo de Bisección *)
Biseccion[f_, a_, b_, tol_] := Module[{c},
  If[f[a] f[b] > 0, Return["No hay raiz en el intervalo"],
    While[Abs[b - a] > tol,
      c = (a + b)/2;
      If[f[c] == 0, Return[c]];
      If[f[a] f[c] < 0, b = c, a = c];
    ];
    Return[(a + b)/2];
  ]
]

(* Polinomio de Taylor para Cos(x) *)
TaylorCos[x_, n_] := Sum[(-1)^k * x^(2 k) / Factorial[2 k], {k, 0, n}]
