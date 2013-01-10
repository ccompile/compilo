open Format
open Mips
open Types
open Ast

let frame_size= ref 0
(*Ce qui suit est une énumération de problèmes qui me viennent en tete

Une table est utilisee dans le TP2 pour gérer les variables
globales, nous la modifions légèrement pour pouvoir stocker l'adresse
mémoire où ce situe la variable globale dans le tas(si possible, à
réflechir pour confirmer), 

Autre idée:

On utilise le travail fait au typage sur les variables et
l'environnement: A-ton pour chaque fonction son environnement? Ceci nous
permettrait de simplifier notre tache, L'environnement serait le tableau
d'activation? Un peu brutal mais "correct" il me semble

Les appels de fonction en gérant avec la pile suffisent-t-ils? Relecture
du dernier cours

Une déclaration de fonctions doit etre label: codemipsdefct

une déclaration de struct c'est différent, c'est quelque chose qui doit
etre dans notre compilateur, on doit se souvenir de la manière dont on
implantera en Mips. Ceci est défini dans le sujet. Autrement dit une
déclaration de struct ne génère pas de code mips.

On squizz les déclarations de variables locales, puisque ceci serait fait
au "préprocess" via ce qui est expliqué plus haut avec le tableau
d'activation.


*)

(*TODO: Constructeur INCR qui incrémente un registre d'un octet*)

let compile_expr env fp= function
  |TE_int(a)->[Li32(A0,a);Sw(A0,Areg(4,SP));Arith(Add,SP,SP,Oimm(4))]
  |TE_char(a)->[Li32(A0,Int32.of_char a);
   Sw(A0,Areg(4,SP));Arith(Add,SP,Sp,Oimm(4))]
  |_ -> assert(false)

let compile_instr env = function
  |_->assert(false)

let compile_decl env= function
  |_->assert(false)


