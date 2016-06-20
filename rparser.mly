/****************************************************************************)
 -*- mode: Tuareg;-*-                                                     
 Filename:    parser.mly                                                  
 Authors:     lgm                                                         
 Creation:    Mon Mar  7 11:48:15 2005                                    
 Copyright:   Biosimilarity LLC 2004 - 2006. All rights reserved.        
              See LICENSE.BIOSIM in the license directory.                
 Description:                                                             
****************************************************************************/

%{ 

open Rho;;
open Logic;;
open Shell;;
open Geometrize;;

exception InvalidParserState of string;;

%}

%token STOP LLIFT RLIFT LPAREN RPAREN LBRACK RBRACK LCURLY RCURLY LANGLE RANGLE LMSET RMSET 
%token CARROT QUOTE DOT SEMICOLON PAR WHIMPER TILDE MODELS VALID QUIT TRUE AMPERSAND EOF
%token GEOMETRY
%token <string> FILENAME

%left AMPERSAND 
%left PAR
%right TILDE

%start main
%type <Rho.process> process par process_atom input lift drop group continuation
%type <Rho.action> guard
%type <Rho.name> name
%type <Logic.judgment> judgment
%type <Logic.formula> property atomic_form negation conjunction mixture descent elevation activity group_form
%type <Logic.indicator> indication
%type <Shell.command> main

%%

main:
       process SEMICOLON                       { evaluate $1 }
|      VALID property SEMICOLON                { prove $2 }
|      judgment SEMICOLON                      { check $1 }
|      geometry SEMICOLON                      { geometry $1 }
|      toplevel SEMICOLON                      { $1 }
|      EOF                                     { quit () }
;

process:
       par                                     { $1 }
;

par:
       process_atom PAR par                    {
                                                 match $3 with
	                                             Par( ps ) -> parstar ($1 :: ps) 
						   | p -> parstar [$1; p] 
                                               }
|      process_atom                            { $1 }
;

process_atom:
       STOP                                    { parstar [] }
|      input                                   { $1 }
|      lift                                    { $1 }
|      drop                                    { $1 }
|      group                                   { $1 }
;

input:
       guard continuation                      { (prefix $1 $2) }
|      guard                                   { (prefix $1 zero) } 
;

guard: 
       name LPAREN name RPAREN                 { (guard $1 $3) }
;

lift: 
       /* name LLIFT process RLIFT            { (lift $1 $3) } */
	   name LMSET process RMSET                { (lift $1 $3) } 
;

drop: 
       /* CARROT name CARROT                      { drop $2 } */
       RANGLE name LANGLE                      { drop $2 } 
;

group: 
       LBRACK process RBRACK                   { $2 }
;

name: 
       QUOTE process QUOTE                     { quote $2 }
;

continuation: 
       /* DOT LCURLY process RCURLY               { $3 } */
       LCURLY process RCURLY                   { $2 } 
;

property: 
       atomic_form                             { $1 }
|      negation                                { $1 } 
|      conjunction                             { $1 }
|      mixture                                 { $1 } 
|      descent                                 { $1 }
|      elevation                               { $1 } 
|      activity                                { $1 }
|      group_form                              { $1 } 
;
  
atomic_form:
       TRUE                                    { ttrue }
|      STOP                                    { zzero } 
;

negation: 
       TILDE property                          { negate $2 }
;
  
conjunction: 
       property AMPERSAND property             {
                                                 match $1 with
                                                     Conjunction( fs ) ->
                                                       (match $3 with 
                                                           Conjunction( gs ) -> conjoin (fs @ gs)
                                                         | phi -> conjoin ( phi :: fs ))
                                                   | phi -> 
                                                       (match $3 with
                                                           Conjunction( gs ) -> conjoin ( phi :: gs )
                                                         | psi -> conjoin [ $1; $3 ]) 
                                               }
;

mixture:
       property PAR property                   { 
                                                 match $1 with
                                                    Mixture( fs ) ->
                                                      (match $3 with
                                                          Mixture( gs ) -> mix (fs @ gs)
                                                        | phi -> mix ( phi :: fs ))
                                                   | phi -> 
                                                       (match $3 with
                                                           Mixture( gs ) -> mix ( phi :: gs )
                                                        | psi -> mix [ $1; $3 ]) 
                                               }
;

descent: 
       RANGLE name LANGLE                      { descend $2 }
;
  
elevation: 
       indication LANGLE property RANGLE       { elevate $1 $3 }
;
  
activity: 
       LANGLE indication WHIMPER name RANGLE LCURLY property RCURLY  { arm (notice $2 $4) $7 }
;

group_form: 
       LBRACK property RBRACK                  { $2 }
;

indication: 
       QUOTE property QUOTE                    { indicate $2 }
;

judgment: 
       process MODELS property                 { judge $1 $3 }
;

geometry:
       GEOMETRY process                        { Geometrize.Stdio( $2 ) }
|      GEOMETRY process FILENAME               { Geometrize.Fileio( $2, $3 ) }
;

toplevel: 
       QUIT                                    { Quit }
;
