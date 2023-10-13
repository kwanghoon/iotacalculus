# iotacalculus

## 프로그램 설치 방법

 - [하스켈 stack 설치](https://docs.haskellstack.org/en/stable/install_and_upgrade/)
 - 
    ```
     $ git clone https://github.com/kwanghoon/iotacalculus
     $ git clone https://github.com/kwanghoon/yapb
     $ cd iotacalculus
     $ stack build
     $ stack exec -- iotacalculus-exe ./examples/turn-on-siren-when-no-one-is-present.iota 
    ```

## iota 프로그램 구문 파싱 방법
 - 
    ```
     $ stack exec -- iotacalculus-exe ./examples/turn-on-siren-when-no-one-is-present.iota 
    ```

## iota 프로그램 실행 방법
 - 
    ```
     $ stack exec -- ghci
     ghci> :set -iapp
     ghci> :load Main
     ghci> Run.test1
     ...
     ghci> Run.test2
     ...
    ```    

 - 실행 결과 (Run.test1)

    * turn-on-hallway-light-when-the-front-door-is-unlocked.iota

    * [test1.txt](https://github.com/kwanghoon/iotacalculus/blob/master/demo/test1.txt)

 - 실행 결과 (Run.test2)

   * turn-off-hallway-light-five-minutes-after-the-front-door-is-locked.iota

   * [test2.txt](https://github.com/kwanghoon/iotacalculus/blob/master/demo/test2.txt)

## The grammar of Iota calculus

<pre>
Program -> Rules | Rule

Rules -> <b>rules</b> string_literal 

               Decl1,  
               ..., 
               Deckn, 

               (Rule1 | Rules1), 
               ..., 
               (Rulek | Rulesk)
         <b>end</b>
         
         (where n>=0, k>=1)

Rule -> <b>rule</b> string_literal 
             Decl1,  
             ..., 
             Deckn, 
             
             EventHandler ; 
             
             MultiplePredicateActions 
        <b>end</b>

        (where n>=0)

Decl -> <b>device</b> identifier : 
          (identifier1 | identifier.identifier1) , 
          ... , 
          (identifierk | identifier.identifierk) ;

        (where k>=1)

Decl -> <b>timer</b> identifier ;

Decl -> <b>input</b> identifier : identifier ;

Decl -> <b>output</b> identifier : identifier ;

Decl -> <b>output</b> identifier : ( identifier1, ..., identifierk ) ;

        (where k>=1)

EventHandler -> FieldOrTimer [ . ~> ]

EventHandler -> FieldOrTimer [ . ~> Constant ]

EventHandler -> FieldOrTimer [ Constant ~> ]

EventHandler -> FieldOrTimer [ Constant ~> Constant ]

EventHandler -> <b>any</b> Group ( identifier -> EventHandler )

MultiplePredicateActions -> 
       Predicate;
       Action1,
       ...
       Actionj1

    | ...

       Predicatek;
       Action1,
       ...
       Actionjk

   (where k>=1, j1>=1, ..., jk>=1)

Predicate -> Predicate || Predicate

Predicate -> Predicate && Predicate

Predicate -> all Group ( identifier -> Predicate )

Predicate -> exists Group ( identifier -> Predicate )

Predicate -> Expression

Expression -> Expression == Expression

Expression -> Expression != Expression

Expression -> Expression < Expression

Expression -> Expression <= Expression

Expression -> Expression > Expression

Expression -> Expression >= Expression

Expression -> Expression + Expression

Expression -> Expression - Expression

Expression -> Expression * Expression

Expression -> Expression / Expression

Expression -> - Expression

Expression -> ! Expression

Expression -> true

Expression -> false

Expression -> number_literal

Expression -> string_literal

Expression -> identifier

Expression -> identifier . identifier

Expression -> ( Expression )

Constant -> identifier

Constant -> number_literal


Action -> FieldOrTimer := Expression

Action -> identifier ( Expression1, ..., Expressionk )

   (where k>=1)

Action -> <b>start</b> identifier at Expression

Action -> <b>stop</b> identifier

Action -> <b>map</b> Group ( identifier -> Action )

FieldOrTimer -> identifier

FieldOrTimer -> identifier . identifier

Group -> identifier

Group -> { identifier1, ..., identifiern }

    (where n>=1)
</pre>
