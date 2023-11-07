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

 - Examples
    * [examples/*.iota](https://github.com/kwanghoon/iotacalculus/tree/main/examples)
    * [more examples](https://github.com/swlabKHCoi/IoTa)

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

    * [test1.txt](https://github.com/kwanghoon/iotacalculus/tree/main/demo/test1.txt)

 - 실행 결과 (Run.test2)

   * turn-off-hallway-light-five-minutes-after-the-front-door-is-locked.iota

   * [test2.txt](https://github.com/kwanghoon/iotacalculus/tree/main/demo/test2.txt)

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
             
             EventHandler <b>;</b> 
             
             MultiplePredicateActions 
        <b>end</b>

        (where n>=0)

Decl -> <b>device</b> identifier <b>:</b> 
          (identifier1 | identifier.identifier1) <b>,</b> 
          ... <b>,</b> 
          (identifierk | identifier.identifierk) <b>;</b>

        (where k>=1)

Decl -> <b>timer</b> identifier <b>;</b>

Decl -> <b>input</b> identifier <b>:</b> identifier <b>;</b>

Decl -> <b>output</b> identifier <b>:</b> identifier <b>;</b>

Decl -> <b>output</b> identifier <b>:</b> <b>(</b> identifier1 <b>,</b> ... <b>,</b> identifierk <b>)</b> <b>;</b>

        (where k>=1)

EventHandler -> FieldOrTimer <b>[</b> <b>.</b> <b>~></b> <b>]</b>

EventHandler -> FieldOrTimer <b>[</b> <b>.</b> <b>~></b> Constant <b>]</b>

EventHandler -> FieldOrTimer <b>[</b> Constant <b>~></b> <b>]</b>

EventHandler -> FieldOrTimer <b>[</b> Constant <b>~></b> Constant <b>]</b>

EventHandler -> <b>any</b> Group <b>(</b> identifier <b>-></b> EventHandler <b>)</b>

MultiplePredicateActions -> 
       Predicate <b>;</b>
       Action1 <b>,</b>
       ...
       Actionj1

    <b>|</b> ...

       Predicatek <b>;</b>
       Action1 <b>,</b>
       ...
       Actionjk

   (where k>=1, j1>=1, ..., jk>=1)

Predicate -> Predicate [ <b>||</b> | <b>&&</b> ] Predicate

Predicate -> all Group ( identifier <b>-></b> Predicate )

Predicate -> exists Group ( identifier <b>-></b> Predicate )

Predicate -> Expression

Expression -> Expression [ <b>==</b> | <b>!=</b> | <b><</b> | <b><=</b> | <b>></b> | <b>>=</b> | <b>+</b> | <b>-</b> | <b>*</b> | <b>/</b> ] Expression

Expression -> [ <b>-</b> | <b>!</b> ] Expression

Expression -> <b>true</b> | <b>false</b> | number_literal | string_literal | identifier | identifier . identifier

Expression -> ( Expression )

Constant -> identifier | number_literal


Action -> FieldOrTimer <b>:=</b> Expression

Action -> identifier <b>(</b> Expression1 <b>,</b> ... <b>,</b> Expressionk <b>)</b>

   (where k>=1)

Action -> <b>start</b> identifier <b>at</b> Expression

Action -> <b>stop</b> identifier

Action -> <b>map</b> Group <b>(</b> identifier <b>-></b> Action <b>)</b>

FieldOrTimer -> identifier

FieldOrTimer -> identifier <b>.</b> identifier

Group -> identifier

Group -> <b>{</b> identifier1 <b>,</b> ... <b>,</b> identifiern <b>}</b>

    (where n>=1)
</pre>
