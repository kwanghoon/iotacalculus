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

    * [test1.txt](https://github.com/kwanghoon/iotacalculus/blob/master/test/test1.txt)

 - 실행 결과 (Run.test2)

   * turn-off-hallway-light-five-minutes-after-the-front-door-is-locked.iota

   * [test2.txt](https://github.com/kwanghoon/iotacalculus/blob/master/test/test2.txt)

## The grammar of Iota calculus

```
0: IotaProg' -> IotaProg
1: IotaProg -> Rules
2: IotaProg -> Rule
3: Rules -> rules string_literal ZeroOrMoreDecls OneOrMoreRules end
4: OneOrMoreRules -> Rule
5: OneOrMoreRules -> Rules
6: OneOrMoreRules -> Rule OneOrMoreRules
7: OneOrMoreRules -> Rules OneOrMoreRules
8: Rule -> rule string_literal ZeroOrMoreDecls EventHandler ; OneOrMorePredicateActions end
9: ZeroOrMoreDecls -> 
10: ZeroOrMoreDecls -> Decl ZeroOrMoreDecls
11: Decl -> device identifier : OneOrMoreCapabilities ;
12: Decl -> timer identifier ;
13: Decl -> input identifier : identifier ;
14: Decl -> output identifier : identifier ;
15: Decl -> output identifier : ( OneOrMoreIdentifiers ) ;
16: OneOrMoreCapabilities -> identifier
17: OneOrMoreCapabilities -> identifier . identifier
18: OneOrMoreCapabilities -> identifier , OneOrMoreCapabilities
19: OneOrMoreCapabilities -> identifier . identifier , OneOrMoreCapabilities
20: OneOrMoreIdentifiers -> identifier
21: OneOrMoreIdentifiers -> identifier , OneOrMoreIdentifiers
22: EventHandler -> FieldOrTimer [ . ~> ]
23: EventHandler -> FieldOrTimer [ . ~> Constant ]
24: EventHandler -> FieldOrTimer [ Constant ~> ]
25: EventHandler -> FieldOrTimer [ Constant ~> Constant ]
26: EventHandler -> any Group ( identifier -> EventHandler )
27: OneOrMorePredicateActions -> Predicate ; Actions
28: OneOrMorePredicateActions -> Predicate ; Actions | OneOrMorePredicateActions
29: Predicate -> OrPred
30: Predicate -> all Group ( identifier -> Predicate )
31: Predicate -> exists Group ( identifier -> Predicate )
32: OrPred -> OrPred || AndPred
33: OrPred -> AndPred
34: AndPred -> AndPred && EqNeqPred
35: AndPred -> EqNeqPred
36: EqNeqPred -> EqNeqPred == CompExpr
37: EqNeqPred -> EqNeqPred != CompExpr
38: EqNeqPred -> CompExpr
39: CompExpr -> CompExpr < AdditiveExpr
40: CompExpr -> CompExpr <= AdditiveExpr
41: CompExpr -> CompExpr > AdditiveExpr
42: CompExpr -> CompExpr >= AdditiveExpr
43: CompExpr -> AdditiveExpr
44: AdditiveExpr -> AdditiveExpr + MultiplicativeExpr
45: AdditiveExpr -> AdditiveExpr - MultiplicativeExpr
46: AdditiveExpr -> MultiplicativeExpr
47: MultiplicativeExpr -> MultiplicativeExpr * UnaryExpr
48: MultiplicativeExpr -> MultiplicativeExpr / UnaryExpr
49: MultiplicativeExpr -> UnaryExpr
50: UnaryExpr -> - PrimaryExpr
51: UnaryExpr -> ! PrimaryExpr
52: UnaryExpr -> PrimaryExpr
53: PrimaryExpr -> true
54: PrimaryExpr -> false
55: PrimaryExpr -> number_literal
56: PrimaryExpr -> string_literal
57: PrimaryExpr -> identifier
58: PrimaryExpr -> identifier . identifier
59: PrimaryExpr -> ( Predicate )
60: Constant -> identifier
61: Constant -> number_literal
62: Actions -> 
63: Actions -> OneOrMoreActions
64: OneOrMoreActions -> Action
65: OneOrMoreActions -> Action , OneOrMoreActions
66: Action -> FieldOrTimer := AdditiveExpr
67: Action -> identifier ( OneOrMoreAdditiveExprs )
68: Action -> start identifier at AdditiveExpr
69: Action -> stop identifier
70: Action -> tick identifier
71: Action -> map Group ( identifier -> Action )
72: OneOrMoreAdditiveExprs -> AdditiveExpr
73: OneOrMoreAdditiveExprs -> AdditiveExpr , OneOrMoreAdditiveExprs
74: FieldOrTimer -> identifier
75: FieldOrTimer -> identifier . identifier
76: Group -> identifier
77: Group -> { zero_or_more_device_identifiers }
78: zero_or_more_device_identifiers -> 
79: zero_or_more_device_identifiers -> one_or_more_device_identifiers
80: one_or_more_device_identifiers -> identifier
81: one_or_more_device_identifiers -> identifier , one_or_more_device_identifiers
```