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
```
"Initially,"
(fromList [("front_door@myhome",("lock",fromList [("lock",ConstantLiteral "unlocked")])),("hallway_light@myhome",("switch",fromList [("switch",ConstantLiteral "off")]))],fromList [],fromList [],fromList [])

"initial events:"
fromList [EventField "front_door@myhome" "lock" (ConstantLiteral "locked") (ConstantLiteral "unlocked")]

"after a cycle of event-condition-action,"
(fromList [("front_door@myhome",("lock",fromList [("lock",ConstantLiteral "unlocked")])),("hallway_light@myhome",("switch",fromList [("switch",ConstantLiteral "on")]))],fromList [],fromList [],fromList [])
fromList [EventField "hallway_light" "switch" (ConstantLiteral "off") (ConstantLiteral "on")]    

"after a cycle of event-condition-action,"
(fromList [("front_door@myhome",("lock",fromList [("lock",ConstantLiteral "unlocked")])),("hallway_light@myhome",("switch",fromList [("switch",ConstantLiteral "on")]))],fromList [],fromList [],fromList [])
fromList []

"Finally,"
(fromList [("front_door@myhome",("lock",fromList [("lock",ConstantLiteral "unlocked")])),("hallway_light@myhome",("switch",fromList [("switch",ConstantLiteral "on")]))],fromList [],fromList [],fromList [])
```

 - 실행 결과 (Run.test2)

```
"Initially,"
(fromList [("front_door@myhome",("lock",fromList [("lock",ConstantLiteral "locked")])),("hallway_light@myhome",("switch",fromList [("switch",ConstantLiteral "on")]))],fromList [],fromList [],fromList [])

"initial events:"
fromList [EventField "front_door@myhome" "lock" (ConstantLiteral "unlocked") (ConstantLiteral "locked")]

"after a cycle of event-condition-action,"
(fromList [("front_door@myhome",("lock",fromList [("lock",ConstantLiteral "locked")])),("hallway_light@myhome",("switch",fromList [("switch",ConstantLiteral "on")]))],fromList [],fromList [],fromList [("hallway_light_timer@myhome",NumberLiteral 1)])
fromList [EventTimer "hallway_light_timer@myhome" (NumberLiteral 0) (NumberLiteral 1)]

"after a cycle of event-condition-action,"
(fromList [("front_door@myhome",("lock",fromList [("lock",ConstantLiteral "locked")])),("hallway_light@myhome",("switch",fromList [("switch",ConstantLiteral "on")]))],fromList [],fromList [],fromList [("hallway_light_timer@myhome",NumberLiteral 2)])
fromList [EventTimer "hallway_light_timer@myhome" (NumberLiteral 1) (NumberLiteral 2)]

"after a cycle of event-condition-action,"
(fromList [("front_door@myhome",("lock",fromList [("lock",ConstantLiteral "locked")])),("hallway_light@myhome",("switch",fromList [("switch",ConstantLiteral "on")]))],fromList [],fromList [],fromList [("hallway_light_timer@myhome",NumberLiteral 3)])
fromList [EventTimer "hallway_light_timer@myhome" (NumberLiteral 2) (NumberLiteral 3)]

"after a cycle of event-condition-action,"
(fromList [("front_door@myhome",("lock",fromList [("lock",ConstantLiteral "locked")])),("hallway_light@myhome",("switch",fromList [("switch",ConstantLiteral "on")]))],fromList [],fromList [],fromList [("hallway_light_timer@myhome",NumberLiteral 4)])
fromList [EventTimer "hallway_light_timer@myhome" (NumberLiteral 3) (NumberLiteral 4)]

"after a cycle of event-condition-action,"
(fromList [("front_door@myhome",("lock",fromList [("lock",ConstantLiteral "locked")])),("hallway_light@myhome",("switch",fromList [("switch",ConstantLiteral "on")]))],fromList [],fromList [],fromList [("hallway_light_timer@myhome",NumberLiteral 5)])
fromList [EventTimer "hallway_light_timer@myhome" (NumberLiteral 4) (NumberLiteral 5)]

"after a cycle of event-condition-action,"
(fromList [("front_door@myhome",("lock",fromList [("lock",ConstantLiteral "locked")])),("hallway_light@myhome",("switch",fromList [("switch",ConstantLiteral "off")]))],fromList [],fromList [],fromList [])    
fromList [EventField "hallway_light" "switch" (ConstantLiteral "on") (ConstantLiteral "off")]

"after a cycle of event-condition-action,"
(fromList [("front_door@myhome",("lock",fromList [("lock",ConstantLiteral "locked")])),("hallway_light@myhome",("switch",fromList [("switch",ConstantLiteral "off")]))],fromList [],fromList [],fromList [])    
fromList []

"Finally,"
(fromList [("front_door@myhome",("lock",fromList [("lock",ConstantLiteral "locked")])),("hallway_light@myhome",("switch",fromList [("switch",ConstantLiteral "off")]))],fromList [],fromList [],fromList []) 
```