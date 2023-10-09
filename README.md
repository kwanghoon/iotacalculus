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
