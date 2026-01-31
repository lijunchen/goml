package main

import (
    "fmt"
)

func int32_to_string(x int32) string {
    return fmt.Sprintf("%d", x)
}

func string_print(s string) struct{} {
    fmt.Print(s)
    return struct{}{}
}

type Expr interface {
    isExpr()
}

type Zero struct {}

func (_ Zero) isExpr() {}

type Succ struct {
    _0 Expr
}

func (_ Succ) isExpr() {}

type Add struct {
    _0 Expr
    _1 Expr
}

func (_ Add) isExpr() {}

type Mul struct {
    _0 Expr
    _1 Expr
}

func (_ Mul) isExpr() {}

func main0() struct{} {
    var ret55 struct{}
    var t51 Expr = Zero{}
    var t52 Expr = Zero{}
    var t50 Expr = Add{
        _0: t51,
        _1: t52,
    }
    var t53 Expr = Zero{}
    var a__0 Expr = Mul{
        _0: t50,
        _1: t53,
    }
    switch a__0 := a__0.(type) {
    case Zero:
        ret55 = print__T_int32(6)
    case Succ:
        ret55 = print__T_int32(6)
    case Add:
        var x1 Expr = a__0._0
        var x2 Expr = a__0._1
        switch x2.(type) {
        case Zero:
            switch x1.(type) {
            case Zero:
                ret55 = print__T_int32(0)
            case Succ:
                ret55 = print__T_int32(2)
            case Add:
                ret55 = print__T_int32(5)
            case Mul:
                ret55 = print__T_int32(5)
            }
        case Succ:
            switch x1.(type) {
            case Zero:
                ret55 = print__T_int32(6)
            case Succ:
                ret55 = print__T_int32(2)
            case Add:
                ret55 = print__T_int32(6)
            case Mul:
                ret55 = print__T_int32(6)
            }
        case Add:
            switch x1.(type) {
            case Zero:
                ret55 = print__T_int32(6)
            case Succ:
                ret55 = print__T_int32(2)
            case Add:
                ret55 = print__T_int32(6)
            case Mul:
                ret55 = print__T_int32(6)
            }
        case Mul:
            switch x1.(type) {
            case Zero:
                ret55 = print__T_int32(6)
            case Succ:
                ret55 = print__T_int32(2)
            case Add:
                ret55 = print__T_int32(6)
            case Mul:
                ret55 = print__T_int32(6)
            }
        }
    case Mul:
        var x3 Expr = a__0._0
        var x4 Expr = a__0._1
        switch x3.(type) {
        case Zero:
            ret55 = print__T_int32(1)
        case Succ:
            switch x4.(type) {
            case Zero:
                ret55 = print__T_int32(3)
            case Succ:
                ret55 = print__T_int32(6)
            case Add:
                ret55 = print__T_int32(6)
            case Mul:
                ret55 = print__T_int32(6)
            }
        case Add:
            switch x4.(type) {
            case Zero:
                ret55 = print__T_int32(3)
            case Succ:
                ret55 = print__T_int32(4)
            case Add:
                ret55 = print__T_int32(4)
            case Mul:
                ret55 = print__T_int32(4)
            }
        case Mul:
            switch x4.(type) {
            case Zero:
                ret55 = print__T_int32(3)
            case Succ:
                ret55 = print__T_int32(6)
            case Add:
                ret55 = print__T_int32(6)
            case Mul:
                ret55 = print__T_int32(6)
            }
        }
    }
    return ret55
}

func print__T_int32(value__0 int32) struct{} {
    var ret56 struct{}
    var t54 string = int32_to_string(value__0)
    ret56 = string_print(t54)
    return ret56
}

func main() {
    main0()
}
