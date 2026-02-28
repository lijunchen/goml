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
    var t50 Expr
    var a__0 Expr
    var x__9 Expr
    var t52 struct{}
    var x0 Expr
    var t53 struct{}
    var x1 Expr
    var x2 Expr
    var t56 struct{}
    var x10 Expr
    var x__2 Expr
    var y__3 Expr
    var t57 struct{}
    var x11 Expr
    var x12 Expr
    var x__8 Expr
    var t58 struct{}
    var x13 Expr
    var x14 Expr
    var t59 struct{}
    var x5 Expr
    var t61 struct{}
    var x15 Expr
    var t62 struct{}
    var x16 Expr
    var x17 Expr
    var t63 struct{}
    var x18 Expr
    var x19 Expr
    var t64 struct{}
    var x6 Expr
    var x7 Expr
    var t66 struct{}
    var x20 Expr
    var t67 struct{}
    var x21 Expr
    var x22 Expr
    var t68 struct{}
    var x23 Expr
    var x24 Expr
    var t69 struct{}
    var x8 Expr
    var x9 Expr
    var t71 struct{}
    var x25 Expr
    var t72 struct{}
    var x26 Expr
    var x27 Expr
    var t73 struct{}
    var x28 Expr
    var x29 Expr
    var t74 struct{}
    var x3 Expr
    var x4 Expr
    var x__1 Expr
    var t76 struct{}
    var x30 Expr
    var x__4 Expr
    var t78 struct{}
    var x35 Expr
    var t79 struct{}
    var x36 Expr
    var x37 Expr
    var t80 struct{}
    var x38 Expr
    var x39 Expr
    var t81 struct{}
    var x31 Expr
    var x32 Expr
    var t83 struct{}
    var x40 Expr
    var y__6 Expr
    var x__5 Expr
    var z__7 Expr
    var t84 struct{}
    var x41 Expr
    var x42 Expr
    var t85 struct{}
    var x43 Expr
    var x44 Expr
    var t86 struct{}
    var x33 Expr
    var x34 Expr
    var t88 struct{}
    var x45 Expr
    var t89 struct{}
    var x46 Expr
    var x47 Expr
    var t90 struct{}
    var x48 Expr
    var x49 Expr
    var t91 struct{}
    _ = x__9
    _ = t52
    _ = x0
    _ = t53
    _ = t56
    _ = x__2
    _ = y__3
    _ = t57
    _ = x11
    _ = x12
    _ = x__8
    _ = t58
    _ = x13
    _ = x14
    _ = t59
    _ = x5
    _ = t61
    _ = t62
    _ = x16
    _ = x17
    _ = t63
    _ = x18
    _ = x19
    _ = t64
    _ = x6
    _ = x7
    _ = t66
    _ = t67
    _ = x21
    _ = x22
    _ = t68
    _ = x23
    _ = x24
    _ = t69
    _ = x8
    _ = x9
    _ = t71
    _ = t72
    _ = x26
    _ = x27
    _ = t73
    _ = x28
    _ = x29
    _ = t74
    _ = x__1
    _ = t76
    _ = x30
    _ = x__4
    _ = t78
    _ = x35
    _ = t79
    _ = x36
    _ = x37
    _ = t80
    _ = x38
    _ = x39
    _ = t81
    _ = t83
    _ = x40
    _ = y__6
    _ = x__5
    _ = z__7
    _ = t84
    _ = x41
    _ = x42
    _ = t85
    _ = x43
    _ = x44
    _ = t86
    _ = x33
    _ = x34
    _ = t88
    _ = x45
    _ = t89
    _ = x46
    _ = x47
    _ = t90
    _ = x48
    _ = x49
    _ = t91
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            t50 = Add{
                _0: Zero{},
                _1: Zero{},
            }
            a__0 = Mul{
                _0: t50,
                _1: Zero{},
            }
            switch a__0.(type) {
            case Zero:
                pc = 2
            case Succ:
                pc = 3
            case Add:
                pc = 4
            case Mul:
                pc = 30
            default:
                panic("non-exhaustive match")
            }
        case 1:
            return struct{}{}
        case 2:
            print__T_int32(6)
            pc = 1
        case 3:
            print__T_int32(6)
            pc = 1
        case 4:
            x1 = a__0.(Add)._0
            x2 = a__0.(Add)._1
            switch x2.(type) {
            case Zero:
                pc = 6
            case Succ:
                pc = 12
            case Add:
                pc = 18
            case Mul:
                pc = 24
            default:
                panic("non-exhaustive match")
            }
        case 5:
            pc = 1
        case 6:
            switch x1.(type) {
            case Zero:
                pc = 8
            case Succ:
                pc = 9
            case Add:
                pc = 10
            case Mul:
                pc = 11
            default:
                panic("non-exhaustive match")
            }
        case 7:
            pc = 5
        case 8:
            print__T_int32(0)
            pc = 7
        case 9:
            x10 = x1.(Succ)._0
            print__T_int32(2)
            pc = 7
        case 10:
            print__T_int32(5)
            pc = 7
        case 11:
            print__T_int32(5)
            pc = 7
        case 12:
            switch x1.(type) {
            case Zero:
                pc = 14
            case Succ:
                pc = 15
            case Add:
                pc = 16
            case Mul:
                pc = 17
            default:
                panic("non-exhaustive match")
            }
        case 13:
            pc = 5
        case 14:
            print__T_int32(6)
            pc = 13
        case 15:
            x15 = x1.(Succ)._0
            print__T_int32(2)
            pc = 13
        case 16:
            print__T_int32(6)
            pc = 13
        case 17:
            print__T_int32(6)
            pc = 13
        case 18:
            switch x1.(type) {
            case Zero:
                pc = 20
            case Succ:
                pc = 21
            case Add:
                pc = 22
            case Mul:
                pc = 23
            default:
                panic("non-exhaustive match")
            }
        case 19:
            pc = 5
        case 20:
            print__T_int32(6)
            pc = 19
        case 21:
            x20 = x1.(Succ)._0
            print__T_int32(2)
            pc = 19
        case 22:
            print__T_int32(6)
            pc = 19
        case 23:
            print__T_int32(6)
            pc = 19
        case 24:
            switch x1.(type) {
            case Zero:
                pc = 26
            case Succ:
                pc = 27
            case Add:
                pc = 28
            case Mul:
                pc = 29
            default:
                panic("non-exhaustive match")
            }
        case 25:
            pc = 5
        case 26:
            print__T_int32(6)
            pc = 25
        case 27:
            x25 = x1.(Succ)._0
            print__T_int32(2)
            pc = 25
        case 28:
            print__T_int32(6)
            pc = 25
        case 29:
            print__T_int32(6)
            pc = 25
        case 30:
            x3 = a__0.(Mul)._0
            x4 = a__0.(Mul)._1
            switch x3.(type) {
            case Zero:
                pc = 32
            case Succ:
                pc = 33
            case Add:
                pc = 39
            case Mul:
                pc = 45
            default:
                panic("non-exhaustive match")
            }
        case 31:
            pc = 1
        case 32:
            print__T_int32(1)
            pc = 31
        case 33:
            switch x4.(type) {
            case Zero:
                pc = 35
            case Succ:
                pc = 36
            case Add:
                pc = 37
            case Mul:
                pc = 38
            default:
                panic("non-exhaustive match")
            }
        case 34:
            pc = 31
        case 35:
            print__T_int32(3)
            pc = 34
        case 36:
            print__T_int32(6)
            pc = 34
        case 37:
            print__T_int32(6)
            pc = 34
        case 38:
            print__T_int32(6)
            pc = 34
        case 39:
            x31 = x3.(Add)._0
            x32 = x3.(Add)._1
            switch x4.(type) {
            case Zero:
                pc = 41
            case Succ:
                pc = 42
            case Add:
                pc = 43
            case Mul:
                pc = 44
            default:
                panic("non-exhaustive match")
            }
        case 40:
            pc = 31
        case 41:
            print__T_int32(3)
            pc = 40
        case 42:
            print__T_int32(4)
            pc = 40
        case 43:
            print__T_int32(4)
            pc = 40
        case 44:
            print__T_int32(4)
            pc = 40
        case 45:
            switch x4.(type) {
            case Zero:
                pc = 47
            case Succ:
                pc = 48
            case Add:
                pc = 49
            case Mul:
                pc = 50
            default:
                panic("non-exhaustive match")
            }
        case 46:
            pc = 31
        case 47:
            print__T_int32(3)
            pc = 46
        case 48:
            print__T_int32(6)
            pc = 46
        case 49:
            print__T_int32(6)
            pc = 46
        case 50:
            print__T_int32(6)
            pc = 46
        default:
            panic("invalid pc")
        }
    }
}

func print__T_int32(value__0 int32) struct{} {
    var t92 string
    var t93 struct{}
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            t92 = int32_to_string(value__0)
            t93 = string_print(t92)
            return t93
        default:
            panic("invalid pc")
        }
    }
}

func main() {
    main0()
}
