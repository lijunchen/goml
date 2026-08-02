package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type Tuple2_4bool_4bool struct {
    _0 bool
    _1 bool
}

func test_nested_match(x__0 Tuple2_4bool_4bool, y__1 Tuple2_4bool_4bool) struct{} {
    var x155 bool = x__0._0
    var x156 bool = x__0._1
    switch x156 {
    case true:
        var x157 bool = y__1._0
        var x158 bool = y__1._1
        switch x158 {
        case true:
            switch x157 {
            case true:
                println__T_string("case4")
                return struct{}{}
            case false:
                println__T_string("case3")
                return struct{}{}
            default:
                panic("non-exhaustive match")
            }
        case false:
            println__T_string("case4")
            return struct{}{}
        default:
            panic("non-exhaustive match")
        }
    case false:
        switch x155 {
        case true:
            var x159 bool = y__1._0
            var x160 bool = y__1._1
            switch x160 {
            case true:
                switch x159 {
                case true:
                    println__T_string("case2")
                    return struct{}{}
                case false:
                    println__T_string("case1")
                    return struct{}{}
                default:
                    panic("non-exhaustive match")
                }
            case false:
                println__T_string("case2")
                return struct{}{}
            default:
                panic("non-exhaustive match")
            }
        case false:
            var x161 bool = y__1._0
            var x162 bool = y__1._1
            switch x162 {
            case true:
                switch x161 {
                case true:
                    println__T_string("case4")
                    return struct{}{}
                case false:
                    println__T_string("case3")
                    return struct{}{}
                default:
                    panic("non-exhaustive match")
                }
            case false:
                println__T_string("case4")
                return struct{}{}
            default:
                panic("non-exhaustive match")
            }
        default:
            panic("non-exhaustive match")
        }
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t186 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
        _0: true,
        _1: false,
    }
    var t187 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
        _0: false,
        _1: true,
    }
    test_nested_match(t186, t187)
    var t188 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
        _0: true,
        _1: false,
    }
    var t189 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
        _0: true,
        _1: false,
    }
    test_nested_match(t188, t189)
    var t190 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
        _0: false,
        _1: true,
    }
    var t191 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
        _0: false,
        _1: true,
    }
    test_nested_match(t190, t191)
    var t192 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
        _0: false,
        _1: true,
    }
    var t193 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
        _0: true,
        _1: false,
    }
    test_nested_match(t192, t193)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t195 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t195)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func main() {
    main0()
}
