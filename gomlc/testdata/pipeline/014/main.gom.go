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
    var x108 bool = x__0._0
    var x109 bool = x__0._1
    switch x109 {
    case true:
        var x110 bool = y__1._0
        var x111 bool = y__1._1
        switch x111 {
        case true:
            switch x110 {
            case true:
                println__T_string("case4")
            case false:
                println__T_string("case3")
            default:
                panic("non-exhaustive match")
            }
        case false:
            println__T_string("case4")
        default:
            panic("non-exhaustive match")
        }
    case false:
        switch x108 {
        case true:
            var x112 bool = y__1._0
            var x113 bool = y__1._1
            switch x113 {
            case true:
                switch x112 {
                case true:
                    println__T_string("case2")
                case false:
                    println__T_string("case1")
                default:
                    panic("non-exhaustive match")
                }
            case false:
                println__T_string("case2")
            default:
                panic("non-exhaustive match")
            }
        case false:
            var x114 bool = y__1._0
            var x115 bool = y__1._1
            switch x115 {
            case true:
                switch x114 {
                case true:
                    println__T_string("case4")
                case false:
                    println__T_string("case3")
                default:
                    panic("non-exhaustive match")
                }
            case false:
                println__T_string("case4")
            default:
                panic("non-exhaustive match")
            }
        default:
            panic("non-exhaustive match")
        }
    default:
        panic("non-exhaustive match")
    }
    return struct{}{}
}

func main0() struct{} {
    var t139 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
        _0: true,
        _1: false,
    }
    var t140 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
        _0: false,
        _1: true,
    }
    test_nested_match(t139, t140)
    var t141 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
        _0: true,
        _1: false,
    }
    var t142 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
        _0: true,
        _1: false,
    }
    test_nested_match(t141, t142)
    var t143 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
        _0: false,
        _1: true,
    }
    var t144 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
        _0: false,
        _1: true,
    }
    test_nested_match(t143, t144)
    var t145 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
        _0: false,
        _1: true,
    }
    var t146 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
        _0: true,
        _1: false,
    }
    test_nested_match(t145, t146)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t148 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t148)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv151 string
    retv151 = self__38
    return retv151
}

func main() {
    main0()
}
