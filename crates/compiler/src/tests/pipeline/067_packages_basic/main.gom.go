package main

import (
    _goml_fmt "fmt"
)

func int32_to_string(x int32) string {
    return _goml_fmt.Sprintf("%d", x)
}

func string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type _goml_m_packages__basic_p_Lib_p_Color int32

const (
    Red _goml_m_packages__basic_p_Lib_p_Color = 0
    Green _goml_m_packages__basic_p_Lib_p_Color = 1
)

func _goml_m_packages__basic_p_main() struct{} {
    var t1 int32 = _goml_m_packages__basic_p_Lib_p_color__to__int(Red)
    var t2 string = int32_to_string(t1)
    println__T_string(t2)
    return struct{}{}
}

func _goml_m_packages__basic_p_Lib_p_color__to__int(c__0 _goml_m_packages__basic_p_Lib_p_Color) int32 {
    var retv5 int32
    var jp7 int32
    switch c__0 {
    case Red:
        jp7 = 1
    case Green:
        jp7 = 2
    default:
        panic("non-exhaustive match")
    }
    retv5 = jp7
    return retv5
}

func main0() struct{} {
    _goml_m_packages__basic_p_main()
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    string_println(value__1)
    return struct{}{}
}

func main() {
    main0()
}
