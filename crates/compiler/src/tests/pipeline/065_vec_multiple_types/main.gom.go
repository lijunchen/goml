package main

import (
    "fmt"
)

func bool_to_string(x bool) string {
    if x {
        return "true"
    } else {
        return "false"
    }
}

func int32_to_string(x int32) string {
    return fmt.Sprintf("%d", x)
}

func string_println(s string) struct{} {
    fmt.Println(s)
    return struct{}{}
}

func main0() struct{} {
    var ret11 struct{}
    var vi__0 []int32 = nil
    var vi__1 []int32 = append(vi__0, 42)
    var val_i__2 int32 = vi__1[0]
    var len_i__3 int32 = int32(len(vi__1))
    var vs__4 []string = nil
    var vs__5 []string = append(vs__4, "hello")
    var vs__6 []string = append(vs__5, "world")
    var val_s__7 string = vs__6[1]
    var len_s__8 int32 = int32(len(vs__6))
    var vb__9 []bool = nil
    var vb__10 []bool = append(vb__9, true)
    var vb__11 []bool = append(vb__10, false)
    var val_b__12 bool = vb__11[0]
    var len_b__13 int32 = int32(len(vb__11))
    var t6 string = int32_to_string(val_i__2)
    string_println(t6)
    var t7 string = int32_to_string(len_i__3)
    string_println(t7)
    string_println(val_s__7)
    var t8 string = int32_to_string(len_s__8)
    string_println(t8)
    var t9 string = bool_to_string(val_b__12)
    string_println(t9)
    var t10 string = int32_to_string(len_b__13)
    string_println(t10)
    ret11 = struct{}{}
    return ret11
}

func main() {
    main0()
}
