package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type Worker struct {
    name string
}

type Station struct {
    worker Worker
}

func _goml_m_trait__impl_i_Ready_i_Worker_i_state(self__0 Worker) string {
    var retv154 string
    var t155 string = self__0.name
    var t156 string = t155 + ":ready"
    retv154 = t156
    return retv154
}

func _goml_m_trait__impl_i_Service_i__l_Worker_r__x40_Station_i_get(self__1 Station) Worker {
    var retv158 Worker
    var t159 Worker = self__1.worker
    retv158 = t159
    return retv158
}

func main0() struct{} {
    var t161 Worker = Worker{
        name: "build",
    }
    var t162 Station = Station{
        worker: t161,
    }
    var t163 string = describe__S_Station__T_Worker(t162)
    println__T_string(t163)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t165 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t165)
    return struct{}{}
}

func describe__S_Station__T_Worker(service__2 Station) string {
    var retv168 string
    var t169 Worker = _goml_m_trait__impl_i_Service_i__l_Worker_r__x40_Station_i_get(service__2)
    var t170 string = _goml_m_trait__impl_i_Ready_i_Worker_i_state(t169)
    retv168 = t170
    return retv168
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv172 string
    retv172 = self__38
    return retv172
}

func main() {
    main0()
}
