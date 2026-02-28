package main

type S struct {}

func _goml_inherent_S_S_method1(self__0 S) struct{} {
    return struct{}{}
}

func main0() struct{} {
    var s__2 S
    var t0 struct{}
    s__2 = S{}
    t0 = _goml_inherent_S_S_method1(s__2)
    return t0
}

func main() {
    main0()
}
