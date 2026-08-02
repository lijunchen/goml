package main

type S struct {}

func main0() struct{} {
    return struct{}{}
}

func main() {
    main0()
}
