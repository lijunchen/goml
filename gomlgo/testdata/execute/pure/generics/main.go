package main

func identity[T any](value T) T {
	return value
}

func pair[A, B any](left A, right B) (A, B) {
	return left, right
}

func countdown[T ~int](value T) T {
	if value == 0 {
		return value
	}
	return countdown(value - 1)
}

type Box[T any] struct {
	value T
}

func (box Box[T]) Get() T {
	return box.value
}

func main() {
	println(identity[int](3), identity("go"))
	left, right := pair(4, "ml")
	println(left, right, countdown(5))
	integerBox := Box[int]{value: 7}
	stringBox := Box[string]{value: "box"}
	println(integerBox.Get(), stringBox.Get())
	boundGet := integerBox.Get
	getString := Box[string].Get
	println(boundGet(), getString(stringBox))
	identityInt := identity[int]
	println(identityInt(12))
}
