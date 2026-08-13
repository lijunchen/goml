package main

type generatedVector struct {
	items []int
}

func generatedVectorNew() *generatedVector {
	return &generatedVector{items: nil}
}

func generatedVectorPush(vector *generatedVector, element int) {
	vector.items = append(vector.items, element)
}

func main() {
	vector := generatedVectorNew()
	generatedVectorPush(vector, 42)
	println(vector.items[0])
}
