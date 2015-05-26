package libmealplanner

type Amount struct {
	Unit  string
	Value float64
}

type Ingredient struct {
	Description string
	Amount      Amount
}

type Recipe struct {
	Ingredients []Ingredient
	Steps       []string
}
