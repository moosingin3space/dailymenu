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
	Name        string
	PrepTime    int
	Ingredients []Ingredient
	Steps       []string
}
