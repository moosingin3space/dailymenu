package libmealplanner

type FoodStyle int
type MealSize int

const (
	Chicken FoodStyle = iota
	Pork
	Shrimp
	Sandwich
	Asian
	Indian
	Mexican
	Pasta
	Pizza
	// TODO add more or make this a string
)

const (
	Lunch MealSize = iota
	Dinner
	// TODO add other descriptors
)

type Amount struct {
	Unit  string
	Value float64
}

type Ingredient struct {
	Description string
	Amount      Amount
}

type Recipe struct {
	Name         string
	PrepTime     int
	PrimaryStyle FoodStyle
	//Styles      []FoodStyle
	Size        MealSize
	Ingredients []Ingredient
	Steps       []string
}
