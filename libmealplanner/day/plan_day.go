package day

import (
	planner "github.com/moosingin3space/dailymenu/libmealplanner"
)

type planDay struct{}

// One lunch, one dinner
func (d planDay) CompatibleSizes(r1 planner.Recipe, r2 planner.Recipe) bool {
	return (r1.Size == planner.Lunch && r2.Size == planner.Dinner) ||
		(r2.Size == planner.Lunch && r1.Size == planner.Dinner)
}

// No two of the same style in one day
func (d planDay) CompatibleStyles(r1 planner.Recipe, r2 planner.Recipe) bool {
	return r1.Style != r2.Style
}

func New() PlanPeriod {
	return planDay{}
}
