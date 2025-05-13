import os
import json
from z3 import *

Levels = DeclareSort('Levels')
bot = Int('bot')
top = Int('top')
value = Function('value', Levels, IntSort())

def get_val(l):
    if l == "top":
        return top
    elif l == "bot":
        return bot
    else:
        return value(Const(l, Levels))

def add_level_constraint(solver, z3_consts, solver_constraints, l1, l2, name):
    if l1 not in z3_consts and l1 != "top" and l1 != "bot": 
        z3_consts[l1] = Const(l1, Levels)
    if l2 not in z3_consts and l2 != "top" and l2 != "bot":
        z3_consts[l2] = Const(l2, Levels)
    constraint = get_val(l1) < get_val(l2)

    solver.assert_and_track(constraint, name)  
    return constraint

def check_inequalities(inequalities, file_path):
    solver = Solver()
    z3_consts = {}
    constraint_map = {}
    solver_constraints = []  

    truths = [
        ForAll([Const('x', Levels)], bot < value(Const('x', Levels))),
        ForAll([Const('x', Levels)], value(Const('x', Levels)) < top),
        bot < top
    ]
    solver.add(*truths)

    for i, ineq in enumerate(inequalities):
        span = ineq["span"]
        l1 = ineq["l1"]
        l2 = ineq["l2"]
        constraint_id = f"constraint_{i}"
        constraint = add_level_constraint(solver, z3_consts, solver_constraints, l1, l2, constraint_id)
        constraint_map[constraint_id] = {"span": span, "l1": l1, "l2": l2, "constraint": constraint}

    unsat_constraints = []
    if solver.check() == sat:
        return []
    else:
        while solver.check() == unsat:
            unsat_core = solver.unsat_core()
            unsat_constraints.extend([
                {
                    "span": constraint_map[str(c)]["span"],
                    "l1": constraint_map[str(c)]["l1"],
                    "l2": constraint_map[str(c)]["l2"],
                    "file_path": file_path,
                }
                for c in unsat_core
            ])

            for c in unsat_core:
                constraint_id = str(c)
                constraint_map.pop(constraint_id)
                solver = rebuild_solver_without_constraint(constraint_map, constraint_id, truths)

        return unsat_constraints

def rebuild_solver_without_constraint(constraint_map, constraint_to_remove, truths):
    new_solver = Solver()
    new_solver.add(*truths)
    for id, constraint in constraint_map.items():
        if id != constraint_to_remove: 
            new_solver.assert_and_track(constraint["constraint"], id)
    return new_solver

def get_constraint_id(constraint, constraint_map):
    for key, value in constraint_map.items():
        if str(value["constraint"]) == str(constraint):
            return key
    
if __name__ == "__main__":
    file_path = os.path.join(os.path.dirname(os.path.abspath(__file__)), "ineq.json")

    with open(file_path, "r") as file:
        input_data = file.read()

    inequalities = json.loads(input_data)
    unsat_constraints = check_inequalities(inequalities, file_path)

    with open(file_path, "w") as file:
        if unsat_constraints:
            json.dump(unsat_constraints, file, indent=4)
        else:
            file.write("")