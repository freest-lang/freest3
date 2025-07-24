import sys
import json
from z3 import *
import re

Levels = DeclareSort('Levels')
bot = Int('bot')
top = Int('top')
value = Function('value', Levels, IntSort())

lower_id = r"\b[a-zA-Z][a-zA-Z0-9_'#:]*\b"

def rewrite_expression(expr):
    if not isinstance(expr, str):
        expr = str(expr)
    def repl(match):
        var = match.group(0)
        return f"value(Const('{var}', Levels))"
    expr_rewritten = re.sub(lower_id, repl, expr)
    val = eval(expr_rewritten, {"value": value, "Const": Const, "Levels": Levels, "top": top, "bot": bot})
    if isinstance(val, int):
        return IntVal(val)
    else:
        return val

def get_val(l):
    if l == "top":
        return top
    elif l == "bot":
        return bot
    else:
        return rewrite_expression(l)

def extract_variables(expr):
    if not isinstance(expr, str):
        expr = str(expr)
    return re.findall(lower_id, expr)

def add_level_constraint(solver, z3_consts, l1, l2, name):
    for var in extract_variables(l1) + extract_variables(l2):
        if var not in z3_consts and var != "top" and var != "bot":
            z3_consts[var] = Const(var, Levels)
    constraint = get_val(l1) < get_val(l2)
    solver.assert_and_track(constraint, name)
    return constraint

def add_level_equality(solver, z3_consts, l1, l2, name):
    for var in extract_variables(l1) + extract_variables(l2):
        if var not in z3_consts and var != "top" and var != "bot":
            z3_consts[var] = Const(var, Levels)
    constraint = get_val(l1) == get_val(l2)
    solver.assert_and_track(constraint, name)
    return constraint

def wrap_thread_num(s):
    return re.sub(r'#(\d+)', r'[#\1]', s)

def check_inequalities(inequalities, file_path):
    solver = Solver()
    z3_consts = {}
    constraint_map = {}

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
        equality = ineq["equality"]
        constraint_id = f"constraint_{i}"
        if equality:
            constraint = add_level_equality(solver, z3_consts, l1, l2, constraint_id)
        else:
            constraint = add_level_constraint(solver, z3_consts, l1, l2, constraint_id)
        constraint_map[constraint_id] = {"span": span, "l1": l1, "l2": l2, "constraint": constraint, "equality": equality}

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
                    "equality": constraint_map[str(c)]["equality"],
                    "file_path": file_path,
                }
                for c in unsat_core
            ])

            for c in unsat_core:
                constraint_id = str(c)
                if not constraint_map[constraint_id]["equality"]:
                    constraint_map.pop(constraint_id)
                    solver = rebuild_solver_without_constraint(constraint_map, constraint_id, truths)
        
        for c in unsat_constraints:
            if isinstance(c["l1"], str):
                c["l1"] = wrap_thread_num(c["l1"])
            if isinstance(c["l2"], str):
                c["l2"] = wrap_thread_num(c["l2"])

        return unsat_constraints

def rebuild_solver_without_constraint(constraint_map, constraint_to_remove, truths):
    new_solver = Solver()
    new_solver.add(*truths)
    for id, constraint in constraint_map.items():
        if id != constraint_to_remove: 
            new_solver.assert_and_track(constraint["constraint"], id)
    return new_solver
    
if __name__ == "__main__":
    file_path = sys.argv[1]

    with open(file_path, "r") as file:
        input_data = file.read()

    inequalities = json.loads(input_data)
    unsat_constraints = check_inequalities(inequalities, file_path)

    with open(file_path, "w") as file:
        if unsat_constraints:
            json.dump(unsat_constraints, file, indent=4)
        else:
            file.write("")