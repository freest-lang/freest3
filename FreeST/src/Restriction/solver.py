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

def wrap_variables(level, function, thread_num):
    vars = extract_variables(level)
    wrapped_level = level
    for var in vars:
        replacement = f"{function}:{var}#{thread_num}"
        wrapped_level = wrapped_level.replace(var, replacement)
    return wrapped_level

def unwrap_variables(expr):
    if not isinstance(expr, str):
        return str(expr)
    pattern = r"([a-zA-Z_][a-zA-Z0-9_]*)\:([a-zA-Z_][a-zA-Z0-9_']*)(#(\d+))"
    matches = re.findall(pattern, expr)
    function = matches[0][0] if matches else None
    thread_num = int(matches[0][3]) if matches else None
    def repl(m):
        return m.group(2)
    unwrapped_expr = re.sub(pattern, repl, expr)
    return unwrapped_expr

def check_inequalities(inequalities, file_path):
    solver = Solver()
    z3_consts = {}
    constraint_map = {}
    equalities = []
    unsat_core_list = []

    truths = [
        ForAll([Const('x', Levels)], bot < value(Const('x', Levels))),
        ForAll([Const('x', Levels)], value(Const('x', Levels)) < top),
        bot < top
    ]
    solver.add(*truths)

    for i, ineq in enumerate(inequalities):
        span = ineq["span"]
        function = ineq["function"]
        thread_num = ineq["thread_num"]
        l1 = wrap_variables(ineq["l1"], function, thread_num)
        l2 = wrap_variables(ineq["l2"], function, thread_num)
        equality = ineq["equality"]
        constraint_id = f"constraint_{i}"
        x_instance = ineq["xinstance"]
        y_instance = ineq["yinstance"]
        if equality:
            constraint = add_level_equality(solver, z3_consts, l1, l2, constraint_id)
            equalities.append(constraint)
        else:
            constraint = add_level_constraint(solver, z3_consts, l1, l2, constraint_id)
            print(f"{constraint} with ({x_instance}, {y_instance})")
        constraint_map[constraint_id] = {"span": span, "l1": l1, "l2": l2, "constraint": constraint, "function": function, "thread_num": thread_num, "equality": equality}

    unsat_constraints = []
    if solver.check() == sat:
        return []
    else:
        while solver.check() == unsat:
            unsat_core = solver.unsat_core()
            unsat_constraints.extend([
                {
                    "span": constraint_map[str(c)]["span"],
                    "l1": unwrap_variables(constraint_map[str(c)]["l1"]),
                    "l2": unwrap_variables(constraint_map[str(c)]["l2"]),
                    "function": constraint_map[str(c)]["function"],
                    "thread_num": constraint_map[str(c)]["thread_num"],
                    "equality": constraint_map[str(c)]["equality"],
                    "file_path": file_path,
                }
                for c in unsat_core
            ])

            for c in unsat_core:
                # print(unwrap_variables(constraint_map[str(c)]["l1"])
                #       + (" = " if constraint_map[str(c)]["equality"] else " < ")
                #       + unwrap_variables(constraint_map[str(c)]["l2"]))
                constraint_id = str(c)
                unsat_core_list.append(constraint_map[constraint_id])
                if not constraint_map[constraint_id]["equality"]:
                    constraint_map.pop(constraint_id)
                    solver = rebuild_solver_without_constraint(constraint_map, constraint_id, truths)
        
        
        
                
        filtered_unsat_constraints = check_equalities(equalities, unsat_core_list, truths, unsat_constraints)
        return filtered_unsat_constraints
        # return unsat_constraints

def rebuild_solver_without_constraint(constraint_map, constraint_to_remove, truths):
    new_solver = Solver()
    new_solver.add(*truths)
    for id, constraint in constraint_map.items():
        if id != constraint_to_remove: 
            new_solver.assert_and_track(constraint["constraint"], id)
    return new_solver

def check_equalities(equalities, unsat_core_list, truths, unsat_constraints):
    for c in unsat_core_list:
        new_solver = Solver()
        new_solver.add(*truths)   
        new_solver.add(c["constraint"])
        for e in equalities:
            new_solver.add(e)
        if new_solver.check() == sat:
            unsat_constraints = remove_constraint(unsat_constraints, c)  
    return unsat_constraints   

def remove_constraint(unsat_constraints, c):
    l1 = unwrap_variables(c["l1"])
    l2 = unwrap_variables(c["l2"])
    function = c["function"]
    thread_num = c["thread_num"]
    equality = c["equality"]
    return [
        uc for uc in unsat_constraints
        if not (
            uc["l1"] == l1 and
            uc["l2"] == l2 and
            uc["function"] == function and
            uc["thread_num"] == thread_num and
            uc["equality"] == equality
        )
    ]

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