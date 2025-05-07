from fandango.language.parse import parse
from fandango.evolution.algorithm import Fandango, LoggerLevel
from fandango.language.tree import DerivationTree, NonTerminal

with open("spec.fan", "r") as file:
    grammar, constraints = parse(file)

fuzzer = Fandango(
    grammar,
    constraints,
    desired_solutions = 100,
    logger_level = LoggerLevel.ERROR,
    profiling=False,
)
fuzzer.evolve()
solutions = fuzzer.solution

#print(solutions)
for solution in solutions:
    print("Program:")
    print(solution)
#print(grammar)
print(constraints)
