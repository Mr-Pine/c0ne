from fandango.language.grammar import NonTerminal
from fandango.language.tree import DerivationTree

def enforce_return(program: DerivationTree) -> bool:
    returns = program.find_all_nodes(NonTerminal("returnstmt"))
    return len(returns) >= 1
