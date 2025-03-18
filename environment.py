import copy
from typing import Optional, Dict, Any

class Environment:
    def __init__(self, bindings: Optional[Dict[str, Any]] = None, outer_scope: "Environment" = None):
        self.bindings = bindings if bindings is not None else {}
        self.outer_scope: Environment = outer_scope # outer env for nested scopes

    def lookup(self, name: str) -> Any:
        if name in self.bindings:
            return self.bindings.get(name)
        elif self.outer_scope is not None:
            return self.outer_scope.lookup(name)
        else:
            raise NameError(f"Undefined symbol: {name}")

    def define(self, name: str, value: str) -> None:
        # Define a new variable in the current environment
        self.bindings.update({ name: value })

    def set(self, name: str, value: str) -> None:
        if name in self.bindings:
            self.bindings.update({ name: value })
        elif self.outer_scope is not None:
            self.outer_scope.set(name, value)
        else:
            raise NameError(f"Undefined symbol: {name}")

    def copy(self) -> "Environment":
        return copy.deepcopy(self)