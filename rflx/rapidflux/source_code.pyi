from pathlib import Path

def register(path: Path, source_code: str) -> None: ...
def retrieve(path: Path) -> str: ...
