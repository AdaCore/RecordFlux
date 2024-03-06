from pathlib import Path
from typing import Optional

class Location:
    def __init__(
        self,
        start: tuple[int, int],
        source: Optional[Path] = None,
        end: Optional[tuple[int, int]] = None,
    ): ...
    @property
    def source(self) -> Optional[Path]: ...
    @property
    def start(self) -> tuple[int, int]: ...
    @property
    def end(self) -> Optional[tuple[int, int]]: ...
    @property
    def short(self) -> Location: ...
