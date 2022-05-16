import shutil
from typing import Any, Union
from dataclasses import dataclass, field
from datetime import date
from os import PathLike
from pathlib import Path, PurePath

@dataclass
class Page:
    source: PurePath | None = None
    output: PurePath | None = None

    data: bytes = b''
    meta: dict[str,Any] = field(default_factory=dict)

    def build(self, root: str | PathLike): 
        if self.source and self.output:
            with open(root / self.output, 'wb') as f:
                f.write(self.data)

    @classmethod
    def from_file(cls, file: str | PathLike) -> 'Page':
        p = PurePath(file)
        with open(p, 'rb') as f:
            data = f.read()
        return cls(p, p, data)

# @dataclass
# class HTMLPage(DataPage):
#     body: str = ''
#     head: str = ''


@dataclass
class Context:
    pages: list[Page] = field(default_factory=list)

    @classmethod
    def from_root(cls, root: str | PathLike) -> 'Context':
        p = Path(root)
        pages = [
            Page.from_file(f) 
            for f in p.glob('**/*')
            if f.is_file()
        ]

        return cls(pages)