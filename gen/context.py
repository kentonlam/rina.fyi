from dataclasses import dataclass, field
from os import PathLike
from pathlib import Path, PurePath
from typing import Any, Iterable


@dataclass
class Page:
    source: PurePath | None = None
    output: PurePath | None = None

    data: bytes = field(default=b'', repr=False)
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
    pages: dict[str, Page | None] = field(default_factory=dict)

    # parent: 'Context' | None = None

    @classmethod
    def from_root(cls, root: str | PathLike) -> 'Context':
        p = Path(root)
        pages: dict[str, Page | None] = {
            str(f): Page.from_file(f)
            for f in p.glob('**/*')
            if f.is_file()
        }

        return cls(pages)

    def update(self, other: 'Context'):
        self.pages.update(other.pages)

    def narrow(self, keys: Iterable[str]) -> 'Context':
        new = Context(
            {k: self.pages[k] for k in keys}, 
            # self
        )
        return new

    # def widen(self) -> 'Context':
    #     assert self.parent, "attempt to widen Context with no parent"
    #     self.parent.pages.update(self.pages)
    #     return self.parent
