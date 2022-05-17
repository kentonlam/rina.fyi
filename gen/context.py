from dataclasses import dataclass, field
from os import PathLike, makedirs
from pathlib import Path, PurePath
from shutil import rmtree
from typing import Any, Iterable


MODULE_PATH = PurePath(__file__).parent

@dataclass
class Page:
    source: PurePath | None = None
    output: PurePath | None = None

    data: bytes = field(default=b'', repr=False)
    meta: dict[str,Any] = field(default_factory=dict)

    def build(self, root: str | PathLike): 
        if self.source and self.output:
            p = root / self.output
            makedirs(p.parent, exist_ok=True)
            with open(p, 'wb') as f:
                f.write(self.data)

    @classmethod
    def from_file(cls, file: str | PathLike) -> 'Page':
        p = PurePath(file)
        with open(p, 'rb') as f:
            data = f.read()
        return cls(p, p, data)


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

    def write(self, out: str | PathLike):
        rmtree(out, ignore_errors=True)
        makedirs(out, exist_ok=True)
        for k, p in self.pages.items():
            if p:
                p.build(out)
