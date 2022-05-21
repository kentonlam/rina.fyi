from dataclasses import dataclass, field
from urllib.parse import quote
from gen.log import log
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
    meta: dict[str,Any] = field(default_factory=dict, repr=False)

    def url(self) -> str:
        assert self.output
        p = self.output
        if p.suffix in ('.html', '.htm'):
            p = p.with_suffix('')
        return quote('/' + p.as_posix())

    def build(self, root: str | PathLike): 
        assert self.output, "attempt to build page with no defined output"
        if self.output:
            p = root / self.output
            makedirs(p.parent, exist_ok=True)
            with open(p, 'wb') as f:
                f.write(self.data)

    @classmethod
    def from_file(cls, file: str | PathLike, root: str | PathLike) -> 'Page':
        p = PurePath(file)
        log.debug('loading file %s', p)
        with open(p, 'rb') as f:
            data = f.read()
        rel = p.relative_to(root)
        return cls(rel, rel, data)


@dataclass
class Context:
    pages: dict[str, Page | None] = field(default_factory=dict)

    tags: dict[str, list[Page]] = field(default_factory=dict)
    # parent: 'Context' | None = None

    def add_page(self, page: Page):
        log.debug('adding page to context %s', page)
        self.pages[str(page.source)] = page

    @classmethod
    def from_root(cls, root: str | PathLike) -> 'Context':
        p = Path(root)
        c = cls()

        for f in p.glob('**/*'):
            if not f.is_file(): continue
            c.add_page(Page.from_file(f, p))

        return c

    def update(self, other: 'Context'):
        self.pages.update(other.pages)
        self.tags.update(other.tags)

    def narrow(self, keys: Iterable[str]) -> 'Context':
        log.debug('narrowing context to %s', keys)
        new = Context(
            {k: self.pages[k] for k in keys}, 
            dict(self.tags)
        )
        return new

    def write(self, out: str | PathLike):
        rmtree(out, ignore_errors=True)
        makedirs(out, exist_ok=True)
        for k, p in self.pages.items():
            if p:
                p.build(out)
