from copy import copy
from dataclasses import dataclass
from pathlib import PurePath
import shutil
import subprocess
from subprocess import PIPE
from typing import Callable, Generic, TypeVar

from abc import ABCMeta, abstractmethod

from gen.context import Context, Page

T = TypeVar('T')
U = TypeVar('U')

class Gen(metaclass=ABCMeta):
    @abstractmethod
    def build(self, ctx: Context) -> Context: ...

class Pred(metaclass=ABCMeta):
    @abstractmethod
    def eval(self, page: Page) -> bool: ...

@dataclass
class Filter(Gen):
    pred: Callable[[Page], bool]

    def build(self, ctx: Context) -> Context:
        ctx.pages = [p for p in ctx.pages if self.pred(p)]
        return ctx

@dataclass(init=False)
class Seq(Gen):
    args: tuple[Gen]

    def __init__(self, *args: Gen):
        self.args = args
    
    def build(self, ctx: Context): 
        for gen in self.args:
            ctx = gen.build(ctx)
        return ctx

@dataclass(init=False)
class Alt(Gen):
    args: tuple[Gen]

    def __init__(self, *args: Gen):
        self.args = args

    def build(self, ctx: Context): 
        for gen in self.args:
            gen.build(copy(ctx))
        return ctx

@dataclass
class Map(Gen):
    f: Callable[[Page], Page]
    
    def build(self, ctx: Context) -> Context:
        for f in ctx.pages:
            self.f(f)
        return ctx

def FilterExt(*exts: str) -> Filter:
    def filter(p: Page) -> bool:
        ext = p.source and PurePath(p.source).suffix
        return ext in exts
    return Filter(filter)

def FilterPath(*prefixes: str) -> Filter:
    paths = [PurePath(p) for p in prefixes]
    def filter(p: Page) -> bool:
        s = p.source
        if not s: return False
        return any(s.is_relative_to(p) for p in paths)
    return Filter(filter)    

def FilterPrefix(*prefixes: str) -> Filter:
    def filter(p: Page) -> bool:
        s = p.source and str(p.source)
        if not s: return False
        return any(s.startswith(p) for p in prefixes)
    return Filter(filter)   

def Pandoc(f: Page) -> Page:
    pandoc = shutil.which('pandoc')
    assert pandoc, "pandoc could not be not found in PATH"
    proc = subprocess.Popen([pandoc], stdin=PIPE, stdout=PIPE)

    out, _ = proc.communicate(f.data)
    print(out)
    return f

def Print(f: Page) -> Page:
    print('found: ', f)
    return f


if __name__ == '__main__':
    c = Context.from_root('./gen/site')

    Seq(
        Seq(
            FilterPath('gen/site'),
            FilterExt('.md'),
            Map(Pandoc)
        ),
    ).build(c)