from abc import ABCMeta, abstractmethod
from dataclasses import dataclass
from pathlib import PurePath
from typing import Callable, TypeVar, cast
from mako.template import Template

from gen.context import Context, Page

T = TypeVar('T')
U = TypeVar('U')

class Gen(metaclass=ABCMeta):
    @abstractmethod
    def build(self, ctx: Context) -> Context: ...

@dataclass
class Filter(Gen):
    pred: Callable[[Page], bool]

    def build(self, ctx: Context) -> Context:
        return ctx.narrow(k for k, v in ctx.pages.items() if v and self.pred(v))

@dataclass(init=False)
class Seq(Gen):
    args: tuple[Gen]

    def __init__(self, *args: Gen):
        self.args = args
    
    def build(self, ctx: Context): 
        c = ctx
        for gen in self.args:
            c = gen.build(c)
        ctx.update(c)
        return ctx

@dataclass(init=False)
class Alt(Gen):
    args: tuple[Gen]

    def __init__(self, *args: Gen):
        self.args = args

    def build(self, ctx: Context): 
        for gen in self.args:
            ctx.update(gen.build(ctx))
        return ctx

PageTransform = Callable[[Page], Page | None]

@dataclass
class Map(Gen):
    f: PageTransform
    
    def build(self, ctx: Context) -> Context:
        for k, v in ctx.pages.items():
            if v: ctx.pages[k] = self.f(v)
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

class PrintContext(Gen):
    def build(self, ctx: Context) -> Context:
        print('seen context: ', ctx)
        return ctx

def Print(f: Page) -> Page:
    print('found: ', f)
    return f

