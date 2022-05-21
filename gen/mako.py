from typing import cast
from mako.template import Template

from gen.context import Page
from gen.gen import PageTransform


def Mako(t: str) -> PageTransform:
    template = Template(filename=t, output_encoding='utf-8')

    def apply(p: Page) -> Page:
        kwargs = p.__dict__ | p.meta
        kwargs['body'] = (kwargs['data'] or b'').decode('utf-8')
        p.data = cast(bytes, template.render(**kwargs))
        # print(p.data)
        return p
    return apply