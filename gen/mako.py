from typing import cast
from mako.template import Template
from gen import log

from gen.context import Page
from gen.gen import PageTransform


def Mako(t: str) -> PageTransform:
    template = Template(filename=t, output_encoding='utf-8')

    def apply(p: Page) -> Page:
        log.info('applying mako %s to page %s' , t, p)
        kwargs = p.__dict__ | p.meta
        kwargs['body'] = (kwargs['data'] or b'').decode('utf-8')
        log.debug('template data keys: %s', list(kwargs.keys()))
        p.data = cast(bytes, template.render(**kwargs))
        # print(p.data)
        return p
    return apply