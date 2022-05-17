from datetime import date, datetime
import json
from pathlib import PurePath
import shutil
import subprocess
from gen.context import MODULE_PATH, Page

def parse_date(s: str) -> date | datetime:
    try:
        return date.fromisoformat(s)
    except ValueError:
        return datetime.fromisoformat(s)

def parse_metadata(b: bytes) -> dict[str,bytes]:
    lines = b.splitlines(keepends=True)
    parsed: dict[str,bytes] = {}

    sep = lines[0]
    name = None
    section = b''
    for l in lines[1:]:
        if name is None:
            name = l.strip().decode('utf-8')
        elif l == sep:
            parsed[name] = section.rstrip()
            name = None
            section = b''
        else:
            section += l
    return parsed

def Pandoc(p: Page) -> Page:
    pandoc = shutil.which('pandoc')
    assert pandoc, "pandoc could not be not found in PATH"
    template = MODULE_PATH / 'pandoc.html'
    proc = subprocess.Popen(
        [pandoc, '-s', '--toc', '--katex', '--template=' + str(template)], 
        stdin=subprocess.PIPE, 
        stdout=subprocess.PIPE
    )

    out, _ = proc.communicate(p.data)
    print(out)
    print()
    print()

    parsed = parse_metadata(out)
    p.data = parsed['body']

    meta = json.loads(parsed['meta'])
    if meta.get('date'):
        meta['date'] = parse_date(meta['date'])
        
    meta['toc'] = parsed['toc']
    p.meta.update(meta)

    assert p.output, "Pandoc must be used with a defined output path"
    p.output = p.output.with_suffix('.html')

    print()

    return p