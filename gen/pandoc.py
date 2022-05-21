from datetime import date, datetime
import json
from pathlib import PurePath
import shutil
import subprocess
from gen import log
from gen.context import MODULE_PATH, Page

def parse_date(s: str) -> date | datetime:
    try:
        return date.fromisoformat(s)
    except ValueError:
        return datetime.fromisoformat(s)

def parse_metadata(b: str) -> dict[str,str]:
    lines = b.splitlines(keepends=True)
    parsed: dict[str,str] = {}

    sep = lines[0]
    name = None
    section = ''
    for l in lines[1:]:
        if name is None:
            name = l.strip()
        elif l == sep:
            parsed[name] = section.rstrip()
            name = None
            section = ''
        else:
            section += l
    return parsed

def Pandoc(p: Page) -> Page:
    log.info('processing page with pandoc %s', p)

    pandoc = shutil.which('pandoc')
    assert pandoc, "pandoc could not be not found in PATH"
    
    template = MODULE_PATH / 'pandoc.html'
    proc = subprocess.Popen(
        [pandoc, '-s', '--toc', '--katex', '--template=' + str(template)], 
        stdin=subprocess.PIPE, 
        stdout=subprocess.PIPE
    )

    out, _ = proc.communicate(p.data)
    # print(out)
    # print()
    # print()

    parsed = parse_metadata(out.decode('utf-8'))
    p.data = parsed['body'].encode('utf-8')

    meta = json.loads(parsed['meta'])
    if meta.get('date'):
        meta['date'] = parse_date(meta['date'])
        
    meta['toc'] = parsed['toc']
    p.meta.update(meta)

    assert p.output, "Pandoc must be used with a defined output path"
    p.output = p.output.with_suffix('.html')

    print()

    return p