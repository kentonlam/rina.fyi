from gen import *

c = Context.from_root('./gen/site')

Alt(
    Seq(
        FilterPath('gen/site'),
        FilterExt('.md'),
        PrintContext(),
        Map(Pandoc),
        Map(Mako('t.html')),
        PrintContext()
    ),
    Seq(
        FilterExt('.png'),
        PrintContext(),
    ),
    PrintContext()
).build(c).write('build')