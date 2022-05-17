from gen import *

c = Context.from_root('./gen/site')
print(c)
Alt(
    Seq(
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
    Seq(
        FilterExt('.html'),
        Map(Mako('default.html'))
    )
).build(c).write('build')