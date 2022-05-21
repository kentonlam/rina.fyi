from gen import *

c = Context.from_root('./gen/site')
print(c)
Alt(
    Seq(
        FilterExt('.md'),
        # PrintContext(),
        Map(Pandoc),
        Map(Mako('t.html')),
        Tag('md')
    ),
    Seq(
        FilterExt('.png'),
        PrintContext(),
    ),
    Seq(
        FilterExt('.asd'),
        New('asdf.html', {'title': 'asdf'}),
        GetTag('md'),
        Map(Mako('post-list.html'))
    ),
    Seq(
        FilterExt('.html'),
        Map(Mako('default.html'))
    ),
).build(c).write('build')